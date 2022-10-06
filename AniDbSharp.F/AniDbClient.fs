module AniDbSharp.F.AniDbClient

open System
open System.Net.Sockets
open System.Text.RegularExpressions
open AniDbSharp.F.Data.Files
open AniDbSharp.F.Data.Masks
open AniDbSharp.F.Responses
open AniDbSharp.F.StatusCodes
open Microsoft.FSharp.Reflection
open AniDbSharp.F.Commands
open AniDbSharp.F.Extensions.Udp
open Polly

[<Literal>]
let ClientPort = 32569

[<Literal>]
let ServerPort = 9000

[<Literal>]
let Endpoint = "api.anidb.net"

[<Literal>]
let SupportedApiVersion = "3"

let GlobalTimeout = TimeSpan.FromSeconds(30)

let GlobalTimeoutOption =
    Some(GlobalTimeout)

let private CooldownPolicy =
    Policy.RateLimitAsync(1, TimeSpan.FromSeconds(2))

let internal UnAuthWhiteList =
    [| Ping, Encrypt, Encoding, Auth, Version |]

type AniDbParam =
    | Int of int
    | Short of int16
    | Long of int64
    | Bool of bool
    | String of string
    | HexString of byte []
    | AnimeMask of AnimeMask
    | FileMask of FileMask

    static member fromAnime(anime: FileByAnimeSearch) =
        match anime with
        | AnimeId v -> Int v
        | AnimeName v -> String v

    static member fromGroup(group: FileByGroupSearch) =
        match group with
        | GroupId v -> Int v
        | GroupName v -> String v


let private serializeAniDbParam param : string =
    match param with
    | AnimeMask v -> animeMaskToHex v
    | FileMask v -> fileMaskToHex v
    | HexString v -> toHex v
    | Bool b -> if b then "true" else "false"
    | v -> v.ToString()

let private buildParamBody (parameterMap: Map<string, AniDbParam>) =
    parameterMap
    |> Map.toSeq
    |> Seq.map (fun (k, v) -> $"{k}={serializeAniDbParam v}")
    |> Seq.toList
    |> String.concat "&"

type AniDbResponseData =
    { Command: Command
      Status: AniDbStatusCode
      Body: string }

type private AniDbPossiblyHandledResponse =
    | Unhandled of AniDbResponseData
    | Handled of AniDbResponse<AniDbResponseData>

    static member map mapper response : AniDbResponse<AniDbResponseData> =
        match response with
        | Unhandled r -> mapper r
        | Handled r -> r

    static member unwrap response =
        match response with
        | Unhandled r -> Success r
        | Handled r -> r

    static member fromResponse response =
        match response with
        | Success r -> Unhandled r
        | r -> Handled r

let private ignoreIfHandled next response =
    match response with
    | Unhandled r -> next r
    | Handled r -> response

/// Filters out errors that can happen on every request sent to AniDb
/// <param name="response"></param>
/// <returns>Possible handled response that's filtered out the </returns>
let private globalRequestErrorFilter response =
    match response.Status with
    | AniDbStatusCode.IllegalInputOrAccessDenied -> Handled(Failure(response.Status, "Detected Illegal Input or access is denied"))
    | AniDbStatusCode.Banned -> Handled(Critical(response.Status, "Client is currently banned, wait for the AniDB API to cooldown before trying again"))
    | AniDbStatusCode.UnknownCommand -> Handled(Failure(response.Status, $"Unknown Command: {response.Command}"))
    | AniDbStatusCode.InternalServerError -> Handled(Critical(response.Status, "An unknown error with the server has occurred"))
    | AniDbStatusCode.AniDbOutOfService -> Handled(Critical(response.Status, "AniDB is currently experiencing service issues, try again later"))
    | AniDbStatusCode.ServerBusy -> Handled(Critical(response.Status, "AniDB's servers are currently overloaded, try again later"))
    | AniDbStatusCode.Timeout -> Handled(Critical(response.Status, "Request timed out"))
    | AniDbStatusCode.ApiViolation -> Handled(Failure(response.Status, "Client has caused an API violation"))
    | _ -> Unhandled response

let private authedRequestErrorFilter response =
    match response.Status with
    | AniDbStatusCode.LoginFirst -> Handled(Failure(response.Status, $"Command {response.Command} requires the client to login first"))
    | AniDbStatusCode.AccessDenied -> Handled(Failure(response.Status, "User is not allowed to access resource"))
    | AniDbStatusCode.InvalidSession -> Handled(Failure(response.Status, "Session is invalid, please login again to start a new session"))
    | _ -> Unhandled response

let private globalErrorFilter =
    ignoreIfHandled globalRequestErrorFilter

let private authedErrorFilter =
    globalErrorFilter
    >> ignoreIfHandled authedRequestErrorFilter

let private messageStartRegex =
    Regex(@"^\d3\s.*$", RegexOptions.Compiled)

module Parsing =
    let parseResponse command (response: string) =
        match response with
        | r when r.Length > 3 -> Error "Invalid AniDB Response: Response doesn't contain enough data to be parsed"
        | r when not <| messageStartRegex.IsMatch r -> Error "Invalid AniDB Response: Response is an invalid AniDB response"
        | r ->
            let rawCode = r[..3]
            let message = r[4..]

            match rawCode |> Int32.TryParse with
            | true, v ->
                let status: AniDbStatusCode =
                    LanguagePrimitives.EnumOfValue v

                Ok
                    { Command = command
                      Status = status
                      Body = message }
            | _ -> Error $"Invalid response code {rawCode}"

    let parseSingleFileResponse (message: string) : FileResponse =
        let parts =
            message.Split(
                "|",
                StringSplitOptions.TrimEntries
                ||| StringSplitOptions.RemoveEmptyEntries
            )

        if parts.Length = 8 then
            match hexToBytes parts[6] with
            | Ok b ->
                Simple
                    { FileId = int parts[0]
                      AnimeId = int parts[1]
                      EpisodeId = int parts[2]
                      ReleaseGroupId = int parts[3]
                      State = int parts[4]
                      Size = int64 parts[5]
                      Ed2kHash = b
                      AniDbFileName = parts[7] }
                |> Success
            | Error e -> Parsing e
        else
            Mask
                { FileId = int parts[0]
                  Data = parts[1..] }
            |> Success

    let parseFileResponse result =
        match result with
        | Success r ->
            match r.Status with
            | AniDbStatusCode.File -> parseSingleFileResponse r.Body
            | AniDbStatusCode.MultipleFilesFound ->
                r.Body.Split(
                    "|",
                    StringSplitOptions.TrimEntries
                    ||| StringSplitOptions.RemoveEmptyEntries
                )
                |> Array.map int
                |> Multiple
                |> Success
            | AniDbStatusCode.NoSuchFile -> Success NotFound
            | _ -> Failure(r.Status, r.Body)
        | r -> AniDbResponse.mapTo r

type AniDbClient(clientName, clientVersion) =
    member private this.udpClient =
        new UdpClient(ClientPort)

    member private this.token = None

    member private this.SendCommandInternal command (filter: AniDbPossiblyHandledResponse -> AniDbPossiblyHandledResponse) parameters =
        let bytes =
            System.Text.Encoding.ASCII.GetBytes $"{commandToValue command} {buildParamBody parameters}"

        async {
            if bytes.Length > 1400 then
                return Validation "Request size greater than 1,400 bytes. Please ensure that you are formatting the request correctly."
            else
                do! sendBytes this.udpClient bytes |> Async.Ignore

                let! response = receiveAscii this.udpClient GlobalTimeoutOption

                return
                    match response with
                    | None -> Critical(AniDbStatusCode.Banned, "Client is currently banned, wait for the AniDB API to cooldown before trying again")
                    | Some r ->
                        let parsed =
                            match Parsing.parseResponse command r with
                            | Ok d -> Success d
                            | Error m -> Parsing m

                        match AniDbPossiblyHandledResponse.fromResponse parsed
                              |> filter
                            with
                        | Handled r -> r
                        | Unhandled r -> Success r
        }


    member this.SendAuthorizedCommand command (parameters: Map<string, AniDbParam>) =
        let param = Option.get this.token |> String

        let parameters =
            Map.add "s" param parameters

        this.SendCommandInternal command authedErrorFilter parameters

    member this.SendCommand command parameters =
        this.SendCommandInternal command globalErrorFilter parameters

    interface IAniDbClient with
        member this.Auth username password =
            let paramMap =
                [ "user", String username
                  "pass", String password
                  "protover", String SupportedApiVersion
                  "client", String clientName
                  "clientver", String clientVersion ]
                |> Map.ofList

            async {
                let! response = this.SendCommand Command.Auth paramMap

                return
                    match response with
                    | Success r ->
                        match r.Status with
                        | AniDbStatusCode.LoginAccepted ->
                            // this.token <- r.Body
                            Success
                                { SessionKey = r.Body
                                  IsNewVersionAvailable = false }
                        | AniDbStatusCode.LoginAcceptedNewVersionAvailable ->
                            Success
                                { SessionKey = r.Body
                                  IsNewVersionAvailable = true }
                        | AniDbStatusCode.LoginFailed -> Failure(r.Status, "Incorrect username or password pair")
                        | c -> Failure(c, r.Body)
                    | r -> AniDbResponse.mapTo r
            }

        member this.FindFileByAnimeInfo searchParams =
            let paramMap =
                [ FileByAnimeSearch.toField searchParams.Anime, AniDbParam.fromAnime searchParams.Anime
                  FileByGroupSearch.toField searchParams.Group, AniDbParam.fromGroup searchParams.Group
                  "epno", Int searchParams.EpisodeNumber
                  "fmask", FileMask searchParams.FileMask
                  "amask", AnimeMask searchParams.AnimeMask ]
                |> Map.ofList

            async {
                let! result = this.SendAuthorizedCommand Auth paramMap

                return Parsing.parseFileResponse result
            }

        member this.FindFileByHash ed2kHash length fileMask animeMask =
            let paramMap =
                [ "ed2k", HexString ed2kHash
                  "size", Long length
                  "fmask", FileMask fileMask
                  "amask", AnimeMask animeMask ]
                |> Map.ofList

            async {
                let! result = this.SendAuthorizedCommand File paramMap

                return Parsing.parseFileResponse result
            }

        member this.FindFileById fileId fileMask animeMask =
            let paramMap =
                [ "fid", Int fileId
                  "fmask", FileMask fileMask
                  "amask", AnimeMask animeMask ]
                |> Map.ofList

            async {
                let! result = this.SendAuthorizedCommand File paramMap

                return Parsing.parseFileResponse result
            }

        member this.Logout() =
            async {
                let! result = this.SendAuthorizedCommand Logout Map.empty

                return
                    match result with
                    | Success r ->
                        match r.Status with
                        | AniDbStatusCode.LoggedOut -> Success()
                        | AniDbStatusCode.NotLoggedIn -> Success()
                        | c -> Failure(c, "An unknown error occurred while trying to lot out")
                    | _ -> AniDbResponse.mapTo result
            }

    interface IDisposable with
        member this.Dispose() = this.udpClient.Dispose()
