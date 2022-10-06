module AniDbSharp.F.Responses

open AniDbSharp.F.StatusCodes
open AniDbSharp.F.Data.Files

type AniDbResponse =
    { Status: AniDbStatusCode
      Message: string
      Body: seq<string> }

// type AniDbRequestFailure =
//     | IllegalInputOrBanned
//     | Banned of reason: string
//     | UnknownCommand
//     | InternalServerError
//     | AniDbOutOfService
//     | ServerBusy
//     | Timeout
//     | AniDbError of string
//     | Error of string

// type AniDbAuthedRequestFailure =
//     | IllegalInputOrBanned
//     | Banned of reason: string
//     | UnknownCommand
//     | InternalServerError
//     | AniDbOutOfService
//     | ServerBusy
//     | Timeout
//     | AniDbError of string
//     | Error of string
//     | LoginFirst
//     | AccessDenied
//     | InvalidSession

type AniDbResponse<'T> =
    | Success of 'T
    | Failure of code: AniDbStatusCode * message: string
    | Critical of code: AniDbStatusCode * message: string
    | Parsing of message: string
    | Validation of message: string
    
    static member isSuccess data =
        match data with
        | Success _ -> true
        | _ -> false
    
    static member isFailure() = not << AniDbResponse.isSuccess
    
    static member map mapper response =
        match response with
        | Success r -> mapper r
        | _ -> response
    
    static member mapTo<'A, 'B> (response: AniDbResponse<'A>): AniDbResponse<'B> =
        match response with
        | Success _ -> Critical (AniDbStatusCode.Unknown, "mapTo can only be called on non-success codes")
        | Failure (c, m) -> Failure (c, m)
        | Critical (c, m) -> Critical (c, m)
        | Parsing m -> Parsing m
        | Validation m -> Validation m

type AuthResponseData = { SessionKey: string; IsNewVersionAvailable: bool; }

type AuthResponse = AniDbResponse<AuthResponseData>

type LogoutResponse = AniDbResponse<Unit>

(*type AuthResponse =
    | Success of sessionKey: string * isNewVersionAvailable: bool
    | LoginFailed
    | ClientVersionOutdated
    | ClientBanned of reason: string
    | AniDbFailure of AniDbRequestFailure*)

type FileResponse = AniDbResponse<FileInfo>
