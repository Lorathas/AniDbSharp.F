module AniDbSharp.F.Data.Masks

open System
open System.Text.RegularExpressions


let private HexVerify =
    Regex("^[a-fA-F0-9]+$", RegexOptions.Compiled)

let private replaceHexDashes (value: string) = value.Replace("-", String.Empty)

let toHex = BitConverter.ToString >> replaceHexDashes

let private getHexVal hex =
    let value = int hex
    //For uppercase A-F letters:
    //return val - (val < 58 ? 48 : 55);
    //For lowercase a-f letters:
    //return val - (val < 58 ? 48 : 87);
    //Or the two combined, but a bit slower:
    value
    - match value with
      | v when v < 58 -> 48
      | v when v < 97 -> 55
      | _ -> 87

let hexToBytes (hex: string) =
    match hex with
    | h when h.Length % 2 = 1 -> Error $"{nameof (hex)} must be an even length."
    | h when not <| HexVerify.IsMatch h -> Error "Invalid value detected."
    | h ->
        let bytes =
            seq {
                for idx = 0 to hex.Length >>> 1 do
                    let first = (getHexVal hex[idx <<< 1]) <<< 4
                    let second = getHexVal hex[(idx <<< 1) + 1]
                    yield byte (first + second)
            }

        Array.ofSeq bytes |> Ok

[<Flags>]
type AnimeMaskOne =
    | TotalEpisodes = 128
    | HighestEpisodeNumber = 64
    | Year = 32
    | Type = 16
    | RelatedAnimeIdList = 8
    | RelatedAnimeIdType = 4
    | CategoryList = 2


[<Flags>]
type AnimeMaskTwo =
    | RomajiName = 128
    | KanjiName = 64
    | EnglishName = 32
    | OtherName = 16
    | ShortNameList = 8
    | SynonymList = 4


[<Flags>]
type AnimeMaskThree =
    | EpisodeNumber = 128
    | EpisodeName = 64
    | EpisodeRomajiName = 32
    | EpisodeKanjiName = 16
    | EpisodeRating = 8
    | EpisodeVoteCount = 4


[<Flags>]
type AnimeMaskFour =
    | GroupName = 128
    | GroupShortName = 64
    | DateAnimeIdRecordUpdated = 1

type AnimeMask =
    { FirstByte: AnimeMaskOne
      SecondByte: AnimeMaskTwo
      ThirdByte: AnimeMaskThree
      FourthByte: AnimeMaskFour }
    
type AnimeMaskBytes = byte[]
type AnimeMaskHex = string

let animeMaskGenerateBytes mask: AnimeMaskBytes =
    [| byte mask.FirstByte
       byte mask.SecondByte
       byte mask.ThirdByte
       byte mask.FourthByte|]

let animeMaskToHex mask: AnimeMaskHex =
    animeMaskGenerateBytes mask |> BitConverter.ToString |> replaceHexDashes

let animeMaskFromHex hex =
    match hexToBytes hex with
    | Ok b ->
        match b with
        | b when b.Length > 4 && b.Length <= 0 -> Error "Invalid hex string result"
        | b when b.Length = 4 ->
            Ok
                { FirstByte = int b[0] |> LanguagePrimitives.EnumOfValue
                  SecondByte = int b[1] |> LanguagePrimitives.EnumOfValue
                  ThirdByte = int b[2] |> LanguagePrimitives.EnumOfValue
                  FourthByte = int b[3] |> LanguagePrimitives.EnumOfValue }
        | b when b.Length = 3 ->
            Ok
                { FirstByte = int b[0] |> LanguagePrimitives.EnumOfValue
                  SecondByte = int b[1] |> LanguagePrimitives.EnumOfValue
                  ThirdByte = int b[2] |> LanguagePrimitives.EnumOfValue
                  FourthByte = LanguagePrimitives.EnumOfValue 0 }
        | b when b.Length = 2 ->
            Ok
                { FirstByte = int b[0] |> LanguagePrimitives.EnumOfValue
                  SecondByte = int b[1] |> LanguagePrimitives.EnumOfValue
                  ThirdByte = LanguagePrimitives.EnumOfValue 0
                  FourthByte = LanguagePrimitives.EnumOfValue 0 }
        | b ->
            Ok
                { FirstByte = int b[0] |> LanguagePrimitives.EnumOfValue
                  SecondByte = LanguagePrimitives.EnumOfValue 0
                  ThirdByte = LanguagePrimitives.EnumOfValue 0
                  FourthByte = LanguagePrimitives.EnumOfValue 0 }
    | Error m -> Error m


[<Flags>]
type FileMaskOne =
    | AnimeId = 64
    | EpisodeId = 32
    | GroupId = 16
    | MyListId = 8
    | OtherEpisodes = 4
    | IsDeprecated = 2
    | State = 1

[<Flags>]
type FileMaskTwo =
    | Size = 128
    | Ed2k = 64
    | Md5 = 32
    | Sha1 = 16
    | Crc32 = 8
    | VideoColorDepth = 2

[<Flags>]
type FileMaskThree =
    | Quality = 128
    | Source = 64
    | AudioCodecList = 32
    | AudioBitrateList = 16
    | VideoCodec = 8
    | VideoBitrate = 4
    | VideoResolution = 2
    | FileType = 1

[<Flags>]
type FileMaskFour =
    | DubLanguage = 128
    | SubLanguage = 64
    | LengthInSeconds = 32
    | Description = 16
    | AiredDate = 8
    | AniDbFilename = 1

[<Flags>]
type FileMaskFive =
    | MyListState = 128
    | MyListFileState = 64
    | MyListViewed = 32
    | MyListViewDate = 16
    | MyListStorage = 8
    | MyListSource = 4
    | MyListOther = 2


type FileMask =
    { FirstByte: FileMaskOne
      SecondByte: FileMaskTwo
      ThirdByte: FileMaskThree
      FourthByte: FileMaskFour
      FifthByte: FileMaskFive }

let fileMaskGenerateBytes mask =
    [| byte mask.FirstByte
       byte mask.SecondByte
       byte mask.ThirdByte
       byte mask.FourthByte
       byte mask.FifthByte |]

type FileMaskBytes = byte[]
type FileMaskHex = string

let fileMaskToHex mask: FileMaskHex =
    fileMaskGenerateBytes mask |> BitConverter.ToString |> replaceHexDashes

let fileMaskFromHex hex =
    match hexToBytes hex with
    | Ok b ->
        match b with
        | b when b.Length > 5 && b.Length <= 0 -> Error "Invalid hex string result"
        | b when b.Length = 5 ->
            Ok
                { FirstByte = int b[0] |> LanguagePrimitives.EnumOfValue
                  SecondByte = int b[1] |> LanguagePrimitives.EnumOfValue
                  ThirdByte = int b[2] |> LanguagePrimitives.EnumOfValue
                  FourthByte = int b[3] |> LanguagePrimitives.EnumOfValue
                  FifthByte = int b[4] |> LanguagePrimitives.EnumOfValue }
        | b when b.Length = 4 ->
            Ok
                { FirstByte = int b[0] |> LanguagePrimitives.EnumOfValue
                  SecondByte = int b[1] |> LanguagePrimitives.EnumOfValue
                  ThirdByte = int b[2] |> LanguagePrimitives.EnumOfValue
                  FourthByte = int b[3] |> LanguagePrimitives.EnumOfValue
                  FifthByte = LanguagePrimitives.EnumOfValue 0 }
        | b when b.Length = 3 ->
            Ok
                { FirstByte = int b[0] |> LanguagePrimitives.EnumOfValue
                  SecondByte = int b[1] |> LanguagePrimitives.EnumOfValue
                  ThirdByte = int b[2] |> LanguagePrimitives.EnumOfValue
                  FourthByte = LanguagePrimitives.EnumOfValue 0
                  FifthByte = LanguagePrimitives.EnumOfValue 0 }
        | b when b.Length = 2 ->
            Ok
                { FirstByte = int b[0] |> LanguagePrimitives.EnumOfValue
                  SecondByte = int b[1] |> LanguagePrimitives.EnumOfValue
                  ThirdByte = LanguagePrimitives.EnumOfValue 0
                  FourthByte = LanguagePrimitives.EnumOfValue 0
                  FifthByte = LanguagePrimitives.EnumOfValue 0 }
        | b ->
            Ok
                { FirstByte = int b[0] |> LanguagePrimitives.EnumOfValue
                  SecondByte = LanguagePrimitives.EnumOfValue 0
                  ThirdByte = LanguagePrimitives.EnumOfValue 0
                  FourthByte = LanguagePrimitives.EnumOfValue 0
                  FifthByte = LanguagePrimitives.EnumOfValue 0 }
    | Error m -> Error m
