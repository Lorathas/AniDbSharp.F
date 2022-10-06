namespace AniDbSharp.F

open AniDbSharp.F.Data.Masks
open AniDbSharp.F.Data.Types
open AniDbSharp.F.Responses

type FileByAnimeSearch =
    | AnimeId of AnimeId
    | AnimeName of AnimeName
    
    static member toField anime =
        match anime with
        | AnimeId _ -> "aid"
        | AnimeName _ -> "aname"

type FileByGroupSearch =
    | GroupId of GroupId
    | GroupName of GroupName
    
    static member toField anime =
        match anime with
        | GroupId _ -> "gid"
        | GroupName _ -> "gname"

type FileByAnimeSearchParams = { Anime: FileByAnimeSearch; Group: FileByGroupSearch; EpisodeNumber: int; FileMask: FileMask; AnimeMask: AnimeMask }

type IAniDbClient =
    abstract member Auth: username: string -> password: string -> Async<AuthResponse>
    abstract member Logout: Unit -> Async<LogoutResponse>
    abstract member FindFileById: fileId: FileId -> fileMask: FileMask -> animeMask: AnimeMask -> Async<FileResponse>
    abstract member FindFileByHash: ed2kHash: Ed2kHash -> length: int64 -> fileMask: FileMask -> animeMask: AnimeMask -> Async<FileResponse>

    abstract member FindFileByAnimeInfo: searchParams: FileByAnimeSearchParams -> Async<FileResponse>
