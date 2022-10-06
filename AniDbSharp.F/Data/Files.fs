module AniDbSharp.F.Data.Files

open AniDbSharp.F.Data.Types

type MaskedFileInfo = { FileId: FileId; Data: string array }

type SimpleFileInfo = { FileId: FileId; AnimeId: AnimeId; EpisodeId: EpisodeId; ReleaseGroupId: GroupId; State: int; Size: int64; Ed2kHash: Ed2kHash; AniDbFileName: AnimeName }

type FileInfo =
    | Mask of MaskedFileInfo
    | Simple of SimpleFileInfo
    | Multiple of FileId array
    | NotFound