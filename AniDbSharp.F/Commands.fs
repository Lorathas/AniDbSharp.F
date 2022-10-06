module AniDbSharp.F.Commands

type Command =
    | Anime
    | AnimeDesc
    | Auth
    | BuddyAdd
    | BuddyDel
    | BuddyAccept
    | BuddyDeny
    | BuddyList
    | BuddyState
    | Calendar
    | Character
    | Creator
    | Episode
    | Encoding
    | Encrypt
    | File
    | Group
    | GroupStatus
    | Logout
    | MyList
    | MyListAdd
    | MyListDel
    | MyListExport
    | MyListStats
    | NotificationAdd
    | NotificationDel
    | Notify
    | NotifyList
    | NotifyGet
    | NotifyAck
    | Ping
    | Push
    | PushAck
    | Random
    | SendMsg
    | Stats
    | Top
    | Updated
    | Uptime
    | User
    | Version
    | Vote

let commandToValue command =
    match command with
    | Anime -> "ANIME"
    | AnimeDesc -> "ANIMEDESC"
    | Auth -> "AUTH"
    | BuddyAdd -> "BUDDYADD"
    | BuddyDel -> "BUDDYDEL"
    | BuddyAccept -> "BUDDYACCEPT"
    | BuddyDeny -> "BUDDYDENY"
    | BuddyList -> "BUDDYLIST"
    | BuddyState -> "BUDDYSTATE"
    | Calendar -> "CALENDAR"
    | Character -> "CHARACTER"
    | Creator -> "CREATOR"
    | Episode -> "EPISODE"
    | Encoding -> "ENCODING"
    | Encrypt -> "ENCRYPT"
    | File -> "FILE"
    | Group -> "GROUP"
    | GroupStatus -> "GROUPSTATUS"
    | Logout -> "LOGOUT"
    | MyList -> "MYLIST"
    | MyListAdd -> "MYLISTADD"
    | MyListDel -> "MYLISTDEL"
    | MyListExport -> "MYLISTEXPORT"
    | MyListStats -> "MYLISTSTATS"
    | NotificationAdd -> "NOTIFICATIONADD"
    | NotificationDel -> "NOTIFICATIONDEL"
    | Notify -> "NOTIFY"
    | NotifyList -> "NOTIFYLIST"
    | NotifyGet -> "NOTIFYGET"
    | NotifyAck -> "NOTIFYACK"
    | Ping -> "PING"
    | Push -> "PUSH"
    | PushAck -> "PUSHACK"
    | Random -> "RANDOM"
    | SendMsg -> "SENDMSG"
    | Stats -> "STATS"
    | Top -> "TOP"
    | Updated -> "UPDATED"
    | Uptime -> "UPTIME"
    | User -> "USER"
    | Version -> "VERSION"
    | Vote -> "VOTE"

let UnAuthCommandWhiteList = [|Auth; Encrypt; Encoding; Ping; Version|]