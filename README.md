# AniDbSharp.F

AniDbSharp.F is a F# library for interfacing with [AniDb's UDP API](https://wiki.anidb.net/UDP_API_Definition#LOGOUT:_Logout)

## AniDbClient
The [`AniDbClient`](https://github.com/Lorathas/AniDbSharp.F/blob/master/AniDbSharp.F/AniDbClient.fs) is 
the main way of interfacing with the API.
It contains all the methods supported by the API, but some may not be implemented yet.  

An [`IAniDbClient`](https://github.com/Lorathas/AniDbSharp.F/blob/master/AniDbSharp.F/IAniDbClient.fs) interface 
is provided for use in IoC containers if needed.
