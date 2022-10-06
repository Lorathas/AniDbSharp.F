module AniDbSharp.F.Extensions.Udp

open System
open System.Net.Sockets

let sendBytes (client: UdpClient) (bytes: byte []) =
    async {
        return!
            client.SendAsync(bytes, bytes.Length)
            |> Async.AwaitTask
    }

let sendAscii (client: UdpClient) (message: string) =
    Text.Encoding.ASCII.GetBytes message
    |> sendBytes client

let sendUtf8 (client: UdpClient) (message: string) =
    Text.Encoding.UTF8.GetBytes message
    |> sendBytes client

let receiveBytes (client: UdpClient) (waitTime: TimeSpan option) =
    async {
        match waitTime with
        | Some t ->
            let task = client.ReceiveAsync()

            do!
                task.WaitAsync(t)
                |> Async.AwaitTask
                |> Async.Ignore

            if task.IsCompleted then
                return Some(task.Result)
            else
                return None
        | None ->
            let! result = client.ReceiveAsync() |> Async.AwaitTask

            return Some(result)
    }

let receiveBuffer (client: UdpClient) (waitTime: TimeSpan option) =
    async {
        let! response = receiveBytes client waitTime

        return Option.map (fun (r: UdpReceiveResult) -> r.Buffer) response
    }

let receiveAscii (client: UdpClient) (waitTime: TimeSpan option): Async<string option> =
    async {
        let! buffer = receiveBuffer client waitTime

        return Option.map (fun (b: byte[]) -> Text.Encoding.ASCII.GetString b) buffer
    }

let receiveUtf8 (client: UdpClient) (waitTime: TimeSpan option): Async<string option> =
    async {
        let! buffer = receiveBuffer client waitTime

        return Option.map (fun (b: byte[]) -> Text.Encoding.UTF8.GetString b) buffer
    }