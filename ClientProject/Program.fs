#if INTERACTIVE
#r "nuget: Akka.FSharp"
#r "nuget: Akka.Remote"
#endif

open System
open System.Text
open System.Security.Cryptography
open Akka.FSharp
open Akka.Remote
open Akka.Routing
open System.Threading


let config =
    Configuration.parse
        @"akka {
            actor {
                provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
            }
            remote {
                helios.tcp {
                    transport-class = ""Akka.Remote.Transport.Helios.HeliosTcpTransport, Akka.Remote""
		            applied-adapters = []
		            transport-protocol = tcp
		            port = 0
		            hostname = localhost
                }
            }
        }"


// let system = System.create "my-system" (Configuration.load())
let system = System.create "MyClient" config


type ClientMessage = ClientTuple of int * string


let master ipAddress noOfWorkers workerRef (mailbox: Actor<'a>) =
    let url = "akka.tcp://BitcoinServer@" + ipAddress + ":8080/user/master"
    let serverMaster = select url system
    let rec loop() = actor {
        let! message = mailbox.Receive()
        match box message with
        | :? string as msg when msg="GetWorkFromServer" -> 
            printfn "[INFO] Client has joined. Getting work from server..."
            serverMaster <! "AssignWorkToMe"
        | :? string as msg when (msg |> String.exists (fun char -> char = '|')) ->
            printfn "[INFP] Received work from the server"
            let result = msg.Split '|'
            let noOfLeadingZeroes = int result.[0]
            let inputString = result.[1]
            workerRef <! ClientTuple(noOfLeadingZeroes, inputString)
        | _ -> ()
        // | :? string as msg when msg = "GetWorkFromServer" ->
            
        // | (:? string as msg) | ClientTuple(noOfLeadingZeroes, inputString)->
        //     printfn "[INFO] Call from the server"
        //     for i = 1 to noOfWorkers do
        //         workerRef <! ClientTuple(noOfLeadingZeroes, inputString)
        return! loop()
    }
    loop()
 

// Worker
let randomAlphanumericString length =
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    let random = Random()
    seq {
        for _ = 1 to length do
            yield chars.[random.Next(chars.Length)]
    }
    |> Seq.toArray
    |> (fun x -> String(x))


let ByteToHex bytes = 
    bytes 
    |> Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x))
    |> String.concat System.String.Empty

let generateHashInput (inputString: string) =
    let hashValue =
        inputString
        |> Encoding.UTF8.GetBytes
        |> (new SHA256Managed()).ComputeHash
        |> ByteToHex
    hashValue

let checkValidCoin inputString hashValue noOfLeadingZeroes =
    let mutable compareString = ""
    let mutable coin = ""
    for i = 1 to noOfLeadingZeroes do
        compareString <- compareString + "0"
    if (string hashValue).StartsWith compareString then
        coin <- "[Client]" + inputString + " " + hashValue
    else
        coin <- "-1"
    coin

let findBitCoins noOfLeadingZeroes inputString SupervisorRef =
    // for i = 1 to 1000000000 do
    while true do
        let nonce = randomAlphanumericString(12)
        let modifiedInputString = inputString + nonce
        let hashValue = generateHashInput modifiedInputString
        let coin = checkValidCoin modifiedInputString hashValue noOfLeadingZeroes 
        if coin <> "-1" then
            SupervisorRef <! coin

let worker ipAddress (mailbox: Actor<'a>) =
    let url = "akka.tcp://BitcoinServer@" + ipAddress + ":8080/user/Supervisor"
    let SupervisorRef = select url system 
    let rec loop () = actor {
        let! ClientTuple(noOfLeadingZeroes, inputString) = mailbox.Receive ()
        // Handle an incoming message
        findBitCoins noOfLeadingZeroes inputString SupervisorRef
        return! loop ()
    }
    loop ()

    
[<EntryPoint>]
let main argv =
    let ipAddress = string argv.[0]
    printfn "IP Address: %s" ipAddress

    let noOfWorkers = 10
    printfn "Number of workers: %d" noOfWorkers

    // Worker
    let workerRef =
        worker ipAddress
        |> spawnOpt system "worker"
        <| [SpawnOption.Router(RoundRobinPool(noOfWorkers))]

    let masterRef =
        master ipAddress noOfWorkers workerRef
        |> spawn system "master"

    masterRef <! "GetWorkFromServer"
    Thread.Sleep(60000000);
    0 // return an integer exit code
