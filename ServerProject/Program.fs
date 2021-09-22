#if INTERACTIVE
#r "nuget: Akka.FSharp"
#r "nuget: Akka.Remote"
#endif

open System
open System.Text
open System.Diagnostics
open System.Security.Cryptography
open Akka.FSharp
open Akka.Remote
open Akka.Routing
open System.Threading


// Configuration
let config =
    Configuration.parse
        @"akka {
            log-config-on-start = on
            stdout-loglevel = DEBUG
            loglevel = DEBUG
            actor {
                provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
            }
            remote {
                helios.tcp {
                    transport-class = ""Akka.Remote.Transport.Helios.HeliosTcpTransport, Akka.Remote""
                    applied-adapters = []
                    transport-protocol = tcp
                    port = 8080
                    hostname = localhost
                }
            }
        }"


// let system = System.create "my-system" (Configuration.load())
let system = System.create "MyServer" config
    

// Coin collector
let printCoinAndIncrementCounter str count =
    let result = (string str).Split ' '
    let inputString = result.[0]
    let hashValue = result.[1]
    printfn "%s\t%s" inputString hashValue
    // printfn "Incrementing counter [%d] by 1." count
    count + 1

let coinCollector f initialState (mailbox: Actor<'a>) = 
    let rec loop lastState = actor {
        let! message = mailbox.Receive()
        let newState = f message lastState
        return! loop newState
    }
    loop initialState

// let coinCollectorRef =
//     coinCollector printCoinAndIncrementCounter 0
//     |> spawn system "coinCollector"

// coinCollectorRef <! "Nikhil 345ynrgkrtot5enkrg";;


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
        coin <- "[Server]" + inputString + " " + hashValue
    else
        coin <- "-1"
    coin

let findBitCoins noOfLeadingZeroes inputString coinCollectorRef =
    // for i = 1 to 1000000 do
    while true do
        let nonce = randomAlphanumericString(12)
        let modifiedInputString = inputString + nonce
        let hashValue = generateHashInput modifiedInputString
        let coin = checkValidCoin modifiedInputString hashValue noOfLeadingZeroes 
        if coin <> "-1" then
            coinCollectorRef <! coin

let worker inputString coinCollectorRef (mailbox: Actor<'a>) = 
    let rec loop () = actor {
        let! message = mailbox.Receive ()
        findBitCoins message inputString coinCollectorRef
        return! loop ()
    }
    loop ()

// Worker configuration
// let noOfWorkers = 10
// let inputString = "n.saoji"

// // Single worker
// let workerRef =
//     worker inputString coinCollectorRef
//     |> spawn system "worker"
// workerRef <! 8

// // Worker router
// let workerRef =
//     worker inputString coinCollectorRef
//     |> spawnOpt system "worker"
//     <| [SpawnOption.Router(RoundRobinPool(noOfWorkers))]
// for i = 1 to noOfWorkers do
//     workerRef <! 4


// Master


let master (noOfLeadingZeroes: int) (noOfWorkers: int) (inputString: string) workerRef (mailbox: Actor<'a>) =
    let rec loop () = actor {
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender ()
        match message with
        | "Mine" ->
            printfn "[INFO] Call from the main"
            for i = 1 to noOfWorkers do
                workerRef <! noOfLeadingZeroes
        | "AssignWorkToMe" ->
            printfn "[INFO] Call from the client"
            let myString = (string noOfLeadingZeroes) + "|" + inputString
            sender <! myString
        | _ -> ()
        return! loop ()
    }
    loop()
    
// let masterRef =
//     master noOfWorkers inputString workerRef
//     |> spawn system "master"

// masterRef <! 3

[<EntryPoint>]
let main argv =
    let noOfLeadingZeroes = int argv.[0]
    // let noOfLeadingZeroes = 4
    printfn "[INFO] Number of leading zeroes: %i" noOfLeadingZeroes

    let noOfWorkers = 12
    printfn "[INFO] Number of workers: %d" noOfWorkers

    let inputString = "n.saoji"
    printfn "[INFO] Input string: %s" inputString

    // Coin collector
    let coinCollectorRef =
        coinCollector printCoinAndIncrementCounter 0
        |> spawn system "coinCollector"

    // Worker
    let workerRef =
        worker inputString coinCollectorRef
        |> spawnOpt system "worker"
        <| [SpawnOption.Router(RoundRobinPool(noOfWorkers))]

    // Master
    let masterRef =
        master noOfLeadingZeroes noOfWorkers inputString workerRef
        |> spawn system "master"

    // Evaluation code
    let proc = Process.GetCurrentProcess()
    let cpuTimeStamp = proc.TotalProcessorTime
    let timer = new Stopwatch()
    timer.Start()
    try
        masterRef <! "Mine"
        Thread.Sleep(60000)
        // system.WhenTerminated.Wait()
    finally
        let cpuTime = (proc.TotalProcessorTime - cpuTimeStamp).TotalMilliseconds
        printfn "CPU time = %d ms" (int64 cpuTime)
        printfn "Absolute time = %d ms" timer.ElapsedMilliseconds
        printfn "Ratio = %f" (cpuTime / float(timer.ElapsedMilliseconds))
    0