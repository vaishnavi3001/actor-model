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

let worker inputString coinCollectorRef (mailbox: Actor<'a>) = 
    let rec loop () = actor {
        let! k = mailbox.Receive ()
        while true do
      

          let rstring n = 
            let r = Random()
            let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|];[|'!' .. '?'|]])  
            let sz = Array.length chars in
            String(Array.init n (fun _ -> chars.[r.Next sz]))

          let randomstr = "hshah1;" + rstring 5

          //SHA 256         
          let randombyte = System.Text.Encoding.ASCII.GetBytes(randomstr)
          use sha256Hash = SHA256Managed.Create()
          let encrypted = sha256Hash.ComputeHash(randombyte)
          let bytetohex bytes = 
            bytes 
            |> Array.map (fun (x: byte) -> System.String.Format("{0:X2}", x))
            |> String.concat System.String.Empty
          let outputstring = bytetohex encrypted 
          // FIND BITCOINS WITH LEADING ZEROS
          let mutable count = 0
          

          for i in 0..k-1 do 
            if outputstring.[i] = '0' then 
              count <- count + 1
            if count = k then
              
              coinCollectorRef <! randomstr + " " + outputstring
        return! loop ()
    }
    loop ()



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
    


[<EntryPoint>]
let main argv =
    let noOfLeadingZeroes = int argv.[0]
    // let noOfLeadingZeroes = 4
    printfn "[INFO] Number of leading zeroes: %i" noOfLeadingZeroes

    let noOfWorkers = 12
    printfn "[INFO] Number of workers: %d" noOfWorkers

    let inputString = "vaishnavi.dongre"
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