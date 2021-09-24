//Imports
open Akka.FSharp
open Akka.Remote
open Akka.Routing
open System
open System.Diagnostics
open System.Security.Cryptography
open Myconfig



// // Configuration
// let config =
//     Configuration.parse
//         @"akka {
//             log-config-on-start = on
//             stdout-loglevel = DEBUG
//             loglevel = DEBUG
//             actor {
//                 provider = ""Akka.Remote.RemoteActorRefProvider, Akka.Remote""
//             }
//             remote {
//                 helios.tcp {
//                     transport-class = ""Akka.Remote.Transport.Helios.HeliosTcpTransport, Akka.Remote""
//                     applied-adapters = []
//                     transport-protocol = tcp
//                     port = 8080
//                     hostname = localhost
//                 }
//             }
//         }"


let system = System.create "BitcoinServer" config
    


let printCoinAndIncrementCounter str count =
    let result = (string str).Split ' '
    let inputString = result.[0]
    let hashValue = result.[1]
    printfn "%s\t%s" inputString hashValue
    count + 1

let Supervisor f initialState (mailbox: Actor<'a>) = 
    let rec loop lastState = actor {
        let! message = mailbox.Receive()
        let newState = f message lastState
        return! loop newState
    }
    loop initialState

let worker inputString SupervisorRef (mailbox: Actor<'a>) = 
    let rec loop () = actor {
        let! (msg:obj) = mailbox.Receive ()
        let (k, i) : Tuple<int, int>= downcast msg

        printfn "%A%A%A" msg k i
        while true do
          let rstring n = 
            let r = Random()
            let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|];[|'!' .. '?'|]])  
            let sz = Array.length chars in
            String(Array.init n (fun _ -> chars.[r.Next sz]))

          let randomstr = inputString + rstring 5

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
              
              SupervisorRef <! randomstr + " " + outputstring
        return! loop ()
    }
    loop ()



// Router
let RouterActor (noOfLeadingZeroes: int) (noOfWorkers: int) (inputString: string) workerRef (mailbox: Actor<'a>) =
    let rec loop () = actor {
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender ()
        match message with
        | "Mine" ->
            printfn "[INFO] Call from the main"
            for i = 1 to noOfWorkers do
                workerRef <! (noOfLeadingZeroes, i)
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
    printfn "[INFO] Number of leading zeroes: %i" noOfLeadingZeroes

    let noOfWorkers = System.Environment.ProcessorCount 
    printfn "[INFO] Number of workers: %d" noOfWorkers

    let inputString = "vaishnavi.dongre"
    printfn "[INFO] Input string: %s" inputString

    let SupervisorRef =
        Supervisor printCoinAndIncrementCounter 0
        |> spawn system "Supervisor"

    let workerRef =
        worker inputString SupervisorRef
        |> spawnOpt system "worker"
        <| [SpawnOption.Router(RoundRobinPool(noOfWorkers))]
                
    // Router
    let RouterActorRef =
        RouterActor noOfLeadingZeroes noOfWorkers inputString workerRef
        |> spawn system "RouterActor"

    let proc = Process.GetCurrentProcess()
    let cpuTimeStamp = proc.TotalProcessorTime
    let timer = new Stopwatch()
    timer.Start()
    try
        RouterActorRef <! "Mine"
        System.Console.ReadLine() |> ignore
    finally
        let cpuTime = (proc.TotalProcessorTime - cpuTimeStamp).TotalMilliseconds
        printfn "CPU time = %d ms" (int64 cpuTime)
        printfn "Absolute time = %d ms" timer.ElapsedMilliseconds
        printfn "Ratio = %f" (cpuTime / float(timer.ElapsedMilliseconds))
    0