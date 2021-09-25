//Imports
open Akka.FSharp
open Akka.Routing
open System
open System.Diagnostics
open System.Security.Cryptography
open Myconfig


let system = System.create "BitcoinServer" config
    

let Supervisor  initialState (mailbox: Actor<'a>) = 
    let rec loop lastState = actor {
        let! message = mailbox.Receive()
        
        let result = (string message).Split ' '
        let gatorID = result.[0]
        let hashValue = result.[1]
        printfn "%s\t%s" gatorID hashValue
        let newState = lastState + 1
        return! loop newState
    }
    loop initialState

let worker gatorID SupervisorRef (mailbox: Actor<'a>) = 
    let rec loop () = actor {
        let! (msg:obj) = mailbox.Receive ()
        let (k, i) : Tuple<int, int>= downcast msg
        while true do
          let rstring n = 
            let r = Random()
            let chars = Array.concat([[|'a' .. 'z'|];[|'A' .. 'Z'|];[|'0' .. '9'|];[|'!' .. '?'|]])  
            let sz = Array.length chars in
            String(Array.init n (fun _ -> chars.[r.Next sz]))

          let randomstr = gatorID + rstring 5

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
              
              SupervisorRef <! "Server:"+ randomstr + " " + outputstring
        return! loop ()
    }
    loop ()



// Router
let RouterActor (noOfZero: int) (noOfWorkers: int) (gatorID: string) workerRef (mailbox: Actor<'a>) =
    let rec loop () = actor {
        let! message = mailbox.Receive ()
        let sender = mailbox.Sender ()
        match message with
        | "ServerInitiation" ->
            for i = 1 to noOfWorkers do
                workerRef <! (noOfZero, i)
        | "ClientInitiation" ->
            let myString = (string noOfZero) + "|" + gatorID
            sender <! myString
        | _ -> ()
        return! loop ()
    }
    loop()




[<EntryPoint>]
let main argv =
    let noOfZero = int argv.[0]
    let gatorID = "vaishnavi.dongre"
    let noOfWorkers = System.Environment.ProcessorCount
    printfn "Number of workers: %d" noOfWorkers

    // Supervisor Instatiation
    let SupervisorRef =
        Supervisor  0
        |> spawn system "Supervisor"

    // Worker Instatitaion
    let workerRef =
        worker gatorID SupervisorRef
        |> spawnOpt system "worker"
        <| [SpawnOption.Router(RoundRobinPool(noOfWorkers))]
                
    // Router Instantiation
    let RouterActorRef =
        RouterActor noOfZero noOfWorkers gatorID workerRef
        |> spawn system "RouterActor"

    // Code Runtime stats
    let procStat = Process.GetCurrentProcess()
    let cpuTimeStamp = procStat.TotalProcessorTime
    let timer = new Stopwatch()
    timer.Start()

    try
        RouterActorRef <! "ServerInitiation"
        System.Console.ReadLine() |> ignore
    finally
        let cpuTime = (procStat.TotalProcessorTime - cpuTimeStamp).TotalMilliseconds
        printfn "CPU time = %dms" (int64 cpuTime)
        printfn "Real time = %dms" timer.ElapsedMilliseconds
        printfn "Ratio = %f" (cpuTime / float(timer.ElapsedMilliseconds))

    0