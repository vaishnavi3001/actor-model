open Akka.FSharp
open Akka.Remote
open Akka.Routing
open System
open System.Security.Cryptography
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



let system = System.create "BitcoinClient" config


type ClientMessage = ClientTuple of int * string


let router ipAddress noOfWorkers workerRef (mailbox: Actor<'a>) =
    let url = "akka.tcp://BitcoinServer@" + ipAddress + ":8080/user/routeractor"
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
        return! loop()
    }
    loop()

let worker ipAddress (mailbox: Actor<'a>) =
    let url = "akka.tcp://BitcoinServer@" + ipAddress + ":8080/user/Supervisor"
    let SupervisorRef = select url system 
    let rec loop () = actor {
        let! ClientTuple(k, inputString) = mailbox.Receive ()
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

    
[<EntryPoint>]
let main argv =
    let ipAddress = string argv.[0]
    printfn "IP Address: %s" ipAddress

    let noOfWorkers = 10
    printfn "Number of workers: %d" noOfWorkers

    let workerRef =
        worker ipAddress
        |> spawnOpt system "worker"
        <| [SpawnOption.Router(RoundRobinPool(noOfWorkers))]

    let routerRef =
        router ipAddress noOfWorkers workerRef
        |> spawn system "router"

    routerRef <! "GetWorkFromServer"
    Thread.Sleep(60000000);
    0
