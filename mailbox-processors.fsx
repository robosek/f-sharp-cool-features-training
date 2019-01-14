let printerAgent = MailboxProcessor.Start(fun inbox ->

    let rec messageLoop() = async {
        let! msg = inbox.Receive()
        printfn "Message is: %s" msg

        return! messageLoop()
    }

    messageLoop()
)

printerAgent.Post "Hello"
printerAgent.Post "Hello again"
printerAgent.Post "And again, Hello!"


open System
open System.Threading

type Utility() = 
    static let rand = new Random()

    static member RandomSleep() = 
        let ms = rand.Next(1,10)
        Thread.Sleep(ms)

type MessageBasedCounter() = 
    static let updateState (count,sum) msg = 
        let newSum = sum + msg
        let newCount = count + 1
        printfn "Count is: %i. Sum is: %i" newCount newSum

        Utility.RandomSleep()

        (newCount, newSum)

    static let agent = MailboxProcessor.Start(fun inbox -> 

        let rec messageLoop oldState = async {
            let! msg = inbox.Receive()
            inbox.PostAndReply

            let newState = updateState oldState msg

            return! messageLoop newState
        }

        messageLoop(0,0)
    )

    static member Add i = agent.Post i

MessageBasedCounter.Add 1
MessageBasedCounter.Add 2
MessageBasedCounter.Add 5

let makeCountingTask addFunction taskId  = async {
    let name = sprintf "Task%i" taskId
    for i in [1..3] do 
        addFunction i
    }

let messageExample5 = 
    [1..5]
        |> List.map (fun i -> makeCountingTask MessageBasedCounter.Add i)
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore