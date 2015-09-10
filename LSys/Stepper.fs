module Stepper

open FSharp.Reflection
open LSystem
let scanCount(count:int[]) =
    if Array.isEmpty count then Array.empty,0
    else
        let sums = seq { 0..Array.length count - 1 } |> Seq.scan (fun sum i -> sum+count.[i]) 0 |> Seq.toArray
        let counts = sums |> Array.take (Array.length count)
        let total = sums.[Array.length count]
        counts,total

type HigherStepper<'a when 'a:equality and 'a:comparison>(rules:Rules<'a>,counter:Counter<'a>) =
    member x.count(axiom) = axiom |> Array.map counter
    member x.step(axiom) =
        let counts = x.count axiom
        let scanCounts,total = scanCount counts
        let mutable res = Array.zeroCreate total
        let set i offset =
            let succ = rules (axiom.[i])
            succ |> Array.iteri (fun j sym -> Array.set res (offset+j) sym)
        scanCounts |> Array.iteri set
        res
 type HigherStepper2<'a when 'a:equality and 'a:comparison>(rules:Rules<'a>,counter:Counter<'a>,n:int) =
    let log format = Printf.kprintf (fun _ -> ()) format
//    let log format = Printf.printfn format
    member val n = n
    member x.count(axiom) =
        let l = Array.length axiom
        log "axiom %A %i" axiom l
        let m = (l + 1) / n
        log "    chunk size %i" m

        let res = (Array.zeroCreate n)
        for i = 0 to n - 1 do
            let delta = l - (i+1)*m
            let chunkLength = if delta < 0 then m + delta else m
            let start = i*m
            log "    chunk @%i:%i" start chunkLength
            let count = Array.sub axiom start chunkLength |> Array.sumBy counter
            Array.set res i count
            ()
        log "        %A" res
        res
    member x.step(axiom) =
        let counts = x.count axiom
        let scanCounts,total = scanCount counts
        let res = Array.zeroCreate total
        let set i offset =
            let succ = rules (axiom.[i])
            succ |> Array.iteri (fun j sym -> Array.set res (offset+j) sym)
        scanCounts |> Array.iteri set
        res       
#nowarn "40"
module Messages =
    type Count = {offset:int; length:int}
    type ParMessage<'a when 'a:equality and 'a:comparison> =
    | Count of Count * Axiom<'a> * AsyncReplyChannel<int[]>
    | Step of  Count * Axiom<'a> * AsyncReplyChannel<'a[]>
    | StepRef of Count * Axiom<'a> * int[] * 'a[] ref * AsyncReplyChannel<bool>

type ActorStepper<'a when 'a:equality and 'a:comparison>(rules:Rules<'a>,counter:Counter<'a>, n:int) =
    let createActor(i) =
        MailboxProcessor.Start(fun inbox ->
            let rec loop = async {
                let! msg = inbox.Receive()
                match msg with
                | Messages.Count(c,axiom, reply) ->
                    let diff = c.offset + c.length - Array.length axiom
                    let len = if diff <= 0 then c.length else c.length - diff
                    let counts = Array.sub axiom c.offset len
                                 |> Array.map counter
                    reply.Reply counts
                | Messages.Step(c,axiom,reply) ->
                    let diff = c.offset + c.length - Array.length axiom
                    let len = if diff <= 0 then c.length else c.length - diff
                    let stepped = Array.sub axiom c.offset len
                                 |> Array.collect rules
                    reply.Reply stepped
                | Messages.StepRef(c,axiom:Axiom<'a>,scanCounts,res,reply) ->
                    let diff = c.offset + c.length - Array.length axiom
                    let len = if diff <= 0 then c.length else c.length - diff
                    let sc = Array.sub scanCounts c.offset len
                    let set i offset =
                        let succ = rules (axiom.[c.offset+i])
                        for j = 0 to -1+Array.length succ do
                            Array.set !res (offset+j) (succ.[j])
//                        succ |> Array.iteri (fun j sym -> Array.set !res (offset+j) sym)
//                    for isc = 0 to len-1 do
//                        set isc scanCounts.[c.offset + isc]
//                    sc |> Array.iteri set
                    reply.Reply true
                    
                return! loop
            }
            loop)

    let actors = Array.init n createActor
    
    member x.count(axiom:Axiom<'a>) =
        let m = (1 + Array.length axiom) / n
        let actorCount i rep = Messages.Count({offset=i*m;length=m}, axiom, rep)
        actors |> Array.mapi (fun i a -> a.PostAndAsyncReply(actorCount i)) |> Async.Parallel |> Async.RunSynchronously |> Array.concat
    member x.doStep(scanCounts,total,axiom:Axiom<'a>) =
        let m = (1 + Array.length axiom) / n
        let actorStep i rep = Messages.Step({offset=i*m;length=m}, axiom, rep)
        actors |> Array.mapi (fun i a -> a.PostAndAsyncReply(actorStep i)) |> Async.Parallel |> Async.RunSynchronously |> Array.concat
    member x.doStepRef(scanCounts,total,axiom:Axiom<'a>) =
        let m = (1 + Array.length axiom) / n
        let res = ref (Array.zeroCreate total)
        let actorStep i rep = Messages.StepRef({offset=i*m;length=m}, axiom, scanCounts, res, rep)
        
        actors |> Array.mapi (fun i a -> a.PostAndAsyncReply(actorStep i)) |> Async.Parallel |> Async.RunSynchronously   |> ignore
        !res
//        let mutable res = Array.zeroCreate total
//        let set i offset =
//            let succ = rules (axiom.[i])
//            succ |> Array.iteri (fun j sym -> Array.set res (offset+j) sym)
//        scanCounts |> Array.iteri set
//        res
    member x.step(axiom) =
        let counts = x.count axiom
        let scanCounts,total = scanCount counts
        x.doStepRef(scanCounts,total,axiom)
        
              