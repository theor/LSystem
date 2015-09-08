module Stepper

open FSharp.Reflection
open LSystem

type CountCache<'a when 'a:equality and 'a:comparison>(rules:Rules<'a>) =
    let _tagr = FSharpValue.PreComputeUnionTagReader typeof<'a>
    member x.tagr (t:'a) = _tagr t
    member val cache =
        rules |> Map.toArray |> Array.map (fun (k,v) -> _tagr k,Array.length v) |> Map.ofArray
let scanCount(count:int[]) =
    if Array.isEmpty count then Array.empty,0
    else
        let sums = seq { 0..Array.length count - 1 } |> Seq.scan (fun sum i -> sum+count.[i]) 0 |> Seq.toArray
        let counts = sums |> Array.take (Array.length count)
        let total = sums.[Array.length count]
        counts,total
//        count |> Array.take (Array.length count - 1) |> Array.scan (fun s x -> s+x) 0

type HigherStepper<'a when 'a:equality and 'a:comparison>(rules:Rules<'a>) =
    let countCache = CountCache<Symbol<'a>>(rules)

    member x.count(axiom) =
        axiom |> Array.map (fun x -> match Map.tryFind (countCache.tagr x) countCache.cache with
                                     | None -> 1
                                     | Some v -> v)
    member x.step(axiom) =
        let counts = x.count axiom
        let scanCounts,total = scanCount counts
        let mutable res = Array.zeroCreate total
        let set i offset =
            let succ = LSystem.matchRules rules (axiom.[i])
            succ |> Array.iteri (fun j sym -> Array.set res (offset+j) sym)
        scanCounts |> Array.iteri set
        res
              
#nowarn "40"
module Messages =
    type Count = {offset:int; length:int}
    type ParMessage<'a when 'a:equality and 'a:comparison> =
    | Count of Count * Axiom<'a> * AsyncReplyChannel<int[]>

type ActorStepper<'a when 'a:equality and 'a:comparison>(rules:Rules<'a>, n:int) =
    let countCache = CountCache<Symbol<'a>>(rules)

    let createActor(i) =
        MailboxProcessor.Start(fun inbox ->
            let rec loop = async {
                let! msg = inbox.Receive()
                match msg with
                | Messages.Count(c,axiom, reply) ->
                    let diff = c.offset + c.length - Array.length axiom
                    let len = if diff <= 0 then c.length else c.length - diff
                    let counts = Array.sub axiom c.offset len
                                 |> Array.map (fun x -> match Map.tryFind (countCache.tagr x) countCache.cache with
                                                        | None -> 1
                                                        | Some v -> v)
                    reply.Reply counts
                return! loop
            }
            loop)

    let actors = Array.init n createActor

    member x.count(axiom:Axiom<'a>) =
        let m = (1 + Array.length axiom) / n
        let actorCount i rep = Messages.Count({offset=i*m;length=m}, axiom, rep)
        actors |> Array.mapi (fun i a -> a.PostAndAsyncReply(actorCount i)) |> Async.Parallel |> Async.RunSynchronously |> Array.concat
    member x.doStep(scanCounts,total,axiom:Axiom<'a>) =
        let mutable res = Array.zeroCreate total
        let set i offset =
            let succ = LSystem.matchRules rules (axiom.[i])
            succ |> Array.iteri (fun j sym -> Array.set res (offset+j) sym)
        scanCounts |> Array.iteri set
        res
    member x.step(axiom) =
        let counts = x.count axiom
        let scanCounts,total = scanCount counts
        x.doStep(scanCounts,total,axiom)
 
              