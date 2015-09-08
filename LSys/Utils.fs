﻿[<AutoOpen>]
module Utils

let duration f =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let returnValue = f()
    sw.Stop()
//    printfn "elapsed: %ims" sw.ElapsedMilliseconds
    returnValue,sw.Elapsed.TotalMilliseconds
let durationp f =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let returnValue = f()
    sw.Stop()
    printfn "elapsed: %ims" sw.ElapsedMilliseconds
    returnValue