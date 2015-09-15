module FrameMonitor
open System.Diagnostics

type FrameMonitor() =
        
    let fSampleInterval = 0.5
    let mutable fTotalFrames = 0
    let mutable fTotalTime = 1.0
    let mutable fFrames = 0
    let fTiming = Stopwatch.StartNew()

    member x.Tick() =
        fFrames <- fFrames + 1
        if fTiming.Elapsed.TotalSeconds > fSampleInterval then
            fTotalTime <- fTiming.Elapsed.TotalSeconds
            fTotalFrames <- fFrames
            fFrames <- 0
            fTiming.Restart()
    member x.FPS = float fTotalFrames / fTotalTime