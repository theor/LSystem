namespace LSys.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open LSystem

module ``Test scancount`` =
    open LSystem.PythTree
    [<Test>]
    let testCount() =
        let sys = PythTree.system()
        let s = LSystem.HigherStepper(sys.rules)
        let count = s.count([|I;LB;O;RB;O|])
        printfn "    count: %A" count
        let scanScount = s.scanCount count
        printfn "scancount: %A" scanScount
        1 |> should equal 1

    let pythSys = PythTree.system()
    let pythStepper = LSystem.HigherStepper(pythSys.rules)

    [<Property(Verbose=false)>]
    let ``sum count[0:-2] = scanCount[-1]`` (xs:array<PythTree.T>) =
        let count = pythStepper.count xs
        let scanCount = pythStepper.scanCount count
        (not <| Array.isEmpty count) ==> (lazy (count |> Array.take(Array.length count - 1)|>Array.sum = scanCount.[Array.length scanCount - 1]))
        
    [<Property(Verbose=false)>]
    let ``count and scanCount have the same length`` (xs:array<PythTree.T>) =
        let count = pythStepper.count xs
        let scanCount = pythStepper.scanCount count
        Array.length count = Array.length scanCount
        
    let parPythStepper = LSystem.ParallelStepper(pythSys.rules)
    [<Property(Verbose=false)>]
    let ``parallel stepper count works`` (xs:array<PythTree.T>) =
        let count = pythStepper.count xs
        let parCount = parPythStepper.count xs
        count = parCount   
    [<Property(Verbose=false)>]
    let ``parallel stepper scanCount works`` (xs:array<PythTree.T>) =
        let count = pythStepper.count xs
        let scanCount = pythStepper.scanCount count
        let parScanCount = parPythStepper.scanCount count
        scanCount = parScanCount

