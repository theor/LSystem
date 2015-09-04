namespace LSys.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open LSystem
open Stepper

module ``Test scancount`` =
    open LSystem.PythTree
    [<Test>]
    let testCount() =
        let sys = PythTree.system()
        let s = HigherStepper(sys.rules)
        let ps = ActorStepper(sys.rules, 2)
        let count = s.count([|I;LB;O;RB;O|])
        let pcount = ps.count([|I;LB;O;RB;O|])
        printfn "    count: %A" count
        printfn "   pcount: %A" pcount
        let scanScount = Stepper.scanCount count
        printfn "scancount: %A" scanScount
        1 |> should equal 1

    let pythSys = PythTree.system()
    let pythStepper = HigherStepper(pythSys.rules)

    [<Property(Verbose=false)>]
    let ``sum count[0:-2] = scanCount[-1]`` (xs:NonEmptyArray<PythTree.T>) =
        let count = pythStepper.count xs.Get
        let scanCount,total = Stepper.scanCount count
        lazy (count |> Array.take(Array.length count - 1)|> Array.sum = scanCount.[Array.length scanCount - 1])
        
    [<Property(Verbose=false)>]
    let ``count and scanCount have the same length`` (xs:NonEmptyArray<PythTree.T>) =
        let count = pythStepper.count xs.Get
        let scanCount,total = Stepper.scanCount count
        Array.length count = Array.length scanCount
        
    let parPythStepper = ActorStepper(pythSys.rules, 2)

    [<Property(Verbose=false)>]
    let ``parallel stepper count works`` (xs:NonEmptyArray<PythTree.T>) =
        let count = pythStepper.count xs.Get
        let parCount = parPythStepper.count xs.Get
        count = parCount   


