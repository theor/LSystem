namespace LSys.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open LSystem
open Stepper

module ``Test scancount`` =
    let duration f =
        let sw = System.Diagnostics.Stopwatch()
        sw.Start()
        let returnValue = f()
        sw.Stop()
    //    printfn "elapsed: %ims" sw.ElapsedMilliseconds
        returnValue,sw.Elapsed.TotalMilliseconds

    open LSystem.PythTree
    [<Test>]
    let testCount() =
        let sys = PythTree.system()
        let s = HigherStepper(sys.rules)
        let ps = ActorStepper(sys.rules, 2)
        let s0 = [|I;LB;O;RB;O|]
        let count = s.count s0
        let pcount = ps.count s0
        printfn "    count: %A" count
        printfn "   pcount: %A" pcount
        let scanScount = Stepper.scanCount count
        printfn "scancount: %A" scanScount
        
        let s1 = LSystem.step sys.rules s0
        let s1' = s.step s0
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

    [<Property(Verbose=false)>]
    let ``step`` (xs:NonEmptyArray<PythTree.T>) =
        let basicStepped,basictime = duration (fun () -> LSystem.step pythSys.rules xs.Get)
        let higherStepped,highertime = duration (fun () -> pythStepper.step xs.Get)
        basicStepped = higherStepped |> Prop.collect (basictime > highertime)
        
    let parPythStepper = ActorStepper(pythSys.rules, 2)

    [<Property(Verbose=false)>]
    let ``parallel stepper count works`` (xs:NonEmptyArray<PythTree.T>) =
        let count = pythStepper.count xs.Get
        let parCount = parPythStepper.count xs.Get
        count = parCount

    [<Property(Verbose=false)>]
    let ``parallel step`` (xs:NonEmptyArray<PythTree.T>) =
        let basicStepped = LSystem.step pythSys.rules xs.Get
        let higherStepped = parPythStepper.step xs.Get
        basicStepped = higherStepped

    // timing
    module Avg =
        let avg axioms f = axioms |> Seq.averageBy (fun axiom ->
            let res,time = duration (fun () -> f axiom)
            time)
        let compare title axioms cases =
            printfn "%s:" title
            cases |> List.iter (fun (label,f) -> avg axioms f |> printfn "%s: %fms" label)
    let stepNElements n size =
        let axiomGen = Gen.arrayOfLength n (Gen.arrayOfLength size Arb.generate<PythTree.T>)
        let axioms = Gen.eval 100 (Random.mkStdGen 42L) axiomGen
        Avg.compare (sprintf "Step 100 %i-symbols" size) axioms [(" naive", LSystem.step pythSys.rules)
                                                                 ("higher", pythStepper.step)
                                                                 ("   par", parPythStepper.step)]

    let [<Test>] ``step 100 elements``() =
        stepNElements 100 100
        stepNElements 100 1000
        stepNElements 100 10000
        stepNElements 100 100000



