﻿namespace LSys.Tests

open NUnit.Framework
open FsUnit
open FsCheck
open FsCheck.NUnit
open LSystem
open Stepper
open LSystem.PythTree

module ``Higher Stepper 2`` =
    let pythSys = PythTree.system()
    let pythStepper2 = HigherStepper2(pythSys.rules, pythSys.count,1)
    let pythStepper22 = HigherStepper2(pythSys.rules, pythSys.count,2)
    
    [<Property(Verbose=false)>]
    let ``count is split in 1 chunks`` (xs:NonEmptyArray<PythTree.T>) =
        let c = pythStepper2.count xs.Get
        Array.length c = pythStepper2.n
    [<Property(Verbose=false)>]
    let ``count is split in 2 chunks`` (xs:NonEmptyArray<PythTree.T>) =
        let c = pythStepper22.count xs.Get
        Array.length c = pythStepper22.n


    [<Property(Verbose=false)>]
    let ``step2`` (xs:NonEmptyArray<PythTree.T>) =
        let basicStepped,basictime = duration (fun () -> LSystem.step pythSys.rules xs.Get)
        let higherStepped,highertime = duration (fun () -> pythStepper2.step xs.Get)
        basicStepped = higherStepped |> Prop.collect (basictime > highertime)
module ``Test scancount`` =

    [<Test>]
    let testCount() =
        let sys = PythTree.system()
        let s = HigherStepper(sys.rules, sys.count)
        let ps = ActorStepper(sys.rules, sys.count, 2)
        let s0 = [|I;LB;O;RB;O|]
        let count = s.count s0
        let pcount = ps.count s0
        printfn "    count: %A" count
        printfn "   pcount: %A" pcount
        let scanScount = Stepper.scanCount count
        printfn "scancount: %A" scanScount
        
        let s1 = LSystem.step sys.rules s0
        let s1' = ps.step s0
        printfn "     step: %A" s1
        printfn "  parstep: %A" s1'
        s1' |> should equal s1

    let pythSys = PythTree.system()
    let pythStepper = HigherStepper(pythSys.rules, pythSys.count)

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
        
    let parPythStepper = ActorStepper(pythSys.rules, pythSys.count, 2)

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

module Timing =
    module Avg =
        let avg axioms f = axioms |> Seq.averageBy (fun axiom ->
            let res,time = duration (fun () -> f axiom)
            time)
        let compare title axioms cases =
            printfn "%s:" title
            cases |> List.iter (fun (label,f) -> avg axioms f |> printfn "%s: %fms" label)
    let pythSys = PythTree.system()
    let pythStepper = HigherStepper(pythSys.rules, pythSys.count)
    let pythStepper2 = HigherStepper2(pythSys.rules, pythSys.count,1)
    let parPythStepper = ActorStepper(pythSys.rules, pythSys.count, 2)
    let parPythStepper4 = ActorStepper(pythSys.rules, pythSys.count, 8)

    let countNElements n size =
        let axiomGen = Gen.arrayOfLength n (Gen.arrayOfLength size Arb.generate<PythTree.T>)
        let axioms = Gen.eval 100 (Random.mkStdGen 42L) axiomGen
        Avg.compare (sprintf "Step 100 %i-symbols" size) axioms [
                                                                 ("higher", pythStepper.count)
                                                                 ("highe2", pythStepper2.count)
                                                                 ("   par", parPythStepper.count)
                                                                 ("  par4", parPythStepper4.count)]
    let scancountNElements n size =
        let axiomGen = Gen.arrayOfLength n (Gen.arrayOfLength size Arb.generate<PythTree.T>)
        let axioms = Gen.eval 100 (Random.mkStdGen 42L) axiomGen |> Array.map parPythStepper4.count
        Avg.compare (sprintf "Step 100 %i-symbols" size) axioms [("higher", Stepper.scanCount)]
        

    let stepNElements n testcases size =
        let axiomGen = Gen.arrayOfLength n (Gen.arrayOfLength size Arb.generate<PythTree.T>)
        let axioms = Gen.eval 100 (Random.mkStdGen 42L) axiomGen
        Avg.compare (sprintf "Step 100 %i-symbols" size) axioms testcases

    let [<Test>] ``count 100 elements``() =
        [100;1000;10000] |> List.iter (countNElements 100)
    let [<Test>] ``scancount 100 elements``() =
        [100;1000;10000] |> List.iter (scancountNElements 100)
    let [<Test>] ``step 100 elements``() =
        let testcases = [(" naive", LSystem.step pythSys.rules)
                         ("higher", pythStepper.step)
                         ("   par", parPythStepper.step)
                         ("  par8", parPythStepper4.step)]
        [100;1000;10000] |> List.iter (stepNElements 100 testcases) 

    let axiomSizes,axiomNumber = [10;100;1000;10000],100
    let [<Test>] ``step 100 elements naive``() =
        axiomSizes |> List.iter (stepNElements axiomNumber [(" naive", LSystem.step pythSys.rules)]) 
    let [<Test>] ``step 100 elements higher``() =
        axiomSizes |> List.iter (stepNElements axiomNumber [("higher", pythStepper.step)])
    let [<Test>] ``step 100 elements higher2``() =
        axiomSizes |> List.iter (stepNElements axiomNumber [("highe2", pythStepper2.step)])
    let [<Test>] ``step 100 elements par``() =
        axiomSizes |> List.iter (stepNElements axiomNumber [("   par", parPythStepper.step)]) 
    let [<Test>] ``step 100 elements par8``() =
        axiomSizes |> List.iter (stepNElements axiomNumber [("  par8", parPythStepper4.step)]) 



