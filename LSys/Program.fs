let duration f =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let returnValue = f()
    sw.Stop()
    printfn "elapsed: %ims" sw.ElapsedMilliseconds
    returnValue

// http://peterwonka.net/Publications/pdfs/2009.VMV.Lipp.ParallelGenerationOfLSystems.final.pdf

type Symbol<'a> = 'a
type Rules<'a> = Symbol<'a> -> Symbol<'a> seq
type LSystem<'a> =
    { axiom: Symbol<'a> seq
      rules: Rules<'a> }
let step rules axiom =
    Seq.collect rules axiom

//module Algea =
//    type T = A | B
//    let system() = { axiom = [A]
//                     rules = function | A -> [A;B] | B -> [A] }
//module AlgeaGen =
//    type T = Sym of string
//    let system() = { axiom = [Sym "A"]
//                     rules = function | Sym "A" -> [Sym "A";Sym "B"] | Sym "B" -> [Sym "A"] }
//module PythTree =
//    type T = O | I | LB | RB
//    with
//        override x.ToString() =
//            match x with
//            | LB -> "["
//            | RB -> "]"
//            | O -> "0"
//            | I -> "1"
//    let system() = { axiom = [O]
//                     rules = function | I -> [I;I] | O -> [I;LB;O;RB;O] | n -> [n] }
//     
//module KochCurve =
//    type T = F | Add | Sub
//        with override x.ToString() =
//                match x with | F -> "F" | Add -> "+" | Sub -> "-"
//    let system() = { axiom = [F]
//                     rules = function | F -> [F;Add;F;Sub;F;Sub;F;Add;F] | n -> [n] }
module CantorTree = // 15: 3900 ms
    type T = A | B
        with override x.ToString() =
                match x with | A -> "A" | B -> "B"
    let system() = { axiom = [A]
                     rules = function | A -> seq{ yield A; yield B; yield A } | B -> seq { yield B; yield B; yield B } }

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open System.Windows.Shapes

let runApp f =
    let app = Application()
    let w = Window()
    let canvas = Canvas(Width=float 800,Height=float 800)
    w.Content <- canvas
    f canvas
    app.Run(window = w)

let test_sys sys n = 
    let folder rules axiom i =
        printfn "%i" i
//        printfn "%i %A" i (sys.axiom |> List.map (sprintf "%A") |> String.concat "")
        step rules axiom
    let final = [1..n] |> Seq.fold (folder sys.rules) sys.axiom
    final |> Seq.map string |> List.ofSeq |> printfn "%A"
//    algea() |> test_sys
let folder (canvas:Canvas) rules axiom i =
    let y = (float i)*15.0
    let w = 800.0 / float (Seq.length axiom)
    axiom |> Seq.iteri (fun j s ->
        if s = CantorTree.A then
            let l = Line(X1=w*float j,Y1=y,X2=w*float j+w,Y2=y,Stroke=Brushes.Black,StrokeThickness=10.0)
            canvas.Children.Add l |> ignore)
    step rules axiom

[<EntryPoint;STAThread>]
let main argv = 
    duration (fun () -> test_sys (CantorTree.system()) 15)
    let sys = CantorTree.system()
    runApp (fun canvas -> [1..15] |> Seq.fold (folder canvas sys.rules) sys.axiom |> ignore)
