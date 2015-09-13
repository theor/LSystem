
    
open System
open System.Drawing
open LSystem

// http://peterwonka.net/Publications/pdfs/2009.VMV.Lipp.ParallelGenerationOfLSystems.final.pdf

let test_sys<'a when 'a:equality and 'a:comparison> (sys:LSystem<'a>) n = 
    let folder rules axiom i =
        printfn "%i" i
//        printfn "%i %A" i (sys.axiom |> List.map (sprintf "%A") |> String.concat "")
        step rules axiom
    let final = [1..n] |> Seq.fold (folder sys.rules) sys.axiom
    final |> Array.map (sprintf "%O")|> printfn "%O"
//    algea() |> test_sys
//let folder rules (axiom,lines) i =
//    let y = (float32 i)*15.0f
//    let w = 800.0f / float32 (Seq.length axiom)
//    let tuples = axiom |> Array.mapi (fun j s ->
//        if s = Cantor.A then
//            let l1 = PointF(w*float32 j,y)
//            let l2 = PointF(w*float32 j+w,y)//,Stroke=Brushes.Black,StrokeThickness=40.0)
//            [|l1;l2|]
//        else [||]) |> Array.collect id
//    (step rules axiom),(Array.concat [ lines; tuples ])



let folderPyth axiom =
    let totalVertices = axiom |> Array.sumBy (function | PythTree.O | PythTree.I -> 2 | _ -> 0)
    let lines = Array.zeroCreate totalVertices
    let factor = 0.005f
    let L,LL = factor*5.0f,factor*8.0f
    let state = [PythTree.init 400.0f 300.0f]
    let folder (stateStack:PythTree.State list,vertexCount) (s:PythTree.T) = 
//        (printfn "\nINSTR: %A" s)
//        stateStack |> List.iter (printfn "%A")
        match stateStack with
        | state :: t ->
            match s with
            | PythTree.O ->
                let newPos = PointF(state.pos.X + float32 (Math.Cos(float state.angle*Math.PI/180.0))*L,
                                    state.pos.Y + float32 (Math.Sin(float state.angle*Math.PI/180.0))*L)
                lines.[vertexCount] <- state.pos
                lines.[vertexCount+1] <- newPos
                ({state with pos = newPos} :: t, (vertexCount+2))
            | PythTree.I -> 
                let newPos = PointF(state.pos.X + float32 (Math.Cos(float state.angle*Math.PI/180.0))*LL,
                                    state.pos.Y + float32 (Math.Sin(float state.angle*Math.PI/180.0))*LL)
                lines.[vertexCount] <- state.pos
                lines.[vertexCount+1] <- newPos
                ({state with pos = newPos} :: t, (vertexCount+2))
            | PythTree.LB ->
                let newState = { state with angle = state.angle + 45 }
                (newState::stateStack,vertexCount)
            | PythTree.RB ->
                match t with
                | h :: t ->
                    let newState = { h with angle = h.angle - 45 }    
                    (newState::t,vertexCount)
                | _ -> (stateStack,vertexCount)
        | _ -> failwith "should not happen"

    let _finalState,finalvertexCount = axiom |> Array.fold folder (state,0)
    lines


open PythTree

[<EntryPoint;STAThread>]
let main argv = 
    let make(n:int) =
        let sys = PythTree.system()
        let naive = LSystem.step sys.rules
//        let stepper = Stepper.HigherStepper(sys.rules)
//        let pstepper = Stepper.ActorStepper(sys.rules, 4)
        let finalDerivation = [1..n] |> Seq.fold (fun state _ -> naive state) sys.axiom
        folderPyth finalDerivation //[|I;LB;O;RB;O|] //
    Renderer2.run()
//    Renderer.run(make)
    0