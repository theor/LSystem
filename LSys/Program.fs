let duration f =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let returnValue = f()
    sw.Stop()
    printfn "elapsed: %ims" sw.ElapsedMilliseconds
    returnValue
    
open System
open System.Drawing

type Symbol<'a> = 'a

module LSys =
    type Rule<'a when 'a:equality> = Symbol<'a> * (Symbol<'a> array)
    type LSystem<'a when 'a:equality> =
        { axiom: Symbol<'a> array
          rules: Rule<'a> list }
    let inline (%>) pred succ = (pred, succ)
    let matchRules rules x =
        let matching = rules |> List.tryFind (fun (pred,_) -> pred = x)
        match matching with
        | None -> [|x|]
        | Some (pred,succ) -> succ

    let step rules axiom =
        axiom |> Array.collect (matchRules rules)
    module Algea =
        type T = A | B
        let system() = { axiom = [| A |]
                         rules = [ A %> [|A;B|]
                                   B %> [|A|] ] }
    module Cantor =
        type T = A | B
        let system() = { axiom = [| A |]
                         rules = [ A %> [|A;B;A|]
                                   B %> [|B;B;B|] ] }
    module PythTree =
        type State = {pos:PointF; angle:int}
        let init x y = {pos=PointF(x,y); angle=0}
        type T = O | I | LB | RB
        let system() = { axiom = [| O |]
                         rules = [ O %> [|I;LB;O;RB;O|]
                                   I %> [|I;I|] ] }

// http://peterwonka.net/Publications/pdfs/2009.VMV.Lipp.ParallelGenerationOfLSystems.final.pdf

//type Rules<'a> = Symbol<'a> -> Symbol<'a> seq
//type LSystem<'a> =
//    { axiom: Symbol<'a> seq
//      rules: Rules<'a> }
//let step rules axiom =
//    Seq.collect rules axiom

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
//module CantorTree = // 15: 3900 ms
//    type T = A | B
//        with override x.ToString() =
//                match x with | A -> "A" | B -> "B"
//    let system() = { axiom = [A]
//                     rules = function | A -> seq{ yield A; yield B; yield A } | B -> seq { yield B; yield B; yield B } }

open System
//open System.Windows
//open System.Windows.Controls
//open System.Windows.Media
//open System.Windows.Shapes

//let runApp f =
//    let app = Application()
//    let w = Window()
//    let canvas = Canvas()//(Width=float 800,Height=float 800)
//    canvas.SetValue(DockPanel.LastChildFillProperty, true)
//    canvas.Background <- Brushes.Beige
//    w.Content <- canvas
//    w.Loaded.Add (fun _ ->f canvas)
//    app.Run(window = w)

let test_sys<'a when 'a:equality> (sys:LSys.LSystem<'a>) n = 
    let folder rules axiom i =
        printfn "%i" i
//        printfn "%i %A" i (sys.axiom |> List.map (sprintf "%A") |> String.concat "")
        LSys.step rules axiom
    let final = [1..n] |> Seq.fold (folder sys.rules) sys.axiom
    final |> Array.map (sprintf "%O")|> printfn "%O"
//    algea() |> test_sys
let folder rules (axiom,lines) i =
    let y = (float32 i)*15.0f
    let w = 800.0f / float32 (Seq.length axiom)
    let tuples = axiom |> Array.mapi (fun j s ->
        if s = LSys.Cantor.A then
            let l1 = PointF(w*float32 j,y)
            let l2 = PointF(w*float32 j+w,y)//,Stroke=Brushes.Black,StrokeThickness=40.0)
            [|l1;l2|]
        else [||]) |> Array.collect id
    (LSys.step rules axiom),(Array.concat [ lines; tuples ])



let folderPyth axiom =
    let factor = 0.005f
    let L,LL = factor*5.0f,factor*8.0f
    let state = [LSys.PythTree.init 400.0f 300.0f]
    let folder (stateStack:LSys.PythTree.State list,lines) (s:LSys.PythTree.T) = 
//        (printfn "\nINSTR: %A" s)
//        stateStack |> List.iter (printfn "%A")
        match stateStack with
        | state :: t ->
            match s with
            | LSys.PythTree.O ->
                let newPos = PointF(state.pos.X + float32 (Math.Cos(float state.angle*Math.PI/180.0))*L,
                                    state.pos.Y + float32 (Math.Sin(float state.angle*Math.PI/180.0))*L)
                ({state with pos = newPos} :: t,
                 Array.concat([lines; [|state.pos; newPos|] ]))
            | LSys.PythTree.I -> 
                let newPos = PointF(state.pos.X + float32 (Math.Cos(float state.angle*Math.PI/180.0))*LL,
                                    state.pos.Y + float32 (Math.Sin(float state.angle*Math.PI/180.0))*LL)
                ({state with pos = newPos} :: t,
                 Array.concat([lines; [|state.pos; newPos|] ]))
            | LSys.PythTree.LB ->
                let newState = { state with angle = state.angle + 45 }
                (newState::stateStack,lines)
            | LSys.PythTree.RB ->
                match t with
                | h :: t ->
                    let newState = { h with angle = h.angle - 45 }    
                    (newState::t,lines)
                | _ -> (stateStack,lines)

//        (stateStack,Array.empty)
    let _finalState,lines = axiom |> Array.fold folder (state,Array.empty)
    lines
//    let y = (float32 i)*15.0f
//    let w = 800.0f / float32 (Seq.length axiom)
//    let state = List.head stateStack
//    let lastState,lines = axiom |> Array.fold (fun (stateStack,lines) (s:LSys.PythTree.T) ->
//        match s with
//        | LSys.PythTree.O ->
//            let newPos = PointF(state.pos.X + float32 (Math.Cos(state.angle))*10.0f,state.pos.Y + float32 (Math.Sin(state.angle))*10.0f)
//            ({state with pos = newPos} :: stateStack, Array.concat([lines; [|state.pos; newPos|] ]))
//        | LSys.PythTree.I -> 
//            let newPos = PointF(state.pos.X + float32 (Math.Cos(state.angle))*10.0f,state.pos.Y + float32 (Math.Sin(state.angle))*20.0f)
//            ({state with pos = newPos} :: stateStack, Array.concat([lines; [|state.pos; newPos|] ]))
//        | LSys.PythTree.LB -> (stateStack,lines)//[||]
//        | LSys.PythTree.RB -> (stateStack,lines) ) (stateStack,lines)
//       
//    ((LSys.step rules axiom),lines,stateStack)

open System.Windows.Forms
open SlimDX
open SlimDX.DXGI
open SlimDX.Direct3D11
open SlimDX.Direct2D
open SlimDX.Windows
open LSys.PythTree

[<EntryPoint;STAThread>]
let main argv = 
    Application.EnableVisualStyles();
    Application.SetCompatibleTextRenderingDefault(false);
    let form = new RenderForm("LSys - Q/A: angleGrowth - W/S: initAngle")
    let swapChainDescription = new SwapChainDescription(BufferCount = 2,
                                                        Usage = Usage.RenderTargetOutput,
                                                        OutputHandle = form.Handle,
                                                        IsWindowed = true,
                                                        ModeDescription = new ModeDescription(0, 0, new Rational(60, 1), Format.R8G8B8A8_UNorm),
                                                        SampleDescription = new SampleDescription(1, 0),
                                                        Flags = SwapChainFlags.AllowModeSwitch,
                                                        SwapEffect = SwapEffect.Discard)
    let (r,device,swapChain) = SlimDX.Direct3D11.Device.CreateWithSwapChain(DriverType.Hardware,
                                                                            DeviceCreationFlags.BgraSupport,
                                                                            swapChainDescription)
    let backBuffer = Surface.FromSwapChain(swapChain, 0)
    use factory = new SlimDX.Direct2D.Factory()
    let dpi = factory.DesktopDpi
    let renderTarget =
        RenderTarget.FromDXGI(factory,
                              backBuffer,
                              new RenderTargetProperties(
                                  HorizontalDpi = dpi.Width,
                                  VerticalDpi = dpi.Height,
                             
                                  MinimumFeatureLevel = SlimDX.Direct2D.FeatureLevel.Default,
                                  PixelFormat = new PixelFormat(Format.Unknown, AlphaMode.Ignore),
                                  Type = RenderTargetType.Default,
                                  Usage = RenderTargetUsage.None
                              ));
    use factory= swapChain.GetParent<SlimDX.DXGI.Factory>()
    factory.SetWindowAssociation(form.Handle, WindowAssociationFlags.IgnoreAltEnter) |> ignore
    
    form.Size = new Size(800, 600) |> ignore
    let sys = LSys.PythTree.system()
//    let f =folderPyth sys.rules

    let finalDerivation = [1..13] |> Seq.fold (fun state _ -> LSys.step sys.rules state) sys.axiom //(sys.axiom,[||],[LSys.PythTree.init 400.0f 300.0f])
    let lines = folderPyth finalDerivation //[|I;LB;O;RB;O|] //
   
//    let _,lines,_ = [1..10] |> Seq.fold f (sys.axiom,[||],[LSys.PythTree.init 400.0f 300.0f])
//    let lines = [|Point(0,0); Point(800,100)|]

    let mainLoop () =
        renderTarget.BeginDraw()
        renderTarget.Transform <- Matrix3x2.Identity
        renderTarget.Clear(Color4(Color.Black))

        use brush = new SolidColorBrush(renderTarget, new Color4(Color.White))
                    
        for i in 0 .. 2 .. (Array.length lines) - 1 do
            let a = lines.[i]
            let b = lines.[i + 1]

            renderTarget.DrawLine(brush, float32 a.X, float32 a.Y, float32 b.X, float32 b.Y, 1.1f)

        renderTarget.EndDraw() |> ignore

        swapChain.Present(0, PresentFlags.None) |> ignore

    MessagePump.Run(form, mainLoop)

    renderTarget.Dispose()
    swapChain.Dispose()
    device.Dispose()


//    duration (fun () -> test_sys (LSys.Cantor.system()) 14)
//    runApp (fun canvas -> [1..10] |> Seq.fold (folder canvas sys.rules) sys.axiom |> ignore)
    0