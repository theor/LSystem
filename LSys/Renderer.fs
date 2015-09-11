module Renderer


open System.Drawing
open System.Windows.Forms
open SlimDX
open SlimDX.DXGI
open SlimDX.Direct3D11
open SlimDX.Direct2D
open SlimDX.Windows

type RenderState = { mutable n : int
                     mutable redraw : bool }


let run(makeLines:int -> PointF[]) =
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

    let mutable renderState = { n = 5; redraw = true }
    let needRedraw() = renderState.redraw <- true
    form.KeyDown.Add (fun e -> 
        match e.KeyCode with
        | Keys.Add -> renderState.n <- renderState.n + 1; needRedraw()
        | Keys.Subtract -> if renderState.n > 0 then renderState.n <- renderState.n - 1; needRedraw()
        | _ -> ())

    let mutable lines = durationp (fun () -> makeLines renderState.n)
   
//    let _,lines,_ = [1..10] |> Seq.fold f (sys.axiom,[||],[LSys.PythTree.init 400.0f 300.0f])
//    let lines = [|Point(0,0); Point(800,100)|]

    let mainLoop () =
        renderTarget.BeginDraw()
        renderTarget.Transform <- Matrix3x2.Identity
        renderTarget.Clear(Color4(Color.Black))

        if renderState.redraw then
            lines <- durationp (fun () -> makeLines renderState.n)
            renderState.redraw <- false

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