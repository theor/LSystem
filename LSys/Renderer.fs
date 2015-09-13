module Renderer


open System.Drawing
open System.Windows.Forms
open SlimDX
open SlimDX.DXGI
open SlimDX.Direct3D11
open SlimDX.Direct2D
open SlimDX.Windows

type RenderState = { mutable n : int
                     mutable redraw : bool
                     mutable zoom : float32
                     mutable translation : float32*float32 }

module TextRenderer =
    open SlimDX.DirectWrite
    open SpriteTextRenderer.SlimDX
    type TextRenderer(device) =
        let sprite = new SpriteRenderer(device)
        member val textBlock = new TextBlockRenderer(sprite, "Arial", FontWeight.Bold, SlimDX.DirectWrite.FontStyle.Normal, FontStretch.Normal, 16.0f)
        interface System.IDisposable with
            member x.Dispose() =
                x.textBlock.Dispose()
                sprite.Dispose()

let run(makeLines:int -> PointF[]) =
    Application.EnableVisualStyles();
    Application.SetCompatibleTextRenderingDefault(false);
    let form = new RenderForm("LSys - Q/A: angleGrowth - W/S: initAngle")
    let swapChainDescription = new SwapChainDescription(BufferCount = 1,
                                                        Usage = Usage.RenderTargetOutput,
                                                        OutputHandle = form.Handle,
                                                        IsWindowed = true,
                                                        ModeDescription = new ModeDescription(800, 600, new Rational(60, 1), Format.R8G8B8A8_UNorm),
                                                        SampleDescription = new SampleDescription(1, 0),
                                                        Flags = SwapChainFlags.None,
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
    
    let resize w h =
        swapChain.ResizeBuffers(1, w, h, Format.R8G8B8A8_UNorm, SwapChainFlags.None) |> ignore
        use backBuffer = Texture2D.FromSwapChain(swapChain, 0)
        let renderView = new RenderTargetView(device, backBuffer)
        device.ImmediateContext.Rasterizer.SetViewports(new Viewport(0.0f, 0.0f, (float32)w, (float32)h, 0.0f, 1.0f));
        device.ImmediateContext.OutputMerger.SetTargets(renderView);
        renderView

//    let renderView = resize 800 600

//    use txt = new TextRenderer.TextRenderer(device)

    let mutable renderState = { n = 5
                                redraw = true
                                zoom = 1.0f
                                translation = 0.0f,0.0f }
    let needRedraw() = renderState.redraw <- true
    form.MouseWheel.Add (fun e ->
        if e.Delta > 0 then renderState.zoom <- renderState.zoom * 1.2f
        else renderState.zoom <- renderState.zoom * 0.8f)
    form.KeyDown.Add (fun e -> 
        let tr = 100.0f
        let x,y = renderState.translation
        match e.KeyCode with
        | Keys.Left -> renderState.translation <- x - tr/renderState.zoom, y
        | Keys.Right -> renderState.translation <- x + tr/renderState.zoom, y
        | Keys.Up -> renderState.translation <- x,y - tr/renderState.zoom
        | Keys.Down -> renderState.translation <- x,y + tr/renderState.zoom
        | Keys.Add -> renderState.n <- renderState.n + 1; needRedraw()
        | Keys.Subtract -> if renderState.n > 0 then renderState.n <- renderState.n - 1; needRedraw()
        | _ -> ())

    let mutable lines = durationp (fun () -> makeLines renderState.n)
   
//    let _,lines,_ = [1..10] |> Seq.fold f (sys.axiom,[||],[LSys.PythTree.init 400.0f 300.0f])
//    let lines = [|Point(0,0); Point(800,100)|]

    let mainLoop () =
        renderTarget.BeginDraw()
        let x,y = renderState.translation
        renderTarget.Transform <-  Matrix3x2.Scale(renderState.zoom,renderState.zoom)* Matrix3x2.Translation(x,y)
        renderTarget.Clear(Color4(Color.Black))

        if renderState.redraw then
            lines <- durationp (fun () -> makeLines renderState.n)
            renderState.redraw <- false

        use brush = new SolidColorBrush(renderTarget, new Color4(Color.White))
                    
        for i in 0 .. 2 .. (Array.length lines) - 1 do
            let a = lines.[i]
            let b = lines.[i + 1]

            renderTarget.DrawLine(brush, float32 a.X, float32 a.Y, float32 b.X, float32 b.Y, 1.1f)
//        txt.textBlock.DrawString((sprintf "%A" renderState), Vector2(30.0f), Color4(Color.White))
        renderTarget.EndDraw() |> ignore

        swapChain.Present(1, PresentFlags.None) |> ignore

    MessagePump.Run(form, mainLoop)

    renderTarget.Dispose()
    swapChain.Dispose()
    device.Dispose()