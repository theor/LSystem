module Renderer2

open System
open System.Diagnostics
open System.Drawing
open SlimDX
open SlimDX.Direct2D
open SlimDX.Direct3D11
open SlimDX.DirectWrite
open SlimDX.DXGI
open SlimDX.Windows
open SpriteTextRenderer.SlimDX
type Device = SlimDX.Direct3D11.Device
type FontStyle = SlimDX.DirectWrite.FontStyle
type TextAlignment = SpriteTextRenderer.TextAlignment

type RenderState = { mutable n : int
                     mutable redraw : bool
                     mutable zoom : float32
                     mutable translation : float32*float32 }
type IDrawable =
    abstract member render : RenderState -> unit
    abstract member compute : RenderState -> unit

let mainLoop (textBlock:TextBlockRenderer) (rt:RenderTarget) (state:RenderState ref) (d:IDrawable) =
    let x,y = state.Value.translation
    rt.Transform <-  Matrix3x2.Scale(state.Value.zoom,state.Value.zoom)* Matrix3x2.Translation(x,y)
    rt.BeginDraw()
    if state.Value.redraw then
        state.Value.redraw <- false
        d.compute !state
    d.render !state
    rt.EndDraw() |> ignore

    textBlock.DrawString((sprintf "n=%i" state.Value.n), Vector2.Zero,
                         new Color4(1.0f, 1.0f, 1.0f)) |> ignore



let setup2d(swapChain) = 
    let backBuffer = Surface.FromSwapChain(swapChain, 0)
    use factory = new SlimDX.Direct2D.Factory()
    let dpi = factory.DesktopDpi
    try
        RenderTarget.FromDXGI(factory,
                          backBuffer,
                          new RenderTargetProperties(
                              HorizontalDpi = dpi.Width,
                              VerticalDpi = dpi.Height,
                          
                              MinimumFeatureLevel = SlimDX.Direct2D.FeatureLevel.Default,
                              PixelFormat = new PixelFormat(Format.Unknown, AlphaMode.Ignore),
                              Type = RenderTargetType.Default,
                              Usage = RenderTargetUsage.None
                          )) |> Some
    with
    | e -> printfn "%A" e; None

let resize (device:Device) (swapChain:SwapChain) (size:Vector2) =
    swapChain.ResizeBuffers(1, (int size.X), (int size.Y), Format.R8G8B8A8_UNorm, SwapChainFlags.None) |> ignore
    let backBuffer = Texture2D.FromSwapChain<Texture2D>(swapChain, 0);

    let renderView = new RenderTargetView(device, backBuffer);
    backBuffer.Dispose();

    device.ImmediateContext.Rasterizer.SetViewports(new Viewport(0.0f, 0.0f, (float32 size.X), (float32 size.Y), 0.0f, 1.0f));
    device.ImmediateContext.OutputMerger.SetTargets(renderView);

//    if (sprite != nul 
//        sprite.RefreshViewport()
    renderView

//type Input = Mouse of RawInput.MouseInputEventArgs | Keyboard of RawInput.KeyboardInputEventArgs
[<RequireQualifiedAccess>]
module IO =
    open SlimDX.RawInput
    open System.Windows.Forms
    let setup(renderState:RenderState ref) =
        Device.RegisterDevice(SlimDX.Multimedia.UsagePage.Generic, SlimDX.Multimedia.UsageId.Mouse, SlimDX.RawInput.DeviceFlags.None);
        Device.MouseInput.Add (fun e ->
//            printfn "MOUSE %A" e.ButtonFlags
            match e.ButtonFlags with
            | MouseButtonFlags.MouseWheel -> renderState.Value.zoom <-renderState.Value.zoom * (if e.WheelDelta > 0 then 1.2f else 0.8f)
            | _ -> ())
        Device.RegisterDevice(SlimDX.Multimedia.UsagePage.Generic, SlimDX.Multimedia.UsageId.Keyboard, SlimDX.RawInput.DeviceFlags.None);
        Device.KeyboardInput.Add (fun e ->
//            printfn "KEYBOARD %A %A" e.Key e.State
            let x,y = renderState.Value.translation
            let tr = 100.0f
            match e.State, e.Key with
            | KeyState.Pressed, Keys.Up -> renderState.Value.translation <- x,y - tr/renderState.Value.zoom
            | KeyState.Pressed, Keys.Down -> renderState.Value.translation <- x,y + tr/renderState.Value.zoom
            | KeyState.Pressed, Keys.Left -> renderState.Value.translation <- x + tr/renderState.Value.zoom,y
            | KeyState.Pressed, Keys.Right -> renderState.Value.translation <- x - tr/renderState.Value.zoom,y
            | KeyState.Pressed, Keys.Escape -> ()
            | KeyState.Pressed, Keys.Add -> renderState.Value.n <- renderState.Value.n + 1; renderState.Value.redraw <- true
            | _,_ -> ())

let run<'a when 'a :> IDrawable>(d:RenderTarget -> 'a) =
    let form = new RenderForm()
    let desc = SwapChainDescription(BufferCount = 1,
                                    ModeDescription = new ModeDescription(form.ClientSize.Width, form.ClientSize.Height, new Rational(60, 1), Format.R8G8B8A8_UNorm),
                                    IsWindowed = true,
                                    OutputHandle = form.Handle,
                                    SampleDescription = new SampleDescription(1, 0),
                                    SwapEffect = SwapEffect.Discard,
                                    Usage = Usage.RenderTargetOutput)
    let _,device,swapChain = Device.CreateWithSwapChain(DriverType.Hardware, DeviceCreationFlags.BgraSupport, desc)
    device.Factory.SetWindowAssociation(form.Handle, WindowAssociationFlags.IgnoreAll) |> ignore
    let renderView = resize device swapChain (Vector2((float32 form.ClientSize.Width), (float32 form.ClientSize.Height)))
    use sprite = new SpriteRenderer(device)
    use textBlock = new TextBlockRenderer(sprite, "Arial", FontWeight.Bold, FontStyle.Normal, FontStretch.Normal, 16.0f)
    use swapChain = swapChain
    use device = device
    use rt = setup2d(swapChain).Value
    let renderState = ref { n = 5
                            redraw = true
                            zoom = 1.0f
                            translation = 0.0f,0.0f }

    let dd = (d rt)
    IO.setup(renderState)
    let fm = FrameMonitor.FrameMonitor()
    MessagePump.Run(form, (fun () ->
        fm.Tick()
        device.ImmediateContext.ClearRenderTargetView(renderView, Color4(Color.DarkBlue))
        mainLoop textBlock rt renderState dd
        textBlock.DrawString((string fm.FPS), Vector2(50.0f), Color4(Color.White)) |> ignore
        sprite.Flush()
        swapChain.Present(1, PresentFlags.None) |> ignore
        ))