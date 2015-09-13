module Renderer2

open System
open System.Diagnostics
open System.Drawing
open SlimDX
open SlimDX.Direct3D11
open SlimDX.DirectWrite
open SlimDX.DXGI
open SlimDX.Windows
open SpriteTextRenderer.SlimDX
type Device = SlimDX.Direct3D11.Device
type FontStyle = SlimDX.DirectWrite.FontStyle
type TextAlignment = SpriteTextRenderer.TextAlignment

let mainLoop (device:Device) (renderView:RenderTargetView) (sprite:SpriteRenderer) (swapChain:SwapChain) (textBlock:TextBlockRenderer) =
    device.ImmediateContext.ClearRenderTargetView(renderView, Color4(Color.DarkBlue))

    

    textBlock.DrawString("ABCDEFGHIJKLMNOPQRSTUVWXYZ" + Environment.NewLine + "abcdefghijklmnopqrstuvwxyz", Vector2.Zero,
                         new Color4(1.0f, 1.0f, 0.0f)) |> ignore
    sprite.Flush()

    swapChain.Present(1, PresentFlags.None) |> ignore

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

let run() =
    let form = new RenderForm()
    let desc = SwapChainDescription(BufferCount = 1,
                                    ModeDescription = new ModeDescription(form.ClientSize.Width, form.ClientSize.Height, new Rational(60, 1), Format.R8G8B8A8_UNorm),
                                    IsWindowed = true,
                                    OutputHandle = form.Handle,
                                    SampleDescription = new SampleDescription(1, 0),
                                    SwapEffect = SwapEffect.Discard,
                                    Usage = Usage.RenderTargetOutput)
    let _,device,swapChain = Device.CreateWithSwapChain(DriverType.Hardware, DeviceCreationFlags.None, desc)
    device.Factory.SetWindowAssociation(form.Handle, WindowAssociationFlags.IgnoreAll) |> ignore
    let renderView = resize device swapChain (Vector2((float32 form.ClientSize.Width), (float32 form.ClientSize.Height)))
    use sprite = new SpriteRenderer(device)
    use textBlock = new TextBlockRenderer(sprite, "Arial", FontWeight.Bold, FontStyle.Normal, FontStretch.Normal, 16.0f)
    use swapChain = swapChain
    use device = device
    
    MessagePump.Run(form, (fun () -> mainLoop device renderView sprite swapChain textBlock))