namespace FSharpHero

module SDL_Handmade =

    open System
    open System.Diagnostics
    open System.Runtime.InteropServices
    open Microsoft.FSharp.NativeInterop
    open SDL2

    type SDLWindow = nativeint
    type SDLRenderer = nativeint
    type SDLTexture = nativeint

    type SDL_WindowSize = { Width : int; Height : int }
    type SDL_BackBuffer =
        {
            mutable Texture : SDLTexture;
            mutable Size : SDL_WindowSize;
            mutable Pixels : int32[]
            mutable BytesPerPixel : int
        }

    let GlobalBackBuffer =
        {
            Texture = Unchecked.defaultof<SDLTexture>
            Size = { Width = 0; Height = 0 }
            Pixels = Array.zeroCreate<int32> 0
            BytesPerPixel = 0
        }

    let SDL_GetWindowSize (window:SDLWindow) =
        match SDL.SDL_GetWindowSize( window ) with
        | ( width, height ) -> { Width = width; Height = height }

    let RenderGradient (blueOffset:int) (greenOffset:int) =
        let pitch = GlobalBackBuffer.Size.Width * GlobalBackBuffer.BytesPerPixel


    let SDL_UpdateWindow (renderer:SDLRenderer) =
        let pixelHandle = GCHandle.Alloc( GlobalBackBuffer.Pixels, GCHandleType.Pinned )
        let pixelPtr = pixelHandle.AddrOfPinnedObject()

        SDL.SDL_UpdateTexture( GlobalBackBuffer.Texture,
                               IntPtr.Zero,
                               pixelPtr,
                               GlobalBackBuffer.Size.Width * GlobalBackBuffer.BytesPerPixel ) |> ignore

        SDL.SDL_RenderCopy( renderer,
                            GlobalBackBuffer.Texture,
                            IntPtr.Zero,
                            IntPtr.Zero ) |> ignore

        SDL.SDL_RenderPresent renderer |> ignore

        pixelHandle.Free()

    let SDL_ResizeTexture (window:SDLWindow) =
        if GlobalBackBuffer.Texture <> IntPtr.Zero then
            SDL.SDL_DestroyTexture GlobalBackBuffer.Texture |> ignore

        let renderer = SDL.SDL_GetRenderer window
        GlobalBackBuffer.Size <- SDL_GetWindowSize window
        GlobalBackBuffer.Pixels <- Array.create ( GlobalBackBuffer.Size.Width * GlobalBackBuffer.Size.Height ) 0
        GlobalBackBuffer.Texture <- SDL.SDL_CreateTexture( renderer,
                                                   SDL.SDL_PIXELFORMAT_ABGR8888,
                                                   (int)SDL.SDL_TextureAccess.SDL_TEXTUREACCESS_STREAMING,
                                                   GlobalBackBuffer.Size.Width, GlobalBackBuffer.Size.Height )

    let SDL_WindowEvent (event:SDL.SDL_Event) =
        let window = SDL.SDL_GetWindowFromID event.window.windowID
        match event.window.windowEvent with
        | SDL.SDL_WindowEventID.SDL_WINDOWEVENT_RESIZED -> SDL_ResizeTexture window
        | SDL.SDL_WindowEventID.SDL_WINDOWEVENT_SIZE_CHANGED -> SDL_ResizeTexture window
        | SDL.SDL_WindowEventID.SDL_WINDOWEVENT_EXPOSED -> SDL_UpdateWindow  window
        | _ -> ()

    let SDL_HandleEvent (event:SDL.SDL_Event) =
        match event.``type`` with
        | SDL.SDL_EventType.SDL_QUIT -> SDL.SDL_Quit()
        | SDL.SDL_EventType.SDL_WINDOWEVENT -> SDL_WindowEvent event
        | _ -> ()


    [<EntryPoint>]
    let main args =

        SDL.SDL_SetHint( SDL.SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING, "1" ) |> ignore

        if SDL.SDL_Init SDL.SDL_INIT_VIDEO <> 0 then
            printfn "SDL Init Error!"
        else
            printfn "SDL Init Successful"

            let window =
                SDL.SDL_CreateWindow( "FSharp Hero",
                                       100,100,1200,900,
                                       SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE )

            let renderer =
                SDL.SDL_CreateRenderer( window, -1, SDL.SDL_RendererFlags.SDL_RENDERER_SOFTWARE )

            SDL_ResizeTexture window

            let mutable sdlEvent = Unchecked.defaultof<SDL.SDL_Event>
            let mutable counter = 0

            let rec GameLoop eventFunction =
                if SDL.SDL_GetWindowID window = (uint32)0 || SDL.SDL_GetRenderer window = IntPtr.Zero then
                    Debug.WriteLine( "Closing GameLoop!" )
                else
                    counter <- counter + 1
                    printfn "%d" counter
                    SDL.SDL_PollEvent( &sdlEvent ) |> ignore
                    SDL_UpdateWindow renderer
                    GameLoop ( SDL_HandleEvent sdlEvent )

            GameLoop ( SDL_HandleEvent sdlEvent )
        0