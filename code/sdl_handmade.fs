namespace FSharpHero

module SDL_Handmade =
    open System
    open System.Diagnostics
    open System.Runtime.InteropServices
    open Microsoft.FSharp.NativeInterop

    open SDL2

    open Handmade

    // #NOTE this probably doesn't work
    type Ptr = { Handle : GCHandle; Address : nativeint }
    let GetPtr value =
        let handle = GCHandle.Alloc( value, GCHandleType.Pinned )
        let address = handle.AddrOfPinnedObject()
        { Handle = handle; Address = address }

    type System.ArraySegment<'T> with
        member this.Item(x) =
            if x < 0 || x >= this.Count then
                raise (System.IndexOutOfRangeException("Index was outside the bounds of the array segment."))
            this.Array.[x + this.Offset]

        member this.GetSlice(start: int option, finish : int option) =
            let start = defaultArg start 0
            let finish = defaultArg finish (this.Count - 1)
            if start < 0 || finish >= this.Count then
                raise (System.IndexOutOfRangeException("Index was outside the bounds of the array segment."))
            new ArraySegment<'T>(this.Array, this.Offset + start, finish - start + 1)


    type SDLWindow = nativeint
    type SDLRenderer = nativeint
    type SDLTexture = nativeint
    type SDLController = nativeint
    type SDLEvent = SDL.SDL_Event



    type SDL_WindowSize = { Width : int32; Height : int32 }
    type SDL_Offscreen_Buffer =
        {
            mutable Texture : SDLTexture;
            mutable Size : SDL_WindowSize;
            mutable Pixels : int32[]
            BytesPerPixel : int32
        } with
            member this.PixelHandle = GCHandle.Alloc( this.Pixels, GCHandleType.Pinned )
            member this.PixelPtr = this.PixelHandle.AddrOfPinnedObject()
    let GlobalBackBuffer =
        {
            Texture = Unchecked.defaultof<SDLTexture>
            Size = { Width = 0; Height = 0 }
            Pixels = Array.zeroCreate<int32> 0
            BytesPerPixel = 4
        }

    let SDL_GetWindowSize (window:SDLWindow) =
        // #NOTE GetWindowSize takes a window and two byRef or "out" int results.
        // F# automatically turns out parameters into tuples instead of the user providing a data container.
        // The matching on ( width, height ) is matching the returned tuple.
        match SDL.SDL_GetWindowSize( window ) with
        | ( width, height ) -> { Width = width; Height = height }


    type SDL_Audio_Output =
        {
            SamplesPerSecond : int
            mutable ToneHz : int
            ToneVolume : int16
            mutable RunningSampleIndex : int
            BytesPerSample : int
            mutable tSine : float
        } with
            member this.WavePeriod = this.SamplesPerSecond / this.ToneHz
            member this.AudioBufferSize = this.BytesPerSample * this.SamplesPerSecond
            member this.LatencySampleCount = this.SamplesPerSecond / 15
    let AudioOutput =
        {
            SamplesPerSecond = 48000
            ToneHz = 256
            ToneVolume = (int16)3000
            RunningSampleIndex = 0
            BytesPerSample = sizeof<int16> * 2
            tSine = 0.0
        }

    type SDL_Audio_Ring_Buffer =
        {
            mutable Size : int
            mutable WriteCursor : int
            mutable PlayCursor : int
            mutable Data : byte[]
            mutable Callback : SDL.SDL_AudioCallback
        }
    let AudioRingBuffer =
        {
            Size = 0
            WriteCursor = 0
            PlayCursor = 0
            Data = Array.zeroCreate<byte> 0
            Callback = Unchecked.defaultof<SDL.SDL_AudioCallback>
        }

    let SDL_AudioCallback (userdata:nativeint) (audiodata:nativeint) (length:int) =
        // #NOTE Use tuples to return values of a condition rather than mutables
        let region1Size, region2Size =
            if( AudioRingBuffer.PlayCursor + length > AudioRingBuffer.Size ) then
                let r1 = AudioRingBuffer.Size - AudioRingBuffer.PlayCursor
                r1, length - r1
            else
                length, 0

        let offsetPtr = NativePtr.add (NativePtr.ofNativeInt<byte> audiodata) region1Size
        Marshal.Copy( AudioRingBuffer.Data, AudioRingBuffer.PlayCursor, audiodata, region1Size )
        Marshal.Copy( AudioRingBuffer.Data, 0, NativePtr.toNativeInt offsetPtr, region2Size )

        AudioRingBuffer.PlayCursor <- (AudioRingBuffer.PlayCursor + length) % AudioRingBuffer.Size
        AudioRingBuffer.WriteCursor <- (AudioRingBuffer.PlayCursor + 2048) % AudioRingBuffer.Size

    let SDL_InitAudio samplesPerSecond bufferSize =
        AudioRingBuffer.Size <- bufferSize
        AudioRingBuffer.Data <- Array.zeroCreate<byte> bufferSize
        AudioRingBuffer.Callback <- new SDL.SDL_AudioCallback( SDL_AudioCallback )

        let mutable settings = SDL.SDL_AudioSpec()
        settings.freq <- samplesPerSecond
        settings.format <- SDL.AUDIO_S16LSB
        settings.channels <- (byte)2
        settings.samples <- (uint16)1024
        settings.callback <- AudioRingBuffer.Callback


        SDL.SDL_OpenAudio (ref settings) |> ignore

        printfn "Initialized an audio device at frequency %d Hz, %d channels" settings.freq settings.channels

        if settings.format <> SDL.AUDIO_S16LSB then
            printfn "Sample format not set to AUDIO_S16LSB, closing audio device!"
            SDL.SDL_CloseAudio()

        SDL.SDL_PauseAudio 0


    let MAX_CONTROLLERS = 4
    let SDL_OpenControllers count =
        // #NOTE This is the cons (construct) pattern used to deconstruct a list.
        // The first element of the list, the head, is popped off and the tail contains the remaining list.
        let rec loop (opened:list<SDLController>) toOpen =
            match toOpen with
            | [] -> List.rev opened
            | head :: tail ->
            // #NOTE Here a tuple is created to match against, with the second element being a conditional
            // A "when" guard could also be used in the matches instead.
            // If the head is opened a new "opened" list is created which contains the head and the old list.
                match ( (SDL.SDL_IsGameController head), head < MAX_CONTROLLERS ) with
                | SDL.SDL_bool.SDL_TRUE, true -> loop ( (SDL.SDL_GameControllerOpen head) :: opened ) tail
                | _ -> loop opened tail

        loop [] count

    let SDL_CloseControllers (controllers:list<SDLController>) =
        let rec loop (toClose:list<SDLController>) =
            match toClose with
            | [] -> ()
            | head :: tail ->
                SDL.SDL_GameControllerClose head
                loop tail

        loop controllers


    let mutable BlueOffset = 0
    let mutable GreenOffset = 0
    let RenderGradient (buffer:SDL_Offscreen_Buffer) (blueOffset:int32) (greenOffset:int32) =
        let w = buffer.Size.Width
        let h = buffer.Size.Height

        let rec colLoop row col =
            if col <> w then
                let b = (col + blueOffset) % 256
                let g = (row + greenOffset) % 256
                buffer.Pixels.[(row*w)+col] <- int32(g <<< 8 ||| b)
                colLoop row (col+1)

        let rec rowLoop row =
            if row <> h then
                colLoop row 0
                rowLoop (row+1)

        rowLoop 0


    let SDL_UpdateWindow (renderer:SDLRenderer) =
        SDL.SDL_UpdateTexture( GlobalBackBuffer.Texture,
                               IntPtr.Zero,
                               GlobalBackBuffer.PixelPtr,
                               GlobalBackBuffer.Size.Width * GlobalBackBuffer.BytesPerPixel ) |> ignore

        SDL.SDL_RenderCopy( renderer,
                            GlobalBackBuffer.Texture,
                            IntPtr.Zero,
                            IntPtr.Zero ) |> ignore

        SDL.SDL_RenderPresent renderer |> ignore


    let SDL_ResizeTexture (window:SDLWindow) =
        if GlobalBackBuffer.Texture <> IntPtr.Zero then
            SDL.SDL_DestroyTexture GlobalBackBuffer.Texture |> ignore

        let renderer = SDL.SDL_GetRenderer window
        GlobalBackBuffer.Size <- SDL_GetWindowSize window
        GlobalBackBuffer.Pixels <- Array.zeroCreate ( GlobalBackBuffer.Size.Width * GlobalBackBuffer.Size.Height )
        GlobalBackBuffer.Texture <- SDL.SDL_CreateTexture( renderer,
                                                   SDL.SDL_PIXELFORMAT_ABGR8888,
                                                   (int)SDL.SDL_TextureAccess.SDL_TEXTUREACCESS_STREAMING,
                                                   GlobalBackBuffer.Size.Width, GlobalBackBuffer.Size.Height )


    let SDL_HandleEvent (event:SDL.SDL_Event) =
        // #TODO replace this mutable with something more functional
        let mutable quitEvent = false
        match event.``type`` with
        | SDL.SDL_EventType.SDL_QUIT -> quitEvent <- true; ()

        | keyevent when keyevent = SDL.SDL_EventType.SDL_KEYDOWN || keyevent = SDL.SDL_EventType.SDL_KEYUP ->
            let IsDown = event.key.state = SDL.SDL_PRESSED
            let WasDown = match event.key with
                          | key when key.state = SDL.SDL_RELEASED -> true
                          | key when key.repeat <> (byte)0 -> true
                          | _ -> false

            if event.key.repeat = (byte)0 then
               match event.key.keysym.sym with
               | SDL.SDL_Keycode.SDLK_w ->
                   if IsDown then printfn "IsDown"
                   elif WasDown then printfn "WasDown"
               | SDL.SDL_Keycode.SDLK_s -> ()
               | SDL.SDL_Keycode.SDLK_d -> ()
               | SDL.SDL_Keycode.SDLK_q -> ()
               | SDL.SDL_Keycode.SDLK_e -> ()
               | SDL.SDL_Keycode.SDLK_UP -> ()
               | SDL.SDL_Keycode.SDLK_LEFT -> ()
               | SDL.SDL_Keycode.SDLK_DOWN -> ()
               | SDL.SDL_Keycode.SDLK_RIGHT -> ()
               | SDL.SDL_Keycode.SDLK_ESCAPE -> quitEvent <- true; ()
               | SDL.SDL_Keycode.SDLK_SPACE -> ()
               | _ -> ()

        | SDL.SDL_EventType.SDL_WINDOWEVENT ->
            let window = SDL.SDL_GetWindowFromID event.window.windowID
            match event.window.windowEvent with
            | SDL.SDL_WindowEventID.SDL_WINDOWEVENT_RESIZED -> SDL_ResizeTexture window
            | SDL.SDL_WindowEventID.SDL_WINDOWEVENT_SIZE_CHANGED -> SDL_ResizeTexture window
            | SDL.SDL_WindowEventID.SDL_WINDOWEVENT_EXPOSED -> SDL_UpdateWindow window
            | _ -> ()

        | _ -> ()

        quitEvent


    let SDL_PollEvents () =
        let rec loop (pendingCount, event) =
            let quitEvent = SDL_HandleEvent event
            match ( quitEvent, (pendingCount > 0) ) with
            | false, true -> SDL.SDL_PollEvent() |> loop
            | _ -> quitEvent
        SDL.SDL_PollEvent() |> loop


    let rec SDL_PollControllers (controllers:list<SDLController>) =
        match controllers with
        | [] -> ()
        | head :: tail ->
            match (SDL.SDL_GameControllerGetAttached head) with
            | SDL.SDL_bool.SDL_FALSE -> ()
            | SDL.SDL_bool.SDL_TRUE ->
                let Up = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_DPAD_UP )
                let Down = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_DPAD_DOWN )
                let Left = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_DPAD_LEFT )
                let Right = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_DPAD_RIGHT )

                let Start = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_START )
                let Back = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_BACK )

                let LeftShoulder = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_LEFTSHOULDER )
                let RightShoulder = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_RIGHTSHOULDER )

                let AButton = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_A )
                let BButton = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_B )
                let XButton = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_X )
                let YButton = SDL.SDL_GameControllerGetButton( head, SDL.SDL_GameControllerButton.SDL_CONTROLLER_BUTTON_Y )

                let StickX = SDL.SDL_GameControllerGetAxis( head, SDL.SDL_GameControllerAxis.SDL_CONTROLLER_AXIS_LEFTX )
                let StickY = SDL.SDL_GameControllerGetAxis( head, SDL.SDL_GameControllerAxis.SDL_CONTROLLER_AXIS_LEFTY )

                match AButton with
                | x when x = (byte)1 -> BlueOffset <- BlueOffset + 1
                | _ -> ()

                GreenOffset <- GreenOffset + (int32)(StickY >>> 12)

//                AudioOutput.ToneHz <- 512 + (int)(256.0f*((float32)StickX / 30000.0f))


            SDL_PollControllers tail



    let SDL_FillSoundBuffer byteToLock byteToWrite =
        let region1Size =
            match (byteToWrite + byteToLock > AudioOutput.AudioBufferSize) with
            | true -> AudioOutput.AudioBufferSize - byteToLock
            | false -> byteToWrite

        let region2Size = byteToWrite - region1Size

        let sampleOut = GCHandle.Alloc( AudioRingBuffer.Data, GCHandleType.Pinned )
        let mutable ptr = NativePtr.ofNativeInt<int16>( sampleOut.AddrOfPinnedObject() )

        let region1SampleCount = region1Size / AudioOutput.BytesPerSample
        ptr <-NativePtr.add ptr (byteToLock / 2)

        // #TODO replace for loops with recursion
        for x in [0..region1SampleCount-1] do
//            AudioOutput.tSine <- ((Math.PI * 2.0) * (float)AudioOutput.RunningSampleIndex) / (float)AudioOutput.WavePeriod
            let t = ((Math.PI * 2.0) * (float)AudioOutput.RunningSampleIndex) / (float)AudioOutput.WavePeriod
            let sineValue = Math.Sin( t )
            let sampleValue = (int16)( sineValue * (float)AudioOutput.ToneVolume )
            NativePtr.write ptr sampleValue
            ptr <- NativePtr.add ptr 1
            NativePtr.write ptr sampleValue
            ptr <- NativePtr.add ptr 1
            AudioOutput.RunningSampleIndex <- AudioOutput.RunningSampleIndex + 1

        let region2SampleCount = region2Size / AudioOutput.BytesPerSample
        ptr <- NativePtr.ofNativeInt<int16>( sampleOut.AddrOfPinnedObject() )
        for x in [0..region2SampleCount-1] do
//            AudioOutput.tSine <- ((Math.PI * 2.0) * (float)AudioOutput.RunningSampleIndex) / (float)AudioOutput.WavePeriod
            let t = ((Math.PI * 2.0) * (float)AudioOutput.RunningSampleIndex) / (float)AudioOutput.WavePeriod
            let sineValue = Math.Sin( t )
            let sampleValue = (int16)( sineValue * (float)AudioOutput.ToneVolume )
            NativePtr.write ptr sampleValue
            ptr <- NativePtr.add ptr 1
            NativePtr.write ptr sampleValue
            ptr <- NativePtr.add ptr 1
            AudioOutput.RunningSampleIndex <- AudioOutput.RunningSampleIndex + 1

        sampleOut.Free()


    [<EntryPoint>]
    let main args =

        SDL.SDL_SetHint( SDL.SDL_HINT_WINDOWS_DISABLE_THREAD_NAMING, "1" ) |> ignore

        if SDL.SDL_Init ( SDL.SDL_INIT_VIDEO ||| SDL.SDL_INIT_GAMECONTROLLER ||| SDL.SDL_INIT_AUDIO ) <> 0 then
            printfn "SDL Init Error!"
        else
            printfn "SDL Init Successful"

            let PerfCountFrequency = SDL.SDL_GetPerformanceFrequency()

            let window =
                SDL.SDL_CreateWindow( "FSharp Hero",
                                       100,100,1200,900,
                                       SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE )

            let renderer =
                SDL.SDL_CreateRenderer( window, -1, SDL.SDL_RendererFlags.SDL_RENDERER_SOFTWARE )

            SDL_ResizeTexture window

            let ControllerHandles = SDL_OpenControllers [0..SDL.SDL_NumJoysticks()-1]

            SDL_InitAudio AudioOutput.SamplesPerSecond AudioOutput.AudioBufferSize
            SDL_FillSoundBuffer 0 (AudioOutput.LatencySampleCount*AudioOutput.BytesPerSample)
            SDL.SDL_PauseAudio(0)

            let stopwatch = Stopwatch()
            stopwatch.Start()

            let rec GameLoop () =
                let LastCounter = SDL.SDL_GetPerformanceCounter()
                stopwatch.Restart()

                let quitEvent = SDL_PollEvents ()
                match quitEvent with
                | false ->
                    SDL_PollControllers ControllerHandles

                    let buffer =
                        {
                            Pixels = GlobalBackBuffer.Pixels
                            Width = GlobalBackBuffer.Size.Width
                            Height = GlobalBackBuffer.Size.Height
                            BytesPerPixel = GlobalBackBuffer.BytesPerPixel
                        }
                    GameUpdateAndRender buffer BlueOffset GreenOffset

                    SDL.SDL_LockAudio()
                    let byteToLock = (AudioOutput.RunningSampleIndex * AudioOutput.BytesPerSample) % AudioOutput.AudioBufferSize
                    let TargetCursor = (AudioRingBuffer.PlayCursor +
                                        (AudioOutput.LatencySampleCount*AudioOutput.BytesPerSample)) % AudioOutput.AudioBufferSize
                    let byteToWrite =
                        if byteToLock > TargetCursor then
                            (AudioOutput.AudioBufferSize - byteToLock) + TargetCursor
                        else
                            AudioRingBuffer.PlayCursor - byteToLock
                    SDL.SDL_UnlockAudio()

                    SDL_FillSoundBuffer byteToLock byteToWrite

                    SDL_UpdateWindow renderer

                    GreenOffset <- GreenOffset + 2

                    stopwatch.Stop()
                    let EndCounter = SDL.SDL_GetPerformanceCounter()
                    let CounterElapsed = EndCounter - LastCounter
                    let MSPerFrame = (((1000.0 * (float)CounterElapsed) / (float)PerfCountFrequency))
                    let FPS = (float)PerfCountFrequency / (float)CounterElapsed
                    printfn "%.02f ms/f, %.02f f/s, FPS:%A " MSPerFrame FPS (1000L/stopwatch.ElapsedMilliseconds)

                    GameLoop ()
                | true -> ()

            GameLoop ()

            SDL_CloseControllers ControllerHandles

            GlobalBackBuffer.PixelHandle.Free()
            SDL.SDL_Quit()
        0 // Exit Application