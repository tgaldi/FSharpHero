namespace FSharpHero

module Handmade =
    open System
    open System.Runtime.InteropServices
    open Microsoft.FSharp.NativeInterop

    type game_offscreen_buffer =
        {
            mutable Pixels : int32[]
            Width : int32
            Height : int32
            BytesPerPixel : int32
        }

    type game_sound_output_buffer =
        {
            SamplesPerSecond : int
            SampleCount : int
            Data : byte[]
        }

    let private toneVolume = 3000
    let private toneHz = 256
    let mutable private tSine = 0.0f
    let private GameOutputSound (soundBuffer:game_sound_output_buffer) =
        let wavePeriod = soundBuffer.SamplesPerSecond / toneHz

        let sampleOut = GCHandle.Alloc( soundBuffer.Data, GCHandleType.Pinned )
        let mutable sampleOutPtr = NativePtr.ofNativeInt<int16>( sampleOut.AddrOfPinnedObject() )

        let rec fillBuffer sampleIndex =
            if sampleIndex < soundBuffer.SampleCount then
                let sineValue = sin( tSine )
                let sampleValue = (int16)( sineValue * (float32)toneVolume )

                NativePtr.write sampleOutPtr sampleValue
                sampleOutPtr <- NativePtr.add sampleOutPtr 1
                NativePtr.write sampleOutPtr sampleValue
                sampleOutPtr <- NativePtr.add sampleOutPtr 1

                tSine <- tSine + 2.0f * (float32)Math.PI * 1.0f / (float32)wavePeriod
                fillBuffer (sampleIndex+1)
        fillBuffer 0

        sampleOut.Free()

    let private RenderGradient (renderBuffer:game_offscreen_buffer) (blueOffset:int32) (greenOffset:int32) =
        let w = renderBuffer.Width
        let h = renderBuffer.Height

        let rec colLoop row col =
            if col <> w then
                let b = (col + blueOffset) % 256
                let g = (row + greenOffset) % 256
                renderBuffer.Pixels.[(row*w)+col] <- int32(g <<< 8 ||| b)
                colLoop row (col+1)

        let rec rowLoop row =
            if row <> h then
                colLoop row 0
                rowLoop (row+1)

        rowLoop 0

    let GameUpdateAndRender (renderBuffer:game_offscreen_buffer) (blueOffset:int32) (greenOffset:int32) (soundBuffer:game_sound_output_buffer) =
        GameOutputSound soundBuffer
        RenderGradient renderBuffer blueOffset greenOffset
