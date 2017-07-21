namespace FSharpHero

module Handmade =

    type game_offscreen_buffer =
        {
            mutable Pixels : int32[]
            Width : int32
            Height : int32
            BytesPerPixel : int32
        }

    let private RenderGradient (buffer:game_offscreen_buffer) (blueOffset:int32) (greenOffset:int32) =
        let w = buffer.Width
        let h = buffer.Height

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

    let GameUpdateAndRender (buffer:game_offscreen_buffer) (blueOffset:int32) (greenOffset:int32) =
        RenderGradient buffer blueOffset greenOffset
