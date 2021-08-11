{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Codec.Picture               (writePng)
import           Codec.Picture.Types         (Image, MutableImage (..), Pixel,
                                              PixelRGBA8 (..),
                                              createMutableImage,
                                              unsafeFreezeImage, writePixel)
import           Codec.Wav                   (exportFile, importFile)
import           Control.Monad               (forM_)
import           Control.Monad.Primitive     (PrimMonad, PrimState)
import           Data.Array.Unboxed          (elems)
import           Data.Audio                  (Audio (Audio), SampleData)
import           Data.Foldable               (foldlM)
import           Data.Int                    (Int32)
import           Data.Maybe                  (fromMaybe)
import           GHC.Float                   (float2Int, int2Float)
import           Graphics.Rasterific         (Cap (CapRound), Join (JoinRound),
                                              V2 (V2), line, renderDrawing,
                                              stroke, withTexture)
import           Graphics.Rasterific.Texture (uniformTexture)
import           System.IO                   (FilePath)
import           Numeric.Transform.Fourier.FFT (fft, rfft)
import           Data.Complex (Complex((:+)), magnitude, polar, realPart, imagPart)
import           Data.Array (Array, listArray, bounds)


-- | Drawing algorithm



-- Audio data

file1 = "sounds/fireplace.wav"
file2 = "sounds/fireplace.wav"

limit :: Float
limit = fromIntegral (maxBound::Int32)

-- | Take two mono files and zip them together
zipAudio :: FilePath -> FilePath -> IO [(Float, Float)]
zipAudio path1 path2 = do
    maybeAudio1 <- importFile path1
    maybeAudio2 <- importFile path2
    let audioToList file =
            case file :: Either String (Audio Int32) of
                Left s -> []
                Right (Audio rate channels samples) ->
                    (\a -> fromIntegral a / limit) <$> elems samples
        list1 = audioToList maybeAudio1
        list2 = audioToList maybeAudio2
        zipped = zip list1 list2
    return zipped

-- | Take two mono files and zip them together
getPolarFromWav :: FilePath -> IO [(Float, Float)]
getPolarFromWav path = do
    maybeAudio <- importFile path
    let audioToList file =
            case file :: Either String (Audio Int32) of
                Left s -> []
                Right (Audio rate channels samples) ->
                    (\a -> fromIntegral a / limit * 0.001) <$> elems samples
        realFFT = elems $ fmap (polToCar . polar) $ rfft $ listArray (0, length (audioToList maybeAudio) - 2) $ audioToList maybeAudio
    return realFFT



polToCar :: Floating b => (b, b) -> (b, b)
polToCar (amp, ph) = (amp * cos ph, amp * sin ph)


main :: IO ()
main = do
    -- AudioData
    -- audio <- zipAudio file1 file2 -- 2 channel audio
    audio <- getPolarFromWav file1 -- 1 channel FFT

    -- Drawing
    let zippedList = zip audio (tail audio)
        -- imgWidth = 19866 -- in px
        -- imgHeight = 19866 -- in px
        imgWidth = 2000 -- in px
        imgHeight = 2000 -- in px
        midpointW = int2Float imgWidth / 2
        midpointH = int2Float imgHeight / 2

        -- Translate onto a canvas
        scalePixels midpoint amp = (amp * midpoint) + midpoint

        -- Set colours
        opacity = 0.25::Float
        white = PixelRGBA8 255 255 255 255
        drawColor = PixelRGBA8 0 0 0 $ round $ 255 * opacity
        img = renderDrawing imgWidth imgHeight white $ do
            forM_ zippedList (\((a1, a2), (b1, b2)) ->
                withTexture (uniformTexture drawColor) $
                    stroke 1 JoinRound (CapRound, CapRound) $
                        line
                            (V2 (scalePixels midpointW a1) (scalePixels midpointH a2))
                            (V2 (scalePixels midpointW b1) (scalePixels midpointH b2))
                            )

    -- Write it out to a file on disc
    writePng "output.png" img


