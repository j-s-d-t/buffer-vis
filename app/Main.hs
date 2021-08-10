{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Codec.Picture           (writePng)
import           Codec.Picture.Types     (Image, MutableImage (..), Pixel,
                                          PixelRGBA8 (..), createMutableImage,
                                          unsafeFreezeImage, writePixel)
import           Codec.Wav               (exportFile, importFile)
import           Control.Monad
import           Control.Monad.Primitive (PrimMonad, PrimState)
import           Data.Array.Unboxed      (elems, listArray)
import           Data.Audio              (Audio (Audio), SampleData)
import           Data.Foldable           (foldlM)
import           Data.Int                (Int32)
import           GHC.Float               (int2Float, float2Int)
import           Data.Maybe              (fromMaybe)
import           System.IO               (FilePath)


-- | Drawing algorithm

type MImage m px = MutableImage (PrimState m) px

-- | Create an image given a function to apply to an empty mutable image
withMutableImage
    :: (Pixel px, PrimMonad m)
    => Int                      -- ^ image width
    -> Int                      -- ^ image height
    -> px                       -- ^ background colour
    -> (MImage m px -> m ())    -- ^ function to apply to mutable image
    -> m (Image px)             -- ^ action
withMutableImage w h px f = createMutableImage w h px >>= \m -> f m >> unsafeFreezeImage m

-- | Plot a pixel at the given point in the given colour
plot
    :: (Pixel px, PrimMonad m)
    => MImage m px  -- ^ mutable image
    -> Int          -- ^ x-coordinate of point
    -> Int          -- ^ y-coordinate of point
    -> px           -- ^ colour
    -> m ()         -- ^ action
plot = writePixel

-- | Draw an antialiased line from first point to second point in given colour
drawAntialiasedLine
    :: forall px m . (Pixel px, PrimMonad m)
    => MImage m px      -- ^ mutable image
    -> Int              -- ^ x-coordinate of first point
    -> Int              -- ^ y-coordinate of first point
    -> Int              -- ^ x-coordinate of second point
    -> Int              -- ^ y-coordinate of second point
    -> (Double -> px)   -- ^ colour generator function
    -> m ()             -- ^ action
drawAntialiasedLine m p1x p1y p2x p2y colour = do
    let steep = abs (p2y - p1y) > abs (p2x - p1x)
        ((p3x, p4x), (p3y, p4y)) = swapIf steep ((p1x, p2x), (p1y, p2y))
        ((ax, ay), (bx, by)) = swapIf (p3x > p4x) ((p3x, p3y), (p4x, p4y))
        dx = bx - ax
        dy = by - ay
        gradient = if dx == 0 then 1.0 else fromIntegral dy / fromIntegral dx

    -- handle first endpoint
    let xpxl1 = ax -- round (fromIntegral ax)
        yend1 = fromIntegral ay + gradient * fromIntegral (xpxl1 - ax)
        xgap1 = rfpart (fromIntegral ax + 0.5)
    endpoint steep xpxl1 yend1 xgap1

    -- handle second endpoint
    let xpxl2 = bx -- round (fromIntegral bx)
        yend2 = fromIntegral by + gradient * fromIntegral (xpxl2 - bx)
        xgap2 = fpart (fromIntegral bx + 0.5)
    endpoint steep xpxl2 yend2 xgap2

    -- main loop
    let intery = yend1 + gradient
    void $ if steep
        then foldlM (\i x -> do
            plot m (ipart i) x (colour (rfpart i))
            plot m (ipart i + 1) x (colour (fpart i))
            pure $ i + gradient) intery [xpxl1 + 1..xpxl2 - 1]
        else foldlM (\i x -> do
            plot m x (ipart i) (colour (rfpart i))
            plot m x (ipart i + 1) (colour (fpart i))
            pure $ i + gradient) intery [xpxl1 + 1..xpxl2 - 1]

    where
        endpoint :: Bool -> Int -> Double -> Double -> m ()
        endpoint True xpxl yend xgap = do
            plot m ypxl xpxl (colour (rfpart yend * xgap))
            plot m (ypxl + 1) xpxl (colour (fpart yend * xgap))
            where ypxl = ipart yend
        endpoint False xpxl yend xgap = do
            plot m xpxl ypxl (colour (rfpart yend * xgap))
            plot m xpxl (ypxl + 1) (colour (fpart yend * xgap))
            where ypxl = ipart yend

swapIf :: Bool -> (a, a) -> (a, a)
swapIf False p     = p
swapIf True (x, y) = (y, x)

ipart :: Double -> Int
ipart = truncate

fpart :: Double -> Double
fpart x
    | x > 0 = x - temp
    | otherwise = x - (temp + 1)
    where temp = fromIntegral (ipart x)

rfpart :: Double -> Double
rfpart x = 1 - fpart x

-- Audio data

file1 = "flutec.wav"
file2 = "note.wav"

limit :: Float
limit = fromIntegral (maxBound::Int32)

-- | Take two mono files and zip them together
inMain :: FilePath -> FilePath -> IO [(Float, Float)]
inMain path1 path2 = do
    maybeAudio1 <- importFile path1
    maybeAudio2 <- importFile path2
    let audioToList file =
            case file :: Either String (Audio Int32) of
                Left s -> []
                Right (Audio rate channels samples) ->
                    fmap (\a -> fromIntegral a / limit) $ elems samples
        list1 = audioToList maybeAudio1
        list2 = audioToList maybeAudio2
        zipped = zip list1 list2
    -- print zipped
    return zipped

brightness :: Double -> PixelRGBA8
brightness br =
    let level = round (br * 255)
    in PixelRGBA8 0 0 0 255

main :: IO ()
main = do
    -- AudioData
    audioData <- inMain file1 file2

    -- Graphics
    let imgWidth = 19066 -- in px
        imgHeight = 19066 -- in px
        midpointW = int2Float imgWidth / 2
        midpointH = int2Float imgHeight / 2
        scalePixels midpoint amp = (amp * midpoint) + midpoint
    img <- withMutableImage imgWidth imgHeight (PixelRGBA8 255 255 255 255) $ \m@(MutableImage w h _) ->
        let zippedList = zip audioData (tail audioData)
        in
        forM_ zippedList (\((a1, a2), (b1, b2)) -> 
            drawAntialiasedLine 
            m 
            (float2Int $ scalePixels midpointW a1) -- start x
            (float2Int $ scalePixels midpointH a2) -- start y
            (float2Int $ scalePixels midpointW b1) -- end x
            (float2Int $ scalePixels midpointH b2) -- end
            brightness)

    -- Write it out to a file on disc
    writePng "test.png" img


