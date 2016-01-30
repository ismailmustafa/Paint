{-# OPTIONS -Wall #-}
module Main where

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UM
import Control.Monad.Primitive
import Control.Monad (filterM)
import Data.Word (Word8)
import System.Random
import Data.Colour

type Width = Int
type Height = Int
type XPos = Int
type YPos = Int
type Color = (Word8, Word8, Word8)
type P2 = (XPos, YPos)
type Pixel    = (P2, Color, Bool)
type Canvas m = UM.MVector m Pixel
type Points = V.Vector P2

width :: Width
width = 640

height :: Height
height = 360

generatePixels :: Width -> Height -> [Pixel]
generatePixels w h = [((j,i),(255,255,255),False) | i <- [0..h-1], j <- [0..w-1]]

createPixelInstruction :: Pixel -> Drawing PixelRGBA8 ()
createPixelInstruction ((x,y),(r,g,b),_) = 
    withTexture (uniformTexture color) . fill $ rectangle point 1 1
    where color = PixelRGBA8 (r :: Word8) (g :: Word8) (b :: Word8) 255
          point = V2 (fromIntegral x) (fromIntegral y)

generateCanvas :: PrimMonad m => Width -> Height -> StdGen -> m (Canvas (PrimState m))
generateCanvas w h g = do
    let seed = (quot w 2, quot h 2)
        seedColor = (207,0,15)
        canvasPoints = V.fromList [(j,i) | i <- [0..height-1], j <- [0..width-1]]
    mV <- UV.thaw $ UV.fromList $ generatePixels w h -- create canvas
    _  <- drawPixel mV seedColor seed -- draw center pixel (seed)
    _  <- draw mV canvasPoints seedColor g
    return mV

draw :: PrimMonad m => Canvas (PrimState m) -> Points -> Color -> StdGen -> m (Canvas (PrimState m))
draw canvas canvasPoints seedColor g = do
    let (nextColor,g') = newColor seedColor g
    points <- touchedPoints canvas canvasPoints
    V.sequence_ $ V.map (drawBorderPixels canvas nextColor g') points
    if (V.length points < (width*height)) then do (draw canvas canvasPoints nextColor g') else return canvas

randVals :: StdGen -> ((Double, Double, Double), StdGen)
randVals g = ((x,y,z),g''')
    where (x, g'  ) = randomR (0,1) g
          (y, g'' ) = randomR (0,1) g'
          (z, g''') = randomR (0,1) g''

newColor :: Color -> StdGen -> (Color,StdGen)
newColor c g = (calculateColor c prob, g')
    where (prob,g') = randVals g

calculateColor :: Color -> (Double, Double, Double) -> Color
calculateColor (r,g,b) (x,y,z) = color
    where color = (incrementColor r (inc x), incrementColor g (inc y), incrementColor b (inc z))

incrementColor :: Word8 -> Inc -> Word8
incrementColor w i
    | i == Add && w == 255 = 244
    | i == Subtract && w == 0 = 1
    | i == Add = w + 5
    | i == Subtract = w - 5
    | otherwise = w


data Inc = Add | Same | Subtract deriving (Eq)

inc :: Double -> Inc
inc d
    | d >= 0.0 && d < 0.33  = Subtract
    | d >= 0.33 && d < 0.66 = Same
    | otherwise             = Add

drawBorderPixels :: PrimMonad m => Canvas (PrimState m) -> Color -> StdGen -> P2 -> m (Canvas (PrimState m))
drawBorderPixels canvas color g point = do
    (_,_,touched) <- UM.read canvas (index point)
    if touched then do
        points <- pointsToDraw canvas point g
        sequence_ $ (drawPixel canvas color) <$> points
        return canvas
    else return canvas

-- Generate list of safe pixels to check
pointsToDraw :: PrimMonad m => Canvas (PrimState m) -> P2 -> StdGen -> m [P2]
pointsToDraw canvas (x,y) g = do
    let validPoints = filter validPoint points
    availablePoints <- untouchedPoints canvas validPoints
    let shuffledPoints = shuffle availablePoints g
        takenPoints = take 1 shuffledPoints
    return takenPoints
    where n  = (x,y-1)
          ne = (x+1,y-1)
          e  = (x+1,y)
          se = (x+1,y+1)
          s  = (x,y+1)
          sw = (x-1,y+1)
          w  = (x-1,y)
          nw = (x-1,y-1)
          points = [n,ne,e,se,s,sw,w,nw]
          

shuffle :: [P2] -> StdGen -> [P2]
shuffle [] _ = []
shuffle xs g = val : shuffle rest g'
    where (idx,g') = randomR (0,length xs - 1) g
          val = xs !! idx
          bef = take idx xs
          aft = drop (idx+1) xs
          rest = bef ++ aft

validPoint :: P2 -> Bool
validPoint (x,y)
    | x < 0 || x >= width  = False
    | y < 0 || y >= height = False
    | otherwise            = True

touchedPoints :: PrimMonad m => Canvas (PrimState m) -> Points -> m Points
touchedPoints canvas points = do
    validPoints <- V.filterM (touchedPoint canvas) points
    return validPoints

--getTouchedPoint :: PrimMonad m => Canvas (PrimState m) -> Points -> m P2
--getTouchedPoint canvas points = do
--    validPoints <- V.filterM (touchedPoint canvas) points
--    return $ V.head $ V.take 1 validPoints

--touchedPoint :: PrimMonad m => Canvas (PrimState m) -> P2 -> m Bool
--touchedPoint canvas point = do
--    (_,_,touched) <- UM.read canvas (index point)
--    if touched then return True else return False

touchedPoint :: PrimMonad m => Canvas (PrimState m) -> P2 -> m Bool
touchedPoint canvas n = do
    (_,_,touched) <- UM.read canvas $ index n
    if touched then return True else return False

untouchedPoints :: PrimMonad m => Canvas (PrimState m) -> [P2] -> m [P2]
untouchedPoints canvas points = do
    validPoints <- filterM (untouchedPoint canvas) points
    return validPoints

untouchedPoint :: PrimMonad m => Canvas (PrimState m) -> P2 -> m Bool
untouchedPoint canvas point = do
    (_,_,touched) <- UM.read canvas (index point)
    if touched then return False else return True

drawPixel :: PrimMonad m => Canvas (PrimState m) -> Color -> P2 -> m (Canvas (PrimState m))
drawPixel canvas color point = do
    let pixel = (point, color, True)
    UM.write canvas (index point) pixel
    return canvas

vecToInstructions :: UV.Vector Pixel -> [Drawing PixelRGBA8 ()]
vecToInstructions v = createPixelInstruction <$> UV.toList v

index :: P2 -> Int
index (x,y) = width * y + x
    
main :: IO ()
main = do
    let rgb = RGB (255,255,255)
        hue = hsvView rgb
    print hue
    gen <- getStdGen
    mutableCanvas <- generateCanvas width height gen
    frozenCanvas <- UV.freeze mutableCanvas
    let drawInstructions = vecToInstructions frozenCanvas
        white = PixelRGBA8 255 255 255 255
        img = renderDrawing width height white $ sequence_ drawInstructions
    writePng "image.png" img
