{-# OPTIONS -Wall #-}
module Main where

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
--import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UM
--import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad (filterM)
import Data.Word (Word8)
--import System.Random

type Width = Int
type Height = Int
type XPos = Int
type YPos = Int
type Color = (Word8, Word8, Word8)
type P2 = (XPos, YPos)
type Pixel    = (P2, Color, Bool)
type Canvas m = UM.MVector m Pixel

width :: Width
width = 200

height :: Height
height = 200

generatePixels :: Width -> Height -> [Pixel]
generatePixels w h = [((j,i),(255,255,255),False) | i <- [0..h-1], j <- [0..w-1]]

createPixelInstruction :: Pixel -> Drawing PixelRGBA8 ()
createPixelInstruction ((x,y),(r,g,b),_) = 
    withTexture (uniformTexture color) . fill $ rectangle point 1 1
    where color = PixelRGBA8 (r :: Word8) (g :: Word8) (b :: Word8) 255
          point = V2 (fromIntegral x) (fromIntegral y)

generateCanvas :: PrimMonad m => Width -> Height -> m (Canvas (PrimState m))
generateCanvas w h = do
    let seed = (quot w 2, quot h 2)
        seedColor = (207,0,15)
    mV <- UV.thaw $ UV.fromList $ generatePixels w h -- create canvas
    _  <- drawPixel mV seedColor seed -- draw center pixel (seed)
    _  <- draw mV
    return mV

draw :: PrimMonad m => Canvas (PrimState m) -> m (Canvas (PrimState m))
draw canvas = do
    points <- borderPoints canvas canvasPoints
    sequence_ $ drawBorderPixels canvas <$> points
    if (length points < (width*height)) then do draw canvas else return canvas
        where canvasPoints = [(j,i) | i <- [0..height-1], j <- [0..width-1]]

borderPoints :: PrimMonad m => Canvas (PrimState m) -> [P2] -> m [P2]
borderPoints canvas points = do touchedPoints canvas points

drawBorderPixels :: PrimMonad m => Canvas (PrimState m) -> P2 -> m (Canvas (PrimState m))
drawBorderPixels canvas point = do
    (_,_,touched) <- UM.read canvas (index point)
    if touched then do
        points <- pointsToDraw canvas point
        sequence_ $ (drawPixel canvas (0,0,0)) <$> points
        return canvas
    else return canvas

-- Generate list of safe pixels to check
pointsToDraw :: PrimMonad m => Canvas (PrimState m) -> P2 -> m [P2]
pointsToDraw canvas (x,y) = do
    let validPoints = filter validPoint points
    availablePoints <- untouchedPoints canvas validPoints
    return availablePoints
    where n  = (x,y-1)
          ne = (x+1,y-1)
          e  = (x+1,y)
          se = (x+1,y+1)
          s  = (x,y+1)
          sw = (x-1,y+1)
          w  = (x-1,y)
          nw = (x-1,y-1)
          points = [n,ne,e,se,s,sw,w,nw]

validPoint :: P2 -> Bool
validPoint (x,y)
    | x < 0 || x >= width  = False
    | y < 0 || y >= height = False
    | otherwise            = True

touchedPoints :: PrimMonad m => Canvas (PrimState m) -> [P2] -> m [P2]
touchedPoints canvas points = do
    validPoints <- filterM (touchedPoint canvas) points
    return validPoints

touchedPoint :: PrimMonad m => Canvas (PrimState m) -> P2 -> m Bool
touchedPoint canvas point = do
    (_,_,touched) <- UM.read canvas (index point)
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
    mutableCanvas <- generateCanvas width height 
    frozenCanvas <- UV.freeze mutableCanvas
    let drawInstructions = vecToInstructions frozenCanvas
        white = PixelRGBA8 255 255 255 255
        img = renderDrawing width height white $ sequence_ drawInstructions
    writePng "image.png" img
