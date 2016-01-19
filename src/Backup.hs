{-# OPTIONS -Wall #-}
module Main where

import Codec.Picture( PixelRGBA8( .. ), writePng )
import Graphics.Rasterific
import Graphics.Rasterific.Texture
import System.Random
import qualified Data.Vector.Mutable as M
import qualified Data.Vector as V
import Control.Monad.ST
import Control.Monad.Primitive
import System.Random

type Width = Int
type Height = Int

data Pixel = Pixel { coord :: Point
                   , color :: PixelRGBA8
                   } deriving (Show)

generatePixels :: Width -> Height -> [Pixel]
generatePixels w h = 
    [Pixel {coord=V2 (fromIntegral j) (fromIntegral i), color=white} | i <- [0..h-1], j <- [0..w-1]]
        where white = PixelRGBA8 255 255 255 255

pixels :: Canvas -> [Drawing PixelRGBA8 ()]
pixels c = createPixelDrawing <$> V.toList (vector c)

createPixelDrawing :: Pixel -> Drawing PixelRGBA8 ()
createPixelDrawing p = withTexture (uniformTexture (color p)) . fill $ rectangle (coord p) 1 1

data Canvas = Canvas { vector :: V.Vector Pixel
                     , width  :: Int
                     , height :: Int
                     } deriving (Show)

generateCanvas :: Width -> Height -> Canvas
generateCanvas w h = Canvas { vector = V.fromList $ generatePixels w h
                        , width  = w
                        , height = h
                        }
type XPos = Int
type YPos = Int

getPixelAtIndex :: XPos -> YPos -> Canvas -> Pixel
getPixelAtIndex x y c 
    | x > width c - 1 || y > height c - 1 = error "Index array out of bounds"
getPixelAtIndex x y c = (vector c) V.! idx
    where idx = (width c * y) + x

--setPixelAtIndex :: XPos -> YPos -> Canvas -> Pixel -> Canvas
--setPixelAtIndex x y c _
--    | x > width c - 1 || y > height c - 1 = error "Index array out of bounds"
--setPixelAtIndex x y c p = c { vector = first V.++ V.fromList [p] V.++ last}
--    where idx = (width c * y) + x
--          first = V.take idx (vector c)
--          last = V.drop (idx+1) (vector c)

--writePixel :: PrimMonad m => [Coord] -> Pixel -> m Canvas
--writePixel coords p = do 
--    M.write 

setPixelAtIndex :: XPos -> YPos -> Canvas -> Pixel -> Canvas
setPixelAtIndex x y c _
    | x > width c - 1 || y > height c - 1 = error "Index array out of bounds"
setPixelAtIndex x y c p = 
    V.modify (M.write (vector c) idx p) (vector c)
        where idx = (width c * y) + x
    --runST $ do
    --mV <- V.thaw $ vector c
    --let idx = (width c * y) + x
    --M.write mV idx p
    --mF <- V.freeze mV
    --return $ c {vector = mF}
    
setPixelColorHelper :: Canvas -> PixelRGBA8 -> Coord -> Canvas
setPixelColorHelper canvas c (x,y) = setPixelAtIndex x y canvas newPixel
    where oldPixel = getPixelAtIndex x y canvas
          newPixel  = oldPixel { color = c }

setPixelColor :: Canvas -> PixelRGBA8 -> [Coord] -> Canvas
setPixelColor c _ []     = c
setPixelColor c p (x:xs) = setPixelColor newCanvas p xs
    where newCanvas = setPixelColorHelper c p x

type Range = (Int, Int)
type Coord = (Int, Int)

randomCoord :: StdGen -> Range -> (Coord, StdGen)
randomCoord g (xr,yr) = ((f,s), g'')
    where (f, g' ) = randomR (0,xr-1) g
          (s, g'') = randomR (0,yr-1) g'

randomCoords :: Int -> StdGen -> Range -> [Coord]
randomCoords 0 _ _ = []
randomCoords n g (xr,yr) = c : randomCoords (n-1) g' (xr,yr)
    where (c, g') = randomCoord g (xr,yr)

main :: IO ()
main = do
    gen <- getStdGen
    let white     = PixelRGBA8 255 255 255 255
        width     = 1280
        height    = 720
        canvas    = generateCanvas width height
    mV <- V.thaw $ vector canvas
    let coords    = randomCoords 100 gen (width, height)
        newCanvas = setPixelColor canvas (PixelRGBA8 0 0 0 255) coords
        img       = renderDrawing width height (PixelRGBA8 255 255 255 255) $ sequence_ (pixels newCanvas)
    writePng "image.png" img

