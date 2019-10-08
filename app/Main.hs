module Main where

import Codec.Picture
import System.Environment (getArgs)
import Data.Vec3
import System.Random
--import System.Random.TF
--import System.Random.TF.Instances
import Control.Monad (liftM2)
import Data.List (head, sort)

type Pos        = CVec3
type Dist       = Double
data Ray        = Ray Pos Pos deriving (Show)
data ShapeT     = Sphere Pos Dist | Triangle Pos Pos Pos deriving (Show)
data Hit        = Hit Dist Pos Pos Material deriving (Show)
data MaterialT  = Lambertian CVec3 | Metal CVec3 deriving (Show)

data Material = Material {
    matType :: MaterialT
}

data Cam = Cam {
    getRay :: Double -> Double -> Ray
}

data Shape = Shape {
    intersect :: Ray -> (Dist, Dist) -> Maybe Hit,
    originalShape :: ShapeT
}

data World = World {
    rndGen      :: StdGen,
    imageSize   :: (Int, Int),
    worldShapes :: [Shape],
    camera      :: Cam
}

instance Show Material where
    show = show . matType

instance Eq Hit where
    Hit t1 _ _ _ == Hit t2 _ _ _ = t1 == t2

instance Ord Hit where
    Hit t1 _ _ _ `compare` Hit t2 _ _ _ = t1 `compare` t2

instance Show Shape where
    show = show . originalShape

makeCamera :: Pos -> Pos -> Pos -> Double -> Double -> Cam
makeCamera orig dir up vfov aspect = 
    let theta = vfov*pi/180
        half_height = tan(theta/2)
        half_width  = aspect * half_height
        w = normalize (orig <-> dir)
        u = normalize (up >< w)
        v = w >< u
        -- Lower left corner
        --llc = CVec3 (-half_width) (-half_height) (-1.0)
        llc = orig <-> (u .^ half_width) <-> (v .^ half_height) <-> w
        horiz = u .^ (2 * half_width)
        vert  = v .^ (2 * half_height) 
        makeRay s t = Ray orig $ normalize (llc <+> (horiz .^ s) <+> (vert .^ t) <-> orig)
    in Cam { getRay = makeRay }
            

makeSphere :: Pos -> Dist -> Shape
makeSphere center radius = Shape { intersect = sphereIntersect, originalShape = Sphere center radius }
    where
        sphereIntersect (Ray rOrigin rDir) (minDist, maxDist) = if isHit then Just hit else Nothing
            where
                toObj  = center <-> rOrigin
                tca    = rDir .* toObj
                d2     = (norm2 toObj) - (tca * tca)
                thc    = sqrt (radius * radius - d2)
                t      = tca - thc
                point  = rOrigin <+> (rDir .^ t)
                normal = normalize (point <-> center)
                hit    = Hit t point normal (Material $ Lambertian (CVec3 0.8 0.8 0.2))
                isHit  = tca >= 0 && d2 <= radius*radius && t >= minDist && t <= maxDist 

-- | Square of Euclidean norm of a vector.
norm2 :: (Vec3 v) => v -> Double
norm2 v = v .* v
{-# INLINE norm2 #-}

mbeHead :: [Maybe a] -> Maybe a
mbeHead []       = Nothing
--mbeHead [a, b]   = case a of Nothing -> b
--                             _       -> a
mbeHead (x : xs) = case x of Nothing -> mbeHead xs
                             _       -> x

intersectAll :: [Shape] -> Ray -> (Dist, Dist) -> Maybe Hit
intersectAll shapes ray dist = mbeHead $ sort mbeHits
    where
        tmpIntersect r d s = intersect s r d
        mbeHits            = map (tmpIntersect ray dist) shapes
     
intersectAllTest :: [Shape] -> Ray -> (Dist, Dist) -> [Maybe Hit]
intersectAllTest shapes ray dist = mbeHits
    where
        tmpIntersect r d s = intersect s r d
        mbeHits            = map (tmpIntersect ray dist) shapes

-- Set up the scene
width  = 600 :: Int
height = 400 :: Int
c1 = CVec3 0 0 (-10)
c2 = CVec3 2.5 0 (-10)
c3 = CVec3 0 30 10

cam :: Cam
cam = makeCamera (CVec3 0 0 0) (CVec3 0 0 (-1)) (CVec3 0 1 0) 60 ((fromIntegral width) / (fromIntegral height))

world :: World
world = World { 
    imageSize   = (width, height), 
    worldShapes = [makeSphere c1 1, makeSphere c2 1.5],-- makeSphere c3 5], 
    rndGen      = mkStdGen 1,
    camera      = cam
}

main :: IO ()
main =
    let bounds = (-1, 1) :: (Double, Double)
    in do 
        [path] <- getArgs
        print $ (randomR bounds $ rndGen world)
        savePngImage path (generateImg world)

generateImg :: World -> DynamicImage
generateImg world = ImageRGBF (generateImage (getPixel world) w h)
    where
        (w, h) = imageSize world


getPixel :: World -> Int -> Int -> PixelRGBF
getPixel world x y = 
    let (w, h) = imageSize world
        ray    = getRay (camera world) ((fromIntegral x) / (fromIntegral w)) (1 - (fromIntegral y) / (fromIntegral h))
        hit    = intersectAll (worldShapes world) ray (0, 10000)
        onPix  = PixelRGBF 1.0 1.0 1.0
        offPix = PixelRGBF 0 0 0
    in case hit of Just h -> onPix
                   Nothing -> offPix
