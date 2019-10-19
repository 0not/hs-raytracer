{-# LANGUAGE BangPatterns #-}
module Main where

import Materials
import Cameras
import Shapes
import Internal

import Codec.Picture
import System.Environment (getArgs)
import Data.Vec3
import Data.List
import System.Random


-- Set up the scene
width  = 300 :: Int
height = 200 :: Int
r1 = 1
r2 = 1
r3 = 1000
r4 = 1
c1 = CVec3 0 r1 0
c2 = CVec3 2.5 r2 0
c3 = CVec3 0 (-r3) 0
c4 = CVec3 (-2.5) r4 0
--mat1 = (makeLambertian (CVec3 0.8 0.8 0.2)) :: Material
mat1 = (makeMetal (CVec3 0.9 0.9 0.9)) :: Material
mat2 = (makeLambertian (CVec3 0.9 0.5 0.5)) :: Material
mat3 = (makeLambertian (CVec3 0.1 0.9 0.1)) :: Material
mat4 = mat1

camOrigin = CVec3 0 2 10
camLookAt = CVec3 0 0 (-1)
camUp     = CVec3 0 1 0

cam :: Camera
cam = makeCamera camOrigin camLookAt camUp 40 ((fromIntegral width) / (fromIntegral height))

world :: World
world = World { 
    imageSize   = (width, height), 
    worldShapes = [makeSphere c1 r1 mat1, makeSphere c2 r2 mat2, makeSphere c3 r3 mat3, makeSphere c4 r4 mat4], 
    rndGen      = mkStdGen 1,
    camera      = cam
}

updateRndGen :: World -> StdGen -> World
updateRndGen w g' = w { rndGen = g' }



main :: IO ()
main =
    let bounds = (-1, 1) :: (Double, Double)
    in do 
        [path] <- getArgs
        print $ (randomR bounds $ rndGen world)
        let (imgFunc, g) = generateImg world
        savePngImage path imgFunc

generateImg :: World -> (DynamicImage, StdGen)
generateImg world = (ImageRGBF img, g')
    where
        (world', img) = generateFoldImage getPixel world w h
        g' = rndGen world'
        (w, h) = imageSize world
        g = rndGen world

-- | Turns consecutive elements into tuple.
--   From numeric-prelude
sliceVertPair :: [a] -> [(a, a)]
sliceVertPair (x0:x1:xs) = (x0, x1) : sliceVertPair xs
sliceVertPair [] = []
sliceVertPair _ = error "odd number of elements"

-- |Numerically stable mean
--  From hstats
avgColor :: [CVec3] -> CVec3
avgColor x = fst $ foldl' addElement (CVec3 0 0 0, 0) x
    where
        addElement (!m, !n) x = (m <+> (x<->m) .^ (1/(n + 1)), n + 1)

getPixel :: World -> Int -> Int -> (World, PixelRGBF)
getPixel world x y =
    let (w, h)    = imageSize world
        bnds      = (-0.5, 0.5) :: (Double, Double)
        g         = rndGen world
        (g1, g2)  = split g
        rnds      = randomRs bnds g1
        nWorld1   = updateRndGen world g2
        horiz a   = (fromIntegral x + a) / (fromIntegral w)
        vert b    = (1 - (fromIntegral y + b) / (fromIntegral h))
        ray (a,b) = getRay (camera world) (horiz a) (vert b)
        col :: World -> (Double, Double) -> (CVec3, StdGen)
        col w ab  = getColor (ray ab) w 0
        cols :: World -> [(Double, Double)] -> [(CVec3, StdGen)]
        cols w ab = newElem : (cols (updateRndGen w $ snd newElem) (tail ab))
            where
                newElem = col w (head ab)
        --cols      = [getColor (ray ab) nWorld1 0 | ab <- take 50 $ sliceVertPair rnds]
        newCols   = take 10 (cols nWorld1 $ sliceVertPair rnds)
        nWorld2   = updateRndGen nWorld1 $ (snd . last) newCols
    in (nWorld2, vecToPixel (avgColor (map fst newCols)))

getColorMult :: [Ray] -> World -> Int -> (CVec3, StdGen)
getColorMult rays w d = avgColorLastGen (colorList rays w d)
    where
        avgColorLastGen :: [(CVec3, StdGen)] -> (CVec3, StdGen)
        avgColorLastGen x = (avgColor (map fst x), (snd . last) x)
        colorList :: [Ray] -> World -> Int -> [(CVec3, StdGen)]
        colorList (r:[]) w d = [getColor r w d]
        colorList (r:rs) w d = newElem : colorList rs (updateRndGen w $ snd newElem) d
            where 
                newElem = getColor r w d 
        --getColor r w d

scatterMult :: Ray -> Hit -> StdGen -> Int -> [(CVec3, Ray, StdGen)]
scatterMult r h g num = 
    let mat = hitMat h
        splitN :: StdGen -> Int -> [StdGen]
        splitN g 0 = [g]
        splitN g n = let g2 = split g in (fst g2) : splitN (snd g2) (n-1) 
    in map (scatter mat r h) (splitN g num) 

getColor :: Ray -> World -> Int -> (CVec3, StdGen)
getColor ray@(Ray ro rd) world depth = 
    let maxDepth = 10 :: Int
        g      = rndGen world
        hit    = intersectAll (worldShapes world) ray (0.001, 10000)
        onPix  = CVec3 0.5 0.5 0.5
        offPix = CVec3 1.0 1.0 1.0
        times  = Data.Vec3.zipWith (*)
        --times (CVec3 a b c) (CVec3 x y z) = CVec3 (a*x) (b*y) (c*z)
        doMiss = 
            let ud = normalize rd
                (x, y, z) = toXYZ ud
                t = 0.5 * (y + 1.0)
                white = CVec3 1 1 1
                blue  = CVec3 0.5 0.7 1.0
            in (white .^ (1.0 - t) <+> blue .^ t, g)
        doHit (att, scat@(Ray so sd), g') =
            if depth < maxDepth 
            then let (col, g'') = getColor scat (updateRndGen world g') (depth+1)
                    in (att `times` col, g'')
            else (CVec3 0 0 0, g')
    in case hit of Just hit -> doHit (scatter (hitMat hit) ray hit g)
                   Nothing -> doMiss
