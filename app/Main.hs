module Main where

import Codec.Picture
import System.Environment (getArgs)
import Data.Vec3
import System.Random
--import System.Random.TF
--import System.Random.TF.Instances
import Data.List (head, sort)
import GHC.Float (double2Float)
import Debug.Trace

debug = (flip trace) False

type Pos        = CVec3
type Dist       = Double
data Ray        = Ray Pos Pos deriving (Show)
data ShapeT     = Sphere Pos Dist | Triangle Pos Pos Pos deriving (Show)
data Hit        = Hit Dist Pos Pos Material deriving (Show)
data MaterialT  = Lambertian CVec3 | Metal CVec3 deriving (Show)

data Material = Material {
    scatter :: Ray -> Hit -> StdGen -> (CVec3, Ray, StdGen),
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
} deriving (Show)

instance Show Material where
    show = show . matType

instance Eq Hit where
    Hit t1 _ _ _ == Hit t2 _ _ _ = t1 == t2

instance Ord Hit where
    Hit t1 _ _ _ `compare` Hit t2 _ _ _ = t1 `compare` t2

instance Show Shape where
    show = show . originalShape

instance Show Cam where
    show x = "Camera"

--
hitMat :: Hit -> Material
hitMat (Hit _ _ _ m) = m

makeLambertian :: CVec3 -> Material
makeLambertian albedo = 
    let mat = Lambertian albedo
        scatterFunc rayIn (Hit _ p n _) g = 
            let (rnd, g') = randInUnitSphere g
                target    = normalize (n <+> rnd)
            in (albedo, Ray p target, g')
    in Material { scatter = scatterFunc, matType = mat }

reflect :: CVec3 -> CVec3 -> CVec3
reflect v n = v <-> n .^ ((v .* n) * 2)

-- TODO: Only scatter if dot(scattered.direction, rec.normal) > 0
makeMetal :: CVec3 -> Material
makeMetal albedo = 
    let mat = Metal albedo
        scatterFunc (Ray ro rd) (Hit _ p n _) g =
            let reflected = reflect (normalize rd) n
            in (albedo, Ray p reflected, g)
        in Material { scatter = scatterFunc, matType = mat}


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
            

makeSphere :: Pos -> Dist -> Material -> Shape
makeSphere center radius material = Shape { intersect = sphereIntersect, originalShape = Sphere center radius }
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
                hit    = Hit t point normal material
                isHit  = tca >= 0 && d2 <= radius*radius && t >= minDist && t <= maxDist 

-- | Square of Euclidean norm of a vector.
norm2 :: (Vec3 v) => v -> Double
norm2 v = v .* v
{-# INLINE norm2 #-}

vecToPixel :: CVec3 -> PixelRGBF
vecToPixel (CVec3 r g b) = PixelRGBF (d2f r) (d2f g) (d2f b)
    where d2f = double2Float



randInUnitSphere :: StdGen -> (CVec3, StdGen)
randInUnitSphere g = 
    let bounds  = (-1, 1)
        (x, g1) = randomR bounds g
        (y, g2) = randomR bounds g1
        (z, g3) = randomR bounds g2
        v       = (CVec3 x y z) -- .^ 2 <-> CVec3 1 1 1
        len2    = norm2 v
    in if len2 <= 1 then (v, g3) else randInUnitSphere g3
    
randOnUnitSphere :: StdGen -> (CVec3, StdGen)
randOnUnitSphere g = 
    let bounds  = (-1, 1)
        (x1, g1) = randomR bounds g
        (x2, g2) = randomR bounds g1
        x        = 2 * x1 * sqrt (1 - x1^2 - x2^2) 
        y        = 2 * x2 * sqrt (1 - x1^2 - x2^2) 
        z        = 1 - 2 * (x1^2 + x2^2)
        v        = (CVec3 x y z) -- .^ 2 <-> CVec3 1 1 1
        len2     = x1^2 + x2^2
    in if len2 < 1 then (v, g2) else randOnUnitSphere g2

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

aLotOfRandTest :: StdGen -> [(Double, Double, Double)]
aLotOfRandTest g = 
    let (v, g') = randInUnitSphere g
    in (toXYZ v) : aLotOfRandTest g'

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

cam :: Cam
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


getPixel :: World -> Int -> Int -> (World, PixelRGBF)
getPixel world x y =
    let (w, h) = imageSize world
        ray    = getRay (camera world) ((fromIntegral x) / (fromIntegral w)) (1 - (fromIntegral y) / (fromIntegral h))
        (col, g) = getColor ray world 0
        newWorld = updateRndGen world g
    in (newWorld, vecToPixel col)

-- getPixel :: World -> Int -> Int -> (PixelRGBF, StdGen)
-- getPixel world x y
--     | x == 150 && y == 100 && debug (show x) = undefined
--     | otherwise =
--         let (w, h) = imageSize world
--             ray    = getRay (camera world) ((fromIntegral x) / (fromIntegral w)) (1 - (fromIntegral y) / (fromIntegral h))
--             (col, g) = getColor ray world 0
--         in (vecToPixel col, g)

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
        doHit (att, scat@(Ray so sd), g') 
            -- | debug ((show sd) ++ " " ++ (show $ norm sd)) = undefined
            | otherwise = 
                if depth < maxDepth 
                then let (col, g'') = getColor scat (updateRndGen world g') (depth+1)
                     in (att `times` col, g'')
                else (CVec3 0 0 0, g')
        --normCols n = (n <+> CVec3 1 1 1) .^ 0.5  
        --mat    = hitMat hit
        --(attenuation, scattered) = scatter (hitMat hit) ray hit
    in case hit of Just hit -> doHit (scatter (hitMat hit) ray hit g)
                   Nothing -> doMiss

-- vec3 unit_direction = unit_vector(r.direction());
-- float t = 0.5*(unit_direction.y() + 1.0);
-- return (1.0-t)*vec3(1.0, 1.0, 1.0) + t*vec3(0.5, 0.7, 1.0);