module Internal 
    ( World(..)
    , Ray(..)
    , Camera(..)
    , Hit(..)
    , ShapeT(..)
    , Shape(..)
    , MaterialT(..)
    , Material(..)
    , debug
    , reflect
    , norm2
    , vecToPixel
    , randInUnitSphere
    , randOnUnitSphere
    , intersectAll ) where

import System.Random
import Data.Vec3
import Codec.Picture (PixelRGBF(..))
import GHC.Float (double2Float)
import Data.List (head, sort)
import Debug.Trace

-- | The 'World' data type contains everything that makes up the scene.
data World = World {
    rndGen      :: StdGen,       -- ^ The current state of the random number generator.
    imageSize   :: (Int, Int),   -- ^ (Width, Height) The size of the image to generate.
    worldShapes :: [Shape],      -- ^ A list of all intersectable objects in the scene.
    camera      :: Camera        -- ^ The camera from which the scene will be rendered.
} deriving (Show)

-- | Ray contains an origin and direction vector
data Ray  = Ray CVec3 CVec3 deriving (Show)

-- | The 'Camera' data type does not keep track of the camera's origin, direction, 
--   or up vectors. Instead, these parameters are passed to a constructor function.
--   That function returns the 'getRay' function that is used to figure out where
--   the ray should start and its direction for a given pixel. 
data Camera = Camera {
    -- | 'getRay' takes two doubles that vary from 0 to 1. This is the normalized
    --    pixel, meaning that zero corresponds to the left (or top) of the image,
    --    and 1 corresponds to the right (or bottom).
    getRay :: Double -> Double -> Ray
}

instance Show Camera where
    show x = "Camera"

-- | 'Hit' encapsulates the state of an intersection.
--   Hit distance point normal material
data Hit = Hit Double CVec3 CVec3 Material deriving (Show)

instance Eq Hit where
    Hit t1 _ _ _ == Hit t2 _ _ _ = t1 == t2

instance Ord Hit where
    Hit t1 _ _ _ `compare` Hit t2 _ _ _ = t1 `compare` t2


-- | Used to track the state of a 'Shape' data type. The values
--   stored in 'ShapeT' are not actually currently used.
data ShapeT = Sphere CVec3 Double          -- | Sphere center radius
            | Triangle CVec3 CVec3 CVec3   -- | Triangle vert1 ver2 ver3
            deriving (Show)

-- | The 'Shape' data type does not need to keep track of any geometric
--   properties of the underlying shape. It only cares about the function
--   that tells whether a ray intersects the shape or not.
data Shape = Shape {
    -- | 'intersect' takes a Ray and a (min distance, max distance) and determines
    --    if an intersection occured along the way in the range (minDist, maxDist).
    intersect :: Ray -> (Double, Double) -> Maybe Hit,
    originalShape :: ShapeT  -- | Keeps track of the original shape, but not needed.
}

instance Show Shape where
    show = show . originalShape

-- | Used to keep track of the underlying material, but is not actually used.
data MaterialT  = Lambertian CVec3   -- | A material that scatters light randomly.
                | Metal CVec3        -- | A material that reflects light specularly.
                deriving (Show)

-- | The 'Material' data type doesn't care about the underlying material. The 
--   'scatter' function contains everything needed for ray-material interaction.
data Material = Material {
    -- | Scatter takes a Ray and Hit (and StdGen) to determine how light
    --   interacts with the material.
    --   Produces a (color, scattered ray, updated random number generator).
    scatter :: Ray -> Hit -> StdGen -> (CVec3, Ray, StdGen),
    matType :: MaterialT    -- | Keeps track of the underlying material.
}

instance Show Material where
    show = show . matType

-- | 'debug' function used print out data from a function guard.
--   Usage: debug "Message"
debug = (flip trace) False

reflect :: CVec3 -> CVec3 -> CVec3
reflect v n = v <-> n .^ ((v .* n) * 2)
        

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



intersectAll :: [Shape] -> Ray -> (Double, Double) -> Maybe Hit
intersectAll shapes ray dist = mbeHead $ sort mbeHits
    where
        tmpIntersect r d s = intersect s r d
        mbeHits            = map (tmpIntersect ray dist) shapes
        mbeHead :: [Maybe a] -> Maybe a
        mbeHead []       = Nothing
        mbeHead (x : xs) = case x of Nothing -> mbeHead xs
                                     _       -> x
     
intersectAllTest :: [Shape] -> Ray -> (Double, Double) -> [Maybe Hit]
intersectAllTest shapes ray dist = mbeHits
    where
        tmpIntersect r d s = intersect s r d
        mbeHits            = map (tmpIntersect ray dist) shapes

aLotOfRandTest :: StdGen -> [(Double, Double, Double)]
aLotOfRandTest g = 
    let (v, g') = randInUnitSphere g
    in (toXYZ v) : aLotOfRandTest g'