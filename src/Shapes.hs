module Shapes
    ( makeSphere ) where

import Internal
import Data.Vec3

makeSphere :: CVec3 -> Double -> Material -> Shape
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