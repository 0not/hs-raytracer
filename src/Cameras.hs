module Cameras
    ( makeCamera
    ) where

import Data.Vec3
import Internal

makeCamera :: CVec3 -> CVec3 -> CVec3 -> Double -> Double -> Camera
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
    in Camera { getRay = makeRay }

    