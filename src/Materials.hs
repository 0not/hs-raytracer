module Materials
    ( hitMat
    , makeLambertian
    , makeMetal
    ) where

import Data.Vec3
import Internal

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

-- TODO: Only scatter if dot(scattered.direction, rec.normal) > 0
makeMetal :: CVec3 -> Material
makeMetal albedo = 
    let mat = Metal albedo
        scatterFunc (Ray ro rd) (Hit _ p n _) g =
            let reflected = reflect (normalize rd) n
            in (albedo, Ray p reflected, g)
        in Material { scatter = scatterFunc, matType = mat}