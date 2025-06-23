module Rel.MeshRel.Sphere.Hit (hitSphere) where

import Rel.MeshRel.IHit.Base
import Struct.Ray
import Struct.Vector.Vec3
import SceneDescriptor.Attribute.Mesh.Primitive.Sphere (Sphere (..))
import SceneDescriptor.Attribute.Mesh.Base (Mesh(..))

hitSphere :: Sphere -> Ray -> HitData
hitSphere s r
  | discriminant < 0 = iHitData (-1.0) (ivec3 0 0 0) (ivec3 0 0 0)
  | firstRoot > 0 = iHitData firstRoot hitPoint normalVector
  | secondRoot > 0 = iHitData secondRoot hitPoint2 normalVector2
  | otherwise = iHitData (-1.0) (ivec3 0 0 0) (ivec3 0 0 0)
  where
    oc = origin r - position s
    a = dot (direction r) (direction r)
    b = 2.0 * dot oc (direction r)
    c = dot oc oc - radius s * radius s
    discriminant = b * b - 4 * a * c

    firstRoot = (-b - sqrt discriminant) / (2 * a)
    secondRoot = (-b + sqrt discriminant) / (2 * a)

    hitPoint = pointAtT r firstRoot
    normalVector = makeUnitVector (hitPoint - position s)

    hitPoint2 = pointAtT r secondRoot
    normalVector2 = makeUnitVector (hitPoint2 - position s)
