module SceneDescriptor.Attribute.Camera.Base (
    Camera (..),
    CameraLike (getRay),
    icam,
) where

import Struct.Ray
import Struct.Vector.Vec3

data Camera = Camera {
	camOrigin :: Vec3,
	lower_left_corner :: Vec3,
	horizontal :: Vec3,
	vertical :: Vec3
}

icam :: Vec3 -> Vec3 -> Vec3 -> Vec3 -> Camera
icam = Camera

class CameraLike a where
    getRay :: a -> Float -> Float -> Ray

instance CameraLike Camera where
    getRay :: Camera -> Float -> Float -> Ray
    getRay camera u v =
        iray
            (camOrigin camera)
            (lower_left_corner camera + horizontal camera *. u + vertical camera *. v)
