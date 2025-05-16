module Utils.Vector.Vec3
  ( Vec3 (..),
    ivec3,
    x,
    y,
    z,
    r,
    g,
    b,
    (!.),
    updateAt,
    (+=),
    (-=),
    (*=),
    (//=),
    (+.),
    (-.),
    (*.),
    (/.),
    squaredLength,
    vectorLength,
    makeUnitVector,
    dot,
    cross,
  )
where

import Data.Vector.Unboxed (Vector, (!), (//))
import qualified Data.Vector.Unboxed as VU

newtype Vec3 = Vec3 {vector_elements :: Vector Float} deriving (Show, Eq)

ivec3 :: Float -> Float -> Float -> Vec3
ivec3 e0 e1 e2 = Vec3 $ VU.fromList [e0, e1, e2]

x :: Vec3 -> Float
x (Vec3 e) = e ! 0

y :: Vec3 -> Float
y (Vec3 e) = e ! 1

z :: Vec3 -> Float
z (Vec3 e) = e ! 2

r :: Vec3 -> Float
r = x

g :: Vec3 -> Float
g = y

b :: Vec3 -> Float
b = z

instance Num Vec3 where
  (Vec3 a) + (Vec3 b) = Vec3 $ VU.zipWith (+) a b
  (Vec3 a) - (Vec3 b) = Vec3 $ VU.zipWith (-) a b
  (Vec3 a) * (Vec3 b) = Vec3 $ VU.zipWith (*) a b
  negate (Vec3 a) = Vec3 $ VU.map negate a

  abs (Vec3 a) = Vec3 $ VU.map abs a
  signum (Vec3 a) = Vec3 $ VU.map signum a
  fromInteger n = Vec3 $ VU.replicate 3 (fromInteger n)

(!.) :: Vec3 -> Int -> Float
(Vec3 e) !. i = e ! i

updateAt :: Int -> Float -> Vec3 -> Vec3
updateAt i val (Vec3 e) = Vec3 $ e // [(i, val)]

(+=) :: Vec3 -> Vec3 -> Vec3
a += b = a + b

(-=) :: Vec3 -> Vec3 -> Vec3
a -= b = a - b

(*=) :: Vec3 -> Vec3 -> Vec3
a *= b = a * b

(//=) :: Vec3 -> Vec3 -> Vec3
(Vec3 a) //= (Vec3 b) = Vec3 $ VU.zipWith (/) a b

infixl 6 +., -.

infixl 7 *., /.

(+.) :: Vec3 -> Float -> Vec3
(Vec3 e) +. t = Vec3 $ VU.map (+ t) e

(-.) :: Vec3 -> Float -> Vec3
(Vec3 e) -. t = Vec3 $ VU.map (t -) e

(*.) :: Vec3 -> Float -> Vec3
(Vec3 e) *. t = Vec3 $ VU.map (* t) e

(/.) :: Vec3 -> Float -> Vec3
(Vec3 e) /. t = Vec3 $ VU.map (/ t) e

squaredLength :: Vec3 -> Float
squaredLength (Vec3 e) = VU.foldl' (\acc i -> acc + i * i) 0 e

vectorLength :: Vec3 -> Float
vectorLength v = sqrt (squaredLength v)

makeUnitVector :: Vec3 -> Vec3
makeUnitVector v = v /. vectorLength v

dot :: Vec3 -> Vec3 -> Float
dot (Vec3 a) (Vec3 b) = VU.ifoldl' (\acc i _ -> acc + ((a ! i) * (b ! i))) 0 a

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a) (Vec3 b) =
  Vec3 $
    VU.fromList
      [ (a VU.! 1) * (b VU.! 2) - (a VU.! 2) * (b VU.! 1),
        (a VU.! 2) * (b VU.! 0) - (a VU.! 0) * (b VU.! 2),
        (a VU.! 0) * (b VU.! 1) - (a VU.! 1) * (b VU.! 0)
      ]
