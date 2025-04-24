module Lib.Vector.Vec3 (Vec3 (..), x, y, z, r, g, b, makeUnitVector, dot, cross) where

newtype Vec3 = Vec3 { elements :: [Float] } deriving (Show, Eq)

vec3 :: Float -> Float -> Float -> Vec3
vec3 e0 e1 e2 = Vec3 [e0, e1, e2]

x :: Vec3 -> Float
x (Vec3 e) = head e

y :: Vec3 -> Float
y (Vec3 e) = e !! 1

z :: Vec3 -> Float
z (Vec3 e) = e !! 2

r :: Vec3 -> Float
r = x

g :: Vec3 -> Float
g = y

b :: Vec3 -> Float
b = z

instance Num Vec3 where
    (Vec3 a) + (Vec3 b) = Vec3 [head a + head b, a !! 1 + b !! 1, a !! 2 + b !! 2]
    (Vec3 a) - (Vec3 b) = Vec3 [head a - head b, a !! 1 - b !! 1, a !! 2 - b !! 2]
    negate (Vec3 a) = Vec3 [negate (head a), negate (a !! 1), negate (a !! 2)]
    (Vec3 a) * (Vec3 b) = Vec3 [head a * head b, a !! 1 * b !! 1, a !! 2 * b !! 2]
    
    abs (Vec3 a) = Vec3 [abs (head a), abs (a !! 1), abs (a !! 2)]
    signum (Vec3 a) = Vec3 [signum (head a), signum (a !! 1), signum (a !! 2)]
    fromInteger n = Vec3 [fromInteger n, fromInteger n, fromInteger n]

(!.) :: Vec3 -> Int -> Float
(Vec3 e) !. i = e !! i

updateAt :: Int -> Float -> Vec3 -> Vec3
updateAt i val (Vec3 e) = Vec3 (take i e ++ [val] ++ drop (i+1) e)

(+=) :: Vec3 -> Vec3 -> Vec3
a += b = a + b

(-=) :: Vec3 -> Vec3 -> Vec3
a -= b = a - b

(*=) :: Vec3 -> Vec3 -> Vec3
a *= b = a * b

(/=) :: Vec3 -> Vec3 -> Vec3
(Vec3 a) /= (Vec3 b) = Vec3 [head a / head b, a !! 1 / b !! 1, a !! 2 / b !! 2]

(*.) :: Vec3 -> Float -> Vec3
(Vec3 a) *. t = Vec3 [head a * t, a !! 1 * t, a !! 2 * t]

(/.) :: Vec3 -> Float -> Vec3
(Vec3 a) /. t = Vec3 [head a / t, a !! 1 / t, a !! 2 / t]

squaredLength :: Vec3 -> Float
squaredLength (Vec3 e) = head e * head e + e !! 1 * e !! 1 + e !! 2 * e !! 2

length :: Vec3 -> Float
length v = sqrt (squaredLength v)

makeUnitVector :: Vec3 -> Vec3
makeUnitVector v = v /. Lib.Vector.Vec3.length v

dot :: Vec3 -> Vec3 -> Float
dot (Vec3 a) (Vec3 b) = head a * head b + a !! 1 * b !! 1 + a !! 2 * b !! 2

cross :: Vec3 -> Vec3 -> Vec3
cross (Vec3 a) (Vec3 b) = Vec3 [
    a !! 1 * b !! 2 - a !! 2 * b !! 1,
    a !! 2 * head b - head a * b !! 2,
    head a * b !! 1 - a !! 1 * head b]
