--Projectile Motion Simulation--

import Graphics.Gloss


type Velocity = Vector

--Each pixel is __ m (The bigger the scl, the more you are zooming out)
scl :: Float
scl = 0.01

--The size of the ball is always __ px, which is __ / scl m
ballsize :: Float
ballsize = 20

v ::  Velocity
v = (4/scl,6/scl)

--Planet Earth
g :: Float
g = 9.81/scl

landingTime :: Velocity -> Float
landingTime (vx,vy) = (2*vy)/g

deltax :: Velocity -> Float -> Float
deltax (vx,vy) t = vx*t

deltay :: Velocity -> Float -> Float
deltay (vx,vy) t  = vy*t - 0.5*g*t*t


path :: Float -> Picture
path t | t < landingTime v = Pictures [translate (deltax v t - 250) (deltay v t - 223) (color red (ThickCircle 1 ballsize)),translate 0 (-250) (color green (polygon (rectanglePath 500 50)))]
       | otherwise = Pictures [translate (deltax v (landingTime v) - 250) (deltay v (landingTime v) - 223) (color red (ThickCircle 1 ballsize)),translate 0 (-250) (color green (polygon (rectanglePath 500 50)))]

main :: IO ()
main = animate (InWindow "Simulation" (500,500) (20,20)) white path
