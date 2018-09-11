import System.Environment
import Data.List
import Data.List.Split

import SvgShapes --(Rect, Circle, Line, toFile)

{-
This is some tedious linear algebra to
find a circumcircle of a set of points.
We start by finding a circle from three points.
http://www.ambrsoft.com/TrigoCalc/Circle3D.htm

This works by creating a set of linear equations, then
finding the determinant of the 4x4 matrix that describes
those equations. It's just a messy bit of algebra.
-}
type FCircle = (Float, Float, Float)
type FPoint = (Float, Float)
findCircle :: FPoint -> FPoint -> FPoint -> FCircle
findCircle (x1,y1) (x2,y2) (x3,y3) =
  let a = x1*(y2-y3) - (y1*(x2-x3)) + (x2*y3) - (x3*y2)
      b = (((x1*x1)+(y1*y1)) * (y3-y2)) +
          (((x2*x2)+(y2*y2)) * (y1-y3)) +
          (((x3*x3)+(y3*y3)) * (y2-y1))
      c = (((x1*x1)+(y1*y1)) * (x2-x3)) +
          (((x2*x2)+(y2*y2)) * (x3-x1)) +
          (((x3*x3)+(y3*y3)) * (x1-x2))
      d = (x1*x1+y1*y1)*(x3*y2-x2*y3) +
          (x2*x2+y2*y2)*(x1*y3-x3*y1) +
          (x3*x3+y3*y3)*(x2*y1-x1*y2)
      x = -b/(2*a)
      y = -c/(2*a)
      r = sqrt ((b*b+c*c-(4*a*d))/(4*a*a))
      in (x,y,r)

allTriples xs = [(x,y,z) | x<-xs, y<-xs, z<-xs,
                 x/=y,y/=z,z/=x]
allCircles :: [FPoint] -> [FCircle]
allCircles xs= map (\(p1,p2,p3) -> findCircle p1 p2 p3) (allTriples xs)

insideCircle :: FCircle -> FPoint -> Bool
insideCircle (cx,cy,r) (x,y) = sqrt ((dx*dx) + (dy*dy)) <= r
  where dx = cx - x
        dy = cy - y

allInsideCircle :: [FPoint] -> FCircle -> Bool
allInsideCircle points circle= all (insideCircle circle) points

circumcircle :: [FPoint] -> [FCircle] -> [FCircle]
circumcircle points circles = filter (allInsideCircle points) circles

points=[(0.4,0.5),
        (0.6,0.5),
        (0.5,0.3),
        (0.5,0.7)]

circles = allCircles points

scale x = floor $ x*500
scalePoint (x,y) = P (scale x)  (scale y)
scaleCircle (cx,cy,r) = (scale cx, scale cy, scale r)
svgPoints :: [(Float, Float)] -> [Shape]
svgPoints points= [Rect (scalePoint p) 1 1 | p <- points]

circleToSVG (cx,cy,r) = Circle (scalePoint (cx,cy)) (scale r)

svgCircles :: [(Float, Float, Float)] -> [Shape]
svgCircles circles = [circleToSVG c | c <- circles]

svgShapes = (svgPoints points)  ++ (svgCircles circles)
file="circles.svg"

printList :: Show a => [a] -> IO ()
printList [] = return ()
printList (x:xs) = do
  putStrLn $ show x
  printList xs

parseLine :: String -> (Float,Float)
parseLine s = (read (ss!!0), read (ss!!1))
              where ss = splitOn " " s

numPointsInCircle :: [FPoint] -> FCircle -> Int
numPointsInCircle [] _ = 0
numPointsInCircle (p:ps) circle  =
  if insideCircle circle p
  then 1 + numPointsInCircle ps circle
  else numPointsInCircle ps circle

circlesWithNumPoints n points circles =
  filter (\c -> (numPointsInCircle points c) == n) circles

isInSquare x1 y1 x2 y2 (cx,cy,r) =
  cx - r >= x1 &&
  cx + r <= x2 &&
  cy - r >= y1 &&
  cy + r <= y2

main = do
  (fn:_) <- getArgs
  pointLines <- fmap lines (readFile fn)
  let points = map parseLine pointLines
      halfNumPoints = (length points) `div` 2
      circles = allCircles points
      circlesInUnitSquare = filter (isInSquare 0.0 0.0 1.0 1.0) circles
      circlesWithHalfPoints = nub $ circlesWithNumPoints
                              halfNumPoints
                              points 
                              circlesInUnitSquare
    in do
    toFile file ((svgCircles circlesWithHalfPoints)++(svgPoints points))
    if circlesWithHalfPoints /= []
      then printList circlesWithHalfPoints
      else putStrLn "No solution"
