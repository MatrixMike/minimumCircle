{- Emit SVG shapes -}

module SvgShapes 
where

bluestroke = " stroke=\"blue\""
redstroke = " stroke=\"red\""

class SVG a where
  svg :: a -> String

data Point = P Int Int
instance Show Point where
  show (P x y) = "(" ++ (show x) ++ "," ++ (show y) ++ ")"
instance SVG Point where
  svg p = svg (Rect p 1 1)

data Shape = Line Point Point
           | Rect Point Int Int
           | Circle Point Int
              deriving (Show)

instance SVG Shape where
  svg (Line (P x1 y1) (P x2 y2)) =
          "<line "
          ++ "x1=\"" ++ (show x1) ++ "\" "
          ++ "y1=\"" ++ (show y1) ++ "\" "
          ++ "x2=\"" ++ (show x2) ++ "\" "
          ++ "y2=\"" ++ (show y2) ++ "\" "
          ++ " width = \"1\""++bluestroke++"/>"
  svg (Rect (P x y) w h) =
    "<rect x =\""
    ++ (show x)
    ++ "\" y=\""
    ++ (show y) ++ "\" "
    ++ "width = \""++(show w)++"\" "
    ++ "height = \""++(show h)++"\""++redstroke++"/>"
  svg (Circle (P x y) r) =
    "<circle "
    ++ "cx = \"" ++ (show x)
    ++ "\" cy=\"" ++ (show y)
    ++ "\" r=\""
    ++ (show r)
    ++ "\" fill=\"none\" "++bluestroke++"/>"

toSvg :: [Shape] -> String
toSvg shapes= "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">\n"
              ++ (unlines $ map svg shapes) ++ "</svg>\n"

toFile :: String -> [Shape] -> IO()
toFile f shapes = writeFile f $ toSvg shapes

p1 = P 100 200
p2 = P 300 400
l1= Line p1 p2
c1 = Circle p2 50

shapes = [(Rect (P 100 75)) 1 1,  l1, c1]

s = toSvg shapes
