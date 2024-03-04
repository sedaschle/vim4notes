module Main where


import Brick
import Graphics.Vty
import Graphics.Vty.Platform.Windows (mkVty)


ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui