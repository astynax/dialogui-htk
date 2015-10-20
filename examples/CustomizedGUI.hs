module Main where

import Control.Monad (void)
import HTk.Toplevel.HTk
import Prelude hiding (interact)

import UI.Dialogui
import UI.Dialogui.HTk

type RGB = (Int, Int, Int)

theme :: (HasColour a, HasFont a) => [Config a]
theme = [ font (Courier, Bold, 18::Int)
        , background ((0x00, 0x00, 0x7F) :: RGB)
        , foreground ((0xFF, 0xFF, 0x00) :: RGB) ]

main :: IO ()
main = runGUIWith opts
       (writeLn "  -=<[ He110! ]>=-")
       $ interact (\s -> "> " ++ s ++ "\n")
  where
    opts = defaultOptions { prepareWindow = set [ text "Customized!" ]
                          , prepareInput  = set   theme
                          , prepareOutput = set $ theme ++ [ size (20, 10) ]
                          }
    set = (void .) . flip configure
