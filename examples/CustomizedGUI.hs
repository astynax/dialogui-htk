module Main where

import Control.Monad (void)
import HTk.Toplevel.HTk
import Prelude hiding (interact)

import UI.Dialogui
import UI.Dialogui.HTk

main :: IO ()
main = runGUIWith opts [] $ interact id
  where
    opts = defaultOptions { prepareWindow = set [ text "Customized!" ]
                          , prepareOutput = set [ size (80, 25) ]}
    set = (void .) . flip configure
