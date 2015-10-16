{-# LANGUAGE LambdaCase #-}
-- | Simple <https://en.wikipedia.org/wiki/Tk_%28software%29 Tk>-based GUI.

module UI.Dialogui.HTk
       ( runGUI, runGUIWith
       , prepareInput, prepareOutput, prepareWindow
       , defaultOptions
       ) where

import           Control.Monad    (void)
import           Data.IORef       (newIORef, readIORef, writeIORef)
import           HTk.Toplevel.HTk hiding (EndOfText)
import qualified HTk.Toplevel.HTk as H

import           UI.Dialogui      hiding (perform)


data GUIOptions =
  GUIOptions { prepareWindow :: HTk          -> IO ()
             , prepareInput  :: Entry String -> IO ()
             , prepareOutput :: Editor       -> IO ()
             }


{- | Returns the 'UI.Dialogui.UI'
(built using 'runGUIWith' + 'defaultOptions')
-}
runGUI :: String      -- ^ Window title
       -> [Action a]  -- ^ Setup
       -> UI a        -- ^ Resulting UI
runGUI title = runGUIWith
               $ defaultOptions { prepareWindow =
                                     void . flip configure [text title] }


{- | Default GUI options -}
defaultOptions :: GUIOptions
defaultOptions = GUIOptions nop nop nop
  where nop = const $ return ()


{- | Builds a GUI then runs some setup on it and returns 'UI.Dialogui.UI'
-}
runGUIWith :: GUIOptions -- ^ Options for window customizations
           -> [Action a] -- ^ List of actions which performs some setup
           -> UI a       -- ^ Resulting UI
runGUIWith opts setup ctl = do
  main <- initHTk []
  prepareWindow opts main

  refState <- newIORef =<< initialize ctl

  entry           <- newEntry  main [] :: IO (Entry String)
  (outFrame, out) <- newOutput main

  prepareInput  opts entry

  pack entry    [ Fill   X ]
  pack outFrame [ Fill   Both
                , Expand On
                ]

  let exit = readIORef refState >>= finalize ctl >> destroy main
      perform = mapM_ $ \case
        Write msg   -> writeTo out msg
        ClearOutput -> clearOutput out
        ScrollTo t  -> out `scrollTo` t
        SetState s  -> writeIORef refState s
        SetInput i  -> void $ entry # value i
        Quit        -> exit

  onCtrlD  <- hotkey entry [Control] "d"
  onReturn <- hotkey entry []        "Return"

  void $ spawnEvent $ forever

    $  onCtrlD  >>> exit

    +> onReturn >>> do
      cmd      <- getValue entry :: IO String

      oldState <- readIORef refState
      actions  <- communicate ctl oldState cmd

      perform actions

  perform setup

  setFocus entry

  finishHTk

  where
    hotkey w mods key =
      fst <$> bind w [WishEvent mods $ KeyPress $ Just $ KeySym key]

    newOutput :: Container a => a -> IO (Frame, Output (IO ()))
    newOutput cont = do
      frame <- newFrame     cont  []
      sb    <- newScrollBar frame []
      ed    <- newEditor    frame [ scrollbar Vertical sb
                                  , wrap      WordWrap
                                  , width     10
                                  , disable
                                  ]
      prepareOutput opts ed

      pack ed [ Side   AtLeft
              , Fill   Both
              , Expand On
              ]
      pack sb [ Side   AtRight
              , Fill   Y
              ]

      return ( frame
             , Output { writeTo = enabling ed
                                  . appendText ed
                      , clearOutput = enabling ed
                                      $ deleteTextRange ed
                                      ((0, 0) :: Position) H.EndOfText
                      , scrollTo = moveto Vertical ed
                                   . \case
                                     BeginOfText -> 0
                                     EndOfText   -> 1
                      })


-- | Temporarily enables the widget until the completion of action
enabling :: HasEnable a => a -> IO b -> IO ()
enabling w action = void $
  configure w [enable]
  >> action
  >> configure w [disable]
