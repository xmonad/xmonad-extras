-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Prompt.Eval
-- Copyright   :  Daniel Schoepe <daniel.schoepe@gmail.com>
-- License     :  BSD3
--
-- Maintainer  :  Daniel Schoepe <daniel.schoepe@gmail.com>
-- Stability   :  unstable
-- Portability :  unportable
--
-- A prompt for evaluating Haskell expressions(in the context of the running
-- xmonad instance).
--
-----------------------------------------------------------------------------

module XMonad.Prompt.Eval (
                          -- * Usage
                          -- $usage
                           evalPrompt
                          ,evalPromptWithOutput
                          ,showWithDzen
                          ) where

import XMonad
import XMonad.Prompt
import XMonad.Actions.Eval
import XMonad.Util.Dzen

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Prompt
-- > import XMonad.Prompt.Eval
--
-- in your keybindings add:
--
-- >   , ((modMask x .|. controlMask, xK_x), evalPrompt defaultEvalConfig)
--
-- For detailed instruction on editing the key binding see
-- "XMonad.Doc.Extending#Editing_key_bindings".

data EvalPrompt = EvalPrompt

instance XPrompt EvalPrompt where
    showXPrompt EvalPrompt = "Eval: "

-- | A prompt that evaluates the entered Haskell expression, whose type has
-- to be an instance of Show.
evalPrompt :: EvalConfig -> XPConfig -> X ()
evalPrompt evc c = evalPromptWithOutput evc c (const $ return ())

-- | The same as 'evalPrompt', but lets the user supply a function to be
-- executed on the returned string, which is produced by applying show
-- to the executed expression. (This is a crude solution, but the returned
-- type has to be monomorphic)
evalPromptWithOutput :: EvalConfig -> XPConfig -> (String -> X ()) -> X ()
evalPromptWithOutput evc c f =
  flip whenJust f =<< mkXPromptWithReturn EvalPrompt c (const $ return []) (evalExpressionWithReturn evc)

-- | A nice default to have the result of an expression displayed by dzen,
-- if it's interesting (i.e. not () or an empty string).
-- The first parameter specifies the display time in microseconds, the second parameter
-- allows to pass additional options to dzen.
showWithDzen :: Int -> [String] -> String -> X ()
showWithDzen t args "Error" = dzenWithArgs "Error" (["-bg","#ff0000","-fg","#000000"]++args) t
showWithDzen t args s | s `elem` ["","()"] = return ()
                      | otherwise = dzenWithArgs s (["-bg","#00c600","-fg","#000000"]++args) t
