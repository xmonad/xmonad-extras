-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Actions.Eval
-- Copyright   :  (c) 2009 Daniel Schoepe
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  Daniel Schoepe <asgaroth_@gmx.de>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Evaluate haskell expressions at runtime in the running xmonad instance.
--
-----------------------------------------------------------------------------

module XMonad.Actions.Eval (
                            -- * Usage
                            -- $usage

                            -- * Documentation
                            -- $documentation

                             evalExpression
                           , evalExpressionWithReturn
                           , EvalConfig(..)
                           , defaultEvalConfig
                           ) where

import XMonad.Core
import XMonad.Util.Run
import Language.Haskell.Interpreter
import Control.Monad
import Data.List

-- $usage
-- This module provides functions to evaluate haskell expressions at runtime
-- To use it, bind a key to evalExpression, for example in combination with a prompt:
--
-- > import XMonad
-- > import XMonad.Actions.Eval
-- > import XMonad.Prompt.Input
-- > ..
-- >   , ((modMask,xK_t), inputPrompt defaultXPConfig "Eval" >>= flip whenJust (evalExpression defaultEvalConfig))
--
-- For detailed instructions on editing your key bindings, see
-- "XMonad.Doc.Extending#Editing_key_bindings".

-- $documentation

-- In here due to the apparent lack of a replace function in the standard library.
-- (Used for correctly displaying newlines in error messages)
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace lst@(x:xs) sub repl | sub `isPrefixOf` lst = repl ++ replace (drop (length sub) lst) sub repl
                            | otherwise = x:(replace xs sub repl)
replace _ _ _ = []

-- | Configuration structure
data EvalConfig = EvalConfig { handleError :: InterpreterError -> X String
                             -- ^ Function to handle errors
                             , imports     :: [(ModuleName,Maybe String)]
                             -- ^ Modules to import for interpreting the expression.
                             -- The pair consists of the module name and an optional
                             -- qualification of the imported module.
                             , modules     :: [String]
                             -- ^ Other source files that should be loaded
                             -- The definitions of these modules will be visible
                             -- regardless of whether they are exported.
                             }

-- | Defaults for evaluating expressions.
defaultEvalConfig :: EvalConfig
defaultEvalConfig = EvalConfig { handleError = handleErrorDefault
                               , imports = [("Prelude",Nothing),("XMonad",Nothing),
                                            ("XMonad.StackSet",Just "W"),("XMonad.Core",Nothing)]
                               , modules = []
                               }

-- | Default way to handle(in this case: display) an error during interpretation of an expression.
handleErrorDefault :: InterpreterError -> X String
handleErrorDefault err = io (safeSpawn "/usr/bin/xmessage" [replace (show err) "\\n" "\n"]) >>
                         return "Error"

-- | Returns an Interpreter action that loads the desired modules and interprets the expression.
interpret' :: EvalConfig -> String -> Interpreter (X String)
interpret' conf s = do
  loadModules $ modules conf
  setTopLevelModules =<< getLoadedModules
  setImportsQ $ imports conf
  interpret ("show `fmap` ("++s++")") (return "")

-- | Evaluates a given expression whose result type has to be an instance of Show
evalExpressionWithReturn :: EvalConfig -> String -> X String
evalExpressionWithReturn conf s = io (runInterpreter $ interpret' conf s) >>=
                                  either (handleError conf) id

-- | Evaluates a given expression, but discard the returned value. Provided for
-- more convenient use in keybindings
evalExpression :: EvalConfig -> String -> X ()
evalExpression cnf = (>> return ()) . evalExpressionWithReturn cnf