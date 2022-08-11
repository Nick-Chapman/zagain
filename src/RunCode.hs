
module RunCode (runCode) where

import Action (Action)
import Code (Code)
import qualified Action (Action(..))

runCode :: Word -> Code -> Action
runCode _ _ = do Action.Debug "**TODO:RunCode.runCode" $ Action.Stop 999
