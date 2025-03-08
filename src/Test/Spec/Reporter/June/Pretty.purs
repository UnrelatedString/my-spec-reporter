module Test.Spec.Reporter.June.Pretty
 ( prettyReporter
 ) where

-- heavily based on https://github.com/purescript-spec/purescript-spec/blob/v8.1.1/src/Test/Spec/Reporter/Spec.purs

import Prelude

import Test.Spec.Console (tellLn)
import Test.Spec.Reporter.Base (RunningItem(..), defaultReporter, defaultSummary, defaultUpdate)
import Test.Spec.Result (Result(..))
import Test.Spec.Runner (Reporter)
import Test.Spec.Runner.Event (Event, Execution(..))
import Test.Spec.Runner.Event as Event
import Test.Spec.Speed as Speed
import Test.Spec.Tree (Path, Tree, TestLocator, parentSuiteName)
import Ansi.Codes (GraphicsParam(..))
import Ansi.Codes as ANSI
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.String as String
import Control.Monad.State (class MonadState, StateT, get, modify)
import Control.Monad.Writer (class MonadWriter, class MonadTell, Writer, tell, listen, censor, execWriterT)
import Control.Monad.Trans.Class (lift)
import Data.Unfoldable (replicate)
import Data.Unfoldable.Trivial ((::<*>)) -- ...I sure hope this cyclical dependency isn't a problem if it's only for testing
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List ((:), List(Nil))
import Data.NonEmpty (NonEmpty, (:|))

prettyReporter :: Reporter
prettyReporter = defaultReporter initialState $ censor show <<< defaultUpdate
 { getRunningItems
 , printFinishedItem
 , putRunningItems
 , update
 }

type PrettyState = { runningItems :: Map TestLocator RunningItem, undoLastSequential :: Writer String Unit }

type PrettyAction = StateT PrettyState (Writer String) Unit

initialState :: PrettyState
initialState = { runningItems: Map.empty, undoLastSequential: pure unit }

getRunningItems :: PrettyState -> Map TestLocator RunningItem
getRunningItems state = state.runningItems

putRunningItems :: Map TestLocator RunningItem -> PrettyState -> PrettyState
putRunningItems items state = state{ runningItems = items }

printFinishedItem :: TestLocator -> RunningItem -> PrettyAction
printFinishedItem locator = case _ of
  RunningTest (Just result) -> pure unit
  RunningTest Nothing -> pure unit
  RunningPending -> pure unit
  RunningSuite finished -> pure unit

letDefaultUpdateHandleThis :: forall m. Applicative m => m Unit
letDefaultUpdateHandleThis = pure unit

update :: Event -> PrettyAction
update (Event.Start nTests) = tellLn $ "Running " <> show nTests <> " tests..."
update (Event.Suite Sequential locator) = pure unit
update (Event.Suite Parallel locator) = letDefaultUpdateHandleThis
update (Event.SuiteEnd locator) = pure unit
update (Event.Test Sequential locator) = do
  indent locator
  untell <- backspace $ formatTest locator Nothing
  void $ modify _{undoLastSequential = untell}
update (Event.Test Parallel locator) = letDefaultUpdateHandleThis
update (Event.TestEnd locator result) = do
  state <- get
  -- lift $ state.undoLastSequential
  formatTest locator $ Just result
  tellLn ""
update (Event.Pending locator) = pure unit
update (Event.End resultTrees) = defaultSummary resultTrees

indent :: TestLocator -> PrettyAction
indent (path /\ _) = tell $ fold ::<*> replicate (Array.length path) "| "

styled :: String -> NonEmpty List GraphicsParam -> String
-- fortunately the associativity of (<>) happens to be defined to make this legal LMAO
styled s = (_ <> s <> clearFormatting) <<< ANSI.escapeCodeToString <<< ANSI.Graphics <<< NonEmptyList

clearFormatting :: String
clearFormatting = ANSI.graphicsParamToString Reset <> ANSI.graphicsParamToString (PBackground ANSI.Black)

backspace :: forall m. MonadWriter String m => m Unit -> m (Writer String Unit)
backspace action = do
  _ /\ s <- listen action
  pure $ tell $ fold ::<*> replicate (String.length s) "\x08"

formatTest :: TestLocator -> Maybe Result -> PrettyAction
formatTest (_ /\ name) r = do
  formatTestResultIndicator r
  tell $ name `styled` (PForeground ANSI.White :| Nil)
  formatTestResultSuffix r

formatTestResultIndicator :: Maybe Result -> PrettyAction
formatTestResultIndicator = const (tell " ") <=< tell <<< case _ of
  Just (Success _ _) -> "✓" `styled` (PForeground ANSI.BrightGreen :| Nil)
  Just (Failure _)   -> "✗" `styled` (PForeground ANSI.BrightRed :| PBackground ANSI.BrightWhite : Nil)
  Nothing            -> "-" `styled` (PForeground ANSI.White :| PMode ANSI.Dim : Nil)

formatTestResultSuffix :: Maybe Result -> PrettyAction
formatTestResultSuffix = const (tell " ") <=< tell <<< case _ of
  Just (Success _ _) -> "... passed" `styled` (PForeground ANSI.White :| Nil)
  Just (Failure _)   -> "... failed!" `styled` (PForeground ANSI.Red :| Nil)
  Nothing            -> "..." `styled` (PForeground ANSI.White :| Nil)
