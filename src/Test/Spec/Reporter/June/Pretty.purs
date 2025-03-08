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
import Test.Spec.Style (styled)
import Test.Spec.Style as Style
import Test.Spec.Tree (Path, Tree, TestLocator, parentSuiteName)
import Data.Tuple.Nested ((/\))
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Control.Monad.State (class MonadState, StateT, get, modify)
import Control.Monad.Writer (class MonadWriter, Writer)

prettyReporter :: Reporter
prettyReporter = defaultReporter initialState $ defaultUpdate
 { getRunningItems
 , printFinishedItem
 , putRunningItems
 , update
 }

type PrettyState = { runningItems :: Map TestLocator RunningItem }

initialState :: PrettyState
initialState = { runningItems: Map.empty }

getRunningItems :: PrettyState -> Map TestLocator RunningItem
getRunningItems state = state.runningItems

putRunningItems :: Map TestLocator RunningItem -> PrettyState -> PrettyState
putRunningItems items state = state{ runningItems = items }

printFinishedItem :: TestLocator -> RunningItem -> StateT PrettyState (Writer String) Unit
printFinishedItem (path /\ name) = case _ of
  RunningTest (Just result) -> tellLn $ show path <> "." <> name <> " finished with result: " <> show result
  RunningTest Nothing -> tellLn $ show path <> "." <> name <> " didn't finish. I have no idea what this means ;_;"
  RunningPending -> tellLn $ show path <> "." <> name <> "is pending :/"
  RunningSuite finished -> tellLn $ show path <> "." <> name <> " finished!... " <> show finished

update :: Event -> StateT PrettyState (Writer String) Unit
update (Event.Start nTests) = pure unit
update (Event.Suite exec (path /\ name)) = pure unit
update (Event.SuiteEnd (path /\ name)) = pure unit
update (Event.Test exec (path /\ name)) = pure unit
update (Event.TestEnd (path /\ name) result) = pure unit
update (Event.Pending (path /\ name)) = pure unit
update (Event.End resultTrees) = defaultSummary resultTrees
