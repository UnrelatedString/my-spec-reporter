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
import Test.Spec.Tree (Tree, TestLocator, bimapTreeWithPaths)
import Ansi.Codes (GraphicsParam(..))
import Ansi.Codes as ANSI
import Data.Tuple (snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, unsnoc)
import Data.String as String
import Control.Monad.State (StateT, class MonadState, modify, get)
import Control.Monad.Writer (Writer, WriterT, class MonadWriter, class MonadTell, tell, execWriterT)
import Data.Unfoldable (replicate)
import Data.Unfoldable.Trivial (refold)
import Data.Foldable (foldMap)
import Data.List.NonEmpty (NonEmptyList(..))
import Data.List ((:), List(Nil))
import Data.NonEmpty ((:|))
import Data.Monoid.Additive (Additive(..))
import Data.Traversable (traverse_, sequence_)
import Data.Number.Format (toStringWith, fixed)
import Data.Time.Duration (Milliseconds(..))
import Data.Newtype (class Newtype, unwrap)

prettyReporter :: Reporter
prettyReporter = defaultReporter initialState $ defaultUpdate
 { getRunningItems: getRunningItems
 , printFinishedItem: (unwrap <<< _) <<< printFinishedItem
 , putRunningItems
 , update: unwrap <<< update
 }

type PrettyState = { runningItems :: Map TestLocator RunningItem, undoLastSequential :: PrettyAction }

newtype PrettyM a = PrettyM (StateT PrettyState (Writer String) a)
derive newtype instance Functor PrettyM
derive newtype instance Apply PrettyM
derive newtype instance Applicative PrettyM
derive newtype instance Bind PrettyM
derive newtype instance Monad PrettyM
derive newtype instance MonadState PrettyState PrettyM
derive newtype instance MonadTell String PrettyM
derive newtype instance MonadWriter String PrettyM
derive instance Newtype (PrettyM a) _

type PrettyAction = PrettyM Unit
type UndoablePrint = forall m. MonadState PrettyState m => MonadWriter (Additive Int /\ String) m => m Unit

initialState :: PrettyState
initialState = { runningItems: Map.empty, undoLastSequential: pure unit }

getRunningItems :: PrettyState -> Map TestLocator RunningItem
getRunningItems state = state.runningItems

putRunningItems :: Map TestLocator RunningItem -> PrettyState -> PrettyState
putRunningItems items state = state{ runningItems = items }

printFinishedItem :: TestLocator -> RunningItem -> PrettyAction
printFinishedItem locator = case _ of
  RunningTest (Just result) -> do
    commit $ indent locator
    commit $ formatTest locator $ Just result
    tellLn ""
  RunningTest Nothing -> pure unit
  RunningPending -> formatPending locator
  RunningSuite true -> do
    formatSuite locator
  RunningSuite false -> pure unit

letDefaultUpdateHandleThis :: forall m. Applicative m => m Unit
letDefaultUpdateHandleThis = pure unit

update :: Event -> PrettyAction
update (Event.Start nTests) = tellLn $ "Running " <> show nTests <> " tests..."
update (Event.Suite Sequential locator) = formatSuite locator
update (Event.Suite Parallel _locator) = letDefaultUpdateHandleThis
update (Event.SuiteEnd _locator) = pure unit
update (Event.Test Sequential locator) = do
  commit $ indent locator
  untell <- backspace $ formatTest locator Nothing
  void $ modify _{undoLastSequential = untell}
update (Event.Test Parallel _locator) = letDefaultUpdateHandleThis
update (Event.TestEnd locator result) = do
  state <- get
  case Map.lookup locator state.runningItems of
    Just _ -> letDefaultUpdateHandleThis
    Nothing -> do
      state.undoLastSequential
      commit $ formatTest locator $ Just result
      tellLn ""
update (Event.Pending locator) = formatPending locator
update (Event.End resultTrees) = do
  commit $ "\n================================\n\n" `styled` (PForeground ANSI.BrightWhite : PMode ANSI.Dim : Nil)
  callOutSlowTests resultTrees
  tellLn ""
  defaultSummary resultTrees

indent :: TestLocator -> UndoablePrint
indent (path /\ _) = do
  (refold $ replicate (Array.length path) "| ") `styled` (PForeground ANSI.BrightMagenta : Nil)

styled :: String -> List GraphicsParam -> UndoablePrint
-- fortunately the associativity of (<>) happens to be defined to make this legal LMAO
styled s (h : t) = tell $ (Additive (String.length s) /\ _) $
                   (_ <> s <> clearFormatting) $
                   ANSI.escapeCodeToString $ ANSI.Graphics $ NonEmptyList $ h :| t
styled s Nil = tell $ Additive (String.length s) /\ s

clearFormatting :: String
clearFormatting = ANSI.escapeCodeToString $ ANSI.Graphics $ NonEmptyList $ ANSI.Reset :| Nil

testNameStyle :: List GraphicsParam
testNameStyle = PForeground ANSI.BrightWhite : Nil

suiteNameStyle :: List GraphicsParam
suiteNameStyle = PForeground ANSI.BrightCyan : PMode ANSI.Italic : Nil

backspace :: forall m n.
  MonadState PrettyState m =>
  MonadWriter String m =>
   MonadState PrettyState n =>
  MonadWriter String n =>
  WriterT (Additive Int /\ String) m Unit -> m (n Unit)
backspace print = do
  Additive n /\ s <- execWriterT print
  tell s
  pure $ tell $ refold $ replicate n "\x08"

commit :: forall m.
  MonadState PrettyState m =>
  MonadWriter String m =>
  WriterT (Additive Int /\ String) m Unit -> m Unit
commit print = do
  _ /\ s <- execWriterT print
  tell s

formatTest :: TestLocator -> Maybe Result -> UndoablePrint
formatTest (_ /\ name) r = do
  formatTestResultIndicator r
  name `styled` testNameStyle
  "..." `styled` Nil
  formatTestResultSuffix r

formatTestResultIndicator :: Maybe Result -> UndoablePrint
formatTestResultIndicator r = do
  case r of
    Just (Success _ _) -> "✓" `styled` (PForeground ANSI.BrightGreen : Nil)
    Just (Failure _)   -> "✗" `styled` (PForeground ANSI.Black : PBackground ANSI.BrightRed : Nil)
    Nothing            -> "-" `styled` (PForeground ANSI.White : PMode ANSI.Dim : Nil)
  " " `styled` Nil

formatTestResultSuffix :: Maybe Result -> UndoablePrint
formatTestResultSuffix = case _ of
  Just (Success _ _) -> " passed" `styled` (PForeground ANSI.Green : Nil)
  Just (Failure _)   ->  do
    " " `styled` Nil
    "failed!" `styled` (PForeground ANSI.BrightRed : PMode ANSI.Underline : Nil)
  Nothing            -> pure unit

formatSuite :: TestLocator -> PrettyAction
formatSuite locator = do
  commit $ indent locator
  commit $ "Suite " `styled` Nil
  commit $ snd locator `styled` suiteNameStyle
  commit $ ":\n" `styled` Nil

formatPending :: TestLocator -> PrettyAction
formatPending locator = do
  commit $ indent locator
  commit $ snd locator `styled` testNameStyle
  commit $ " is unimplemented\n" `styled` (PForeground ANSI.BrightYellow : Nil)

-- why do the type variables get flipped around in the docs at random...
callOutSlowTests :: Array (Tree String Void Result) -> PrettyAction
callOutSlowTests = traverse_ $ sequence_ <<< bimapTreeWithPaths (flip const) vibeCheck
  where vibeCheck :: NonEmptyArray String -> Result -> PrettyAction
        vibeCheck shittyLocator result = case result of
          Success Speed.Fast _ -> pure unit
          Success Speed.Medium (Milliseconds ms) -> callOut shittyLocator ms do
            commit $ "kinda slow!" `styled` (PForeground ANSI.BrightYellow : Nil)
          Success Speed.Slow (Milliseconds ms) -> callOut shittyLocator ms do
            commit $ "really slow!" `styled` (PForeground ANSI.BrightRed : Nil)
          Failure _ -> pure unit
        callOut :: NonEmptyArray String -> Number -> PrettyAction -> PrettyAction
        callOut shittyLocator ms specific = do
          commit $ "! " `styled` (PForeground ANSI.Blue : Nil)
          commit $ "Test " `styled` Nil
          let { init: path, last: name } = unsnoc shittyLocator
          commit $ foldMap (_ <> ".") path `styled` suiteNameStyle
          commit $ name `styled` (PMode ANSI.Underline : testNameStyle)
          commit $ " is " `styled` Nil
          specific
          commit $ " (" `styled` Nil
          commit $ toStringWith (fixed 0) ms `styled` (PForeground ANSI.BrightWhite : Nil)
          commit $ "ms)\n" `styled` Nil
