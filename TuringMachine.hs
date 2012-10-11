{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE ViewPatterns                                                #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module TuringMachine where
import           Control.Applicative
import           Control.Arrow        hiding (left, right)
import           Control.Lens         hiding (Tape, Zipper, left, right)
import           Control.Monad.Writer
import           Data.Char
import           Data.Data
import           Data.List.Lens
import qualified Data.Map             as M
import           Data.Maybe

data Instruction s = GoLeft
                   | GoRight
                   | Clear
                   | Write s
                     deriving (Show, Eq, Ord, Data, Typeable)

newtype TuringMachine q s = TM { unTM :: M.Map (q, Maybe s) (Instruction s, q) }
    deriving (Show, Eq, Ord, Data, Typeable)
data ID q s = ID { curTape :: Tape s, curState :: q }
              deriving (Show, Eq, Ord, Data, Typeable)

instance Show (ID Int Char) where
  show (ID tape st) = showID (tape, intToDigit st)

type SimpleMachine = TuringMachine Int Char

type Zipper a = ([a], [a])
type Tape   a = Zipper (Maybe a)

showID :: (Tape Char, Char) -> String
showID ((xs, reverse . dropWhile isNothing . reverse -> ys), q) = concat (map toStr $ dropWhile isNothing $ reverse $ ys)  ++ ('[' : q : "]") ++ concat (map toStr xs)
  where
    toStr = maybe "_" pure

right :: Tape a -> Tape a
right ([x], rs)  = ([Nothing], x:rs)
right (x:ls, rs) = (ls, x:rs)
right ([], rs)   = ([Nothing], rs)

left :: Tape a -> Tape a
left (xs, [y]) = (y:xs, [Nothing])
left (xs, y:ys) = (y:xs, ys)
left (xs, []) = (Nothing:xs, [])

getState :: Tape a -> Maybe a
getState (x:_, _) = x
getState ([],  _) = Nothing

toTape :: [a] -> Tape a
toTape [] = ([Nothing], [])
toTape as = (map Just as, [])

fromTape :: Tape a -> [Maybe a]
fromTape (xs, ys) = dropWhile isNothing (reverse ys) ++ reverse (dropWhile isNothing $ reverse xs)

runTM :: (Ord t, Ord a) => TuringMachine t a -> t -> Tape a -> ID t a
runTM tm q tp = fst $ runWriter $ calculate tm q tp

execTM :: (Ord t, Ord a) => TuringMachine t a -> t -> Tape a -> [ID t a]
execTM tm q tp = execWriter $ calculate tm q tp

calculate :: (Ord a, Ord t) => TuringMachine t a -> t -> Tape a -> Writer [ID t a] (ID t a)
calculate tm q tp = do
  tell [ID tp q]
  case eval q tp tm of
    Nothing        -> return $ ID tp q
    Just (tp', q') -> calculate tm q' tp'

eval :: (Ord q, Ord a) => q -> Tape a -> TuringMachine q a -> Maybe (Tape a, q)
eval q tape tm = sub <$> step q tape tm
  where
    sub (Clear, q')     = (_1 %~ _head .~ Nothing $ tape, q')
    sub (GoLeft,  q')   = (left tape, q')
    sub (GoRight, q')   = (right tape, q')
    sub (Write c, q') = (_1 %~ _head .~ Just c $ tape, q')

step :: (Ord q, Ord a) => q -> Tape a -> TuringMachine q a -> Maybe (Instruction a, q)
step q tape (TM dic) = M.lookup (q, getState tape) dic

makeTM :: (Ord q, Ord a) => [(q, Maybe a, Instruction a, q)] -> TuringMachine q a
makeTM d = TM $ M.fromList $ map ((view _1 &&& view _2) &&& (view _3 &&& view _4)) d

makeTMSimple :: (Ord q, Ord a) => [(q, a, Instruction a, q)] -> TuringMachine q a
makeTMSimple d = TM $ M.fromList $ map ((view _1 &&& views _2 Just) &&& (view _3 &&& view _4)) d
