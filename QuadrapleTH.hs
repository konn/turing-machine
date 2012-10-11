{-# LANGUAGE PatternGuards, TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
module QuadrapleTH where
import Control.Arrow
import Control.Lens              hiding (Tape)
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import Data.Ord
import Language.Haskell.TH.Quote
import TuringMachine

tm :: QuasiQuoter
tm = QuasiQuoter { quoteExp = qExp }
  where
    qExp src =
      let dic = dataToExpQ (const Nothing) (parseQuadraple src)
      in [| makeTM $(dic) :: SimpleMachine |]

parseQuadraple :: String -> [(Int, Maybe Char, Instruction Char, Int)]
parseQuadraple src = check $ mapMaybe (parseLine . dropWhile isSpace) $ lines src
  where
    fs = view _1 &&& view _2
    check xs =
        let inss = groupBy ((==) `on` fs) $ sortBy (comparing fs) xs
        in if all (null . drop 1) inss
           then xs
           else error $ "overlapping: \n" ++ unlines (map (unlines . map showQuad) $ filter (not . null . drop 1) inss)
    showQuad (q, c, i, q') = [intToDigit q, fromMaybe 'B' c, i', intToDigit q']
      where
        i' | Clear   <- i = 'B'
           | GoLeft  <- i = 'L'
           | GoRight <- i = 'R'
           | Write c <- i = c
    parseLine ('-':'-':_)  = Nothing
    parseLine (q:c:i:q':_) = Just (digitToInt q, input, inst, digitToInt q')
      where
        input | c == 'B'  = Nothing
              | otherwise = Just c
        inst  | 'B' <- i  = Clear
              | 'L' <- i  = GoLeft
              | 'R' <- i  = GoRight
              | otherwise = Write i
    parseLine _ = Nothing


tape :: QuasiQuoter
tape = QuasiQuoter { quoteExp = \src -> dataToExpQ (const Nothing) (parseTape src) }

parseTape :: String -> Tape Char
parseTape str = (map replace $ filter (`notElem` "\r\n") $ dropWhile isSpace str, [])
  where
    replace '_' = Nothing
    replace ' ' = Nothing
    replace 'B' = Nothing
    replace c   = Just c
