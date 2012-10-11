{-# LANGUAGE QuasiQuotes #-}
module Main where
import QuadrapleTH
import TuringMachine

zeroOneDouble :: SimpleMachine
zeroOneDouble = [tm|
 -- 0: initial state; seeking '0'
 -- 1: seeking first '1'
 -- 2: seeking second '2'
 -- 3: return to the last 'X'
 -- 4: ensuring every '1' replaced by 'Y'
 -- 5: accepted.
 -- a-: rejected.
 00X1  -- read '0' and started to seek '1'
 1XR1  -- skip 'X' while seeking '1'
 10R1  -- skip '0'
 1YR1  -- skip '1's already read, namely'Y'
 11Y2  -- first  '1' found; rewrite as 'Y' then seeking another '1'
 2YR2  -- go to next
 21Y3  -- second '1' found; rewrite as 'Y' then return to the 'X'
 2BBa  -- too few '1'! error!
 3YL3  -- skip 'Y's until 'X' found
 30L3  -- skip '0's until 'X' found
 3XR0  -- if 'X' was found, then searching '0'
 0YR4  -- if all the '1' have been found, let's ensure no '1' left there
 4YR4  -- skip 'Y'
 4BB5  -- blank. every '1' was replaced. Let's greet
 5BR6
 6BO6
 6OR7
 7BK7
 7KR8
 8B!8
 8!R9
 411a  -- failed. there are '1' still left!
 a1Ra
 aYRa
 aXRa
 a0Ra
 aBRb
 bBEb
 bERc
 cBrc
 crRd
 dBrd
 drRe
 eB!e
 e!Rf
 |]

ops :: Tape Char
ops = [tape|1111 11 111 111|]

markOPTH :: SimpleMachine
markOPTH = [tm|
  01L4
  4BH1
  1HR1
  11R1
  1BR2
  21R1
  2BL3
  3BT3
 |]

zeroOneEqual :: SimpleMachine
zeroOneEqual = [tm|
  00X1
  0YR5
  1XR2
  20R2
  2YR2
  21Y3
  2BBa
  3YL4
  4YL4
  40L4
  4XR0
  511a
  5YR5
  5BR6
  6BO6
  6OR7
  7BK7
  7KR8
  8B!8
  8!R9
  a1Ra
  aBRb
  bBEb
  bERc
  cBrc
  crRd
  dBrd
  drRe
  eB!e
  e!Rf
 |]

headTailMarker :: TuringMachine Int Char
headTailMarker = makeTM [(0, Just '1', GoLeft, 0)
                        ,(0, Nothing , Write 'H', 1)
                        ,(1, Just 'H', GoRight, 1)
                        ,(1, Just '1', GoRight, 1)
                        ,(1, Nothing, Write 'T', 2)
                        ]

main :: IO ()
main = do
  putStrLn "mark H at Head and T at Tail on \"11111\", started from the third"
  mapM_ print $ execTM headTailMarker 0 (right $ right [tape|11111|])
  putStrLn "==========\n"
  putStrLn "parses 0^{n}1^{n}"
  print $ runTM zeroOneEqual 0 [tape|0011|]
  putStrLn "==========\n"
  putStrLn "parses 0^{n}1^{2n}; but fail."
  mapM_ print $ execTM zeroOneDouble 0 [tape|00111|]
  putStrLn "==========\n"
  putStrLn "parses 0^{n}1^{2n}; but fail again."
  mapM_ print $ execTM zeroOneDouble 0 [tape|0111|]
  putStrLn "==========\n"
  putStrLn "parses 0^{n}1^{2n}; success!."
  mapM_ print $ execTM zeroOneDouble 0 [tape|001111|]

