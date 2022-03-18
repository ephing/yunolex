{-# LANGUAGE GADTs, FlexibleContexts #-}
module Lexer where
import Data.Map
import Control.Monad

data Action where
    Name :: String -> Action
    Skip :: Action
    Err :: String -> Action
    deriving (Show, Eq)

type Automata = (Int, [Int], Action, Map Int (Map Char Int))
type Token = (Action, String, (Int, Int))

lexeme :: Token -> String
lexeme (_, t, _) = t

tRow :: Token -> Int
tRow (_, _, (r, _)) = r

tCol :: Token -> Int
tCol (_, _, (_, c)) = c

startState :: Automata -> Int
startState (s, _, _, _) = s

finStates :: Automata -> [Int]
finStates (_, f, _, _) = f

action :: Automata -> Action
action (_, _, a, _) = a

deltaT :: Automata -> Map Int (Map Char Int)
deltaT (_, _, _, d) = d

death :: [Maybe Int] -> Bool
death = Prelude.foldr ((&&) . (==) Nothing) True

getInd :: [Automata] -> [Maybe Int] -> Int -> Maybe Int
getInd (a:as) (m:ms) n = if m == Nothing || notElem (unmaybe m) (finStates a) then getInd as ms (n + 1) else Just n
getInd _ _ _ = Nothing

unmaybe :: Maybe a -> a
unmaybe Nothing = error "unmaybe: cant unmaybe a nothing"
unmaybe (Just n) = n

moveAhead :: Char -> [Automata] -> [Maybe Int] -> [Maybe Int]
moveAhead c (a:as) (m:ms) = case m of
                                Nothing -> Nothing : moveAhead c as ms
                                Just m' ->
                                    case Data.Map.lookup m' (deltaT a) of
                                        Nothing -> Nothing : moveAhead c as ms
                                        Just t -> Data.Map.lookup c t : moveAhead c as ms
moveAhead _ _ _ = []

tokDiff :: String -> String -> String
tokDiff s t = Prelude.drop (length t) s

lexer :: [Automata] -> String -> Maybe [Token]
lexer a s = actualLexer a s "" (replicate (length a) (Just 0)) 1 1 []

--              spec        input       token     state mach    row     col   tk track   output
actualLexer :: [Automata] -> String -> String -> [Maybe Int] -> Int -> Int -> [Token] -> Maybe [Token]
actualLexer [] _ _ _ _ _ _  = Nothing
actualLexer a s t m row col tks = do {
  if s == "" || death (moveAhead (head s) a m) then 
    case tks of
      [] -> if t == "" then Just [] else Nothing
      tok : _ -> 
        case actualLexer a c "" r (tRow tok) (tCol tok + 1) [] of
          Nothing -> Nothing
          Just output -> Just (tok : output)
          where
              r = replicate (length a) (Just 0)
              c = tokDiff t (lexeme tok) ++ s
  else case head s of
      '\n' -> actualLexer a (tail s) (t ++ [head s]) mv (row + 1) 1 x
          where
              mv = moveAhead (head s) a m
              x = case getInd a mv 0 of
                  Nothing -> tks
                  Just n -> (action (a !! n), t ++ "\n", (row, col)) : tks
      hs -> actualLexer a (tail s) (t ++ [hs]) mv row (col + 1) x
          where
              mv = moveAhead (head s) a m
              x = case getInd a mv 0 of
                  Nothing -> tks
                  Just n -> (action (a !! n), t ++ [hs], (row, col)) : tks
}
