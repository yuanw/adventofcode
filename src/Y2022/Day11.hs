
module Y2022.Day11 where

type Index = Int
type Val = Int
data Monkey = Monkey Index [Val] (Val -> Val) Val Index Index
