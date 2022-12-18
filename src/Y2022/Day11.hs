
module Y2022.Day11 where

type Index = Int
type Val = Int
data Monkey = Monkey { _index :: Index, _items :: [Val], _op :: Val -> Val,
                      _divisible :: Val, _trueThrow :: Index , _falseThrow :: Index}

eval :: [Monkey] -> Monkey -> [Monkey]
eval monkeys monkey = if null (_items monkey) then monkeys else undefined

add :: [Monkey] -> Index -> Val -> [Monkey]
add = undefined

pop :: [Monkey] -> Monkey -> [Monkey]
pop = undefined
