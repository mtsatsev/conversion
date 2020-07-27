
module Huffman
where
import Satellite
import Tree
import Data.List
--Mario Tsatsev s1028415
-- -------------------------------------------------------------------------------
-- Warm-up: constructing a frequency table.

frequencies  ::  (Ord char) => [char] -> [With Int char]
frequencies [] = []
frequencies c = let d = sort(c) in [ head (map length (group (d))) :- collapse(group (d)) ] ++ frequencies (concat (tail (group d)))
 where
  collapse a = head(head a)
-- -------------------------------------------------------------------------------

-- Constructing a Huffman tree.

huffman :: [With Int char] -> Tree char
huffman [a]   = Leaf (satellite a)
huffman [a,b] = Leaf (satellite a) :^: Leaf (satellite b)
huffman a = ((\x ->  head x :^: last x) (take 2(map (\x -> Leaf (satellite x)) (sort a)))) :^: huffman (tail (tail a))
--well technically it gives a bit different result than the one showed in the assignment :/
-- -------------------------------------------------------------------------------

-- Encoding ASCII text.

data Bit = O | I
  deriving (Show, Eq, Ord)

encode :: (Eq char) => Tree char -> [char] -> [Bit]
encode _ [] = []
encode t (c:cs) = (checkCode c (codes t)) ++ encode t cs
  where
    checkCode x ((first,second):cs) = if x == first then second else checkCode x cs
--encode t c =  concat(map snd (codes t))
--this was the attempt that "worked" with my huffman tree.

codes :: Tree char -> [(char, [Bit])]
codes (Leaf x) = [(x,[])]
codes (x :^: y) = (map (\(a, b) -> (a, O:b)) (codes x)) ++ (map (\(c, b) -> (c, I:b)) (codes y))
-- -------------------------------------------------------------------------------

--Decoding a Huffman binary.

decode :: Tree char -> [Bit] -> [char]
decode t [] = []
decode t x = l:(decode t xs) where
  (xs, l) = letter t x
  letter (Leaf a) x = (x, a)
  letter (_ :^: b) (I:x) = letter b x
  letter (a :^: _) (O:x) = letter a x


-- -------------------------------------------------------------------------------

-- Some test data.

hw, why :: String
hw =
  "hello world"

-- code = huffman (frequencies hw)
-- encode code hw
-- decode code it
-- decode code it == hw

why =
  "As software becomes more and more complex, it\n\
  \is  more  and  more important to structure it\n\
  \well.  Well-structured  software  is  easy to\n\
  \write,   easy   to   debug,  and  provides  a\n\
  \collection  of modules that can be re-used to\n\
  \reduce future programming costs. Conventional\n\
  \languages place a conceptual limit on the way\n\
  \problems   can   be  modularised.  Functional\n\
  \languages  push  those  limits  back. In this\n\
  \paper we show that two features of functional\n\
  \languages    in    particular,   higher-order\n\
  \functions and lazy evaluation, can contribute\n\
  \greatly  to  modularity.  Since modularity is\n\
  \the key to successful programming, functional\n\
  \languages  are  vitally important to the real\n\
  \world."

-- code = huffman (frequencies why)
-- encode code why
-- decode code it
-- decode code it == why
