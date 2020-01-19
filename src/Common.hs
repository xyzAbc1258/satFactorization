module Common where

-- length in bits and bits values
sizeInBits::Integer -> (Integer, [Integer])
sizeInBits n = sizeInBitsH n 0 []
  where sizeInBitsH 0 s bits = (s, reverse bits)
        sizeInBitsH cn s bits = sizeInBitsH (cn `div` 2) (s + 1) (cn `mod` 2 : bits)


isPrime::Integer -> Bool
isPrime k = (k > 1) && null [x | x <- [2 .. k - 1], k `mod` x == 0]


joinNewLine::[String] -> String
joinNewLine = foldl1 (\a b -> a ++ "\n" ++ b)
