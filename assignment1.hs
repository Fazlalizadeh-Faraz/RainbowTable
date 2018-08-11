import RainbowAssign
import System.Random
import Data.Int
import Data.List
import qualified Data.Map as Map

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table

toInt :: Int32 -> Int
toInt x = fromIntegral(x)

toString :: [Int] -> [Char]
toString xs = map toLetter xs

convertBase :: Int -> [Int]
convertBase x = (take pwLength $ (x `mod` nLetters) : convertBase (x `div` nLetters))

pwReduce:: Int32 -> String
pwReduce x = toString (reverse (take pwLength (convertBase (toInt(x)))))

-- Functions to calculate the rainbowTable
rainbowTableRecursive :: Int -> Passwd -> Hash
rainbowTableRecursive 0 y = pwHash (y)
rainbowTableRecursive x y = rainbowTableRecursive (x-1) (pwReduce $! (pwHash (y)))

rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable x [] = Map.fromList [(0, "")]
rainbowTable 0 (y:ys) = Map.fromList (map rainbowMappingBaseCase (y:ys))
    where rainbowMappingBaseCase y = (pwHash y, y)
rainbowTable x (y:ys) = Map.fromList (map rainbowMapping (y:ys))
    where rainbowMapping y = (rainbowTableRecursive x y, y)

-- Functions for testing
generateTable :: IO ()
generateTable = do
    table <- buildTable rainbowTable nLetters pwLength width height
    writeTable table filename

test1 = do
    table <- readTable filename
    return (Map.lookup 0 table)

-- Function to understand last required function from assignment as the web page instructions are very poorly written. 
-- Hard to understand what you guys actually want as it is very obscure.
loopHashReduce :: Passwd -> Int -> Passwd
loopHashReduce x 0 = x
loopHashReduce x y
    | (pwHash x) == (19040862) = x
    | otherwise = loopHashReduce (pwReduce (pwHash x)) (y-1)

--Functions to calculate the final function
changePasswdInList :: Hash -> Passwd -> (Hash, Passwd)
changePasswdInList hash newPasswd = (hash, newPasswd)

searchWidthCurrentPasswd :: (Hash, Passwd) -> Int -> Hash -> (Hash, Passwd)
searchWidthCurrentPasswd x y z
    | (pwHash (snd (x))) == z = x
    | y == 0 = x
    | otherwise = searchWidthCurrentPasswd
        (changePasswdInList (fst x) (pwReduce (pwHash (snd (x)))))
        (y-1)
        z

findPasswordRecursive :: [(Hash, Passwd)] -> Int -> Hash -> Maybe Passwd
findPasswordRecursive (x:xs) 0 z 
    | (pwHash (snd x)) == z = Just (snd (x))
    | length xs == 0 = Nothing
    | otherwise = findPasswordRecursive xs 0 z

findPasswordRecursive (x:xs) y z
    | (pwHash (snd(searchWidthCurrentPasswd x y z))) == z = Just (snd (searchWidthCurrentPasswd x y z))
    | length xs == 0 = Nothing
    | otherwise = findPasswordRecursive xs y z

findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword xs 0 z = findPasswordRecursive (Map.toList (xs)) 0 z
findPassword xs y 0 = Nothing
findPassword xs y z = findPasswordRecursive (Map.toList (xs)) y z 