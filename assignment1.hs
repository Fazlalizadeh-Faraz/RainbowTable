-- objective: rainbow table
-- 1) need the hashfunction that the system uses
-- 2) reduce fucntion : map the has value to arbiturary password() does not reverse the hashfunction
-- Faraz Fazlalizadeh
-- Cmpt 383
-- A1



import RainbowAssign
import System.Random
import Data.Int
import Data.List
import qualified Data.Map as Map
import Data.Maybe as Maybe


-- type Hash = Int32
-- type Passwd = String
pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8          -- length of each password
nLetters = 5           -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table



-- used for conversion 
int32toInt:: Int32 -> Int
int32toInt x = fromIntegral(x)

-- for reducing the paswd
pwReduce:: Int32 -> String
pwReduce x = 
    (toString (reverse (take pwLength  (toBase (int32toInt(x))))))

-- helper in pwReduce
toString:: [Int] -> [Char]
toString xs = map toLetter xs


--  for dealing with negative
toBase :: Int -> [ Int ]
toBase x  = (take pwLength $ (x `mod` nLetters) : toBase (x `div` nLetters) )



--RainbowTable recursive function to calculate final hash
rainbowTableRecursive :: Int -> Passwd -> Hash
rainbowTableRecursive 0 y = pwHash (y)
rainbowTableRecursive x y = rainbowTableRecursive (x-1) (pwReduce $! (pwHash (y)))



--RainbowTable function to map Passwd list to a recursive hash calculator
rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable x [] = Map.fromList [(0, "")]
rainbowTable 0 (w:ws) = Map.fromList (map baseFor0 (w:ws))
    where baseFor0 w = (pwHash w, w)
rainbowTable x (w:ws) = Map.fromList (map rainbowMappingForNon0 (w:ws))
    where rainbowMappingForNon0 w = (rainbowTableRecursive x w, w)

generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename


test1 = do
  table <- readTable filename
  return (Map.lookup 0 table)



fromJust :: Maybe Int -> Int
fromJust x = Main.fromJust x

isJust :: Maybe Passwd -> Bool
isJust x = Main.isJust x




-- helper fucntions incase needed

  -- make tuple
makePassAndHashTuple :: Hash -> Passwd -> (Hash, Passwd)
makePassAndHashTuple hash givenPass = (hash, givenPass)


makeNewPaswd :: (Hash, Passwd) -> Passwd
makeNewPaswd givenTuple = pwReduce (pwHash (snd givenTuple))


findTheIndexOfGivenElement :: [(Hash, Passwd)] -> Hash -> Int
findTheIndexOfGivenElement ws x = Maybe.fromJust (x `elemIndex` (map fst (ws)))       

searchRecur :: (Hash, Passwd) -> Hash -> Int -> Maybe Passwd
searchRecur givenTuple hashVal counter
    | ((pwHash (snd givenTuple)) == hashVal) = Just (snd givenTuple)
    | (counter == 0) = Nothing
    | otherwise = searchRecur 
        (makePassAndHashTuple (fst givenTuple) (makeNewPaswd givenTuple))
        hashVal
        (counter-1)

falsePosCheck :: Map.Map Hash Passwd -> Int -> Int -> Int -> Hash -> Hash -> Hash -> Maybe Passwd
falsePosCheck ws countNotFalse countCopy counterOriginal hashEdit hashVal hashFalse
    | (Maybe.isJust checkFunction) = checkFunction
    | (Maybe.isNothing checkFunction) = findPaswdRecur ws (counterOriginal-1) countCopy (pwHash (pwReduce (hashEdit))) hashFalse
        where checkFunction = searchRecur ((Map.toList ws)!!(findTheIndexOfGivenElement (Map.toList ws) hashEdit)) hashVal countNotFalse

-- gets the table...amount of times its been hashed.. hashvalue
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword ws 0 hashVal = (Map.lookup hashVal ws)
findPassword ws y 0 = Nothing
findPassword ws y hashVal = findPaswdRecur ws y y hashVal hashVal


findPaswdRecur :: Map.Map Hash Passwd -> Int -> Int -> Hash -> Hash  -> Maybe Passwd
findPaswdRecur ws counterCounting counterCopy hashEdit hashVal
    | (Maybe.isJust (Map.lookup hashEdit ws)) = falsePosCheck ws counterCopy counterCopy counterCounting hashEdit hashVal hashVal 
    | (counterCounting <= 0) = Nothing 
    | otherwise = findPaswdRecur 
        ws 
        (counterCounting-1) 
        counterCopy
        (pwHash (pwReduce hashEdit)) 
        hashVal

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
    table <- readTable filename
    pws <- randomPasswords nLetters pwLength n
    let hs = map pwHash pws
    let result = Maybe.mapMaybe (findPassword table width) hs
    return (result, length result)
