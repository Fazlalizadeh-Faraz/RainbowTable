# Rainbow Table Assignment 

RainbowAssign.hs was provided by the proffessor (GB). This was an assignment for CMPT 383. The project goal was to reverse a set of hash functions in order to decipher there corresponding passwords.


## Important Functions:
  1) pwHash: Converts a string (pswd) into a hash value
  2) pwReduce: Converts a hash value to possible pswd


## Steps to load 
  1) Open ghci
  2) Use ':c' at the location of your saved files
  3) Use ':l assignment1.hs'
  4) Creating a table:
  *Main> :t rainbowTable
    rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
  *Main> rainbowTable 2 ["dccdecee","cdeccaed","acbcaeec","eeeeaebd","ccdccbeb"]
 
   Alternatively, to create a table you can call the 'generateTable' function.
   
   
## Examples:
   
   *Main> let table = rainbowTable 40 ["abcdeabc", "aabbccdd", "eeeeeeee"]
   *Main> findPassword table 40 1726491528
   Just "abcdeabc"
   *Main> findPassword table 40 (-206342227)
   Just "dbddecab"
   *Main> findPassword table 40 1726491529
   Nothing
   *Main> findPassword table 40 0
   Nothing
