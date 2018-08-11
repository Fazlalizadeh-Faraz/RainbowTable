# RainbowTable


RanbowAssign provided by cmpt 383 prof (GB)

Main Idea: reversing some hash functions and figure out the corresponding passwords


Important Functions:
  1) pwHash: convert string (pswd) to a hash value
  2) pwReduce: convert hash value to possible pswd

How to load 
  1) open ghci
  2) :c where ever you save the files
  3) :l assignment1.hs
  

  4) creating table:
  *Main> :t rainbowTable
    rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
  *Main> rainbowTable 2 ["dccdecee","cdeccaed","acbcaeec","eeeeaebd","ccdccbeb"]
 
   or simpler -> just call generateTable
   
   
   some Examples:
   
   *Main> let table = rainbowTable 40 ["abcdeabc", "aabbccdd", "eeeeeeee"]
   *Main> findPassword table 40 1726491528
   Just "abcdeabc"
   *Main> findPassword table 40 (-206342227)
   Just "dbddecab"
   *Main> findPassword table 40 1726491529
   Nothing
   *Main> findPassword table 40 0
   Nothing
