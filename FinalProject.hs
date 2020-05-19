import DataFile

--wordToken:: String -> [String]
--wordToken s = words (putSpace s)
wordToken s = split (putSpace s) []

putSpace [] = []
putSpace (x:xs) | elem x punct = ' ':x:' ':putSpace(xs)
                | otherwise = x:putSpace(xs)
                
split [] acc = if acc==[] then [] else [acc]
split (x:xs) acc	| (x==' ' && acc/=" " && acc/="") = acc : (split xs [])
					| (x/=' ') =  split xs (acc ++ [x])
					| otherwise = split xs []

wordTokenList :: [String] -> [String]
wordTokenList [] = []
wordTokenList (x:xs) = wordToken x ++ wordTokenList xs 

bigramsHelper [] acc= acc
bigramsHelper [_] acc= acc
bigramsHelper (x:y:res) acc =if elem (x,y) acc then bigramsHelper (y:res) acc 
                                               else bigramsHelper (y:res) (acc++[(x,y)]) 

uniqueBigrams inp =bigramsHelper inp []

trigramsHelper [] acc= acc
trigramsHelper [_] acc= acc
trigramsHelper [_,_] acc= acc
trigramsHelper (x:y:z:res) acc =if elem (x,y,z) acc then trigramsHelper (y:z:res) acc 
                                               else trigramsHelper (y:z:res) (acc++[(x,y,z)]) 

uniqueTrigrams inp =trigramsHelper inp []


bigramsFreq listOfWords = countBigrams (uniqueBigrams listOfWords) listOfWords

countBigrams [] _ = []
countBigrams (bigr:bigrs) listOfWords = getTriple (bigr,0) listOfWords : ( countBigrams bigrs listOfWords )

getTriple triple []=triple
getTriple triple [_]=triple

getTriple ((x,y),i) (wx:wy:ws) | x==wx && y==wy = getTriple ((x,y),i+1) (wy:ws) 
                               |otherwise = getTriple ((x,y),i) (wy:ws)

trigramsFreq listOfWords = countTrigrams (uniqueTrigrams listOfWords) listOfWords

countTrigrams [] _ = []
countTrigrams (trigr:trigrs) listOfWords = getQuadriple (trigr,0) listOfWords : ( countTrigrams trigrs listOfWords )

getQuadriple quad []=quad
getQuadriple quad [_]=quad
getQuadriple quad [_,_]=quad

getQuadriple ((x,y,z),i) (wx:wy:wz:ws) | x==wx && y==wy && z==wz = getQuadriple ((x,y,z),i+1) (wy:wz:ws) 
                               |otherwise = getQuadriple ((x,y,z),i) (wy:wz:ws)


getFreq _ [] =0
getFreq a ((b,freq):xs) | a==b = freq
                        | otherwise = getFreq a xs


generateOneProb x []=0
generateOneProb ((a,b,c),freq) (((x,y),freq2):xs) |a==x && b==y = freq/freq2
                                                  |otherwise = generateOneProb ((a,b,c),freq) xs 
                                                  
genProbPairs [] _ =[]
genProbPairs ((tri,freq):tris) bigrams = (tri,generateOneProb (tri,freq) bigrams) : genProbPairs tris bigrams

goodPair [x,y] ((a,b,_),p) =if x==a && y==b && p>0.03 then True
                                                      else False
generateNextWord prefix probList = if l==[] then error  "Sorry, it is not possible to infer from current database"
                                            else target
    where l = filter (goodPair prefix) probList  
          ((_,_,target),_)=  l !! randomZeroToX ((length l)-1)
          

generateText str n |n==0 = str
                   |otherwise = generateText str2 (n-1)
                   where str2= str++" "++newString
                         newString=generateNextWord (getLastTwo str) probList
                         probList=genProbPairs (trigramsFreq flattenedList) (bigramsFreq flattenedList)
                         flattenedList=wordTokenList docs


getLastTwo str= [x,y]
        where (y:x:xs)=reverse (wordToken str)


