import System.Random
import System.IO.Unsafe
randomZeroToX :: Int -> Int
randomZeroToX x= unsafePerformIO (getStdRandom (randomR (0, x)))



users = ["user1","user2","user3","user4"]
items = ["item1","item2","item3","item4","item5","item6"]
purchasesHistory =[("user1",[["item1","item2","item3"],["item1","item2","item4"]]),("user2",[["item2","item5"],["item4","item5"]]),("user3",[["item3","item2"]]),("user4",[])]



createEmptyFreqList :: [a] -> [(a, [b])]
createEmptyFreqList []=[]
createEmptyFreqList(x:xs)= (((x, [])):createEmptyFreqList xs)

getAllUsersStats :: [(String, [[String]])] -> [(String, [(String, [(String, Int)])])]


getAllUsersStats  []=[]
getAllUsersStats ((x,y):xs)=((x,(helper y (createEmptyFreqList items) )):getAllUsersStats xs)

helper _ []=[]
helper [] x=x
helper y ((x,z):xs)=(x,(helper2 y x z items)):helper y xs

concat7 []=[]
concat7 (x:xs)=x ++ concat7 xs

helper2 _ _ _ []=[]
helper2 x y z (w:ws)=if y/=w then (helper3 y w x z) ++ helper2 x y z ws else helper2 x y z ws 

helper3 y w listofLists z=if helpcount y w listofLists/=0 then (w,(helpcount y w listofLists)):z else z 


helpermember y []=False
helpermember y (x:xs)=if y==x then True else helpermember y xs

helpcount y w []=0
helpcount y w (x:xs)=if helpermember y x then (helpcounter w x)+helpcount y w xs else helpcount y w xs


helpcounter w []=0
helpcounter w (x:xs)=if w==x then 1+helpcounter w xs else helpcounter w xs

freqListItems:: String -> [(String, Int)]
freqListItems x=helper77 x (getAllUsersStats purchasesHistory)

-- helper77 takes a string denoting the user and the the output of getAllUsersStats of type [(String, [(String, [(String, Int)])])]
helper77 _ [] = []
helper77 x ((y,ys):p)=if x==y then helper70 ys else helper77 x p


helper70 [] =[]
helper70 ((l,r):rs)=if helper71 r /=0 then (l,helper71 r):helper70 rs else helper70 rs

helper71 []=0
helper71 ((w,z):f)=z+helper71 f





recommendEmptyCart :: String -> String
recommendEmptyCart x=helper40 x purchasesHistory

helper40 x []=[]
helper40 x ((z,y):xs)=if z==x then helper41 y else helper40 x xs

helper41 y=if length (concat7 y)/=0 then  helper42 (concat7 y) else "" 

helper42 y= if y/=[] then y !! randomZeroToX ((length y)-1) else ""




freqListCart:: String ->[String] -> [(String, Int)]
freqListCart x y=helper11 x y (getAllUsersStats purchasesHistory)   

helper11 x y []=[]
helper11 x y ((w,z):xs) =if w==x then helper18 y z else helper11 x y xs


helper18 y z =helper19 (helper12 y z) items

helper19 x []=[]
helper19 x (y:ys)= if helper27 x y/=0 then (y,(helper27 x y)):helper19 x ys else helper19 x ys

helper27 [] _ =0
helper27 ((x,r):xs) y =if x==y then r+(helper27 xs y) else helper27 xs y

helper12  [] z=[]
helper12 (x:xs) z=helper13 x z ++ helper12 xs z

helper13 x []=[]
helper13 x ((y,ys):xs)=if x==y then ys ++ (helper13 x xs) else helper13 x xs









helper95 x []=[]
helper95 x ((y,ys):xs)= if x==y then ys else helper95 x xs 

helper96 _ [] = []
helper96 x ((y,ys):xs)=if x==y then helper96 x xs else (y,ys):helper96 x xs


freqListUsers:: String -> [(String, Int)]
freqListUsers x=helper110( fn (helper90 (purchasesIntersection (helper95 x (getAllUsersStats purchasesHistory)) (helper96 x (getAllUsersStats purchasesHistory))))  items)

helper90 []=[]
helper90(y:ys)= helper91 y ++ helper90 ys

helper91 []=[]
helper91 ((t,ts):tv)= ts++helper91 tv

fn _ []=[]
fn v (i:ik)=helper92 v (i,0):fn v ik

helper92 [] (i,r) = (i,r)
helper92 ((vt,vx):vs) (i,r) = if i==vt then helper92 vs (i,r+vx) else helper92 vs (i,r)

helper110 []=[]
helper110 ((l,r):ys)=if r==0 then helper110 ys else (l,r):helper110 ys




recommendBasedOnUsers :: String -> String

recommendBasedOnUsers x = helper42 (helper93 (freqListUsers x))

helper93 []=[]
helper93 (y:ys)= helper94 y ++ helper93 ys

helper94 (x,0)=[]
helper94(x,t)=x:helper94(x,t-1)




freqListCartAndItems:: String -> [String] -> [(String, Int)]
freqListCartAndItems x y=helper30 (freqListItems x) (freqListCart x y) 

helper30 x y=if (length x) >= (length y) then helper31 x y else helper31 y x  

helper31 [] y=[]
helper31 ((x,s):xs) y=(x,helper32 ((x,s)) y):helper31 xs y

helper32 ((x,s)) []=s
helper32 ((x,s)) ((y,f):ys)=if x==y then f+helper32 ((x,s)) ys else helper32 ((x,s)) ys





recommendBasedOnItemsInCart :: String -> [String] -> String

recommendBasedOnItemsInCart x y=helper42 (helper93 (freqListCartAndItems x y))

helper43 x y []=[]
helper43 x y ((z,ys):xs)=if z==x then helper44 y ys else helper43 x y xs

helper44 y ys= if length (concat7 ys)/=0 then helper42 (helper47 y (concat7 ys)) else ""

helper47 [] ys=ys
helper47 (x:xs) ys=helper47 xs (helper45 x ys)
helper45 y ys=remove y ys 


remove y []=[]
remove y (x:xs)= if y==x then remove y xs else x:remove y xs 





-- purchasesIntersection :: Eq a =>[(a,[(a,Int)])] -> [(a,[(a,[(a,Int)])])] -> [[(a,[(a,Int)])]]

purchasesIntersection :: [(String,[(String,Int)])] -> [(String,[(String,[(String,Int)])])] -> [[(String,[(String,Int)])]]
purchasesIntersection x y = helper103 x (helper96 (helper58 x (getAllUsersStats (purchasesHistory) )) (getAllUsersStats(purchasesHistory)))

helper58 x []=""
helper58 x ((z,y):ys)= if x==y then z else helper58 x ys

helper103 x []=[]
helper103 x (y:ys) = helper50 x y :helper103 x ys


-- helper50 finds the intersection between the current user's stats and the tuple of the other user (user, stats)
helper50 [] _=[]
helper50 (x:xs) (y,(z:zs))= (helper51 x z) ++ (helper50 xs (y,zs))

helper51 (x,r) (y,z)=if r /= [] && z/=[] then [(x, helper52 items (r++z))] else []

helper52 [] z=[]
helper52 (x:xs) z=if (helper53 x z)/=0 then (x,helper53 x z):helper52 xs z else helper52 xs z


helper53 x []=0
helper53 x ((z,y):zs)=if x==z then y+helper53 x zs else helper53 x zs





recommend :: String -> [String] -> String
recommend x y=if y==[] then helper120 ([recommendEmptyCart x]++[recommendBasedOnUsers x]) else helper120 ([recommendBasedOnItemsInCart x y]++[recommendBasedOnUsers x]) 


helper120 x=if length (remove "" x)/=0 then helper42 (remove "" x) else helper42 items 





