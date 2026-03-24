-- # 5. labor

-- I. Írjuk meg a beépített splitAt, notElem, concat, repeat, replicate, cycle, iterate, any, all függvényeket.

--melyik index, milyen lista - szetvagas
--adott indexnel szetvalasztja a listat
mySplitAt idx ls = (idxElotti, idxUtani)
    where
        idxElotti = take idx ls
        idxUtani = drop idx ls

--egy adott elem nem resze a listanak - megnezi
myNotElem e (k : ls)
    | null ls = True
    | e == k = False
    | otherwise = myNotElem e ls

myElem e (k : ls)
    | null ls = False
    | e == k = True
    | otherwise = myElem e ls

-- tobb listat fuz ossze
--listakat tartalmazo listakatt var
myConcat lss = foldl1 (++) lss

--egy adott elemet ismetel es visszaadja listaban
myRepeat n = n : myRepeat n

--ezt csak azert h ne kelljen vegtelen listaval dolgozni
myTakeRepeat elemSzam n = take elemSzam $ myRepeat n

--egy erteket replikal adott hanyszor
myReplicate n e
    | n > 1 = e : myReplicate (n-1) e
    | otherwise = [e]

myReplicate2 n e
    | n == 1 = [e]
    | otherwise = e : myReplicate2 (n-1) e

--adott erteket ismetel, listakra is mukodik
myCycle ls = ls ++ myCycle ls

myTakeCycle n ls = take n ( myCycle ls )

--kap egy erteket es egy fgt, ugy hozza letre a listat h az ertekre alkalmazza a fgt
myIterate fg e = fg e : myIterate fg (fg e)

myTakeIterate n fg e = take n $ myIterate fg e

--egy adott feltetelnek barmelyik ertek megfelel-e
myAny feltetel (x:ls)
    | null ls = False
    | feltetel x = True
    | otherwise = myAny feltetel ls

--minden ertek megfelel-e
myAll feltetel (x:ls)
    | null ls = True
    | not (feltetel x) =  False
    | otherwise = myAll feltetel ls


-- II. Írjunk Haskell-függvényt, amely a foldl vagy a foldr függvényt alkalmazva

-- - implementálja a length, sum, elem, reverse, product, maximum, insert-sort, ++, map, filter függvényeket,
mylength ls = foldl (\res x -> res + 1) 0 ls

mylength2 ls = foldr (\x res -> res + 1) 0 ls

--

mysum ls = foldl (+) 0 ls
--
myelem e ls = foldl (\res x -> if x == e then True else res) False ls
--
myreverse ls = foldl (\res x -> x : res) [] ls

myreverse2 ls = foldr (\x res -> res ++ [x]) [] ls
--
myproduct ls = foldl (*) 1 ls
myproduct2 ls = foldl1 (*) ls
--
mymaximum ls = foldl1 (\res x -> if x > res then x else res) ls

-- beszurasos rendezes
myinsertsort [] = []
myinsertsort (x:xs) = insert x (myinsertsort xs)
    where
        insert y [] = [y]
        insert y (z:zs) = if y <= z then insert y:z:zs
                                    else z : insert y:zs

myinsertsort2 ls = foldr insert [] ls
    where 
        insert x [] = [x]
        insert x (y:ys) = if x <= y then x:y:ys
                                    else y : insert x ys

-- ++
listakFuz lss = foldl (++) [] lss

mymap fg ls = foldl (\res x -> res ++ [fg x]) [] ls

mymap2 fg ls = foldr (\x res -> fg x : res) [] ls

myfilter feltetel ls = foldl (\res x -> if feltetel x then res ++ [x] else res) [] ls

-- - meghatározza egy lista pozitív elemeinek összegét,

pozSum ls = foldl (\res x -> if x > 0 then res + x else res) 0 ls

-- - egy lista páros elemeinek szorzatát,
parosSzorzat ls = foldl(\res x -> if even x then res * x else res) 1 ls

-- - n-ig a négyzetszámokat.
negyzetSzamokN n = foldl (\res x -> res ++ [x ** 2]) [] [1..n]

-- - meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét: $$a_0 + x_0(a_1 + x_0(a_2 + x_0(a_3 + \ldots + x_0(a_{n-1}+ x_0 \cdot a_n))))$$

-- III.

-- - Írjunk egy Haskell-függvényt, amely egy String típusú listából meghatározza azokat a szavakat, amelyek karakterszáma a legkisebb. Például ha a lista a következő szavakat tartalmazza:  function class Float higher-order monad tuple variable Maybe recursion  akkor az eredmény-lista a következőkből áll: class Float monad tuple Maybe
-- - Írjunk egy talalat Haskell-függvényt, amely meghatározza azt a listát, amely a bemeneti listában megkeresi egy megadott elem előfordulási pozícióit.
--   Például a következő függvényhívások esetében az első az 5-ös előfordulási pozícióit, míg a második az e előfordulási pozícióinak listáját határozza meg.

--   ```haskell
--   > talalat 5 [3, 13, 5, 6, 7, 12, 5, 8, 5]
--   [2, 6, 8]
--   > talalat 'e' "Bigeri-vizeses"
--   [3,10,12]
--   ```
-- - Írjunk egy osszegT Haskell-függvényt, amely meghatározza egy (String, Int)értékpárokból álló lista esetében az értékpárok második elemeiből képzett összeget.
--   Például:

--   ```haskell
--   > ls = [("golya",120),("fecske",85),("cinege",132)]
--   > osszegT ls
--   337
--   ```
-- - Írjunk egy atlagTu Haskell-függvényt, amely egy kételemű, tuple elemtípusú lista esetében átlagértékeket számol a második elem szerepét betöltő listaelemeken. Az eredmény egy tuple elemtípusú lista legyen, amelynek kiíratása során a tuple-elemeket formázzuk, és külön sorba írjuk őket.
--   Például:

--   ```haskell
--   > :set +m
--   > ls = [("mari",[10, 6, 5.5, 8]), ("feri",[8.5, 9.5]),
--   | ("zsuzsa",[4.5, 7.9, 10]),("levi", [8.5, 9.5, 10, 7.5])]
--   > atlagTu ls
--   mari 7.375
--   feri 9.0
--   zsuzsa 7.466666666666666
--   levi 8.875
--   ```
