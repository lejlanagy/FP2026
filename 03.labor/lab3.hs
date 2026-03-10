import Prelude
import System.Win32 (xBUTTON1, SECURITY_ATTRIBUTES (nLength))
-- # 3. labor

-- I. Mit csinálnak az alábbi függvényhívások, ahol az atlag a számok átlagát meghatározó függvény?

-- ```haskell
atlag :: (Floating a) => [a] -> a
atlag ls = (sum ls) / fromIntegral (length ls)

-- > (atlag . filter (>= 4.5)) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > atlag $ filter (< 4.5) [6.5, 7.4, 8.9, 9.5, 3.5, 6.3, 4.2]
-- > (take 4 . reverse . filter odd ) [1..20]
-- > take 4 . reverse . filter odd $ [1..20]
-- > take 4 ( reverse ( filter odd [1..20]))
-- > take 4 $ reverse $ filter odd $ [1..20]
-- ```
--a $ egy funvtion application, zarojelek helyett, tobb fg osszefuzese
--pl (f g) [...]
--   f g $ [..]
--   f (g [..])
--   f $ g [..]

-- II. Könyvtárfüggvények használata nélkül írjuk meg azt a Haskell függvényt, amely

-- - meghatározza egy lista elemszámát, 2 módszerrel (myLength),

myLength [] = 0
myLength (x : xs) = 1 + myLength xs 

myLength2 ls = foldr (\_ db -> 1 + db) 0 ls

myLength3 xs = foldr (\x -> (+) 1) 0 xs

myLength5 ls = foldl (\x db -> 1 + db) 0 ls

myLength4 [] res = res
myLength4 (x:xs) res = myLength4 xs (res + 1)

-- - összeszorozza a lista elemeit, 2 módszerrel (myProduct),

myProduct [] = 1
myProduct (x : xs) = x * myProduct xs

myProduct2 [] res = res
myProduct2 (x : xs) res = myProduct2 xs (res * x)

myProduct3 ls = foldr (*) 1 ls

myProduct4 ls = foldl (*) 1 ls

myProduct5 ls = foldr1 (*) ls

myProduct6 ls = foldl1 (*) ls

--nagyobb szamokra
--myProduct7 ls = foldl' (*) 1 ls

-- - meghatározza egy lista legkisebb elemét (myMinimum),
myMinimum [x] = x
myMinimum (x1 : x2 : xs) = if x1 < x2 then myMinimum (x1:xs) else myMinimum (x2:xs)

myMinimum2 [x] = x
myMinimum2 (x1:x2:xs)
    | x1 < x2 = myMinimum2(x1:xs)
    | otherwise = myMinimum2(x2:xs)

myMinimum3 ls = minimum ls

myMinimum4 ls = foldr1 min ls

-- - meghatározza egy lista legnagyobb elemét (myMaximum),

myMaximum [x] = x
myMaximum (x1 : x2 : xs) = if x1 > x2 then myMaximum (x1:xs) else myMaximum (x2:xs)

myMaximum2 [x] = x
myMaximum2 (x1:x2:xs)
    | x1 > x2 = myMaximum2(x1:xs)
    | otherwise = myMaximum2(x2:xs)

myMaximum3 ls = maximum ls

myMaximum4 ls = foldl1 max ls

-- - meghatározza egy lista n-ik elemét (!!),

listaN ls n = ls !! n

listaN2 :: [a] -> Int -> a
listaN2 ls n
    | null ls        = error "ures lista"
    | length ls <= n = error "tul nagy index"
    | n < 0          = error "negativ index"
    | otherwise      = ls !! n


-- - egymásután fűzi a paraméterként megadott két listát (++),

listaFuz ls1 ls2 = ls1 ++ ls2
listaFuz2 ls1 ls2 = (++) ls1 ls2

-- - megállapítja egy listáról, hogy az palindrom-e vagy sem,

palindrom ls = if ls == reverse ls then "palindrom"
                else  "nem palindrom"

palindrom2 [] = True
palindrom2 [x] = True
palindrom2 ls = (head ls == last ls) && palindrom2 (init $ tail ls)

-- - meghatározza egy egész szám számjegyeinek listáját,

szjLs x
    | x < 10 = [x]
    | otherwise = szjLs (div x 10) ++ [mod x 10]

szjLs2 x = reverse (sg x)
    where 
        sg x
            | x < 10 = [x]
            | otherwise = (mod x 10) : szjLs2 (div x 10)

-- - a lista első elemét elköltözteti a lista végére,

elsoUtolso (x:xs) = xs ++ [x]

elsoUtolso2 xs = tail xs ++ [head xs]

-- - meghatározza egy egész elemű lista elemeinek átlagértékét,

lsAtlag ls = osszeg / hossz
    where 
        osszeg = sum ls
        hossz = fromIntegral (length ls)

-- - meghatározza egy 10-es számrendszerbeli szám p számrendszerbeli alakját,
decP x p = decP (div x p) p ++ [mod x p]

decP2 x p
    | x < p = [x]
    | otherwise = decP2 (div x p) p ++ [mod x p]

-- - meghatározza egy p számrendszerben megadott szám számjegyei alapján a megfelelő 10-es számrendszerbeli számot.

pDec ls p = foldl (\sg x -> sg * p + x) 0 ls

pDec2 x p = [i + (p ^ hatvany) | (i, hatvany) <- zip (szamjegyek x p) [0..]]
    where 
        szamjegyek x p
            | x < p = [x]
            | otherwise = szamjegyek (div x 10) p ++ [mod x 10]

-- III. Alkalmazzuk a map függvényt a II.-nél megírt függvényekre.
ls1 = [[1,2,3],[1..10]]
myLengthMap = map myLength ls1

myProductMap ls = map myProduct ls

-- IV. Írjunk egy Haskell függvényt, amely meghatározza a $$P(x) = a_0 + a_1 x + a_2 x^2 + \ldots + a_n x^n$$ polinom adott $x_0$ értékre való behelyettesítési értékét.

-- V. Ha adva van egy P pont koordinátája a kétdimenziós síkban, és adott az lsP pontok egy listája, írjunk egy Haskell függvényt, amely meghatározza azt az lsP-beli P1 pontot, amely legközelebb van a P ponthoz.
