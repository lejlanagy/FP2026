import Data.List (sort)
import Data.Char
import Numeric

-- # 7. labor

-- I. Írjunk egy-egy Haskell függvényt, amely

-- - az n-nél kisebb négyzetszámokat kiírja egy szövegállományba,

writeSquares n path = do
    let squares = [ x * x | x <- [1..], x * x < n ]
    let content = unlines $ map show squares
    writeFile path content

-- - az n-nél kisebb számok négyzetgyökét kiírja egy szövegállományba. A nr négyzetgyök meghatározásához használjuk a következőket, 
--   ahol az iterációt addig kell végezni, amíg $x_{n+1}$ nem egyenlő $x_n$-nel:
--   ```
--   x_0 = 1
--   x_{n+1} = (x_n + nr/x_n)/2
--   ```

mySqrt nr = sqrtIter 1.0
    where
        sqrtIter xn
            | xnext == xn = xn
            | otherwise = sqrtIter xnext
            where xnext = (xn + nr / xn) / 2.0

writeRoots :: Double -> FilePath -> IO ()
writeRoots n path = do
    -- Az [1.0 .. n-1] tartományt használjuk
    let roots = [ mySqrt x | x <- [1.0 .. (n - 1.0)] ] :: [Double]
    let content = unlines (map show roots)
    writeFile path content

-- - az n-nél kisebb számok köbgyökét kiírja egy szövegállományba.A köbgyök meghatározásához használjuk a következőket, ahol az iterációt addig kell végezni, amíg $x_{n+1}$ nem egyenlő $x_n$-nel:

--   ```
--   x_0 = 1
--   x_{n+1} = (2·x_n + nr/(x_n·x_n))/3
--   ```

myCubeRoot nr = cubeIter 1.0
    where
        cubeIter xn 
            | xnext == xn = xn
            | otherwise = cubeIter xnext
            where xnext = (2.0 * xn + nr / (xn * xn)) / 3.0

writeCubeRoots :: Double -> FilePath -> IO ()
writeCubeRoots n path = do
    let cubeRoots = [ myCubeRoot x | x <- [1.0 .. (n-1.0)] ] :: [Double]
    let content = unlines (map show cubeRoots)
    writeFile path content

-- II. Írjunk egy-egy Haskell függvényt, amely szövegállományban levő számokat olvas be egy listába, és kiírja formázva egy másik szövegállományba

-- - a számok rendezett sorrendjét,

sortNumbers inputPath outputPath = do 
    content <- readFile inputPath
    let numbers = map read (words content) :: [Double]
    let sorted = sort numbers
    writeFile outputPath (unlines (map show sorted))

-- - a számokkal együtt a számok 2-es számrendszerbeli alakját, illetve, hogy hány egyes szerepel a 2-es számrendszerbeli alakban,

toBin 0 = "0"
toBin n = reverse (helper n)
    where
        helper 0 = ""
        helper x = show (x `mod` 2) ++ helper (x `div` 2)

countOnes s = length (filter (== '1') s)

writeBinaryInfo inputPath outputPath = do
    content <- readFile inputPath
    let numbers = map read (words content)
    let formatted = [ show n ++ " " ++ toBin (round n) ++ " " ++ show (countOnes (toBin (round n))) | n <- numbers ]
    writeFile outputPath (unlines formatted)

-- - a számokkal együtt a számok 2, 16, 256-os számrendszerbeli alakját,

digitToChar d = "0123456789ABCDEF" !! d

manualBase 0 _ = "0"
manualBase n base = reverse (helper n)
  where
    helper 0 = ""
    helper x = digitToChar (x `mod` base) : helper (x `div` base)

-- itt azert bonyolitjuk meg mert nem fernek bele a tartomanyba
toBase256 0 = "0"
toBase256 n = intercalate "." (map show (reverse (helper n)))
  where
    helper 0 = []
    helper x = (x `mod` 256) : helper (x `div` 256)
    -- Az intercalate-hez kell a Data.List, de megírható anélkül is:
    intercalate _ [] = ""
    intercalate _ [s] = s
    intercalate sep (h:t) = h ++ sep ++ intercalate sep t

writeMultipleBasesManual inputPath outputPath = do
    content <- readFile inputPath
    let numbers = map read (words content) :: [Int]
    
    let formatted = flip map numbers $ \n ->
            let b2   = manualBase n 2
                b16  = manualBase n 16
                b256 = toBase256 n
            in show n ++ " -> [B2: " ++ b2 ++ 
                         ", B16: " ++ b16 ++ 
                         ", B256: " ++ b256 ++ "]"
                         
    writeFile outputPath (unlines formatted)

-- - a számokkal együtt a számok prímosztóit.

primeFactors 0 = []
primeFactors n = factor n 2
    where 
        factor n d
            | n < 2      = []
            | n `mod` d == 0 = d : factor (n `div` d) d
            | d * d > n      = [n]
            | otherwise      = factor n (d + 1)

writePrimeFactors inputPath outputPath = do
    content <- readFile inputPath
    let numbers = map read (words content) :: [Int]
    let formatted = flip map numbers $ \n ->
            show n ++ " prime factors: " ++ show (primeFactors n)
    writeFile outputPath (unlines formatted)

-- III. Írjunk egy Haskell függvényt, amely amely kigenerálja egy állományba

-- - az $a$ és $b$ közötti Hamming számokat, használjuk a takeWhile, dropWhile függvényeket ($a > 300$),
-- - az 10000-nél kisebb prímszámokat, a prímszámokat Eratoszthenész szitájával határozzuk meg,
-- - az 10000-nél kisebb szerencsés számokat ([Lucky number](https://en.wikipedia.org/wiki/Lucky_number)).

-- IV. Írjunk egy-egy Haskell függvényt, amely

-- - meghatározza, hogy két bináris állományban milyen pozíciókon található különböző bájt,
-- - megvizsgálja, hogy egy adott bájtszekvencia benne van-e egy bináris állományban,
-- - meghatározza egy adott állomány bájtméretét, ahol az állománynevet a billentyűzetről olvassuk be,
-- - meghatározza bináris állományok méret szerinti rendezett sorrendjét, ahol az állományneveket a billentyűzetről olvassuk be,
-- - másolatot készít bináris állományokról, ahol az állományok nevét a billentyűzetről olvassuk be,

-- V. Írjunk egy Haskell programot, amely titkosítja karakterek (bájtok) egy adott listáját, majd vissza is fejti a rejtjelezett értéket:

-- - a titkosításhoz egy titkos információt, egy kulcsot (karaktereket/bájtokat) kell megadni,
-- - a titkosítás azt fogja jelenti, hogy a bemeneti bájtok és a kulcs bájtjai között alkalmazzuk az xor műveletet, úgy hogy a kulcs bájtjait körkörösen vesszük, ami azt jelenti, hogy ha elfogytak a kulcs bájtjai, akkor a kulcs első bájtjával folytatjuk az xor műveletet, egészen addig, amíg a bemenet bájtjain is végig nem mentünk,
-- - a helyes működés miatt fontos, hogy ugyanazt a kulcsot használjuk mind a titkosításhoz, mind a visszafejtéshez,
-- - a titkosított értéket hexadecimális string-ként írjuk ki,
-- - a program során legyen választási lehetőség arra vonatkozóan, hogy a kulcs értékét:
--   - beolvassuk a billentyűzetről, mint hexadecimális string
--   - véletlenszerűen generáljuk, mint 0 és 255 közötti természetes számok.

-- Például:

-- ```haskell
-- > bemenet = "sapientia marosvasarhelyi tudomanyegyetem"
-- > kulcs = "c 38 ff 66 71 22 38 4e 79 65"
-- > cryptStr bemenet kulcs
-- titkositott ertek: 7f 59 8f f 14 4c 4c 27 18 45 61 59 8d 9 2 54 59 3d 18 17 64 5d 93 1f 18 2 4c 3b 1d a 61 59 91 1f 14 45 41 2b d 0 61
-- ```
