--fg-ek definialasa
--addition, substract, multiply, division
osszeg :: Num a => a -> a -> a
osszeg a b = a + b;

--bemenet -> bemenet -> kimenet
kulonbseg :: Double -> Double -> Double
kulonbseg a b = (-) a b

szorzat :: Num a => a -> a -> a
szorzat a b = (*) a b

hanyados1 :: Fractional a => a -> a -> a
hanyados1 a b = a / b

--ez csak int tipusra fog mukodni
hanyados2 :: Integral a => a -> a -> a
hanyados2 a b = div a b

--ugyanazokat a maradekokat adjak
maradek :: Integral a => a -> a -> a
maradek a b = mod a b

maradek2 :: Integral a => a -> a -> a
maradek2 a b = a `mod` b

--elsofoku egyenlet gyokenek meghatarozasa
elsoF a b = (-b) / a

--abszolut ertek
abszolut a = if a < 0 then -a else a
--fontosak a tordelesek
-- ifhez hasonlo a |
abszolut2 a
    | a < 0 = -a
    | otherwise = a

-- szam elojele
elojel a = if a < 0 then "negative" else if a > 0 then "positive" else "nulla"
elojel2 a 
    | a < 0 = "negativ"
    | a > 0 = "positive"
    | otherwise = "zero"

-- ket argumentum kozul a maximum es minimum
--van beepitett max maximum, min minimum argumentumra es listara
max1 a b = if a > b then a else b
max2 a b
    | a > b = a
    | otherwise = b

min1 a b = if a < b then a else b
min2 a b 
    | a < b = a
    |otherwise = b

-- masodfoku egyenlet gyokei
--a*(x**2) + b*x + c = 0
--delta = b**2 - 4*a*c
--x1 = (-b + sqrt(delta))/2a
--x2 = (-b - sqrt(delta))/2a
masodF a b c 
    | delta < 0 = error "komplex szamok"
    | otherwise = (gy1, gy2)
    where
        delta = b^2 - 4*a*c
        gy1 = (-b + sqrt delta ) / (2*a)
        gy2 = (-b - sqrt delta ) / (2*a)

--negyzetgyok - elso N db termeszetes szamra
negyzetgyokN n = [sqrt i | i <- [ 1 .. n ]]

negyzetN n = [i * i | i <- [1 .. n]]

--koboles
kobN n = [i ^ 3 | i <- [1 .. n]]

--nem negyzetszamok
nemNegyzetN n = [i | i <- [1 .. n], (sqrt i * sqrt i) /= i]

xhatvanyN x n = [x ^ i | i <- [1 .. n]]

osztokN n = [i | i <- [1 .. n], n `mod` i == 0, mod i 2 == 0]
osztokN2 n = [i | i <- [2,4 .. n], mod n i == 0]

osztok n = [i | i <- [1..n], mod n i == 0]
primszam n = osztok n == [1,n]

primszamokN n = [i | i <- [2..n], primszam i]

primszamokN2 n = [i | i <- [2..n], primszam i]
    where 
        primszamL n = osztokL n == [1,n]
        osztokL n = [i | i <- [1 .. n], mod n i == 0]

-- osszetett szamok listaja
--ami nem primszam
osszetettN n = [i | i <- [0..n], not(primszam i)]

--paratlan osszetett szamok listaja
paratlanOsszetettN n = [i | i <- [0..n], not(primszam i), mod i 2 /= 0]
paratlanOsszetettN2 n = [i | i <- [3,5..n], not(primszam i)]

--n-nel kisebb pitagoraszi szamharmasok
pitagorasz n = [(a,b,c) | c <- [1..n], b <- [1..c], a <- [1..b], a ** 2 + b ** 2 == c ** 2]

betuSzam = zip['a' .. 'z'] [0 .. 25]

szamok1 = zip [0 .. 10] [5,4 .. 0]
szamok2 n = [(i, n - i) | i <- [0 .. n]]
szamok3 n = zip [0 .. n] [n, n-1 .. 0]

--vegtelen lista
tfLs n = take n ls
    where 
        ls = [True,False] ++ ls

--kb ez lenne a main
main :: IO()
main = do
    putStrLn "x hatvany n" 
    print(xhatvanyN 5 3)
    putStrLn "paros osztok"
    print(osztokN 48)
    putStrLn "osszetett szamok listaja"
    print(osszetettN 48)
    putStrLn "n-nel kisebb pitagoraszi szamharmasok"
    print(pitagorasz 100)
    putStrLn ("n-nel kisebb pitagoraszi szamharmasok" ++ show (pitagorasz 50))
    print betuSzam
    print szamok1
    print(szamok2 10)
    print(szamok3 10)
    putStrLn("5 True,False" ++ show (tfLs 5))