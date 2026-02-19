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