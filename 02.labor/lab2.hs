-- # 2. labor

-- I. Könyvtárfüggvények használata nélkül, definiáljuk azt a függvényt, amely meghatározza:

-- - egy szám számjegyeinek szorzatát (2 módszerrel),
szjSzorzat 0 = 1
szjSzorzat x 
    | x < 0 = error "negativ szam"
    | otherwise = mod x 10 * szjSzorzat(div x 10)

szjSzorzat2 x
    | x < 0 = error "negativ szam"
    | x == 0 = 1
    | otherwise = mod x 10 * szjSzorzat2(div x 10)

-- - egy szám számjegyeinek összegét (2 módszerrel),
szjOsszeg 0 = 0
szjOsszeg x = mod x 10 + szjOsszeg(div x 10)

szjOsszeg2 x
    | x < 0 = error "Negativ szam"
    | x == 0 = 0
    | otherwise = mod x 10 + szjOsszeg2(div x 10)

-- - egy szám számjegyeinek számát (2 módszerrel),
szjSzam 0 = 0
szjSzam x = 1 + szjSzam(div x 10)

szjSzam2 x
    | x < 0 = szjSzam2(abs x)
    | x == 0 = 0
    |otherwise = 1 + szjSzam2(div x 10)

-- - egy szám azon számjegyeinek összegét, mely paraméterként van megadva, pl. legyen a függvény neve fugv4, ekkor a következő meghívásra, a következő eredményt kell kapjuk:

--   ```haskell
--   > fugv4 577723707 7
--   35
--   ```
-- - egy szám páros számjegyeinek számát,
-- - egy szám legnagyobb számjegyét,
-- - egy szám $b$ számrendszerbeli alakjában a $d$-vel egyenlő számjegyek számát (például a $b = 10$-es számrendszerben a $d = 2$-es számjegyek száma),
--   Példák függvényhívásokra:

--   ```haskell
--   fugv 7673573 10 7 -> 3
--   fugv 1024 2 1 -> 1
--   fugv 1023 2 1 -> 10
--   fugv 345281 16 4 -> 2
--   ```
-- - az 1000-ik Fibonacci számot.

-- II. Alkalmazzuk a map függvényt a I.-nél megírt függvényekre.

-- **Megoldott feladatok:**

-- - Határozzuk meg egy szám számjegyeinek összegét:
--   I. módszer:

--   ```haskell
--   szOsszeg :: Int -> Int
--   szOsszeg 0 = 0
--   szOsszeg x = ( x `mod` 10 ) + szOsszeg (x `div` 10)

--   > szOsszeg 123
--   ```

--   II. módszer:

--   ```haskell
--   szOsszeg1 :: Int -> Int -> Int
--   szOsszeg1 0 t = t
--   szOsszeg1 x t = szOsszeg1 (x `div` 10) ( t + x `mod` 10 )

--   > szOsszeg1 123 0
--   ```

--main = print (szjSzorzat 1234)

main :: IO ()
main = do
    let fel1 = szjSzorzat 1234
    print fel1