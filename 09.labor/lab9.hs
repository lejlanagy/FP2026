-- # 9. labor

import Data.List (dropWhile, sort, isPrefixOf, find, findIndex)
import Data.Char (isSpace, isUpper, isDigit, ord, isAlpha)
import Control.Concurrent.STM (check)

-- I. Formázzuk egy adott szövegállomány tartalmát a következőképpen: azok után az írásjelek után, amelyek benne vannak a $\{.,!?;\}$ halmazban szigorúan egy szóközt tegyünk, hagyjunk.
punctuation = ".,!?;"
formatText :: String -> String
formatText[] = []
formatText (x:xs)
    | x `elem` ".,!?;\\" = x : ' ' : formatText (dropWhile isSpace xs)
    | otherwise = x : formatText xs

f1 = do
    contents <- readFile "random.txt"
    let formatted = formatText contents
    --let formattedOnlyReduce = formatTextOnlyReduce contents
    writeFile "formatted.txt" formatted
    --writeFile "formattedOnlyReduce.txt" formattedOnlyReduce

-- II. Az [iban.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/iban.txt) állomány IBAN kódokat tartalmaz. Írjunk egy-egy Haskell függvényt, amely

-- - beolvassa, majd rendezi az állományban levő adatokat ábécé sorrendbe,


processSort :: FilePath -> FilePath -> IO ()
processSort inputFile outputFile = do
    contents <- readFile inputFile
    let ibanList = lines contents
    let sortedIbans = sort ibanList
    writeFile outputFile (unlines sortedIbans)

f2a = processSort "iban.txt" "sortedIban.txt"

-- - bináris keresést alkalmazva ellenőrzi, hogy egy megadott IBAN kód szerepel-e az adatok között,

binarySearch [] _ = False
--fg megkap egy rendezett listat es egy keresett kodot
binarySearch list target 
    | midElement == target = True
    | length list == 1 = False
    | target < midElement = binarySearch leftHalf target
    | otherwise = binarySearch rightHalf target
    where midIndex = length list `div` 2
          (leftHalf, midElement:rightHalf) = splitAt midIndex list

searchTarget = "GB82WEST12345698765432" -- például
checkIbanInFile fileName searchTarget = do 
    contents <- readFile fileName
    let sortedIbans = lines contents
    if binarySearch sortedIbans searchTarget
        then putStrLn $ "A(z) " ++ searchTarget ++ " szerepel az állományban."
        else putStrLn $ "A(z) " ++ searchTarget ++ " NEM szerepel az állományban."

f2b = do
    checkIbanInFile "sortedIban.txt" searchTarget

-- - átírja egy okIban.txt állományba azokat az IBAN kódokat, amelyek megfelelő formátumúak. Egy IBAN kód akkor tekinthető megfelelő formátumúnak
--   - ha csak számjegyeket és angol ábécébeli nagybetűket tartalmaz,
--   - ha az IBAN kód hossza megegyezik az országhoz tartozó hosszal, ahol az országhoz tartozó hosszérték az [ibanLength.txt](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/ibanLength.txt) állományból olvasható ki,
--   - ha az átcsoportosítás és a helyettesítés után kapott egész szám 97-el való osztási maradéka egyenlő eggyel, ahol
--     - átcsoportosítás: az IBAN kód első négy karakterét kitöröljük a kód elejéről és a kód végéhez fűzzük,
--     - helyettesítés:
--       - az alfanumerikus karaktereket helyettesítsük a következő kódokkal: $$A \to 10,\ B \to 11,\ \ldots,\ Z \to 35$$
--       - az így kapott karakterláncot egész számnak tekintjük

--   Például:
--   legyen az IBAN kód: $$\texttt{GB82WEST12345698765432}$$
--   - hossz: $$22$$
--   - átcsoportosítás:
--     $$\texttt{WEST12345698765432}\ \texttt{GB82}$$
--   - helyettesítés:
--     $$32142829\quad 12345698765432\quad 1611\quad 82$$
--   - ellenőrzés: $$3214282912345698765432161182 \bmod 97 = 1$$

--ellenorzes a nagybetukhoz es a szamokhoz
isAlphanumeric :: String -> Bool
isAlphanumeric = all (\c -> isUpper c || isDigit c)

-- karakter helyettesites szammal
charToValue :: Char -> String
charToValue c
    | isDigit c = [c]
    | isUpper c = show (ord c - ord 'A' + 10)
    | otherwise = error "Nem alfanumerikus karakter: " ++ [c]

-- 3. Az algoritmus magja: átcsoportosítás, helyettesítés és mod 97
isValidChecksum :: String -> Bool
isValidChecksum iban = 
    let (prefix, rest) = splitAt 4 iban
        rearranged = rest ++ prefix
        replaced = concatMap charToValue rearranged
        bigNumber = read replaced :: Integer
    in bigNumber `mod` 97 == 1

isValidIban lengths iban =
    let countryCode = take 2 iban
        expectedLength = lookup countryCode lengths
    in isAlphanumeric iban &&
       case expectedLength of
           Just len -> length iban == len && isValidChecksum iban
           Nothing  -> False

f2c = do
    contents <- readFile "iban.txt"
    lengthContents <- readFile "ibanLength.txt"
    let ibans = lines contents
    let lengths = map ((\[c, l] -> (c, read l :: Int)) . words) (lines lengthContents)
    let validIbans = filter (isValidIban lengths) ibans
    writeFile "okIban.txt" (unlines validIbans)

-- III. Egy szövegállományban egy adott személyről következő adatok vannak eltárolva: vezetéknév, keresztnév, születési dátum. Hozzuk létre a következő típusú adatszerkezeteket, majd olvassuk ki az adatokat az állományból és állapítsuk meg mindegyik személyről, hogy a hét milyen napján született és mikor van a névnapja. A névnapok megállapításához használhatjuk a [névnapokat](https://www.ms.sapientia.ro/~mgyongyi/Funk_Log/nevnapok.txt) tartalmazó szövegállományt.

data Datum = Datum {
  nap :: Int,
  honap:: Int,
  ev :: Int
} deriving (Show)

data Szemely = Szemely {
  vnev :: [Char],
  knev :: [Char],
  szdatum :: Datum
} deriving (Show)

honapNevToSzam :: String -> Int
honapNevToSzam nev =
    let honapok = ["januar", "februar", "marcius", "aprilis", "majus", "junius", "julius", "augusztus", "szeptember", "oktober", "november", "december"]
    in case findIndex (isPrefixOf nev) honapok of
        Just index -> index + 1
        Nothing -> 0

extractDigits :: String -> Int
extractDigits s = read (filter isDigit s)

parseNevnapSor :: String -> (String, (Int, Int))
parseNevnapSor sor =
    let nev = takeWhile (/= '(') sor 
        datumResz = dropWhile (/= '(') sor
        szavak = words (filter (`notElem` "(),") datumResz)
        h = honapNevToSzam (head szavak)
        n = extractDigits (last szavak)
    in (filter (not . isSpace) nev, (h, n))

parseSzemely :: String -> Szemely
parseSzemely s = let [vn, kn, e, h, n] = words s
                 in Szemely vn kn (Datum (read e) (read h) (read n))

hetNapja :: Datum -> String
hetNapja (Datum e h n) = 
    let (m, y) = if h < 3 then (h + 12, e - 1) else (h, e)
        k = y `mod` 100
        j = y `div` 100
        h_index = (n + (13 * (m + 1)) `div` 10 + k + k `div` 4 + j `div` 4 + 5 * j) `mod` 7
        napok = ["Szombat", "Vasárnap", "Hétfő", "Kedd", "Szerda", "Csütörtök", "Péntek"]
    in napok !! h_index

f3 = do
    nevnapTartalom <- readFile "nevnapok.txt"
    let nevnapAdatbazis = map parseNevnapSor (lines nevnapTartalom)
    
    -- Személyek beolvasása (Pl: "Kovacs Janos 1990 11 12")
    szemelyTartalom <- readFile "szemelyek.txt"
    let szemelyek = map parseSzemely (lines szemelyTartalom)
    
    mapM_ (\sz -> do
        let d = szdatum sz
            hNap = hetNapja d
            -- Megkeressük a keresztnevet a névnap adatbázisban
            talalat = lookup (knev sz) nevnapAdatbazis
            nevnapStr = case talalat of
                Just (h, n) -> show h ++ ". " ++ show n ++ "."
                Nothing     -> "Ismeretlen"
        
        putStrLn $ vnev sz ++ " " ++ knev sz ++ ":"
        putStrLn $ "  Szuletett: " ++ hNap
        putStrLn $ "  Nevnap: " ++ nevnapStr
        ) szemelyek