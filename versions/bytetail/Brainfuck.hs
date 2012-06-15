module Brainfuck where

import Data.Char (chr, ord)
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

---

class Data d where
    emptyData       :: d
    dataGet         :: d -> Int
    dataModifyValue :: d -> (Int -> Int) -> d
    dataModifyPos   :: (Int -> Int) -> d -> d

dataMoveRight :: Data d => d -> d
dataMoveRight = dataModifyPos (+1)

dataMoveLeft :: Data d => d -> d
dataMoveLeft = dataModifyPos (\x -> x - 1)

data DataMap = DataMap
    { currentPos :: Int
    , values     :: M.Map Int Int
    }

emptyDataMap :: DataMap
emptyDataMap = DataMap 0 M.empty

instance Data DataMap where
    emptyData = emptyDataMap

    dataGet dat = M.findWithDefault 0 (currentPos dat) (values dat)

    dataModifyValue dat fn = dat { values = newValues }
        where
            value     = M.findWithDefault 0 (currentPos dat) (values dat)
            newValues = M.insert (currentPos dat) (fn value) (values dat)

    dataModifyPos fn dat = dat { currentPos = fn (currentPos dat) }

data CachingData = CachingData
    { currentValue :: Maybe Int
    , dataMap      :: DataMap
    }

emptyCachingDataMap :: CachingData
emptyCachingDataMap = CachingData (Just 0) emptyDataMap

instance Data CachingData where
    emptyData = emptyCachingDataMap

    dataGet (CachingData Nothing dataMap) = dataGet dataMap
    dataGet (CachingData (Just x) _)      = x

    dataModifyValue (CachingData Nothing  dataMap) fn = CachingData (Just $ fn $ dataGet dataMap) dataMap
    dataModifyValue (CachingData (Just x) dataMap) fn = CachingData (Just $ fn x)                 dataMap

    dataModifyPos fn (CachingData Nothing  dataMap) = CachingData Nothing $ dataModifyPos fn dataMap
    dataModifyPos fn (CachingData (Just x) dataMap) = CachingData Nothing $ dataModifyPos fn $ dataModifyValue dataMap $ const x

---

data ByteCode
    = BInc   ByteCode
    | BDec   ByteCode
    | BLeft  ByteCode
    | BRight ByteCode
    | BPrint ByteCode
    | BRead  ByteCode
    | BLoop  ByteCode ByteCode
    | BEND
    deriving (Show, Eq)

data Token
    = TInc
    | TDec
    | TLeft
    | TRight
    | TPrint
    | TRead
    | TLoop [Token]
    deriving (Show, Eq)

parseTokens :: String -> [Token]
parseTokens input =
    case parse bfTokens "" (filter (`elem` "+-<>,.[]") input) of
        Left  err -> error (show err)
        Right x   -> x
    where
        bfTokens :: Parser [Token]
        bfTokens =  many bfToken
        bfToken  =  fmap (const TInc)   (char '+')
                <|> fmap (const TDec)   (char '-')
                <|> fmap (const TLeft)  (char '<')
                <|> fmap (const TRight) (char '>')
                <|> fmap (const TPrint) (char '.')
                <|> fmap (const TRead)  (char ',')
                <|> fmap TLoop          (between (char '[') (char ']') bfTokens)

toByteCode :: [Token] -> ByteCode
toByteCode tokens = toByteCode' tokens BEND
    where
        toByteCode' :: [Token] -> ByteCode -> ByteCode
        toByteCode' []            end = end
        toByteCode' (TInc    :xs) end = BInc   (toByteCode' xs end)
        toByteCode' (TDec    :xs) end = BDec   (toByteCode' xs end)
        toByteCode' (TLeft   :xs) end = BLeft  (toByteCode' xs end)
        toByteCode' (TRight  :xs) end = BRight (toByteCode' xs end)
        toByteCode' (TPrint  :xs) end = BPrint (toByteCode' xs end)
        toByteCode' (TRead   :xs) end = BRead  (toByteCode' xs end)
        toByteCode' (TLoop ls:xs) end = let inner = toByteCode' ls loop
                                            rest  = toByteCode' xs end
                                            loop  = BLoop inner rest
                                        in  loop

run :: Data d => ByteCode -> String -> d -> String
run BEND               input  dat = "done!\n"
run (BInc   next)      input  dat = run next input (dataModifyValue dat (+1))
run (BDec   next)      input  dat = run next input (dataModifyValue dat (\x -> x - 1))
run (BLeft  next)      input  dat = run next input (dataMoveLeft dat)
run (BRight next)      input  dat = run next input (dataMoveRight dat)
run (BPrint next)      input  dat = chr (dataGet dat) : run next input dat
run (BRead  next)      (i:is) dat = run next is (dataModifyValue dat (const (ord i)))
run (BLoop  loop next) input  dat = if dataGet dat == 0
                                        then run next input dat
                                        else run loop input dat

execute :: String -> IO ()
execute program = interact (\input -> run (toByteCode (parseTokens program)) input emptyCachingDataMap)
