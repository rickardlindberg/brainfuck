module Brainfuck where

import qualified Data.Char as C
import qualified Data.Map as M
import Text.ParserCombinators.Parsec

-- Data typeclass

class Data d where
    emptyData       :: d
    dataGet         :: d -> Int
    dataModifyValue :: (Int -> Int) -> d -> d
    dataModifyPos   :: (Int -> Int) -> d -> d

dataMoveRight :: Data d => d -> d
dataMoveRight = dataModifyPos inc

dataMoveLeft :: Data d => d -> d
dataMoveLeft = dataModifyPos dec

dataGetAscii :: Data d => d -> Char
dataGetAscii dat = C.chr $ dataGet dat

dataWriteAscii :: Data d => Char -> d -> d
dataWriteAscii i = dataModifyValue (const $ C.ord i)

dataIncValue :: Data d => d -> d
dataIncValue = dataModifyValue inc

dataDecValue :: Data d => d -> d
dataDecValue = dataModifyValue dec

inc :: Int -> Int
inc = (+1)

dec :: Int -> Int
dec x = x - 1

-- Different instances of the data typeclass

data DataMap = DataMap
    { currentPos :: Int
    , values     :: M.Map Int Int
    }

emptyDataMap :: DataMap
emptyDataMap = DataMap 0 M.empty

instance Data DataMap where
    emptyData = emptyDataMap

    dataGet dat = M.findWithDefault 0 (currentPos dat) (values dat)

    dataModifyValue fn dat = dat { values = newValues }
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

    dataModifyValue fn (CachingData Nothing  dataMap) = CachingData (Just $ fn $ dataGet dataMap) dataMap
    dataModifyValue fn (CachingData (Just x) dataMap) = CachingData (Just $ fn x)                 dataMap

    dataModifyPos fn (CachingData Nothing  dataMap) = CachingData Nothing $ dataModifyPos fn dataMap
    dataModifyPos fn (CachingData (Just x) dataMap) = CachingData Nothing $ dataModifyPos fn $ dataModifyValue (const x) dataMap

-- The brainfuck code

data Token
    = TInc
    | TDec
    | TLeft
    | TRight
    | TPrint
    | TRead
    | TLoop [Token]
    deriving (Show, Eq)

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

parseTokens :: String -> [Token]
parseTokens input =
    case parse bfTokens fileName (removeComments input) of
        Left  err -> error (show err)
        Right x   -> x
    where
        fileName :: String
        fileName = ""
        removeComments :: String -> String
        removeComments = filter (`elem` "+-<>.,[]")
        bfTokens :: Parser [Token]
        bfTokens = many bfToken
        bfToken :: Parser Token
        bfToken  =  fmap (const TInc)   (char '+')
                <|> fmap (const TDec)   (char '-')
                <|> fmap (const TLeft)  (char '<')
                <|> fmap (const TRight) (char '>')
                <|> fmap (const TPrint) (char '.')
                <|> fmap (const TRead)  (char ',')
                <|> fmap TLoop          (between (char '[') (char ']')
                                                 bfTokens)

compile :: String -> ByteCode
compile = toByteCode . parseTokens

run :: Data d => ByteCode -> d -> String -> String
run BEND               dat input  = "done!\n"
run (BInc   next)      dat input  =                      run next (dataIncValue dat)     input
run (BDec   next)      dat input  =                      run next (dataDecValue dat)     input
run (BLeft  next)      dat input  =                      run next (dataMoveLeft dat)     input
run (BRight next)      dat input  =                      run next (dataMoveRight dat)    input
run (BPrint next)      dat input  = (dataGetAscii dat) : run next dat                    input
run (BRead  next)      dat (i:is) =                      run next (dataWriteAscii i dat) is
run (BLoop  loop next) dat input
    | dataGet dat == 0            =                      run next dat                    input
    | otherwise                   =                      run loop dat                    input

execute :: String -> IO ()
execute program = interact (run (compile program) emptyCachingDataMap)
