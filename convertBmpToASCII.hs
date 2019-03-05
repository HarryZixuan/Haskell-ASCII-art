import Codec.BMP
import Data.ByteString
import Data.Either
import GHC.Word
import System.IO.Unsafe

loadBitmap :: FilePath -> [[(Int, Int, Int)]]
loadBitmap filename = repackAs2DList (either returnEmptyOnError processDataOnBMP (unsafePerformIO (readBMP filename)))

returnEmptyOnError :: Error -> ([(Int, Int, Int)], (Int, Int))
returnEmptyOnError _ = ([], (0, 0))

processDataOnBMP :: BMP -> ([(Int, Int, Int)], (Int, Int))
processDataOnBMP bmp = ((parseIntoRGBVals (convertToInts (unpack (unpackBMPToRGBA32 bmp)))), (bmpDimensions bmp))

convertToInts :: [Word8] -> [Int]
convertToInts [] = []
convertToInts (h:t) = (fromIntegral (toInteger h)) : (convertToInts t)

parseIntoRGBVals :: [Int] -> [(Int, Int, Int)]
parseIntoRGBVals [] = []
parseIntoRGBVals (h:i:j:_:t) = (h,i,j) : (parseIntoRGBVals t)

repackAs2DList :: ([(Int, Int, Int)], (Int, Int)) -> [[(Int, Int, Int)]]
repackAs2DList (pixels, (width, height)) = (Prelude.reverse (repackAs2DList' pixels width height))

repackAs2DList' :: [(Int, Int, Int)] -> Int -> Int -> [[(Int, Int, Int)]]
repackAs2DList' []  width  height = []
repackAs2DList' pixels width height = (Prelude.take width pixels) : (repackAs2DList' (Prelude.drop width pixels) width height)

showAsASCIIArt :: [[Char]] -> IO ()
showAsASCIIArt pixels = Prelude.putStr (unlines pixels)

-------------------------------------------------------------------------------
-- #####run this command to test##### --
--showAsASCIIArt (question1 ".-+*#" True (loadBitmap "sample_image_to_search.bmp"))

question1 :: [Char] -> Bool -> [[(Int, Int, Int)]] -> [[Char]]
question1 greyscale notConvert pixelList
 |notConvert == True =  (question1Control1 pixelList (greyscale))
 |notConvert == False = (question1Control1 pixelList (reverseList greyscale))

question1Control1 :: [[(Int, Int, Int)]] -> [Char] ->[[Char]]
question1Control1 [] greyscale = []
question1Control1 (h:t) greyscale = (questionControl2 h greyscale) : (question1Control1 t greyscale)

questionControl2 :: [(Int, Int, Int)] -> [Char] -> [Char]
questionControl2 [] greyscale = []
questionControl2 (h:t) greyscale = (convertRGBtoChar h greyscale) : (questionControl2 t greyscale)



convertRGBtoChar :: (Int, Int, Int) -> [Char] -> Char
convertRGBtoChar (r,g,b) greyscale
  |r >= 255 && g >= 255 && b >= 255 = greyscale !! (len(greyscale)-1) --otherwise may cause outOfBoundException
  |otherwise = greyscale !! (convertDoubleToInt (((((fromIntegral (r::Int) :: Double )+(fromIntegral (g::Int) :: Double)+(fromIntegral (b::Int) :: Double ))/3)/(255/fromIntegral(len(greyscale)::Int)::Double))))
-- greyscale !! (sum of rgb /3)/(225/len)

-- only keeps Integer
convertDoubleToInt :: Double -> Int
convertDoubleToInt num
  |(fromIntegral (round(num)::Int) :: Double) - num <= 0 = round(num)
  |(fromIntegral (round(num)::Int) :: Double) - num > 0 = round(num) -1


len :: [Char] -> Int
len [] = 0
len (h:t) = 1 + len t

reverseList :: [Char] -> [Char]
reverseList [] = []
reverseList (h:t) = reverseList t ++ [h]
