module Numeric.DigitGroup where

import Data.List
import Data.List.Split 
import Data.Char
import Numeric

digitGroupIntegral :: Integral a => Char -> p -> a -> [Char]
digitGroupIntegral separator _ n = (if n < 0 then "-" else "") 
                  ++ (reverse $ intercalate [separator] $ chunksOf 3 (reverse $ map (intToDigit . fromIntegral)  $ nums n))  
                  where 
                   nums x = reverse $  unfoldr (\x -> case  divMod x 10 of  { (0,0) -> Nothing  ; (x,y) -> Just (y,x)}) x  

digitGroupRealFrac :: RealFloat a => Char -> Maybe Char -> a -> [Char]
digitGroupRealFrac separator1 separator2 n = (if n < 0 then "-" else "") 
             ++ reverse (intercalate [separator1] (chunksOf 3 $ reverse whole')) 
             ++ "." ++  intercalate separator2' (chunksOf 3 frac)
  where 
  separator2' = case separator2 of { (Just x) -> [x] ; _ -> [] } 
  whole' = if whole == [] then "0" else whole
  (whole,frac)  = splitAt y digitString
  digitString = map intToDigit ( replicate (if y < 0 then abs y else 0 ) 0 ++ xs ++ replicate (y-length xs+if y >= 1 then 1 else 0 ) 0) 
  (xs,y) = floatToDigits 10 n


class DigitGroupShow a where
  digitGroup :: Char -> Maybe Char -> a -> String

instance DigitGroupShow Integer where
  digitGroup separator _ x = digitGroupIntegral separator Nothing x

instance DigitGroupShow Int where
  digitGroup separator _ x = digitGroupIntegral separator Nothing x

instance DigitGroupShow Float where
  digitGroup separator1 separator2 x = digitGroupRealFrac separator1 separator2 x

instance DigitGroupShow Double where
  digitGroup separator1 separator2 x = digitGroupRealFrac separator1 separator2 x

digitGroupComma :: DigitGroupShow a => a -> String
digitGroupComma num = digitGroup ',' (Just ' ') num

digitGroupUnderScore :: DigitGroupShow a => a -> String
digitGroupUnderScore num = digitGroup '_' (Just '_') num
