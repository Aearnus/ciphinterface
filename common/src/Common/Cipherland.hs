{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE EmptyDataDecls #-}

module Common.Cipherland where

import Data.Function
import Data.Maybe
import Data.Char
import Control.Lens

type Stringrep = Iso' String [Integer]

-- Convert to/from numeric representations
alphabetic :: Stringrep
alphabetic = iso 
  (\s -> (fromIntegral . (`mod` 26) . (\n -> n - fromEnum 'a') . fromEnum <$> s))
  (\c -> toEnum . (+ fromEnum 'a') . (`mod` 26) . fromIntegral <$> c)

alphabetic' :: Stringrep
alphabetic' = iso 
  (\s -> (fromIntegral . (`mod` 26) . (\n -> n - fromEnum 'a') . fromEnum . toLower <$> s))
  (\c -> toEnum . (+ fromEnum 'a') . (`mod` 26) . fromIntegral <$> c)

-- Ciphers
-- First, using ciphers
-- Cipher type parametrized over the key k
type Cipher k = k -> Iso' [Integer] [Integer]

encCipher :: Stringrep -> Cipher k -> k -> String -> String
encCipher strep cipher key = strep %~ view (cipher key)

decCipher :: Stringrep -> Cipher k -> k -> String -> String
decCipher strep cipher key = strep %~ review (cipher key)

-- The ciphers themselves
rot :: Cipher Integer
rot key = (iso `on` fmap) (+ key) (\x -> x - key)

zmodMultInv :: Integer -> Integer -> Maybe Integer
zmodMultInv n group 
  | gcd n group == 1 = Just (head [nmin | nmin <- [1..], n * nmin `mod` group == 1])
  | otherwise = Nothing

-- todo: custom mod value
affine :: Cipher (Integer, Integer)
affine (m, b) = iso (fmap (\x -> m * x + b)) (fmap (\x -> fromJust (zmodMultInv m 26) * (x - b)))