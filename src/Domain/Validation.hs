module Domain.Validation where

import           Data.Maybe                     ( maybeToList )
import           Data.MonoTraversable           ( MonoFoldable(olength) )
import           Data.Text                      ( Text )
import           Text.Regex.TDFA                ( (=~) )

type Validation e a = a -> Maybe e

validate :: (a -> b) -> [Validation e a] -> a -> Either [e] b
validate constructor validations val =
  case concatMap (\f -> maybeToList $ f val) validations of
    []     -> Right $ constructor val
    errors -> Left errors

lengthBetween :: MonoFoldable a => Int -> Int -> e -> Validation e a
lengthBetween minLen maxLen errMsg val =
  rangeBetween minLen maxLen errMsg (olength val)

rangeBetween :: (Ord a) => a -> a -> e -> Validation e a
rangeBetween minRange maxRange errMsg val =
  if val >= minRange && val <= maxRange then Nothing else Just errMsg

regexMatches :: Text -> e -> Validation e Text
regexMatches regex errMsg val = if val =~ regex then Nothing else Just errMsg
