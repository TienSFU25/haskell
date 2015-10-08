-- Module SparseCharSet provides a data structure for
-- representing the characters of a character class as a
-- sorted list of ranges.


module SparseCharSet (SparseCharClass, CharSetItem(..), elemCC, negateClass, insert1, joinCharSets, insertRange) where

import Data.Char
import Data.Bits

type CodePoint = Int
unicodeMax :: CodePoint
unicodeMax = 0x10FFFF

data CharSetItem = CharRange(CodePoint, CodePoint) deriving Show

-- A character class is represented as a list of CharSetItems,
-- kept in sorted order.

type SparseCharClass = [CharSetItem]

-- Determine whether a character code is in a set
elemCC :: (CodePoint, [CharSetItem]) -> Bool
elemCC (c, []) = False
elemCC (c, CharRange(lo, hi):more)
  | c < lo  =  False
  | c > hi  = elemCC(c, more)
  | otherwise = True

-- Insert a codepoint into a class
insert1 :: (CodePoint, SparseCharClass) -> SparseCharClass
insert1 (c, cc) = insertRange(c, c, cc)

-- Insert a range of codepoints into a class
insertRange :: (CodePoint, CodePoint, SparseCharClass) -> SparseCharClass
insertRange(lo, hi, []) = [CharRange(lo, hi)]
insertRange(lo, hi, CharRange(a, b):more)
  | hi < a-1      = CharRange(lo, hi):CharRange(a, b):more
  | lo > b+1      = CharRange(a, b):insertRange(lo, hi, more)
-- Ranges overlap, insert the combined range.  But do it
-- recursively to possibly collapse even further.
  | otherwise     = insertRange(min a lo, max b hi, more)

joinCharSets([], items) = items
joinCharSets(CharRange(lo1, hi1): items1, items2) = insertRange(lo1, hi1, joinCharSets(items1, items2))

negateClass :: SparseCharClass -> SparseCharClass

negateClass c = negateClass_helper(c, 0)

negateClass_helper([], b)
  | b > unicodeMax =  []
  | otherwise           = [CharRange(b, unicodeMax)]

negateClass_helper(CharRange(lo, hi):more, b)
  |  b < lo          = CharRange(b, lo-1): (negateClass_helper(more, hi+1))
  |  otherwise       = negateClass_helper(more, hi+1)


-- Remove a codepoint from a class
remove1 :: (CodePoint, SparseCharClass) -> SparseCharClass
remove1 (c, cc) = removeRange(c, c, cc)

-- Insert a range of codepoints into a class
removeRange :: (CodePoint, CodePoint, SparseCharClass) -> SparseCharClass
removeRange(lo, hi, []) = []
removeRange(lo, hi, CharRange(a, b):more)
  | hi < a-1            = CharRange(a, b):more
  | lo > b+1            = CharRange(a, b):removeRange(lo, hi, more)
-- Ranges overlap
  | lo <= a && hi >=b   = removeRange(lo, hi, more)
  | lo <= a             = CharRange(hi+1, b):more
  | hi >= b             = CharRange(a, lo-1):removeRange(lo, hi, more)
  | otherwise           = CharRange(a, lo-1):CharRange(hi+1,b):more

