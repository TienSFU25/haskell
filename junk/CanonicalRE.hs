-- Module CanonicalRE defines a canonical representation for regular expressions 
-- that uses a small number of alternative forms (e.g., combining
-- all forms of repetition (Kleene star, Kleene plus, ?, {m,n}) into 
-- a single Rep structure.  

-- Robert D. Cameron, 2013

module CanonicalRE (RE(..), unboundedRep, mkSeq, mkAlt, mkRep, simplify) where

import SparseCharSet

-- RE is the data type for regular expressions

data RE = CC SparseCharClass | Start | End | Seq [RE] | Alt [RE] | Rep (RE, Int, Int)
          deriving Show

--   unbounded repetition is represented by -1
unboundedRep :: Int
unboundedRep = -1


-- In the following comments, CC String is used instead of CC SparseCharClass for
-- illustrative purposes.
--
-- CC "abcd" represents the character class with the 4 characters a, b, c and d, i.e., [a-d].
-- Start represents the ^ metacharacter for start of line or string matching
-- End represents the $ metacharacter for end of line or string matching
-- Seq [CC "abcd", CC "e", CC "f", CC "ghkl"] represents the regexp [a-d]ef[ghkl]
-- Alt [Seq[CC "r", CC "e", CC "d"],  Seq[CC "b", CC "l", CC "u", CC "e"]] represents red|blue
-- Rep (r, lb, ub) represents repetition of at least lb and at most ub occurrences of r
-- Rep (CC "a", 0, 1) represents a?
-- Rep (Seq[CC "a", CC "b", CC "c"], 0, unboundedRep)  represents (abc)*, (without substring capture)
-- Rep (CC "abcedefghijklmnopqrstuvwxyz", 1, unboundedRep) represents [a-z]+
-- Rep (CC "ab", 5, unboundedRep) represents [ab]{5,}
-- Rep (CC "ha", 5, 10) represents [ha]{5,10}

-- Special cases:
-- Seq [] represents the empty regular expression, matching only the empty string.
-- Alt [] represents the empty set, i.e., matching nothing.


-- Optimizing constructors
-- mkSeq - make a sequence, but flatten.  Result might not be a Seq.
-- If there is only one component for a sequence, simply return that.
mkSeq [r] = r
mkSeq rs = Seq (mkSeqList(rs))

-- Build a list for Seq, flattening subsequences
mkSeqList [] = []
mkSeqList ((Seq rs): more) = mkSeqList(rs ++ more)
mkSeqList (r:rs) = r:(mkSeqList(rs))

-- mkAlt make a list of alternatives, but flatten, and combine character classes 
-- If there is only one alternative, simply return that.
mkAlt [r] = r
mkAlt rs = Alt (mkAltList(rs))

-- Build a list for Alt, flattening alternative subgroups,
-- and combining character classes.   We move character
-- classes towards the end of the list to ensure that
-- all combinations are found.
mkAltList [] = []
mkAltList ((Alt rs): more) = mkAltList(rs ++ more)
mkAltList (CC(cs1):CC(cs2):more) = mkAltList(CC(joinCharSets(cs1, cs2)): more)
mkAltList (CC(cs1):a2:more) = mkAltList(a2:CC(cs1):more)
mkAltList (r:rs) = r:(mkAltList(rs))

ubCombine (h1, h2)
  |  h1 == unboundedRep   = unboundedRep
  |  h2 == unboundedRep   = unboundedRep
  |  otherwise            = h1 * h2

mkRep(Seq [], lb, ub) = Seq []
mkRep(Rep(r, lb1, ub1), lb2, ub2)
   | ub1 == unboundedRep && lb2 > 0    = Rep(r, lb1 * lb2, unboundedRep)
   | ub1 == unboundedRep && lb1 <= 1   = Rep(r, lb1 * lb2, unboundedRep)
   | ub1 == unboundedRep && lb2 == 0   = Rep(Rep(r, lb1, unboundedRep), 0, 1)
   | lb2 == ub2                        = mkRep(r, lb1 * lb2, ub1 * ub2)
   | ub1 * lb2 >= lb1 * (lb2 + 1) - 1  = mkRep(r, lb1 * lb2, ubCombine(ub1, ub2))
   | otherwise = Rep(Rep(r, lb1, ub1), lb2, ub2)
mkRep(r, lb, ub) 
   | lb == 0 && ub == 0  = Seq []
   | lb == 1 && ub == 1  = r
   | otherwise           = Rep(r, lb, ub)


-- Recursive simplifier
simplify (Alt as) = mkAlt(map simplify as) 
simplify (Seq as) = mkSeq(map simplify as)
simplify (Rep (r, lb, ub)) = mkRep(simplify(r), lb, ub)
simplify r = r
