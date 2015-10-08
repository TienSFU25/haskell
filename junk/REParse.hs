-- Module REparse provides parsers for parsing regular expressions
-- in different representations, converting to the CanonicalRE 
-- representation.

-- Robert D. Cameron, 2014

module REparse (parseRE, ParseResult(..)) where
        
import Data.Char
import SparseCharSet
import CanonicalRE

data ParseResult = ParseSuccess RE | ParseFailure String deriving Show

parseRE :: String -> ParseResult
parseRE_helper :: String -> (ParseResult, String)

-- parsing is accomplished using a recursive-descent style
-- parsing functions, which return a partial result and a
-- remaining input string.  At the top-level, the entire
-- input must be consumed or the parse fails.

parseRE(s) =
  case parseRE_helper(s) of
    (result, "")         -> result
    (ParseSuccess _, remaining) -> ParseFailure "Junk remaining"  
    (failresult, remain) -> failresult 
--
-- A Simple Grammar of Regular Expressions
--
-- A regular expression can consist of one or more alternative
-- forms, separated by vertical bars ("|").
-- <RE> ::= <RE_form> { "|" <RE_form> }
--
-- Each alternative form is a concatenation of one or more items.
-- <RE_form> ::= <RE_item> {<RE_items>}
--
-- Each item is basic unit, or a repeated item.
-- <RE_item> ::= <RE_unit> | <RE_item> <repeat_specification>
-- <repeat_specification> ::= '?' | '*' | '+' | '{' <int> ["," [<int>]} '}'
-- <int> ::= <digit> {<digit>}
--
-- Each unit is a parenthesized RE, a character class item or
-- a start-of-line or end-of-line assertion.
-- <RE_unit> ::= '(' <RE> ')' | <CC> | '^' | '$'
--
-- A character class unit is either an ordinary character, the "any"
-- character class represented by '.', an escaped character class 
-- or a bracketted character class.
-- <CC> ::= <non-metacharacter> | '.'  | <escaped_CC> | <bracketted_CC>
-- 
-- An escaped character class is an escaped metacharacter or
-- a named escape class such as \s for whitespace, \w for word...
-- <escaped_CC> ::= '\' <metacharacter> | '\' <letter>
--
-- A bracketted character class is either a positive or negative
-- opener followed by a character class body.
--
-- <bracketted_CC> ::= '[' <CC_body> ']' | '[^' <CC_body> ']'
-- The character class body is a list of individual characters
-- or ranges specified with hyphens.  "]" and "-" may not
-- be specified as body characters except that either may 
-- occur as the first character of a class and "-" may occur
-- as the final character.
-- <CC_body> ::= <any_char> ['-' <body_char>] { <body_char> ['-' <body_char>]} ['-']
--
-- 

parseRE_helper(s) =
  case parseRE_alt_form_list(s) of
    ([], _)            -> (ParseFailure "No regular expression found", s)
    ([a], remaining)   -> (ParseSuccess a, remaining) -- a single alternative, just return it
    (forms, remaining) -> (ParseSuccess (Alt forms), remaining)

parseRE_alt_form_list :: String -> ([RE], String)
parseRE_alt_form_list(s) =
  case parseRE_form(s) of
    (ParseSuccess form1, '|' : more) -> let (more_results, after_more) = parseRE_alt_form_list(more)
                                        in (form1:more_results, after_more)
    (ParseSuccess form1, remaining) -> ([form1], remaining)
    _ -> ([], s)
              
parseRE_form :: String -> (ParseResult, String)
parseRE_form(s) =
   case parseRE_item_list(s) of
     ([], _) -> (ParseFailure "No regular expression found", s) 
     ([item], remaining) -> (ParseSuccess item, remaining)
     (items, remaining) -> (ParseSuccess (Seq items), remaining)


parseRE_item_list :: String -> ([RE], String)
parseRE_item_list(s) =
  case parseRE_item(s) of
    (ParseSuccess form1, more) -> let (more_results, after_more) = parseRE_item_list(more)
                                        in (form1:more_results, after_more)
    _ -> ([], s)

parseRE_item :: String -> (ParseResult, String)
parseRE_item(s) =
  case parseRE_unit(s) of
    (ParseSuccess form1, remaining) -> extend_item(form1, remaining)
    failure -> failure

extend_item :: (RE, String) -> (ParseResult, String)
extend_item(r, '*':more) = extend_item(Rep(r, 0, unboundedRep), more)
extend_item(r, '?':more) = extend_item(Rep(r, 0, 1), more)
extend_item(r, '+':more) = extend_item(Rep(r, 1, unboundedRep), more)
extend_item(r, '{':more) = 
  case parseInt(more) of
    (Just i, '}' : even_more) -> extend_item(Rep(r, i, i), even_more)
    (Just i, ',':'}': even_more) -> extend_item(Rep(r, i, unboundedRep), even_more)
    (Just i, ',': even_more) -> 
      case parseInt(even_more) of
        (Just j, '}' : remaining) -> extend_item(Rep(r, i, j), remaining)
        _ -> (ParseFailure "Bad upper bound", even_more)
    _ -> (ParseFailure "Bad lower bound", more)
-- default if we cannot extend
extend_item(r, remaining) = (ParseSuccess r, remaining)


parseRE_unit :: String -> (ParseResult, String)
parseRE_unit([]) = (ParseFailure "Incomplete regular expression", "")
parseRE_unit ('(':more) = 
  case parseRE_helper(more) of
    (ParseSuccess r, ')': remaining) -> (ParseSuccess r, remaining)
    _ -> (ParseFailure "Bad parenthesized RE", more)
parseRE_unit('^':more) = (ParseSuccess Start, more)
parseRE_unit('$':more) = (ParseSuccess End, more)
-- Now look for a single character or character class
parseRE_unit(s) = parseCC(s)

-- Make a character class from a single character
cc1 c = let v = ord c in CC [CharRange(v, v)]

-- parseCC deals with individual characters (unitary character classes)
-- and all other forms specifying classes of characters.
parseCC('.':more) = (ParseSuccess (CC [CharRange(0,9), CharRange(11,127)]), more)
--
-- Any of the RE metacharacters may be represented using a backslash escape.
-- 
parseCC('\\':'?':more) = (ParseSuccess (cc1('?')), more)
parseCC('\\':'+':more) = (ParseSuccess (cc1('+')), more)
parseCC('\\':'*':more) = (ParseSuccess (cc1('*')), more)
parseCC('\\':'(':more) = (ParseSuccess (cc1('(')), more)
parseCC('\\':')':more) = (ParseSuccess (cc1(')')), more)
parseCC('\\':'{':more) = (ParseSuccess (cc1('{')), more)
parseCC('\\':'}':more) = (ParseSuccess (cc1('}')), more)
parseCC('\\':'[':more) = (ParseSuccess (cc1('[')), more)
parseCC('\\':']':more) = (ParseSuccess (cc1(']')), more)
parseCC('\\':'|':more) = (ParseSuccess (cc1('|')), more)
parseCC('\\':'.':more) = (ParseSuccess (cc1('.')), more)
parseCC('\\':'\\':more) = (ParseSuccess (cc1('\\')), more)
--
-- Any other use of backslash is an error.
-- 
parseCC('\\':more) = (ParseFailure "Illegal backslash escape", more)
--

parseCC('[':'^':more) = negateCharClassResult(parseCC_body(more))
parseCC('[':more) = parseCC_body(more)
--
-- Now just have a single character, but it cannot be a metacharacter.
parseCC(c:more)
   | elem c "?+*(){}[]|"   = (ParseFailure "Metacharacter alone", c:more)
   | otherwise             = (ParseSuccess (cc1 c), more)


-- 
-- To parse the body of a character class, we use helpers 
-- parseCC_body1(c, s) 
parseCC_body :: String -> (ParseResult, String)
parseCC_body1 :: (Char, String, SparseCharClass) -> (ParseResult, String)
parseCC_body0 :: (String, SparseCharClass) -> (ParseResult, String)

parseCC_body([]) = (ParseFailure "Unclosed character class", [])
parseCC_body(a:more) = parseCC_body1(a, more, [])



parseCC_body1(a, [], ccSoFar) = (ParseFailure "Unclosed character class", [])
parseCC_body1(a, ']':more, ccSoFar) = (ParseSuccess (CC (insert1(ord(a), ccSoFar))), more)
parseCC_body1(a, [_], ccSoFar) = (ParseFailure "Unclosed character class", [])
parseCC_body1(a, '-':']':more, ccSoFar) = (ParseSuccess (CC (insert1(ord(a), insert1(ord('-'), ccSoFar)))), more)
parseCC_body1(a, '-':b:more, ccSoFar) = parseCC_body0(more, insertRange(ord(a), ord(b), ccSoFar))
parseCC_body1(a, b:more, ccSoFar) = parseCC_body1(b, more, insert1(ord(a), ccSoFar))

parseCC_body0([], ccSoFar) = (ParseFailure "Unclosed character class", [])
parseCC_body0(']':more, ccSoFar) = (ParseSuccess (CC ccSoFar), more)
parseCC_body0('-':']':more, ccSoFar) = (ParseSuccess (CC (insert1(ord('-'), ccSoFar))), more)
parseCC_body0('-':more, ccSoFar) = (ParseFailure "Bad range in character class", more)
parseCC_body0(a:more, ccSoFar) = parseCC_body1(a, more, ccSoFar)


negateCharClassResult(ParseSuccess (CC s), remaining) = 
-- should be the following, but there are issues
--   (ParseSuccess (CC (negateClass s)), remaining)
-- We use the following hack for ASCII for now
   (ParseSuccess (CC (negateClass (insert1(10, insertRange(128, 0x10FFFF, s))))), remaining) 
negateCharClassResult(failureResult, remaining) = (failureResult, remaining)



parseInt1 :: (String, Int) -> (Int, String)
parseInt1([],numSoFar) = (numSoFar, [])
parseInt1(d:more,numSoFar) 
  | isDigit(d)    = parseInt1(more, numSoFar * 10 + (ord(d) - 48))
  | otherwise     = (numSoFar, d:more)

parseInt :: String -> (Maybe Int, String)
parseInt([]) = (Nothing, [])
parseInt(d:more)
  | isDigit(d)    = let (i, remaining) = parseInt1(more, ord(d) - 48) in (Just i, remaining)
  | otherwise     = (Nothing, d:more)
  
