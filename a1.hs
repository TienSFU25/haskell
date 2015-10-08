import Text.Regex.Posix
import Data.Maybe
import Data.Char
import System.IO  
import System.Directory


data PabloE = All(Int) | Var(String) | And(PabloE, PabloE) | Or(PabloE, PabloE) | Xor(PabloE, PabloE)
               | Not(PabloE) | Advance(PabloE, Int) | MatchStar(PabloE, PabloE) | End | ErrorE { message :: String }
   deriving (Show, Eq)

data PabloS = Assign(String, PabloE) |  If (PabloE, [PabloS], [PabloS])| While (PabloE, [PabloS]) | EndS | ErrorS { msg :: String }
   deriving (Show)

instance Eq PabloS where
    ErrorS x == ErrorS y = True
    _ == _ = False

data TokenType = TWhile | TIf | TAssign | TColon | TOr | TAnd | TVariable | TInteger | TUnidentified | TTerminator
    | TLeftP | TRightP | TNegate | TAdvance | TMatchStar | TXor | TDot | TElse | TAllZero | TAllOne | TEOF
    deriving (Show, Eq)

data Token = Token { value :: String, typeof :: TokenType} deriving Eq
instance Show Token where
    show (Token{value = s, typeof = t}) = tail(show(t)) ++ "Token:" ++ s

solve :: String -> IO (Maybe[PabloS])
solve fileName = do
    text <- readFile fileName
    return (parseStmts(tokenize(text)))

parseStmts :: [Token] -> Maybe[PabloS]
parseStmts [] = Just []
parseStmts ts = 
    case stmt of
        ErrorS "_" -> Nothing
        _ -> 
            case stmts of
                Nothing -> Nothing
                _ -> Just(stmt:(fromJust stmts))
                where stmts = parseStmts rest
    where (stmt, rest) = parseStmt ts

parseStmt :: [Token] -> (PabloS, [Token])
parseStmt [] =  (EndS, [])
parseStmt (Token {value=v, typeof=TVariable}:Token {typeof=TAssign}:ts) = 
    case parseExpr(ts) of
        (ErrorE { message = m }, rest) -> (ErrorS { msg = m }, rest)
        (expr, rest) -> (Assign(v, expr), rest)
parseStmt (Token{typeof=TIf}:[]) = (ErrorS { msg = "missing if expression " }, [])
parseStmt (Token{typeof=TIf}:rest) =
    case parseExpr(rest) of
        (ErrorE { message = m }, rest2) -> (ErrorS { msg = "Error in parsing if" }, rest2)
        (expr, Token{typeof=TColon}: rest3) ->
            case parseTill(rest3, [TElse, TDot]) of
                (stmts, Token{typeof=TDot}:rest4) -> 
                    case len of
                        0 -> (If(expr, stmts, []), rest4)
                        _ -> (ErrorS { msg = "Invalid statement in if"}, rest4)
                    where len = length $ filter(==ErrorS "_") stmts
                (stmts, Token{typeof=TElse}:rest4) ->
                    let (elseStmts, t:rest5) = parseTill(rest4, [TDot]);
                        len = length $ filter(==ErrorS "_") elseStmts
                    in
                    case len of
                        0 -> (If(expr, stmts, elseStmts), rest5)
                        _ -> (ErrorS { msg = "Invalid statement in else"}, rest5)
                (stmts, rest4) -> (ErrorS { msg = "Missing dot or else in if statement"}, rest4) 
        (expr, rest6) -> (ErrorS { msg = "Missing colon in if statement" }, rest6)
parseStmt (Token{typeof=TWhile}:rest) =
    case parseExpr(rest) of
        (ErrorE { message = m }, rest2) -> (ErrorS { msg = "Error in parsing while" }, rest2)
        (expr, Token{typeof=TColon}: rest3) ->
            case parseTill(rest3, [TDot]) of
                (stmts, Token{typeof=TDot}:rest4) ->
                    case len of
                        0 -> (While(expr, stmts), rest4)
                        _ -> (ErrorS { msg = "Invalid statement in while"}, rest4)
                    where len = length $ filter(==ErrorS "_") stmts
                (stmts, rest5) -> (ErrorS { msg = " Missing dot in while loop "}, rest5)
        (expr, rest6) -> (ErrorS { msg = "Missing colon in while statement" }, rest6)    
parseStmt (_:rest) = (ErrorS {msg = "Undefined statement"}, [])

isIn :: (Token, [TokenType]) -> Bool
isIn (Token{typeof=a}, b) =
    case length $ filter(==a) b of
        0 -> False
        _ -> True

parseTill :: ([Token], [TokenType]) -> ([PabloS], [Token])
parseTill ([], _) = ([], [])
parseTill (first:rest, ts)
    | isIn(first, ts) = ([], first:rest)
    | otherwise = 
    case parseStmt(first:rest) of
        (ErrorS { msg = m }, rest2) -> ([(ErrorS { msg = m })], rest2)
        (stmt, rest2) -> (stmt:stmts, rest3) 
            where (stmts, rest3) = parseTill(rest2, ts)

parseExpr :: [Token] -> (PabloE, [Token])
parseExpr [] =  (End, [])
parseExpr ts =
    case parseTerm(ts) of
        (ErrorE { message = m }, rest) -> (ErrorE { message = "Error in parseExpr: " ++ m }, rest)
        (expr, Token{ typeof = TOr }:[] ) -> (ErrorE { message = "Missing 2nd expr for Or" }, [])
        (expr, Token{ typeof = TXor }: [] ) -> (ErrorE {message = "Missing 2nd expr for Xor" }, [])
        (expr, rest2) -> parseExprWhile(expr, rest2)

parseExprWhile :: (PabloE, [Token]) -> (PabloE, [Token])
parseExprWhile (lExpr, Token{ typeof = TOr }:ts) =
    case parseTerm(ts) of
        (ErrorE { message = m }, rest) -> (ErrorE { message = "Error in parseExprWhile: " ++ m }, rest)
        (rexpr, rest2) -> parseExprWhile(Or(lExpr, rexpr), rest2)

parseExprWhile (lExpr, Token{ typeof = TXor }:ts) =
    case parseTerm(ts) of
        (ErrorE { message = m }, rest) -> (ErrorE { message = "Error in parseExprWhile: " ++ m }, rest)
        (rexpr, rest2) -> parseExprWhile(Xor(lExpr, rexpr), rest2)

parseExprWhile (lExpr, ts) = (lExpr, ts)

parseTerm :: [Token] -> (PabloE, [Token])
parseTerm t =
    case parseFactor(t) of
        (ErrorE { message = m }, rest) -> (ErrorE { message = "Error in parseTerm: " ++ m }, rest)
        (expr, Token { typeof = TAnd }:rest2) ->
            case parseTerm(rest2) of
                (ErrorE { message = m }, rest3) -> (ErrorE { message = "Error in parseTerm: " ++ m }, rest3)
                (expr2, rest4) -> (And(expr, expr2), rest4)
        (expr, rest3) -> (expr, rest3)

parseFactor :: [Token] -> (PabloE, [Token])
parseFactor (Token{typeof=TAllZero}:rest) =  (All(0), rest)
parseFactor (Token{typeof=TAllOne}:rest) =  (All(1), rest)
parseFactor (Token{value=v,typeof=TVariable}:rest) =  (Var(v), rest)
parseFactor (Token{value=v,typeof=TLeftP}:rest) =
    case parseExpr(rest) of
        (ErrorE { message = m }, rest2) -> (ErrorE { message = m }, rest2)
        (expr, Token{typeof=TRightP}:rest2) -> (expr, rest2)
        (expr, first:rest2) -> (ErrorE { message = "Expecting rightp, got " ++ show(first)}, rest2)
parseFactor (Token{typeof=TNegate}:rest) =
    case parseExpr(rest) of
        (ErrorE { message = m }, rest2) -> (ErrorE { message = m }, rest2)
        (expr, rest3) -> (Not(expr), rest3)        
parseFactor (Token{typeof=TAdvance}:rest) =
    case parseExpr(rest) of
        (ErrorE { message = m }, rest2) -> (ErrorE { message = m }, rest2)
        (expr, []) -> (ErrorE { message = "Missing integer for Advance" }, [])
        (expr, Token{typeof=TInteger, value=v}:rest3) -> (Advance(expr, read v :: Int), rest3)
        (expr, rest4) -> (ErrorE { message = "Missing integer for Advance" }, [])
parseFactor (Token{typeof=TMatchStar}:rest) =
    case parseExpr(rest) of
        (ErrorE { message = m }, rest2) -> (ErrorE { message = m }, rest2)
        (expr, []) -> (ErrorE { message = "Missing second expr for MatchStar" }, [])
        (expr, rest4) -> 
            case parseExpr(rest4) of
                (ErrorE { message = m }, rest5) -> (ErrorE { message = "2nd MS expr: " ++ m}, rest)
                (expr2, rest6) -> (MatchStar(expr, expr2), rest6)
parseFactor (t:rest) = (ErrorE { message = "Parse error, unrecognized token " ++ show(t)}, t:rest)

getType :: Token -> TokenType
getType (Token {value=v, typeof=t}) = t

tokenize :: String -> [Token]
tokenize s = tokenizeH(words s)

--turns a giant string into a giant list of tokens
tokenizeH :: [String] -> [Token]
tokenizeH [] = []
tokenizeH (s:xs) = (token s) ++ (tokenizeH xs)

token :: String -> [Token]
token "" = []
token rest@(x:xs)
    | x == '=' = Token {typeof=TAssign, value=""} : token(xs)
    | x == ':' = Token {typeof=TColon, value=""} : token(xs)
    | x == '^' = Token {typeof=TXor, value=""} : token(xs)
    | x == '|' = Token {typeof=TOr, value=""} : token(xs)
    | x == '&' = Token {typeof=TAnd, value=""} : token(xs)
    | x == ';' = Token {typeof=TTerminator, value=""} : token(xs)
    | x == '(' = Token {typeof=TLeftP, value=""} : token(xs)
    | x == ')' = Token {typeof=TRightP, value=""} : token(xs)
    | x == '~' = Token {typeof=TNegate, value=""} : token(xs)
    | x == '.' = Token {typeof=TDot, value=""} : token(xs)
    | otherwise = token2 rest

token2 :: String -> [Token]
token2 x
    | x == "while" = Token {typeof=TWhile, value=""} : []
    | x == "if" = Token {typeof=TIf, value=""} : []
    | x == "else:" = Token {typeof=TElse, value=""} : []
    | x == "End" = Token {typeof=TEOF, value=""} : []
    | x == "Advance" = Token {typeof=TAdvance, value=""} : []
    | x == "MatchStar" = Token {typeof=TMatchStar, value=""} : []
    | x == "000..." = Token {typeof=TAllZero, value=""} : []
    | x == "111..." = Token {typeof=TAllOne, value=""} : []
    | otherwise = case (isAlpha .||. (=='_'))(head x)of
            True -> Token {typeof=TVariable, value=v} : token rest
                where (v, rest) = span (isAlphaNum .||. (=='_')) x
            False -> case isDigit(head x) of
                True -> Token {typeof=TInteger, value=v} : token rest
                    where (v, rest) = span isDigit x
                False -> Token {typeof=TUnidentified, value=""} : []

(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.||.) f g a = (f a) || (g a)

thepablo = "not_ = (~basis7)\nnot_1 = (~basis5)\nnot_2 = (~basis3)\nnot_3 = (~basis2)\nnot_4 = (~basis1)\nnot_5 = (~basis0)\nand_ = (basis6 & not_)\nand_1 = (basis4 & not_1)\nor_ = (basis2 | basis3)\nnot_6 = (~or_)\nor_1 = (basis0 | basis1)\nnot_7 = (~or_1)\nand_2 = (and_ & and_1)\nor_2 = (or_ | or_1)\nnot_8 = (~or_2)\nand_3 = (and_2 & not_8)\nCC_a = and_3\nLF = CC_a\nnot_9 = (~basis6)\nand_4 = (basis7 & not_9)\nand_5 = (basis4 & basis5)\nand_6 = (and_4 & and_5)\nand_7 = (and_6 & not_8)\nCC_d = and_7\nand_8 = (basis4 & not_8)\nxor_ = (basis5 ^ basis6)\nand_9 = (and_8 & xor_)\nCC_a_d = and_9\nif CC_d:\n  cr1 = Advance CC_d 1\n  and_10 = (LF & cr1)\n  crlf = and_10\nelse:\n  crlf = 000...\n.\nand_11 = (basis0 & basis1)\nor_3 = (basis4 | basis5)\nor_4 = (basis6 | basis7)\nBC_c0_ff = and_11\nand_21 = (basis0 & not_4)\nBC_80_bf = and_21\nif BC_c0_ff:\n  and_12 = (not_3 & and_11)\n  or_5 = (basis6 | or_3)\n  or_6 = (basis3 | or_5)\n  and_13 = (and_12 & or_6)\n  BC_c2_df = and_13\n  and_14 = (basis2 & not_2)\n  and_15 = (and_11 & and_14)\n  BC_e0_ef = and_15\n  not_10 = (~basis4)\n  and_16 = (basis2 & basis3)\n  and_17 = (and_11 & and_16)\n  and_18 = (not_10 & and_17)\n  not_11 = (~or_4)\n  not_12 = (~basis5)\n  and_19 = (basis5 & or_4)\n  not_13 = (~and_19)\n  and_20 = (and_18 & not_13)\n  BC_f0_f4 = and_20\n  u8suffix = BC_80_bf\n  if BC_c2_df:\n    advance = Advance BC_c2_df 1\n    u8scope22 = advance\n    or_7 = (basis4 | basis5)\n    not_14 = (~or_7)\n    and_22 = (and_ & not_14)\n    and_23 = (not_6 & and_11)\n    and_24 = (and_22 & and_23)\n    BC_c2 = and_24\n    advance1 = Advance BC_c2 1\n    and_25 = (basis5 & not_10)\n    and_26 = (and_4 & and_25)\n    and_27 = (not_6 & and_21)\n    and_28 = (and_26 & and_27)\n    BC_85 = and_28\n    and_29 = (advance1 & BC_85)\n    NEL = and_29\n  else:\n    u8scope22 = 000...\n    NEL = 000...\n  .\n  if BC_e0_ef:\n    advance2 = Advance BC_e0_ef 1\n    u8scope32 = advance2\n    advance3 = Advance BC_e0_ef 2\n    or_8 = (u8scope32 | advance3)\n    u8scope3X = or_8\n    or_9 = (basis4 | basis5)\n    not_15 = (~or_9)\n    and_30 = (and_ & not_15)\n    and_31 = (and_15 & and_30)\n    BC_e2 = and_31\n    advance4 = Advance BC_e2 1\n    or_10 = (basis6 | basis7)\n    not_16 = (~or_10)\n    or_11 = (or_9 | or_10)\n    not_17 = (~or_11)\n    and_32 = (not_6 & and_21)\n    and_33 = (not_17 & and_32)\n    BC_80 = and_33\n    and_34 = (advance4 & BC_80)\n    advance5 = Advance and_34 1\n    and_35 = (and_1 & not_9)\n    and_36 = (and_14 & and_21)\n    and_37 = (and_35 & and_36)\n    BC_a8_a9 = and_37\n    and_38 = (advance5 & BC_a8_a9)\n    LS_PS = and_38\n    and_39 = (and_15 & not_17)\n    BC_e0 = and_39\n    advance6 = Advance BC_e0 1\n    and_40 = (not_3 & and_21)\n    BC_80_9f = and_40\n    and_41 = (advance6 & BC_80_9f)\n    and_42 = (and_6 & and_15)\n    BC_ed = and_42\n    advance7 = Advance BC_ed 1\n    and_43 = (basis2 & and_21)\n    BC_a0_bf = and_43\n    and_44 = (advance7 & BC_a0_bf)\n    or_12 = (and_41 | and_44)\n    EX_invalid = or_12\n  else:\n    u8scope32 = 000...\n    u8scope3X = 000...\n    LS_PS = 000...\n    EX_invalid = 000...\n  .\n  if BC_f0_f4:\n    u8scope42 = Advance BC_f0_f4 1\n    u8scope43 = Advance u8scope42 1\n    u8scope44 = Advance u8scope43 1\n    or_13 = (u8scope42 | u8scope43)\n    u8scope4nonfinal = or_13\n    or_14 = (u8scope44 | u8scope4nonfinal)\n    u8scope4X = or_14\n    or_15 = (basis6 | basis7)\n    not_18 = (~or_15)\n    or_16 = (basis4 | basis5)\n    not_19 = (~or_16)\n    or_17 = (or_15 | or_16)\n    not_20 = (~or_17)\n    and_45 = (and_17 & not_20)\n    BC_f0 = and_45\n    advance8 = Advance BC_f0 1\n    and_46 = (not_6 & and_21)\n    BC_80_8f = and_46\n    and_47 = (advance8 & BC_80_8f)\n    and_48 = (basis5 & not_10)\n    and_49 = (not_18 & and_48)\n    and_50 = (and_17 & and_49)\n    BC_f4 = and_50\n    advance9 = Advance BC_f4 1\n    or_18 = (basis2 | basis3)\n    and_51 = (and_21 & or_18)\n    BC_90_bf = and_51\n    and_52 = (advance9 & BC_90_bf)\n    or_19 = (and_47 | and_52)\n    FX_invalid = or_19\n  else:\n    u8scope4nonfinal = 000...\n    u8scope4X = 000...\n    FX_invalid = 000...\n  .\n  or_20 = (u8scope3X | u8scope4X)\n  or_21 = (u8scope22 | or_20)\n  or_22 = (BC_c2_df | BC_e0_ef)\n  or_23 = (BC_f0_f4 | or_22)\n  xor_1 = (u8suffix ^ or_21)\n  or_24 = (EX_invalid | FX_invalid)\n  xor_2 = (BC_c0_ff ^ or_23)\n  or_25 = (xor_1 | or_24)\n  or_26 = (xor_2 | or_25)\n  u8invalid = or_26\n  not_21 = (~u8invalid)\n  u8valid = not_21\n  and_53 = (BC_c0_ff & u8valid)\n  valid_pfx = and_53\n  or_27 = (BC_c0_ff | u8scope32)\n  or_28 = (u8scope4nonfinal | or_27)\n  and_54 = (u8valid & or_28)\n  nonfinal = and_54\n  or_29 = (NEL | LS_PS)\n  NEL_LS_PS = or_29\nelse:\n  u8invalid = 000...\n  valid_pfx = 000...\n  nonfinal = 000...\n  NEL_LS_PS = 000...\n.\nor_30 = (CC_a_d | NEL_LS_PS)\nor_31 = (basis2 | basis3)\nCC_0_7f = not_5\nnot_22 = (~u8invalid)\nand_55 = (CC_0_7f & not_22)\ninitial = (valid_pfx | and_55)\nor_32 = (u8invalid | nonfinal)\nfinal = (~or_32)\nnot_23 = (~crlf)\nand_56 = (or_30 & not_23)\nnot_24 = (~and_56)\nand_57 = (and_56 & not_23)\nlf = and_57\nBC_80_bf1 = and_21\nnot_25 = (~basis4)\nor_33 = (basis6 | basis7)\nnot_26 = (~or_33)\nor_34 = (basis4 | basis5)\nnot_27 = (~or_34)\nand_58 = (basis2 & not_2)\nor_35 = (or_33 | or_34)\nnot_28 = (~or_35)\nand_59 = (and_58 & not_7)\nand_60 = (not_28 & and_59)\nCC_20 = and_60\nand_61 = (basis1 & not_5)\nand_62 = (and_4 & not_27)\nand_63 = (and_58 & and_61)\nand_64 = (and_62 & and_63)\nCC_61 = and_64\nand_65 = (and_ & not_27)\nand_66 = (and_63 & and_65)\nCC_62 = and_66\nand_67 = (basis5 & not_25)\nand_68 = (and_63 & and_67)\nnot_29 = (~basis6)\nand_69 = (basis6 & basis7)\nnot_30 = (~and_69)\nand_70 = (and_68 & not_30)\nCC_64_66 = and_70\nm = (CC_20 & not_24)\nand_72 = (CC_61 & not_24)\nand_73 = (CC_62 & not_24)\nand_74 = (CC_64_66 & not_24)\nadv = Advance m 1\ntest = adv\npending = adv\naccum = adv\nwhile test:\n  m1 = (and_72 & pending)\n  adv1 = Advance m1 1\n  m2 = (and_73 & adv1)\n  m3 = (and_74 & pending)\n  alt = (m2 | m3)\n  adv2 = Advance alt 1\n  not_31 = (~accum)\n  and_75 = (adv2 & not_31)\n  pending = and_75\n  or_36 = (accum | adv2)\n  accum = or_36\n  test = pending\n.\nunbounded = accum\nm4 = (m & unbounded)\nmatchstar = MatchStar m4 not_24\nand_76 = (and_56 & matchstar)\nmatches = and_76"

w = "a & b & c | d & e ^ f & g | j & k"
r = " abc = def"
t = tokenize w
k = tokenize ("if " ++ w ++ " :" ++ r ++ " else:" ++ r ++ " .")

ww = "if (stream_7) : x = stream_7 ^ else: x = 000... ."
