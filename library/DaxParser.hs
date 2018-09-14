-- http://book.realworldhaskell.org/read/using-parsec.html
-- https://hackage.haskell.org/package/megaparsec
-- http://www.stephendiehl.com/llvm/

import Text.ParserCombinators.Parsec
import Text.Megaparsec

module DaxParser (main) where

-- http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part2.html
-- 5 Expression Syntax
-- 5.2 Basic Expressions
DaxFormula = do
    char "="
    -- expr <- Expression
    -- return expr
    return Expression

-- todo: return stuff?
Expression = do
    spaces
    ExprType
    spaces
ExprType = do
        Number
    <|> String
    <|> Array
    <|> PrefixedExpr
    <|> PostfixedExpr
    <|> InfixedExpr
    <|> QuotedExpr
    <|> FunctionCall
    <|> Reference
    <|> QuotedLabel
    <|> AutomaticIntersection
    <|> NamedExpression
    <|> Error
    <?> "ExprType"
PrefixedExpr = do
    PrefixOp
    Expression
PostfixedExpr = do
    Expression
    PostfixOp
InfixedExpr = do
    Expression
    InfixOp
    Expression
QuotedExpr = do
    char '('
    Expression
    char ')'
FunctionCall = do
    FunctionName
    spaces
    char '('
    ParameterList
    char ')'
SingleQuoted = do
    char '\''
    content <- many $ string "''" <|> noneOf "'" <?> "any character but '"
    char '\''
    return content

-- 5.3 Constant Numbers
Number = do
    StandardNumber
    <|> do
        char '.'
        some digit
        try do
            E
            try Sign
            some digit
    <?> "Number"
StandardNumber = do
    some digit
    try $ do
        char '.'
        some digit
    try $ do
        E
        try Sign
        some digit
Sign = oneOf "-+"
E = oneOf "eE"

-- 5.4 Constant Strings
String = do
    char '"'
    many $ string "\"\"" <|> noneOf "\"\x0000"
    char '"'

-- 5.5 Operators
PrefixOp = char '+' <|> char '-' <?> "PrefixOp"
PostfixOp = char '%'
InfixOp = ArithmeticOp <|> ComparisonOp <|> StringOp <|> ReferenceOp <?> "InfixOp"
ArithmeticOp = char '+' <|> char '-' <|> char '*' <|> char '/' <|> char '^' <?> "ArithmeticOp"
ComparisonOp = char '=' <|> string "<>" <|> char '<' <|> char '>' <|> string "<=" <|> string ">=" <?> "ComparisonOp"
StringOp = char '&'

ReferenceOp = IntersectionOp <|> ReferenceConcatenationOp <|> RangeOp <?> "ReferenceOp"
IntersectionOp = char '!'
ReferenceConcatenationOp = char '~'
RangeOp = char ':'

-- 5.6 Functions and Function Parameters
FunctionName = do
    LetterXML
    many FunctionNameChar
FunctionNameChar = do
    LetterXML
    <|> DigitXML
    <|> oneOf "_."
    <|> CombiningCharXML
     <?> "FunctionNameChar"

-- ParameterList = do
--     empty
--     <|> do
--         Parameter
--         many do
--             Separator
--             EmptyOrParameter
--     <|> do -- First param empty
--         Separator
--         EmptyOrParameter
--     many do
--         Separator
--         EmptyOrParameter
ParameterList = sepBy EmptyOrParameter Separator
EmptyOrParameter = Parameter <|> spaces <?> "EmptyOrParameter"
Parameter = Expression
Separator = char ';'

-- 5.8 References
Reference = do
    char '['
    ReferenceError <|> do
        try Source
        RangeAddress
    <?> "Reference"
    ']'
RangeAddress = do
        do
            SheetLocatorOrEmpty
            char '.'
            Column
            Row
            try do
                char ':'
                char '.'
                Column
                Row
    <|> do
        SheetLocatorOrEmpty
        char '.'
        Column
        char ':'
        char '.'
        Column
    <|> do
        SheetLocatorOrEmpty
        char '.'
        Row
        char ':'
        char '.'
        Row
    <|> do
        SheetLocator
        char '.'
        Column
        Row
        char ':'
        SheetLocator
        char '.'
        Column
        Row
    <|> do
        SheetLocator
        char '.'
        Column
        char ':'
        SheetLocator
        char '.'
        Column
    <|> do
        SheetLocator
        char '.'
        Row
        char ':'
        SheetLocator
        char '.'
        Row
     <?> "ReferenceAddress"
SheetLocatorOrEmpty = try SheetLocator
SheetLocator = do
    SheetName
    many do
        char '.'
        SubtableCell
SheetName = QuotedSheetName <|> do
    try char '$'
    some noneOf "]. #$'"
    <?> "SheetName"
QuotedSheetName = do
    try char '$'
    SingleQuoted
SubtableCell = QuotedSheetName <|> do
    Column
    Row
    <?> "SubtableCell"
ReferenceError = string "#REF!"
Column = do
    try char '$'
    some upper
Row = do
    try char '$'
    oneOf ['1'..'9']
    many digit
Source = do
    char '\''
    IRI
    char '\''
    char '#'
CellAddress = do
    SheetLocatorOrEmpty
    char '.'
    Column
    Row -- Not used directly

-- 5.9 Reference List
ReferenceList = sepBy Reference do
    spaces
    ReferenceConcatenationOp
    spaces

-- 5.10 Quoted Label
QuotedLabel = SingleQuoted

AutomaticIntersection = do
    QuotedLabel
    spaces
    string "!!"
    spaces
    QuotedLabel

-- 5.11 Named Expressions
NamedExpression =
        SimpleNamedExpression
    <|> SheetLocalNamedExpression
    <|> ExternalNamedExpression
    <?> "NamedExpression"
SimpleNamedExpression =
        Identifier
    <|> do
        string "$$"
        Identifier <|> SingleQuoted
    <?> "SimpleNamedExpression"
SheetLocalNamedExpression = do
    QuotedSheetName
    char '.'
    SimpleNamedExpression
ExternalNamedExpression = do
    Source
    SimpleNamedExpression <|> SheetLocalNamedExpression <?> "ExternalNamedExpression"

Identifier = do
    do
        LetterXML
        many $ LetterXML <|> DigitXML <|> char '_' <|> CombiningCharXML <?> "IdentifierLetter"
    <|> do
        some letter
        some digit
    <|> do
        oneOf "Tt"
        oneOf "Rr"
        oneOf "Uu"
        oneOf "Ee"
    <|> do
        oneOf "Ff"
        oneOf "Aa"
        oneOf "Ll"
        oneOf "Ss"
        oneOf "Ee"
    <?> "Identifier"

-- 5.12 Constant Errors
Error = do
    char '#'
    some alphaNum
    oneOf "!?" <|> do
        char '/'
        upper <|> do
            digit
            oneOf "!?"
        <?> "ErrorCharacter"
    <?> "ErrorMark"

-- 5.13 Inline Arrays
Array = do
    char '{'
    sepBy MatrixRow $ char '|'
    char '}'
MatrixRow = sepBy Expression $ char ';'

-- csvFile = endBy line eol
-- line = sepBy cell (char ',')
-- cell = quotedCell <|> many (noneOf ",\n\r")

-- quotedCell = 
--     do char '"'
--        content <- many quotedChar
--        char '"' <?> "quote at end of cell"
--        return content

-- quotedChar =
--         noneOf "\""
--     <|> try (string "\"\"" >> return '"')

-- eol =   try (string "\n\r")
--     <|> try (string "\r\n")
--     <|> string "\n"
--     <|> string "\r"
--     <?> "end of line"

-- parseCSV :: String -> Either ParseError [[String]]
-- parseCSV input = parse csvFile "(unknown)" input

parseDAX :: String -> Either ParseError [[String]]
parseDAX input = parse DaxFormula "(unknown)" input

main :: IO ()
-- main = return ()
main =
    do c <- getContents
       case parse csvFile "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right r -> mapM_ print r
