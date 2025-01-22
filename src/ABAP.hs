{-# LANGUAGE OverloadedStrings #-}

module ABAP where

import           Data.Char(isAlpha, isAlphaNum)
import qualified Data.Text as T
import           Data.Text(Text)
import Text.Megaparsec
import Text.Megaparsec.Char ( char, space, hspace, string', newline, digitChar )
-- import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void Text

data ABAP_AST = ABAP_AST [DeclStatementClass]

data DeclStatementClass = Dec Declaration
                        | St Statement
                        | Intf Interfaces
                        | Comments [Comment] deriving Show

data Declaration = DecMethod Method
                 | DecClass ClassDefinition
                 | ImpClass ClassImplementation
                 | DATA [DATADeclaration]
                 | TYPES Ty
                 | CONSTANTS [Constants]
                 | CLASSDATA ClassDataDeclaration deriving Show

data Statement = Assignment Assign
               | IfBranch Expression [ELSEIF_ELSE]
               | Cas Scrutinee [When] [DeclStatementClass]
               | TableSt TableStatement
               | LoopSt LoopStatement deriving Show

data Interfaces = Interfaces Text deriving Show

data Method = Method Text [DeclStatementClass] deriving Show

data ClassDefinition = ClassDefinition Text Init [Section] deriving Show

data ClassImplementation = ClassImplementation Text [DeclStatementClass] deriving Show

data DATADeclaration = DATAType Text ABAP_Type (Maybe Text) (Maybe Text)
                     | DATALike Text Reference deriving Show

data Ty = Ty deriving Show
data Constants = Constants deriving Show
data ClassDataDeclaration = ClassDataDeclaration deriving Show

data Assign = Assign ComplexAccess AssingOperator Expression deriving Show
data Expression = BinExpr Expression Expression
                | Num Text
                | New Expression deriving Show
type Scrutinee = Text
data TableStatement = Append Row Table
                    | ReadTable Table Text Index deriving Show
data LoopStatement = LoopAt Table Var deriving Show

data Comment = Comment String | NoComment deriving Show

data Init = Init deriving Show
data Section = Section Visibility [DeclStatementClass] deriving Show
data Visibility = PUBLIC | PROTECTED | PRIVATE deriving Show

data When = When [Scrutinee] [DeclStatementClass] deriving Show
data ELSEIF_ELSE = ELSEIF Expression | ELSE Expression deriving Show
-- data Whens = WHEN Expression | WHENOTHERS Expression deriving Show
type Table = Text
type Var = Text
type Index = Int
type Row = Text
data Reference = Text deriving Show
data TYPESStatement = TYPESStatement
data Try = Try [DeclStatementClass] Text [DeclStatementClass]
data ABAP_Type = Table Text | SimpleType ABAP_SimpleType |
                 LikeLineOf Text | TableOf TableType Text Text deriving Show
data ABAP_SimpleType = TI TypeI | TC TypeC | TP TypeP | TS TypeString | TD TypeD | TT TypeT deriving Show
data TypeI = TypeI Int deriving Show
data TypeC = TypeC Text deriving Show
data TypeP = TypeP Int Int deriving Show
data TypeString = TypeString Text deriving Show
data TypeD = TypeD Text deriving Show
data TypeT = TypeT Text deriving Show
data StringTemplate = StringTemplate Text deriving Show

data Clear = Clear deriving Show
data Exit = Exit deriving Show
data IsInitial = IsInitial Bool deriving Show

data StringOrExpression = Str StringExpr deriving Show
data StringExpr = StringExpr Expression FormatOption deriving Show
data FormatOption = FormatOption FormatOptionVar FormatOptionAssign deriving Show
data FormatOptionVar = Date | Number | Sign | Style deriving Show
type FormatOptionAssign = Text
data PVariable = PVariable Decimals deriving Show
type Decimals = Int
data Else = ElseIf ControlStructure | Else Statement [Else] deriving Show
data ControlStructure = IF Expression Statement [Else] deriving Show
data BooleanExpression = AND | OR | Not | GT | LT | GE | LE | BetweenAnd deriving Show

data AndAnd = AndAnd
data Between = Between Text Text

data CONSTANTSStatement = CONSTANTSStatement
data DoTimes = DoTimes Int [DeclStatementClass]
data DoLoop = DoLoop [DeclStatementClass]
data TypeRefTo = TypeRefTo Text
data Fields = Fields Text Text
data Methods = Methods [DATADeclaration] [DATADeclaration] [DATADeclaration] [DATADeclaration] [Exception]
data Exception = Exception
data Importing = Importing
data Exporting = Exporting
data Changing = Changing
data Raising = Raising
data Returning = Returning

data Rust_AST = Rust_AST deriving Show

class ShowIndent a where showIndent :: Int -> a -> Text

repl i = T.replicate i " "

instance ShowIndent ABAP_AST where
  showIndent i (ABAP_AST topLevels) = repl i `T.append` T.intercalate "\n" (map (showIndent i) topLevels)

instance ShowIndent ClassDefinition where
  showIndent i (ClassDefinition def itf sections) =
    repl i `T.append` "CLASS " `T.append` def `T.append` " DEFINITION\n" `T.append`
    showIndent (i+2) itf `T.append` T.intercalate "\n" (map (showIndent (i+2)) sections) `T.append`
    repl i `T.append` "\nENDCLASS."

instance ShowIndent Section where
  showIndent i (Section vis decStCls) =
    repl i `T.append` T.pack (show vis) `T.append` " SECTION.\n" `T.append` T.intercalate "\n" (map (showIndent (i+2)) decStCls)

instance ShowIndent Interfaces where
  showIndent i (Interfaces t) =
    repl i `T.append` "INTERFACES " `T.append` t `T.append` "."

instance ShowIndent DeclStatementClass
  where showIndent i (Dec dec)         = repl i `T.append` showIndent (i+2) dec
        showIndent i (St statement)    = repl i `T.append` showIndent (i+2) statement
        showIndent i (Intf interfaces) = repl i `T.append` showIndent (i+2) interfaces
        showIndent i (Comments cs)     = repl i `T.append` showIndent (i+2) (head cs)

instance ShowIndent Comment
  where showIndent i (Comment c)     = repl i `T.append` (T.pack c)

instance ShowIndent Init
  where showIndent i Init = repl i `T.append` "PUBLIC\n" `T.append`
                            repl i `T.append` "FINAL\n" `T.append`
                            repl i `T.append` "CREATE PUBLIC.\n"

instance ShowIndent Declaration
  where showIndent i (DecMethod method) = repl i `T.append` ""
        showIndent i (DecClass classDefinition) = repl i `T.append` ""
        showIndent i (ImpClass classImplementation) = repl i `T.append` ""
        showIndent i (DATA dataStatement) = repl i `T.append` ""
        showIndent i (TYPES types) = repl i `T.append` ""
        showIndent i (CONSTANTS constants) = repl i `T.append` ""
        showIndent i (CLASSDATA classDataStatement) = repl i `T.append` ""

instance ShowIndent Statement
  where showIndent i (Assignment expression) = repl i `T.append` ""
        showIndent i (IfBranch expression elseifs) = repl i `T.append` ""
--        showIndent i (CaseBranch expression whens) = repl i `T.append` ""
        showIndent i (TableSt tableStatement) = repl i `T.append` ""
        showIndent i (LoopSt loopStatement) = repl i `T.append` ""

instance ShowIndent ClassImplementation
  where showIndent i (ClassImplementation t sections) = repl i `T.append` ""

--------------------------------------------------------------------------------

abapModernSyntax :: String -> Text -> Either (ParseErrorBundle Text Void) Text
abapModernSyntax file t = prettyPrintABAP (convertToABAP_AST file t)

convertToABAP_AST :: String -> Text -> Either (ParseErrorBundle Text Void) ABAP_AST
convertToABAP_AST file t = p
       where p :: Either (ParseErrorBundle Text Void) ABAP_AST
             p = parse parseABAP file t

prettyPrintABAP :: Either (ParseErrorBundle Text Void) ABAP_AST -> Either (ParseErrorBundle Text Void) Text
prettyPrintABAP ast = fmap (showIndent 0) ast

convertToRust :: String -> Text -> Either (ParseErrorBundle Text Void) Text
convertToRust file = prettyPrintRust . abapToRust . convertToABAP_AST file

abapToRust :: Either (ParseErrorBundle Text Void) ABAP_AST -> Either (ParseErrorBundle Text Void) Rust_AST
abapToRust t = fmap (const Rust_AST) t

prettyPrintRust :: Either (ParseErrorBundle Text Void) Rust_AST -> Either (ParseErrorBundle Text Void) Text
prettyPrintRust t = fmap (T.pack . show) t

---------------------------------------------------------------

parseABAP :: Parser ABAP_AST
parseABAP = do as <- parseClassDefinition -- some (choice [ -- parseStatement, parseDeclaration, 
--                                    parseClassDefinition ])
--                                  , parseClassImplementation ])
               bs <- parseClassImplementation
               return (ABAP_AST [as,bs])

parseData :: Parser DeclStatementClass
parseData =
  do _ <-  space
     _ <- string' "DATA"
     _ <-  space
     var <- parseName
     _ <-  space
     _ <- string' "TYPE"
     ty <- choice [parseTypeP, parseTableOf, parseStandardTableOf, parseSortedTableOf, parseHashedTableOf, parseLikeLineOf]
     _ <-  space
-- optional
     _ <- string' "VALUE"
     val <- parseName
     _ <-  space

-- optional
     _ <- string' "IS INITIAL"
     initial <- parseName
     _ <-  space

     _ <- string' "."
     return (Dec (DATA [DATAType var ty (Just val) (Just initial)]))

parseLikeLineOf :: Parser ABAP_Type
parseLikeLineOf =
  do _ <-  space
     _ <- string' "LIKE"
     _ <-  space
     _ <- string' "LINE"
     _ <-  space
     _ <- string' "OF"
     var <- parseName
     return (LikeLineOf var)

parseDatas :: Parser DeclStatementClass
parseDatas =
  do _ <-  space
     _ <- string' "DATA:"
     _ <-  space
     var <- parseName
     _ <-  space
     _ <- string' "TYPE"
     ty <- choice [parseTypeP]
     _ <-  space
-- optional
     _ <- string' "VALUE"
     ty <- parseName
     _ <-  space

-- optional
     _ <- string' "IS INITIAL"
     ty <- parseName
     _ <-  space

     _ <- string' "."
     return (Dec (DATA [DATAType "name" (SimpleType (TI (TypeI 0))) Nothing Nothing]))

parseInlineData :: Parser DeclStatementClass
parseInlineData =
  do _ <-  space
     _ <- string' "DATA()"
     _ <-  space
     var <- parseName
     _ <-  space
     _ <- string' ")"
     _ <-  space
     _ <- string' "="
     _ <-  space
     expr <- parseExpression
     _ <-  space
     _ <- string' "."
     return (Dec (DATA [DATAType "name" (SimpleType (TI (TypeI 0))) Nothing Nothing]))

parseConstants :: Parser DeclStatementClass
parseConstants =
  do _ <-  space
     _ <- string' "CONSTANTS"
     _ <-  space
     var <- parseName
     _ <-  space
     _ <- string' "TYPE"
     ty <- choice [parseTypeP]
     _ <-  space
-- optional
     _ <- string' "VALUE"
     ty <- parseName
     _ <-  space

-- optional
     _ <- string' "IS INITIAL"
     ty <- parseName
     _ <-  space

     _ <- string' "."
     return (Dec (CONSTANTS [Constants]))


parseTableOf :: Parser ABAP_Type
parseTableOf =
  do return (Table "1")

data TableType = Standard | Sorted | Hashed deriving Show

parseStandardTableOf :: Parser ABAP_Type
parseStandardTableOf =
  do _ <- string' "STANDARD"
     _ <-  space
     _ <- string' "TABLE"
     _ <-  space
     _ <- string' "OF"
     _ <-  space
     name <- parseName
     key <- parseKey
     return (TableOf Standard name key)

parseSortedTableOf :: Parser ABAP_Type
parseSortedTableOf =
  do _ <- string' "SORTED"
     _ <-  space
     _ <- string' "TABLE"
     _ <-  space
     _ <- string' "OF"
     _ <-  space
     name <- parseName
     key <- parseKey
     return (TableOf Sorted name key)

parseHashedTableOf :: Parser ABAP_Type
parseHashedTableOf =
  do _ <- string' "HASHED"
     _ <-  space
     _ <- string' "TABLE"
     _ <-  space
     _ <- string' "OF"
     _ <-  space
     name <- parseName
     key <- parseKey
     return (TableOf Hashed name key)

parseKey :: Parser Text
parseKey =
  do _ <- string' "WITH"
     _ <-  space
     un_ <- choice [parseUnique, parseNonUnique]
     _ <-  space

     -- optional
     _ <- string' "DEFAULT"
     _ <-  space

     _ <- string' "KEY"
     _ <-  space

     --  secondary key optional
     _ <- string' "COMPONENTS"
     _ <-  space
     return ""

data Unique = Unique | NonUnique

parseUnique :: Parser Unique
parseUnique =
  do _ <- string' "UNIQUE"
     return Unique

parseNonUnique :: Parser Unique
parseNonUnique =
  do _ <- string' "NON-UNIQUE"
     return NonUnique

data ConvertUtcLong = ConvertUtcLong Text Text Text Text

parseConvertUtcLong :: Parser ConvertUtcLong
parseConvertUtcLong =
  do _ <- string' "CONVERT"
     _ <- string' "UTCLONG"
     utclong <- parseName
     _ <- string' "TIME"
     _ <- string' "ZONE"
     timezone <- parseName
     _ <- string' "INTO"
     _ <- string' "DATE"
     date <- parseName
     _ <- string' "TIME"
     time <- parseName
     return (ConvertUtcLong utclong timezone date time)

{-
CONVERT DATE today
TIME connection-departure_time
TIME ZONE airports[ airport_id = connection-
airport_from_id ]-timzone
INTO UTCLONG DATA(departure_utclong).
-}

data ConvertDate = ConvertDate Text Text Text Text

parseConvertDate :: Parser ConvertDate
parseConvertDate =
  do _ <- string' "CONVERT"
     _ <- string' "DATE"
     date <- parseName
     _ <- string' "TIME"
     time <- parseName
     _ <- string' "TIME"
     _ <- string' "ZONE"
     timezone <- parseName
     _ <- string' "INTO"
     _ <- string' "UTCLONG"
     utclong <- parseName
     return (ConvertDate date time timezone utclong)

data Literal = LiteralNumber | LiteralText | LiteralString

-- -123, 123  type i, type p
parseLiteralNumber :: Parser Literal
parseLiteralNumber =
  do return LiteralNumber

-- 'Hallo' type c, type p
parseLiteralText :: Parser Literal
parseLiteralText =
  do return LiteralText

-- `Hallo` type STRING
parseLiteralString :: Parser Literal
parseLiteralString =
  do return LiteralString

parseAssignment :: Parser DeclStatementClass
parseAssignment =
  do variable <- parseStructureAccess
     op <- choice [parseEqual, parsePlusEqual, parseMinusEqual, parseMulEqual, parseDivEqual]
     expression <- parseExpression
     return (St (Assignment (Assign variable op expression)))

parseBlankSeparatedAssignments :: Parser [DeclStatementClass]
parseBlankSeparatedAssignments =
  do assign <- many parseAssignment
     return assign

data AssingOperator = Equal | PlusEqual | MinusEqual | MulEqual | DivEqual deriving Show

parseEqual :: Parser AssingOperator
parseEqual =
  do _ <- string' "="
     return Equal

parsePlusEqual :: Parser AssingOperator
parsePlusEqual =
  do _ <- string' "+="
     return PlusEqual

parseMinusEqual :: Parser AssingOperator
parseMinusEqual =
  do _ <- string' "-="
     return MinusEqual

parseMulEqual :: Parser AssingOperator
parseMulEqual =
  do _ <- string' "*="
     return MulEqual

parseDivEqual :: Parser AssingOperator
parseDivEqual =
  do _ <- string' "/="
     return DivEqual

data AccessOperator = Mi | Tilde | Arrow | DoubleArrow
data ComplexAccess = ComplexAccess Text deriving Show -- AccessOperator

parseStructureAccess :: Parser ComplexAccess
parseStructureAccess =
  do name <- parseName
     op <- choice [string' "-", string' "~", string' "->", string' "=>"]
     return (ComplexAccess name)

isAlphaNum2 x = isAlphaNum x || x == '_' || x == '~'

data Value = VALUE [DeclStatementClass] | CORRESPONDING [DeclStatementClass] | REDUCE DeclStatementClass Text Text DeclStatementClass

-- VALUE #(  )
parseVALUE :: Parser Value
parseVALUE =
  do _ <- space
     _ <- string' "VALUE"
     _ <- space
     _ <- string' "#"
     _ <- space
     _ <- string' "("
     _ <-  space
     args <- parseBlankSeparatedAssignments
     _ <-  space
     _ <- string' ")"
     _ <- space
     _ <- string' "."
     return (VALUE args)

-- CORRESPONDING #(  )
parseCORRESPONDING :: Parser Value
parseCORRESPONDING =
  do _ <-  space
     _ <- string' "CORRESPONDING"
     _ <-  space
     _ <- string' "#"
     _ <-  space
     _ <- string' "("
     _ <-  space
     args <- parseBlankSeparatedAssignments
     _ <- string' ")"
     _ <-  space
     _ <- string' "."
     return (CORRESPONDING args)


-- REDUCE #(  )
parseREDUCE :: Parser Value
parseREDUCE =
  do _ <-  space
     _ <- string' "REDUCE"
     _ <-  space
     _ <- string' "#"
     _ <-  space
     _ <- string' "("
     _ <-  space
     _ <- string' "INIT"
     _ <-  space
     assign <- parseAssignment
     _ <- space
     _ <- string' "FOR"
     _ <- space
     for <- parseName
     _ <- space
     _ <- string' "IN"
     _ <- space
     in_ <- parseName
     _ <- space
     _ <- string' "NEXT"
     _ <- space
     next <- parseAssignment
     _ <- space
     _ <- string' ")"
     _ <-  space
     _ <- string' "."
     return (REDUCE assign for in_ next)

--   "+" "-" "'" "/" "DIV" "MOD" "sqrt" "ipow" "table []" "table [ key k_from]"
parseExpression :: Parser Expression
parseExpression = 
  do _ <-  space
     expr0 <- parseExpression
     _ <- space
     _ <- string' "+"
     _ <-  space
     expr1 <- parseExpression
     return (BinExpr expr0 expr1)

data InstanceOf = InstanceOf

parseIsInstanceOf :: Parser InstanceOf
parseIsInstanceOf = 
  do _ <-  space
     _ <- string' "IS"
     _ <-  space
     _ <- string' "INSTANCE"
     _ <-  space
     _ <- string' "OF"
     _ <-  space
     return (InstanceOf)

data Cast = Cast

parseCast :: Parser Cast
parseCast = 
  do _ <-  space
     _ <- string' "CAST"
     _ <-  space
     return (Cast)

parseNew :: Parser Expression
parseNew = 
  do _ <-  space
     _ <- string' "NEW"
     _ <-  space
     _ <- string' "#"
     _ <-  space
     _ <- string' "("
     _ <-  space
     expr <- parseExpression
     _ <- space
     _ <- string' ")"
     _ <-  space
     _ <- string' "."
     return (New expr)


parseStringTemplates :: Parser StringTemplate
parseStringTemplates = do
  return (StringTemplate "")
-- | res: {d DATE = ISO} {}|
-- | res: {d DATE = USER} {}|
-- | res: {number SIGN = RIGHT } {}|
-- | res: {number STYLE = SCIENTIFIC } {}|

-- && concatenation
parseAndAnd :: Parser AndAnd
parseAndAnd =
  do return AndAnd

parseBetweenAnd :: Parser Between
parseBetweenAnd =
  do _ <- string' "BETWEEN"
     val0 <- parseName
     _ <- string' "AND"
     val1 <- parseName
     return (Between val0 val1)

parseIsInitial :: Parser IsInitial
parseIsInitial =
  do _ <- string' "IS INITIAL"
     return (IsInitial True)

parseIsNotInitial :: Parser IsInitial
parseIsNotInitial =
  do _ <- string' "IS NOT INITIAL"
     return (IsInitial False)

parseClear :: Parser Clear
parseClear =
  do _ <- string' "CLEAR"
     return Clear

parseDoTimes :: Parser DoTimes
parseDoTimes =
  do _ <- string' "DO"
     int <- parseInt
     _ <- string'"TIMES"
     statements <- many parseStatement
     _ <- string' "ENDDO"
     return (DoTimes int statements)

parseDoLoop :: Parser DoLoop
parseDoLoop =
  do _ <- string' "DO"
     stmnts <- many parseStatement
     _ <- string' "ENDDO"
     return (DoLoop stmnts)

parseExit :: Parser Exit
parseExit =
  do _ <- string' "EXIT"
     return Exit

parseLoopAt :: Parser Statement
parseLoopAt =
  do _ <- string' "LOOP"
     _ <-  space
     _ <- string' "AT"
     _ <-  space

     -- optional
     _ <- string' "USING"
     _ <-  space
     _ <- string' "KEY"
     _ <-  space
     key <- parseName
     _ <-  space

     -- optional
     _ <- string' "ASSIGNING"
     _ <-  space
     _ <- string' "<"
     assigning <- parseName
     _ <- string' ">"
     _ <-  space

     _ <- string' "WHERE"
     _ <-  space
     wheres <- many parseWhere

     internalTable <- parseName

     -- optional
     _ <- string' "INTO"
     var <- parseName

     _ <- string' "ENDLOOP"
     return (LoopSt (LoopAt "" ""))

parseAppendTable :: Parser DeclStatementClass
parseAppendTable =
  do _ <- string' "APPEND"
     row <- parseName
     _ <-  space
     _ <- string' "TO"
     table <- parseName
     _ <-  space
     return (St (TableSt (Append row table)))

parseReadTable :: Parser DeclStatementClass
parseReadTable =
  do _ <- string' "READ"
     _ <-  space
     _ <- string' "TABLE"
     itab <- parseName
     _ <-  space
     _ <- string' "INTO"
     _ <-  space
     wa <- parseName
     _ <-  space
     _ <- string' "INDEX"
     _ <-  space
     index <- parseInt
     _ <-  space
     return (St (TableSt (ReadTable itab wa index)))

parseTypes :: Parser TYPESStatement
parseTypes =
  do _ <-  space
     _ <- string' "TYPES"
     _ <-  space
     var <- parseName
     _ <-  space
     _ <- string' "TYPE"
     choice [parseTypeP]
     _ <-  space
     _ <- string' "."
     return (TYPESStatement )


parseTypesStructureBegin :: Parser TYPESStatement
parseTypesStructureBegin =
  do _ <-  space
     _ <- string' "TYPES"
     _ <-  space
     _ <- string' "BEGIN"
     _ <-  space
     _ <- string' "OF"
     _ <-  space
     var <- parseName
     _ <-  space
     _ <- string' "."
     return (TYPESStatement )


parseTypesStructureEnd :: Parser TYPESStatement
parseTypesStructureEnd =
  do _ <-  space
     _ <- string' "TYPES"
     _ <-  space
     _ <- string' "END"
     _ <-  space
     _ <- string' "OF"
     _ <-  space
     var <- parseName
     _ <-  space
     _ <- string' "."
     return (TYPESStatement )

data Structure = Structure Text [DATADeclaration] Text

parseStructure :: Parser Structure
parseStructure =
  do _ <-  space
     _ <- string' "BEGIN"
     _ <-  space
     _ <- string' "OF"
     _ <-  space
     var0 <- parseName
     _ <-  space
     _ <- string' ","
     _ <-  space
     lines <- many parseType
     _ <- string' "END"
     _ <-  space
     _ <- string' "OF"
     _ <-  space
     var1 <- parseName
     _ <-  space
     _ <- string' "."
     return (Structure var0 lines var1)

parseType :: Parser DATADeclaration
parseType = -- do return (AbapInt 0)
  do _ <-  space
     var0 <- parseName
     _ <-  space
     _ <- string' "TYPE"
     _ <-  space
     _ <- string' ","
     _ <-  space
     ty <- choice [parseTypeP, parseTableOf, parseStandardTableOf, parseSortedTableOf, parseHashedTableOf, parseLikeLineOf]
     return (DATAType var0 ty Nothing Nothing)


parseTypesStructure :: Parser Types
parseTypesStructure =
  do _ <-  space
     _ <- string' "TYPES:"
     _ <-  space
     struct <- parseStructure
     return (Types struct)

data Types = Types Structure

parseConstantsStructure :: Parser Types
parseConstantsStructure =
  do _ <-  space
     _ <- string' "CONSTANTS:"
     _ <-  space
     struct <- parseStructure
     return (Types struct)

parseTypeP :: Parser ABAP_Type
parseTypeP =
  do ty <- parseName
     _ <-  space
     _ <- string' "LENGTH"
     _ <-  space
     len <- parseInt
     _ <-  space
     _ <- string' "DECIMALS"
     dec <- parseInt
     return (SimpleType (TP (TypeP len dec)))

parseTypeTable :: Parser ABAP_Type
parseTypeTable =
  do _ <- string' "TABLE"
     _ <- string' "OF"
     rt <- parseName
     return (Table rt)

parseTypeRefTo :: Parser TypeRefTo
parseTypeRefTo =
  do _ <- string' "REF"
     _ <- string' "TO"
     var <- parseName
     return (TypeRefTo var)

parseFIELdSyMbols :: Parser Fields
parseFIELdSyMbols =
  do _ <- string' "FIELD-SYMBOLS"
     _ <- space
     _ <- string' "<"
     var <- parseName
     _ <- string' ">"
     _ <- space
     _ <- string' "LIKE"
     _ <- space
     _ <- string' "LINE"
     _ <- space
     _ <- string' "OF"
     _ <- space
     of_ <- parseName
     _ <- space
     _ <- string' "."
     return (Fields var of_)

parseStringTemplate :: Parser StringTemplate
parseStringTemplate =
  do _ <- char '|'
    
     _ <- char '|'
     return (StringTemplate "")

parseMethods :: Parser Methods
parseMethods =
  do _ <- string' "METHODS"
     _ <- space
     importing <- many parseImporting
     _ <- space
     exporting <- many parseExporting
     _ <- space
     changing <- many parseChanging
     _ <- space
     raising <- many parseRaising
     _ <- space
     returning <- many parseReturning
     _ <- space
     _ <- string' "."
     _ <- space
     return (Methods importing exporting changing returning raising)

parseImporting :: Parser DATADeclaration
parseImporting = do return (DATAType "" (SimpleType (TI (TypeI 0))) Nothing Nothing)

parseExporting :: Parser DATADeclaration
parseExporting = do return (DATAType "" (SimpleType (TI (TypeI 0))) Nothing Nothing)

parseChanging :: Parser DATADeclaration
parseChanging = do return (DATAType "" (SimpleType (TI (TypeI 0))) Nothing Nothing)

parseRaising :: Parser Exception
parseRaising = do return Exception

parseReturning :: Parser DATADeclaration
parseReturning = do return (DATAType "" (SimpleType (TI (TypeI 0))) Nothing Nothing)

parseMethod :: Parser Method
parseMethod =
  do _ <- string' "METHOD"
     _ <- space
     name <- parseName
     _ <- string' "."
     _ <- space
     decStatements <- many (choice [space2, parseStatement, parseData])
     _ <- string' "ENDMETHOD"
     return (Method name decStatements)

space2 :: Parser DeclStatementClass
space2 = do a <- many mySpace
            _ <- space
            return (Comments a)

mySpace :: Parser Comment
mySpace = choice [commentStar, spaceLine]

spaceLine :: Parser Comment
spaceLine = do _ <- hspace
               _ <- newline
               return NoComment

commentStar :: Parser Comment
commentStar = 
  do _ <- string' "*"
     comment <- many (satisfy (/= '\n'))
     _ <- newline
     return (Comment comment)

parseName :: Parser Text
parseName = do
  first <- satisfy isAlpha
  rest <- many (satisfy isAlphaNum2)
  return (T.pack (first : rest))

-- isAlphaNum2 x = isAlphaNum x || x == '_' -- || x == '~'

data AbapType = AbapInt Int

parseInt :: Parser Int
parseInt = do i <- many digitChar
              return ((read i) :: Int)

parseClassDefinition :: Parser DeclStatementClass
parseClassDefinition =
  do _ <- string' "CLASS"
     _ <- space
     var <- parseName
     _ <- space
     _ <- string' "DEFINITION"
     _ <- space
     i <- parseInit
     let sections = []
     sections <- some (choice [publicSection, protectedSection, privateSection])
     _ <- string' "ENDCLASS"
     _ <- space
     _ <- string' "."
     _ <- space
     return (Dec (DecClass (ClassDefinition var i sections)))

parseInit :: Parser Init
parseInit = do
    _ <- string' "PUBLIC"
    _ <- space
    _ <- string' "FINAL"
    _ <- space
    _ <- string' "CREATE"
    _ <- space
    _ <- string' "PUBLIC"
    _ <- space
    _ <- string' "."
    _ <- space
    return Init


parseClassImpl :: Parser ClassImplementation
parseClassImpl =
  do _ <- string' "CLASS"
     _ <- string' "IMPLEMENTATION"
     _ <- string' "ENDCLASS"
     return (ClassImplementation "" [])

publicSection :: Parser Section
publicSection =
  do _ <- string' "PUBLIC"
     _ <- space
     _ <- string' "SECTION"
     _ <-  space
     _ <- string' "."
     _ <-  space
     sds <- many (choice [parseInterfaces, parseStatement, parseData])
     return (Section PUBLIC sds)

parseInterfaces :: Parser DeclStatementClass
parseInterfaces =
  do _ <- string' "INTERFACES"
     _ <- space
     var <- parseName
     _ <-  space
     _ <- string' "."
     _ <-  space
     return (Intf (Interfaces var))

protectedSection :: Parser Section
protectedSection =
  do _ <- string' "PROTECTED"
     _ <-  space
     _ <- string' "SECTION"
     _ <-  space
     _ <- string' "."
     _ <-  space
     sds <- many (choice [parseStatement, parseData])
     return (Section PROTECTED sds)

privateSection :: Parser Section
privateSection =
  do _ <- string' "PRIVATE"
     _ <-  space
     _ <- string' "SECTION"
     _ <-  space
     _ <- string' "."
     _ <-  space
     sds <- many (choice [parseStatement, parseData])
     return (Section PRIVATE sds)

parseClassImplementation :: Parser DeclStatementClass
parseClassImplementation =
  do _ <- string' "CLASS"
     _ <-  space
     var <- parseName
     _ <-  space
     _ <- string' "IMPLEMENTATION"
     _ <-  space
     _ <- string' "."
     _ <-  space
     statements <- many parseStatement
     _ <- string' "ENDCLASS"
     _ <-  space
     return (Dec (ImpClass (ClassImplementation var statements)))

parseStatement :: Parser DeclStatementClass
parseStatement = choice [parseAssignment]

parseCase :: Parser DeclStatementClass
parseCase =
  do  _ <-  space
      _ <- string' "CASE"
      _ <-  space
      scrutinee <- parseName
      _ <-  space
      _ <- string' "."
      _ <-  space
      whens <- many parseWhen
      _ <- space

-- should be optional
      _ <- string' "WHEN"
      _ <- space
      _ <- string' "OTHERS"
      _ <- space
      _ <- string' "."
      _ <- space
      whenOther <- some parseStatement
      _ <-  space

      _ <- string' "ENDCASE"
      return (St (Cas scrutinee whens whenOther))

parseWhen :: Parser When
parseWhen =
  do _ <-  space
     _ <- string' "WHEN"
     _ <-  space
     operand <- parseName
     _ <-  space
     operands <- many parseOrOperand
     _ <- string' "."
     _ <-  space
     sts <- some parseStatement
     _ <-  space
     return (When (operand : operands) sts)

parseOrOperand :: Parser Text
parseOrOperand =
  do _ <-  space
     _ <- string' "OR"
     _ <-  space
     operand <- parseName
     _ <-  space
     return operand

parseTry :: Parser Try
parseTry =
  do  _ <-  space
      _ <- string' "TRY"
      _ <-  space
      _ <- string' "."
      _ <-  space
      stdecls <- some parseStatement
      _ <- space
      _ <- string' "CATCH"
      _ <- space
      var <- parseName
      _ <-  space
      _ <- string' "."
      _ <- space
      catchStatements <- some parseStatement
      _ <-  space
      _ <- string' "ENDTRY"
      _ <-  space
      _ <- string' "."
      return (Try stdecls var catchStatements)

data Select = Select JoinOrTable [JoinTable] [Field] [ComprWhere] SQLPart

parseSelect :: Parser Select
parseSelect =
  do  _ <-  space
      _ <- string' "SELECT"
      _ <-  space
      _ <- string' "FROM"
      _ <-  space
      from <- choice [parseTableName, parseJoin]
      _ <-  space
      joins <- many (choice [parseInnerJoin, parseLeftOuterJoin, parseRightOuterJoin])
      _ <- space
      _ <- string' "FIELDS"
      _ <- space
      fields <- some parseField
      _ <-  space
      _ <- string' "WHERE"
      _ <-  space
      wheres <- many parseWhere
      _ <-  space
      _ <- string' "INTO"
      _ <-  space
      -- optional
      _ <- string' "CORRESPONDING"
      _ <-  space
      _ <- string' "FIELDS"
      _ <-  space
      _ <- string' "OF"
      _ <-  space

      -- optional
      _ <- string' "@APPENDING"
      _ <-  space

      _ <- string' "@DATA"
      _ <-  space
      _ <- string' "("
      _ <-  space
      
      _ <-  space
      _ <- string' ")"
      wheres <- many parseWhere
      _ <-  space

      -- optional
      orderBy <- parseOrderBy
      _ <- string' "."

      -- optional
      orderBy <- parseGroupBy
      _ <- string' "."

      return (Select from joins fields wheres orderBy)

parseWhere :: Parser ComprWhere
parseWhere = do return Where

parseTableName :: Parser JoinOrTable
parseTableName =
  do name <- parseName
     return (T name)

data Field = Field Text ABAPSQLExpression Text

parseField :: Parser Field
parseField =
  do _ <- string' "DISTINCT"
     _ <-  space
     n1 <- parseName
     _ <-  space
     _ <- string' "CAST"
     _ <-  space
     _ <- string' "("
     _ <-  space
     exp <- parseABAPSQLExpression
     _ <-  space
     _ <- string' ")"

    -- optional
     _ <- string' "AS"
     _ <-  space
     n2 <- parseName
     _ <-  space
     return (Field n1 exp n2)

data ABAPSQLExpression = ABAPSQLExpression Text ABAPSQLOp Text

parseABAPSQLExpression :: Parser ABAPSQLExpression
parseABAPSQLExpression =
  do _ <-  space
     n1 <- parseName
     _ <-  space
     operator <- choice [ parseFlDiv, parseFlMul, parseDIV, parseMOD, parseDIVISION, parseRound, parseMul] -- , parseAndAnd]
     _ <-  space
     n2 <- parseName
     _ <-  space
     return (ABAPSQLExpression n1 operator n2)

data ABAPSQLOp = FlDiv | FlMul | DIV | MOD | DIVISION | Round | Mul | AndAnd_

parseFlDiv :: Parser ABAPSQLOp
parseFlDiv = do _ <- string' "/"
                return FlDiv

parseFlMul :: Parser ABAPSQLOp
parseFlMul = do _ <- string' "*"
                return FlMul

parseDIV :: Parser ABAPSQLOp
parseDIV = do _ <- string' "DIV"
              return DIV

parseMOD :: Parser ABAPSQLOp
parseMOD = do _ <- string' "MOD"
              return MOD

parseDIVISION :: Parser ABAPSQLOp
parseDIVISION = do _ <- string' "DIVISION"
                   return DIVISION

parseRound :: Parser ABAPSQLOp
parseRound = do _ <- string' "Round"
                return Round

parseMul :: Parser ABAPSQLOp
parseMul = do _ <- string' "*"
              return Mul

--parseAndAnd :: Parser ABAPSQLOp
--parseAndAnd = do _ <- string' "("
--                 return AndAnd

{-
Function CONCAT( ) concatenates two strings (without blanks).
Function CONCAT_WITH_SPACE( ) does the same but inserts a specified number of
blanks in the middle.
Functions UPPER( ) and LOWER( ) transforms an argument to uppercase / lowercase.
Function INITCAP( ) works like LOWER( ) but transforms the first letter of each word to
upper case.
Functions LEFT( ) extracts the first n characters from a given argument. RIGHT( ) does the
same but starts from the right.
Function SUBSTRING( )
-}

data JoinOrTable = Join JoinTable JoinOn | T Table
data JoinOn = On

parseJoin :: Parser JoinOrTable
parseJoin =
  do  _ <-  space
      _ <- string' "("
      _ <-  space
      j <- choice [parseInnerJoin, parseLeftOuterJoin, parseRightOuterJoin]
      _ <-  space
      _ <- string' ")"
      _ <- string' "ON"
      ons <- many parseOn
      _ <-  space
      return (Join j (On))

parseOn :: Parser JoinOn
parseOn = do return On

data JoinTable = JoinTable JoinType JoinOrTable
data JoinType = Inner | LeftOuter | RightOuter

parseInnerJoin :: Parser JoinTable
parseInnerJoin =
  do  _ <-  space
      _ <- string' "INNER"
      _ <-  space
      _ <- string' "JOIN"
      _ <-  space
      table <- choice [parseJoin, parseTableName]
      _ <-  space
      return (JoinTable Inner table)


parseLeftOuterJoin :: Parser JoinTable
parseLeftOuterJoin =
  do  _ <- string' "LEFT"
      _ <-  space
      _ <- string' "OUTER"
      _ <-  space
      _ <- string' "JOIN"
      _ <-  space
      table <- choice [parseJoin, parseTableName]
      _ <-  space
      return (JoinTable LeftOuter table)


parseRightOuterJoin :: Parser JoinTable
parseRightOuterJoin =
  do  _ <- string' "RIGHT"
      _ <-  space
      _ <- string' "OUTER"
      _ <-  space
      _ <- string' "JOIN"
      _ <-  space
      table <- choice [parseJoin, parseTableName]
      _ <-  space
      return (JoinTable RightOuter table)


----------------------------------------
-- aggregation

data OrderBy = OrderBy Text By

parseOrderBy :: Parser OrderBy
parseOrderBy =
  do _ <-  space
     _ <- string' "ORDER"
     _ <-  space
     _ <- string' "BY"
     op1 <- parseName
     _ <-  space
     by <- choice [parseAscending, parseDescending]
     return (OrderBy op1 by)

data By = Ascending | Descending

parseAscending :: Parser By
parseAscending =
  do _ <- string' "ASCENDING"
     return Ascending

parseDescending :: Parser By
parseDescending =
  do _ <- string' "DESCENDING"
     return Descending

data Aggregation = SUM Expression | MAX Expression | MIN Expression | SVG Expression | AVG Expression | COUNT Expression

parseSUM :: Parser Aggregation
parseSUM =
  do _ <- space
     _ <- string' "SUM"
     _ <- space
     _ <- string' "("
     _ <- space
     expression <- parseExpression
     _ <- string' ")"
     _ <- space
     return (SUM expression)


parseMAX :: Parser Aggregation
parseMAX =
  do _ <- space
     _ <- string' "MAX"
     _ <- space
     _ <- string' "("
     _ <- space
     expression <- parseExpression
     _ <- string' ")"
     _ <- space
     return (MAX expression)

parseMIN :: Parser Aggregation
parseMIN =
  do _ <- space
     _ <- string' "MIN"
     _ <- space
     _ <- string' "("
     _ <- space
     expression <- parseExpression
     _ <- string' ")"
     _ <- space
     return (MAX expression)

parseAVG :: Parser Aggregation
parseAVG =
  do _ <- space
     _ <- string' "AVG"
     _ <- space
     _ <- string' "("
     _ <- space
     expression <- parseExpression
     _ <- string' ")"
     _ <- space
     return (AVG expression)


parseCOUNT :: Parser Aggregation
parseCOUNT =
  do _ <- space
     _ <- string' "COUNT"
     _ <- space
     _ <- string' "("
     _ <- space

     _ <- string' "*"

     _ <- string' "DISITNCT"
     expression <- parseExpression

     _ <- string' ")"
     _ <- space
     return (COUNT expression)

data SQLPart = GroupBy Text | Sort Text [By]

parseGroupBy :: Parser SQLPart
parseGroupBy =
  do _ <-  space
     _ <- string' "GROUP"
     _ <-  space
     _ <- string' "BY"
     _ <-  space
     name <- parseName
     return (GroupBy name)

parseSort :: Parser SQLPart
parseSort =
  do _ <- string' "SORT"
     _ <-  space
     date <- parseName
     _ <-  space
     -- optional
     _ <- string' "BY"
     _ <-  space
     bys <- many parseBys
     _ <- string' "."
     return (Sort date bys)

parseBys :: Parser By
parseBys =
  do date <- parseName
     _ <-  space
     dir <- choice [parseAscending, parseDescending]
     _ <-  space
     return dir

data DeleteAdjacent = DeleteAdjacent Text

parseDeleteAdjacent :: Parser DeleteAdjacent
parseDeleteAdjacent =
  do _ <- string' "DELETE"
     _ <-  space
     _ <- string' "ADJACENT"
     _ <-  space
     _ <- string' "DUPLICATES"
     _ <-  space
     _ <- string' "FROM"
     _ <-  space
     from <- parseName
     _ <-  space
     -- optional
     _ <- string' "COMPARING"
     _ <-  space

     _ <- string' "."
     return (DeleteAdjacent from)

data ComprWhere = FromTo Text | Where
data Comprehension = Comprehension Text ComprWhere Text

parseComprehension :: Parser Comprehension
parseComprehension =
  do _ <- string' "FOR"
     _ <-  space
     line <- parseName
     _ <-  space
     _ <- string' "IN"
     _ <-  space
     compr <- choice [parseFromTo, parseWhere]
     _ <-  space
     table <- parseName
     _ <-  space
     _ <- string' "."
     return (Comprehension line compr table)

parseFromTo :: Parser ComprWhere
parseFromTo =
  do _ <- string' "FROM"
     _ <-  space
     line <- parseName
     _ <-  space
     _ <- string' "TO"
     return (FromTo line)

data Modify = Modify Text Text

parseModify :: Parser Modify
parseModify =
  do _ <- string' "MODIFY"
     _ <-  space
     table <- parseName
     _ <- space
     _ <- string' "FROM"
     _ <- space
     line <- parseName
     _ <- space
     return (Modify table line)


{-
data BraceTree = NormalBrace [[BraceTree]] | RectBrace [[BraceTree]] | Leaf Text
data BracesCount = BracesCount { normalBraces :: Int, rectBraces :: Int }

-- | Add newlines after commas and braces to shrink the size of a haskell-value
myShow :: Show a => a -> String
myShow = T.unpack . T.concat . map showTree . braceTree . T.pack . show

showTree :: BraceTree -> Text
showTree (NormalBrace trees) = "(" `T.append` (T.intercalate "," (map (T.concat . map (maybeInsertNewline . showTree)) trees)) `T.append` ")"
showTree (RectBrace trees) =   "[" `T.append` (T.intercalate "," (map (T.concat . map (maybeInsertNewline . showTree)) trees)) `T.append` "]"
showTree (Leaf t) = t

maybeInsertNewline :: Text -> Text
maybeInsertNewline t | T.length t > 80 = t `T.append` "\n"
                     | otherwise = t

parseAll :: Parser [BraceTree]
parseAll = many (AT.choice [parseBrace1, parseBrace2, rest])

rest :: Parser BraceTree
rest = do content <- many nonCommaBrace
          return (Leaf (T.pack content))
  where nonCommaBrace :: Parser Char
        nonCommaBrace = satisfy (\x -> not (x == ',' || x == '(' || x == ')' || x == '[' || x == ']'))

parseBrace1 :: Parser BraceTree
parseBrace1 =
  do _ <- AT.char '('
     content <- commaSeparated
     _ <- AT.char ')'
     return (NormalBrace content)

parseBrace2 :: Parser BraceTree
parseBrace2 =
  do _ <- AT.char '['
     content <- commaSeparated
     _ <- AT.char ']'
     return (RectBrace content)

commaSeparated :: Parser [[BraceTree]]
commaSeparated = sepBy parseAll (string ",")
-}
