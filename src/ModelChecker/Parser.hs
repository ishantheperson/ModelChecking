module ModelChecker.Parser where 

import ModelChecker.AST 

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok 

-- | Parses the given string, and returns either
--   a parse error or a fully parsed Statement
parseString :: String -> Either ParseError Statement 
parseString = parse (whitespace *> input) "" 
  where input = Statement <$> many quantifier <*> matrix 

-- | Parses one quantifier expression 
quantifier :: Parser Quantifier
quantifier = quant "forall" Forall <|> quant "exists" convertExists 
  where quant string op = do 
          reserved string 
          name <- identifier 
          reservedOp "."
          return $ op name 

matrix, term :: Parser Matrix 
-- | Parses the non-quantifier portion of the formula 
matrix = buildExpressionParser operators term <?> "expression"
term = parens matrix <|> Variable <$> identifier <?> "expression"

operators = [[Prefix (Negation <$ choice [reservedOp "~", reservedOp "!"])],
             [mkOp "&&" And,
              mkOp "==" Equals,
              mkOp "->" RelatedTo,
              mkOp "||" convertOr,
              mkOp "!=" convertNotEqual]]
  where mkOp s f = Infix (f <$ reservedOp s) AssocLeft

lexer = Tok.makeTokenParser (emptyDef {
  Tok.reservedNames = ["forall", "exists"],
  Tok.identStart = letter,
  Tok.identLetter = alphaNum <|> char '\'' <?> "identifier",
  Tok.commentLine = "#"
})

integer = Tok.natural lexer 

whitespace = Tok.whiteSpace lexer 

parens = Tok.parens lexer 
reserved = Tok.reserved lexer 
reservedOp = Tok.reservedOp lexer 
identifier = Tok.identifier lexer 