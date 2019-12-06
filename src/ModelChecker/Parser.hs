module ModelChecker.Parser where 

import ModelChecker.AST 

import Control.Applicative ((<*>), (<$>))

import Text.ParserCombinators.Parsec 
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok 

-- | Parses the given string, and returns either
--   a parse error or a fully parsed Statement
parseString :: String -> Either ParseError Statement 
parseString = parse (whitespace *> input <* eof) "" 
  where input = Statement <$> (concat <$> many quantifier) <*> matrix

-- | Parses one quantifier expression (might introduce many variables) 
quantifier :: Parser [Quantifier]
quantifier = quant "forall" Forall <|> quant "exists" Exists <?> "quantifiers"
  where quant string op = do 
          reserved string 
          names <- many1 identifier 
          reservedOp "."
          return $ map op names 

matrix, term :: Parser Matrix 
-- | Parses the non-quantifier portion of the formula 
matrix = buildExpressionParser operators term 
term =  parens matrix 
    <|> ternary 
    <|> binary 

ternary = do 
  a <- try $ do identifier <* reservedOp "+"

  b <- identifier
  op <- (TernaryOp <$ (reservedOp "=" <|> reservedOp "=="))
        <|> ((\x y z -> Negation (TernaryOp x y z)) <$ reservedOp "!=")
  c <- identifier
  return $ op a b c 

binary = do 
  (a, op) <- try $ do a <- identifier 
                      op <- choice binOps 
                      return (a, op)
  b <- identifier

  return $ op a b 

  where binOps = [mkOp "="  Equals,
                  mkOp "==" Equals,
                  mkOp "->" RelatedTo,
                  mkOp "!=" convertNotEqual]    
        mkOp s f = f <$ reservedOp s 

operators = [[Prefix (Negation <$ choice [reservedOp "~", reservedOp "!"])],
             [mkOp "&&" And,
              mkOp "and" And,
              mkOp "||" convertOr,
              mkOp "or" convertOr],
             [mkOp "=>" convertImplies]]
  where mkOp s f = Infix (f <$ reservedOp s) AssocLeft

lexer = Tok.makeTokenParser (emptyDef {
  Tok.reservedNames = ["forall", "exists", "and", "or"],
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