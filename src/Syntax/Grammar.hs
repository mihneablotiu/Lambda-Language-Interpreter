module Syntax.Grammar where

import Syntax.Expression
import Syntax.Parser
import Control.Applicative
import Data.Char (isSpace)

parseProgram :: String -> Maybe [Expression]
parseProgram = parse $ some (parseExpression <* many parseWhitespace) <* eof

parseExpressionWithoutDef :: Parser Expression
parseExpressionWithoutDef = parseVar <|> parseLambda <|> parseApp

parseExpression :: Parser Expression
parseExpression = parseDef <|> parseExpressionWithoutDef

parseVar :: Parser Expression
parseVar = Var <$> parseString

parseLambda :: Parser Expression
parseLambda = liftA2 Lambda (token '\\' *> parseString <* token '.') parseExpressionWithoutDef

parseApp :: Parser Expression
parseApp = liftA2 App (token '(' *> parseExpressionWithoutDef <* parseWhitespace) (parseExpressionWithoutDef <* token ')')

parseDef :: Parser Expression
parseDef = liftA2 Def (parseString <* token '=') parseExpressionWithoutDef

parseString :: Parser String
parseString = some letter

parseWhitespace :: Parser Char
parseWhitespace = spot isSpace
