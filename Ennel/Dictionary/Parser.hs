module Ennel.Dictionary.Parser where
	import Ennel.Linguistics
	import Ennel.Dictionary.Data

	import Data.List (isPrefixOf, isSuffixOf, intersperse)

	import Data.JustParse
	import Data.JustParse.Char
	import Data.JustParse.Combinator

	import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)

	parse :: String -> Maybe Rules
	parse = parseOnly (entirely rules)

	entirely :: Stream s t => Parser s a -> Parser s a
	entirely p = p <* eof

	rules :: Parser String Rules
	rules = takeRules <$> line `sepBy` newline
		where
			takeRules [] = []
			takeRules ((Just x):xs) = x : (takeRules xs)
			takeRules (Nothing:xs) = takeRules xs

	line :: Parser String (Maybe Rule)
	line = (pure Nothing <* comment) <||> (Just <$> rule) <||> (pure Nothing <* emptyLine)
		where
			emptyLine = many hspace

	rule :: Parser String Rule
	rule = makeRule <$> (typeSignature <* newline) <*> ruleBody
		where
			makeRule (t, sig) (pre, t', post, sem) = Rule t sig pre post sem

	typeSignature :: Parser String (String, TypeSignature)
	typeSignature = makeTypeSignature <$> (targetName <* (spaced (char ':'))) <*> typeValue `sepBy1` (spaced (string "->"))
		where
			makeTypeSignature t xs = (t, TypeSignature (init xs) (last xs))

	targetName :: Parser String String
	targetName = combine <$> (many1 (noneOf " \t:>[\n~")) `sepBy_` (many1 hspace)
		where
			combine = concat . intersperse " "

	typeValue :: Parser String LexicalCategory
	typeValue = makeLexicalCategory <$> many1 (noneOf " (-\n") <*> (many hspace *> (pure [] <||> parenthesized (csv inflection)))
		where
			makeLexicalCategory c = case last c of
				'\'' -> LexicalCategory (read (init c)) Bar
				'P' -> LexicalCategory (read (init c)) Phrase
				_ -> LexicalCategory (read c) Head

	inflection :: Parser String Inflection
	inflection = makeInflection <$> spaced (oneOf "+-") <*> (many1 (noneOf " \t),"))
		where
			makeInflection '+' i = Plus i
			makeInflection '-' i = Minus i

	ruleBody :: Parser String ([Bind], String, [Bind], SemanticTree)
	ruleBody = (,,,) <$> spaced (bind `sepBy_` (many hspace)) <*> targetName <*> spaced (bind `sepBy_` (many hspace)) <*> (spaced (char '~') *> semanticValue)

	bind :: Parser String Bind
	bind = bracketed (Bind <$> (targetName <* (spaced (char '~'))) <*> (many1 (noneOf " []")))

	semanticValue :: Parser String SemanticTree
	semanticValue = SemanticTree <$> spaced (many1 (noneOf " (),\n")) <*> (pure [] <||> parenthesized (csv semanticValue))

	comment :: Parser String String
	comment = many hspace *> string "--" *> many (noneOf "\n")

	newline :: Parser String ()
	newline = (pure <$> char '\n' <||> string "\r\n") >> pure ()

	csv :: Parser String a -> Parser String [a]
	csv p = p `sepBy` (spaced (char ','))

	parenthesized :: Parser String a -> Parser String a
	parenthesized p = char '(' *> p <* char ')'

	bracketed :: Parser String a -> Parser String a
	bracketed p = char '[' *> p <* char ']'

	hspace :: Parser String Char
	hspace = oneOf " \r\t"

	spaced :: Parser String a -> Parser String a
	spaced p = many hspace *> p <* many hspace
