{-# LANGUAGE FlexibleInstances #-}
module Ennel.Dictionary.Data where
	import Ennel.Linguistics

	import Data.JustParse
	import Data.JustParse.Char
	import Data.JustParse.Combinator

	import Data.Monoid

	type Rules = [Rule]

	data Rule = Rule { target :: String, signature :: TypeSignature, prebinds :: [Bind], postbinds :: [Bind], definition :: SemanticTree }
		deriving (Show, Eq)

	data TypeSignature = TypeSignature { arguments :: [LexicalCategory], result :: LexicalCategory }
		deriving (Show, Eq)

	data Bind = Bind { selector :: String, variableName :: String }
		deriving (Show, Eq)

	type Word = String
	type Phrase = [Word]

	data Template = Template { before :: [Binding], phrase :: Phrase, after :: [Binding], semantics :: SemanticTree }
		deriving (Eq, Show)

	type Binding = (Bind, LexicalCategory)

	type TemplateTable = [(LexicalCategory, Template)]

	type Interpreter = LexicalCategory -> Parser Phrase SemanticTree