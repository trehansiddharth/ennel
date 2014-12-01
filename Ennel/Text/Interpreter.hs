module Ennel.Text.Interpreter where
	import Ennel.Linguistics
	import Ennel.Dictionary.Data hiding (phrase, bindings)

	import Data.JustParse
	import Data.JustParse.Char
	import Data.JustParse.Combinator

	import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
	import Control.Monad
	import Data.Monoid

	interpret :: TemplateTable -> Interpreter
	interpret table cat = case interpretations of
		[] -> mempty
		_ -> choice_ interpretations
		where
			interpretations = do
				(c, Template bs t as s) <- table
				is' <- maybe mempty return (c `fitInto` cat)
				return $ do
					sb <- sequence . map (binding table is') $ bs
					phrase t
					sa <- sequence . map (binding table is') $ as
					return $ foldr substitute s (sb ++ sa)

	phrase :: Phrase -> Parser Phrase Phrase
	phrase = sequence . map token

	binding :: TemplateTable -> [Inflection] -> Binding -> Parser Phrase (String, SemanticTree)
	binding table is' (Bind sel v, cat) = do
		if sel == "" then lookAhead (phrase (words sel)) else return []
		s <- interpret table cat
		return (v, s)

	substitute :: (String, SemanticTree) -> SemanticTree -> SemanticTree
	substitute (x', SemanticTree x xs) (SemanticTree y ys) = if x' == y
		then SemanticTree x xs
		else SemanticTree y (map (substitute (x', SemanticTree x xs)) ys)
