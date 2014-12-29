module Ennel.Text.Interpreter where
	import Ennel.Linguistics
	import Ennel.Dictionary.Data hiding (phrase)

	import Data.JustParse
	import Data.JustParse.Char
	import Data.JustParse.Combinator hiding (branch)

	import Control.Applicative ((<$>), (<*>), (<*), (*>), pure)
	import Control.Monad
	import Data.Monoid

	import Control.Applicative.Automaton

	import Data.Traversable hiding (sequence)

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

	interpret' :: TemplateTable -> LexicalCategory -> QueueAutomaton Word SemanticTree
	interpret' table cat = step *> branch (toPriorityTree interpretations)
		where
			interpretations = do
				(c, Template bs t as s) <- table
				is' <- maybe mempty return (c `fitInto` cat)
				let sb = sequenceA . map (binding' table is') $ bs
				let p = phrase' t
				let sa = sequenceA . map (binding' table is') $ as
				return $ (\sb' p' sa' -> foldr substitute s (sb' ++ sa')) <$> sb <*> p <*> sa

	phrase :: Phrase -> Parser Phrase Phrase
	phrase = sequence . map token

	phrase' :: Phrase -> QueueAutomaton Word Phrase
	phrase' = sequenceA . map (accept . (==))

	binding :: TemplateTable -> [Inflection] -> Binding -> Parser Phrase (String, SemanticTree)
	binding table is' (Bind sel v, cat) = do
		if sel == "" then lookAhead (phrase (words sel)) else return []
		s <- interpret table cat
		return (v, s)

	binding' :: TemplateTable -> [Inflection] -> Binding -> QueueAutomaton Word (String, SemanticTree)
	binding' table is' (Bind sel v, cat) = (,) <$> pure v <*> interpret' table cat

	substitute :: (String, SemanticTree) -> SemanticTree -> SemanticTree
	substitute (x', SemanticTree x xs) (SemanticTree y ys) = if x' == y
		then SemanticTree x xs
		else SemanticTree y (map (substitute (x', SemanticTree x xs)) ys)
