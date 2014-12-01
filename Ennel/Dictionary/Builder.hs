module Ennel.Dictionary.Builder where
	import Ennel.Linguistics
	import Ennel.Dictionary.Data

	import qualified Data.Map as Map

	import Control.Monad
	import Data.Monoid
	import Control.Applicative

	build :: Rules -> TemplateTable
	build rules = singletonTemplates ++ (rules >>= makeTemplate)
		where
			makeTemplate (Rule t s pre post def) = case arguments s of
				[] -> do
					let cat = result s
					c <- contexts cat
					let preargs = leftSisters c
					let postargs = rightSisters c
					guard (length preargs >= length pre)
					guard (length postargs >= length post)
					let befores = zip (pre ++ repeat (Bind "" "_")) (leftSisters c)
					let afters = zip (post ++ repeat (Bind "" "_")) (rightSisters c)
					cat' <- maybe mempty return . foldl (<|>) (cat `into` parent c) . map (`into` parent c) $ preargs ++ postargs
					return (cat', Template befores (words t) afters def)
				args -> do
					let preargs = take (length pre) args
					let postargs = drop (length pre) args
					let befores = zip pre preargs
					let afters = zip post postargs
					return (result s, Template befores (words t) afters def)

	singletonTemplates :: TemplateTable
	singletonTemplates = do
		c <- [N .. Adv]
		p <- [Head .. Phrase]
		let cat = LexicalCategory c p []
		cx <- contexts cat
		case cx of
			Singleton cat' -> do
				guard (LexicalCategory c p [] /= cat')
				return (cat', Template [] [] [(Bind "" "x", cat)] (SemanticTree "x" []))
			_ -> mempty