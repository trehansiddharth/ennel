module Ennel where
	import Ennel.Linguistics
	import Ennel.Dictionary.Data
	import Ennel.Dictionary.Parser (entirely, parse)
	import Ennel.Dictionary.Builder (build)
	import Ennel.Text.Interpreter (interpret)

	import Data.JustParse
	import Data.JustParse.Combinator

	loadFile :: FilePath -> IO TemplateTable
	loadFile file = do
		contents <- readFile file
		case parse contents of
			Nothing -> fail "Error while parsing file."
			Just rules -> return (build rules)

	runInterpreter :: TemplateTable -> LexicalCategory -> String -> Maybe SemanticTree
	runInterpreter table cat = parseOnly (entirely $ interpret table cat) . words