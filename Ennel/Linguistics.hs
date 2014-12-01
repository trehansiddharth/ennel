module Ennel.Linguistics where
	import Data.List

	import Control.Monad

	data LexicalCategory = LexicalCategory { categoryName :: PartOfSpeech, projection :: LexicalProjection, inflections :: [Inflection] }
		deriving (Show, Eq, Ord)

	n = LexicalCategory N Head []
	v = LexicalCategory V Head []
	det = LexicalCategory Det Head []
	prep = LexicalCategory Prep Head []
	adj = LexicalCategory Adj Head []
	adv = LexicalCategory Adv Head []

	n' = LexicalCategory N Bar []
	v' = LexicalCategory V Bar []
	det' = LexicalCategory Det Bar []
	prep' = LexicalCategory Prep Bar []
	adj' = LexicalCategory Adj Bar []
	adv' = LexicalCategory Adv Bar []

	np = LexicalCategory N Phrase []
	vp = LexicalCategory V Phrase []
	detp = LexicalCategory Det Phrase []
	prepp = LexicalCategory Prep Phrase []
	adjp = LexicalCategory Adj Phrase []
	advp = LexicalCategory Adv Phrase []

	data PartOfSpeech = N | V | Det | Prep | Adj | Adv
		deriving (Show, Eq, Ord, Read, Enum)

	data LexicalProjection = Head | Bar | Phrase
		deriving (Show, Eq, Ord, Enum)

	data MergeLevel = Complement | Modifier | Specifier
		deriving (Show, Eq, Ord, Enum)

	data Inflection = Plus { inflectionName :: String } | Minus { inflectionName :: String }
		deriving (Show, Eq, Ord)

	data SemanticTree = SemanticTree { root :: String, children :: [SemanticTree] }
		deriving (Show, Eq)

	data SyntacticContext = Before [LexicalCategory] LexicalCategory
		| After [LexicalCategory] LexicalCategory
		| Around [LexicalCategory] [LexicalCategory] LexicalCategory
		| Singleton LexicalCategory
		deriving (Show, Eq)

	leftSisters :: SyntacticContext -> [LexicalCategory]
	leftSisters (Before xs _) = xs
	leftSisters (Around xs _ _) = xs
	leftSisters _ = []

	rightSisters :: SyntacticContext -> [LexicalCategory]
	rightSisters (After xs _) = xs
	rightSisters (Around _ xs _) = xs
	rightSisters _ = []

	sisters :: SyntacticContext -> [LexicalCategory]
	sisters s = leftSisters s ++ rightSisters s

	parent :: SyntacticContext -> LexicalCategory
	parent (Before _ x) = x
	parent (After _ x) = x
	parent (Around _ _ x) = x
	parent (Singleton x) = x

	fits :: LexicalCategory -> LexicalCategory -> Bool
	fits (LexicalCategory cx px isx) (LexicalCategory cy py isy) = and $ (++) [cx == cy, px == py] $ do
		iy <- isy
		return $ case iy of
			Plus iny -> (Plus iny) `elem` isx
			Minus iny -> not $ (Plus iny) `elem` isx

	canfit :: LexicalCategory -> LexicalCategory -> Bool
	canfit (LexicalCategory cx px isx) (LexicalCategory cy py isy) = and $ (++) [cx == cy, px == py] $ do
		iy <- isy
		return $ case iy of
			Plus iny -> True
			Minus iny -> not $ (Plus iny) `elem` isx

	fitInto :: LexicalCategory -> LexicalCategory -> Maybe [Inflection]
	fitInto (LexicalCategory cx px isx) (LexicalCategory cy py isy) = do
		guard (cx == cy && px == py)
		sequence $ do
			iy <- isy
			case iy of
				Plus iny -> if (Plus iny) `elem` isx
					then []
					else if (Minus iny) `elem` isx
						then return Nothing
						else return (Just (Plus iny))
				Minus iny -> if (Plus iny) `elem` isx
					then return Nothing
					else if (Minus iny) `elem` isx
						then []
						else return (Just (Minus iny))

	into :: LexicalCategory -> LexicalCategory -> Maybe LexicalCategory
	into (LexicalCategory cx px isx) (LexicalCategory cy py isy) = if cx == cy && px <= py
		then Just $ LexicalCategory cy py $ union ((filter plus isx) \\ (map flop . filter minus $ isy)) (filter plus isy)
		else Nothing
			where
				plus (Plus _) = True
				plus _ = False
				minus (Minus _) = True
				minus _ = False
				flop (Plus x) = Minus x
				flop (Minus x) = Plus x

	contexts :: LexicalCategory -> [SyntacticContext]
	contexts (LexicalCategory c p is) = (:) inert $ case p of
		Head -> case c of
			N -> [Singleton n', After [prepp] n']
			V -> [Singleton v', After [prepp] v', After [np] v']
			Det -> [Singleton det']
			Prep -> [After [np] prep']
			Adj -> [Singleton adj']
			Adv -> [Singleton adv']
		Bar -> case c of
			N -> [Singleton np, Before [detp] np, After [adjp] n']
			V -> [Singleton vp, Before [advp] v', After [advp] v']
			Det -> [Singleton detp]
			Prep -> [Singleton prepp]
			Adj -> [Singleton adjp]
			Adv -> [Singleton advp]
		Phrase -> case c of
			N -> [Before [v] v', Before [prep] prep']
			V -> []
			Det -> [After [n'] np]
			Prep -> [Before [n] n', Before [v] v']
			Adj -> [After [n'] n']
			_ -> [] -- TODO
		where
			inert = Singleton (LexicalCategory c p [])
