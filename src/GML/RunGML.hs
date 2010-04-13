module GML.RunGML (runGML, module GML.ToRenderObject) where

import GML.Parser
import GML.AST
import GML.Evaluate
import GML.ToRenderObject

import GML.Parser.ApplicativeParsec
import Text.ParserCombinators.Parsec.Token

import System.FilePath
import qualified Data.Map as M
import Data.Texture

-- | Datatype containing preprocessing directives
data Include = TextureDef String String | Include String

-- | Retrieve all gml includes
getIncludes::[Include] -> [String]
getIncludes []               = []
getIncludes (Include i:xs) = i:getIncludes xs
getIncludes (_:xs)           = getIncludes xs

-- | Retrieve all texture bindings
getTextures::[Include] -> [(String,String)]
getTextures []                     = []
getTextures (TextureDef n f:xs) = (n,f):getTextures xs
getTextures (_:xs)                 = getTextures xs

-- | Evaluate a gml file
runGML::String -> IO ([Scene], Textures)
runGML file = do (gml, txs) <- preprocess file
                 let (_, st, _) = evaluate (M.empty,[],gml)
                 return (foldr takeRender [] st, txs)
    where takeRender (Render s) ls = s:ls
          takeRender _          _  = [] 

-- | Preprocess a gml file. Recursively calls itself to import includes. Also includes textures.
preprocess :: String -> IO (GML, Textures)
preprocess file = do fstr <- readFile file
                     let newgml = either (\e -> error $ "Error in gml "++file++": "++show e) id $ parseGML fstr
                     let addcurdir x = takeDirectory file ++ "/" ++ x
                     --Parse includes
                     let incs = either (\e -> error $ "Error in includes "++file++": "++show e) id $ parseIncludes fstr
                     --Run gmls
                     incsall <- mapM (preprocess.addcurdir) $ getIncludes incs
                     let (incsgml, incstxs) = unzip incsall
                     --Load textures
                     let (txsnames, txsfiles) = unzip $ getTextures incs
                     txsdata <- mapM (loadTexture.addcurdir) txsfiles                     
                     let newtxs = M.fromList (zip txsnames txsdata)
                    
                     return (concat incsgml ++ newgml, M.unions $ newtxs:incstxs)

-- | Language definition for preprocessing
incDef::LanguageDef a
incDef = LanguageDef  
   { commentStart   = ""
   , commentEnd     = ""
   , commentLine    = ""
   , nestedComments = False
   , identStart     = empty
   , identLetter    = empty
   , opStart        = empty
   , opLetter       = empty
   , reservedOpNames= []
   , reservedNames  = ["%include","%texture"]
   , caseSensitive  = False  
   }

incLexer::TokenParser a
incLexer = makeTokenParser incDef

-- | Parser for include directives. Parses only the top part of a file.
parseIncludes::String -> Either ParseError [Include]
parseIncludes = parse (whiteSpace incLexer *> many (parseInclude <|> parseTexture)) ""

parseInclude::Parser Include
parseInclude = reserved incLexer "%include" 
            *> (Include <$> stringLiteral incLexer)
           <?> "include"

parseTexture::Parser Include
parseTexture = reserved incLexer "%texture" 
            *> (TextureDef <$> stringLiteral incLexer <*> stringLiteral incLexer)
           <?> "texture"
