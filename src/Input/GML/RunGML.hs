module Input.GML.RunGML (runGML, module Input.GML.ToRenderObject) where

import Input.GML.Parser
import Input.GML.AST
import Input.GML.Evaluate
import Input.GML.ToRenderObject

import Input.GML.Parser.ApplicativeParsec
import Text.ParserCombinators.Parsec.Token

import System.FilePath

import qualified Data.Map as M

runGML::String -> IO [Scene]
runGML file = do gml <- preprocess file
                 let (_, st, _) = evaluate (M.empty,[],gml)
                 return (foldr takeRender [] st)
    where takeRender (Render s) ls = s:ls
          takeRender _          _  = [] 

preprocess :: String -> IO GML
preprocess file = do fstr <- readFile file
                     let incs = either (\e -> error $ "Error in includes "++file++": "++show e) id $ parseIncludes fstr
                     let incsPath = map (\x->takeDirectory file ++"/"++x) incs
                     incgml <- mapM preprocess incsPath
                     let thisgml = either (\e -> error $ "Error in gml "++file++": "++show e) id $ parseGML fstr
                     return $ (concat incgml) ++ thisgml

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

parseIncludes::String -> Either ParseError [String]
parseIncludes = parse (whiteSpace incLexer *> many parseInclude) ""

parseInclude::Parser String
parseInclude = reserved incLexer "%include" *> stringLiteral incLexer       
               <?> "include"

parseTexture::Parser (String,String)
parseTexture = reserved incLexer "%texture" *> 
            ((,) <$> stringLiteral incLexer <*> stringLiteral incLexer)
               <?> "texture"
