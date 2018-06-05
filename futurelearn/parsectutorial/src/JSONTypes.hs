{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}


module JSONTypes
    ( JValue(..)
    , mkJPair
    , mkJObj
    ) where


import           Data.Map                               hiding (map)
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token    as P

type JMap = Data.Map.Map String JValue
data JValue = JString String
            | JNumber Integer
            | JObject JMap
            | JArray [JValue]
            | JBool Bool
            | JNull
    deriving (Show)

mkJPair k v = JObject (Data.Map.singleton k v)

mkJObj :: [JValue] -> JValue
mkJObj j_vals =
    let
        list_of_maps = map (\(JObject pair) -> pair) j_vals
        combined_map = Data.Map.unions list_of_maps
    in
        JObject combined_map

json_parser :: Parser JValue
json_parser = do
        whiteSpace
        j_top <- ( json_array_parser <|> json_obj_parser)
        return j_top

json_string_parser = do
    v <- stringLiteral
    return $ JString v

json_array_parser = do
    j_vals <- brackets $ commaSep json_value_parser
    return $ JArray j_vals

json_bool_parser = do
    bstr <- ( symbol "true" <|> symbol "false" )
    let
        bval = if bstr == "true" then True else False
    return $ JBool bval

json_pair_parser = do
    k <- stringLiteral
    colon
    v <- json_value_parser
    return $ mkJPair k v

json_obj_parser = do
    j_vals <- braces $ commaSep json_pair_parser -- a list of pairs
    return $ mkJObj j_vals

json_null_parser = do
    return $ JNull


json_value_parser =
    json_array_parser <|>
    json_obj_parser <|>
    json_string_parser <|>
--    json_number_parser <|>
    json_bool_parser <|>
    json_null_parser


lexer         = P.makeTokenParser emptyDef

parens        = P.parens lexer
brackets      = P.brackets lexer
braces        = P.braces lexer
commaSep      = P.commaSep lexer
whiteSpace    = P.whiteSpace lexer
symbol        = P.symbol lexer
identifier    = P.identifier lexer
integer       = P.integer lexer
stringLiteral = P.stringLiteral lexer
colon         = P.colon lexer
