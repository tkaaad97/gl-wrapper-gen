{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Newtype
    ( parseNewtype
    , genNewtypeDeclaresCode
    , writeNewtypeDeclaresCode
    ) where

import qualified Data.Text as T (intercalate)
import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Shakespeare.Text (lt)
import qualified Text.XML as XML (Element)
import Text.XML.Lens
import qualified Types

parseNewtype :: XML.Element -> Maybe Types.Newtype
parseNewtype elem' = do
    nname <- elem' ^? attr "name"
    type' <- Types.parsePrimType (elem' ^? el "newtype" ./ el "type" . text)
    return (Types.Newtype nname type')

genNewtypeDeclaresCode :: [Types.Newtype] -> LT.Text
genNewtypeDeclaresCode newtypes =
    let newtypeNames = map Types.newtypeName newtypes
        newtypeDeclares = map genNewtypeDeclare newtypes
    in [lt|module GLW.Types
    ( #{T.intercalate "(..)\n    , " newtypeNames}(..)
    ) where

import qualified Graphics.GL as GL

#{LT.intercalate "\n" newtypeDeclares}|]

genNewtypeDeclare :: Types.Newtype -> LT.Text
genNewtypeDeclare ntype =
    [lt|newtype #{newtypeName} = #{newtypeName} {
    un#{newtypeName} :: #{Types.printPrimType "GL." internalType}
    } deriving (Show, Read, Eq)
|]
    where
    newtypeName = Types.newtypeName ntype
    internalType = Types.newtypeType ntype

writeNewtypeDeclaresCode :: FilePath -> [Types.Newtype] -> IO ()
writeNewtypeDeclaresCode outputPath newtypes =
    let code = genNewtypeDeclaresCode newtypes
        path = outputPath ++ "/GLW/Types.hs"
    in LT.writeFile path code
