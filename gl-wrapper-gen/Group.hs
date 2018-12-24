{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Group
    ( genGroupDeclaresCode
    , parseGroup
    , writeAll
    , writeGroupDeclaresCode
    ) where

import qualified Data.Text as T (Text, intercalate, unpack)
import qualified Data.Text.Lazy as LT (Text, intercalate)
import qualified Data.Text.Lazy.IO as LT (writeFile)
import Text.Shakespeare.Text (lt)
import qualified Text.XML as XML (Element)
import Text.XML.Lens
import qualified Types

parseGroup :: XML.Element -> Maybe Types.Group
parseGroup elem = do
    name <- elem ^? attr "name"
    let members = elem ^.. el "group" ./ el "enum" . attr "name"
    return $ Types.Group name members

writeAll :: [Types.Group] -> IO ()
writeAll = writeGroupDeclaresCode

writeGroupDeclaresCode :: [Types.Group] -> IO ()
writeGroupDeclaresCode groups = do
    let code = genGroupDeclaresCode groups
        path = "gl-wrapper/GLW/Enums.hs"
    LT.writeFile path code

genGroupDeclaresCode :: [Types.Group] -> LT.Text
genGroupDeclaresCode groups =
    let groupNames = map Types.groupName groups
        groupDeclares = map genGroupDeclare groups
    in [lt|module GLW.Enums
    ( #{T.intercalate "\n    , " groupNames}
    ) where

import qualified Graphics.GL.Internal.Shared as GL
import GLW.Classes

#{LT.intercalate "\n" groupDeclares}|]

genGroupDeclare :: Types.Group -> LT.Text
genGroupDeclare group =
    [lt|data #{groupName} =
    #{LT.intercalate " |\n    " groupMembers'}
    deriving (Show, Read, Eq)

instance GLConstant #{groupName} where
    #{LT.intercalate "\n    " (map toGLConstantBody groupMembers)}

    #{LT.intercalate "\n    " (map fromGLConstantBody groupMembers)}
    fromGLConstant _ = Nothing
|]
    where
    groupName = Types.groupName group
    groupMembers = Types.groupMembers group
    groupMembers' =  map addPrefix groupMembers
    addPrefix groupMember = [lt|#{groupName}_#{groupMember}|]
    toGLConstantBody groupMember =
        [lt|toGLConstant #{groupName}_#{groupMember} = GL.#{groupMember}|]
    fromGLConstantBody groupMember =
        [lt|fromGLConstant a | a == GL.#{groupMember} = Just #{groupName}_#{groupMember}|]
