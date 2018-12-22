{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Group
    ( genEnumsExportCode
    , genGroupDeclaresCode
    , genMemberDeclaresCode
    , parseGroup
    , writeAll
    , writeEnumExportCode
    , writeGroupDeclaresCode
    , writeMemberDeclaresCodes
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
writeAll groups = do
    writeEnumExportCode groups
    writeGroupDeclaresCode groups
    writeMemberDeclaresCodes groups

writeEnumExportCode :: [Types.Group] -> IO ()
writeEnumExportCode groups =
    let groupNames = map Types.groupName groups
        code = genEnumsExportCode groupNames
        path = "gl-wrapper/GLW/Enums.hs"
    in LT.writeFile path code

genEnumsExportCode :: [T.Text] -> LT.Text
genEnumsExportCode groupNames =
    [lt|module GLW.Enums
    ( #{T.intercalate "\n    , " groupNames}
    ) where

import GLW.Internal.Enums
|]

writeGroupDeclaresCode :: [Types.Group] -> IO ()
writeGroupDeclaresCode group = do
    let code = genGroupDeclaresCode group
        path = "gl-wrapper/GLW/Internal/Enums.hs"
    LT.writeFile path code

genGroupDeclaresCode :: [Types.Group] -> LT.Text
genGroupDeclaresCode groups =
    let groupNames = map Types.groupName groups
        groupDeclares = map genGroupDeclare groupNames
    in [lt|module GLW.Internal.Enums
    ( #{T.intercalate "\n    , " groupNames}
    ) where

import qualified Graphics.GL.Types as GL (GLenum)

#{LT.intercalate "\n" groupDeclares}|]

genGroupDeclare :: T.Text -> LT.Text
genGroupDeclare groupName = [lt|newtype #{groupName} = #{groupName}
    { un#{groupName} :: GL.GLenum
    } deriving (Show, Read, Eq)
|]

writeMemberDeclaresCodes :: [Types.Group] -> IO ()
writeMemberDeclaresCodes = mapM_ writeMemberDeclaresCode

writeMemberDeclaresCode :: Types.Group -> IO ()
writeMemberDeclaresCode group =
    let groupName = Types.groupName group
        groupMembers = Types.groupMembers group
        code = genMemberDeclaresCode groupName groupMembers
        path = "gl-wrapper/GLW/Enums/" ++ T.unpack groupName ++ ".hs"
    in LT.writeFile path code

genMemberDeclaresCode :: T.Text -> [T.Text] -> LT.Text
genMemberDeclaresCode groupName groupMembers =
    let memberDeclares = map (genMemberDeclare groupName) groupMembers
    in [lt|module GLW.Enums.#{groupName}
    ( #{groupName}
    , #{T.intercalate "\n    , " groupMembers}
    ) where

import qualified Graphics.GL.Internal.Shared as GL (#{T.intercalate ", " groupMembers})
import GLW.Internal.Enums (#{groupName}(..))

#{LT.intercalate "\n" memberDeclares}|]

genMemberDeclare :: T.Text -> T.Text -> LT.Text
genMemberDeclare group member = [lt|#{member} :: #{group}
#{member} = #{group} GL.#{member}
|]
