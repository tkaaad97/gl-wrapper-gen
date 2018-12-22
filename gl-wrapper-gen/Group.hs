{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Group
    ( genGroupCode
    , parseGroup
    , writeGroupCode
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

writeGroupCode :: Types.Group -> IO ()
writeGroupCode group = do
    let code = genGroupCode group
        groupName = Types.groupName group
        path = "gl-wrapper/GLW/Internal/Enums/" ++ T.unpack groupName ++ ".hs"
    LT.writeFile path code

genGroupCode :: Types.Group -> LT.Text
genGroupCode group =
    let groupName = Types.groupName group
        groupMembers = Types.groupMembers group
        memberDeclares = map (genMemberDeclare groupName) groupMembers
    in [lt|module GLW.Internal.Enums.#{groupName}
    ( #{groupName}(..)
    , #{T.intercalate "\n    , " groupMembers}
    ) where

import qualified Graphics.GL.Internal.Shared as GL (#{T.intercalate ", " groupMembers})
import qualified Graphics.GL.Types as GL (GLenum)

newtype #{groupName} = #{groupName}
    { un#{groupName} :: GL.GLenum
    } deriving (Show, Read, Eq)

#{LT.intercalate "\n" memberDeclares}
|]

genMemberDeclare :: T.Text -> T.Text -> LT.Text
genMemberDeclare group member = [lt|#{member} :: #{group}
#{member} = #{group} GL.#{member}
|]
