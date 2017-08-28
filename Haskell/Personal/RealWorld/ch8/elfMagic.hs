-- qualified lets us alias the import, because we have some of the same functions
-- Prelude has, e.g. take
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy as L1

-- this function checks if a string content is in an ELF object file : used by Unix-like systems
-- we have to check the first four bytes of the file - they are a sort of identifier
hasElfMagic :: L.ByteString -> Bool
hasElfMagic content = L1.take 4 content == elfMagic
  where elfMagic = L1.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
  content <- L1.readFile path
  return $ hasElfMagic content

readInteger :: L.ByteString -> Maybe (Int, L.ByteString)
readInteger = L.readInt
