import           Control.Monad
import           System.Directory
import           System.FilePath
import           Test.DocTest

main = do
    allFiles <- allHsFiles root
    print allFiles
    doctest allFiles

root = "src/Rho"

allHsFiles :: FilePath -> IO [FilePath]
allHsFiles path = do
    contents <- filter (\f -> head f /= '.') `fmap` getDirectoryContents path
    fs <- forM contents $ \f -> do
      case takeExtension f of
        "" -> do
          isDir <- doesDirectoryExist (path </> f)
          if isDir then allHsFiles (path </> f) else return []
        ".hs" -> return [path </> f]
        _ -> return []
    return $ concat fs
