{-# LANGUAGE LambdaCase, FlexibleContexts #-}

module Main (main) where

import           Control.Concurrent
import           Control.Monad
import           Data.Char
import           Data.IORef
import           Data.List
import           Graphics.UI.Gtk
import           System.Random

data Row = Row
  { rName         :: String
  , rSeeders      :: Int
  , rLeechers     :: Int
  , rDlSpeed      :: Int
  , rUpSpeed      :: Int
  , rCompleted    :: Float
  , rListStoreIdx :: Int
  } deriving (Show)

type SortFn = IO ()

data GUI = GUI
  { gListStore :: ListStore Row
  , gSortedCol :: IORef (Maybe (TreeViewColumn, SortFn))
  }

revSort :: SortType -> SortType
revSort SortAscending = SortDescending
revSort SortDescending = SortAscending

modifyListStore :: ListStore a -> ([a] -> [a]) -> IO ()
modifyListStore store m = do
    as <- listStoreToList store
    listStoreClear store
    forM_ (m as) $ listStoreAppend store

sortFn :: Ord a => TreeViewColumn -> GUI -> (Row -> a) -> IO ()
sortFn col gui@(GUI listStore sorted) sel = do
    readIORef sorted >>= \case
      Just (sortedCol, _)
        | sortedCol == col -> do
            sortty <- treeViewColumnGetSortOrder col
            set col [ treeViewColumnSortOrder := revSort sortty ]
            modifyListStore listStore reverse
      _ -> do
        writeIORef sorted $ Just (col, sortFn col gui sel)
        sortty <- treeViewColumnGetSortOrder col
        modifyListStore listStore $ \rows ->
          sortBy
            ((case sortty of
                SortAscending -> id
                _ -> flip) (\r1 r2 -> sel r1 `compare` sel r2)) rows

initRow :: Int -> String -> Row
initRow idx name = Row name 0 0 0 0 40 idx

updater :: GUI -> [Row] -> IO ()
updater gui rows = do
    updateIdx <- randomRIO (0, length rows - 1)
    updateGUI gui updateIdx (\row -> row{rCompleted = rCompleted row + 1})
    threadDelay 10000
    updater gui rows

updateGUI :: GUI -> Int -> (Row -> Row) -> IO ()
updateGUI (GUI listStore _) idx update = do
    row <- listStoreGetValue listStore idx
    listStoreSetValue listStore idx (update row)

main :: IO ()
main = do
    _ <- unsafeInitGUIForThreadedRTS
    win <- windowNew
    _ <- win `on` objectDestroy $ mainQuit

    content <- readFile "rho-torrent.cabal"
    let rows = zipWith initRow [0..] (lines content)

    listStore <- listStoreNew rows

    sorted <- newIORef Nothing
    let gui = GUI listStore sorted

    treeView <- treeViewNewWithModel listStore
    btnLayout <- initButtons treeView listStore

    initNameCol treeView gui
    initCompletedCol treeView gui
    initIntCol treeView gui "Seeders" (show . rSeeders) rSeeders
    initIntCol treeView gui "Leechers" (show . rLeechers) rLeechers
    -- FIXME: Find a nice way to show dl/up speeds

    -- to annoy the user: don't allow any columns to be dropped at the far right
    treeViewSetColumnDragFunction treeView $ Just $ \_ rCol _ -> do
      return (rCol /= Nothing)

    treeViewSetSearchEqualFunc treeView $ Just $ \str (TreeIter _ n _ _) -> do
      row <- listStoreGetValue listStore (fromIntegral n)
      return (map toLower str `isPrefixOf` map toLower (filter isAlphaNum (rName row)))

    vbox <- vBoxNew False 10
    boxPackStart vbox btnLayout PackNatural 0

    swin <- scrolledWindowNew Nothing Nothing
    boxPackStart vbox swin PackGrow 0

    set swin [ containerChild := treeView ]
    set win [ containerChild := vbox ]

    -- FIXME: Make sure things stay sorted
    _ <- forkIO $ updater gui rows

    widgetShowAll win
    mainGUI

initButtons :: TreeView -> ListStore Row -> IO HButtonBox
initButtons treeView listStore = do
    btnBox <- hButtonBoxNew
    buttonBoxSetLayout btnBox ButtonboxStart

    addNewBtn <- buttonNewFromStock stockAdd
    addNewBtn `on` buttonActivated $ putStrLn "showing add window"

    startPauseBtn <- buttonNewFromStock stockMediaPlay
    startPauseBtn `on` buttonActivated $ do
      sel <- treeViewGetSelection treeView
      rows <- treeSelectionGetSelectedRows sel
      -- TreeView should be in SingleSelection mode, and we don't have tree
      -- views, so this should return at most one element.
      case join rows of
        [] -> putStrLn "Nothing selected."
        [i] -> do
          row <- listStoreGetValue listStore i
          putStrLn $ "Toggling " ++ rName row
        _ -> error $ "Unexpected selected: " ++ show rows

    containerAdd btnBox addNewBtn
    containerAdd btnBox startPauseBtn

    return btnBox

setColAttrs :: TreeViewColumn -> String -> IO ()
setColAttrs col colName =
    set col [ treeViewColumnTitle         := colName,
              treeViewColumnReorderable   := True,
              treeViewColumnResizable     := True,
              treeViewColumnClickable     := True,
              treeViewColumnSortIndicator := True ]

-- FIXME: Reduce duplication

initNameCol :: TreeView -> GUI -> IO ()
initNameCol treeView gui@(GUI listStore _) = do
    col <- treeViewColumnNew
    setColAttrs col "Name"
    _ <- treeViewAppendColumn treeView col

    cell <- cellRendererTextNew
    cellLayoutPackStart col cell True

    _ <- onColClicked col (sortFn col gui rName)

    cellLayoutSetAttributes col cell listStore $ \row ->
      [ cellText := rName row ]


initCompletedCol :: TreeView -> GUI -> IO ()
initCompletedCol treeView gui@(GUI listStore _) = do
    col <- treeViewColumnNew
    setColAttrs col "Completed"
    _ <- treeViewAppendColumn treeView col

    _ <- onColClicked col (sortFn col gui rCompleted)

    cell <- cellRendererProgressNew
    cellLayoutPackStart col cell True
    cellLayoutSetAttributes col cell listStore $ \row ->
      [ cellProgressValue := round $ rCompleted row ]

initIntCol :: Ord a => TreeView -> GUI -> String -> (Row -> String) -> (Row -> a) -> IO ()
initIntCol treeView gui@(GUI listStore _) colName showFn sortSel = do
    col <- treeViewColumnNew
    setColAttrs col colName
    _ <- treeViewAppendColumn treeView col

    _ <- onColClicked col (sortFn col gui sortSel)

    cell <- cellRendererTextNew
    cellLayoutPackStart col cell True
    cellLayoutSetAttributes col cell listStore $ \row ->
      [ cellText := showFn row ]
