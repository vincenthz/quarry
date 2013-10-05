module Main where

import System.Exit
import System.Environment
import System.Console.GetOpt

import Control.Monad (forM_, when, unless)
import Control.Monad.Trans
import Control.Applicative ((<$>))
import Tools.Quarry
import Data.Word
import Data.List
import Data.Maybe
import System.IO
import System.Process

data SubCommand = Init | Import | Set | Get | Find | Tags | Cats | Info | Exist | View
    deriving (Show,Eq)
data InitOpt = InitHelp
    deriving (Show,Eq)
data ImportOpt = ImportHelp | ImportTy ImportType | ImportDate String | ImportTag String | ImportFilename String
    deriving (Show,Eq)
data FindOpt = FindHelp
    deriving (Show,Eq)
data TagOpt = TagHelp | TagCategory String
    deriving (Show,Eq)
data CatsOpt = CatsHelp
    deriving (Show,Eq)
data ExistOpt = ExistHelp | ExistMissing | ExistAlready
    deriving (Show,Eq)
data ViewOpt = ViewHelp
    deriving (Show,Eq)

usage Init   = error "usage: quarry init <repository-path>"
usage Import = error "usage: quarry import <repository-path> <file>"
usage Set    = error "usage: quarry set <repository-path> <digest> [+/-tag]"
usage Get    = error "usage: quarry get <repository-path> <digest>"
usage Find   = error "usage: quarry find <repository-path> <query>"
usage Tags   = error "usage: quarry tags [--category <category>] <repository-path> <tag prefix> ..."
usage Cats   = error "usage: quarry cats <add|list> <repository-path> <cat prefix> ..."
usage Exist  = error "usage: quarry exists <repository-path> <file> ..."
usage Info   = error "usage: quarry info <repository-path>"
usage View   = error "usage: quarry view <repository-path> <digest>"

reportOptError errOpts
    | null errOpts = return ()
    | otherwise    = mapM_ (putStrLn . ("parseError: " ++)) errOpts >> exitFailure

fromTagArg :: String -> Either TagName Tag
fromTagArg s = case break (== ':') s of
    (_,"")      -> Left s
    (_  ,[':']) -> error "empty tag, expecting 'category:tag', got 'category:'"
    (cat,':':t) -> Right $ Tag { tagName = t, tagCat = cat }
    _           -> error "impossible with break"

tag s = maybe (error $ "cannot resolve " ++ s) id <$> resolveTag (fromTagArg s)

-- create a tag object, automatically filling category to personal if cannot be found.
resolveAddTag s = do
    let t = fromTagArg s
    r <- resolveTag t
    case r of
        Just _  -> return r
        Nothing -> case t of
                      Left _ -> return $ Just $ Tag { tagName = s, tagCat = "personal" } 
                      _      -> return r


cmdInit args = do
    let (optArgs, nonOpts, errOpts) = getOpt Permute options args
    when (InitHelp `elem` optArgs) $ do usage Init >> exitSuccess
    reportOptError errOpts
    case nonOpts of
        [path] -> initialize True path >> return ()
        _      -> usage Init
  where options =
            [ Option ['h'] ["help"] (NoArg InitHelp) "show help"
            ]

-- | import a file
cmdImport args = do
    let (optArgs, nonOpts, errOpts) = getOpt Permute options args
    when (ImportHelp `elem` optArgs) $ do usage Import >> exitSuccess
    reportOptError errOpts
    case nonOpts of
        [path,file] -> doImport optArgs path file
        _           -> usage Import
  where options =
            [ Option ['h'] ["help"] (NoArg ImportHelp) "show help"
            , Option ['s'] ["symlink"] (NoArg (ImportTy ImportSymlink)) "use a symlink to import into the hashfs"
            , Option [] ["hardlink"] (NoArg (ImportTy ImportHardlink)) "use a hardlink to import into the hashfs"
            , Option ['d'] ["date"] (ReqArg ImportDate "date") "add a date in posix seconds"
            , Option ['t'] ["tag"] (ReqArg ImportTag "tag") "add a tag"
            , Option ['f'] ["filename"] (ReqArg ImportFilename "filename") "override the filename"
            ]
        hardcodedDataCat = CategoryPersonal
        doImport optArgs path file = do
            let (date,tags,mFilename,ity) = foldl (\acc@(d,accTags,accFile,t) f -> case f of
                                                        ImportTy ty   -> (d,accTags,accFile,ty)
                                                        ImportDate da -> (read da,accTags,accFile,t)
                                                        ImportTag ta  -> (d,ta:accTags,accFile,t)
                                                        ImportFilename fi -> (d,accTags,Just fi,t)
                                                        _             -> acc) (0 :: Word64, [], Nothing, ImportCopy) optArgs
            let mDate = case date of
                            0 -> Nothing
                            _ -> Just $ fromIntegral date
            conf   <- initialize False path
            (digest,isNew) <- runQuarry conf $ do
                addTags <- catMaybes <$> mapM resolveAddTag tags
                importFile ity hardcodedDataCat mDate (("/old/" ++) `fmap` mFilename) addTags file
            if isNew
                then putStrLn (show digest)
                else hPutStrLn stderr (show digest ++ " already existing ")

cmdSet args =
    case args of
        path:digest:tagPatches -> doSet path (maybe (error "not a valid digest") id $ readDigest digest) tagPatches
        _                      -> usage Import
  where doSet path digest tagPatches = do
            let (addTagArgs, delTagArgs) = foldl readPatch ([], []) tagPatches
            conf <- initialize False path
            runQuarry conf $ do
--tag s = maybe (error $ "cannot resolve " ++ s) id <$> resolveTag (fromTagArg s)
                addTags <- catMaybes <$> mapM resolveAddTag addTagArgs
                delTags <- catMaybes <$> mapM (resolveTag . fromTagArg) delTagArgs
                liftIO $ putStrLn ("deleting tags: " ++ show delTags)
                liftIO $ putStrLn ("adding tags: " ++ show addTags)
                updateDigest digest addTags delTags
        readPatch (a,d) s =
            case s of
                '+':toAdd -> (toAdd:a, d)
                '-':toRem -> (a, toRem:d)
                _         -> (a,d)

cmdGet _ =
    error ("get not implemented")

cmdFind args = do
    let (optArgs, nonOpts, errOpts) = getOpt Permute options args
    when (FindHelp `elem` optArgs) $ do usage Find >> exitSuccess
    reportOptError errOpts
    case nonOpts of
        path:tags -> doFind path tags
        _         -> usage Find
  where options =
            [ Option ['h'] ["help"] (NoArg FindHelp) "show help"
            ]
        doFind path tagArgs = do
            conf <- initialize False path
            runQuarry conf $ do
                tags    <- mapM tag tagArgs
                digests <- findDigestWithTags tags
                liftIO $ mapM_ (putStrLn . show) digests

cmdTags args = do
    let (optArgs, nonOpts, errOpts) = getOpt Permute options args
    when (TagHelp `elem` optArgs) $ do usage Tags >> exitSuccess
    reportOptError errOpts
    case nonOpts of
        []     -> usage Tags
        path:l -> doTags optArgs path l
  where options =
            [ Option ['h'] ["help"] (NoArg TagHelp) "show help"
            , Option ['c'] ["category"] (ReqArg TagCategory "category") "restrict tag search to a specific category"
            ]
        doTags optArgs path l = do
            let cat = foldl (\acc f -> case f of
                                TagCategory c -> Just c
                                _             -> acc) Nothing optArgs
            conf <- initialize False path
            runQuarry conf $
                forM_ l $ \s -> do
                    tags <- findTags (Just s) cat
                    mapM_ (liftIO . putStrLn . show) tags

cmdExist args = do
    let (optArgs, nonOpts, errOpts) = getOpt Permute options args
    when (ExistHelp `elem` optArgs) $ do usage Exist >> exitSuccess
    when (ExistMissing `elem` optArgs &&
          ExistAlready `elem` optArgs) $
        putStrLn "cannot specify missing and exist at the same time" >> exitFailure
    reportOptError errOpts
    case nonOpts of
        path:f1:fs -> doExist optArgs path (f1:fs)
        _          -> usage Exist
  where options =
            [ Option ['h'] ["help"] (NoArg ExistHelp) "show help"
            , Option ['m'] ["missing"] (NoArg ExistMissing) "show only missing file from the store"
            , Option ['e'] ["exist"] (NoArg ExistAlready) "show only already existing file in the store"
            ]
        doExist optArgs path filenames = do
            conf <- initialize False path
            runQuarry conf $
                mapM_ (check optArgs) filenames
        check optArgs filename = do
            d <- computeDigest filename
            x <- exist d
            liftIO $ do
                case (ExistAlready `elem` optArgs, ExistMissing `elem` optArgs) of
                    (True, True)   -> error "impossible"
                    (False, True)  -> when x $ putStrLn filename 
                    (True, False)  -> unless x $ putStrLn filename 
                    (False, False) ->
                        if x
                            then putStrLn (filename ++ " " ++ show d)
                            else putStrLn (filename ++ " MISSING")

cmdCats args = do
    let (optArgs, nonOpts, errOpts) = getOpt Permute options args
    when (CatsHelp `elem` optArgs) $ do usage Cats >> exitSuccess
    reportOptError errOpts
    case nonOpts of
        []         -> usage Cats
        "add":path:name:[] -> doCatAdd optArgs path name
        "list":path:_      -> doCats optArgs path
        _:_                -> error ("error unknown sub command " ++ show nonOpts ++ " in cats: expected list or add")
  where options =
            [ Option ['h'] ["help"] (NoArg CatsHelp) "show help"
            ]
        doCatAdd _ path name = do
            conf <- initialize False path
            runQuarry conf $ addCategory name
        doCats _ path = do
            conf <- initialize False path
            runQuarry conf $ do
                cats <- getCategoryTable
                mapM_ (\(_,(c,a)) -> liftIO $ putStrLn (c ++ " (abstract=" ++ show a ++ ")")) cats

cmdInfo args = do
    case args of
        path:[] -> doInfo path
        _       -> usage Import
  where doInfo path = do
            conf <- initialize False path
            info <- runQuarry conf $ getInfo
            putStrLn ("files      : " ++ show (infoNFile info))
            putStrLn ("tags       : " ++ show (infoNTag info))
            putStrLn ("categories : " ++ show (infoNCategory info))

cmdView args = do
    case args of
        path:digests -> doView path digests
        _            -> usage View
  where doView path digests_ = do
            let digests = catMaybes $ map readDigest digests_
            conf <- initialize False path
            l    <- runQuarry conf $ mapM getPathAndType $ digests
            forM_ l $ \(p, ty) -> do
                case ty of
                    QuarryTypeImage -> do ec <- rawSystem "eog" [p]
                                          unless (ec == ExitSuccess) $ exitWith ec
                    _               -> putStrLn (show ty ++ " " ++ show p)
        getPathAndType digest = do
            path <- getDigestPath digest
            ty   <- getQuarryFileType path
            return (path, ty)

commands =
    [ ("init"  , (cmdInit, "initialize a repository"))
    , ("import", (cmdImport, "import a file into quarry"))
    , ("set"   , (cmdSet, "set some metadata"))
    , ("tags"  , (cmdTags, "list tags"))
    , ("cats"  , (cmdCats, "list categories"))
    , ("get"   , (cmdGet, "get some metadata"))
    , ("find"  , (cmdFind, "find contents by query"))
    , ("exist" , (cmdExist, "check if some file already exist"))
    , ("info"  , (cmdInfo, "get quarry general state information"))
    , ("view"  , (cmdView, "use a viewer on digest"))
    ]

main = do
    args <- getArgs
    case args of
        []          -> error ("expecting one subcommand of: " ++ intercalate ", " (map fst commands))
        cmd:subArgs -> case lookup cmd commands of
                            Nothing            -> error ("unrecognized command: " ++ cmd)
                            Just (fCommand, _) -> fCommand subArgs
