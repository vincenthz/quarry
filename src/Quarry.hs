module Main where

import System.Exit
import System.Environment
import System.Console.GetOpt

import Control.Monad (forM_, when)
import Control.Monad.Trans
import Control.Applicative ((<$>))
import Tools.Quarry
import Data.List
import Data.Maybe

data SubCommand = Init | Import | Set | Get | Find | Tags
    deriving (Show,Eq)
data InitOpt = InitHelp
    deriving (Show,Eq)
data ImportOpt = ImportHelp
    deriving (Show,Eq)
data FindOpt = FindHelp
    deriving (Show,Eq)

usage Init   = error "usage: quarry init <repository-path>"
usage Import = error "usage: quarry import <repository-path> <file>"
usage Set    = error "usage: quarry set <repository-path> <digest> [+/-tag]"
usage Get    = error "usage: quarry get <repository-path> <digest>"
usage Find   = error "usage: quarry find <repository-path> <query>"
usage Tags   = error "usage: quarry tags <repository-path> <tag prefix> ..."

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
        [path,file] -> doImport path file
        _           -> usage Import
  where options =
            [ Option ['h'] ["help"] (NoArg ImportHelp) "show help"
            ]
        hardcodedDataCat = CategoryPersonal
        doImport path file = do
            conf   <- initialize False path
            digest <- runQuarry conf $ importFile hardcodedDataCat [] file
            putStrLn (show digest)

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
        resolveAddTag s = do
            let t = fromTagArg s
            r <- resolveTag t
            case r of
                Just _  -> return r
                Nothing -> case t of
                              Left _ -> return $ Just $ Tag { tagName = s, tagCat = "personal" } 
                              _      -> return r

        readPatch (a,d) s =
            case s of
                '+':toAdd -> (toAdd:a, d)
                '-':toRem -> (a, toRem:d)
                _         -> (a,d)

cmdGet _ =
    undefined

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

cmdTags args = case args of
                []     -> usage Tags
                path:l -> doTags path l
  where doTags path l = do
            conf <- initialize False path
            runQuarry conf $
                forM_ l $ \s -> do
                    tags <- findTags (Just s) Nothing
                    mapM_ (liftIO . putStrLn . show) tags

commands =
    [ ("init",
        ( cmdInit
        , "initialize a repository"
        )
      )
    , ("import",
        ( cmdImport
        , "import a file into quarry"
        )
      )
    , ("set",
        ( cmdSet
        , "set some metadata"
        )
      )
    , ("tags",
        ( cmdTags
        , "list tags"
        )
      )
    , ("get",
        ( cmdGet
        , "get some metadata"
        )
      )
    , ("find",
        ( cmdFind
        , "find contents by query"
        )
      )
    ]

main = do
    args <- getArgs
    case args of
        []          -> error ("expecting one subcommand of: " ++ intercalate ", " (map fst commands))
        cmd:subArgs -> case lookup cmd commands of
                            Nothing            -> error ("unrecognized command: " ++ cmd)
                            Just (fCommand, _) -> fCommand subArgs
