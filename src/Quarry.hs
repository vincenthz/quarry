module Main where

import System.Exit
import System.Environment
import System.Console.GetOpt

import Control.Monad (forM_, when)
import Control.Monad.Trans
import Control.Applicative ()
import Tools.Quarry
import Data.List

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
        doImport path file = do
            conf   <- initialize False path
            digest <- runQuarry conf $ importFile [] file
            putStrLn (show digest)

cmdSet args =
    case args of
        path:digest:tagPatches -> doSet path (maybe (error "not a valid digest") id $ readDigest digest) tagPatches
        _                      -> usage Import
  where doSet path digest tagPatches = do
            putStrLn (show tagPatches)
            let (addTags, delTags) = foldl readPatch ([], []) tagPatches
            putStrLn ("deleting tags: " ++ show delTags)
            putStrLn ("adding tags: " ++ show addTags)
            conf <- initialize False path
            runQuarry conf $ updateDigest digest addTags delTags

        readPatch (a,d) s
            | "+" `isPrefixOf` s = (drop 1 s : a, d)
            | "-" `isPrefixOf` s = (a, drop 1 s : d)
            | otherwise          = (a, d)

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
        doFind path tags = do
            conf   <- initialize False path
            runQuarry conf $ do
                digests <- findDigestWithTags tags
                liftIO $ mapM_ (putStrLn . show) digests

cmdTags args = case args of
                []     -> usage Tags
                path:l -> doTags path l
  where doTags path l = do
            conf <- initialize False path
            runQuarry conf $
                forM_ l $ \s -> do
                    tags <- findTags s
                    mapM_ (liftIO . putStrLn) tags

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
