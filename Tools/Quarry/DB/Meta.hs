module Tools.Quarry.DB.Meta
    where

import Tools.Quarry.Types
import Tools.Quarry.Monad
import Tools.Quarry.DBHelper
import Tools.Quarry.DB.Types
import Tools.Quarry.DB.Utils
import Control.Applicative
import Control.Monad.Trans
import Database.HDBC

-- | Create a specific tag
--
-- will check if tag already exists
dbCreateTag :: Tag -> QuarryM KeyTag
dbCreateTag tag = do
    mt <- dbFindTag tag
    case mt of
        Nothing -> do cat <- dbFindCategory (tagCat tag)
                      case cat of
                            Nothing -> undefined
                            Just c  -> withDB $ \conn -> liftIO $ do
                                        stmt <- prepare conn queryInsertTag
                                        KeyTag <$> insertAndGetID conn stmt [toSql (tagName tag), toSql $ getPrimaryKey c]
        Just t  -> return t
  where queryInsertTag = "INSERT INTO tag (name, category) VALUES (?, ?)"

-- | Create a specific category
--
-- will check if category already exists.
-- will always create an abstract category
dbCreateCategory :: Category -> QuarryM KeyCategory
dbCreateCategory cat = do
    mt <- dbFindCategory cat
    case mt of
        Nothing -> withDB $ \conn -> liftIO $ do
                        stmt <- prepare conn queryInsertCat
                        KeyCategory <$> insertAndGetID conn stmt [toSql cat]
        Just t  -> return t
  where queryInsertCat = "INSERT INTO cat (name, abstract) VALUES (?, 1)"

-- | Try to find the key associated to a Tag
--
-- fix SQL escape
dbFindTag :: Tag -> QuarryM (Maybe KeyTag)
dbFindTag tag = do
    mcat <- dbFindCategory (tagCat tag)
    case mcat of
        Nothing  -> return Nothing
        Just cat -> withDB $ \conn -> do
            r <- liftIO $ quickQuery conn ("SELECT id FROM tag WHERE name='" ++ tagName tag ++ "' AND category=" ++ show (getPrimaryKey cat)) []
            case r of
                []      -> return Nothing
                [[uid]] -> return $ Just $ KeyTag $ fromSql uid
                _       -> error ("dbFindTag: " ++ show tag ++ " unexpected sql output format " ++ show r)

dbFindCategory :: Category -> QuarryM (Maybe KeyCategory)
dbFindCategory cat = withDB $ \conn -> do
    r <- liftIO $ quickQuery conn ("SELECT id FROM category WHERE name='" ++ cat ++ "'") []
    case r of
        []      -> return Nothing
        [[uid]] -> return $ Just $ KeyCategory $ fromSql uid
        _       -> error ("dbFindCategory: " ++ show cat ++ " unexpected sql output format " ++ show r)

dbGetCategories :: QuarryM [(KeyCategory, (Category, Bool))]
dbGetCategories = withDB $ \conn -> liftIO $ getTableMap conn tableCategory KeyCategory toVal
  where toVal [name,abstr] = (fromSql name, fromSql abstr)
        toVal r            = error $ "unexpected value in category table: " ++ show r
