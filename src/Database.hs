module Database where

import qualified Database.SQLite.Simple as SQL
import Data.Foldable (for_)
import Control.Monad (unless)

migrations :: [SQL.Query]
migrations =
  [ "CREATE TABLE IF NOT EXISTS cache (link TEXT PRIMARY KEY, content TEXT, time TIMESTAMP)"
  ]

runMigrations :: SQL.Connection -> IO ()
runMigrations conn = do
  SQL.execute_ conn baseMigration
  for_ indexedMigrations $ \(version, query) -> do
    exists <- doesExist <$> SQL.query conn "select version from migrations where version = ?" (SQL.Only version)
    unless exists $ SQL.execute_ conn query
  where
    doesExist :: [SQL.Only Int] -> Bool
    doesExist [] = False
    doesExist _ = True
    baseMigration :: SQL.Query
    baseMigration = "create table if not exists migrations (version INTEGER)"
    indexedMigrations :: [(Int, SQL.Query)]
    indexedMigrations = zip [1..] migrations
