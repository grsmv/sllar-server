-- Bridge to SQLite database, which is used for storing
-- information about packages. Also can create new database.
--     http://www.sqlite.org/foreignkeys.html
-- NB: `PRAGMA foreign_keys = ON` for enabling foreign keys support
-- TODO: add `uploadDate` for packages and versions

module Database (create) where

import Database.SQLite
import qualified Paths_sllar_server as Paths


--
-- List of database tables with description of their
-- columns and properties
-- Output: list of tables with descriptions
--
dataTables :: [(String, [(String, SQLType, [Clause])])]
dataTables =
    [ ("packages",
        --  column name     column type                additional properties
          [ ("id",          SQLInt NORMAL False False, [PrimaryKey True, Unique])
          , ("name",        SQLVarChar 100,            m)
          , ("description", SQLVarChar 1000,           m)
          , ("author",      SQLVarChar 100,            m)
          , ("maintainer",  SQLVarChar 100,            o)
          , ("license",     SQLVarChar 100,            o)
          , ("copyright",   SQLVarChar 100,            o)
          , ("homepage",    SQLVarChar 100,            o)
          , ("tracker",     SQLVarChar 100,            o)])
      , ("versions",
          [ ("id",          SQLInt NORMAL False False, [PrimaryKey True, Unique])
          , ("package_id",  SQLInt NORMAL False False, [ForeignKey "packages" ["id"] [OnUpdate Cascade, OnDelete Cascade] Nothing])
          , ("version",     SQLVarChar 10,             m)
          , ("uploaded_at", SQLDateTime DATETIME,      m)])]
      where (m, o) = ([IsNullable False], [IsNullable True])


--
-- Creating sqlite database from scratch
--
create :: IO ()
create = do
    handle <- Paths.getDataFileName "database.sqlite" >>= openConnection
    mapM_ (\(name, columns) ->
        defineTable handle
          VirtualTable
            { tabName = name
            , tabColumns = map (\(n, t, c) -> Column n t c) columns
            , tabConstraints = []
            , tabUsing = "FTS3" }
      ) dataTables
    closeConnection handle
    return ()
