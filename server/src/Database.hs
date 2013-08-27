-- Bridge to SQLite database, which is used for storing
-- information about packages. Also can create new database.
--     http://www.sqlite.org/foreignkeys.html
--
-- NB: `PRAGMA foreign_keys = ON` for enabling foreign keys support
-- NB: selecting all packages with most recent versions:
--       select p.id, p.name, p.description, v.version, v.uploaded_at
--         from packages p join versions v on v.package_id = p.id
--         order by v.uploaded_at desc limit 1

module Database (create{-, createPackage-}) where

-- sllar
--import qualified Package

-- system
import Control.Exception (bracket)
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
-- Wrapping each SQLite-related action to a connection acquiring-resource releasing cycle
-- Input: function, that needed to be evaluated between opening and closing connection
--
withConnection :: (SQLiteHandle -> IO ()) -> IO ()
withConnection = bracket (Paths.getDataFileName "database.sqlite" >>= openConnection)
                         closeConnection


--
-- Creating sqlite database from scratch
--
create :: IO ()
create =
    withConnection $ \handle -> do
      mapM_ (\(name, columns) ->
          defineTable handle
            VirtualTable
              { tabName = name
              , tabColumns = map (\(n, t, c) -> Column n t c) columns
              , tabConstraints = []
              , tabUsing = "FTS3" }
        ) dataTables
      return ()


--
--
--
--createPackage :: Package -> IO ()
--createPackage pkg = withConnection $

