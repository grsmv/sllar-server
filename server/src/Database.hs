module Database (create) where

import Database.SQLite
import qualified Paths_sllar_server as Paths

--
-- Helpers for column description
--
m, o :: [Clause]
m = [IsNullable False] -- mandatory
o = [IsNullable True]  -- optional


--
-- List of database tables with description of their
-- columns and properties
-- Output: list of tables with descriptions
--
dataTables :: [(String, [(String, SQLType, [Clause])])]
dataTables = [ ("packages",
               --  column name     column type                additional properties
                 [ ("id",          SQLInt NORMAL False False, [PrimaryKey True])
                 , ("name",        SQLVarChar 200,            m)
                 , ("description", SQLVarChar 1000,           m)
                 , ("author",      SQLVarChar 200,            m)
                 , ("version",     SQLVarChar 10,             m)
                 , ("maintainer",  SQLVarChar 200,            o)
                 , ("license",     SQLVarChar 200,            o)
                 , ("copyright",   SQLVarChar 200,            o)
                 , ("homepage",    SQLVarChar 200,            o)
                 , ("tracker",     SQLVarChar 200,            o)])
             , ("versions",
                 [ ("id",          SQLInt NORMAL False False, [PrimaryKey True])
                 , ("packageId",   SQLInt NORMAL False False, m)
                 , ("version",     SQLVarChar 200,            m)])]


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
