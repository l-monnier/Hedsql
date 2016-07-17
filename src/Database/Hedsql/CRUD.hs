{-# LANGUAGE FlexibleContexts #-}

{-|
Module      : Database/Hedsql/CRUD.hs
Description : CRUD pre-built queries.
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Pre-built generic CRUD SQL statements which can then be parsed to specific
backends.

==Note

Nothing prevents you from using one of these statements as a basis to build a
more complex one. For example if you wish to add a LIMIT clause to 'selectAll'
you could do the following:

@
selectAllLimit table columns maxResults = do
    selectAll table columns
    limit maxResults
@
-}
module Database.Hedsql.CRUD
    ( selectAll
    , selectOne
    , insertOne
    , updateOne
    , deleteOne
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql
import Database.Hedsql.Ext

--------------------------------------------------------------------------------
-- PRIVATE
--------------------------------------------------------------------------------

-- | Assign a placeholder linked to each column.
assignPlaceholders ::
       (ToCol k (Column colType dbVendor))
    => [k] -- ^ Name of the columns.
    -> [Assignment dbVendor]
assignPlaceholders = map (\k -> assign k (/?))

--------------------------------------------------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

{-|
Select query returning all the rows of the provided table(s).
The generated SQL code will be of kind:
> SELECT "Column1", "Column2"
> FROM "Table"
-}
selectAll ::
       ( ToList table [tables], ToTableRef tables (TableRef dbVendor)
       , SelectConstr columns (Query [[Undefined]] dbVendor)
       )
    => table   -- ^ Table(s) or name of the table(s).
    -> columns -- ^ Column(s) or name of the column(s).
    -> Query [[Undefined]] dbVendor
selectAll tb columns = do
    select columns
    from tb

{-|
Select query with a placedholder for the expected matching key.
The generated SQL code will be of kind:
> SELECT "Column1", "Column2"
> FROM "Table"
> WHERE "key" = ?
-}
selectOne ::
       ( ToList table [tables], ToTableRef tables (TableRef dbVendor)
       , SelectConstr columns (Query [[Undefined]] dbVendor)
       , ToColRef key (ColRef keyType dbVendor)
       )
    => table   -- ^ Table(s) or names of the table(s).
    -> columns -- ^ Columns or names of the columns.
    -> key     -- ^ Primary key which has to be matched by the placeholder.
    -> Query [[Undefined]] dbVendor
selectOne tb columns key = do
    selectAll tb columns
    where_ $ key /== (/?)

{-|
Generic insert statement using placeholders.
The generated SQL code will be of kind:
> INSERT INTO "Table"
> ("Column1", "Column2")
> VALUES (?, ?)
-}
insertOne ::
       ( ToTable table (Table dbVendor)
       , ToCol column (Column colType dbVendor)
       )
    => table    -- ^ Table or name of the table.
    -> [column] -- ^ Columns or names of the columns.
    -> InsertStmt dbVendor
insertOne tb values =
    insert tb $ assignPlaceholders values

{-|
Generic update statement using placeholders.
The generated SQL code will be of kind:
> UPDATE "Table"
> SET "Column1" = ?, "Column2" = ?
> WHERE "key" = ?
-}
updateOne ::
       ( ToTable table (Table dbVendor)
       , ToCol column (Column colType dbVendor)
       , ToColRef key (ColRef keyType dbVendor)
       )
    => table    -- ^ Table or name of the table.
    -> [column] -- ^ Columns or names of the columns.
    -> key
    -> UpdateStmt dbVendor
updateOne tb values key = do
    update tb $ assignPlaceholders values
    where_ $ key /== (/?)

{-|
Generic delete statement using a placeholder.
The generated SQL code will be of kind:
> DELETE FROM "Table"
> WHERE "id" = ?
-}
deleteOne ::
       ( ToTable table (Table dbVendor)
       , ToColRef key (ColRef keyType dbVendor)
       )
    => table -- ^ Table or name of the table.
    -> key   -- ^ Primary key which has to be matched by the placeholder.
    -> DeleteStmt dbVendor
deleteOne tb key = do
    deleteFrom tb
    where_ $ key /== (/?)
