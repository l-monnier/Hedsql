{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-|
Module      : Database/Hedsql/Common/Constructor/DataManipulation.hs
Description : DELETE, INSERT and UPDATE SQL statements.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

SQL data manipulation queries constructors for DELETE, INSERT and UPDATE.
-}
module Database.Hedsql.Common.Constructor.DataManipulation
    ( assign
    , deleteFrom
    , insertInto
    , update
    ) where

--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

import Database.Hedsql.Common.AST  
import Database.Hedsql.Common.Constructor.Columns
import Database.Hedsql.Common.Constructor.Tables

------Database.Hedsql.Common.AST--------------------------------------
-- PUBLIC
--------------------------------------------------------------------------------

-- | Create a column/value pair to be used in an UPDATE statement.
assign ::
    (  ToCols    a [Column c d]
    ,  ToColRefs b [ColRef c d]
    )
    => a -- ^ Column or name of the column.
    -> b -- ^ Value for this column. It can also be an expression.
    -> Assignment d
assign a val = Assignment (toCol a) (expr val)

-- | Create a DELETE FROM statement.
deleteFrom ::
       ToTables a [Table b]
    => a        -- ^ Table or name of the table to delete from.
    -> Delete b
deleteFrom t = Delete (table t) Nothing

{-|
Create an INSERT INTO statement.

The values to insert are a list of list of assignments because you may insert
more than one row in the database.
-}
insertInto ::
    ( ToTables a [Table e]
    )
    => a              -- ^ Table or name of the table to insert the data into.
    -> [Assignment e] -- ^ Values to insert.
    -> Insert e
insertInto = Insert . table

-- | Create an UPDATE statement.
update ::
       ToTables a [Table b]
    => a              -- ^ Table to update.
    -> [Assignment b] -- ^ Column/value assignements.
    -> Update b
update t assignments = Update (table t) assignments Nothing