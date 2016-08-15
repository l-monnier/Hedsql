{-# LANGUAGE GADTs #-}
{-|
Module      : Database/Hedsql/Commun/Grammar
Description : SQL Grammar
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Description of the grammar to build SQL statements.
-}
module Database.Hedsql.Common.Grammar
    ( -- * Select
      SelectStmt(..)
    , SelectFromStmt(..)
    , SelectWhereStmt(..)
    , SelectGroupByStmt(..)
    , SelectHavingStmt(..)
    , SelectOrderByStmt(..)
    , SelectLimitStmt(..)
    , SelectOffsetStmt(..)

      -- * Insert
    , InsertFromStmt(..)
    , InsertReturningStmt(..)

      -- * Update
    , UpdateSetStmt(..)
    , UpdateWhereStmt(..)
    , UpdateReturningStmt(..)

      -- * Delete
    , DeleteFromStmt(..)
    , DeleteWhereStmt(..)
    , DeleteReturningStmt(..)
    ) where

import Database.Hedsql.Common.AST

--------------------------------------------------------------------------------
-- SELECT
--------------------------------------------------------------------------------

data SelectStmt colType dbVendor =
      SelectSingleStmt (SelectQ colType dbVendor)
    | SelectCombinedStmt (Combination dbVendor) [Select colType dbVendor]

data SelectFromStmt colType dbVendor =
    SelectFromStmt (From dbVendor) (SelectStmt colType dbVendor)

data SelectWhereStmt colType dbVendor =
    SelectWhereStmt (Where dbVendor) (SelectFromStmt colType dbVendor)

data SelectGroupByStmt colType dbVendor =
      SelectFromGroupByStmt
          (GroupBy dbVendor)
          (SelectFromStmt colType dbVendor)
    | SelectWhereGroupByStmt
          (GroupBy dbVendor)
          (SelectWhereStmt colType dbVendor)

{-|
The choice has been made to forbid use of HAVING without a previously defined
GROUP BY clause eventhough some database vendors accept such syntax.
-}
data SelectHavingStmt colType dbVendor =
    SelectHavingStmt (Having dbVendor) (SelectGroupByStmt colType dbVendor)

data SelectOrderByStmt colType dbVendor =
      SelectFromOrderByStmt
          (OrderBy dbVendor)
          (SelectFromStmt colType dbVendor)
    | SelectWhereOrderByStmt
          (OrderBy dbVendor)
          (SelectWhereStmt colType dbVendor)
    | SelectGroupByOrderByStmt
          (OrderBy dbVendor)
          (SelectGroupByStmt colType dbVendor)
    | SelectHavingOrderByStmt
          (OrderBy dbVendor)
          (SelectHavingStmt colType dbVendor)

{-|
An ORDER BY clause is requested before a LIMIT since the order of the rows
in the returned result is otherwise not guaranteed.
-}
data SelectLimitStmt colType dbVendor =
    SelectLimitStmt (Limit dbVendor) (SelectOrderByStmt colType dbVendor)

{-|
A LIMIT clause is requested before an OFFSET.
-}
data SelectOffsetStmt colType dbVendor =
    SelectOffsetStmt (Offset dbVendor) (SelectLimitStmt colType dbVendor)

--------------------------------------------------------------------------------
-- INSERT
--------------------------------------------------------------------------------

data InsertFromStmt dbVendor =
    InsertFromStmt (Table dbVendor) [Assignment dbVendor]

data InsertReturningStmt colType dbVendor =
    InsertReturningStmt (Returning colType dbVendor) (InsertFromStmt dbVendor)

--------------------------------------------------------------------------------
-- UPDATE
--------------------------------------------------------------------------------

data UpdateSetStmt dbVendor =
    UpdateSetStmt (Table dbVendor) [Assignment dbVendor]

data UpdateWhereStmt dbVendor =
    UpdateWhereStmt (Where dbVendor) (UpdateSetStmt dbVendor)

data UpdateReturningStmt colType dbVendor =
    UpdateReturningStmt (Returning colType dbVendor) (UpdateWhereStmt dbVendor)

--------------------------------------------------------------------------------
-- DELETE
--------------------------------------------------------------------------------

data DeleteFromStmt dbVendor =
    DeleteFromStmt (Table dbVendor)

data DeleteWhereStmt dbVendor =
    DeleteWhereStmt (Where dbVendor) (DeleteFromStmt dbVendor)

data DeleteReturningStmt colType dbVendor =
      DeleteFromReturningStmt
          (Returning colType dbVendor)
          (DeleteFromStmt dbVendor)
    | DeleteWhereReturningStmt
          (Returning colType dbVendor)
          (DeleteWhereStmt dbVendor)
