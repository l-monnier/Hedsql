{-# LANGUAGE FlexibleContexts #-} 

{-|
Module      : Database/Hedsql/Drivers/PostgreSQL/Constructor.hs
Description : PostgreSQL specific constructors.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

PostgreSQL specific constructors for functions/clauses specific to this vendor.
-}
module Database.Hedsql.Drivers.PostgreSQL.Constructor
    ( default_
    , lateral
    , selectDistinctOn
    ) where
    
import Database.Hedsql.Common.Constructor.Select
import Database.Hedsql.Common.DataStructure
import Database.Hedsql.Drivers.PostgreSQL.Driver

import Control.Lens (set)

-- Public.

{-|
DEFAULT instruction when used to insert a DEFAULT value.
For example:
> INSERT INTO films Values (DEFAULT, 'Bananas', 88, '1971-07-13', 'Comedy');
-}
default_ :: Value a PostgreSQL
default_ = DefaultVal

-- | Create a sub-query preceded by LATERAL.
lateral ::
       SelectWrap PostgreSQL -- ^ Select query of the lateral clause.
    -> String                -- ^ Alias of the lateral clause.
    -> TableRef PostgreSQL   -- ^ Lateral table reference.
lateral s a = LateralTableRef s $ TableRefAs a []

-- | Create a SELECT DISTINCT ON query.
selectDistinctOn ::
       SelectConstr a (Select c PostgreSQL)
    => [ColRefWrap PostgreSQL]              -- ^ Distinct expression.
    -> a                                    -- ^ Select clause.
    -> Select c PostgreSQL                  -- ^ Select query.
selectDistinctOn distinctExpr =
    set (selectBody . selectType) (Just $ DistinctOn distinctExpr) . select 