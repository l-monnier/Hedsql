{-|
Module      : Hedsql/Drivers/PostgreSQL/Constructor.hs
Description : PostgreSQL specific constructors.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

PostgreSQL specific constructors for functions/clauses specific to this vendor.
-}
module Hedsql.Drivers.PostgreSQL.Constructor
    (
      dropTable
    , lateral
    , default_
    )
    where

import Hedsql.Common.DataStructure.Base
import Hedsql.Drivers.PostgreSQL.Parser

import qualified Hedsql.Common.Constructor as Constructor

-- | Create a PostgreSQL DROP TABLE statement.
dropTable ::
       String -- ^ Name of the table. 
    -> DropTable PostgreSQL
dropTable = Constructor.dropTable

-- | Create a sub-query preceded by LATERAL.
lateral :: SelectQuery -> String -> TableReference
lateral select alias =
    LateralTableReference select $ TableReferenceAlias alias []

{-|
DEFAULT instruction when used to insert a DEFAULT value.
For example:
> INSERT INTO films Values (DEFAULT, 'Bananas', 88, '1971-07-13', 'Comedy');
-}
default_ :: SqlValue
default_ = SqlValueDefault