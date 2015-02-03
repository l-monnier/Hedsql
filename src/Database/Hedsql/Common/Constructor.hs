{-|
Module      : Database/Hedsql/Common/Constructor.hs
Description : SQL queries and statements creation.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructors function.

They provide a flexible and natural way to create
the various SQL data types.

*Pre-requisites

** Deactivate the GHC OverloadedStrings language extension

If you wish to provide parameters as simple strings,
you should turn off the GHC OverloadedStrings language extension.

> LANGUAGE NoOverloadedStrings

This will allow to write queries like this:

>     select "col1"
> /++ from "table1"

instead of:

>     select ("col1"::String)
> /++ from ("table1"::String)

For more explanations related to the why and how, please refer to the
following discussion:
http://haskell.1045720.n5.nabble.com/Proposal-Improving-the-IsString-String
-instance-td5734824.html

** Hiding prelude functions

To stay close to SQL, some functions are using the same name as in the prelude.
If this happens, you can or use a qualitifed name or hide them during the 
import of the Prelude or Hedsql.

> import Prelude hiding (and, or, null)

*Building a query

The idea is to provide a limited set of functions with similar or close
naming from SQL.
The results of those functions can then be composed to form complete
queries.
For example, when using the 'select' function, you will not provide a FROM
clause but only the arguments specific to the 'select' clause which are the
columns.

> select ["col1", "col2"]

The additional FROM clause can be added using the '/++' function of the
'Add' class.
With our previous example, you could add a FROM part as so:
> select ["col1", "col2"] /++ from "Table1"

Thanks to type classes, those functions are polymorphic.
It is therefore possible to pass different type of argument to the same
functions. Let's take a look at the 'select' function:

> select $ column "col1"
> select "col1"
> select [column "col1", column "col2"]
> select ["col1", "col2"]

All the above examples are valid.
We can first see that it is possible to pass a single argument or a list to
the 'select' function.
It is also possible to pass arguments of the type 'Column' - returned by
the 'column' function - or of type String.

*Naming

Most functions have the same name as their SQL functions counterpart.
However, since some words are reserved in Haskell, an underscore is added at
the end in some cases (as does Esqueletto):
- WHERE becomes 'where_'
- AS becomes 'as_'

*Special cases

**ORDER BY

Limit and offset have to be added to the 'OrderBy' and are not part of a
query on their own.
This way, we ensure to have an ORDER BY clause defined when using OFFSET
and LIMT, which is a good practice, because SQL does not guarantee any
order of the result unless explicitly specified.
This means that without an ORDER BY clause, using LIMIT or OFFSET would
result in random results.
-}
module Database.Hedsql.Common.Constructor
    ( module Database.Hedsql.Common.Constructor.Columns
    , module Database.Hedsql.Common.Constructor.Composition
    , module Database.Hedsql.Common.Constructor.Conditions
    , module Database.Hedsql.Common.Constructor.DataManipulation
    , module Database.Hedsql.Common.Constructor.Functions
    , module Database.Hedsql.Common.Constructor.Select
    , module Database.Hedsql.Common.Constructor.Statements
    , module Database.Hedsql.Common.Constructor.Tables
    , module Database.Hedsql.Common.Constructor.TablesManipulation
    , module Database.Hedsql.Common.Constructor.Types
    , module Database.Hedsql.Common.Constructor.Values
) where

import Database.Hedsql.Common.Constructor.Columns
import Database.Hedsql.Common.Constructor.Composition
import Database.Hedsql.Common.Constructor.Conditions
import Database.Hedsql.Common.Constructor.DataManipulation
import Database.Hedsql.Common.Constructor.Functions
import Database.Hedsql.Common.Constructor.Select
import Database.Hedsql.Common.Constructor.Statements
import Database.Hedsql.Common.Constructor.Tables
import Database.Hedsql.Common.Constructor.TablesManipulation
import Database.Hedsql.Common.Constructor.Types
import Database.Hedsql.Common.Constructor.Values
