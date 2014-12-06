{-|
Module      : Hedsql/Common/Constructor.hs
Description : SQL queries and statements creation.
Copyright   : (c) Leonard Monnier, 2014
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Constructors function.

They provide an easier, more flexible and shorter way to create
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

> select $ "col1"::String
> from $ "table1"::String

For more explanations related to the why and how, please refer to the
following discussion:
http://haskell.1045720.n5.nabble.com/Proposal-Improving-the-IsString-String
-instance-td5734824.html

** Hiding prelude "and" function

Since this constructor class also use a function named "and" as the Prelude
package, it is recommended to hide the "and" function of the Prelude in the
import.

> import Prelude hiding (and)

*Building a query

The idea is to provide a limited set of functions with similar or close
naming from SQL.
Those functions have only a requested minimal set of arguments.
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
However, since some words are reserved in Haskell, the following functions
have been renamed:
- WHERE becomes 'w'
- ALIAS becomes 'as' for table aliaising and 'out' for output aliaising.

*Special cases

**ORDER BY

Limit and offset have to be added to the 'OrderBy' and are not part of a
query on their own.
This way, we ensure to have an ORDER BY clause defined when using OFFSET
and LIMT, which is a good practice, because SQL does not guarantee any
order of the result unless explicitly specified.
This means that without an ORDER BY clause, using limit or offset would
result in random results.
-}
module Hedsql.Common.Constructor
    (
      module Hedsql.Common.Constructor.Columns
    , module Hedsql.Common.Constructor.Composition
    , module Hedsql.Common.Constructor.Conditions
    , module Hedsql.Common.Constructor.DataManipulation
    , module Hedsql.Common.Constructor.Functions
    , module Hedsql.Common.Constructor.Select
    , module Hedsql.Common.Constructor.Tables
    , module Hedsql.Common.Constructor.TablesManipulation
    , module Hedsql.Common.Constructor.Types
    , module Hedsql.Common.Constructor.Values
    ) where
    
import Hedsql.Common.Constructor.Columns
import Hedsql.Common.Constructor.Composition
import Hedsql.Common.Constructor.Conditions
import Hedsql.Common.Constructor.DataManipulation
import Hedsql.Common.Constructor.Functions
import Hedsql.Common.Constructor.Select
import Hedsql.Common.Constructor.Tables
import Hedsql.Common.Constructor.TablesManipulation
import Hedsql.Common.Constructor.Values
import Hedsql.Common.Constructor.Types