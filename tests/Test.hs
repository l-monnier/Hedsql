module Main where

import Test.Framework (defaultMain)

import Database.Hedsql.Tests.Connection.SqLiteSimple
import Database.Hedsql.Tests.Delete
import Database.Hedsql.Tests.Insert
import Database.Hedsql.Tests.Select
import Database.Hedsql.Tests.PrettyPrint
import Database.Hedsql.Tests.TableManipulations
import Database.Hedsql.Tests.Update
import Database.Hedsql.CodeGenerator.Quoter.Tests

-- | Run the tests.
main :: IO()
main = defaultMain
    [ Database.Hedsql.CodeGenerator.Quoter.Tests.tests
    , Database.Hedsql.Tests.Delete.tests
    , Database.Hedsql.Tests.Insert.tests
    , Database.Hedsql.Tests.Select.tests
    , Database.Hedsql.Tests.TableManipulations.tests
    , Database.Hedsql.Tests.Update.tests
    , Database.Hedsql.Tests.PrettyPrint.tests
    , Database.Hedsql.Tests.Connection.SqLiteSimple.tests
    ]
