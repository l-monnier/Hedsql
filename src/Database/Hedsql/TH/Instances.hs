{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

{-|
Module      : Database/Hedsql/TH/Instances.hs
Description : Template haskell instances generator.
Copyright   : (c) Leonard Monnier, 2016
License     : GPL-3
Maintainer  : leonard.monnier@gmail.com
Stability   : experimental
Portability : portable

Generate instances using template haskell.
-}
module Database.Hedsql.TH.Instances
    ( mkSelectionConstr
    , mkSelectionConstrs
    ) where

import Data.List
import Language.Haskell.TH

{-|
Create a 'SelectionConstr' instance for a tuple of size 'n'.
For n=2:
instance SelectionConstr (a1, a2) (Selection [(b1, b2)] db)
    => ( ToColRef a1 (ColRef b1 db)
       , ToColRef a2 (ColRef b2 db)
       ) where
           selection (x1, x2) = Selection [
                 ColRefWrap (colRef x1)
               , ColRefWrap (colRef x2)
               ]
-}
mkSelectionConstr ::
       Int   -- ^ n.
    -> Q Dec
mkSelectionConstr n =
    instanceD (scTQ n "a" "b" "db") (scCxtQ n "a" "b") [pure $ scFunD n "x"]

{-|
Create 'SelectionConstr' instances for tuple up to size 'n' starting at 2.
-}
mkSelectionConstrs ::
       Int     -- ^ n.
    -> Q [Dec]
mkSelectionConstrs n = mapM mkSelectionConstr [2..n]

{-|
Create the constraint head of the SelectionConstr (sc) instance.
For tuples sizes = 2, a = "a" and b = "b":
> SelectionConstr (a1, a2) (Selection [(b1, b2)] db)
-}
scCxtQ ::
      Int    -- ^ Tuples sizes.
   -> String -- ^ a.
   -> String -- ^ b.
   -> Q Type
scCxtQ n a b =
    [t|
        $(nameConT "SelectionConstr")
        ($(pure $ tupleTs n a))
        ( $(nameConT "Selection")
          [$(pure $ tupleTs n b)]
          $(nameVarT "db")
        )
    |]

{-|
Create the type constraints of the SelectionConstr (sc) instance.
For tuple size = 2, a = "a", b = "b" and c = "db":
@
( ToColRef a1 (ColRef b1 db)
, ToColRef a2 (ColRef b2 db)
)
@
-}
scTQ ::
       Int    -- ^ Tuple size.
    -> String -- ^ a.
    -> String -- ^ b.
    -> String -- ^ c.
    -> Q [Pred]
scTQ n a b c =
    mapM (\x -> singleScTQ (a ++ show x) (b ++ show x) c) [1..n]

{-|
Create a single SelectConstr (sc) instance type.
For a = "a", b = "b" and c = "db":
> ToColRef a (ColRef b db)
-}
singleScTQ ::
       String -- ^ a.
    -> String -- ^ b.
    -> String -- ^ c.
    -> Q Type
singleScTQ a b c =
    [t|
        $(nameConT "ToColRef")
        $(nameVarT a)
        ( $(nameConT "ColRef")
          $(nameVarT b)
          $(nameVarT c)
        )
    |]

{-|
Create the function of the SelectionConstr (sc) instance.
For tuple size = 2 and name = "x":
@
selection (x1, x2) = Selection [ColRefWrap (colRef x1), ColRefWrap (colRef x2)]
@
-}
scFunD ::
       Int    -- ^ Tuple size.
    -> String -- ^ Name.
    -> Dec
scFunD n name =
    FunD (mkName "selection") [Clause [tupleP n name] (scBody n name) []]

{-|
Create the selection function of a SelectConstr instance (sc):
For tuple size=2 and name="x":
@
selection (x1, x2) = Selection [ColRefWrap (colRef x1), ColRefWrap (colRef x2)]
@
-}
scBody ::
       Int    -- ^ Tuple size.
    -> String -- ^ name.
    -> Body
scBody n = NormalB . AppE (ConE $ mkName "Selection") . scExprs n

{-|
Create a list of colRefWrap expressions.
For tuple size=2 and name="x":
> [ColRefWrap (colRef x1), ColRefWrap (colRef x2)]]
-}
scExprs ::
       Int    -- ^ Tuple size.
    -> String -- ^ Name.
    -> Exp
scExprs n name = ListE $ map (scExpr . (++) name . show) [1..n]

{-|
Create a ColRefWrap expression for the 'selection' function a 'SelectConstr'
instance (sc).
For name="x":
> ColRefWrap (colRef x)
-}
scExpr :: String -> Exp
scExpr name =
    ConE (mkName "ColRefWrap")
    `AppE` (
        VarE (mkName "colRef") `AppE` VarE (mkName name)
           )

--------------------------------------------------------------------------------
-- Generic functions which could be re-used in other modules
--------------------------------------------------------------------------------

{-|
Create a tuple type such as for tuple size=2 and name = "a":
> (a1, a2)
-}
tupleTs ::
       Int    -- ^ Tuple size.
    -> String -- ^ Name.
    -> Type
tupleTs n a =
    foldl' AppT (TupleT n) $ map (VarT . mkName . (++) a . show) [1..n]

{-|
Create a tuple pattern such as for tuple size n=2 and name="x":
> (x1, x2)
-}
tupleP ::
       Int    -- ^ Tuple size.
    -> String -- ^ Name.
    -> Pat
tupleP n name = TupP $ map (VarP . mkName . (++) name . show) [1..n]

{-|
Create a variable type name which can then be used in a type splice @[t|..|]@.
-}
nameVarT :: String -> Q Type
nameVarT = return . VarT . mkName

{-|
Create a constructor type name which can then be used
in a type splice @[t|..|]@.
-}
nameConT :: String -> Q Type
nameConT = return . ConT . mkName
