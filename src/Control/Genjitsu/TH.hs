{-# LANGUAGE TemplateHaskell #-}
module Control.Genjitsu.TH ( genjitsu ) where

import Control.Eff ( Member, SetMember, Eff )
import Control.Eff.Lift ( Lift, lift )
import Data.Char ( toUpper )
import Data.Typeable ( Typeable1 )
import Language.Haskell.TH

-- | Convert the first character of a string to upper case
toUpperHead :: String -> String
toUpperHead "" = ""
toUpperHead (c : x) = toUpper c : x

-- $setup
-- >>> :set -XTemplateHaskell

-- | Is type ... -> IO a ?
--
-- >>> runQ $ fmap isIO [t| IO () |]
-- True
-- >>> runQ $ fmap isIO [t| Maybe Int |]
-- False
-- >>> runQ $ fmap isIO [t| Read a => IO a |]
-- True
-- >>> runQ $ fmap isIO [t| Read a => Maybe a |]
-- False
-- >>> runQ $ fmap isIO [t| Show a => (a -> String) -> IO a -> IO String |]
-- True
-- >>> runQ $ fmap isIO [t| (Int -> IO Int) -> Int -> Int |]
-- False
--
isIO :: Type -> Bool
isIO (ForallT _ _ t) = isIO t
isIO (AppT (ConT io) _) | io == ''IO = True
isIO (AppT _ t) = isIO t
isIO (SigT t _) = isIO t
isIO _ = False

-- | Replace ... -> IO a to ... -> m a
--
-- >>> x <- runQ [t| IO () |]
-- >>> pprint x == pprint (replaceIOByM x (ConT ''IO))
-- True
-- >>> a <- runQ [t| IO (IO ()) |]
-- >>> b <- runQ [t| Maybe (IO ()) |]
-- >>> pprint b == pprint (replaceIOByM a (ConT ''Maybe))
-- True
-- >>> a <- runQ [t| Show a => (a -> String) -> IO a -> IO String |]
-- >>> b <- runQ [t| Show a => (a -> String) -> IO a -> Maybe String |]
-- >>> pprint b == pprint (replaceIOByM a (ConT ''Maybe))
-- True
--
replaceIOByM :: Type -> Type -> Type
replaceIOByM (ForallT a b t) m = ForallT a b (replaceIOByM t m)
replaceIOByM (AppT (ConT io) t) m | io == ''IO = AppT m t
replaceIOByM (AppT a t) m = AppT a (replaceIOByM t m)
replaceIOByM (SigT t a) m = SigT (replaceIOByM t m) a
replaceIOByM x _ = x

-- | The arity of a type.
--
-- >>> runQ $ fmap arity [t| IO () |]
-- 0
-- >>> runQ $ fmap arity [t| Int -> String |]
-- 1
-- >>> runQ $ fmap arity [t| Show a => a -> a -> a |]
-- 2
--
arity :: Type -> Int
arity (ForallT _ _ t) = arity t
arity (AppT (AppT ArrowT _) x) = 1 + arity x
arity _ = 0

-- | Action generator
genjitsu :: Name -> DecsQ
genjitsu function = reify function >>= gen where
    gen (VarI f t _ _) | isIO t = do
      let baseName = toUpperHead $ nameBase f
      let c = mkName ("Allow" ++ baseName)
      let i = mkName ("allow" ++ baseName)
      let gens = [ genClassDecl, genInstanceIODecl, genTypeDecl, genFunctionDecl ]
      decls <- mapM (\g -> g f t c i) gens
      -- runIO $ mapM_ (putStrLn . (++"\n") . pprint) decls
      return decls
    gen _ = error "invalid usage"

-- | Class generator
--
-- >>> let f = 'readLn
-- >>> t <- runQ [t| Read a => IO a |]
-- >>> let c = mkName "AllowReadLn"
-- >>> let i = mkName "allowReadLn"
-- >>> x <- runQ $ genClassDecl f t c i
-- >>> ppr x
-- class AllowReadLn m
--     where allowReadLn :: forall a_0 . GHC.Read.Read a_0 => m a_0
--
genClassDecl :: Name -> Type -> Name -> Name -> DecQ
genClassDecl _ t c i = classD (cxt []) c [PlainTV m] [] [sigD i (return (replaceIOByM t (VarT m)))] where
    m = mkName "m"

-- | IO Instance generator
--
-- >>> let f = 'readLn
-- >>> t <- runQ [t| Read a => IO a |]
-- >>> let c = mkName "AllowReadLn"
-- >>> let i = mkName "allowReadLn"
-- >>> x <- runQ $ genInstanceIODecl f t c i
-- >>> ppr x
-- instance AllowReadLn GHC.Types.IO
--     where allowReadLn = System.IO.readLn
--
genInstanceIODecl :: Name -> Type -> Name -> Name -> DecQ
genInstanceIODecl f _ c i = instanceD (cxt []) (appT (conT c) (conT ''IO)) [valD (varP i) (normalB (varE f)) []]

-- | Teypdecl generator
--
-- >>> let f = 'readLn
-- >>> t <- runQ [t| Read a => IO a |]
-- >>> let c = mkName "AllowReadLn"
-- >>> let i = mkName "allowReadLn"
-- >>> x <- runQ $ genTypeDecl f t c i
-- >>> ppr x
-- readLn :: forall r m . (Data.Typeable.Internal.Typeable1 m,
--                         AllowReadLn m,
--                         Data.OpenUnion1.Member (Control.Eff.Lift.Lift m) r,
--                         Data.OpenUnion1.SetMember Control.Eff.Lift.Lift
--                                                   (Control.Eff.Lift.Lift m)
--                                                   r) =>
--                        forall a_0 . GHC.Read.Read a_0 => Control.Eff.Eff r a_0
--
genTypeDecl :: Name -> Type -> Name -> Name -> DecQ
genTypeDecl f t c _ = sigD n (forallT [PlainTV r,PlainTV m] (cxt [classP ''Typeable1 [varT m],classP c [varT m],classP ''Member [appT (conT ''Lift) (varT m),varT r],classP ''SetMember [conT ''Lift,appT (conT ''Lift) (varT m),varT r]]) (return (replaceIOByM t (AppT (ConT ''Eff) (VarT r))))) where
    n = mkName $ nameBase f
    r = mkName "r"
    m = mkName "m"

-- | Fundecl generator
--
-- >>> let f = 'readLn
-- >>> t <- runQ [t| Read a => IO a |]
-- >>> let c = mkName "AllowReadLn"
-- >>> let i = mkName "allowReadLn"
-- >>> x <- runQ $ genFunctionDecl f t c i
-- >>> ppr x
-- readLn = Control.Eff.Lift.lift allowReadLn
--
genFunctionDecl :: Name -> Type -> Name -> Name -> DecQ
genFunctionDecl f t _ i = funD n [clause (map varP params) (normalB (appE (varE 'lift) (foldl appE (varE i) (map varE params)))) []] where
    n = mkName $ nameBase f
    params = map (mkName . ('x':) . show) [1..arity t]
