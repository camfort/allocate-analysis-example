{-# LANGUAGE TemplateHaskell #-}
module Language.Fortran.Analysis.BalancedAllocs where

import Language.Fortran.AST qualified as F

import Prelude hiding ( log )

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.TH
import Effectful.State.Dynamic ( State )
import Effectful.State.Dynamic qualified as State
import System.Exit qualified
import Data.Map qualified as Map
import Data.Map ( Map )

import Data.Foldable ( traverse_ )

{- | Design

  * Check balanced allocs
  * Also try check references (only access an allocatable when allocated)
    * more complex, do as extra

TODO
  * support ALLOCATABLE statement
  * support ALLOCATABLE attribute in declaration
  * support complex dummy, intent cases
    * https://stackoverflow.com/questions/64783329/passing-unallocated-allocatable-variable-as-an-allocatable-dummy-argument
    * https://fortran-lang.org/en/learn/best_practices/allocatable_arrays/
  * onliy supporting plain vars, not fancy record stuff

General notes:
  * ignore 'F.AllocOpt' seems only for fancy debugging
-}

{-
-- ne = non-empty list (or use an emptyable one idgaf)
data AllocStmt ne anno = AllocStmt
  { allocStmtType :: AllocType
  , allocStmtNames :: 

data AllocType = Alloc | Dealloc
  = AllocStmt   (ne (Expression anno))
  | DeallocStmt (ne (Expression
-}

type Ctx = Map F.Name VarState

data VarState
  -- | Declared.
  = VarIsFresh

  -- | Allocatable. Counts number of times allocated.
  | VarIsAllocatable AllocState Int

data AllocState
  = Allocd
  | Unallocd

data Analysis :: Effect where
    EmitErr  :: String -> Analysis m a
    EmitWarn :: String -> String -> Analysis m ()
    AskVar :: F.Name -> Analysis m (Maybe VarState)

    -- | If already declared, you pick what happens. Warn, error, silent.
    DeclareVar :: F.Name -> Analysis m ()

    MakeVarAllocatable :: F.Name -> Analysis m ()
    AllocVar :: F.Name -> Analysis m ()
    DeallocVar :: F.Name -> Analysis m ()

--type instance DispatchOf FileSystem = Dynamic
makeEffect ''Analysis

runAnalysis
    :: (IOE :> es, State Ctx :> es)
    => Eff (Analysis : es) a
    -> Eff es a
runAnalysis = interpret $ \_ -> \case
  EmitErr msg -> err msg
  EmitWarn bad fix -> warn bad fix
  AskVar v -> State.gets $ Map.lookup v
  DeclareVar v -> do
    st <- State.get
    case Map.lookup v st of
      Nothing -> State.put $ Map.insert v VarIsFresh st
      Just{}  -> warn "tried to declare var that already existed" "doing nothing"

  MakeVarAllocatable v -> do
    st <- State.get
    case Map.lookup v st of
      Nothing  -> do
          warn "tried to make undeclared var allocatable" "declaring first"
          State.put $ Map.insert v (VarIsAllocatable Unallocd 0) st
      Just vst ->
        case vst of
          VarIsFresh -> State.put $ Map.insert v (VarIsAllocatable Unallocd 0) st
          VarIsAllocatable{} -> warn "tried to make allocatable var allocatable" "doing nothing"

  AllocVar v -> do
    st <- State.get
    case Map.lookup v st of
      Nothing  -> err "tried to allocate undeclared var"
      Just vst ->
        case vst of
          VarIsFresh -> err "tried to allocate unallocatable var"
          VarIsAllocatable vstAllocState vstAllocCount ->
            case vstAllocState of
              Allocd -> err "tried to allocate allocated var"
              Unallocd -> do
                let vst' = VarIsAllocatable Allocd (vstAllocCount+1)
                State.put $ Map.insert v vst' st

  DeallocVar v -> do
    st <- State.get
    case Map.lookup v st of
      Nothing  -> err "tried to deallocate undeclared var"
      Just vst ->
        case vst of
          VarIsFresh -> err "tried to deallocate unallocatable var"
          VarIsAllocatable vstAllocState vstAllocCount ->
            case vstAllocState of
              Unallocd -> err "tried to deallocate unallocated var"
              Allocd -> do
                let vst' = VarIsAllocatable Unallocd vstAllocCount
                State.put $ Map.insert v vst' st

  where
    warn :: IOE :> es => String -> String -> Eff es ()
    warn bad fix = liftIO $ putStrLn $ "warn: "<>bad<>"; "<>fix
    err :: IOE :> es => String -> Eff es a
    err msg = liftIO $ do
        putStrLn $ "error: "<>msg
        System.Exit.exitWith $ System.Exit.ExitFailure 1

analyse :: Analysis :> es => [F.Block a] -> Eff es ()
analyse = traverse_ go
  where
    go = \case
      F.BlStatement _ _ _ st -> analyseStmt st
      _ -> pure ()

analyseStmt :: Analysis :> es => F.Statement a -> Eff es ()
analyseStmt = \case
  F.StDeclaration _ _ _ attribs decls ->
    traverse_ (declare attribs) (F.aStrip decls)
  F.StAllocatable _ _ decls ->
    traverse_ makeAllocatable (F.aStrip decls)
  F.StAllocate   _ _ _ es _ ->
    -- TODO whats the typespec for. not for decl it seems??
    traverse_ allocate (F.aStrip es)
  F.StDeallocate _ _   es _ ->
    traverse_ deallocate (F.aStrip es)
  st -> analyseStmtAccess st

declare
    :: Analysis :> es
    => Maybe (F.AList F.Attribute a) -> F.Declarator a -> Eff es ()
declare mAttribs d =
    case F.declaratorVariable d of
      F.ExpValue _ _ (F.ValVariable dv) -> do
        declareVar dv
        case mAttribs of
          Nothing      -> pure ()
          Just attribs ->
            if   attribListIncludesAllocatable (F.aStrip attribs)
            then makeVarAllocatable dv
            else pure ()
      _ -> emitWarn "bad declarator form" "ignoring"

attribListIncludesAllocatable :: [F.Attribute a] -> Bool
attribListIncludesAllocatable = \case
  []   -> False
  a:as ->
    case a of F.AttrAllocatable{} -> True; _ -> attribListIncludesAllocatable as

makeAllocatable :: Analysis :> es => F.Declarator a -> Eff es ()
makeAllocatable d =
    case F.declaratorVariable d of
      F.ExpValue _ _ (F.ValVariable dv) -> makeVarAllocatable dv
      _ -> emitWarn "bad declarator form" "ignoring"

analyseStmtAccess :: Analysis :> es => F.Statement a -> Eff es ()
analyseStmtAccess _ = todo "try check if an unallocated var is accessed"

todo :: Analysis :> es => String -> Eff es a
todo = emitErr

allocate :: Analysis :> es => F.Expression a -> Eff es ()
allocate = \case
  F.ExpSubscript _ _ (F.ExpValue _ _ (F.ValVariable v)) _dims ->
    allocVar v
  _ -> emitWarn "unsupported ALLOCATE form" "ignoring"

deallocate :: Analysis :> es => F.Expression a -> Eff es ()
deallocate = \case
  F.ExpValue _ _ (F.ValVariable v) ->
    deallocVar v
  _ -> emitWarn "unsupported DEALLOCATE form" "ignoring"
