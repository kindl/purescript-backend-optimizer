module PureScript.Transmogrify.Builder
  ( BuildEnv
  , BuildOptions
  , buildModules
  , readCoreFnModule
  , coreFnModulesFromOutput
  ) where

import Prelude

import Control.Parallel (parTraverse)
import Data.Argonaut as Json
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Bifunctor (lmap)
import Data.Compactable (separate)
import Data.Either (Either(..))
import Data.FoldableWithIndex (foldrWithIndex)
import Data.List (List, foldM)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Glob.Basic (expandGlobs)
import Node.Path (FilePath)
import PureScript.Transmogrify.Analysis (BackendAnalysis)
import PureScript.Transmogrify.Convert (BackendModule, toBackendModule)
import PureScript.Transmogrify.Semantics (EvalRef, ExternImpl, InlineDirective)
import PureScript.Transmogrify.CoreFn (Ann, Ident, Module(..), Qualified)
import PureScript.Transmogrify.CoreFn.Json (decodeModule)
import PureScript.Transmogrify.CoreFn.Sort (sortModules)

type BuildEnv =
  { implementations :: Map (Qualified Ident) (Tuple BackendAnalysis ExternImpl)
  , moduleCount :: Int
  , moduleIndex :: Int
  }

type BuildOptions =
  { directives :: Map EvalRef InlineDirective
  , onPrepareModule :: BuildEnv -> Module Ann -> Aff (Module Ann)
  , onCodegenModule :: BuildEnv -> Module Ann -> BackendModule -> Aff Unit
  }

coreFnModulesFromOutput :: FilePath -> Aff (Either (NonEmptyArray (Tuple FilePath String)) (List (Module Ann)))
coreFnModulesFromOutput path = do
  { left, right } <- map separate $ expandGlobs path [ "*/corefn.json" ] >>= Array.fromFoldable >>> parTraverse readCoreFnModule
  case NonEmptyArray.fromArray left of
    Just errors ->
      pure $ Left errors
    Nothing ->
      pure $ Right $ sortModules right

readCoreFnModule :: FilePath -> Aff (Either (Tuple FilePath String) (Module Ann))
readCoreFnModule filePath = do
  contents <- FS.readTextFile UTF8 filePath
  case lmap Json.printJsonDecodeError <<< decodeModule =<< Json.jsonParser contents of
    Left err -> do
      pure $ Left $ Tuple filePath err
    Right mod ->
      pure $ Right mod

buildModules :: BuildOptions -> List (Module Ann) -> Aff Unit
buildModules options coreFnModules =
  void $ foldM go { directives: options.directives, implementations: Map.empty, moduleIndex: 0 } (sortModules coreFnModules)
  where
  moduleCount = List.length coreFnModules
  go { directives, implementations, moduleIndex } coreFnModule = do
    let buildEnv = { implementations, moduleCount, moduleIndex }
    coreFnModule'@(Module { name }) <- options.onPrepareModule buildEnv coreFnModule
    let
      backendMod = toBackendModule coreFnModule'
        { currentModule: name
        , currentLevel: 0
        , toLevel: Map.empty
        , implementations
        , moduleImplementations: Map.empty
        , directives
        , dataTypes: Map.empty
        , rewriteLimit: 10_000
        }
      newImplementations =
        foldrWithIndex Map.insert implementations backendMod.implementations
    options.onCodegenModule (buildEnv { implementations = newImplementations }) coreFnModule' backendMod
    pure
      { directives: foldrWithIndex Map.insert directives backendMod.directives
      , implementations: newImplementations
      , moduleIndex: moduleIndex + 1
      }