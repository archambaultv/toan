-- |
-- Module      :  Toan.Parser.Location
-- Copyright   :  © 2022 Vincent Archambault
-- License     :  MIT
--
-- Maintainer  :  Vincent Archambault <vincentarchambault@icloud.com>
-- Stability   :  experimental

module Toan.Parser.Location
  (
    M.SourcePos(..),
    M.sourcePosPretty,
    M.initialPos,
    Location(..),
    Located,
    -- LocatedF,
    -- locationF,
    located,
    startPosPretty,
    endPosPretty
   )
  where

-- import Toan.Annotated (Annotated, AnnotatedF)
import qualified Text.Megaparsec as M

-- Taken from https://www.reddit.com/r/haskell/comments/4x22f9/labelling_ast_nodes_with_locations/d6cmdy9/

-- | The 'Location' datatype represents a source span 
data Location = Span M.SourcePos M.SourcePos
              deriving (Eq, Ord, Show)

-- | Pretty prints @S1@ of a @'Span' S1 _@ object with 'sourcePosPretty'
startPosPretty :: Location -> String
startPosPretty (Span s _) = M.sourcePosPretty s

-- | Pretty prints @S2@ of a @'Span' _ S2@ object with 'sourcePosPretty'
endPosPretty :: Location -> String
endPosPretty (Span _ s) = M.sourcePosPretty s

-- | The 'Located' datatype adds a source span to the type @a@
type Located a = (Location, a)

-- -- | The 'Located' datatype adds a source span to the type @a@ recursively
-- type LocatedF f = Annotated Located f

-- -- | Returns the location
-- locationF :: LocatedF f -> Location
-- locationF = fst . unfix

-- | The 'located' function adds a source span to a parser.
located :: (M.MonadParsec e s m, M.TraversableStream s) => m a -> m (Located a)
located parser = do
  begin <- M.getSourcePos
  result <- parser
  end <- M.getSourcePos
  return $ (Span begin end, result)