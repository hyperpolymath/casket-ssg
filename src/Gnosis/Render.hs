{- |
Module      : Render
Description : Template rendering engine with Tri-Guard safety
Copyright   : (c) 2025-2026 Jonathan D.A. Jewell
License     : AGPL-3.0-or-later
Maintainer  : hyperpolymath

This module implements the core rendering logic that transforms
templates with (:placeholder) syntax into final output.
-}

module Gnosis.Render
    ( render
    , renderWithBadges
    , sanitize
    , RenderMode(..)
    ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Gnosis.Types (FlexiText(..), Context, renderFlexiText, renderFlexiTextBadge)

-- | Rendering mode determines how FlexiText values are output.
data RenderMode
    = PlainText      -- ^ Output visual text only
    | ShieldsIOBadge -- ^ Output as Shields.io badge with alt-text
    deriving (Show, Eq)

-- | Render a template string, replacing (:key) placeholders with values.
-- Uses PlainText mode by default.
render :: Context -> String -> String
render = renderWithMode PlainText

-- | Render a template with Shields.io badges for visual elements.
renderWithBadges :: Context -> String -> String
renderWithBadges = renderWithMode ShieldsIOBadge

-- | Internal render function with configurable mode.
renderWithMode :: RenderMode -> Context -> String -> String
renderWithMode _ _ [] = []
renderWithMode mode ctx ('(':':':rest) =
    let (key, remaining) = span (/= ')') rest
        tailStr = drop 1 remaining  -- Drop closing ')'
        replacement = case Map.lookup key ctx of
            Just ft -> case mode of
                PlainText -> renderFlexiText ft
                ShieldsIOBadge -> renderFlexiTextBadge key ft
            Nothing -> "(:MISSING:" ++ key ++ ")"
    in replacement ++ renderWithMode mode ctx tailStr
renderWithMode mode ctx (c:cs) = c : renderWithMode mode ctx cs

-- | Sanitize a string for safe inclusion in various contexts.
-- Guard 1 of the Tri-Guard system.
sanitize :: SanitizeContext -> String -> String
sanitize ctx = filter (allowed ctx)
  where
    allowed TableCell c = c /= '|' && c /= '\n'
    allowed AltText c = c /= '[' && c /= ']' && c /= '"'
    allowed Url c = c `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "-_./:")
    allowed PlainTextCtx _ = True

-- | Context for sanitization determines which characters are allowed.
data SanitizeContext
    = TableCell    -- ^ Inside a Markdown table cell
    | AltText      -- ^ Inside image alt-text brackets
    | Url          -- ^ Inside a URL
    | PlainTextCtx -- ^ No restrictions
    deriving (Show, Eq)
