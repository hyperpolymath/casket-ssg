{- |
Module      : DAX
Description : DAX (Data eXpression) Era - Conditional rendering and basic logic
Copyright   : (c) 2025-2026 Jonathan D.A. Jewell
License     : PMPL-1.0-or-later
Maintainer  : hyperpolymath

DAX provides conditional rendering for templates based on metadata values.
Simplified MVP version for v1.0.
-}

module Gnosis.DAX
    ( evalCondition
    , processConditionals
    , processLoops
    , processTemplate
    , applyFilter
    , thousandsSeparator
    , relativeTime
    ) where

import qualified Data.Map.Strict as Map
import Gnosis.Types (Context, FlexiText(..))

-- | Evaluate a simple condition like "phase == alpha"
evalCondition :: Context -> String -> Bool
evalCondition ctx condition =
    let trimmed = trim condition
    in case parseCondition trimmed of
        Just (key, op, value) ->
            case Map.lookup key ctx of
                Just (FlexiText actual _) -> compareValues op actual value
                Nothing -> False
        Nothing -> False

-- | Parse condition string into (key, operator, value)
parseCondition :: String -> Maybe (String, String, String)
parseCondition s
    | "==" `isInfixOf` s = splitOn "==" s >>= \(k, v) -> Just (trim k, "==", trim v)
    | "!=" `isInfixOf` s = splitOn "!=" s >>= \(k, v) -> Just (trim k, "!=", trim v)
    | otherwise = Nothing
  where
    splitOn :: String -> String -> Maybe (String, String)
    splitOn needle haystack =
        case breakOn needle haystack of
            (before, after) | not (null after) ->
                Just (before, drop (length needle) after)
            _ -> Nothing

    breakOn :: String -> String -> (String, String)
    breakOn needle haystack = go "" haystack
      where
        go acc [] = (reverse acc, "")
        go acc str@(x:xs)
            | needle `isPrefixOf` str = (reverse acc, str)
            | otherwise = go (x:acc) xs

-- | Compare values based on operator
compareValues :: String -> String -> String -> Bool
compareValues "==" a b = a == b
compareValues "!=" a b = a /= b
compareValues _ _ _ = False

-- | Process {{#if}} conditionals in template
processConditionals :: Context -> String -> String
processConditionals ctx template = processIfBlocks ctx template

-- | Process {{#for}} loops in template
processLoops :: Context -> String -> String
processLoops ctx template = processForBlocks ctx template

-- | Process all DAX features (conditionals + loops)
processTemplate :: Context -> String -> String
processTemplate ctx template =
    let withConditionals = processConditionals ctx template
        withLoops = processLoops ctx withConditionals
    in withLoops

-- | Process {{#if condition}} ... {{/if}} blocks
processIfBlocks :: Context -> String -> String
processIfBlocks ctx template = go template
  where
    go [] = []
    go str
        | "{{#if " `isPrefixOf` str =
            let (condition, rest1) = extractUntil "}}" (drop 6 str)
                (trueBlock, rest2) = extractUntil "{{/if}}" rest1
                shouldShow = evalCondition ctx condition
                result = if shouldShow then trueBlock else ""
            in result ++ go (drop 7 rest2)  -- drop "{{/if}}"
        | otherwise = take 1 str ++ go (drop 1 str)

-- | Process {{#for item in list}} ... {{/for}} blocks
processForBlocks :: Context -> String -> String
processForBlocks _ template = template  -- TODO: Implement in full version

-- Helper: Extract text until delimiter
extractUntil :: String -> String -> (String, String)
extractUntil delimiter str = go "" str
  where
    go acc [] = (reverse acc, "")
    go acc s
        | delimiter `isPrefixOf` s = (reverse acc, drop (length delimiter) s)
        | otherwise = go (head s : acc) (tail s)

-- Helper functions
trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  where
    isSpace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

-- | Apply a filter function to a value
applyFilter :: String -> String -> String
applyFilter "thousands-separator" value = thousandsSeparator value
applyFilter "relativeTime" value = relativeTime value
applyFilter "uppercase" value = map toUpper value
applyFilter "lowercase" value = map toLower value
applyFilter "capitalize" value = capitalize value
applyFilter "round" value = roundValue value
applyFilter _ value = value  -- Unknown filter, return as-is

-- | Add thousands separator to numbers
thousandsSeparator :: String -> String
thousandsSeparator str =
    let digits = reverse str
        grouped = groupBy3 digits
    in reverse $ intercalate "," grouped
  where
    groupBy3 [] = []
    groupBy3 s
        | length s <= 3 = [s]
        | otherwise = take 3 s : groupBy3 (drop 3 s)

    intercalate _ [] = ""
    intercalate _ [x] = x
    intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- | Convert timestamp to relative time (simplified)
relativeTime :: String -> String
relativeTime _ = "recently"  -- Simplified for v1.0

-- | Capitalize first letter
capitalize :: String -> String
capitalize [] = []
capitalize (x:xs) = toUpper x : xs

-- | Round numeric value (simplified)
roundValue :: String -> String
roundValue str = str  -- Simplified for v1.0

-- | Convert to uppercase
toUpper :: Char -> Char
toUpper c
    | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
    | otherwise = c

-- | Convert to lowercase
toLower :: Char -> Char
toLower c
    | c >= 'A' && c <= 'Z' = toEnum (fromEnum c + 32)
    | otherwise = c
