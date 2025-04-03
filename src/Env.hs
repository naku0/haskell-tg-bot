{-# LANGUAGE OverloadedStrings #-}

module Env (extractEnv) where

extractEnv :: String -> String -> String
extractEnv content key =
  case findLineForKey key (lines content) of
    Just value -> value
    Nothing -> error $ "Key " ++ show key ++ " not found"

findLineForKey :: String -> [String] -> Maybe String
findLineForKey _ [] = Nothing
findLineForKey key (line:lines) =
  case break (== '=') line of
    (k, '=':val) | trim k == key -> Just (trim val)
    _ -> findLineForKey key lines
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse