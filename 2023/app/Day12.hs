module Day12 where

import Debug.Trace
import Control.Monad
import Data.List
import qualified Data.Map as M


data Site = Fil | Emp | Que deriving(Show, Eq, Ord)
parseSite :: Char -> Site
parseSite '#' =  Fil
parseSite '.' =  Emp
parseSite '?' =  Que
parseSite  c  = error "AA"

show' Fil = '#' 
show' Emp = '.' 
show' Que = '?' 

parseList :: [Char] -> [Int]
parseList [] = []
parseList (',':s) = parseList s
parseList s = read num:parseList rest
    where (num, rest) = span (/=',') s

unfold' :: [a] -> [a]
unfold' x = take (length x * 5) $ cycle x

unfoldSites :: [Site] -> [Site]
unfoldSites ss = tail $ unfold' (Que:ss)

type Cache = M.Map ([Int], [Site]) Int

countArr' :: Cache -> [Int] -> [Site] -> Cache
countArr' cache ns ss | (ns, ss) `M.member` cache = cache
countArr' cache [] [] = M.insert ([], []) 1 cache
countArr' cache ns [] = M.insert (ns, []) 0 cache
countArr' cache [] ss | Fil `elem` ss = M.insert ([], ss) 0 cache
                      | otherwise     = M.insert ([], ss) 1 cache

countArr' cache sizes@(n:ns) sites@(Fil:ss) =
    case removeGroup n sites of
        Nothing -> M.insert (sizes, sites) 0 cache
        Just ss'-> M.insert (sizes, sites) result newCache
            where newCache = countArr' cache ns ss'
                  result = newCache M.! (ns, ss')

countArr' cache ns sites@(Emp:ss) = M.insert (ns, sites) result newCache
    where newCache = countArr' cache ns ss
          result   = newCache M.! (ns, ss)

countArr' cache ns sites@(Que:ss) = cache'''
    where cache'  = countArr' cache  ns (Fil:ss)
          cache'' = countArr' cache' ns (Emp:ss)
          n  = cache'' M.! (ns, Fil:ss)
          n' = cache'' M.! (ns, Emp:ss)
          cache''' = M.insert (ns, sites) (n + n') cache''

countArr ns ss = finalCache M.! (ns, ss)
    where finalCache = countArr' M.empty ns ss

countArrF ns ss = countArr (unfold' ns) (unfoldSites ss)

removeGroup :: Int -> [Site] -> Maybe [Site]
removeGroup 0 [] = Just []
removeGroup n [] = Nothing
removeGroup 0 (Fil:ss) = Nothing
removeGroup 0 (Que:ss) = Just ss
removeGroup 0 (Emp:ss) = Just ss
removeGroup n (Fil:ss) = removeGroup (n-1) ss
removeGroup n (Que:ss) = removeGroup (n-1) ss
removeGroup n (Emp:ss) = Nothing

main :: IO ()
main = do
    inputs <- lines <$!> readFile "data/Day12.txt"
    let sites = map parseSite . head . words <$> inputs
    let sizes = parseList . last . words <$> inputs
    print $ sum $ zipWith countArr sizes sites

    let sizes' = map unfold' sizes
    let sites' = map unfoldSites sites
    print $ sum $ zipWith countArr sizes' sites'
