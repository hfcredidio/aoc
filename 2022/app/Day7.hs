module Day7 where

import Control.Monad
import Data.Maybe

data Dir = Dir { dirName :: String, dirFiles :: [File], dirChildren :: [Dir]  } deriving(Show)
data File = File { fileName :: String, fileSize :: Int } deriving(Show)
type ZipDir = (Dir, [Dir])

popWhere :: (a -> Bool) -> [a] -> Maybe (a, [a])
popWhere pred [] = Nothing
popWhere pred (x:xs)
  | pred x    = Just (x, xs)
  | otherwise = do
      (found, rest) <- popWhere pred xs
      return (found, x:rest)

popChild :: String -> Dir -> (Dir, Dir)
popChild name dir = fromMaybe (error $ "Dir " ++ name ++ " not found") $ do
    (found, otherChildren) <- popWhere ((==name) . dirName) (dirChildren dir)
    let poppedDir = dir { dirChildren = otherChildren }
    return (found, poppedDir)

addFile :: File -> Dir -> Dir
addFile f d = d { dirFiles = f:dirFiles d }

addDir :: Dir -> Dir -> Dir
addDir newDir d = d { dirChildren = newDir:dirChildren d }

zipUp :: ZipDir -> ZipDir
zipUp (dir, []) = error "No parent directory"
zipUp (dir, parent:ps) = (dir', ps)
    where dir' = parent { dirChildren = dir:dirChildren parent}

zipTop :: ZipDir -> ZipDir
zipTop (dir, []) = (dir, [])
zipTop z = zipTop $ zipUp z

zipDown :: String -> ZipDir -> ZipDir
zipDown name (dir, parents) = (found, parent:parents)
    where (found, parent) = popChild name dir

doAction :: ZipDir -> String -> ZipDir
doAction z@(dir, parents) s = case words s of
   ["$", "cd", "/"]  -> zipTop z
   ["$", "cd", ".."] -> zipUp z
   ["$", "cd", name] -> zipDown name z
   ["$", "ls"]       -> z
   ["dir", name]     -> (addDir newDir dir, parents)
                        where newDir = Dir name [] []
   [size, name]      -> (addFile newFile dir, parents)
                        where newFile = File name (read size)
   _ -> error $ "Invalid commans " ++ s

dirSizes :: Dir -> [Int]
dirSizes dir = sum fSizes + sum childrenSizes : childrenSizes ++ otherSizes
    where fSizes = map fileSize (dirFiles dir)
          dSizes = map dirSizes (dirChildren dir)
          childrenSizes = map head dSizes
          otherSizes = concatMap tail dSizes

dirSize :: Dir -> Int
dirSize = head . dirSizes

main :: IO ()
main = do
    commands <- lines <$!> readFile "data/Day7.txt"
    let (root, []) = zipTop $ foldl doAction (Dir "/" [] [], []) commands
    print $ sum $ filter (<100000) $ dirSizes root 

    let totalMemory = 70000000
    let requiredMemory = 30000000
    let availableMemory = totalMemory - dirSize root
    let toBeFreed = requiredMemory - availableMemory
    print $ minimum $ filter (>toBeFreed) $ dirSizes root 
