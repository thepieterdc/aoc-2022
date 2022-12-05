module Utils.IO (module Utils.IO) where
import           System.Environment (getArgs)

loadInput :: IO String
loadInput = do {file:_ <- getArgs; readFile file}
