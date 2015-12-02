import System.Environment
import Tsuru

-- | 'main' runs the main program
main :: IO ()
main = getArgs >>= print . haqify . head
