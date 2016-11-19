{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

import           System.IO.Unsafe (unsafePerformIO)
import           System.Random    (Random (..))
import           Test.QuickCheck

import           Lib

deepCheck :: Testable prop => prop -> IO ()
deepCheck = quickCheckWith (stdArgs { maxSuccess = 10000})

instance Random Hour where
    random g = let (a :: Int, g') = random g in (Hour a, g')
    randomR (Hour l, Hour r) g = let (a :: Int, g') = randomR (l, r) g in (Hour a, g')

instance Arbitrary Hour where
    arbitrary = choose (Hour 1, Hour 24)


main :: IO ()
main = deepCheck (\(Hour h) -> unsafePerformIO (getTweetsHours MockedTwitter "itmo" (Hour h)) == replicate h 1) >>= print
