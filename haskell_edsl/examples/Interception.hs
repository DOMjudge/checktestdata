-- NCPC 2016 - Interception
-- See https://ncpc.idi.ntnu.no/ncpc2016/

import Checktestdata
import Control.Monad

main :: IO ()
main = ctdMain $ do
  n <- int 4 250000
  space
  m <- int 1 500000
  space
  c1 <- int 1 (n-1)
  space
  c2 <- int 1 (n-1)
  newline

  assert $ n `mod` 2 == 0
  assert $ c1 `mod` 2 == 1
  assert $ c2 `mod` 2 == 1
  assert $ c1 < c2

  ab <- replicateM (fromInteger m) $ do
    ai <- int 1 n
    space
    bi <- int 1 n
    assert $ ai /= bi
    if (ai `mod` 2 /= bi `mod` 2) then do
      space
      c <- int 1 n
      newline
      assert $ c == c1 || c == c2
    else newline
    return $ if ai > bi then (bi, ai) else (ai, bi)

  assert $ unique ab
