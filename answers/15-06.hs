sqroot :: Double -> Double
sqroot n = snd (last (takeWhile rough [(x,y) | (x,y) <- zip ests (tail ests)]))
           where
              rough (x,y) = abs(x-y) > 0.00001
              next a = (a + n/a) / 2
              ests = iterate next 1.0

-- > sqroot 3
-- 1.7320508100147274

