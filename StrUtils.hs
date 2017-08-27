module StrUtils where


--center m line =
pad n = take n (repeat ' ')

padCenter :: Int -> String -> String
padCenter m line =
    let ll = length line
        lp = floor (fromIntegral (m - ll) / 2)
        rp = m - lp - ll
    in pad lp ++ line ++ pad rp    
