module Line where

line :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
line (x0, y0) (x1, y1) =
    let dx = abs (x1 - x0)
        dy = abs (y1 - y0)
        sx = if x0 < x1 then 1 else -1
        sy = if y0 < y1 then 1 else -1
        err = dx - dy

        line' x0 y0 err =
            let e2 = err * 2
                err' = if e2 > -dx then err - dy else err
                x0' = if e2 > -dx then x0 + sx else x0
                err'' = if e2 < dx then err' + dx else err'
                y0' = if e2 < dx then y0 + sy else y0
                in if x0 == x1 && y0 == y1
                      then [(x0, y0)]
                      else (x0, y0) : line' x0' y0' err''

        in line' x0 y0 err
