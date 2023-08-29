; #lang racket
(if (and (eq? (read) 0) (eq? (read) 1))
    (if (or (< 10 11)
            (<= 11 13))
        0
        42)
    19)