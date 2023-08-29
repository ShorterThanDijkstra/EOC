(if #t
    (if #f
        (if #t
            10
            12)
        (let ([x #t]) (if (not x) 17 18)))
    13)
