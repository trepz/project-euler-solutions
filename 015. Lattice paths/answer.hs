paths n = div (product [((n * 2) - n + 1)..(n * 2)]) (product [1..n])

answer = paths 20
