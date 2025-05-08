init_1 xs = reverse (tail (reverse xs))

init_2 xs = take (length xs - 1) xs
