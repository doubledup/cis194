fun1' :: [Integer] -> Integer
fun1' xs = product . map (subtract 2) . filter even $ xs

