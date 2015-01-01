safetail_if :: [a] -> [a]
safetail_if xs = if null xs then []
                            else tail xs

safetail_guard :: [a] -> [a]
safetail_guard xs | null xs   = []
                  | otherwise = tail xs

safetail_pattern :: [a] -> [a]
safetail_pattern [] = []
safetail_pattern xs = tail xs
