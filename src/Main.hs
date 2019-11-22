import ModelChecker.Parser

main :: IO () 
main = print =<< parseString <$> getContents 
