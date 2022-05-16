module UI.Functions(
  setContainer,
  setLineWidth,
)where
import Root.Functions

addBlock :: String-> String -> Int -> String
addBlock add val left
  | left > 0 = addBlock add (val++add) (left-1)
  | otherwise = val


addRow :: Int -> (String,String) -> String
addRow len (block,space) =  space++block++addBlock space "" (len-length block-1)

setLineWidth:: Int -> Int -> String -> String
setLineWidth len itr app
  | itr < len = setLineWidth len (itr+1) (append '-' app)
  | otherwise  = app

lengthHandler::String->Int->Int 
lengthHandler input len 
  | length input > len =  length input 
  | otherwise = len
setContainer :: [(String,String)] ->Int-> Int -> String -> String
setContainer list size itr set
  | itr == 0 = setContainer list size (itr+1) (
    set
    ++ endl  ++br
    ++ row1Container++midl ++br
    )
  | itr < length list = setContainer list size (itr+1) (
    set
    ++ row1Container
    )
  | otherwise =
    set
    ++  endl  ++br
  where
    row1Container = block ++ getColRow col1Len col1 ++ block ++ getColRow col2Len col2 ++ block ++ br
    col1 = fst (list!!itr)
    col2 = snd (list!!itr)
    col1Len = lengthHandler col1 len
    col2Len = lengthHandler col2 len
    getColRow l ln = addRow l (ln,space)
    col2Row = addRow len (snd (list!!itr),space)
    br = "\n"
    len = size

    space =" " -- 27
    block = "|"
    line = setLineWidth len 0 ""
    endl = "+"++line++"+"++line++"+"
    midl = block++line++"+"++line++block