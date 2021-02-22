module Utilities.Language where

-- | Punctuators of C, sorted in descending order by their length
punctuators_list :: [String]
punctuators_list = ["typeof","sizeof","...",">>=","<<=","##",
                    "|=","^=","&=","-=","+=","%=","/=","*=",
                    "||","&&","!=","==",">=","<=",">>","<<",
                    "--","++","->",":",";","#",",","=",":",
                    "?","|","^",">","<","%","/","!","~","-",
                    "+","*","&",".",")","(","]","[","}","{"]