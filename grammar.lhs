{----------------------------------------------------------------------------------------
-- Sascha Strand
-- dfa-plotter
-- Copyright November 2017
----------------------------------------------------------------------------------------}

import System.IO          -- for the type Handle and the IO functions that use it
import Data.Char          -- for the Char data type and functions that operate on it
import Text.ParserCombinators.ReadP
import Control.Applicative((<|>)) -- for <|> operator

digit, lower, upper :: ReadP Char       
digit                = satisfy isDigit  
lower                = satisfy isLower  
upper                = satisfy isUpper

some  :: ReadP a -> ReadP [a]
some p = many1 p <|> return []

nat :: ReadP Int                        
nat  = fmap read (many1 digit)          

letter :: ReadP Char                    
letter  = lower <|> upper               

{----------------------------------------------------------------------------------------
-- grammar
-- This parser performs the work of translating the regular grammar input encoding
-- of a FSA into the encoding used by the Graphiz dot tools to represent a graph 
-- of the same type.
----------------------------------------------------------------------------------------}
grammar :: ReadP [Char]
grammar = do 
  start <- many1 digit
  trans <- transSymbol
  input <- many1 letter <|> do return ""
  end   <- nat <|> (do satisfy(=='$')
                       return (-1))
  if (end >= 0) 
     then return $ " \""++start++"\" -> \""++(show end)++"\" [label=\""++input++"\"]\n"
     else do
          return $ " \""++start++"\" [shape=doublecircle]\n"

{----------------------------------------------------------------------------------------
-- transSymbol
-- This parser recognizes the transition symbol used in the regular grammar input
-- encoding, consumes it from the input string, and returns a value of the Unit type
-- indicating that the value is of no interest to the larger parser.
----------------------------------------------------------------------------------------}
transSymbol :: ReadP ()
transSymbol = do 
  trans <- string " -> "
  return ()

{----------------------------------------------------------------------------------------
-- inputChar
-- This parser recognizes a lower case letter a-z, an uper case letter A-Z, and the
-- ',' symbol
----------------------------------------------------------------------------------------}
inputChar :: ReadP Char
inputChar  = letter <|> satisfy (==',')
