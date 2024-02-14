{- Haskell Lexicon - Chris Blom 

   load with : 
   ghci -XTypeFamilies -XFlexibleInstances -XFlexibleContexts -XGeneralizedNewtypeDeriving -XTypeOperators -XMultiParamTypeClasses -XDeriveDataTypeable
   then:
   :load Lexicon.hs
   use
   :load Parser.hs
   to enable parsing
   -}
module Lexicon where

import Data.Maybe
import Data.List
import Control.Monad

import Frame
import FrameUtils
import Syntax

{---- Combinators and Utility Functions -----------------}

{- Exercise 1:

   a) Open Frame.hs and expand the definition of E, such that E
   corresponds with the following set of entities : {John,Bob,Mary,Tina}

   b) Add lexicon entries for all the new entities in the lexicon below (see "Tina"'s for an example)

   c) Define charf: remove undefined and redefine the function charf such that it takes a list of entities
      and returns its characteristic function
      (hint: use the builtin function elem)
-}

charf :: [E] -> E -> T
charf entlist ent = ent `elem` entlist

{- d) After you have defined charf, try entering thin and tall in the console.
      If your definition is correct, ghci will display the sets tall and thin characterize
        >thin
        {Tina}
        and
        >tall
        {Tina}

   e) Check your answers:
  Enter this:
    :load Parser
  to load the parser, and try:
    parse "Tina is tall and Tina is thin"

  If you have done everything so far correctly, this will give you a
  structure annotated with types and denotations.
  The den. of "Tina is tall and Tina is thin" should be True.
  The den. of "John is tall or John is thin" should return False
  
  f) Now try to define charf2 :: [(E,E)] -> E -> E -> T 
  such that it takes a list of tuples and returns the characteristic function of this list.
  For an example if Tina likes Tina and Tina likes Bob, the list of tuples should be, [(Tina,Tina),(Tina,Bob)].
  
  The den. of "Tina likes Bob" should be True.
  the den. of "Bob likes Tina" should be False.
  
  Note: a transitive verb combines first with the word on his right side and then on its left
  -}
charf2:: [(E,E)] -> E -> E -> T 
charf2 tuplist e1 e2 = (e2,e1) `elem` tuplist

{---- Denotations ---------------------------------------}

tall :: E -> T
tall = charf [Tina]

thin :: E -> T
thin = charf [Tina]

likes :: E -> E -> T
likes = charf2 [(Tina,Tina), (Tina,Bob)]

{---- Exercise 2 ----

 Give denotations and lexicon entries such that:
  all the following sentences can be parsed and all are True
 In your denotations use charf, charf2 and make use of lists where possible,
 and don't forget to give the types.

 You can check your answers with:
    parse (sentence n)           (where n is the number of the sentence you want to check)
 or all at once with
    testEx2


-}

sentence 1  = "John is_a boy and Bob is_a boy"
sentence 2  = "Mary is_a girl and Tina is_a girl"
sentence 3  = "John met Mary and Mary met John"
sentence 4  = "Bob swam and John swam and it_is_not_the_case_that Mary swam"
sentence 5  = "John ran and Mary ran and Tina ran and it_is_not_the_case_that Bob ran"
sentence 6  = "John met Mary and John kissed Mary"
sentence 7  = "Mary met John and Mary kissed John"
sentence 8  = "It_is_not_the_case_that Tina met Bob"
sentence 9  = "Mary introduced Bob to John"
sentence 10 = "Bob introduced Mary to Tina"

boy :: E -> T
boy = charf [John, Bob]

girl :: E -> T
girl = charf [Mary, Tina]

ran :: E -> T
ran = charf [John, Mary, Tina]

swam :: E -> T
swam = charf [Bob, John]

met :: E -> E -> T
met = charf2 [(John, Mary), (Mary, John)]

kissed :: E -> E -> T
kissed = charf2 [(John, Mary),(Mary, John)]


charf3 :: [(E,E,E)] -> E -> E -> E -> T 
-- volgorde controleren 
charf3 tuplist e1 e2 e3 = (e3, e1, e2) `elem` tuplist

introduced_to :: E -> E -> E -> T
introduced_to = charf3 [(Mary, Bob, John), (Bob, Mary, Tina)]

-- ONCE YOU HAVE DEFINED THESE FUNCTIONS, UNCOMMENT THEM IN THE LEXICON!

{---- Lexicon -------------------------------------------}

-- some abbreviations for common syntactic categories
n       = N               -- nouns
s       = S               -- sentences
np      = NP              -- noun phrases
iv      = np :\ s         -- intransitive verbs
tv      = iv :/ np        -- transitive verbs
dtv     =((np:\s):/np):/np-- ditransitive verbs
adj     = N :/ N          -- adjectives
adv     = iv :/ iv        -- adverbs

type LexiconEntry = (String, SynCat, Denotation, [MP])
newtype Lexicon = Lexicon { entries :: [LexiconEntry] }
{- the lexicon : a list of Strings associated with their syntactic category, denotation -}

lexicon = Lexicon
  [ entry "is_a"          ((np:\s):/n)            ( (\x -> x) :: (E->T) -> (E->T))
  , entry "is"            ((np:\s):/n)            ( (\x -> x) :: (E->T) -> (E->T))
  , entry "to"            (np:/np)                ( (\x -> x) :: E->E )

{-=== Entities ===-}
  , entry "John"          np                      John
  , entry "Bob"           np                      Bob
  , entry "Tina"          np                      Tina
  , entry "Mary"          np                      Mary

{-=== Nouns and Adjectives ===-}
  , entry "boy"           n                       boy
  , entry "girl"          n                       girl 
  , entry "tall"          n                       (tall :: E -> T)
  , entry "thin"          n                       (thin :: E -> T)    

{-=== Verbs and Adverbs===-}
  , entry "ran"           iv                      ran
  , entry "swam"          iv                      swam
  , entry "met"           tv                      met
  , entry "likes"         tv                      likes
  , entry "kissed"        tv                      kissed
  , entry "introduced"    dtv                     introduced_to

{-=== Coordinations ===-}
  , entry "and"           ((s:\s):/s)             ( (\x -> \y -> x && y ))
  , entry "or"            ((s:\s):/s)             ( (\x -> \y -> x || y ))
  , entry "it_is_not_the_case_that" (s:/s)        ( \x -> not x )
  ]


------------------------------------------------------------------
-- Dont change anything below this line
------------------------------------------------------------------
    {-
checkMP :: [f] -> (f->T) -> Bool
checkMP functions  mp = not . null $ filter mp functions


(.|.) :: (FrameType f) => LexiconEntry -> (String,f) -> LexiconEntry
(str,syn,den,mp) .|. (des,mp2) = (str,syn,den,(MP des $ hide mp2) : mp)


entry :: (FrameType t,SyntacticType s ,CorrespondingTypes s t)
      => String -> s -> t  -> LexiconEntry
      -}
entry string syntax denotation = (string,wrap syntax, hide denotation,[])

getEntry :: String -> IO ()
getEntry string = sequence_ $ do
  entry@(str,syn,den,mp) <- entries lexicon
  guard ( str `case_eq` string )
  return (putStrLn . show $ entryString entry)

entryString (str,syn,den,mp) =
    [ dbracket $ show str ," = ",show den," ",showType den ,"\n",show syn]
entryArr (str,syn,den,mp) =
     [ show str , " | "
     , show syn     , "|"
     , showType den   , "|"
     , show den       , "|"
     , if not $ null mp then unwords $ map show mp else ""
      ]

instance Show Lexicon where
  show lexicon =  unlines $ map space rows  where
    rows    = map entryArr (entries lexicon)
    widths  = foldr (zipWith max . map length ) [0..] $ rows
    space   = concat . map (trimTo 40) . zipWith fillTo widths
    fillTo n string = take n $ string ++ repeat ' '
    trimTo n string = if length string > n then (take (n-3) string) ++ "..." else string

data MP = MP String Denotation
instance Show MP  where show (MP str _) = str
instance Eq MP    where (MP a _) == (MP b _) = a == b














