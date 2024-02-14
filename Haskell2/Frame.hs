module Frame where

import Data.Typeable
import Data.List

{- In Frame.hs we set up our frame.
   All we have to do is give the atomic domains (of entities and truthvalues),
   Haskell will then do all the lambda abstraction and function application stuff-}

{- E : Entities, the datatype used to represent Entities -}
data E = John | Bob | Mary | Tina
  deriving (Eq,Show,Enum,Bounded,Typeable)

{- T : a type synonym for Bool. Bool is Haskell's built-in type for truth values.
 - (It is defined as: data Bool = True | False )
 - For consistency with the material used in class, and to keep our types short
 - we allow Bool to also be written as T -}
type T = Bool

{- entities : a list of all entities in the frame -}
entities :: [E]
entities = [minBound..maxBound]

{- truthvalues : a list of all truthvalues in the frame -}
truthvalues :: [T]
truthvalues = [True,False]
