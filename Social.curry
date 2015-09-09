{-# OPTIONS_CYMAKE -X TypeClassExtensions #-}

module Social where

import PFLP
import BayesianNetwork
import ShowDist
import SetFunctions

data Person = Ted | Marshall | Lilly | Robin

persons :: [Person]
persons = [Ted,Marshall,Lilly,Robin]

friend :: Person -> Person -> Bool
friend p1 p2 = case (p1,p2) of
                   (Ted, Marshall)   -> True
                   (Marshall, Ted)   -> True
                   (Marhsall, Robin) -> True
                   (Lilly, Marshall) -> True
                   (Robin, Marshall) -> True
                   _                 -> False

friends :: Person -> [Person]
friends p1 = filter (friend p1) persons

-- does not terminate (mutually recursive)
smokes :: Person -> Dist Bool
smokes p = stress p
smokes p | friend p p' =
  influences p' p >>>= \i ->
  smokes p' >>>= \s ->
  pure (i && s)
 where p' free

(<||>) :: Dist Bool -> Dist Bool -> Dist Bool
d1 <||> d2 = (||) <$> d1 <*> d2

stress :: Person -> Dist Bool
stress p = bernoulli 0.3

influences :: Person -> Person -> Dist Bool
influences p1 p2 = bernoulli 0.2

query q =
  smokes Marshall =: True >>>= \_ ->
  influences Robin Marshall =: False >>>= \_ ->
  q

instance (Ord a, Show a) => Show (Dist a) where
  show = showLight

-- Learning:

-- smokes :: Person -> Dist Bool
-- smokes p = stress' p
-- smokes p = influences' p' p >>>= \i ->
--            smokes p' >>>= \s ->
--            pure (friend p p' && i && s)
--  where p' free

-- stress' :: Person -> Dist Bool
-- stress' p = _

-- influences' :: Person -> Person -> Dist Bool
-- influences' p1 p2 = _

-- does not work: cannot generate C_Float
-- evidence = smokes Marshall =: False >>>= \m ->
--            smokes Robin =: True >>>= \r ->
--            influences' Ted Marshall =: False >>>= \itm ->
--            influences' Robin Marshall =: False >>>= \irm ->
--            influences' Marshall Lilly =: True >>>= \iml ->
--            stress' Ted =: True >>>= \st ->
--            pure st -- [itm,irm,iml]