module Social where

import PFLP
import BayesianNetwork

data Person = Ted | Marshall | Lilly | Robin

friend :: Person -> Person -> Bool
friend p1 p2 = case (p1,p2) of
                   (Ted, Marshall)   -> True
                   (Marshall, Ted)   -> True
                   (Marhsall, Robin) -> True
                   (Lilly, Barney)   -> True
                   (Robin, Marshall) -> True
                   _                 -> False

smokes :: Person -> Dist Bool
smokes p = stress p
smokes p = influences p' p >>>= \i ->
           smokes p' >>>= \s ->
           pure (friend p p' && i && s)
 where p' free

stress :: Person -> Dist Bool
stress p = _

influences :: Person -> Person -> Dist Bool
influences p1 p2 = _

-- does not work: cannot generate C_Float
-- evidence = smokes Marshall =: False >>>= \m ->
--            smokes Robin =: True >>>= \r ->
--            influences Ted Marshall =: False >>>= \itm ->
--            influences Robin Marshall =: False >>>= \irm ->
--            influences Marshall Lilly =: True >>>= \iml ->
--            stress Ted =: True >>>= \st ->
--            pure st -- [itm,irm,iml]