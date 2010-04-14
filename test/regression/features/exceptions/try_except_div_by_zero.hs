module Main where
import Berp.Base
import qualified Prelude
main = start init
init
  = try (stmt (1 / 0))
      (\ _t_0 ->
         exceptDefault
           (do _t_1 <- read _s_print
               stmt (_t_1 @@ [string "caught it"]))
           (raise _t_0))
