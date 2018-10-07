module Router where    

import Prelude

import Data.Foldable (oneOf)
import Routing.Match (Match, end, lit, str)

type UserName = String
data Route
  = SelectSender
  | SelectReceiver UserName
  | Stats


instance showRoute :: Show Route where
  show = routeFor

  
routes :: Match Route
routes =
  lit "insult" *> oneOf
  [ SelectReceiver <$> (lit "from" *> str)
  , SelectSender   <$ lit "from"
  , Stats          <$ lit "stats"
  ] <* end

routeFor :: Route -> String
routeFor SelectSender              = "#insult/from"
routeFor (SelectReceiver userName) = "#insult/from/" <> userName
routeFor Stats                     = "#insult/stats"
