module Capability.Codec where


import Data.Argonaut (Json)
import Data.Either (Either)
import Effect.Exception (Error)


-- capability to encode things that goes across domain boundaries
-- and has to be versioned
-- TODO: actually do versioning
class Encodable a where
  encode :: a -> Json

class Decodable a where
  decode :: Json -> Either Error a
