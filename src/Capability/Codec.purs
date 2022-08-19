module Capability.Codec where


import Data.Argonaut (Json)
import Data.Either (Either)
import Effect.Exception (Error)

class Encodable a where
  encode :: a -> Json

class Decodable a where
  decode :: Json -> Either Error a
