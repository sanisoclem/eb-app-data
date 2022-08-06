module Data.Request
  ( RequestMethod (..)
  )
  where

data RequestMethod = GET
  | POST
  | DELETE
  | PUT
  | Unknown String