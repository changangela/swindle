module Eval where

import Syntax

import Data.Maybe
import Data.Functor

isNum :: Expression -> Bool
isNum Zero     = True
isNum (Succ t) = isNum t
isNum _        = False

isVal :: Expression -> Bool
isVal Tr = True
isVal Fl = True
isVal t | isNum t = True
isVal _ = False

eval' :: Expression -> Maybe Expression
eval' x = case x of
  IsZero Zero               -> Just Tr
  IsZero (Succ t) | isNum t -> Just Fl
  IsZero t                  -> IsZero <$> (eval' t)
  Succ t                    -> Succ <$> (eval' t)
  Pred Zero                 -> Just Zero
  Pred (Succ t) | isNum t   -> Just t
  Pred t                    -> Pred <$> (eval' t)
  If Tr  c _                -> Just c
  If Fl _ a                 -> Just a
  If t c a                  -> (\t' -> If t' c a) <$> eval' t
  _                         -> Nothing

nf :: Expression -> Expression
nf x = fromMaybe x (nf <$> eval' x)

eval :: Expression -> Maybe Expression
eval t = case nf t of
  nft | isVal nft -> Just nft
      | otherwise -> Nothing -- term is "stuck"
