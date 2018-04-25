{-# LANGUAGE ConstraintKinds #-}
module Tmp where

import qualified Data.Map as M
import Crypto.Hash
import Select

-- | Should "degree of remove" from a link of the type : 'Blob, Blob' be modeled by 'Int'?
--
-- I.e., Can I predict the iductive data model which results from the "take
-- thing -- this is level 0; take a second thing that points to a thing on
-- level 0-- this is level 1; ... ; take an Nth thing that points to a thing on
-- level (N-1).
-- 
-- a           : 0
-- to l0 : 1
-- points to ...aa raegous aoercu aheuh saor eh
