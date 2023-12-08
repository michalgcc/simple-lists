{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}

module Repository.Db where

import           Database.Beam
import           Repository.Data

data SimpleListDb f = SimpleListDb
  { entries :: f (TableEntity EntryT),
    lists   :: f (TableEntity ListT)
  }
  deriving (Generic, Database be)

simpleListDb :: DatabaseSettings be SimpleListDb
simpleListDb = defaultDbSettings
