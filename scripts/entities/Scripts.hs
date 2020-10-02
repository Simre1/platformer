module Scripts where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import ScriptPrelude

import qualified Player04718852
import qualified Platform07493618

scriptMap :: HM.HashMap T.Text EntityScript
scriptMap = HM.fromList $ ("04718852",Player04718852.entityScript):("07493618",Platform07493618.entityScript):[]