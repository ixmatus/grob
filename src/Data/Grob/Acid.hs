{-# LANGUAGE CPP, TypeFamilies, OverloadedStrings, DeriveDataTypeable, FlexibleContexts,
    GeneralizedNewtypeDeriving, RecordWildCards, TemplateHaskell, MultiParamTypeClasses, FlexibleInstances #-}

module Data.Grob.Acid where

import           Control.Monad.State       (get, put)
import           Control.Monad.Reader      (ask)
import           Data.Grob.Types
import           Data.Acid                 (Update, Query, makeAcidic)
import           Data.IxSet                ( Indexable(..), (@=), getOne )
import qualified Data.IxSet as IxSet

initialBotState :: Bots
initialBotState = Bots
    {
        robots = empty
    }

-- | insert a robot into the DB
newRobot :: Robot -> Update Bots Robot
newRobot robot = do
    b@Bots{..} <- get
    put $ b {
                robots = IxSet.insert robot robots
             }
    return robot

 -- | update the robot in the database (indexed by RobotId)
updateRobot :: Robot -> Update Bots ()
updateRobot updatedRobot =
    do b@Bots{..} <- get
       put $ b { robots = IxSet.updateIx (robotId updatedRobot) updatedRobot robots
               }

-- | get a robot record by its ID (sha1 of its uri)
robotById :: RobotId -> Query Bots (Maybe Robot)
robotById rid =
     do Bots{..} <- ask
        return $ getOne $ robots @= rid

$(makeAcidic ''Bots
  [ 'newRobot
  , 'updateRobot
  , 'robotById
  ])
