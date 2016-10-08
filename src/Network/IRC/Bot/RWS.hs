
module Network.IRC.Bot.RWS (
                            stepBots
                           ,initialBot
                           ) where

import Control.Applicative
import Control.Monad.Free.Church
import Control.Monad.RWS as RWS hiding (mapM)
import Data.Either
import Data.Monoid
import Data.List
import qualified Data.Sequence as Seq
import Data.Traversable
import Prelude hiding (mapM)

import Network.IRC
import Network.IRC.Bot

type ExecOut i o = Endo [Either SystemMessage o]

type RWSBot i o = RWS i (ExecOut i o) Bool

data Exec i o = Running (RWSBot i o (Exec i o)) | Halted

isRunning :: Exec i o -> Bool
isRunning (Running _) = True
isRunning _ = False

startBot :: Bot i o a -> RWSBot i o (Exec i o)
startBot b = put False >> iterM phi (b >> return Halted) where
    phi (ReadIn p g) = filtAsk p g
    phi (WriteOut o a) = tell' (Right o) >> a           
    phi (System sm a) = tell' (Left sm) >> a

    filtAsk p g = do
        wait <- get
        i <- ask
        if not wait && p i
            then do
                put True
                g i
            else
                return (Running (put False >> filtAsk p g))

    tell' = tell . Endo . (:)

stepBot :: Exec i o -> RWSBot i o (Exec i o)
stepBot Halted = return Halted
stepBot (Running b) = b

type Bots = Seq.Seq (Exec Message Response)

stepBots :: Message -> Bots -> (Bots , [SystemMessage], [Response])
stepBots i bots = (bots'' <> new', sys', os) where
    (bots', _, out) = runRWS (mapM exec bots) i False
    bots'' = Seq.filter isRunning bots'
    exec b = put False >> stepBot b
    (sys, os) = partitionEithers (appEndo out [])
    (new, sys') = partition forks sys
    forks (Fork _) = True
    forks _ = False
    new' = Seq.fromList $ fmap (Running . startBot) [b | Fork b <- new]

initialBot :: Bot Message Response a -> Bots
initialBot b = Seq.singleton (Running $ startBot b)

