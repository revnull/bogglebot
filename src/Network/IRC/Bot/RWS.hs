
module Network.IRC.Bot.RWS (
                            stepBots
                           ,initialBot
                           ) where

import Control.Monad.Free.Church
import Control.Monad.RWS as RWS hiding (mapM)
import Data.List
import qualified Data.Sequence as Seq
import Data.Traversable
import Prelude hiding (mapM)

import Network.IRC.Bot

type ExecOut i o = Endo [o]

type RWSBot i o = RWS i (ExecOut i o) Bool

data Exec i o = Running (RWSBot i o (Exec i o)) | Halted

startBot :: RawBot i o a -> RWSBot i o (Exec i o)
startBot b = put False >> iterM phi (b >> return Halted) where
    phi (ReadIn g) = filtAsk g
    phi (WriteOut o a) = tell' o >> a

    filtAsk g = do
        wait <- get
        i <- ask
        case (wait, g i) of
            (False, Just g') -> put True >> g'
            _ -> return $ Running (put False >> filtAsk g)

    tell' = tell . Endo . (:)

stepBot :: Exec i o -> RWSBot i o (Exec i o)
stepBot Halted = return Halted
stepBot (Running b) = b

type Bots = Seq.Seq (Exec BotMessage BotResponse)

stepBots :: BotMessage -> Bots -> (Bots, [BotResponse])
stepBots i bots = processForks (bots', appEndo out []) where
    (bots', _, out) = runRWS (mapM exec bots) i False
    exec b = put False >> stepBot b

processForks :: (Bots, [BotResponse]) -> (Bots, [BotResponse])
processForks (bots, resps) = (bots', resps') where
    (forks, resps') = partition isFork resps
    bots' = bots <> foldMap (\(Fork b) -> initialBot b) forks

initialBot :: IRCBot a -> Bots
initialBot b = Seq.singleton (Running $ startBot b)

