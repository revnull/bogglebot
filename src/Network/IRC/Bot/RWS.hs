
module Network.IRC.Bot.RWS (
                            stepBots
                           ,initialBot
                           ) where

import Control.Monad.Free.Church
import Control.Monad.RWS as RWS hiding (mapM)
import Data.Foldable
import Data.List
import qualified Data.Sequence as Seq
import Data.Traversable
import Prelude hiding (mapM)
import Data.FixFile

import Network.IRC.Bot

type ExecOut i o = Endo [o]

type RWSBot i o = RWST i (ExecOut i o) Bool IO

data Exec i o = Running (RWSBot i o (Exec i o)) | Halted

startBot :: Root r => FixFile r -> RawBot r i o a -> RWSBot i o (Exec i o)
startBot ff b = put False >> iterM phi (b >> return Halted) where
    phi (ReadIn g) = filtAsk g
    phi (WriteOut o a) = tell' o >> a
    phi (Query q g) = liftIO (q ff) >>= g

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

type Bots r = Seq.Seq (Exec BotMessage (BotResponse r))

stepBots :: Root r => FixFile r ->
    BotMessage -> Bots r -> IO (Bots r, [BotResponse r])
stepBots ff i bots = do
    let exec b = put False >> stepBot b
    (bots', _, out) <- runRWST (mapM exec bots) i False
    return $ processForks ff (bots', appEndo out [])

processForks :: Root r => FixFile r -> 
    (Bots r, [BotResponse r]) -> (Bots r, [BotResponse r])
processForks ff (bots, resps) = (bots', resps') where
    (forks, resps') = partition isFork resps
    bots' = bots <> foldMap (\(Fork b) -> initialBot ff b) forks

initialBot :: Root r => FixFile r -> IRCBot r a -> Bots r
initialBot ff b = Seq.singleton (Running $ startBot ff b)

