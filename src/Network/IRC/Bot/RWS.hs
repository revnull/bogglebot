
module Network.IRC.Bot.RWS (
                            interpret
                           ,BotResponse
                           ,runRWSBot
                           ) where

import Control.Applicative
import Network.IRC
import Network.IRC.Bot
import Control.Monad.Free.Church
import Control.Monad.RWS as RWS
import Data.Monoid

type BotResponse o = Either (Maybe Channel, Int) o

type RWSBot s i o = RWS i (Endo [BotResponse o]) s

interpret :: Bot s i o a -> RWSBot s i o a
interpret = foldF int . runBot where
    int (ReadIn f) = f <$> ask
    int (WriteOut o a) = tell (Endo (Right o:)) >> return a
    int (GetState f) = f <$> get
    int (PutState s a) = put s >> return a
    int (MapState f a) = get >>= put . f >> return a
    int (PutTimeout mch i a) = tell (Endo (Left (mch, i):)) >> return a

runRWSBot :: RWSBot s i o a -> i -> s -> (a, s, [BotResponse o])
runRWSBot b i s = (a, s', appEndo o []) where
    (a, s', o) = runRWS b i s

