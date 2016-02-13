module Main where

import Data.Default
import Control.Monad.State
import System.IO (BufferMode(..), stdin, stdout, hSetBuffering)

instance (Default a, Default b, Default c, Default d,
          Default e, Default f, Default g, Default h)
       => Default (a, b, c, d, e, f, g, h) where
    def = (def, def, def, def, def, def, def, def)

data ChessPlayer    = WhitePlayer | BlackPlayer
data ChessPieceType = Pawn | Rook | Knight | Bishop | Queen | King
data ChessPiece     = ChessPiece ChessPlayer ChessPieceType
type ChessTile      = Maybe ChessPiece
type ChessRow       = (ChessTile, ChessTile, ChessTile, ChessTile, ChessTile, ChessTile, ChessTile, ChessTile)
type ChessBoard     = (ChessRow,  ChessRow,  ChessRow,  ChessRow,  ChessRow,  ChessRow,  ChessRow,  ChessRow)

data ChessState = ChessState { char  :: Char, board :: ChessBoard }
instance Default ChessState where def = ChessState { char = 'a', board = def }

type ChessT = StateT ChessState
type Chess = State ChessState

class UCIRead a where uciRead :: String -> [a]
class UCIShow a where uciShow :: a -> [String]

data UCIMessageIn  = UCIUCI
                   | UCIDebug (Maybe Bool)
                   | UCIIsReady
                   | UCINewGame
                   | UCIQuit
                   | UCIUnknown String

instance UCIRead UCIMessageIn where
    uciRead "uci"        = [UCIUCI]
    uciRead "debug"      = [UCIDebug Nothing]
    uciRead "debug on"   = [UCIDebug (Just True)]
    uciRead "debug off"  = [UCIDebug (Just False)]
    uciRead "isready"    = [UCIIsReady]
    uciRead "ucinewgame" = [UCINewGame]
    uciRead xs           = [UCIUnknown xs]

data UCIMessageOut = UCIID String String
                   | UCIOK
                   | UCIReadyOK
                   | UCIBestMove String (Maybe String)
                   | UCIInfo String

instance UCIShow UCIMessageOut where
    uciShow (UCIID name author)        = ["id name " ++ name, "id author " ++ author]
    uciShow UCIOK                      = ["uciok"]
    uciShow UCIReadyOK                 = ["readyok"]
    uciShow (UCIBestMove m1 Nothing)   = ["bestmove " ++ m1]
    uciShow (UCIBestMove m1 (Just m2)) = ["bestmove " ++ m1 ++ " ponder " ++ m2]
    uciShow (UCIInfo xs)               = ["info string " ++ xs]

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    void $ runStateT (forever tick) def

receive :: (MonadIO m, UCIRead a) => m [a]
receive = uciRead <$> liftIO getLine

send :: (MonadIO m, UCIShow a) => a -> m ()
send = mapM_ (liftIO . putStrLn) . uciShow

tick :: ChessT IO ()
tick = receive >>= mapM_ handleIn

handleIn :: UCIMessageIn -> ChessT IO ()
handleIn UCIQuit = send (UCIInfo "bye")
handleIn UCIUCI = do
    send (UCIID "gameai-chess" "shockk")
    send UCIOK
handleIn (UCIDebug _) = return ()
handleIn UCIIsReady = send UCIReadyOK
handleIn UCINewGame = put def
handleIn (UCIUnknown ('g':'o':' ':_)) = do
    gets char >>= \c -> send (UCIBestMove [c, '7', c, '6'] Nothing)
    modify $ \s -> s { char = succ (char s) }
handleIn (UCIUnknown xs) = send (UCIInfo xs)
