module Main where

import Data.Foldable (traverse_)
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
type Chess  = State  ChessState

data ChessRowPos = RowA | RowB | RowC | RowD | RowE | RowF | RowG | RowH
data ChessColPos = Col1 | Col2 | Col3 | Col4 | Col5 | Col6 | Col7 | Col8
type ChessPos    = (ChessRowPos, ChessColPos)
type ChessMove   = (ChessPos,    ChessPos)

parseRowPos :: Char -> ChessRowPos
parseRowPos 'a' = RowA
parseRowPos 'b' = RowB
parseRowPos 'c' = RowC
parseRowPos 'd' = RowD
parseRowPos 'e' = RowE
parseRowPos 'f' = RowF
parseRowPos 'g' = RowG
parseRowPos 'h' = RowH
parseRowPos _   = undefined

parseColPos :: Char -> ChessColPos
parseColPos '1' = Col1
parseColPos '2' = Col2
parseColPos '3' = Col3
parseColPos '4' = Col4
parseColPos '5' = Col5
parseColPos '6' = Col6
parseColPos '7' = Col7
parseColPos '8' = Col8
parseColPos _   = undefined

parsePos :: String -> ChessPos
parsePos [r, c] = (parseRowPos r, parseColPos c)
parsePos _      = undefined

parseMove :: String -> ChessMove
parseMove [r1, c1, r2, c2] = (parsePos [r1, c1], parsePos [r2, c2])
parseMove _                = undefined

class UCIRead a where uciRead :: [String] -> [a]
class UCIShow a where uciShow :: a -> [[String]]

data UCIMessageIn = UCIUCI
                  | UCIDebug (Maybe Bool)
                  | UCIIsReady
                  | UCINewGame
                  | UCIPosition (Maybe ChessBoard) [ChessMove]
                  | UCIGo
                  | UCIQuit
                  | UCIUnknown [String]

instance UCIRead UCIMessageIn where
    uciRead ["uci"]                            = [UCIUCI]
    uciRead ["debug"]                          = [UCIDebug  Nothing]
    uciRead ["debug", "on"]                    = [UCIDebug (Just True)]
    uciRead ["debug", "off"]                   = [UCIDebug (Just False)]
    uciRead ["isready"]                        = [UCIIsReady]
    uciRead ["ucinewgame"]                     = [UCINewGame]
    uciRead ("position":"fen":fen :"moves":ws) = [UCIPosition  undefined (fmap parseMove ws)]
    uciRead ("position":"startpos":"moves":ws) = [UCIPosition (Just def) (fmap parseMove ws)]
    uciRead ("position"           :"moves":ws) = [UCIPosition  Nothing   (fmap parseMove ws)]
    uciRead ("go":_)                           = [UCIGo]
    uciRead ws                                 = [UCIUnknown ws]

data UCIMessageOut = UCIID String String
                   | UCIOK
                   | UCIReadyOK
                   | UCIBestMove String (Maybe String)
                   | UCIInfo String

instance UCIShow UCIMessageOut where
    uciShow (UCIID name author)        = [["id", "name", name], ["id", "author", author]]
    uciShow UCIOK                      = [["uciok"]]
    uciShow UCIReadyOK                 = [["readyok"]]
    uciShow (UCIBestMove m1 Nothing)   = [["bestmove", m1]]
    uciShow (UCIBestMove m1 (Just m2)) = [["bestmove", m1, "ponder", m2]]
    uciShow (UCIInfo xs)               = [["info", "string", xs]]

receive :: (MonadIO m, UCIRead a) => m [a]
receive = uciRead . words <$> liftIO getLine

send :: (MonadIO m, UCIShow a) => a -> m ()
send = traverse_ (liftIO . putStrLn . unwords) . uciShow

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    void $ runStateT (forever tick) def

tick :: ChessT IO ()
tick = receive >>= traverse_ handleIn

handleIn :: UCIMessageIn -> ChessT IO ()
handleIn  UCIQuit = send (UCIInfo "bye")
handleIn  UCIUCI = traverse_ send [UCIID "gameai-chess" "shockk", UCIOK]
handleIn (UCIDebug _)               = pure ()
handleIn  UCIIsReady                = send UCIReadyOK
handleIn  UCINewGame                = pure ()
handleIn (UCIPosition (Just brd) _) = modify $ \s -> s { board = brd }
handleIn (UCIPosition  Nothing _)   = pure ()
handleIn  UCIGo = do
    gets char >>= \c -> send (UCIBestMove [c, '7', c, '6'] Nothing)
    modify $ \s -> s { char = succ (char s) }
handleIn (UCIUnknown xs) = send (UCIInfo (unwords xs))
