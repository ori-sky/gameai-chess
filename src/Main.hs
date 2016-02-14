{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Foldable (traverse_)
import Data.Default
import Control.Monad.State
import Control.Lens (makeFields, (%=), use)
import System.IO (BufferMode(..), stdin, stdout, hSetBuffering)
import qualified Data.UCI as UCI

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

data ChessState = ChessState { _chessStateChar  :: Char
                             , _chessStateBoard :: ChessBoard
                             }
makeFields ''ChessState

instance Default ChessState where def = ChessState { _chessStateChar  = 'a'
                                                   , _chessStateBoard = def
                                                   }

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

receive :: MonadIO m => m [UCI.MessageIn]
receive = UCI.read <$> liftIO getLine

send :: MonadIO m => UCI.MessageOut -> m ()
send = liftIO . traverse_ putStrLn . UCI.show

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    void $ runStateT (forever tick) def

tick :: ChessT IO ()
tick = receive >>= traverse_ handleIn

handleIn :: UCI.MessageIn -> ChessT IO ()
handleIn  UCI.UCI                    = traverse_ send [UCI.ID "gameai-chess" "shockk", UCI.OK]
handleIn  UCI.Quit                   = send (UCI.Info "bye")
handleIn  UCI.IsReady                = send UCI.ReadyOK
handleIn  UCI.NewGame                = pure ()
handleIn (UCI.Position _ _)          = pure ()
handleIn (UCI.Debug _)               = pure ()
handleIn  UCI.Go = do
    use char >>= \c -> send (UCI.BestMove [c, '7', c, '6'] Nothing)
    char %= succ
