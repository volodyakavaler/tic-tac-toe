{-# LANGUAGE DeriveGeneric #-}

module Main where

import Graphics.Gloss
import Data.Monoid

-- | Итем игрового поля.
data Item = X | O
  deriving (Eq, Show)

-- | Игровое поле.
type Board = [[Maybe Item]]

-- | Инициализация игрового поля (все ячейки пысты -- Nothing).
initBoard :: Board
initBoard = replicate 3 (replicate 3 Nothing)

-- | Отрисовка игрового поля.
drawBoard :: Board -> Picture
drawBoard board = borders <> items
  where
    -- отрисовка границ
    borders = color black (line [(-30, -90), (-30,  90)]) <>
              color black (line [(-90, -30), ( 90, -30)]) <>
              color black (line [( 30, -90), ( 30,  90)]) <>
              color black (line [(-90,  30), ( 90,  30)])
    -- отрисовка поставленных итемов
    items = mconcat [translate (fromIntegral $ (x - 1) * 60) (fromIntegral $ (y - 1) * 60) $
                     case play of
                       X -> color red   (thickCircle 1 50)
                       O -> color green (thickCircle 1 50)
                     | x <- [0..2], y <- [0..2], Just play <- [(board !! x) !! y ]
                    ]
-- | Хэндлер по нажатию мыши.
-- handleInput :: Event -> Board -> Board

-- | Метод, проверяющий победу.
-- gameIsOver :: Board -> Picture

main :: IO ()
main =
  play display bgColor fps initWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "QQ" winSize winOffset
    bgColor = white
    fps = 60

    initWorld = initBoard
    renderWorld w = drawBoard initBoard
    handleWorld _ w = w
    updateWorld _ w = w
    winSize = (180, 180)
    winOffset = (100, 100)
