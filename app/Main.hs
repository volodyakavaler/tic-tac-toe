{-# LANGUAGE DeriveGeneric #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Monoid

-- | Итем игрового поля.
data Item = X | O
  deriving (Eq, Show)

-- | Игровое поле.
type Board = [[Maybe Item]]

-- | Инициализация игрового поля (все ячейки пысты -- Nothing).
initWorld :: Board
initWorld = replicate 3 (replicate 3 Nothing)

-- | Отрисовка игрового поля.
renderWorld :: Board -> Picture
renderWorld board = borders <> items
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
handleWorld :: Event -> Board -> Board
handleWorld (EventKey (MouseButton LeftButton) Up _ (x, y)) board = insertPoint board X (snap x, snap y)
  where
    snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/ 60) . (+ 30)
    -- вставка итема на игровое поле по координатам
    insertPoint board item (x, y) =
      take x board ++ [take y (board !! x) ++ [Just item] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board
handleWorld _ board = board

-- | Метод, проверяющий победу.
-- gameIsOver :: Board -> Picture

-- | Обновление мира.
updateWorld :: Float -> Board -> Board
updateWorld _  board = board

-- | main.
main :: IO ()
main =
  play display bgColor fps initWorld renderWorld handleWorld updateWorld
  where
    display = InWindow "QQ" winSize winOffset
    bgColor = white
    fps = 60
    winSize = (180, 180)
    winOffset = (100, 100)
