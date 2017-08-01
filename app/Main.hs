{-# LANGUAGE DeriveGeneric #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Monoid

boardSize :: Int
boardSize = 3

-- | Итем игрового поля.
data Item = X | O
  deriving (Eq, Show)

-- | Игровое поле.
type Board = [[Maybe Item]]

-- | Победитель
type Winner = Maybe Item

-- | Инициализация игрового поля (все ячейки пысты -- Nothing).
initWorld :: Board
initWorld = replicate boardSize (replicate boardSize Nothing)

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
handleWorld (EventKey (MouseButton LeftButton) Up _ (x, y)) board =
  insertPoint board X (snap x, snap y)
    where
      snap = (+1) . min 1 . max (-1) . fromIntegral . floor . (/ 60) . (+ 30)
      -- вставка итема на игровое поле по координатам
      insertPoint board item (x, y) =
        take x board ++ [take y (board !! x) ++ [Just item] ++ drop (y + 1) (board !! x)] ++ drop (x + 1) board
handleWorld _ board = board

-- | Метод, проверяющий победу.
-- Вычисляется методом сложения X и O; для каждой "полоски" вычисляется значение, которое там установлено, и
-- записывается в кортеж (a, b), с возможными знчениями 1 или 0, где a -- что поставлен Х, b -- O (мет. itemSum).
-- После чего  для каждой полоски вычисляется сумма всех значений Х и О соответсвено (так же в кортеже (sX, sO)).
-- Если sX или sO будет равно 3, то победителем будет, у кого это значение будет равно 3.
gameIsOver :: Board -> (String, Winner)
gameIsOver board
  -- поиск победителя по диагоналям
  | (fst diagonal  == boardSize) || (snd diagonal  == boardSize) = returnWinner (diagonal , diagonal)  "diag"
  | (fst diagonal' == boardSize) || (snd diagonal' == boardSize) = returnWinner (diagonal', diagonal') "diag'"

  -- поиск победителя по вертикалям
  | verticalX || verticalO = returnWinner (verticalX, verticalO) "vert"

  -- поиск по горизонталям
  | horizontalX || horizontalO = returnWinner (horizontalX, horizontalO ) "hor"

  -- если победа ненайдена
  | otherwise = ("diag", Nothing)
  where
    diagonal  = itemSum $ map (\(i, j) -> board !! i !! j) $ zip [0..boardSize - 1] [0..boardSize - 1]
    diagonal' = itemSum $ map (\(i, j) -> board !! i !! j) $ zip [0..boardSize - 1] (reverse [0..boardSize - 1])

    -- вычисление значений по вретикали/горизонтали
    lineVertOrDiag f items =
      foldr1 (||) $ f $ unzip $ map (\(a, b) -> (a == boardSize, b == boardSize)) items

    verticalX = lineVertOrDiag fst (map itemSum board)
    verticalO = lineVertOrDiag snd (map itemSum board)

    horizontalX = lineVertOrDiag fst (map itemSum transpon)
    horizontalO = lineVertOrDiag snd (map itemSum transpon)
    transpon = foldr1 (zipWith (++)) (map (map (\y -> [y])) board)

    -- сумма итемов
    itemSum xs = (\(a, b) -> (sum a, sum b)) $ unzip $ map (\x ->
      case x of
        (Just X) -> (1, 0)
        (Just O) -> (0, 1)
        otherwise -> (0, 0)
      )xs

    -- вернуть победителя, согласно подсчетам
    returnWinner (sX, sO) str = if (sX > sO) then (str , Just X) else (str , Just O)

-- | Обновление мира.
updateWorld :: Float -> Board -> Board
updateWorld _ board = if ((snd $ gameIsOver board) == Nothing) then board else initWorld

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
