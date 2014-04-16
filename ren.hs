--2014/04/16
--@maeken2010
-- 確率と統計第一回目の課題プログラム
-- int2bin:10進数を2進数に変換しリストとして出力
-- ren:リストの要素をグループごとに分けてそのグループの数を出力
-- main:int2bin，renを用いて，0〜2^10-1までの数を2進数で表しその要素(1,0)のグループ(連)数をリストに格納していく
--また，コマンドラインから求めたい連の数の値を受け取りその連だけの総数を抽出
--

import Data.List
import System.Environment (getArgs)

int2bin   :: Int -> [Int]
int2bin 0 =  []
int2bin n =  n `mod` 2 : int2bin (n `div` 2)

ren :: [Int] -> Int
ren [] = 1
ren x 
  | len /= n  = length (group x) + 1
  | otherwise = length(group x)
  where len = length x 
        n = 10

main :: IO()
main = do 
  args <- getArgs
  case args of
    [] -> print "ERROR"
    (m:_) ->   print $ length (filter (==(read m :: Int))[ren (int2bin x) | x <- [0..2^10-1]])