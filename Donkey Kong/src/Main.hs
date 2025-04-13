module Main where
  
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import LI12324
import Jogo
import Estado
import Desenha
import Reage
import Tempo
import Jogo

janela :: Display
janela = InWindow "Donkey Kong" (largura_janela, altura_janela) (0,0)

background :: Color
background = black

frameRate :: Int
frameRate = 60

main :: IO ()
main = do 
  imgs <- carregarIMG
  playIO janela background frameRate (Estado {jogoInicial = jogo1, jogo = jogo1, imagens = imgs, modo = Menu OpcaoNovoJogo, tempoDecorrido = 0}) desenha reage tempo