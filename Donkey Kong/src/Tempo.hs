module Tempo where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import LI12324
import Tarefa1
import Tarefa3
import Estado
import Reage
import LI12324 (Personagem(aplicaDano))

tempo :: Float -> Estado -> IO Estado
tempo t e@(Estado {jogo = jogo@Jogo {jogador = jogador@Personagem {aplicaDano = (aplicaDanoBool, aplicaDanoTempo)},inimigos = listadeinimigos, colecionaveis = listaDeColecionaveis, mapa = Mapa _ (xFinal, yFinal) _}, imagens = imagens, modo = EmJogo, tempoDecorrido = tempo}) 
    -- Caso de Win
    | hitboxesCruzam ((xFinal, yFinal), (xFinal+1, yFinal-1)) (hitboxPersonagem jogador)
        = return e {modo = FimJogo Win}
    -- Caso de Game Over
    | vida jogador == 0
        = return e {modo = FimJogo GameOver}
    -- Caso regular (atualiza)
    | otherwise 
        = return e {jogo = movimenta (length listadeinimigos) (realToFrac t) jogo, imagens = imagens, modo = EmJogo, tempoDecorrido = tempo + t}
tempo _ e = return e

-- antes de chamar a movimenta chamo a que decide o movimento dos inimigos (vetor velocidade