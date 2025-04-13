{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : José Lourenço Ferreira Fernandes <a106937@alunos.uminho.pt>
              Duarte Escairo Brandão Reis Silva <a106936@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where
import Data.Maybe
import LI12324
import Tarefa3
import Tarefa1
import GHC.Read (list)

{-|

A função 'atualiza' retorna um jogo com os vetores velocidade do Jogador atualizados

-}

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesinimigos acaojogador jogo@(Jogo {inimigos = listadeinimigos, colecionaveis = listaDeColecionaveis, jogador = jogador@(Personagem {tipo= Jogador , posicao = (posx,posy), direcao = d, velocidade = v, tamanho = (tamx,tamy), emEscada = u, ressalta = o, vida = vida, pontos = pontos, aplicaDano = aplicaDano, invencibilidade = invencibilidade})}) =                                                                 
   (jogo {
        jogador = jogador {velocidade = atualizaVelocidadeJogador acaojogador u, direcao = atualizaDirecao acaojogador d u, vida = atualizaVidaPersonagem jogador listadeinimigos, aplicaDano = atualizaAplicaDano jogador listaDeColecionaveis aplicaDano, invencibilidade = atualizaInvencibilidade invencibilidade},
        inimigos = atualizaFantasmas (atualizaVelocidadeFantasma acoesinimigos) listadeinimigos})

{-|

A função 'atualizaVelocidadeJogador', dado um Maybe Acao e um Bool (que representa se o Jogado está em Escada), retorna a velocidade atualizada do Jogador.

==Exemplos: 

>> atualizaVelocidadeJogador (Just Descer) True
(0, 35)

>> atualizaVelocidadeJogador (Just AndarDireita) False
(10, 0)

>> atualizaVelocidadeJogador Nothing True
(0,0)

-}
atualizaVelocidadeJogador :: Maybe Acao -> Bool -> Velocidade
atualizaVelocidadeJogador (Just AndarDireita) _ = (10,0)
atualizaVelocidadeJogador (Just AndarEsquerda) _ = (-10,0)
atualizaVelocidadeJogador (Just Descer) True = (0,10)
atualizaVelocidadeJogador (Just Saltar) True = (0,-10)
atualizaVelocidadeJogador (Just Saltar) _ = (0,-100)
atualizaVelocidadeJogador (Just Parar) _ = (0,0)
atualizaVelocidadeJogador _ _ = (0,0)

{-|

A função 'atualizaVelocidadeFantasma' devolve uma lista de Velocidades, dada uma lista de Maybe Ações que são
resultado da função geraAleatorios. A ideia é se o Personagem tem de andar para a direita o vetor terá de ter 
valores (a,b), sendo a >0 e b = 0. 

==Exemplos: 

>> atualizaVelocidadeFantasma [Just AndarEsquerda, Just AndarDireita, Just Saltar] 
[(-10,0), (10,0), (0,-10)]

>> atualizaVelocidadeFantasma [Just Saltar, Just Parar, Just Parar] 
[(0,-10),(0,0),(0,0)]

-}

atualizaVelocidadeFantasma :: [Maybe Acao] -> [Velocidade]
atualizaVelocidadeFantasma [] = []
atualizaVelocidadeFantasma ((Just AndarDireita):xs) = (10,0) : atualizaVelocidadeFantasma xs
atualizaVelocidadeFantasma ((Just AndarEsquerda):xs) = (10,0) : atualizaVelocidadeFantasma xs
atualizaVelocidadeFantasma ((Just Saltar):xs) = (0,-10) : atualizaVelocidadeFantasma xs
atualizaVelocidadeFantasma ((Just Parar):xs) = (0,0) : atualizaVelocidadeFantasma xs
atualizaVelocidadeFantasma (_:xs) = (0,0) : atualizaVelocidadeFantasma xs

{-|

A função 'atualizaFantasmas', dada uma lista de Velocidades de Fantasmas e uma lista de Fantasmas, aplica a velocidade a cada Fantasma e devolve a lista de Fantasmas atualizada.

==Exemplos: 

>> atualizaFantasmas [(-10,0), (10,0)] [Personagem {tipo=Fantasma, velocidade = (0,0)}, Personagem {tipo=Fantasma, velocidade = (0,-10)}] 
[Personagem {tipo=Fantasma, velocidade = (-10,0)}, Personagem {tipo=Fantasma, velocidade = (10,0)}] 

>> atualizaFantasmas [(0,0), (0,0)] [Personagem {tipo=Fantasma, velocidade = (10,0)}, Personagem {tipo=Fantasma, velocidade = (0,-10)}] 
[Personagem {tipo=Fantasma, velocidade = (0,0)}, Personagem {tipo=Fantasma, velocidade = (0,0)}]

-}

atualizaFantasmas :: [(Double, Double)] -> [Personagem] -> [Personagem]
atualizaFantasmas [] _ = []
atualizaFantasmas _ [] = []
atualizaFantasmas ((f,t):xs) (inimigo1@(Personagem {direcao = a,velocidade = (x,y), tipo= Fantasma, posicao = (posx, posy),tamanho = (tamx, tamy), emEscada = u, ressalta = o, vida = vida, pontos = pontos, aplicaDano = (bool, int)}):resto) =
  inimigoAtualizado : atualizaFantasmas xs resto
  where
    inimigoAtualizado = inimigo1 {velocidade = (x,y), tipo= Fantasma, posicao = (posx, posy), direcao = a, tamanho = (tamx, tamy), emEscada = u, ressalta = o, vida = vida, pontos = pontos, aplicaDano = (bool, int)}
atualizaFantasmas _ inimigosRestantes = inimigosRestantes

{-|

A função 'atualizaDirecao',dado um Maybe Acao, a direção atual do personagem e um Bool (que representa se está em escada), devolve a direção atualizada do personagem após executar a ação.

==Exemplos: 

>> atualizaDirecao (Just Descer) Norte True
Sul

>> atualizaDirecao (Just AndarDireita) Oeste False
Este

>> atualizaDirecao (Just Saltar) Oeste False
Oeste

-}

atualizaDirecao :: Maybe Acao -> Direcao -> Bool -> Direcao
atualizaDirecao (Just AndarDireita) _ _ = Este
atualizaDirecao (Just AndarEsquerda)_ _ = Oeste
atualizaDirecao (Just Descer) _ True = Sul
atualizaDirecao (Just Subir) _ True = Norte
atualizaDirecao _ d _ = d

{-|

A função 'atualizaVidaPersonagem' dado um Personagem e uma lista de inimigos, verifica se o jogador deve perder 1 vida por ter entrado em contacto com um inimigo.

==Exemplos: 

>> atualizaVidaPersonagem Personagem {tipo = Jogador, vida = 3, tamanho = (1.0,1.0), posicao = (2.0,2.0)} [Personagem {tipo = Fantasma, vida = 1, tamanho = (1.0,1.0), posicao = (2.0,2.0)}]
2

>> atualizaVidaPersonagem Personagem {tipo = Jogador, vida = 3, tamanho = (1.0,1.0), posicao = (2.0,2.0)} [Personagem {tipo = Fantasma, vida = 1, tamanho = (1.0,1.0), posicao = (4.0,7.0)}]
3
-}

atualizaVidaPersonagem :: Personagem -> [Personagem] -> Int
atualizaVidaPersonagem p i = fst $ jogadorPerde1VidaAoSerAtingidoPorInimigo p i

{-|

A função 'atualizaAplicaDano' dado um Personagem, uma lista de colecionáveis e as suas posições e o argumento aplicaDano, a função retorna o novo argumento aplicaDano apropriado.

== Exemplos:

>> atualizaAplicaDano Personagem {posicao = (4.0,4.0), tamanho = (1.0,1.0), aplicaDano = (False, 0)} [(Martelo,(4.0,4.0))] (False, 0)
(True, 10)

>> atualizaAplicaDano Personagem {posicao = (4.0,5.0), tamanho = (1.0,1.0), aplicaDano = (False, 0)} [(Martelo,(3.0,3.0))] (False, 0)
(False, 0)

>> atualizaAplicaDano Personagem {posicao = (4.0,5.0), tamanho = (1.0,1.0), aplicaDano = (False, 0)} [(Martelo,(3.0,3.0))] (True, 7)
(True, 6)

-}

atualizaAplicaDano :: Personagem -> [(Colecionavel, Posicao)] -> (Bool, Double) -> (Bool, Double)
atualizaAplicaDano _ _ (True, aplicaDanoTempo)
  | aplicaDanoTempo > 0 = (True, aplicaDanoTempo - 1)
  | otherwise = (False, 0) 
atualizaAplicaDano jogador listaDeColecionaveis@((a,(b1,b2)):xs) (False, aplicaDanoTempo)
  | hitboxesCruzam hitboxplayer ((x1,y1),(x2,y2)) && a == Martelo
    = (True, 400)
  | otherwise = (False, 0)
  where hitboxplayer = hitboxPersonagem jogador
        ((x1,y1),(x2,y2)) = ((b1,b2),(b1+1,b2-1))

{-|

A função 'atualizaInvencibilidade' recebe o argumento relativo à invencibilidade do Jogador e atualiza-a.
==Exemplos: 

>> atualizaInvencibilidade (True, 1)
(False, 0)

>> atualizaInvencibilidade (True, 4)
(False, 3)

>> atualizaInvencibilidade (False, 0)
(False, 0)

-}
atualizaInvencibilidade :: (Bool, Double) -> (Bool, Double)
atualizaInvencibilidade (True, v)
    | v == 1 = (False, 0)
    | v > 1 = (True, v - 1)
atualizaInvencibilidade (False, 0)
    = (False, 0)