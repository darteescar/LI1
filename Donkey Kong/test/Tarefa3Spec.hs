module Tarefa3Spec (testes_tarefa_3) where

import LI12324
import Tarefa3
import Test.HUnit

blocos1 :: [[Bloco]]
blocos1 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap1 :: Mapa
gameMap1 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos1

pl1 :: Personagem
pl1 = Personagem { velocidade = (0.0,0.0), tipo = Jogador, posicao = (8.5,7), direcao = Oeste, tamanho = (0.8,0.8), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (True, 10.0), invencibilidade = (False, 0)}

en1 :: Personagem
en1 = Personagem { velocidade = (0.0,0.0), tipo = Fantasma, posicao = (8,7), direcao = Este, tamanho = (0.8,0.8), emEscada = False, ressalta = True, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}
en2 :: Personagem
en2 = Personagem { velocidade = (0.0,0.0), tipo = Fantasma, posicao = (8.7,7), direcao = Este, tamanho = (0.8,0.8), emEscada = False, ressalta = True, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}

c1 :: (Colecionavel, (Double, Double))
c1 = (Martelo, (5,1))

j1 :: Jogo
j1 = Jogo gameMap1 [en1,en2] [c1] pl1

teste1A :: Test
teste1A = "T1A: Inimigo 1 perde vida." ~: True ~=? (vida . head . inimigos $ movimenta 100 1.0 j1) < 10
teste1B :: Test
teste1B = "T1B: Jogador perde vida." ~: True ~=? (vida . jogador $ movimenta 100 1.0 j1) < 10
teste1C :: Test
teste1C = "T1C: Inimigo 2 não perde vida." ~: True ~=? (vida . last . inimigos $ movimenta 100 1.0 j1) == 10

pl2 :: Personagem
pl2 = Personagem { velocidade = (0.0,0.0), tipo = Jogador, posicao = (5.2,1), direcao = Oeste, tamanho = (0.8,0.8), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}

j3 :: Jogo
j3 = Jogo gameMap1 [] [c1] pl2

j4 :: Jogo
j4 = Jogo gameMap1 [] [] (pl2 {aplicaDano = (True, 10.0)})

teste2A :: Test
teste2B :: Test
teste2A = "T2A: Jogador apanha martelo e a flag fica True." ~: True ~=? (fst . aplicaDano . jogador $ movimenta 100 1.0 j3) 
teste2B = "T2B: Jogador apanha martelo e o tempo restante é maior que zero." ~: True ~=? (snd . aplicaDano . jogador $ movimenta 100 1.0 j3) > 0

pl3 :: Personagem
pl3 = Personagem { velocidade = (0.0,0.0), tipo = Jogador, posicao = (3.5,4), direcao = Oeste, tamanho = (0.8,0.8), emEscada = True, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}
j5 :: Jogo

teste3 :: Test
j5 = Jogo gameMap1 [] [] pl3

teste3 = "T3: Jogador não cai quando esta na escada." ~: j5 ~=? movimenta 100 1.0 j5

pl4 :: Personagem
pl4 = Personagem { velocidade = (-1.0,0.0), tipo = Jogador, posicao = (0.5,10.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}
j6 :: Jogo

j6 = Jogo gameMap1 [] [] pl4

teste4 :: Test
teste4 = "T4: Jogador não atravessa o limite do mapa." ~: False ~=? (fst . posicao . jogador $ movimenta 100 1.0 j6) < 0.0
pl5 :: Personagem

pl5 = Personagem { velocidade = (0.0,0.0), tipo = Jogador, posicao = (5,7.6), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}
en3 :: Personagem
en3 = Personagem { velocidade = (0.0,0.0), tipo = Fantasma, posicao = (2.5,7.6), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}
j7 :: Jogo

j7 = Jogo gameMap1 [en3] [] pl5

blocos2 :: [[Bloco]]
blocos2 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Alcapao, Plataforma, Plataforma, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap2 :: Mapa
gameMap2 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos2

teste5 :: Test
teste5 = "T5: Alcapao e removido por jogador mas nao pelo inimigo." ~: gameMap2 ~=? (mapa $ movimenta 100 1.0 j7)

pl6 :: Personagem
pl6 = Personagem { velocidade = (0.0,0.0), tipo = Jogador, posicao = (5,1), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}
c2 :: (Colecionavel, (Double, Double))
c2 = (Moeda, (5,1))

j8 :: Jogo
j8 = Jogo gameMap1 [] [c2] pl6

teste6 :: Test
teste6 = "T6: Jogador apanha uma moeda" ~: True ~=? (pontos . jogador $ movimenta 100 1.0 j8) > (pontos . jogador $ j8)

testes_tarefa_3 :: Test
testes_tarefa_3 = test [teste1A, teste1B, teste1C, teste2A, teste2B, teste3, teste4, teste5, teste6]
