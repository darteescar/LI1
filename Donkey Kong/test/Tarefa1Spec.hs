module Tarefa1Spec where

import Test.HUnit
import LI12324
import Tarefa1

-- | Colisoes entre personagens

p1 :: Personagem
p1 = Personagem {velocidade = (0,0), tipo = Jogador, posicao = (5,4), direcao= Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}
p2 :: Personagem
p2 = Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (4,4), direcao= Oeste, tamanho = (1,1), emEscada = True, ressalta = False, vida = 2, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}

teste1 :: Test
teste1 = "T1: Personagens colidem " ~: True ~=? colisoesPersonagens p1 p2

p3 :: Personagem
p3 = Personagem {velocidade = (0,0), tipo = Jogador, posicao = (2,7), direcao= Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}
p4 :: Personagem
p4 = Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (4,4), direcao= Oeste, tamanho = (1,1), emEscada = True, ressalta = False, vida = 2, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}

teste2 :: Test
teste2 = "T2: Personagens nao colidem " ~: False ~=? colisoesPersonagens p3 p4

p5 :: Personagem
p5 = Personagem {velocidade = (0,0), tipo = Jogador, posicao = (3,2), direcao= Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}
p6 :: Personagem
p6 = Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (3,3), direcao= Oeste, tamanho = (1,1), emEscada = True, ressalta = False, vida = 2, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}

teste3 = "T3: Personagens colidem " ~: True ~=? colisoesPersonagens p5 p6


-- | Colisoes com paredes

blocos1 :: [[Bloco]]
blocos1 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

gameMap1 :: Mapa
gameMap1 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos1

pl1 :: Personagem
pl1 = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (8.5,6.5), direcao = Este, tamanho = (0.6,0.6), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}

teste4 :: Test
teste4 = "T4: Jogador nao colide com nenhuma parede " ~: False ~=? colisoesParede gameMap1 pl1

pl2 :: Personagem
pl2 = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (0.2,6.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}

teste5 :: Test
teste5 = "T5: Jogador colide com limite lateral " ~: True ~=? colisoesParede gameMap1 pl2

pl3 :: Personagem
pl3 = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (8.5,0.2), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}

teste6 :: Test
teste6 = "T6: Jogador colide com limite superior " ~: True ~=? colisoesParede gameMap1 pl3

pl4 :: Personagem
pl4 = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (7.5,2.5), direcao = Este, tamanho = (2,2), emEscada = False, ressalta = False, vida = 10, pontos = 0, aplicaDano = (False, 0.0), invencibilidade = (False, 0)}

teste7 :: Test
teste7 = "T7: Jogador colide com plataforma " ~: True ~=? colisoesParede gameMap1 pl4

testes_tarefa_1 :: Test
testes_tarefa_1 = test [teste1, teste2, teste3, teste4, teste5, teste6, teste7]