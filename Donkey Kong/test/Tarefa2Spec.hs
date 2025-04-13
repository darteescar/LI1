module Tarefa2Spec where

import Test.HUnit
import LI12324
import Tarefa2
import ModelosParaTestes
import Data.List (transpose)

testes_valida :: Test
testes_valida =
    test ["Jogo válido" ~: True ~=? valida jogo1
         ,"Inimigo1 nasce numa plataforma" ~: False ~=? valida jogo1 {inimigos = [inimigo1{posicao = (1.0,1.0)},inimigo2]}
         ,"Mapa é inválido, mapa3" ~: False ~=? valida jogo1 {mapa = mapa3}
         ,"Mapa é inválido, mapa6" ~: False ~=? valida jogo1 {mapa = mapa6}
         ,"Número mínimo de inimigos incumprido" ~: False ~=? valida jogo1 {inimigos = [inimigo1]}
         ,"Fantasmas com vidas impróprias" ~: False ~=? valida jogo1 {inimigos = [inimigo1, inimigo2{vida = 2}]}
         ,"Jogador colide com Fantasma" ~: False ~=? valida jogo1 {inimigos = [inimigo1, inimigo2 {posicao = (3.0, 3.0)}]}
         ]

testes_temChao :: Test
testes_temChao = 
    test ["Mapa vazio" ~: False ~=? temChao []
         ,"Mapa sem Plataforma" ~: False ~=? temChao matriz_mapa4
         ,"Mapa com Plataforma" ~: True ~=? temChao matriz_mapa1
        ]

testes_verificaRessalta :: Test
testes_verificaRessalta = 
    test ["Jogador com argumento ressalta = True"
                        ~: True ~=? verificaRessalta jogador1 [inimigo1, inimigo2, inimigo3]
    ,"Jogador com argumento ressalta = True, lista com 3 inimigos com ressalta = True"
                        ~: False ~=? verificaRessalta jogador1{ressalta = True} [inimigo1, inimigo2, inimigo3]
    ,"Jogador com argumento ressalta = False, lista com 3 inimigos, 2 com ressalta = True e 1 com ressalta = False"
                        ~: False ~=? verificaRessalta jogador1 [inimigo1, inimigo2, inimigo3{ressalta = False}]
    ,"Jogador com argumento ressalta = False"
                        ~: True ~=? verificaRessalta jogador1 []
    ]

testes_naoHaColisaoDePosicoes :: Test
testes_naoHaColisaoDePosicoes =
    test ["Jogador não colide com nenhum inimigo" ~: True ~=? naoHaColisaoDePosicoes jogador1 [inimigo1, inimigo2, inimigo3]
         ,"Jogador não colide com nenhum inimigo, inimigos colidem entre si" ~: True ~=? naoHaColisaoDePosicoes jogador1 [inimigo1, inimigo1, inimigo2, inimigo2, inimigo3]
         ,"Jogador colide com inimigo (nascem no mesmo local)" ~: False ~=? naoHaColisaoDePosicoes jogador1{posicao = (0.0, 5.0)} [inimigo1, inimigo2, inimigo2, inimigo3]
         ,"Jogador colide com inimigo (hiboxes intersetam-se)" ~: False ~=? naoHaColisaoDePosicoes jogador1{posicao = (0.80, 5.80)} [inimigo1, inimigo2, inimigo3]
         ,"Jogador não colide com inimigo (nascem lado a lado)" ~: True ~=? naoHaColisaoDePosicoes jogador1{posicao = (0.0, 6.0)} [inimigo1, inimigo2, inimigo3]
         ]

testes_numeroMinimoDeInimigos :: Test
testes_numeroMinimoDeInimigos = 
    test ["Lista com 2 inimigos" ~: True ~=? numeroMinimoDeInimigos [inimigo1, inimigo3]
         ,"Lista com 4 inimigos" ~: True ~=? numeroMinimoDeInimigos [inimigo1, inimigo2, inimigo2, inimigo3]
         ,"Lista com 1 inimigo" ~: False ~=? numeroMinimoDeInimigos [inimigo2]
         ,"Lista sem inimigos" ~: False ~=? numeroMinimoDeInimigos []
         ]

testes_numeroVidasFantasmas :: Test
testes_numeroVidasFantasmas =
    test ["Dois fantasmas com vida = 1, um jogador e um macaco" ~: True ~=? numeroVidasFantasmas [jogador1, inimigo1, inimigo2, inimigo3]
         ,"Cinco fantasmas com vida = 1" ~: True ~=? numeroVidasFantasmas [inimigo1, inimigo1, inimigo2, inimigo2, inimigo2]
         ,"Dois fantasmas com vida = 2, um jogador e um macaco" ~: False ~=? numeroVidasFantasmas [jogador1, inimigo3, inimigo1{vida = 2}, inimigo2{vida = 2}]
         ,"Quatro fantasmas com vida = 3" ~: False ~=? numeroVidasFantasmas [inimigo1{vida = 3}, inimigo1{vida = 3}, inimigo1{vida = 3}, inimigo2{vida = 3}, inimigo2{vida = 3}]
         ,"Dois fantasmas, um com vida = 1 e um com vida = 2" ~: False ~=? numeroVidasFantasmas [inimigo1, inimigo1{vida = 2}]
         ]

testes_validaEscada :: Test
testes_validaEscada =
    test ["Mapa válido" ~: True ~=? validaEscada (transpose matriz_mapa1)
         ,"Mapa com escada a começar em alcapao" ~: False ~=? validaEscada (transpose matriz_mapa2)
         ,"Mapa com escada a terminar em alcapao" ~: False ~=? validaEscada (transpose matriz_mapa3)
         ,"Mapa com escada a começar e terminar em vazio" ~: False ~=? validaEscada (transpose matriz_mapa4)
         ]

testes_personagensOuColecionaveisDentroDeBlocos :: Test
testes_personagensOuColecionaveisDentroDeBlocos =
    test ["Objetos não se encontram dentro do mapa" ~: True ~=? verificaPosicaoDeObjetos 0 [jogador1, inimigo1, inimigo2] colecionaveis1 matriz_mapa1
         ,"Um colecionável encontra-se dentro do mapa" ~: False ~=? verificaPosicaoDeObjetos 0 [jogador1, inimigo1, inimigo2] colecionaveis2 matriz_mapa1
         ,"Um jogador encontra-se dentro do mapa" ~: False ~=? verificaPosicaoDeObjetos 0 [jogador1{posicao = (3.0, 1.0)}, inimigo1, inimigo2] colecionaveis1 matriz_mapa1
         ]

testes_alcapaoMaiorQueJogador :: Test
testes_alcapaoMaiorQueJogador =
    test ["Tamanho do jogador 1 e mapa com alçapões" ~: True ~=? alcapaoMaiorQueJogador 1 matriz_mapa1
         ,"Tamanho do jogador 1 e mapa sem alçapões" ~: True ~=? alcapaoMaiorQueJogador 1 matriz_mapa4
         ,"Tamanho do jogador 2 e mapa com alçapões com 1 de largura" ~: False ~=? alcapaoMaiorQueJogador 2 matriz_mapa5 
         ,"Tamanho do jogador 2 e mapa com alçapão com 3 de largura" ~: True ~=? alcapaoMaiorQueJogador 2 matriz_mapa6 
         ]

testes_tarefa_2 :: Test
testes_tarefa_2 = test [testes_valida
                  ,testes_temChao
                  ,testes_verificaRessalta
                  ,testes_naoHaColisaoDePosicoes
                  ,testes_numeroMinimoDeInimigos
                  ,testes_numeroVidasFantasmas
                  ,testes_validaEscada
                  ,testes_personagensOuColecionaveisDentroDeBlocos
                  ,testes_alcapaoMaiorQueJogador]