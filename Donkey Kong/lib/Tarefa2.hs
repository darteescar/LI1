{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : José Lourenço Ferreira Fernandes <a106937@alunos.uminho.pt>
              Duarte Escairo Brandão Reis Silva <a106936@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Data.List (transpose)
import Tarefa1

{-|
A função 'valida' é responsável por verificar que um Jogo não viola nenhuma das restrições impostas para a sua geração.
-}
valida :: Jogo -> Bool
valida jogo@(Jogo {mapa = mapa@(Mapa (posicao_inicial, direcao_inicial) posicao_final matriz_mapa)
                  ,inimigos = inimigos
                  ,colecionaveis = colecionaveis
                  ,jogador = jogador
                  }) 
    = temChao (tail matriz_mapa) -- ^ a função 'temChao' é chamada com o argumento @tail matriz_mapa@, pois a existência de uma Plataforma na primeira linha horizontal, implicaria que o personagem do jogador nascesse fora da área da tela.
    && verificaRessalta jogador inimigos
    && naoHaColisaoDePosicoes jogador inimigos
    && numeroMinimoDeInimigos inimigos
    && numeroVidasFantasmas inimigos
    && validaEscada (transpose matriz_mapa) 
    && alcapaoMaiorQueJogador (fst (tamanho jogador)) matriz_mapa
    && verificaPosicaoDeObjetos 0 (jogador:inimigos) colecionaveis matriz_mapa

{-| A função 'temChao', dada a matriz geradora de um mapa verifica se existe uma linha da matriz do Mapa cuja constituição é inteiramente de Plataformas (ou seja, é passível do surgimento do jogador, sendo impossível este sair do mapa inferiormente).

== Exemplos:

>> temChao [[Vazio, Vazio, Escada], [Plataforma, Plataforma, Plataforma]]
True

>>> temChao [[Vazio, Vazio, Vazio], [Vazio, Vazio, Vazio]]
False
-}

temChao :: [[Bloco]] -> Bool
temChao [] = False
temChao (linha:linhas) | and (map (\bloco -> bloco == Plataforma) linha) = True
                       | otherwise = temChao linhas

{-| A função 'verificaRessalta' recebe dois argumentos, Personagem, o jogador e uma lista de Personagem, a lista de inimigos e verifica que a propriedade ressalta está devidamente atribuída a todos eles.
Para o jogador, ressalta = False.
Para os inimigos, ressalta = True.

== Exemplos:
>>> verificaRessalta (Personagem {ressalta = False}) [Personagem {ressalta = True}, Personagem {ressalta = True}]
True

>>> verificaRessalta (Personagem {ressalta = True}) [Personagem {ressalta = True}, Personagem {ressalta = True}]
False

>>> verificaRessalta (Personagem {ressalta = False}) [Personagem {ressalta = False}]
False
-}
verificaRessalta :: Personagem -> [Personagem] -> Bool
verificaRessalta jogador inimigos = 
    (not (ressalta jogador)) && (and lista_ressalta_inimigos)
        where lista_ressalta_inimigos = map (\ inimigo -> ressalta inimigo) inimigos

{-| A função 'naoHaColisaoDePosicoes' dado o jogador e a lista de inimigos, verifica se a hitbox do jogador se cruza com a hitbox de algum dos inimigos.

== Exemplos:

>>> naoHaColisaoDePosicoes Personagem{posicao = (0.0, 5.0), tamanho = (1.0, 1.0)} [Personagem{posicao = (8.0, 5.0), tamanho = (1.0, 1.0)}]
True

>>> naoHaColisaoDePosicoes Personagem{posicao = (0.0, 5.0), tamanho = (1.0, 1.0)} [Personagem{posicao = (0.5, 5.0), tamanho = (1.0, 1.0)}]
False
-}

naoHaColisaoDePosicoes :: Personagem -> [Personagem] -> Bool
naoHaColisaoDePosicoes jogador inimigos
    | and mapeamentoCruzamentoHitboxes = True
    | otherwise = False
    where mapeamentoCruzamentoHitboxes = map (\hitboxInimigo -> not ((hitboxesCruzam (hitboxPersonagem jogador) hitboxInimigo) || (fst (hitboxPersonagem jogador) == fst hitboxInimigo))) hitboxesInimigos
          hitboxesInimigos = map (\inimigo -> hitboxPersonagem inimigo) inimigos 

{-| A função 'numeroMinimoDeInimigos' verifica se os inimigos são pelo menos dois.

== Exemplos:

>>> numeroMinimoDeInimigos [Personagem {}, Personagem {}]
True

>>> numeroMinimoDeInimigos [Personagem {}]
False
-}
numeroMinimoDeInimigos :: [Personagem] -> Bool
numeroMinimoDeInimigos inimigos = length inimigos >= 2

{-|
A função 'numeroVidasFantasmas' verifica na lista de inimigos se todos os fantasmas têm uma vida apenas.

== Exemplos:

>>> numeroVidasFantasmas [Personagem {tipo = MacacoMalvado, vida = 5}, Personagem {tipo = Fantasma, vida = 1}, Personagem {tipo = Fantasma, vida = 1}]
True

>>> numeroVidasFantasmas [Personagem {tipo = Fantasma, vida = 2}, Personagem {tipo = Fantasma, vida = 1}, Personagem {tipo = Fantasma, vida = 1}]
False
-}
numeroVidasFantasmas :: [Personagem] -> Bool
numeroVidasFantasmas inimigos = and $ map (\fantasma -> vida fantasma == 1) fantasmas
    where fantasmas = filter (\inimigo -> tipo inimigo == Fantasma) inimigos

{-|
A função 'validaEscada' recebe como argumento uma matriz transposta do mapa e retorna um Bool que informa se o posicionamento de uma Escada viola alguma restrição à sua colocação (estar acima ou abaixo de um Alcapao ou estar suspensa entre Vazio)

== Exemplos:

>>> validaEscada [[Vazio, Vazio, Plataforma], [Vazio, Plataforma, Escada], [Vazio, Escada, Plataforma]]
True

>>> validaEscada [[Vazio, Vazio, Vazio], [Vazio, Plataforma, Escada], [Vazio, Escada, Vazio]]
False
-}
validaEscada :: [[Bloco]] -> Bool
validaEscada [] = True
validaEscada matriz_mapa = and $ map (\linha -> verificaEscada [] linha && verificaEscada [] (reverse linha)) matriz_mapa

verificaEscada :: [Bloco] -> [Bloco] -> Bool
verificaEscada l [] | elem Plataforma l = True
                    | otherwise = False
verificaEscada l [bloco] = verificaEscada (bloco : l) []
verificaEscada l (Alcapao:Escada:blocos) = False
verificaEscada l (bloco:Escada:blocos) =  verificaEscada (bloco : Escada : l) blocos
verificaEscada l (bloco:blocos) = verificaEscada (bloco:l) blocos

{-|
A função 'personagensOuColecionaveisDentroDeBlocos', uma matriz do mapa, a lista dos personagens (jogador e inimigos) e a lista dos colecionáveis e suas posições, verifica se existe algum jogador ou colecionável dentro de blocos do mapa.

== Exemplos:

>>> personagensOuColecionaveisDentroDeBlocos 0 [Personagem {Posicao = (0,0)}, Personagem {Posicao = (1,0)}] [(Moeda,(0,1)), (Martelo, (0,2))] [[Vazio, Vazio, Plataforma], [Vazio, Plataforma, Escada], [Vazio, Escada, Plataforma]]
True

>>> personagensOuColecionaveisDentroDeBlocos 0 [] [(Moeda,(0,1)), (Moeda,(1,1))] [[Vazio, Vazio, Vazio], [Vazio, Plataforma, Escada], [Vazio, Escada, Vazio]]
False
-}
 --[[Bloco]] -> [Personagem] -> [(Colecionavel, Posicao)] -> Bool

verificaPosicaoDeObjetos :: Double -> [Personagem] -> [(Colecionavel, Posicao)]-> [[Bloco]] -> Bool
verificaPosicaoDeObjetos y personagens colecionaveis [] = True
verificaPosicaoDeObjetos y personagens colecionaveis (linha:linhas) = verificaPosicaoDeObjetosLinha (0,y) posicoes linha && verificaPosicaoDeObjetos (y+1) personagens colecionaveis linhas 
    where posicoes = (map (\personagem -> posicao personagem) personagens) ++ (map (\(colecionavel, pos) -> pos) colecionaveis)

verificaPosicaoDeObjetosLinha :: Posicao -> [Posicao] -> [Bloco] -> Bool
verificaPosicaoDeObjetosLinha _ posicoes [] = True
verificaPosicaoDeObjetosLinha posicao_atual@(x, y) posicoes (bloco:blocos)
    | elem posicao_atual posicoes = if bloco == Vazio then verificaPosicaoDeObjetosLinha (x+1, y) posicoes blocos else False
    | otherwise = verificaPosicaoDeObjetosLinha (x+1, y) posicoes blocos

transformaPersonagensEmPosicoes :: [Personagem] -> [Posicao]
transformaPersonagensEmPosicoes lista = map (\personagem -> posicao personagem) lista

transformaColecionaveisEmPosicoes :: [(Colecionavel, Posicao)] -> [Posicao]
transformaColecionaveisEmPosicoes lista = map (\(colecionavel, posicao)-> posicao) lista

{-|
A função 'alcapaoMaiorQueJogador', recebe o tamanho do jogador e uma matriz do mapa e devolve um Bool informando se o jogador pode ou não cair em todos os alçapões do mapa (o tamanho do jogador é menor que qualquer alçapão)

== Exemplos:

>>> alcapaoMaiorQueJogador 1 [[Vazio, Alcapao, Plataforma], [Vazio, Plataforma, Vazio], [Alcapao, Escada, Plataforma]]
True

>>> alcapaoMaiorQueJogador 2 [[Vazio, Alcapao, Plataforma], [Vazio, Plataforma, Vazio], [Alcapao, Alcapao, Alcapao]]
False
-}

alcapaoMaiorQueJogador :: Double -> [[Bloco]] -> Bool
alcapaoMaiorQueJogador tamanho_jogador matriz_mapa = length (filter (\n -> tamanho_jogador > n) matriz_numero_alcapoes_filter) == 0
    where matriz_numero_alcapoes_filter = filter (\n -> n > 0) matriz_numero_alcapoes
          matriz_numero_alcapoes = map (\linha -> maximoAlcapoesSeguidosLinha 0 0 linha) matriz_mapa

maximoAlcapoesSeguidosLinha :: Double -> Double -> [Bloco] -> Double
maximoAlcapoesSeguidosLinha novo_maximo maximo [] = maximo
maximoAlcapoesSeguidosLinha novo_maximo maximo (Alcapao:blocos) 
    | novo_maximo+1 > maximo = maximoAlcapoesSeguidosLinha (novo_maximo+1) (novo_maximo+1) blocos
    | otherwise = maximoAlcapoesSeguidosLinha (novo_maximo + 1) maximo blocos
maximoAlcapoesSeguidosLinha novo_maximo maximo (_:blocos) =
    maximoAlcapoesSeguidosLinha 0 maximo blocos