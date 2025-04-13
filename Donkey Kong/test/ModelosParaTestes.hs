module ModelosParaTestes where

import LI12324

jogador1 = Personagem {velocidade = (0.0,0.0)
                      ,tipo = Jogador
                      ,posicao = (3.0, 3.0)
                      ,direcao = Oeste
                      ,tamanho = (1.0, 1.0)
                      ,emEscada = False
                      ,ressalta = False
                      ,vida = 3
                      ,pontos = 0
                      ,aplicaDano = (False, 0)
                      }

inimigo1 = Personagem {velocidade = (0.0,0.0)
                      ,tipo = Fantasma
                      ,posicao = (0.0, 5.0)
                      ,direcao = Este
                      ,tamanho = (1.0, 1.0)
                      ,emEscada = False
                      ,ressalta = True
                      ,vida = 1
                      ,pontos = 0
                      ,aplicaDano = (False, 0)
                      }

inimigo2 = Personagem {velocidade = (0.0,0.0)
                      ,tipo = Fantasma
                      ,posicao = (5.0, 6.0)
                      ,direcao = Oeste
                      ,tamanho = (1.0, 1.0)
                      ,emEscada = False
                      ,ressalta = True
                      ,vida = 1
                      ,pontos = 0
                      ,aplicaDano = (False, 0)
                      }

inimigo3 = Personagem {velocidade = (0.0,0.0)
                      ,tipo = MacacoMalvado
                      ,posicao = (12.0, 12.0)
                      ,direcao = Este
                      ,tamanho = (2.5, 2.0)
                      ,emEscada = False
                      ,ressalta = True
                      ,vida = 1
                      ,pontos = 0
                      ,aplicaDano = (False, 0)
                      }

colecionaveis1 :: [(Colecionavel, Posicao)]
colecionaveis1 = [(Moeda, (0.0, 0.0)), (Martelo, (2.0, 0.0))]

colecionaveis2 :: [(Colecionavel, Posicao)]
colecionaveis2 = [(Moeda, (0.0, 0.0)), (Martelo, (1.0, 0.0)), (Moeda, (5.0,1.0))]

mapa1 :: Mapa
mapa1 = Mapa ((3.0, 3.0), Oeste) (5,5) matriz_mapa1 
matriz_mapa1 = [
                [Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio], 
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma], 
                [Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Escada], 
                [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Plataforma, Alcapao, Alcapao, Plataforma, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Escada, Vazio, Escada, Vazio, Vazio, Escada],
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
               ]

mapa2 :: Mapa
mapa2 = Mapa ((3.0, 3.0), Oeste) (5,5) matriz_mapa2
matriz_mapa2 = [
                [Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio], 
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma], 
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
                [Plataforma, Alcapao, Alcapao, Plataforma, Escada, Vazio, Vazio, Alcapao],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
               ]

mapa3 :: Mapa
mapa3 = Mapa ((3.0, 3.0), Oeste) (5,5) matriz_mapa3
matriz_mapa3 = [
                [Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio], 
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma], 
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Alcapao],
                [Plataforma, Alcapao, Alcapao, Plataforma, Escada, Vazio, Vazio, Vazio],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
               ]

mapa4 :: Mapa
mapa4 = Mapa ((3.0, 3.0), Oeste) (5,5) matriz_mapa4
matriz_mapa4 = [
                [Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio], 
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio], 
                [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
                [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Plataforma, Plataforma, Plataforma, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio]
               ]

mapa5 :: Mapa
mapa5 = Mapa ((3.0, 3.0), Oeste) (5,5) matriz_mapa4
matriz_mapa5 = [
                [Vazio,Vazio, Alcapao, Vazio, Vazio, Vazio,Vazio,Vazio], 
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma, Alcapao,Vazio], 
                [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
                [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Plataforma, Plataforma, Plataforma, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Vazio, Alcapao, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Vazio]
               ]

mapa6 :: Mapa
mapa6 = Mapa ((3.0, 3.0), Oeste) (5,5) matriz_mapa2
matriz_mapa6 = [
                [Vazio,Vazio, Vazio, Vazio, Vazio, Vazio,Vazio,Vazio], 
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma], 
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
                [Alcapao, Alcapao, Alcapao, Plataforma, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
                [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
               ]

jogo1 :: Jogo
jogo1 = Jogo {mapa = mapa1, inimigos = [inimigo1, inimigo2], colecionaveis = colecionaveis1, jogador = jogador1}