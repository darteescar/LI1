{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : José Lourenço Ferreira Fernandes <a106937@alunos.uminho.pt>
              Duarte Escairo Brandão Reis Silva <a106936@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1
import Tarefa2 ()

{-| 

A função 'movimenta' atualiza o jogo, ao verificar todas as colisões e atualizações, seja a nível de velocidade
posição, apanhar de objetos, perda de vidas, ganho de pontos , etc, refletindo as respetivas consequências nas 
personagens, no mapa ou nos colecionáveis.

== Exemplos:

mapa1 = [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],


mapa1 = [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],
         [Vazio,Alcapao,Alcapao,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],
         [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]

listadeinimigos = [inimigo1,inimigo2]

inimigo2 = (Personagem {velocidade = (0.0,0.0)
                      ,tipo = Fantasma
                      ,posicao = (15.0, 2.0)
                      ,direcao = Este
                      ,tamanho = (1.0, 1.0)
                      ,emEscada = False
                      ,ressalta = True
                      ,vida = 1
                      ,pontos = 0
                      ,aplicaDano = (False, 0)
                      })

inimigo1 = (Personagem {velocidade = (0.0,0.0)
                      ,tipo = MacacoMalvado
                      ,posicao = (24.0, 12.5)
                      ,direcao = Este
                      ,tamanho = (2.5, 2.0)
                      ,emEscada = False
                      ,ressalta = True
                      ,vida = 1
                      ,pontos = 0
                      ,aplicaDano = (False, 0)
                      })

jogo_movimenta = Jogo {
    mapa = (Mapa ((10,12),Norte) (5,6) ) mapa1,
    inimigos = listadeinimigos,
    colecionaveis = [],
    jogador = Personagem {
        vida = 3,
        posicao = (1, 1),
        pontos = 0,
        aplicaDano = (False, 0),
        emEscada = False,
        velocidade = (10,0),
        tipo = Jogador,
        direcao = Este,
        tamanho = (1,1),
        ressalta = False
    }}

>>> movimenta 10 1 jogo_movimenta 
Jogo {mapa = Mapa ((10.0,12.0),Norte) (5.0,6.0) [[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Alcapao,Alcapao,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,5.0), tipo = MacacoMalvado, posicao = (24.0,12.5), direcao = Este, tamanho = (2.5,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)},Personagem {velocidade = (0.0,5.0), tipo = Fantasma, posicao = (15.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [], jogador = Personagem {velocidade = (10.0,0.0), tipo = Jogador, posicao = (101.0,1.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}

-}

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente tempo jogo@(Jogo{
mapa = mapa1@(Mapa j k p) ,
inimigos = listadeinimigos,
colecionaveis = listadecolecionaveis,
jogador = jogador})=
    jogo {
           inimigos = novosInimigos,
           jogador = novoJogador,
           mapa = mapa1,
           colecionaveis = novosColecionaveis
         }
    where novosInimigos = map (atualizaInimigo tempo mapa1 jogador) listadeinimigos
          novoJogador = atualizaJogador tempo jogo jogador listadeinimigos
          novosColecionaveis = atualizaColecionaveis listadecolecionaveis jogador

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
------------------------------------------ ATUALIZA COLECIONÁVEIS --------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

atualizaColecionaveis :: [(Colecionavel,Posicao)] -> Personagem -> [(Colecionavel,Posicao)]
atualizaColecionaveis [] _ = []
atualizaColecionaveis colecionaveis@((a,b):xs) jogador
    | hitboxesCruzam hitboxdojogador g = xs
    | otherwise =  (a,b) : atualizaColecionaveis xs jogador
    where hitboxdojogador = hitboxPersonagem jogador
          (g:ys) = hitboxesDosColecionaveis colecionaveis

{-|

A função atualizaColecionaveis remove um colecionavel da lista caso a hitbox deste se cruze com a hitbox de uma
Personagem. Esta utiliza as funções hitboxPersonagem e hitboxesDosColecionaveis para calcular as hitboxes e a
hitboxesCruzam para verificar se existe ou não uma interseção.

== Exemplos:

>>> atualizaColecionaveis [] (Personagem {posicao = (1,1), tamanho = (1,1)}) = []
[]

>>> atualizaColecionaveis [(Moeda,(1,1)),(Martelo,(5,5)),(Martelo,(3,3))] (Personagem {posicao = (1,1), tamanho = (1,1)})
[(Martelo,(5.0,5.0)),(Martelo,(3.0,3.0))]

-}

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
------------------------------------------- ATUALIZA MAPA ----------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

atualizaMapa :: [[Bloco]] -> Hitbox -> [[Bloco]]
atualizaMapa [[]] _ = undefined

{-|

A função atualizaMapa apenas precisa de alterar o Mapa caso um Alcapao seja pisado, logo, recebe um mapa e uma
personagem (que será o Jogador) e troca o Alcapao por um Vazio, caso seja pisado.

== Exemplos:

mapa1 = [[Escada,Escada,Alcapao,Alcapao,Alcapao][Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,]]

jogador1 = (Personagem {tipo = Jogador, posicao = (3,3), tamanho = (1,1)})

>>> atualizaMapa mapa1 jogador1
[[Escada,Escada,Alcapao,Vazio,Alcapao][Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]

-}

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------ATUALIZA OS INIMIGOS----------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

atualizaInimigo :: Tempo -> Mapa -> Personagem -> Personagem -> Personagem
atualizaInimigo tempo mapa@(Mapa _ _ mapa1) jogador1@( Personagem { tipo = jogador } ) inimigo1@(Personagem { velocidade = (a,b) , tipo = inimigo , posicao = (posx,posy)}) =
    inimigo1 {
        vida = novaVida,
        posicao = novaPosicao,
        emEscada = novoEmEscada,
        velocidade = decideVelocidade mapa1 inimigo1
            }
    where
          novaVida = inimigoPerde1VidaAoCruzarComMartelo jogador1 inimigo1
          novaPosicao = (posx+a*tempo,posy+b*tempo)
          novoEmEscada = verificaSeEstaEmEscada inimigo1 mapa1

{-|

A função atualizaInimigo, dado um Mapa, dois personagens (o Jogador e o Inimigo) devolve uma personagem
atualizada (o Inimigo). São usadas as funções inimigoPerde1VidaAoCruzarComMartelo para diminuir a vida do 
inimigo sempre que necessário e a verificaSeEstaEmEscada.

== Exemplo:

jogador1 = (Personagem {velocidade = (3.0,3.0), tipo = Jogador,  posicao = (100.0, 100.0), direcao = Oeste, tamanho = (2.0, 2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False, 0)})

inimigo1 = (Personagem {velocidade = (3.0,3.0), tipo = MacacoMalvado, posicao = (4.0, 4.0), direcao = Este, tamanho = (2.0, 2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)})

mapa3 = [[Vazio,Vazio, Alcapao, Vazio, Vazio, Vazio,Vazio,Vazio], 
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma, Alcapao,Vazio], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma, Plataforma, Plataforma, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Alcapao, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Vazio],
        [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]
       ]

>>> atualizaInimigo 1 (Mapa ((0, 0), Norte) (5,5) mapa3) jogador1 inimigo1
Personagem {velocidade = (3.0,3.0), tipo = MacacoMalvado, posicao = (7.0,7.0), direcao = Este, tamanho = (2.0,2.0), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}

-}

hitboxJogadorArmado :: Personagem -> Maybe Hitbox
hitboxJogadorArmado a@(Personagem {tipo = Jogador , direcao = Este , aplicaDano = (True,_), posicao = (x,y), tamanho = (x1,y1) } ) = Just ((x2,y1),(x2+(x2-x1),y2))
    where ((x1,y1),(x2,y2)) = hitboxPersonagem a
hitboxJogadorArmado a@(Personagem {tipo = Jogador , direcao = Oeste , aplicaDano = (True,_), posicao = (x,y), tamanho = (x1,y1) } ) = Just ((x2-(x2-x1),y1),(x1,y2))
    where ((x1,y1),(x2,y2)) = hitboxPersonagem a
hitboxJogadorArmado (Personagem {tipo = Jogador , aplicaDano = (False,_)} ) = Nothing
hitboxJogadorArmado (Personagem {tipo = Fantasma}) = Nothing
hitboxJogadorArmado (Personagem {tipo = MacacoMalvado}) = Nothing

{-|

A função hitboxJogadorArmado calcula a hitbox de um Jogador que esteja armado, tendo em conta a direção para
a qual o personagem está virado. Caso a componente aplicaDano do Jogador não esteja ativa não é retornada 
nenhuma hitbox. É usada também a função hitboxPersonagem de maneira a auxiliar no cálculo da hitbox do Martelo.

== Exemplos:

jogador1 = (Personagem {tipo = Jogador , vida = 5, posicao = (3.5,3.7) , tamanho = (1,1), aplicaDano = (False,0), direcao = Oeste})

jogador2 = (Personagem {tipo = Jogador , vida = 2, posicao = (5.2,5.4) , tamanho = (1,1), aplicaDano = (True,10), direcao = Oeste})

jogador3 = (Personagem {tipo = Jogador , vida = 2, posicao = (10.4,7.6) , tamanho = (2,2), aplicaDano = (True,10), direcao = Este})

inimigo1 = (Personagem {tipo = Fantasma , vida = 3, posicao = (4.2,4.6) , tamanho = (1,1)})

inimigo2 = (Personagem {tipo = MacacoMalvado , vida = 3, posicao = (5.2,5.8) , tamanho = (1,1)})

>>> hitboxJogadorArmado jogador1
Nothing

>>> hitboxJogadorArmado jogador2
Just ((5.2,5.4),(5.2,4.4))

>>> hitboxJogadorArmado jogador3 
Just ((12.4,7.6),(14.4,5.6))

>>> hitboxJogadorArmado inimigo1
Nothing

>>> hitboxJogadorArmado inimigo2
Nothing

-}

hitboxdeInimigoCruzaComMartelo :: Personagem -> Personagem -> Bool
hitboxdeInimigoCruzaComMartelo jogador inimigo = if (hitboxJogadorArmado jogador) == Nothing then False else hitboxesCruzam x (hitboxPersonagem inimigo)
    where Just x = hitboxJogadorArmado jogador

{-|

A função hitboxdeInimigoCruzaComMartelo verifica se a hitbox do Martelo de um Jogador cruza-se com a Hitbox de
um inimigo, usando a função hitboxJogadorArmado para calcular qual a hitbox de um Jogador armado e a função 
hitboxesCruzam para verificar a interseção.

== Exemplos:

jogador1 = (Personagem {tipo = Jogador , vida = 3, posicao = (3,3) , tamanho = (1,1), aplicaDano = (False,0), direcao = Este})

jogador2 = (Personagem {tipo = Jogador , vida = 2, posicao = (5,5) , tamanho = (1,1), aplicaDano = (True,10), direcao = Oeste})

jogador3 = (Personagem {tipo = Jogador , vida = 2, posicao = (5,5) , tamanho = (2,2), aplicaDano = (True,10), direcao = Este})

inimigo1 = (Personagem {tipo = Fantasma , vida = 2, posicao = (4,4) , tamanho = (1,1)})

inimigo2 = (Personagem {tipo = MacacoMalvado , vida = 3, posicao = (5,5) , tamanho = (1,1)})

inimigo3 = (Personagem {tipo = Fantasma , vida = 3, posicao = (6,6) , tamanho = (1,1)})

>>> hitboxdeInimigoCruzaComMartelo jogador1 inimigo1
False

>>> hitboxdeInimigoCruzaComMartelo jogador2 inimigo2
True

>>> hitboxdeInimigoCruzaComMartelo jogador3 inimigo3
True

-}

inimigoPerde1VidaAoCruzarComMartelo :: Personagem -> Personagem -> Int
inimigoPerde1VidaAoCruzarComMartelo personagem inimigo@(Personagem {vida = x}) = if hitboxdeInimigoCruzaComMartelo personagem inimigo then x-1 else x

{-|

A função inimigoPerde1VidaAoCruzarComMartelo retira uma vida a um inimigo caso a hitbox do Martelo
de um Jogador com a componente aplicaDano ativa se cruze com a hitbox de um outro Personagem.

== Exemplos:

jogador1 = (Personagem {tipo = Jogador , vida = 5, posicao = (3,3) , tamanho = (1,1), aplicaDano = (False,0), direcao = Este})

jogador2 = (Personagem {tipo = Jogador , vida = 3, posicao = (5,5) , tamanho = (1,1), aplicaDano = (True,10), direcao = Oeste})

jogador3 = (Personagem {tipo = Jogador , vida = 3, posicao = (5,5) , tamanho = (1,1), aplicaDano = (True,10), direcao = Este})

inimigo1 = (Personagem {tipo = Fantasma , vida = 3, posicao = (4,4) , tamanho = (1,1)})

inimigo2 = (Personagem {tipo = Fantasma , vida = 6, posicao = (6,6) , tamanho = (1,1)})

inimigo3 = (Personagem {tipo = MacacoMalvado , vida = 4, posicao = (6,6) , tamanho = (1,1)})

>>> inimigoPerde1VidaAoCruzarComMartelo jogador1 inimigo1
3

>>> inimigoPerde1VidaAoCruzarComMartelo jogador2 inimigo2
6

>>> inimigoPerde1VidaAoCruzarComMartelo jogador3 inimigo3
3

-}

--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
-------------------------------------------- ATUALIZA O JOGADOR ----------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------

atualizaJogador :: Tempo -> Jogo -> Personagem -> [Personagem] -> Personagem
atualizaJogador tempo jogo@(Jogo { colecionaveis = g , mapa = l@(Mapa _ _ blocos )}) jogador@(Personagem { velocidade = (a,b) , posicao = (x,y) } ) listadeinimigos =
    jogador {
        vida = novaVida,
        posicao = novaPosicao,
        pontos = novosPontos,
        aplicaDano = novoAplicaDano,
        emEscada = novoEmEscada,
        velocidade =  novaVelocidade,
        invencibilidade = novaInvecibilidade
            }
    where
        (novaVida, novaInvecibilidade) = jogadorPerde1VidaAoSerAtingidoPorInimigo jogador listadeinimigos
        novaPosicao = (x+a*tempo,y+b*tempo)
        novosPontos = fst (jogadorCruzaComColecionavel jogador g)
        novoAplicaDano = snd (jogadorCruzaComColecionavel jogador g)
        novoEmEscada = verificaSeEstaEmEscada jogador blocos
        novaVelocidade = decideVelocidade blocos jogador

{-|

A função atualizaJogador recebe um tempo, um jogo, uma personagem (que será o Jogador), uma lista de personagens
(que serão os Inimigos) e devolve um personagem atualizado (o Jogador). Esta usa as funções que a sucedem no 
ficheiro para atualizar os parâmetros pelos quais a função movimenta é responsável por atualizar.

== Exemplo:

mapa1 = [[Vazio,Vazio, Alcapao, Vazio, Vazio, Vazio,Vazio,Vazio], 
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma, Alcapao,Vazio], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma, Plataforma, Plataforma, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Alcapao, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Vazio]
       ]

mapa2 = (Mapa ((0, 0), Norte) (5,5) mapa1)

inimigos1 = [Personagem {velocidade = (2.0,2.0), tipo = Fantasma, posicao = (3.0, 3.0), direcao = Este, tamanho = (2.0, 2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)},Personagem {velocidade = (3.0,3.0), tipo = MacacoMalvado, posicao = (4.0, 4.0), direcao = Este, tamanho = (2.0, 2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)}]

jogador1 = Personagem {velocidade = (3.0,3.0), tipo = Jogador,  posicao = (100.0, 100.0), direcao = Este, tamanho = (2.0, 2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False, 0)}

>>> atualizaJogador 1 (Jogo {mapa = mapa2 , inimigos = inimigos1 , colecionaveis = [(Moeda, (100.0,100.0)),(Martelo,(15,5)),(Martelo,(20,6))] , jogador = jogador1 } ) jogador1 inimigos1
Personagem {velocidade = (3.0,3.0), tipo = Jogador, posicao = (103.0,103.0), direcao = Oeste, tamanho = (2.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 100, aplicaDano = (False,0.0)}

jogador2 = Personagem {velocidade = (-3.0,0.0), tipo = Jogador,  posicao = (15,5), direcao = Oeste, tamanho = (2.0, 2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False, 0)}

>>> atualizaJogador 1 (Jogo {mapa = mapa2 , inimigos = inimigos1 , colecionaveis = [(Moeda, (50.0,50.0)),(Martelo,(15,5)),(Martelo,(20,6))] , jogador = jogador1 } ) jogador1 inimigos1
Personagem {velocidade = (-3,0), tipo = Jogador, posicao =  }

>>> atualizaJogador 1 (Jogo {mapa = mapa2 , inimigos = inimigos1 , colecionaveis = [(Moeda, (50.0,50.0)),(Martelo,(15,5)),(Martelo,(20,6))] , jogador = jogador2 } ) jogador2 inimigos1
Personagem {velocidade = (-3.0,5.0), tipo = Jogador, posicao = (12.0,5.0), direcao = Oeste, tamanho = (2.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,10.0)}

-}

decideVelocidade :: [[Bloco]] -> Personagem -> (Double,Double)
decideVelocidade mapa personagem@(Personagem {velocidade = (a,b)})
    | verificaSeEstaEmEscada personagem mapa  = (a,b)
    | estaNoAr mapa personagem = (a,5)
    | temChao1 mapa personagem = (a,0)
    | otherwise = (0,5)

{-|

A função 'decideVelocidade' trata da lógica das alterações que podem acontecer à velocidade do Jogador, isto é,
se o Jogador estiver em cima de uma Plataforma, este para. Se estiver no Ar, isto é, sem todos os cantos da sua
hitbox estiverem em contacto com apenas Hitboxes de Vazios, este cai.

== Exemplos:

mapa1 = [[Vazio,Vazio, Alcapao, Vazio, Vazio, Vazio,Vazio,Vazio], 
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma, Alcapao,Vazio], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma, Plataforma, Plataforma, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Alcapao, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Vazio]
        ]

personagem1 = (Personagem {posicao = (0,7.1), tamanho = (1,1), velocidade = (0,10)})

personagem2 = (Personagem {posicao = (0.5,6), tamanho = (1,1), velocidade = (5,10)})

personagem3 = (Personagem {posicao = (1,5), tamanho = (1,1), velocidade = (5,10)})

>>> decideVelocidade mapa1 personagem1
(0.0,0.0)

>>> decideVelocidade mapa1 personagem2 
(5.0,5.0)

>>> decideVelocidade mapa1 personagem3
(5.0,5.0)

-}

estaNoAr :: [[Bloco]] -> Personagem -> Bool
estaNoAr lista personagem = pontoDentroDaHitboxLista (x1,y1) listafiltrada2 && pontoDentroDaHitboxLista (x2,y2) listafiltrada2 && pontoDentroDaHitboxLista (x1,y2) listafiltrada2 && pontoDentroDaHitboxLista (x2,y1) listafiltrada2
    where lista1 = calculaHitboxBlocos lista
          listafiltrada1 = filter (\ (x,y) -> x == Vazio ) (calculaHitboxBlocos lista)
          listafiltrada2 = map snd listafiltrada1
          ((x1,y1),(x2,y2)) = hitboxPersonagem personagem

{-|

A função 'estaNoAr' verifica se a Hitbox do Personagem se toca com alguma outra Hitbox de um qualquer 
Vazio. Isto é feito calculando a Hitbox de todos os Blocos do Mapa e filtrando apenas aqueles
que são Vazios, de seguida, retira as Hitboxes desses Vazios e verifica se algum deles se cruza
com a Hitbox do Personagem.

mapa1 = [[Vazio,Vazio, Alcapao, Vazio, Vazio, Vazio,Vazio,Vazio], 
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma, Alcapao,Vazio], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma, Plataforma, Plataforma, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Alcapao, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Vazio]
        ]

personagem1 = (Personagem {posicao = (8,8), tamanho = (1,1), velocidade = (0,10)})

personagem2 = (Personagem {posicao = (0.5,6), tamanho = (1,1), velocidade = (5,10)})

personagem3 = (Personagem {posicao = (1,5), tamanho = (1,1), velocidade = (5,10)})

>>> estaNoAr mapa1 personagem1 
False

>>> estaNoAr mapa1 personagem2
True

>>> estaNoAr mapa1 personagem3
True

-}

temChao1 :: [[Bloco]] -> Personagem -> Bool
temChao1 [] _ = False
temChao1 lista@(y:ys) personagem
    | pontoDentroDaHitboxLista (x1,y1) listafiltrada2 || pontoDentroDaHitboxLista (x2,y1) listafiltrada2 = True
    | otherwise = temChao1 ys personagem
    where listafiltrada1 = filter (\ (x,y) -> x == Plataforma ) (calculaHitboxBlocos lista)
          listafiltrada2 = map snd listafiltrada1
          ((x1,y1),(x2,y2)) = hitboxPersonagem personagem

{-|

A função 'temChao1' verifica se a Hitbox do Personagem se toca inferiormente com alguma outra Hitbox de uma
qualquer Plataforma. Esta faz isto calculando a Hitbox de todos os Blocos do Mapa e filtrando apenas aqueles
que são Plataformas, de seguida, retira as Hitboxes dessas Plataformas e verifica se alguma delas se cruza
com a Hitbox do Personagem.

== Exemplos:

mapa1 = [[Vazio,Vazio, Alcapao, Vazio, Vazio, Vazio,Vazio,Vazio], 
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma, Alcapao,Vazio], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
        [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma, Plataforma, Plataforma, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Alcapao, Escada, Vazio, Vazio, Escada],
        [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Vazio]
        ]

personagem1 = (Personagem {posicao = (8,8), tamanho = (1,1), velocidade = (0,10)})

personagem2 = (Personagem {posicao = (0.5,6), tamanho = (1,1), velocidade = (5,10)})

personagem3 = (Personagem {posicao = (1,5), tamanho = (1,1), velocidade = (5,10)})

>>> temChao1 mapa1 personagem1 
False

>>> temChao1 mapa1 personagem2
True

>>> temChao1 mapa1 personagem3
True

-}

verificaSeEstaEmEscada :: Personagem -> [[Bloco]] -> Bool
verificaSeEstaEmEscada personagem [[]] = False
verificaSeEstaEmEscada personagem lista = listaDeHitboxesCruzam hitboxdapersonagem listadeescadas
    where listadeescadas = filter (\ (x,y) -> x == Escada ) listacalculada
          listacalculada = calculaHitboxBlocos lista
          hitboxdapersonagem = hitboxPersonagem personagem

{-|

A função verificaSeEstaEmEscada verifica se a hitbox de um Personagem cruza com a hitbox de uma Escada,
retornando um Bool. Ela usa a função calculaHitboxBlocos como auxiliar para calcular a lista de Hitboxes do Mapa 
e filtra esta lista para ter apenas as hitboxes das Escadas. Depois disto a função listadeHitboxesCruzam é 
chamada para verificar se a hitbox do Personagem se cruza com alguma destas hitboxes.

== Exemplos: 

personagem1 = (Personagem {tipo = Jogador , posicao = (10,10), tamanho = (1,1)})

personagem2 = (Personagem {tipo = Fantasma, posicao = (3,3), tamanho = (2,2)})

mapa1 =  [[Vazio,Vazio, Alcapao, Vazio, Vazio, Vazio,Vazio,Vazio], 
          [Plataforma,Escada,Plataforma,Plataforma,Plataforma,Plataforma, Alcapao,Vazio], 
          [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada], 
          [Escada, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
          [Plataforma, Plataforma, Plataforma, Escada, Vazio, Vazio, Escada],
          [Vazio, Vazio, Vazio, Alcapao, Escada, Vazio, Vazio, Escada],
          [Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada],
          [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Plataforma,Vazio]
         ]
 
>>> verificaSeEstaEmEscada personagem1 mapa1
False

>>> verificaSeEstaEmEscada personagem2 mapa1
True

-}

jogadorCruzaComColecionavel :: Personagem -> [(Colecionavel,Posicao)] -> (Int,(Bool,Double))
jogadorCruzaComColecionavel jogador@(Personagem { pontos = x , aplicaDano = (a,b)}) [] = (x,(a,b))
jogadorCruzaComColecionavel jogador@(Personagem { pontos = x , aplicaDano = (a,b)}) lista@((cole,(posx,posy)):xs)
    | hitboxesCruzam h hitboxdocolecionavel && cole == Martelo = (x,(True,50))
    | hitboxesCruzam h hitboxdocolecionavel && cole == Moeda = (x+100,(a,b))
    | otherwise = jogadorCruzaComColecionavel jogador xs
    where hitboxdocolecionavel = ((posx,posy),(posx+1,posy-1))
          h = hitboxPersonagem jogador

{-|

A função jogadorCruzaComColecionavel verifica se um jogador se cruza com um colecionavel e retorna as
consequências de ter apanhado um colecionável. Se apanhou um Martelo, a componente AplicaDano fica True por 10
segundos. Se apanhou uma Moeda, são adicionados 100 pontos aos pontos atuais do personagem. São usadas as 
funções hitboxesCruzam (para verificar se a Hitbox do Jogador se cruza com as dos Colecionaveis) e a função
hitboxPersonagem para calcular a Hitbox do Jogador.

== Exemplos:

col1 = [(Moeda, (4.0,3.0))]

col2 = [(Moeda, (5.0,7.0)),(Martelo,(3.5,6.7))]

col3 = [(Martelo,(2.0,3.0))]

jogador1 = (Personagem {tipo = Jogador, posicao = (4.5,3.5) , tamanho = (1,1), aplicaDano = (False,0), pontos = 69})

jogador2 = (Personagem {tipo = Jogador, posicao = (7.0,7.0) , tamanho = (1,1), aplicaDano = (False,0), pontos = 0})

jogador3 = (Personagem {tipo = Jogador, posicao = (2.0,3.0) , tamanho = (1,1), aplicaDano = (False,0), pontos = 0})

>>> jogadorCruzaComColecionavel jogador1 col1 
(169,(False,0.0))

>>> jogadorCruzaComColecionavel jogador2 col2
(0,(False,0.0))

>>> jogadorCruzaComColecionavel jogador3 col3
(0,(True,10.0))

-}

jogadorPerde1VidaAoSerAtingidoPorInimigo :: Personagem -> [Personagem] -> (Int,(Bool, Double))
jogadorPerde1VidaAoSerAtingidoPorInimigo jogador@(Personagem { vida = x} ) [] =  (x, invencibilidade jogador)
jogadorPerde1VidaAoSerAtingidoPorInimigo jogador@(Personagem { tipo = Jogador ,vida = x}) (inimigo:xs)
    | colisoesPersonagens jogador inimigo && fst (invencibilidade jogador) = (x, invencibilidade jogador)
    | colisoesPersonagens jogador inimigo = (x-1,(True, 5))
    | otherwise = jogadorPerde1VidaAoSerAtingidoPorInimigo jogador xs

{-|

A função 'jogadorPerde1VidaAoSerAtingidoPorInimigo' verifica se a hitbox de um Personagem (que será o Jogador) 
se cruza com a Hitbox de algum outro Personagem (que serão os Inimigos). Caso se verifique que sim, o valor da
sua vida terá de diminuir em 1 e define a invencibilidade do jogador para True, senão continuará igual.

== Exemplos: 

jogador1 = (Personagem {posicao = (2.7,3.2), vida = 3, tipo = Jogador, tamanho = (1,1) }) 

jogador2 = (Personagem {posicao = (6,10), vida = 2, tipo = Jogador, tamanho = (1,1) }) 

jogador3 = (Personagem {posicao = (5,7), vida = 1, tipo = Jogador, tamanho = (1,1) }) 

inimigo1 = (Personagem {posicao = (4,4), tamanho = (1,1), vida = 1})

inimigo2 = (Personagem {posicao = (6,6), tamanho = (1,1), vida = 1})

inimigo3 = (Personagem {posicao = (5,7), tamanho = (1,1), vida = 0})

listadeinimigos1 = [inimigo1,inimigo2,inimigo3]

listadeinimigos2 = [inimigo1,inimigo3]

>>> jogadorPerde1VidaAoSerAtingidoPorInimigo jogador1 listadeinimigos1
(3,(False, 0))

>>> jogadorPerde1VidaAoSerAtingidoPorInimigo jogador2 listadeinimigos1
(2,(False, 0))

>>> jogadorPerde1VidaAoSerAtingidoPorInimigo jogador3 listadeinimigos2
(0,(True, 5))

-}