{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : José Lourenço Ferreira Fernandes <a106937@alunos.uminho.pt>
              Duarte Escairo Brandão Reis Silva <a106936@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}

module Tarefa1 where
import LI12324

---------------------------------------------------------------------------------------------------------

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede a@(Mapa _ _ mapa) personagem = not (listaDeHitboxesCruzam hitboxdapersonagem listafiltrada) || personagemDentroDoMapa mapa personagem
   where listaHitboxesMapa = calculaHitboxBlocos mapa
         listafiltrada = filter (\ (x,y) -> x == Plataforma) listaHitboxesMapa
         hitboxdapersonagem = hitboxPersonagem personagem

{-|

A função 'colisoesParede' deteta se um personagem está dentro dos limites de um mapa e
se a sua hitbox se encontra em contacto com alguma outra hitbox de uma Plataforma seja
lateralmente, superiormente ou inferiormente.

== Exemplos:

exemploMapa1 = [[Plataforma, Plataforma, Plataforma],
                [Plataforma, Plataforma, Plataforma],
                [Plataforma, Plataforma, Plataforma],
                [Plataforma, Escada, Plataforma],
                [Plataforma, Escada, Plataforma],
                [Plataforma, Escada, Plataforma],
                [Plataforma, Escada, Plataforma],
                [Plataforma, Plataforma, Plataforma]]

>>> colisoesParede (Mapa ((2,2),Este) (1,1) exemploMapa1) (Personagem { posicao = (1,7) , tamanho = (1,1) } )  
True

>>> exemploMapa2 =
  [[Plataforma, Plataforma, Plataforma, Plataforma],
   [Plataforma, Plataforma, Plataforma, Plataforma],
   [Plataforma, Plataforma, Plataforma, Plataforma],
   [Plataforma, Escada, Plataforma, Plataforma],
   [Plataforma, Escada, Plataforma, Plataforma],
   [Vazio, Vazio, Vazio, Plataforma],
   [Vazio, Vazio, Vazio, Plataforma],
   [Vazio, Vazio, Vazio, Plataforma]]

>>> colisoesParede (Mapa ((2,2),Este) (1,1) exemploMapa2) (Personagem { posicao = (2,6)  , tamanho = (1,1) } )
False

-}

listaDeHitboxesCruzam :: Hitbox -> [(Bloco,Hitbox)] -> Bool
listaDeHitboxesCruzam x [] = False
listaDeHitboxesCruzam x ((y,z):us)
  | hitboxesCruzam x z = True
  | otherwise = listaDeHitboxesCruzam x us

{-|

A função 'listaDHitboxesCruzam' verifica se uma hitbox se cruza com uma lista de pares de Bloco 
e a sua respetiva hitbox, usando a função hitboxesCruzam para testar cada uma das hitboxes e esgotando a lista.
A ideia é usar esta função para verificar se um personagem está a tocar numa Plataforma ou algum
outro tipo de bloco específico, depois de filtrada a lista inicial.

== Exemplos :

hitbox = ((3,3),(4,2))

lista = [(Vazio,((0.0,1.0),(1.0,0.0))),(Vazio,((1.0,1.0),(2.0,0.0))),(Vazio,((2.0,1.0),(3.0,0.0))),(Vazio,((3.0,1.0),(4.0,0.0))),(Vazio,((4.0,1.0),(5.0,0.0))),(Vazio,((5.0,1.0),(6.0,0.0))),(Vazio,((6.0,1.0),(7.0,0.0))),(Vazio,((7.0,1.0),(8.0,0.0))),(Plataforma,((0.0,2.0),(1.0,1.0))),(Plataforma,((1.0,2.0),(2.0,1.0))),(Plataforma,((2.0,2.0),(3.0,1.0))),(Plataforma,((3.0,2.0),(4.0,1.0))),(Plataforma,((4.0,2.0),(5.0,1.0))),(Plataforma,((5.0,2.0),(6.0,1.0))),(Plataforma,((6.0,2.0),(7.0,1.0))),(Plataforma,((7.0,2.0),(8.0,1.0))),(Vazio,((0.0,3.0),(1.0,2.0))),(Vazio,((1.0,3.0),(2.0,2.0))),(Vazio,((2.0,3.0),(3.0,2.0))),(Vazio,((3.0,3.0),(4.0,2.0))),(Escada,((4.0,3.0),(5.0,2.0))),(Vazio,((5.0,3.0),(6.0,2.0))),(Vazio,((6.0,3.0),(7.0,2.0))),(Escada,((7.0,3.0),(8.0,2.0))),(Vazio,((0.0,4.0),(1.0,3.0))),(Vazio,((1.0,4.0),(2.0,3.0))),(Vazio,((2.0,4.0),(3.0,3.0))),(Vazio,((3.0,4.0),(4.0,3.0))),(Escada,((4.0,4.0),(5.0,3.0))),(Vazio,((5.0,4.0),(6.0,3.0))),(Vazio,((6.0,4.0),(7.0,3.0))),(Alcapao,((7.0,4.0),(8.0,3.0))),(Plataforma,((0.0,5.0),(1.0,4.0))),(Alcapao,((1.0,5.0),(2.0,4.0))),(Alcapao,((2.0,5.0),(3.0,4.0))),(Plataforma,((3.0,5.0),(4.0,4.0))),(Escada,((4.0,5.0),(5.0,4.0))),(Vazio,((5.0,5.0),(6.0,4.0))),(Vazio,((6.0,5.0),(7.0,4.0))),(Vazio,((7.0,5.0),(8.0,4.0))),(Vazio,((0.0,6.0),(1.0,5.0))),(Vazio,((1.0,6.0),(2.0,5.0))),(Vazio,((2.0,6.0),(3.0,5.0))),(Vazio,((3.0,6.0),(4.0,5.0))),(Escada,((4.0,6.0),(5.0,5.0))),(Vazio,((5.0,6.0),(6.0,5.0))),(Vazio,((6.0,6.0),(7.0,5.0))),(Escada,((7.0,6.0),(8.0,5.0))),(Vazio,((0.0,7.0),(1.0,6.0))),(Vazio,((1.0,7.0),(2.0,6.0))),(Vazio,((2.0,7.0),(3.0,6.0))),(Vazio,((3.0,7.0),(4.0,6.0))),(Escada,((4.0,7.0),(5.0,6.0))),(Vazio,((5.0,7.0),(6.0,6.0))),(Vazio,((6.0,7.0),(7.0,6.0))),(Escada,((7.0,7.0),(8.0,6.0))),(Plataforma,((0.0,8.0),(1.0,7.0))),(Plataforma,((1.0,8.0),(2.0,7.0))),(Plataforma,((2.0,8.0),(3.0,7.0))),(Plataforma,((3.0,8.0),(4.0,7.0))),(Plataforma,((4.0,8.0),(5.0,7.0))),(Plataforma,((5.0,8.0),(6.0,7.0))),(Plataforma,((6.0,8.0),(7.0,7.0))),(Plataforma,((7.0,8.0),(8.0,7.0)))]

>>> listaDeHitboxesCruzam hitbox lista 
True

hitbox2 = ((5,5),(6,4))

lista2 = [(Vazio,((0.0,1.0),(1.0,0.0))),(Vazio,((1.0,1.0),(2.0,0.0))),(Vazio,((2.0,1.0),(3.0,0.0))),(Vazio,((3.0,1.0),(4.0,0.0))),(Vazio,((4.0,1.0),(5.0,0.0))),(Vazio,((5.0,1.0),(6.0,0.0))),(Vazio,((6.0,1.0),(7.0,0.0))),(Vazio,((7.0,1.0),(8.0,0.0))),(Plataforma,((0.0,2.0),(1.0,1.0))),(Plataforma,((1.0,2.0),(2.0,1.0))),(Plataforma,((2.0,2.0),(3.0,1.0))),(Plataforma,((3.0,2.0),(4.0,1.0))),(Plataforma,((4.0,2.0),(5.0,1.0))),(Plataforma,((5.0,2.0),(6.0,1.0))),(Plataforma,((6.0,2.0),(7.0,1.0))),(Plataforma,((7.0,2.0),(8.0,1.0))),(Vazio,((0.0,3.0),(1.0,2.0))),(Vazio,((1.0,3.0),(2.0,2.0))),(Vazio,((2.0,3.0),(3.0,2.0))),(Vazio,((3.0,3.0),(4.0,2.0))),(Escada,((4.0,3.0),(5.0,2.0))),(Vazio,((5.0,3.0),(6.0,2.0))),(Vazio,((6.0,3.0),(7.0,2.0))),(Escada,((7.0,3.0),(8.0,2.0)))]

>>> listaDeHitboxesCruzam hitbox2 lista2 
False

-}

personagemDentroDoMapa :: [[Bloco]] -> Personagem -> Bool
personagemDentroDoMapa  mapa@(x:xs)  personagem =
  x1 >= 0 &&
  fromIntegral (length x+1) >= x2 &&
  y2 >= 0 &&
  fromIntegral (length mapa) >= y1
  where ((x1,y1),(x2,y2)) = hitboxPersonagem personagem

{-|

A função 'personagemDentroDoMapa' verifica se um personagem está dentro do mapa. 
Dado um Mapa e uma Personagem a hitbox do personagem é calculada usando a função hitboxPersonagem.
Tendo a hitbox em conta são extraídos os valores das coordenadas x e y dos 2 pontos que constituem a hitbox.
Se:
  - o x do ponto esquerdo da hitbox for positivo e diferente ou igual a zero
  - o y do ponto esquerdo da hitbox for positivo e diferente ou igual a zero
  - o x do ponto direito da hitbox for diferente ou igual à largura do mapa (length x)
  - o y do ponto direito da hitbox for diferente ou igual à altura do mapa (length mapa)
  a função devolve um valor verdadeiro, caso contrário a função devolve um valor falso.

== Exemplos:

>>> exemplomapa1 = [[Plataforma, Plataforma, Plataforma],
                   [Plataforma, Plataforma, Plataforma],
                   [Plataforma, Plataforma, Plataforma],
                   [Plataforma, Plataforma, Plataforma],
                   [Plataforma, Plataforma, Plataforma],
                   [Plataforma, Plataforma, Plataforma],
                   [Plataforma, Plataforma, Plataforma],
                   [Plataforma, Plataforma, Plataforma]]

exemplopersonagem1 = Personagem { posicao = (-1, 0), tamanho = (1, 1) }

>>> personagemDentroDoMapa exemplomapa1 exemplopersonagem1
False

exemplopersonagem2 = Personagem { posicao = (1.9, 6), tamanho = (1, 1) }

>>> personagemDentroDoMapa exemplomapa1 exemplopersonagem2
True

-}

calculaHitboxBlocos :: [[Bloco]] -> [(Bloco, Hitbox)]
calculaHitboxBlocos = concatMap hitboxesLinha . zip [1..]

{-|

A função 'calculaHitboxBlocos' recebe uma matriz de blocos e calcula
as hitboxes associadas a cada bloco, considerando as posições na matriz.
Ela utiliza a função 'hitboxesLinha' sobre as linhas da matriz, 
que por sua vez chama a 'hitboxBloco' sobre cada bloco na linha.

== Exemplos:

>>> calculaHitboxBlocos [[Plataforma],[Escada],[Alcapao]]
[(Plataforma,((0.0,1.0),(1.0,0.0))),(Escada,((0.0,2.0),(1.0,1.0))),(Alcapao,((0.0,3.0),(1.0,2.0)))]

>>> calculaHitboxBlocos [[]]  
[]

>>> calculaHitboxBlocos [[Plataforma,Plataforma,Plataforma,Plataforma,Alcapao,Escada]]
[(Plataforma,((0.0,1.0),(1.0,0.0))),(Plataforma,((1.0,1.0),(2.0,0.0))),(Plataforma,((2.0,1.0),(3.0,0.0))),(Plataforma,((3.0,1.0),(4.0,0.0))),(Alcapao,((4.0,1.0),(5.0,0.0))),(Escada,((5.0,1.0),(6.0,0.0)))]

-}

hitboxesLinha :: (Double, [Bloco]) -> [(Bloco, Hitbox)]
hitboxesLinha (y, linha) =
  concatMap (\(x, bloco) -> map (\hitbox -> (bloco, hitbox)) (hitboxBloco x y bloco)) (zip [0..] linha)

{-|

A função 'hitboxesLinha' recebe uma tupla contendo um valor 'y' 
(a posição na vertical desse Bloco) e uma lista de blocos. Ela mapeia a função
'hitboxBloco' sobre cada bloco na linha, produzindo uma lista de tuplas (Bloco, Hitbox).
A ideia é que mapeia uma linha de Blocos e retorne as suas hitboxes.

== Exemplos:

>>> hitboxesLinha (1, [Plataforma, Escada, Plataforma, Alcapao])
[(Plataforma,((0.0,1.0),(1.0,0.0))),(Escada,((1.0,1.0),(2.0,0.0))),(Plataforma,((2.0,1.0),(3.0,0.0))),(Alcapao,((3.0,1.0),(4.0,0.0)))]

>>> hitboxesLinha (2, [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma])
[(Plataforma,((0.0,2.0),(1.0,1.0))),(Plataforma,((1.0,2.0),(2.0,1.0))),(Plataforma,((2.0,2.0),(3.0,1.0))),(Plataforma,((3.0,2.0),(4.0,1.0))),(Plataforma,((4.0,2.0),(5.0,1.0)))]

-}

hitboxBloco :: Double -> Double -> Bloco -> [Hitbox]
hitboxBloco x y _ = [((x, y), (x + 1, y - 1))]

{-|

A função 'hitboxBloco' recebe uma posição horizontal 'x', uma posição vertical 'y',
e um bloco. Ela retorna uma lista de Hitbox que representa a hitbox associada ao bloco na posição dada.

== Exemplos:

>>> hitboxBloco 0.0 1.0 Plataforma
[((0.0,1.0),(1.0,0.0))]

>>> hitboxBloco 2.0 2.0 Vazio
[((2.0,2.0),(3.0,1.0))]

>>> hitboxBloco 6.0 4.0 Alcapao
[((6.0,4.0),(7.0,3.0))]

>>> hitboxBloco 40.0 30.0 Escada
[((40.0,30.0),(41.0,29.0))]

-}

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens a@(Personagem {vida = 0}) _ = False
colisoesPersonagens _ a@(Personagem {vida = 0}) = False
colisoesPersonagens personagem1 personagem2 = hitboxesCruzam (hitboxPersonagem personagem1) (hitboxPersonagem personagem2)

{-|

A função 'colisoesPersonagens' verfifica se duas hitboxes de personagens se cruzam usando 
a função 'hitboxpersonagem' para calcular a hitbox de duas personagens e a função 'hitboxesCruzam' para
verificar se essas hitboxes se cruzam. 
Caso um dos personagens envolvido nesse teste de colisão tenha vida 0 então nunca colide com nenhum outro.

== Exemplos:

>>> colisoesPersonagens Personagem { posicao = (2, 6), tamanho = (1, 1), vida = 3  } Personagem { posicao = (3,4), tamanho = (1,1), vida = 3 }
False

>>> colisoesPersonagens Personagem { posicao = (2,2), tamanho = (1, 1), vida = 4} Personagem { posicao = (2,1), tamanho = (1,1), vida = 2}
True

>>> colisoesPersonagens Personagem { posicao = (2,2), tamanho = (1, 1), vida = 0} Personagem { posicao = (2,1), tamanho = (1,1), vida = 2}
False

>>> colisoesPersonagens Personagem { posicao = (2,2), tamanho = (1, 1), vida = 4} Personagem { posicao = (2,1), tamanho = (1,1), vida = 0}
False

-}

hitboxPersonagem :: Personagem -> Hitbox
hitboxPersonagem (Personagem {posicao =(posx,posy), tamanho = (tamx,tamy), tipo = Jogador}) = ((posx,posy),(posx+tamx,posy-tamy))
hitboxPersonagem (Personagem {posicao =(posx,posy), tamanho = (tamx,tamy), tipo = Fantasma}) = ((posx,posy+1),(posx+tamx,posy-tamy))
hitboxPersonagem (Personagem {posicao =(posx,posy), tamanho = (tamx,tamy), tipo = MacacoMalvado}) = ((posx,posy+1),(posx+tamx,posy-tamy+0.1))

{-|

A função 'hitboxPersonagem' calcula a hitbox (em que o primeiro par é o ponto 
inferior esquerdo da hitbox e o segundo par é o ponto superior direito da hitbox) 
de uma personagem dada a sua altura e a sua largura e a sua posicao no mapa.

== Exemplos:

>>> hitboxPersonagem (Personagem {posicao = (3,3) , tamanho = (2,2) } )
((3.0,3.0),(5.0,1.0))

>>> hitboxPersonagem (Personagem {posicao = (17,34) ,tamanho = (1,1) } )
((17.0,34.0),(18.0,33.0))

-}

hitboxesCruzam :: Hitbox -> Hitbox -> Bool
hitboxesCruzam (ponto1@(x1,y1),ponto2@(x2,y2)) hitbox =
  pontoDentroDaHitbox ponto1 hitbox || pontoDentroDaHitbox ponto2 hitbox || pontoDentroDaHitbox ponto3 hitbox || pontoDentroDaHitbox ponto4 hitbox
  where ponto3 = (x2,y1)
        ponto4 = (x1,y2)

{-|

A função 'hitboxesCruzam' verifica se duas hitboxes se cruzam, usando a função pontoDentroDaHitbox. 
A lógica usada é: se um dos cantos de uma hitbox estiver dentro de uma outra hitbox então é verdadeiro.

== Exemplos:

>>> hitboxesCruzam ((2,2),(3,1)) ((1,3),(2,2))
True

>>> hitboxesCruzam ((2,2),(3,1)) ((5,5),(6,4))
False

-}

pontoDentroDaHitbox :: Posicao -> Hitbox -> Bool
pontoDentroDaHitbox (x3,y3) ((x1,y1),(x2,y2)) = x1 <= x3 && x3 <= x2 && y2 <= y3 && y3 <= y1

{-|

A função 'pontoDentroDaHitbox' verifica se um ponto está dentro de uma hitbox.

== Exemplos:

>>> pontoDentroDaHitbox (1,1) ((5,5),(6,4))
False

>>> pontoDentroDaHitbox (2,2) ((1.5,2.5),(2.5,1.5))
True

-}

hitboxesDosColecionaveis :: [(Colecionavel,Posicao)] -> [Hitbox]
hitboxesDosColecionaveis [] = []
hitboxesDosColecionaveis a@((_,(u,f)):xs) = ((u,f+0.1),(u+0.5,f-0.5)) : hitboxesDosColecionaveis xs

{-|

A função 'hitboxesDosColecionaveis' recebe como argumento uma lista de colecionáveis e
 as suas posições e retorna a lista das suas hitboxes

== Exemplos:

>>> hitboxesDosColecionaveis [(Moeda, (3.0,3.0)), (Martelo, (5.0, 4.0))]
[((3.0,3.0),(4.0,2.0)),((5.0,4.0),(6.0,3.0))]

-}

pontoDentroDaHitboxLista :: Posicao -> [Hitbox] -> Bool
pontoDentroDaHitboxLista _ [] = False
pontoDentroDaHitboxLista (x,y) (a:as)
  | pontoDentroDaHitbox (x,y) a = True
  | otherwise = pontoDentroDaHitboxLista (x,y) as

{-|

A função 'pontoDentroDaHitboxlista' verifica se um dado ponto se cruza com uma qualquer Hitbox de uma lista.
Esta usa a função pontoDentroDaHitbox, que trata da lógica da interseção, aplicando esta função a cada elemento
da lista.

== Exemplos:

ponto1 = (3.7, 4.2)

ponto2 = (5.7, 4.5)

lista1 = []

lista2 =  [ ((0.5, 1.0), (2.5, 3.0)), 
            ((1.2, 2.3), (3.4, 4.5)), 
            ((2.0, 3.0), (4.5, 5.5)),
            ((5.0, 5.0), (6.0,4.0)),
            ((3.5, 4.5), (6.0, 7.2)), 
            ((4.7, 5.9), (8.1, 9.3)), 
            ((5.2, 6.4), (10.0, 11.2)), 
            ((6.6, 7.8), (12.5, 13.7))]

>>> pontoDentroDaHitboxlista ponto1 lista1
False

>>> pontoDentroDaHitboxlista ponto2 lista2
True

-}