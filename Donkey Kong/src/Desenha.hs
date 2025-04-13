module Desenha where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import Data.Maybe
import LI12324
import Estado
import Tarefa1
import Tarefa2
import Data.Bool (otherwise)
import Graphics.Gloss (Picture)

largura_janela :: Int
largura_janela = 1920

altura_janela :: Int
altura_janela = 1080

tamanho_bloco :: Int
tamanho_bloco = 8

tamanho_bloco_float :: Float
tamanho_bloco_float = fromIntegral tamanho_bloco

largura_janela_float :: Float
largura_janela_float = fromIntegral largura_janela

altura_janela_float :: Float
altura_janela_float = fromIntegral altura_janela

largura_mapa_float :: Mapa -> Float
largura_mapa_float (Mapa _ _ matriz_mapa) = fromIntegral $ length (head matriz_mapa) * tamanho_bloco

altura_mapa_float :: Mapa -> Float
altura_mapa_float (Mapa _ _ matriz_mapa) = fromIntegral $ length matriz_mapa * tamanho_bloco


desenha :: Estado -> IO Picture
desenha estado@(Estado {modo = Menu OpcaoNovoJogo, imagens = imgs}) = return $ Pictures [titulo imgs, opcaoNovoJogoSelec imgs, opcaoContinuarNaoSelec imgs, opcaoSairDoJogoNaoSelec imgs]
desenha estado@(Estado {modo = Menu OpcaoContinuar, imagens = imgs}) = return $ Pictures [titulo imgs, opcaoNovoJogoNaoSelec imgs, opcaoContinuarSelec imgs, opcaoSairDoJogoNaoSelec imgs]
desenha estado@(Estado {modo = Menu OpcaoSairDoJogo, imagens = imgs}) = return $ Pictures [titulo imgs, opcaoNovoJogoNaoSelec imgs, opcaoContinuarNaoSelec imgs, opcaoSairDoJogoSelec imgs]
desenha estado@(Estado {modo = FimJogo Win, imagens = imgs}) = return $ Pictures [winScreen imgs, opcaoVoltarParaMenuSelec imgs]
desenha estado@(Estado {modo = FimJogo GameOver, imagens = imgs}) = return $ Pictures [gameOverScreen imgs, opcaoTentarNovamenteSelec imgs]
desenha estado@(Estado {jogo = jogo@Jogo{jogador = jog, mapa = Mapa _ _ matriz}, imagens = imgs, modo = EmJogo, tempoDecorrido = tempo}) =
                                                       return $ Scale 3 3 $ translacao $ Pictures $
                                                       desenhaMapa (mapa jogo) imgs
                                                       ++ desenhaColecionaveis (colecionaveis jogo) imgs
                                                       ++ desenhaPersonagens (inimigos jogo++[jogador jogo]) tempo imgs
                                                       ++ desenhaBar (vida jog) (pontos jog)

  where translacao = Translate (-largura_mapa_float (mapa jogo)/2) (altura_mapa_float (mapa jogo)/2.3)

a mat per = (map (\x -> hitboxPersonagem x) per) ++ (map (\x -> snd x) (calculaHitboxBlocos mat))
desenhaHitboxes :: [Hitbox] -> [Picture]
desenhaHitboxes hitboxes = map (\hitbox -> desenhaHitbox hitbox) hitboxes

desenhaHitbox :: Hitbox -> Picture
desenhaHitbox ((x1,y1), (x2, y2)) = color white $ Translate (realToFrac x1) (realToFrac y1) $ Scale 8 8 $ line path
  where path = [(realToFrac x1,realToFrac y1), (realToFrac x1,realToFrac y2), (realToFrac x2,realToFrac y2), (realToFrac x2,realToFrac y1), (realToFrac x1,realToFrac y1)]

titulo :: Imagens -> Picture
titulo imgs = Translate 0 190 $ Scale 4 4 $ getImagem TituloIMG imgs

winScreen :: Imagens -> Picture
winScreen imgs = Translate 0 80 $ Scale 4 4 $ getImagem WinIMG imgs

gameOverScreen :: Imagens -> Picture
gameOverScreen imgs = Translate 0 80 $ Scale 4 4 $ getImagem GameoverIMG imgs

opcaoNovoJogoSelec :: Imagens -> Picture
opcaoNovoJogoSelec imgs = Translate 0 (-150) $ getImagem NovojogoselecIMG imgs

opcaoNovoJogoNaoSelec :: Imagens -> Picture
opcaoNovoJogoNaoSelec imgs = Translate 0 (-150) $ getImagem NovojogonaoselecIMG imgs

opcaoContinuarSelec :: Imagens -> Picture
opcaoContinuarSelec imgs = Translate 0 (-280) $ getImagem ContinuarselecIMG imgs

opcaoContinuarNaoSelec :: Imagens -> Picture
opcaoContinuarNaoSelec imgs = Translate 0 (-280) $ getImagem ContinuarnaoselecIMG imgs

opcaoSairDoJogoSelec :: Imagens -> Picture
opcaoSairDoJogoSelec imgs = Translate 0 (-410) $ getImagem SairdojogoselecIMG imgs

opcaoSairDoJogoNaoSelec :: Imagens -> Picture
opcaoSairDoJogoNaoSelec imgs = Translate 0 (-410) $ getImagem SairdojogonaoselecIMG imgs

opcaoTentarNovamenteSelec :: Imagens -> Picture
opcaoTentarNovamenteSelec imgs = Translate 0 (-280) $ getImagem TentarnovamenteselecIMG imgs

opcaoVoltarParaMenuSelec :: Imagens -> Picture
opcaoVoltarParaMenuSelec imgs = Translate 0 (-280) $ getImagem VoltarparamenuselecIMG imgs

selecionaMatrizMapa :: Mapa -> [[Bloco]]
selecionaMatrizMapa (Mapa _ _ matriz_blocos) = matriz_blocos

desenhaPersonagens :: [Personagem] -> Float -> Imagens -> [Picture]
desenhaPersonagens personagens tempo imgs = map (\personagem -> desenhaPersonagem personagem tempo imgs) personagens 

desenhaPersonagem :: Personagem -> Float -> Imagens -> Picture

-- desenha Mário Parado com Martelo
desenhaPersonagem (Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = pos, direcao = Oeste, aplicaDano = (True,_)}) _ imgs
  = desenhaImagem pos Mario1emIMG True imgs
desenhaPersonagem (Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = pos, direcao = Este, aplicaDano = (True, _)}) _ imgs
  = desenhaImagem pos Mario1dmIMG True imgs

-- desenha Mário em Movimento com Martelo
desenhaPersonagem (Personagem {tipo = Jogador, posicao = pos, direcao = direcao, aplicaDano = (True, _)}) tempo imgs
  | mod (floor tempo) 2 == 0 && direcao == Oeste = desenhaImagem pos Mario2emIMG True imgs
  | mod (floor tempo) 2 == 1 && direcao == Oeste = desenhaImagem pos Mario3emIMG True imgs
  | mod (floor tempo) 2 == 0 && direcao == Este = desenhaImagem pos Mario2dmIMG True imgs
  | mod (floor tempo) 2 == 1 && direcao == Este = desenhaImagem pos Mario3dmIMG True imgs

-- desenha Mário em Escada
desenhaPersonagem (Personagem {tipo = Jogador, posicao = pos, emEscada = True}) tempo imgs
  | mod (floor tempo) 2 == 0 = desenhaImagem pos Mario1escIMG True imgs
  | mod (floor tempo) 2 == 1 = desenhaImagem pos Mario2escIMG True imgs

-- desenha Mário Parado
desenhaPersonagem (Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = pos, direcao = Oeste}) _ imgs
  = desenhaImagem pos Mario1eIMG True imgs
desenhaPersonagem (Personagem {velocidade = (0.0, 0.0), tipo = Jogador, posicao = pos, direcao = Este}) _ imgs
  = desenhaImagem pos Mario1dIMG True imgs
  
-- desenha Mário em Movimento
desenhaPersonagem (Personagem {tipo = Jogador, posicao = pos, direcao = direcao}) tempo imgs
  | mod (floor tempo) 2 == 0 && direcao == Oeste = desenhaImagem pos Mario2eIMG True imgs
  | mod (floor tempo) 2 == 1 && direcao == Oeste = desenhaImagem pos Mario3eIMG True imgs
  | mod (floor tempo) 2 == 0 && direcao == Este = desenhaImagem pos Mario2dIMG True imgs
  | mod (floor tempo) 2 == 1 && direcao == Este = desenhaImagem pos Mario3dIMG True imgs

-- desenha Fantasma 
desenhaPersonagem (Personagem {tipo = Fantasma, posicao = pos, direcao = direcao}) tempo imgs
  | mod (floor tempo) 2 == 0 && direcao == Oeste = desenhaImagem pos Fantasma1eIMG True imgs
  | mod (floor tempo) 2 == 1 && direcao == Oeste = desenhaImagem pos Fantasma2eIMG True imgs
  | mod (floor tempo) 2 == 0 && direcao == Este = desenhaImagem pos Fantasma1dIMG True imgs
  | mod (floor tempo) 2 == 1 && direcao == Este = desenhaImagem pos Fantasma2dIMG True imgs

-- desenha Macaco Parado
desenhaPersonagem (Personagem {tipo = MacacoMalvado, posicao = pos}) _ imgs
  = desenhaImagem pos MacacoIMG False imgs

desenhaImagem :: Posicao -> Imagem -> Bool -> Imagens -> Picture
desenhaImagem (x, y) img translacionaPersonagem imgs 
  | translacionaPersonagem = Translate (realToFrac x*tamanho_bloco_float) (-realToFrac y*tamanho_bloco_float+tamanho_bloco_float) $ Scale 0.5 0.5 $ getImagem img imgs
  | otherwise = Translate (realToFrac x*tamanho_bloco_float) (-realToFrac y*tamanho_bloco_float) $ Scale 0.5 0.5 $ getImagem img imgs


desenhaColecionaveis :: [(Colecionavel, Posicao)] -> Imagens -> [Picture]
desenhaColecionaveis colecionaveis imgs = map (\colecionavel -> desenhaColecionavel colecionavel imgs) colecionaveis

desenhaColecionavel :: (Colecionavel, Posicao) -> Imagens -> Picture
desenhaColecionavel (Moeda, pos) imgs = desenhaImagem pos MoedaIMG False imgs
desenhaColecionavel (Martelo, pos) imgs = desenhaImagem pos MarteloIMG False imgs

desenhaMapa :: Mapa -> Imagens -> [Picture]
desenhaMapa mapa_jogo imgs =  desenhaMapaAux matriz_blocos (0,0) imgs
  where matriz_blocos = selecionaMatrizMapa $ mapa_jogo

desenhaMapaAux :: [[Bloco]] -> (Int, Int)-> Imagens -> [Picture]
desenhaMapaAux [] _ _ = []
desenhaMapaAux (linha:linhas) (x, y) imgs = (translacao (Pictures (desenhaLinha linha (x, y) imgs))) : desenhaMapaAux linhas (x, y-1) imgs
    where translacao = Translate 0 (fromIntegral (y*tamanho_bloco))

desenhaLinha :: [Bloco] -> (Int, Int) -> Imagens -> [Picture]
desenhaLinha [] _ _ = []
desenhaLinha (bloco:blocos) (x, y) imgs = (translacao (blocoParaImagem bloco imgs)) : desenhaLinha blocos (x+1, y) imgs
    where translacao = Translate (fromIntegral (x*tamanho_bloco)) 0

desenhaBar :: Int -> Int -> [Picture]
desenhaBar vida pontos = [Translate 0 5 $ Scale 0.05 0.05 $ color white $ text ("HP: " ++ show vida)
                         ,Translate 175 5 $ Scale 0.05 0.05 $ color white $ text ("Pontos: " ++ show pontos)]

blocoParaImagem :: Bloco -> Imagens -> Picture
blocoParaImagem Plataforma imgs = getImagem PlataformaIMG imgs
blocoParaImagem Escada imgs = getImagem EscadaIMG imgs
blocoParaImagem Alcapao imgs = getImagem AlcapaoIMG imgs
blocoParaImagem Vazio imgs = getImagem VazioIMG imgs

getImagem :: Imagem -> Imagens -> Picture
getImagem chave dicionario = fromJust $ lookup chave dicionario

carregarIMG :: IO Imagens
carregarIMG = do
  -- Mário esquerda e direita (1 -> parado, 2 e 3 -> movimento)
  mario1e <- loadBMP "sprites/mario1e.bmp"
  mario2e <- loadBMP "sprites/mario2e.bmp"
  mario3e <- loadBMP "sprites/mario3e.bmp"
  mario1d <- loadBMP "sprites/mario1d.bmp"
  mario2d <- loadBMP "sprites/mario2d.bmp"
  mario3d <- loadBMP "sprites/mario3d.bmp"

  -- Mário esquerda e direita com martelo (1 -> parado, 2 e 3 -> movimento)
  mario1em <- loadBMP "sprites/mario1em.bmp"
  mario2em <- loadBMP "sprites/mario2em.bmp"
  mario3em <- loadBMP "sprites/mario3em.bmp"
  mario1dm <- loadBMP "sprites/mario1dm.bmp"
  mario2dm <- loadBMP "sprites/mario2dm.bmp"
  mario3dm <- loadBMP "sprites/mario3dm.bmp"

  -- Mário em escada
  mario1esc <- loadBMP "sprites/mario1esc.bmp"
  mario2esc <- loadBMP "sprites/mario2esc.bmp"

  -- Mário Game Over
  mariogameover <- loadBMP "sprites/mariogameover.bmp"

  -- Fantasma esquerda e direita (movimento)
  fantasma1e <- loadBMP "sprites/fantasma1e.bmp"
  fantasma2e <- loadBMP "sprites/fantasma2e.bmp"
  fantasma1d <- loadBMP "sprites/fantasma1d.bmp"
  fantasma2d <- loadBMP "sprites/fantasma2d.bmp"

  -- Macaco
  macaco <- loadBMP "sprites/macaco.bmp"

  -- Colecionáveis
  moeda <- loadBMP "sprites/moeda.bmp"
  martelo <- loadBMP "sprites/martelo.bmp"

  -- Blocos
  plataforma <- loadBMP "sprites/plataforma.bmp"
  escada <- loadBMP "sprites/escada.bmp"
  alcapao <- loadBMP "sprites/alcapao.bmp"
  vazio <- loadBMP "sprites/vazio.bmp"

  -- Telas
  titulo <- loadBMP "sprites/titulo.bmp"
  win <- loadBMP "sprites/win.bmp"
  gameover <- loadBMP "sprites/gameover.bmp"

  -- Botões
  novojogoselec <- loadBMP "sprites/novojogoselec.bmp"
  novojogonaoselec <- loadBMP "sprites/novojogonaoselec.bmp"
  continuarselec <- loadBMP "sprites/continuarselec.bmp"
  continuarnaoselec <- loadBMP "sprites/continuarnaoselec.bmp"
  sairdojogoselec <- loadBMP "sprites/sairdojogoselec.bmp"
  sairdojogonaoselec <- loadBMP "sprites/sairdojogonaoselec.bmp"
  tentarnovamenteselec <- loadBMP "sprites/tentarnovamenteselec.bmp"
  voltarparamenuselec <- loadBMP "sprites/voltarparamenuselec.bmp"

  return $ [(Mario1eIMG, mario1e), (Mario2eIMG, mario2e), (Mario3eIMG, mario3e), (Mario1dIMG, mario1d), (Mario2dIMG, mario2d), (Mario3dIMG, mario3d)
           ,(Mario1emIMG, mario1em), (Mario2emIMG, mario2em), (Mario3emIMG, mario3em), (Mario1dmIMG, mario1dm), (Mario2dmIMG, mario2dm), (Mario3dmIMG, mario3dm)
           ,(Mario1escIMG, mario1esc), (Mario2escIMG, mario2esc)
           ,(MariogameoverIMG, mariogameover)
           ,(Fantasma1eIMG, fantasma1e), (Fantasma2eIMG, fantasma2e), (Fantasma1dIMG, fantasma1d), (Fantasma2dIMG, fantasma2d)
           ,(MacacoIMG, macaco)
           ,(MoedaIMG, moeda), (MarteloIMG, martelo)
           ,(PlataformaIMG, plataforma), (EscadaIMG, escada), (AlcapaoIMG, alcapao), (VazioIMG, vazio)
           ,(TituloIMG, titulo), (WinIMG, win), (GameoverIMG, gameover)
           ,(NovojogoselecIMG, novojogoselec), (NovojogonaoselecIMG, novojogonaoselec), (ContinuarselecIMG, continuarselec), (ContinuarnaoselecIMG, continuarnaoselec), (SairdojogoselecIMG, sairdojogoselec), (SairdojogonaoselecIMG, sairdojogonaoselec), (TentarnovamenteselecIMG, tentarnovamenteselec), (VoltarparamenuselecIMG, voltarparamenuselec)
           ]