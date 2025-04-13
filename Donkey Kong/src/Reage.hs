module Reage where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import System.Exit (exitFailure)
import LI12324
import Tarefa3
import Tarefa4
import Jogo
import Estado
import Desenha

reage :: Event -> Estado -> IO Estado
reage evento estado@(Estado {modo = Menu opcao}) = reageMenu evento estado
reage evento estado@(Estado {modo = EmJogo}) = reageJogo evento estado
reage evento estado@(Estado {modo = FimJogo opcaoFimJogo}) = reageFimJogo evento estado

reageJogo :: Event -> Estado -> IO Estado
reageJogo (EventKey (SpecialKey KeyUp) Down _ _) estado = 
    novoJogo Saltar estado
reageJogo (EventKey (SpecialKey KeyDown) Down _ _) estado = 
    novoJogo Descer estado
reageJogo (EventKey (SpecialKey KeyRight) Down _ _ ) estado = 
    novoJogo AndarDireita estado 
reageJogo (EventKey (SpecialKey KeyLeft) Down _ _ ) estado = 
    novoJogo AndarEsquerda estado 
reageJogo (EventKey (SpecialKey KeyEsc) Down _ _ ) estado =
    return estado {modo = Menu OpcaoNovoJogo}
reageJogo (EventKey (SpecialKey _) Up _ _) estado = 
    novoJogo Parar estado
reageJogo _ e = return e

novoJogo :: Acao -> Estado -> IO Estado
novoJogo acao e@(Estado {jogo = jogo@(Jogo {inimigos = listadeinimigos , colecionaveis = listadecolecionaveis})}) = 
    return $ e {jogo = atualiza (decideDirecaoFantasma 10 (length listadeinimigos)) (Just acao) jogo}

reageMenu :: Event -> Estado -> IO Estado
-- Reações ao botão Novo Jogo
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {modo = Menu OpcaoNovoJogo, jogoInicial = jogoI, jogo=jogo}) =
    return e {modo = EmJogo, jogo = jogoI}
reageMenu (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado {modo = Menu OpcaoNovoJogo}) =
    return e  {modo = Menu OpcaoContinuar}

-- Reações ao botão Continuar
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {modo = Menu OpcaoContinuar}) =
    return e  {modo = EmJogo}
reageMenu (EventKey (SpecialKey KeyUp) Down _ _) e@(Estado {modo = Menu OpcaoContinuar}) =
    return e  {modo = Menu OpcaoNovoJogo}
reageMenu (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado {modo = Menu OpcaoContinuar}) =
    return e  {modo = Menu OpcaoSairDoJogo}

-- Reações ao botão Sair Do Jogo
reageMenu (EventKey (SpecialKey KeyEnter) Down _ _) (Estado {modo = Menu OpcaoSairDoJogo}) = 
    exitFailure
reageMenu (EventKey (SpecialKey KeyUp) Down _ _) e@(Estado {modo = Menu OpcaoSairDoJogo}) =
    return e  {modo = Menu OpcaoContinuar}
reageMenu _ e = return e

reageFimJogo :: Event -> Estado -> IO Estado
reageFimJogo (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {modo = FimJogo Win, jogoInicial = jogoI, jogo=jogo}) =
    return e {modo = Menu OpcaoNovoJogo, jogo = jogoI}
reageFimJogo (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {modo = FimJogo GameOver, jogoInicial = jogoI, jogo=jogo}) =
    return e {modo = EmJogo, jogo = jogoI}
reageFimJogo _ e = return e

decideReal :: [Int] -> [Maybe Acao]
decideReal [] = []
decideReal (x:xs) = case mod x 4 of 
    0 -> Just AndarDireita : decideReal xs
    1 ->  Just AndarEsquerda : decideReal xs
    2 -> Just Parar : decideReal xs
    3 -> Nothing : decideReal xs 

decideDirecaoFantasma :: Int -> Int -> [Maybe Acao]
decideDirecaoFantasma x y = decideReal (geraAleatorios x y)