module Estado where
    
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Pure.Game
import LI12324

data Estado
  = Estado
  { jogoInicial :: Jogo,
    jogo :: Jogo,
    imagens :: Imagens,
    modo :: Modo,
    tempoDecorrido :: Float
  }

data Modo
  = Menu Opcoes
  | EmJogo
  | FimJogo OpcoesFimJogo

data Opcoes
  = OpcaoNovoJogo
  | OpcaoContinuar
  | OpcaoSairDoJogo

data OpcoesFimJogo
  = Win 
  | GameOver 

data Imagem 
  = Mario1eIMG | Mario2eIMG | Mario3eIMG | Mario1dIMG | Mario2dIMG | Mario3dIMG 
  | Mario1emIMG | Mario2emIMG | Mario3emIMG | Mario1dmIMG | Mario2dmIMG | Mario3dmIMG
  | Mario1escIMG | Mario2escIMG
  | MariogameoverIMG
  | Fantasma1eIMG | Fantasma2eIMG | Fantasma1dIMG | Fantasma2dIMG
  | MacacoIMG
  | MoedaIMG | MarteloIMG
  | PlataformaIMG | EscadaIMG | AlcapaoIMG | VazioIMG
  | TituloIMG | WinIMG | GameoverIMG
  | NovojogoselecIMG | NovojogonaoselecIMG | ContinuarselecIMG | ContinuarnaoselecIMG | SairdojogoselecIMG | SairdojogonaoselecIMG | TentarnovamenteselecIMG | VoltarparamenuselecIMG
  deriving (Eq, Show)

type Imagens = [(Imagem, Picture)]