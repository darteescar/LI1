# Donkey Kong

Projeto de grupo desenvolvido no âmbito da UC de LI!.

O jogo consiste em controlar um personagem até à saída de um mapa (representada por uma estrela, geralmente no topo), pelo meio enfrentando ou esquivando-se de inimigos. O jogo termina assim que o jogador alcance a saída (caso de vitória) ou perca as vidas em confrontos com os inimigos (caso de derrota.)

## Membros do grupo

* [darteescar](https://github.com/darteescar)
* [JoseLourencoFernandes](https://github.com/JoseLourencoFernandes)

## Executável

Pode compilar e executar o programa através dos comandos `build` e `run` do `cabal`.

```bash
cabal run primate-kong
```

## Interpretador

Pode abrir o interpretador do Haskell (GHCi) utilizando o cabal com o projecto automaticamente carregado.

```bash
cabal repl
```

## Testes

O projecto utiliza a biblioteca [HUnit](https://hackage.haskell.org/package/HUnit) para fazer testes unitários.

Pode correr os testes utilizando o seguinte comando.

```bash
cabal test
```

Se pretender executar os exemplos da documentação como testes unitários utiliza-se a biblioteca [Doctest](https://hackage.haskell.org/package/doctest).

```bash
cabal repl --build-depends=QuickCheck,doctest --with-ghc=doctest
```

## Documentação

Pode gerar a documentação com o [Haddock](https://haskell-haddock.readthedocs.io/).

```bash
cabal haddock
```
