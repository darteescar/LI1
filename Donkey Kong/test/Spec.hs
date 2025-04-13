module Main where

import Test.HUnit
import Tarefa1Spec
import Tarefa2Spec
import Tarefa3Spec
import Tarefa4Spec
import ModelosParaTestes

main :: IO ()
main = runTestTTAndExit $ test [testes_tarefa_1
                               ,testes_tarefa_2
                               ,testes_tarefa_3
                               ,testes_tarefa_4
                               ]
