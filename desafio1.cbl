      $set sourceformat"free"

      *>   Divisão de identificação do programa
       identification division.
       program-id. "desafio1".
       author.     "Michele de Lima".
       installation. "PC".
       date-written. 08/07/2020.
       date-compiled. 10/07/2020.



      *>   Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *>   Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *>   Declaração de variáveis
       data division.

      *>   Variaveis de arquivos
       file section.


      *>   Variaveis de trabalho
       working-storage section.

       01  ws-relatorio  occurs  20.
           05 ws-nome                              pic x(25).
           05 ws-diametro                          pic 9(03).
           05 ws-preco                             pic 9(03)v99.
           05 ws-preco_cm2                         pic 9(03)v99.
           05 ws-diferenca_rel                     pic 9(03)v99.

       01 ws-tela-cad-pizza.
           05 ws-preco-pizza                       pic 9(03)v99.
           05 ws-diametro-pizza                    pic 9(03)v99.
           05 ws-nome-pizza                        pic x(25).
           05 ws-preco-pizza-cm2                   pic 9(03)v99.
           05 ws-msn                               pic x(50).

       01  relatorio-aux.
           05 nome-aux                             pic x(25).
           05 diametro-aux                         pic 9(03).
           05 preco-aux                            pic 9(03)v99.
           05 preco_cm2-aux                        pic 9(03)v99.
           05 diferenca_rel-aux                    pic 9(03)v99.

       01 ws-tela-menu.
          05 ws-cadastro-pizza                     pic x(01).
          05 ws-relatorio-tela                     pic x(01).
          05 ws-sair                               pic x(01).


       77  delta_preco                             pic 9(03)v99.
       77  pizza_area                              pic 9(03)v99.
       77  raio                                    pic 9(03)v99.
       77  ind                                     pic 9(03)v99.
       77  menu                                    pic x(01).
       77  controle                                pic x(10).
       77  linha_conteudo                          pic 9(03).
       77  ind_aux                                 pic 9(03).

      *>---------------------------------------------------------------------------------------------------------------
      *>   Variaveis para comunicação entre programas
       linkage section.

      *>---------------------------------------------------------------------------------------------------------------

      *>   Declaração de tela
       screen section.
      *>   tela menu principal

       01  sc-tela-menu.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Pizzas por cm2                                   ".
           05 line 03 col 01 value "      MENU                                                                       ".
           05 line 04 col 01 value "        [ ]Cadastro de pizzas                                                    ".
           05 line 05 col 01 value "                                                                                 ".



           05 sc-sair-menu         line 01  col 71 pic x(01)
           using ws-sair           foreground-color 12.

           05 sc-cadastro-pizza    line 04  col 10 pic x(01)
           using ws-cadastro-pizza foreground-color 15.

      *>---------------------------------------------------------------------------------------------------------------


      *>   tela cadastro de pizzas

       01  sc-tela-cad-pizza.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Cadastro de Pizzas                               ".
           05 line 03 col 01 value "      Nome da Pizza      :                                                       ".
           05 line 04 col 01 value "      Diametro da Pizza  :                                                       ".
           05 line 05 col 01 value "      Preco da Pizza     :                                                       ".
           05 line 22 col 01 value "              [__________________________________________________]               ".



           05 sc-sair-menu    line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

           05 sc-nome        line 03  col 27 pic x(25)
           using ws-nome-pizza foreground-color 12.

           05 sc-diametro    line 04  col 27 pic 9(03)
           using ws-diametro-pizza foreground-color 12.

           05 sc-preco       line 05  col 27 pic 9(03)v99
           using ws-preco-pizza foreground-color 12.

           05 sc-msn-cad-jog             line 22  col 16 pic x(50)
           using ws-msn  foreground-color 12.

      *>---------------------------------------------------------------------------------------------------------------


       01  sc-relatorio-pizza.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 blank screen.
           05 line 01 col 01 value "                                                                     [ ]Sair     ".
           05 line 02 col 01 value "                                Relatorio                                        ".
           05 line 03 col 01 value "                                                                                 ".

           05 sc-sair-menu        line 01  col 71 pic x(01)
           using ws-sair foreground-color 12.

      *>---------------------------------------------------------------------------------------------------------------



       01 sc-relatori-pizza-conteudo.
      *>                                0    1    1    2    2    3    3    4    4    5    5    6    6    7    7    8
      *>                                5    0    5    0    5    0    5    0    5    0    5    0    5    0    5    0
      *>                            ----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+----+
           05 line linha_conteudo col 01 value "      Nome:          Diametro:         Preco Pizza:          Preco cm2:          ".
      *>   variavel para deixar as linhas serem adaptaveis ao numero de informações
      *>   variavel linha_conteudo para poder gerar as demais informações da tabela
           05 sc-nome            line linha_conteudo  col 11 pic x(25)
           using ws-nome-pizza foreground-color 12.

           05 sc-diametro        line linha_conteudo  col 31 pic 9(03)v99
           using ws-diametro-pizza foreground-color 12.

           05 sc-preco          line linha_conteudo  col 51 pic 9(03)v99
           using ws-preco-pizza foreground-color 12.

           05 sc-preco-cm2      line linha_conteudo  col 71 pic 9(03)v99
           using ws-preco-pizza-cm2 foreground-color 12.

      *>---------------------------------------------------------------------------------------------------------------


      *>   Declaração do corpo do programa
       procedure division.
           perform inicializa.
           perform processamento.
           perform finaliza.
      *>---------------------------------------------------------------------------------------------------------------

      *>   Inicilizacao de variaveis, abertura de arquivos
      *>   procedimentos que serao realizados apenas uma vez
       inicializa section.
           .
       inicializa-exit.
           exit.

      *>---------------------------------------------------------------------------------------------------------------

       processamento section.

           perform until ws-sair = "X"
                      or ws-sair = "x"

               move space  to ws-cadastro-pizza
               move space  to ws-sair

               display sc-tela-menu
               accept sc-tela-menu


               if  ws-cadastro-pizza  = "X"
               or  ws-cadastro-pizza  = "x"  then
                    perform cadastro-pizza
               end-if

           end-perform

      *> chama a tabela relatorio para tela
                   perform calculo
                   perform ordenacao
                   perform porcentagem-pizza


           .
       processamento-exit.
           exit.

      *>---------------------------------------------------------------------------------------------------------------


       cadastro-pizza section.

           perform until ws-sair = "V"
                      or ws-sair = "v"

               move space  to ws-nome-pizza
               move   0    to ws-diametro-pizza
               move   0    to ws-preco-pizza

      *>     deixa a tela parada no console
               display sc-tela-cad-pizza
               accept sc-tela-cad-pizza

               move space     to   ws-msn


      *>        nomes = spaces  são ignorados
               if ws-nome-pizza <> space then
                   perform descobrir-prox-ind-piz
                   if ind <= 20 then
      *>       consistencia da quantidade de pizzas para evitar estouro de tabela

      *>               salvar pizza na tabela de cadastro

                       move ws-nome-pizza       to  ws-nome(ind)
                       move ws-diametro-pizza   to  ws-diametro(ind)
                       move ws-preco-pizza      to  ws-preco(ind)

                   else
                       move "Quantidade de pizzas completa" to ws-msn
                   end-if
               end-if

           end-perform


           .
       cadastro-pizza-exit.
           exit.

      *>---------------------------------------------------------------------------------------------------------------
       descobrir-prox-ind-piz section.

           perform varying ind from 1 by 1 until ind > 20
                                              or ws-nome(ind) = space
               continue
           end-perform


           .
       descobrir-prox-ind-piz-exit.
           exit.
      *>---------------------------------------------------------------------------------------------------------------


      *>   calculo para calcular o preço da pizza em cm2
       calculo section.

           move 1 to ind

           perform until  ind = 20
                       or ws-nome(ind) = spaces


               compute raio = ws-diametro(ind)/2
               compute pizza_area = 3,14 * (raio * raio)

               if pizza_area > 0 then
                  compute ws-preco_cm2(ind) = ws-preco(ind)/pizza_area
               else
                  move 0 to ws-preco_cm2(ind)
               end-if

               add 1 to ind
           end-perform


           .
       calculo-exit.
           exit.

      *>---------------------------------------------------------------------------------------------------------------
      *> calculo para fazer a porcentagem entre as pizzas com base no
      *>                        preço cm2
       porcentagem-pizza section.



           move 1 to ind

           perform until  ind = 20
                       or ws-nome(ind + 1) = spaces

           compute delta_preco =
                   ws-preco_cm2(ind + 1) - ws-preco_cm2(ind)

           compute ws-diferenca_rel(ind + 1) =
                  (delta_preco * 100) / ws-preco_cm2(ind)


           add 1 to ind
           end-perform

           .
       porcentagem-pizza-exit.
           exit.

      *>---------------------------------------------------------------------------------------------------------------

      *> ordena os preços os preços em cm2
       ordenacao section.

           display sc-relatorio-pizza

           move "trocou" to controle

           move    0     to relatorio-aux

           perform until controle <> "trocou"
      *>       move 1 para o ind para realizar a ordenação
              move 1            to ind
              move "nao_trocou" to controle


              perform until ind = 20
                         or ws-nome(ind + 1) = space
      *>            se o preço(ind) for menor que preço(ind + 1) então faz a troca
                   if ws-preco_cm2(ind) < ws-preco_cm2(ind + 1) then
      *>                faz a troca em decrescente
                       move ws-relatorio(ind + 1) to relatorio-aux
                       move ws-relatorio(ind)     to ws-relatorio(ind + 1)
                       move relatorio-aux         to ws-relatorio(ind)
                       move "trocou"              to controle

                   end-if
                   add 1 to ind
              end-perform
           end-perform


           move 1 to ind_aux
      *>   move da linha 4 da tela em diante, e adiciona 1 para as demais ifnromações
      *>                  sejam exibidas
           move 4 to linha_conteudo
           perform until  ind_aux = 20
                       or ws-nome(ind_aux) = spaces
      *>       move o que foi informado para a variavel que executa a calculo e ordenação
               move ws-nome(ind_aux)      to ws-nome-pizza
               move ws-diametro(ind_aux)  to ws-diametro-pizza
               move ws-preco(ind_aux)     to ws-preco-pizza
               move ws-preco_cm2(ind_aux) to ws-preco-pizza-cm2
               display sc-relatori-pizza-conteudo
      *>       display a tela exibir os dados

               add 1 to ind_aux
               add 1 to linha_conteudo
            end-perform

             accept sc-relatorio-pizza
           .
       ordenacao-exit.
           exit.

      *>---------------------------------------------------------------------------------------------------------------
      *> finaliza o programa
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.

