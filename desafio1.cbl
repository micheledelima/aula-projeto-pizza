      *Divisão de identificação do programa
       identification division.
       program-id. "desafio1".
       author.     "Michele de Lima".
       installation. "PC".
       date-written. 08/07/2020.
       date-compiled. 10/07/2020.



      *Divisão para configuração do ambiente
       environment division.
       configuration section.
           special-names. decimal-point is comma.

      *-----Declaração dos recursos externos
       input-output section.
       file-control.
       i-o-control.

      *Declaração de variáveis
       data division.

      *----Variaveis de arquivos
       file section.


      *----Variaveis de trabalho
       working-storage section.

       01  relatorio  occurs  20.
           05 nome                                 pic x(15).
           05 filler                               pic x(03)
              value " - ".
           05 diametro                             pic 9(03).
           05 filler                               pic x(03)
              value " - ".
           05 preco                                pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 preco_cm2                            pic 9(03)v99.
           05 filler                               pic x(03)
              value " - ".
           05 diferenca_rel                        pic 9(03)v99999.
           05 filler                               pic x(03)
              value " - ".

       77  delta_preco                             pic 9(03)v99999.
       77  pizza_area                              pic 9(03)v99999.
       77  aux                                     pic 9(03).
       77  raio                                    pic 9(03)v99999.
       77  ind                                     pic 9(02).
       77  menu                                    pic x(01).
       77  controle                                pic x(10).


      *----Variaveis para comunicação entre programas
       linkage section.


      *----Declaração de tela
       screen section.


      *Declaração do corpo do programa
       procedure division.
           perform inicializa.
           perform processamento.
           perform finaliza.
      *--------------------------------------------------------

      * Inicilizacao de variaveis, abertura de arquivos
      * procedimentos que serao realizados apenas uma vez
       inicializa section.
           move   "S"       to     menu
           .
       inicializa-exit.
           exit.

      *--------------------------------------------------------

       processamento section.
           move 0 to ind

           perform until menu <> "S"
               display erase
               add 1 to ind

               if ind > 20 then
                   display "Vc atingiu o limite de 20 pizzas"
               else
                   display "Informe o nome da pizza "
                   accept nome(ind)

                   display "Informe o diametro "
                   accept diametro(ind)

                   display "Informe o preco "
                   accept preco(ind)
               end-if

      * chama a tabela para tela
               perform calculo
               perform ordenacao
               perform porcentagem-pizza

               display "Deseja cadastrar mais pizza? 'S'im  ou 'N'ao "
               accept menu
           end-perform

      * quando usuario n completa o total de 20, pode concluir com nome
           perform varying ind from 1 by 1 until ind > 20

                                              or nome(ind) = space
           display relatorio(ind)

           end-perform

           .
       processamento-exit.
           exit.
      *--------------------------------------------------------
      * calculo para calcular o preço da pizza em cm2
       calculo section.

           compute raio = diametro(ind)/2
           compute pizza_area = 3,14 * (raio * raio)

           compute preco_cm2(ind) = preco(ind)/pizza_area

           .
       calculo-exit.
           exit.

      *--------------------------------------------------------
      * calculo para fazer a porcentagem entre as pizzas com base no
      *                        preço cm2
       porcentagem-pizza section.

           move 1 to ind

           perform until  ind = 20
                       or nome(ind + 1) = spaces

           compute delta_preco =
                   preco_cm2(ind + 1) - preco_cm2(ind)

           compute diferenca_rel(ind + 1) =
                  (delta_preco * 100) / preco_cm2(ind)

           add 1 to ind
           end-perform

           .
       porcentagem-pizza-exit.
           exit.


      *--------------------------------------------------------
      * ordena os preços de forma crescente os preços cm2

       ordenacao section.

           move 'trocou' to controle
           perform until controle <> 'trocou'

              move 1            to ind
              move 'nao_trocou' to controle


              perform until ind = 20
                         or nome(ind + 1) = space
                   if preco_cm2(ind)>preco_cm2(ind + 1) then
                       move preco_cm2(ind + 1) to aux
                       move preco_cm2(ind)     to preco_cm2(ind + 1)
                       move aux                to preco_cm2(ind)
                       move 'trocou'           to controle
                   end-if

                   add 1 to ind
              end-perform
           end-perform

           .
       ordenacao-exit.
           exit.

      *--------------------------------------------------------
      * finaliza o programa
       finaliza section.
           Stop run
           .
       finaliza-exit.
           exit.

