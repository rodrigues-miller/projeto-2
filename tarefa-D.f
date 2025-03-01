      !Definindo a subroutine que calculará a entropia do sistema de
      !andarilhos
      subroutine entropia(n)
        !Definindo dois vetores "ipassox" e "ipassoy", que servirão
        !como incrementos em x e em y a cada passo do andarilho.
        !A matriz P guardará a posicão final, x e y, de cada andarilho
        dimension :: ipassox(4), ipassoy(4), P(10000,2)
        ipassox(1) = 1
        ipassox(2) =-1
        ipassox(3) = 0
        ipassox(4) = 0
        ipassoy(1) = 0
        ipassoy(2) = 0
        ipassoy(3) = 1
        ipassoy(4) =-1
        !Valor inicial para a entropia "S" é igual a 0
        S = 0e0
        !Loop que calculará a posição final de todos andarilhos
        do m = 1, 10000
          !ix e iy guardarão a posição do andarilho a cada ciclo do
          !loop que será definido abaixo (Loop que calculará a posição
          !final de cada andarilho)
          ix = 0
          iy = 0
          !Loop que calculará a posição final de cada andarilho
          do j = 1, n
            !Sorteando ao número aleatório que definirá a posição dos
            !vetores de incremento que serão utilizados no incremento
            !da posição em x e y
            isort = rand()*4+1
            ix = ix + ipassox(isort) 
            iy = iy + ipassoy(isort)
          end do
          !Guardando a posição final de cada andarilho na matriz P
          P(m,1) = ix
          P(m,2) = iy
        end do
        !Analisando os gráficos modifiquei o valor de passos para
        !minimizar o tempo de processamento
        if (n == 1000) then
          no = 200
        else if (n == 10000) then
          no = 500
        else if (n == 100000) then
          no = 1000
        else if (n == 1000000) then
          no = 4000
        else
          no = n
        end if
        !Criando o ciclo que contará quantos andarilhos há em cada
        !ponto do plano, utilizando os valores "no" de passos
        !otimizados
        do k=-no,no
          do j=-no,no
            !definindo o contador de andarilhos em cada ponto
            icont = 0
            !Verificando quantos andarilhos estão na posição k, j no
            !plano
            do m = 1,10000
              if (P(m,1) == k) then
                if (P(m,2) == j) then
                  !Adicionando 1 ao contador toda vez que se encontra
                  !um andarilho na posição k, j
                  icont = icont+1  
                end if
              end if
            end do
            !Calculando a probabilidade de se encontrar um andarilho
            !nesse ponto, utilizando fração de andarilhos encontrados
            !no ponto dividido pelo número total de andarilhos
            prob = icont/10000.e0
            !Colocando a condição de prob ser diferente de 0 pois log
            !não é definido para prob = 0
            if (prob/=0) then
              !Somando os valores em cada ponto para se obter o valor
              !final da entropia
              S = S-prob*log(prob)
            end if
          end do
        end do
        !Escrevendo o valor da entropia encontrado no documento
        write(8,*) n, S
      end subroutine entropia
!-----------------------------------------------------------------------
      Program main
        !Abrindo o documento na onde os valores serão escritos
        open(8, file='saída-1-tarefa-D.')
        call entropia(10)
        call entropia(100)
        call entropia(1000)
        call entropia(10000)
        call entropia(100000)
        call entropia(1000000)
        close(8)
      end program main
