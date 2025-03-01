      !Definindo a subroutine que calculará a posição final de cada
      !andarilho, onde n é o número de passos que cada andarilho.
      subroutine andarilho2D(n)
        character nomearquivo*18
        !Definindo dois vetores que serviram como incrementos em x e
        !em y cada posição deles.
        dimension :: ipassox(4), ipassoy(4)
        ipassox(1) = 1
        ipassox(2) =-1
        ipassox(3) = 0
        ipassox(4) = 0
        ipassoy(1) = 0
        ipassoy(2) = 0
        ipassoy(3) = 1
        ipassoy(4) =-1
        !m é o número de andarilhos.
        m = 10000
        !Definindo o valor inicial das variáveis que guardarão a média
        !da posição x, a média da posição y e a média do raio ao
        !quadrado, respectivamente.
        amediax = 0e0
        amediay = 0e0
        amediar2 = 0e0
        !Loop que calculará a posição final de todos andarilhos.
        do i = 1, m
          !ix e iy guardarão a posição do andarilho a cada ciclo do
          !loop que será definido abaixo (Loop que calculará a posição
          !final de cada andarilho).
          ix = 0
          iy = 0
          !Loop que calculará a posição final de cada andarilho.
          do j = 1, n
            !sorteando um número aleatório que definirá a posição dos
            !vetores de incremento que serão utilizadas no incremento
            !da posição em x e em y, utilizando variável inteira para
            !que tenha truncamento.
            isort = rand()*4+1
            ix = ix + ipassox(isort) 
            iy = iy + ipassoy(isort)
          end do
          !Somando os valores da posição de cada andarilho em suas
          !respectivas médias.
          amediax = amediax1 + ix
          amediay = amediay1 + iy
          amediar2 = amediar2 + ix**2 + iy**2
          !Definindo o nome do arquivo que será aberto para salvar os
          !dados.
          if (n == 10) then
            nomearquivo = 'saída-1-tarefa-C.'
          else if (n == 100) then
            nomearquivo = 'saída-2-tarefa-C.'
          else if (n == 1000) then
            nomearquivo = 'saída-3-tarefa-C.'
          else if (n == 10000) then
            nomearquivo = 'saída-4-tarefa-C.'
          else if (n == 100000) then
            nomearquivo = 'saída-5-tarefa-C.'
          else
            nomearquivo = 'saída-6-tarefa-C.'
          end if            
          !Abrindo o arquivo que foi selecionado na condicional.
          open(2,file = nomearquivo)
          !imprimindo no documento o valor final obtido para a posição
          !de cada andarilho.
          write(2,*) ix, iy
        end do
        !Finalizando o cálculo das médias dividindo pelo número total
        !de andarilhos.
        amediax = amediax*1e0/m
        amediay = amediay*1e0/m
        amediar2 = amediar2*1e0/m
        !Calculando o valor de delta ao quadrado.
        delta2 = amediar2 - amediax**2 - amediay**2
        !Imprimindo na tela os valores que foram solicitados.
        write(*,100) 
        write(*,101) n
        write(*,102) amediax, amediay
        write(*,103) delta2
        !Definindo os formartos utilizados para imprimir os dados no
        !arquivo.
100     format("-------------------------------------------")
101     format("Para o número de passos igual a:",1x,I7)
102     format("<r> é igual a:",1x,'(',f6.3,',',1x,f6.3,')')
103     format("<r²>-<r>.<r> é igual a:",1x,f11.3)
      end subroutine andarilho2D
!-----------------------------------------------------------------------
      program dadosandarilho2D
        !Rodando o programa para diferentes valores de n.
        call andarilho2D(10)
        call andarilho2D(100)
        call andarilho2D(1000) 
        call andarilho2D(10000)
        call andarilho2D(100000)
        call andarilho2D(1000000)
        write(*,100)
100     format("-------------------------------------------")
      end program dadosandarilho2D
