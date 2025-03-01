      !Definindo a subroutine que imprimirá no documento o valor da
      !posição de cada andarilho e também calculará o momento e o
      !momento quadrático das posições finais dos andarilhos
      subroutine andarilho1D()
        !m é o número de andarilhos e n o número de passos que cada
        !andarilho deu
        m = 10000
        n = 1000
        !Definindo o valor inicial do momento "amomento" e do momento
        !quadrático "amomento2"
        amomento = 0.e0
        amomento2 = 0.e0
        !Abrindo o documento na onde a posição final de cada andarilho
        !será impressa
        open(unit = 2, file = 'saída-1-tarefa-b1.')
        !Abrindo o loop que imprimirá no documento a posição final de
        !cada andarilho e calculará cada momento das posições finais
        !dos andarilhos
        do i = 1, m
          !Chamando a função passos para calcular a posição final 'x'
          !de cada andarilho
          x = passos(n)
          !Calculando o momento e o momento quadrático
          amomento = amomento + x
          amomento2 = amomento2 + x**2
          !Escrvendo no arquivo a posição final de cada
          !andarilho
          write(2,*) x
        end do
        !Teminando o cálculo de cada momento e o imprimindo na tela do
        !computador
        amomento = amomento/m
        amomento2 = amomento2/m
        write(*,100) amomento
        write(*,101) amomento2
        !Definindo os formatos utilizados no programa.
100     format('<x> =',1x,f14.9) 
101     format('<x²> =',1x,f14.9) 
      end subroutine andarilho1D
!-----------------------------------------------------------------------
      !Definindo a função que calculará cada passo dado pelo andarilho
      !e retornará o valor da posição final do andarilho
      function passos(n)
        !Definindo que cada andarilho inicia na posição 0
        passos = 0
        !loop que gerará os números aleatórios e fará com que os
        !andarilhos andem
        do j = 1, n
          a = rand()
          !Condição para que o andarilho de um passo para a direita
          if (a > 1/2.e0) then
            passos = passos+1
            !Condição para que o andarilho de um passo para a esquerda
          else    
            passos = passos-1
          end if
        end do
        return
      end function passos
!-----------------------------------------------------------------------
      program dadosandarilhos
        !Chamando a subroutine que fará todos os cálculos do
        !programa
        call andarilho1D()
      end program dadosandarilhos
