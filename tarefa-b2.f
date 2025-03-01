        !Definindo a função que calculará cada passo dado pelo andarilho
        !e retornará o valor da posição final do andarilho.
        function passos(n, ap)
          !Definindo que cada andarilho inicia na posição 0.
          passos = 0
          !loop que gerará os números aleatórios e fará com que os
          !andarilhos andem.
          do j = 1, n
            a = rand()
            !Condição para que o andarilho de um passo para a esquerda.
            if (a > ap) then
              passos = passos-1
              !Condição para que o andarilho de um passo para a direita.
            else    
              passos = passos+1
            end if
          end do
          return
        end function passos
!-----------------------------------------------------------------------
        !Definindo a subroutine que imprimirá no documento o valor da
        !posição de cada andarilho e também calculará o momento e o
        !momento quadrático das posições finais dos andarilhos.
        subroutine andarilho1D(ap)
          character nomearquivo*19
          !m é o número de andarilhos e n o número de passos que cada
          !andarilho deu.
          m = 10000
          n = 1000
          !Definindo o valor inical do momento "amomento" e do momento
          !quadrático "amomento2".
          amomento = 0.e0
          amomento2 = 0.e0
          !Utilizando a condicional para definir o nome do documento de
          !saída.
          if (ap == 1/3e0) then
            nomearquivo = 'saída-1-tarefa-b2.'
          else if (ap == 1/4e0) then
            nomearquivo = 'saída-2-tarefa-b2.'
          else
            nomearquivo = 'saída-3-tarefa-b2.'
          end if
          !Abrindo o documento onde são salvas as posições finais de
          !cada andarilho.
          open(unit = 2, file = nomearquivo)
          !Abrindo o loop que imprimirá no documento a posição final de
          !cada andarilho e calculará cada momento das posições finais
          !dos andarilhos.
          do i = 1, m
            !Chamando a função passos para calcular a posição final 'x'
            !de cada andarilho.
            x = passos(n, ap)
            !Calculando o momento e o momento quadrático.
            amomento = amomento + x
            amomento2 = amomento2 + x**2
            !Escrevendo no arquivo a posição final de cada andarilho.
            write(2,*) x
          end do
          !Teminando o cálculo de cada momento e o imprimindo na tela do
          !computador.
          amomento = amomento/m
          amomento2 = amomento2/m
          write(*,*) '------------------------------'
          write(*,*) 'Para p =', ap
          write(*,*) '<x> =', amomento
          write(*,*) '<x^2> =', amomento2
        end subroutine andarilho1D
!-----------------------------------------------------------------------
        program main
          !ap é a probbiliddade de o andarilho dar um passo a
          !direita.
          call andarilho1D(1/3e0)
          call andarilho1D(1/4e0)
          call andarilho1D(1/5e0)
        end program main
