      !Definindo a função que calculará o momento para cada n, onde 'k'
      !equivale a N.
      function amomento(n, k)
        !Definindo o valor inicial do momento.
        amomento = 0.e0
        !Definindo o loop que somará k valores de x ao momento.
        do i = 1, k
          x = rand()**n
          amomento = amomento + x
        end do
        !Dividindo o resultado da soma dos k números por k, para se
        !obter o valor do momento.
        amomento = amomento/k
        return
      end function amomento
!-----------------------------------------------------------------------
      program main
        !Abrindo o arquivo que armazena os dados de saída.
        open(2,file='saída-1-tarefa-A.')
        !Imprimindo os resultados obtidos, em formato de tabela.
        write(2,102)
        write(2,50)
        write(2,101)
        write(2,100) 100, amomento(1, 100),
     &amomento(2, 100), amomento(3, 100),
     &amomento(4, 100)
        write(2,101)
        write(2,100) 1000, amomento(1, 1000),
     &amomento(2, 1000), amomento(3, 1000),
     &amomento(4, 1000)
        write(2,101)
        write(2,100) 10000, amomento(1, 10000),
     &amomento(2, 10000), amomento(3, 10000),
     &amomento(4, 10000)
        write(2,101)
        write(2,100) 100000, amomento(1, 100000),
     &amomento(2, 100000), amomento(3, 100000),
     &amomento(4, 100000)
        write(2,101)
        write(2,100) 1000000, amomento(1, 1000000),
     &amomento(2, 1000000), amomento(3, 1000000),
     &amomento(4, 1000000)
        write(2,101)
        write(2,100) 10000000, amomento(1, 10000000),
     &amomento(2, 10000000), amomento(3, 10000000),
     &amomento(4, 10000000)
        write(2,102)
        close(2)
        !Definindo os formatos utilizados no programa.
50      format('|     N    |      <x>     |     <x²>     |     <x³>     
     &  |     <x⁴>     |')
100     format('|',1x,I8,1x,'|',1x,f12.9,1x,'|',1x,f12.9,1x,'|',
     &1x,f12.9,1x,'|',1x,f12.9,1x,'|')
101     format('|----------|--------------|--------------|--------------
     &|--------------|')
102     format('--------------------------------------------------------
     &----------------')
      end program main
