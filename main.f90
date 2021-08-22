module METODOS
    IMPLICIT NONE
    
    CONTAINS
!--------------------------------------------------------------------------------------------------------------------------------------------------------------

        !MÉTODO DA BISSEÇÃO - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 3.3)
        !é necessário definir a função f(x) a ser utilizada no método em um arquivo separado, denominado "funcao"
        !método para encontrar onde f(x)=0 ou se aproxima muito desse valor
        
        !calcula o valor onde f(x) = 0, até a tolerância ser atingida ou o máximo de iterações ser alcançado
        SUBROUTINE bissecao (a, b, imax, toli)
            IMPLICIT NONE 
            REAL    :: a, b, toli, x, fa, fb, fx, tol
            INTEGER :: i=0, imax
            !a, b: extremidades do intervalo
            !tol, toli: tolerância atual e a tolerância de parada
            !i, imax: valor inicial do contador e valor máximo para parada
            !x: valor de estimativa calculado
            !fa, fb, fx: valores da função calculados de a, b e x
            
            !calcula o valor da função nas extremidades do intervalo
            fa = f(a)
            fb = f(b)

            !checa se f(a) e f(b) tem sinais opostos como devem ter
            IF (fa*fb>0) THEN 
                print*, "A função tem o mesmo sinal no ponto a e b."
            ELSE
                DO WHILE(i/=imax)    
                    !estima o valor de x
                    x = (a+b)/2.0
                    !calcula o valor de f(x)
                    fx = f(x)
        
                    print*, "Iteração =", i, "Valor da estimativa x =", x, "F(x) =", fx   !imprime os valores em cada iteração
        
                    IF(fx==0) print*, "Solução exata encontrada: ", x !checa se x zera a função
        
                    tol = ABS(f(x))
        
                    IF(tol < toli) EXIT !checa se a tolerância encontrada é menor que a definida
        
                    !troca os valores dos limites do intervalo
                    IF(fa*fx < 0) THEN 
                        b = x
                    ELSE
                        a = x 
                    ENDIF 
                    i = i+1    
                END DO    
            END IF
        END SUBROUTINE
!-------------------------------------------------------------------------------------------------------------------------------------------------------------- 

        !MÉTODO DA SECANTE - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 3.6)
        !é necessário definir a função f(x) a ser utilizada no método em um arquivo separado, denominado "funcao"        
        !método para encontrar onde f(x)=0 ou se aproxima muito desse valor        

        !calcula o valor onde f(x) = 0, até a tolerância ser atingida ou o máximo de iterações ser alcançado        
        SUBROUTINE secante(a, b, imax, toli)
            REAL    :: a, b, toli, x, fa, fb, fx, tol
            INTEGER :: i=0, imax
            !a, b: extremidades do intervalo
            !tol, toli: tolerância atual e a de parada
            !i, imax: valor inicial do contador e valor máximo para parada
            !x: valor de estimativa calculado
            !fa, fb, fx: valores da função calculados de a, b e x
            
            !calcula o valor da função nas extremidades do intervalo
            fa = f(a)
            fb = f(b)

            !checa se f(a) e f(b) tem sinais opostos como devem ter
            IF (fa*fb>0) THEN 
                print*, "A função tem o mesmo sinal no ponto a e b."
            ELSE
                DO WHILE(i/=imax)    
                    !estima o valor de x
                    x = b - ((f(b)*(a-b))/(f(a)-f(b)))
                    !calcula o valor de f(x)
                    fx = f(x)
        
                    print*, "Iteração =", i, "Valor da estimativa x =", x, "F(x) =", fx   !imprime os valores em cada iteração
        
                    IF(fx==0) print*, "Solução exata encontrada: ", x !checa se x zera a função
        
                    tol = ABS(f(x))
        
                    IF(tol < toli) EXIT !checa se a tolerância encontrada é menor que a definida
        
                    !troca os valores dos limites do intervalo
                    IF(fa*fx < 0) THEN 
                        b = x
                    ELSE
                        a = x 
                    ENDIF 
                    i = i+1    
                END DO    
            END IF
        END SUBROUTINE
!--------------------------------------------------------------------------------------------------------------------------------------------------------------  

        !MÉTODO REGULA FALSI - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 3.4)
        !é necessário definir a função f(x) a ser utilizada no método em um arquivo separado, denominado "funcao"        
        !método para encontrar onde f(x)=0 ou se aproxima muito desse valor        

        !calcula o valor onde f(x) = 0, até a tolerância ser atingida ou o máximo de iterações ser alcançado        
        SUBROUTINE regula(a, b, imax, toli)
            REAL    :: a, b, toli, x, fa, fb, fx, tol
            INTEGER :: i=0, imax
            !a, b: extremidades do intervalo
            !tol, toli: tolerância atual e a de parada
            !i, imax: valor inicial do contador e valor máximo para parada
            !x: valor de estimativa calculado
            !fa, fb, fx: valores da função calculados de a, b e x            
            
            !calcula o valor da função nas extremidades do intervalo
            fa = f(a)
            fb = f(b)

            !checa se f(a) e f(b) tem sinais opostos como devem ter
            IF (fa*fb>0) THEN 
                print*, "A função tem o mesmo sinal no ponto a e b."
            ELSE
                DO WHILE(i/=imax)    
                    !estima o valor de x
                    x = (a*f(b)-b*f(a))/(f(b)-f(a))
                    
                    !calcula o valor de f(x)
                    fx = f(x)
        
                    print*, "Iteração =", i, "Valor da estimativa x =", x, "F(x) =", fx   !imprime os valores em cada iteração
        
                    IF(fx==0) print*, "Solução exata encontrada: ", x !checa se x zera a função
        
                    tol = ABS(f(x))
        
                    IF(tol < toli) EXIT !checa se a tolerância encontrada é menor que a definida
        
                    !troca os valores dos limites do intervalo
                    IF(fa*fx < 0) THEN 
                        b = x
                    ELSE
                        a = x 
                    ENDIF 
                    i = i+1    
                END DO    
            END IF
        END SUBROUTINE        
!--------------------------------------------------------------------------------------------------------------------------------------------------------------

        !MÉTODO DO PONTO FIXO - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 3.7)
        !é necessário definir a função f(x) e uma g(x) a ser utilizada no método em um arquivo separado, denominado "funcao"        
        !método para encontrar onde f(x)=0 ou se aproxima muito desse valor        
        
        !calcula o valor onde g(x) = 0, até a tolerância ser atingida ou o máximo de iterações ser alcançado        
        SUBROUTINE fixo(x_i, imax, eps)
            IMPLICIT NONE
            REAL    :: x_i, eps, x, tol
            INTEGER :: imax, i=0 
            !x_i: recebe o valor inicial para a busca
            !i, imax: o número inicial e o maximo de iterações
            !tol, eps: tolerância atual e a de parada
            
            write(*,*) "Iteração    x(i-1)          x          Delta"
            DO WHILE(i/=imax)    
                x = g(x_i)
                tol = ABS(f(x))
        
                write(*,'(i5, 3f14.5)') i, x_i, x, tol
        
                IF(x==0) print*, "Solução exata encontrada: ", x
                IF(tol < eps) EXIT !checa se a tolerância encontrada é menor que a definida
        
                !troca o valor de x e soma +1 em i
                x_i = x    
                i = i+1
            END DO 
        END SUBROUTINE
!--------------------------------------------------------------------------------------------------------------------------------------------------------------
        
        !MÉTODO DE GAUSS - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 4.2 - 4.3)
        !método para escalonar a matriz recebida e encontrar as soluções de um sistema [A][X]=[B]
        
        SUBROUTINE EscalonaG(a, X) 
            REAL, DIMENSION(:,:)              :: a
            REAL, DIMENSION(:), ALLOCATABLE   :: Temp      
            INTEGER                           :: L, C, i, j, k 
            REAL, DIMENSION(:)                :: X            
            !a: recebe a matriz aumentada do sistema linear a ser resolvido
            !temp: vetor para salvar temporáriamente a linha da matriz caso seja preciso fazer a pivotação   
            !L, C, i, j, k: variáveis que guardam o tamanho da matriz aumentada e variáveis "contadoras" 
            !X: vetor que armazenará a solução do sistema 
            
            L = SIZE(a, dim=1) !checa quantas linhas a matriz tem
            C = SIZE(a, dim=2) !checa quantas colunas a matriz tem
        
            ALLOCATE(Temp(C))  !aloca memória para o vetor temporário que guardará a linha caso necessário
        
            !escalona a matriz 
            DO i=1, L-1     !controla a iteração das linhas pivôs
                IF(a(i,i) == 0) THEN !checa se o pivo é igual a zero 
                DO k=i+1, L 
                    IF(a(k, i)/=0) THEN !troca pela nova linha caso o pivo seja diferente zero
                        Temp = a(i,:)              !salva a linha com pivo zero
                        a(i,:) = a(k,:)            !troca a linha com pivo diferente de zero pro lugar da linha com pivo zero
                        a(k,:) = Temp              !escreve a linha com pivo zero no lugar da linha que tinha pivo diferente de zero
                    ENDIF    
                END DO    
                ENDIF 
                DO j=i+1, L 
                    a(j,:) = a(j,:) - ((a(j,i)/a(i,i))*a(i,:)) !faz a subtração da linha seguinte pela linha pivô multiplicado por m (controla as colunas por array slicing)
                END DO
            END DO   
            
            !acha as raízes do sistema
            DO i = C-1, 1, -1
                X(i) = a(i,C)/a(i,i)
            
                IF(i>0) THEN !faz a retrosubstituição na matriz triangular
                    DO k=C-1, i, -1
                        a(i-1, C) = a(i-1, C) - (a(i-1, k)*X(k))
                    END DO
                END IF    
            END DO 
            RETURN 
        END SUBROUTINE
        
        !subroutine pra imprimir a matriz
        SUBROUTINE printmat(x)
            REAL, DIMENSION (:,:) :: x 
            INTEGER               :: i
            
            DO i=1, SIZE(x, dim=1)
                write(*, '(8f8.2)') x(i,:)
            END DO
            
            RETURN
        END SUBROUTINE
        
!--------------------------------------------------------------------------------------------------------------------------------------------------------------
        !MÉTODO DE GAUSS-JORDAN - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 4.4, 4.7)
        !método para escalonar totalmente a matriz recebida e encontrar as soluções de um sistema [A][X]=[B]
        
        SUBROUTINE EscalonaGJ(a, X, ainv)  
            REAL, DIMENSION(:,:)              :: a
            REAL, DIMENSION(:), ALLOCATABLE   :: Temp   
            REAL, DIMENSION(:,:)              :: ainv
            INTEGER                           :: L, C, i, j, k
            REAL, DIMENSION(:)                :: X
            !a: recebe a matriz aumentada do sistema linear a ser resolvido
            !temp: vetor para salvar temporáriamente a linha da matriz caso seja preciso fazer a pivotação   
            !ainv: guarda a matriz inversa do sistema 
            !L, C, i, j, k: variáveis que guardam o tamanho da matriz aumentada e variáveis "contadoras" 
            !X: vetor que armazenará a solução do sistema 
            
            L = SIZE(a, dim=1)                                                  !checa quantas linhas a matriz tem
            C = SIZE(a, dim=2)                                                  !checa quantas colunas a matriz tem

            !ESCALONA A MATRIZ:
            DO i=1, L 
                !PIVOTAÇÃO(SE PRECISAR)
                IF(a(i,i) == 0) THEN                                            !checa se o pivo é igual a zero
                    ALLOCATE(Temp(C))                                           !define o tamanho do vetor Temp (usado pra salvar a linha com pivo zero)
                    DO k=i+1, L 
                        IF(a(k, i+1)/=0) THEN                                   !troca a linha caso o pivo seja zero
                            Temp = a(i,:)                                       !salva a linha com pivo zero
                            a(i,:) = a(k,:)                                     !troca a linha com pivo diferente de zero pro lugar da linha com pivo zero
                            a(k,:) = Temp                                       !escreve a linha com pivo zero no lugar da linha que tinha pivo diferente de zero
                        ENDIF    
                    END DO         
                ENDIF 
            
                a(i,:) = a(i,:)/a(i,i)                                          !deixando a linha pivo com pivo = 1
            
                DO j=i, L
                    IF(j/=i) THEN
                        a(j,:) = a(j,:) - ((a(j,i)/a(i,i))*a(i,:))              !faz a subtração das linhas seguintes pela linha pivô multiplicada por m
                    ELSE
                        DO k = j-1, 1, -1 
                            a(j-k,:) = a(j-k,:) - ((a(j-k,i)/a(i,i))*a(i,:))    !faz a subtração das linhas anteriores pela linha pivo multiplicada por m
                        END DO
                    ENDIF
                END DO
            END DO 
        
            !PEGA A SOLUÇÃO DO SISTEMA
            X(:) = a(:,C)
        
            !SEPARA A MATRIZ INVERSA DA MATRIZ AUMENTADA IAinv    
            DO i=1, L                      
                DO j=L+1, 2*L 
                    ainv(i, j-4) = a(i,j)
                END DO 
            END DO        
            RETURN 
        END SUBROUTINE        

!--------------------------------------------------------------------------------------------------------------------------------------------------------------
    
        !MÉTODO FATORAÇÃO LU-GAUSS - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 4.5.1)
        !método para escalonar a matriz recebida, "quebrar" em duas matriz [L] e [U] resolver um sistema [L][Y]=[B] e encontrar as soluções de um sistema [U][X]=[Y]
        !que dará a solução do sistema [A][X]=[B], usando Gauss
        SUBROUTINE LUgauss(a, b, X)
            REAL, DIMENSION(:,:)              :: a 
            REAL                              :: m    
            REAL, DIMENSION(:,:), ALLOCATABLE :: L     
            INTEGER                           :: Lin, Col, i, j, k 
            REAL, DIMENSION(:)                :: b       
            REAL, DIMENSION(:), ALLOCATABLE   :: Y   
            REAL, DIMENSION(:)                :: X      
            !a: recebe a matriz A do sistema a ser resolvido
            !m: guarda a divisão do elemento pivô pelo elemento da linha a ser escalonada
            !L: matriz triangular inferior a ser formada
            !Lin, Col, i, j, k: variáveis para guardar o tamanho da matriz e variáveis contadoras 
            !b: recebe o vetor coluna B do sistema a ser resolvido
            !Y: vetor coluna para resolver o sistema posteriormente
            !X: vetor para armazenar a solução do sistema
            
            Lin = SIZE(a, dim=1) !checa quantas linhas a matriz tem
            Col = SIZE(a, dim=2) !checa quantas colunas a matriz tem
        
            ALLOCATE(L(Lin, Col)) !aloca memória para a matriz L
      
            L(:,:)=0.0          !zera toda a matriz L
            DO i=1, Lin 
                L(i,i) = 1.0    !define as diagonais de L como 1
            END DO     
            !escalona a matriz A
            DO i=1, Lin-1     !controla a iteração das linhas pivôs
                DO j=i+1, Lin 
                        m = a(j,i)/a(i,i)          !calcula o coeficiente m
                        L(j,i) = m 
                    DO k=1, Lin+1
                        a(j,k) = a(j,k) - m*a(i,k) !faz a subtração da linha seguinte pela linha pivô multiplicado por m (controla as colunas)
                    END DO
                END DO
            END DO

            ALLOCATE(Y(Lin))  !aloca memória para o vetor Y 
        
            !acha as raízes do sistema [L][y] = [b]
            DO i=1, Lin 
                Y(i) = b(i)/L(i,i)
                DO j=2, Lin  
                    b(j) = b(j) - L(j,i)*Y(i)
                END DO 
            END DO 

            !acha as raízes do sistema [U][x] = [y]
            DO i=Lin, 1, -1
                X(i) = Y(i)/a(i,i)
                DO j=Lin-1, 1, -1 
                    Y(j) = Y(j) - a(j,i)*X(i)
                END DO 
            END DO 
            RETURN 
        END SUBROUTINE
!--------------------------------------------------------------------------------------------------------------------------------------------------------------

        !MÉTODO LU-CROUTZ - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 4.5.2)
        !método para escalonar a matriz recebida, "quebrar" em duas matriz [L] e [U] resolver um sistema [L][Y]=[B] e encontrar as soluções de um sistema [U][X]=[Y]
        !que dará a solução do sistema [A][X]=[B], usando Croutz 
        
        SUBROUTINE LUcroutz(a, b, X)
            IMPLICIT NONE 
            REAL, DIMENSION(:,:)              :: a   
            REAL, DIMENSION(:,:), ALLOCATABLE :: L, U
            INTEGER                           :: Lin, i, j
            REAL, DIMENSION(:)                :: b  
            REAL, DIMENSION(:), ALLOCATABLE   :: Y
            REAL, DIMENSION(:)                :: X
            !a: recebe a matriz A do sistema a ser resolvido
            !L, U: matriz triangular inferior e superior a serem formadas
            !Lin, i, j, k: variável para guardar o tamanho da matriz e variáveis contadoras 
            !b: recebe o vetor coluna B do sistema a ser resolvido
            !Y: vetor coluna para resolver o sistema posteriormente
            !X: vetor para armazenar a solução do sistema            
            
            Lin = SIZE(a, dim=1)
            ALLOCATE(L(SIZE(a, dim=1), SIZE(a, dim=2)))
            ALLOCATE(U(SIZE(a, dim=1), SIZE(a, dim=2)))
            
            L(:,1) = a(:,1)  !define a primeira coluna da matriz L igual a coluna da matriz A recebida 

            DO i=1, Lin 
                U(i,i)=1.d0 !define a diagonal da matriz U como 1
            END DO

            U(1, 2:) = a(1,2:)/L(1,1)  !calcula a primeira linha da matriz U 
            
            DO i=2, Lin 
                DO j=2, i 
                    L(i,j) = a(i,j) - SUM(L(i,1:j-1)*U(1:j-1,j)) !define o restante da matriz L
                END DO 
                DO j=i+1, Lin 
                    U(i,j) = (a(i,j) - SUM(L(i,1:i-1)*U(1:i-1,j)))/L(i,i) !define o restante da matriz U 
                END DO     
            END DO 

            ALLOCATE(Y(SIZE(X)))
            
            !acha as raízes do sistema [L][y] = [b]
            DO i=1, Lin 
                Y(i) = b(i)/L(i,i)
                DO j=2, Lin  
                    b(j) = b(j) - L(j,i)*Y(i)
                END DO 
            END DO 
            
            !acha as raízes do sistema [U][x] = [y]
            DO i=Lin, 1, -1
                X(i) = Y(i)/U(i,i)
                DO j=Lin-1, 1, -1 
                    Y(j) = Y(j) - U(j,i)*X(i)
                END DO 
            END DO 
            RETURN    
        END SUBROUTINE        
!--------------------------------------------------------------------------------------------------------------------------------------------------------------        
        
        !MÉTODO NORMAS - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 4.10.2)
        !método para calcular a norma infinita de um dado vetor e de uma matriz 
        
        !subroutine para calcular a norma de um vetor 
        REAL FUNCTION norm_v(vec)
            IMPLICIT NONE
            REAL, DIMENSION(:) :: vec        
            !vec: vetor recebido
     
            norm_v = MAXVAL(ABS(vec))
            RETURN
        END FUNCTION norm_v

        !subroutine para calcular a norma infinita de uma dada matriz 
        REAL FUNCTION norm_m(mat)
            IMPLICIT NONE
            REAL, DIMENSION(:,:) :: mat       
            !mat: matriz recebida

            norm_m = MAXVAL(SUM(ABS(mat), dim=2))
            RETURN
        END FUNCTION norm_m   
!--------------------------------------------------------------------------------------------------------------------------------------------------------------

        !MÉTODO DOS MÍNIMOS QUADRADOS - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 5.2.2)
        !método para realizar regressão linear de uma função utilizando minímos quadrados
        
        !subroutine para fazer o ajuste linear
        SUBROUTINE minquad(n, x, y, a1, a0, E)
            IMPLICIT NONE 
            INTEGER  :: n, i, j
            REAL     :: x(n), y(n), a1, a0, E, sx, sy, sxy, sxx, den 
            !n, i, j: variável para armazenar o numero de pontos, e variáveis contadoras
            !x, y: vetores com os pontos experimentais
            !a1, a0, E: variáveis para armazenas os coeficientes a1 e a0 calculados e o Erro encontrado
            !sx, sy, sxy, sxx, den: variáveis que armazenam os somatórios realizados e o denominador da divisão a ser feita

            sx = SUM(x)    !faz a soma dos valores dos pontos de x 
            sy = SUM(y)    !faz a soma dos valores dos pontos de y
            sxy = SUM(x*y) !faz a soma dos valores de x multiplicado por y
            sxx = SUM(x*x) !faz a soma dos valores de x²
            den = ((n*sxx) - (sx**2))   !calcula o denominador 
            a1 = ((n*sxy - sx*sy)/den)  !calcula o valor de a1 
            a0 = ((sxx*sy - sxy*sx)/den) !calcula o valor de a0
            E = SUM((y - (a1*x + a0))**2) !encontra o erro 

            RETURN 
        END SUBROUTINE          
!--------------------------------------------------------------------------------------------------------------------------------------------------------------
        !MÉTODO DE REGRESSÃO POLINOMIAL - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 5.4)
        !método para realizar regressão polinomial de uma função utilizando polinômios de ordem >=2         
        
        !subroutine para fazer a regressão polinomial
        SUBROUTINE Reg(x, Sol, m)
            IMPLICIT NONE 
            INTEGER                         :: m     
            REAL, DIMENSION(m+1,m+2)        :: x         
            INTEGER                         :: i, j, k  
            REAL                            :: Sol(m+1) 
            !m: armazena a ordem do polinômio 
            !x: armazena a matriz que contém os coeficientes do sistema (soma das potências de x) 
            !i, j, k: variáveis contadoras 
            !sol: vetor que armazenará a solução do sistema (a0, a1, a2, a3 e a4)
            
        
            !escalona a matriz 
            DO i=1, m     
                DO j=i+1, m+1
                        x(j,:) = x(j,:) - ((x(j,i)/x(i,i))*x(i,:))
                END DO        
            END DO  
            
            !acha as raízes do sistema (retrosubstituição)
            DO i = m+1, 1, -1
                Sol(i) = x(i,m+2)/x(i,i)
                DO k=m+1, i, -1
                    x(i-1, m+2) = x(i-1, m+2) - (x(i-1, k)*Sol(k))
                END DO
            END DO 
            RETURN 
        END SUBROUTINE        
!--------------------------------------------------------------------------------------------------------------------------------------------------------------

        !MÉTODO DE INTERPOLAÇÃO DE LAGRANGE - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 5.5.1)
        !método para realizar o ajuste de uma curva usando os polinômios interpoladores de Lagrange 
        
        SUBROUTINE Interpola(a, b, val, tam, Yint)
            IMPLICIT NONE 
            REAL(8), DIMENSION(:) :: a, b
            REAL(8)               :: val, L(tam), Yint
            INTEGER               :: tam, i, j
            !tam: armazena o número de pontos
            !a, b: vetores que armazenam os pontos experimentais
            !val, Yint: recebe o valor de x e armazena o valor de Y interpolado
            !L: vetor com os termos do produtório
            !i, j: variaveis "contadoras"
    
            DO i=1, tam 
                L(i) = 1 
                DO j=1, tam 
                    IF (j/=i) L(i) = L(i)*(val-a(j))/(a(i)-a(j)) 
                END DO
            END DO
        
            Yint = SUM(b*L)         !calcula o valor de Y interpolado
            RETURN 
        END SUBROUTINE
!--------------------------------------------------------------------------------------------------------------------------------------------------------------
        !MÉTODO DO RETANGULO COMPOSTO - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 7.2)
        !método para realizar a integração numérico de uma função usando o método do retângulo composto 
        !é necessário definir a função f(x) a ser integrada no método em um arquivo separado, denominado "funcao"         
        
        !function para calcular o valor da integral da f(x)
        REAL FUNCTION Int_ret(inf, sup, n)
            IMPLICIT NONE 
            REAL    :: inf, sup, x_i, dx
            INTEGER :: n, i
            !inf, sup: recebe os limites inferior e superior
            !n, i: limite de passos a serem feitos e variável "contadora"
            !x_i, dx: valor numérico a ser passado para a função e o salto de integração
            
            dx = (sup-inf)/n !calcula o valor do passo de integração
            Int_ret = 0.d0   !define o valor da função como 0 inicialmente
    
            !faz o calculo do valor da integral
            DO i=1, n 
                x_i = inf + (i-1)*dx
                Int_ret = Int_ret + f(x_i)*dx
            END DO 
    
            RETURN 
        END FUNCTION Int_ret
!--------------------------------------------------------------------------------------------------------------------------------------------------------------

        !MÉTODO DO PONTO CENTRAL - Referência: Gilat-Subranamian, Métodos Numéricos (Cap - 7.2)
        !método para realizar a integração numérico de uma função usando o método do ponto central  
        !é necessário definir a função f(x) a ser integrada no método em um arquivo separado, denominado "funcao"         

        !function para calcular o valor da integral da f(x) definida
        REAL FUNCTION Int_PC(inf, sup, n)
            IMPLICIT NONE 
            REAL    :: inf, sup, x_i, dx
            INTEGER :: n, i
            !inf, sup: recebe os limites inferior e superior
            !n, i: limite de passos a serem feitos e variável "contadora"
            !x_i, dx: valor numérico a ser passado para a função e o salto de integração            

            dx = (sup-inf)/n        !calcula o valor do passo de integração
            Int_PC = 0.d0           !define o valor da função como 0 inicialmente
    
            !faz o calculo do valor da integral
            DO i=1, n 
                x_i = inf + (i-0.5)*dx
                Int_PC = Int_PC + f(x_i)*dx
            END DO 
            RETURN 
        END FUNCTION Int_PC
!--------------------------------------------------------------------------------------------------------------------------------------------------------------

        !MÉTODO DO TRAPÉZIO - Referência: Wikipedia - (https://en.wikipedia.org/wiki/Trapezoidal_rule)
        !método para realizar a integração numérico de uma função usando o método do trapézio composto    
        !é necessário definir a função f(x) a ser integrada no método em um arquivo separado, denominado "funcao"        

        REAL FUNCTION Int_Tr(inf, sup, n, eps)
            IMPLICIT NONE 
            REAL    :: inf, sup, x_i, dx, eps
            INTEGER :: n, i
            !inf, sup: recebe os limites inferior e superior
            !n, i: limite de passos a serem feitos e variável "contadora"
            !x_i, dx: valor numérico a ser passado para a função e o salto de integração   
            !eps: erro assintótico

            n = CEILING(ABS(-((sup-inf)*SQRT((df(sup)-df(inf))/(12*eps)))))
            
            dx = (sup-inf)/n        !calcula o valor do passo de integração
            Int_Tr = 0.d0           !define o valor da função como 0 inicialmente
            
            !faz o calculo do valor da integral
            DO i=1, n-1 
                x_i = inf + i*dx
                Int_Tr = Int_Tr + f(x_i)
            END DO 
            Int_Tr = (Int_Tr + (f(inf)+f(sup))/2.d0)*dx
            RETURN 
        END FUNCTION Int_Tr
!--------------------------------------------------------------------------------------------------------------------------------------------------------------
include "funcao"
include "derivada"
end module METODOS
