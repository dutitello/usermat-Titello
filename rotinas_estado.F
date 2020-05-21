c*************************************************************************
c      
c     Infelizmente F77 não tem classes e objetos, então vamos adaptar 
c     o vetor de variáveis de estado como se fosse uma classe. Para isso
c     todas as rotinas que chamarem alguma das subrotinas a seguir
c     deverão incorporar em sua chamada "ustatev" e "nStatev" para 
c     que a subrotina seja conectada à raiz da usermat.
c     
c     Todas as alterações e leituras das variáveis de estado devem ser
c     realizadas através destes comandos, isso facilitará a organização
c     das variáveis de estado e futuras alterações.     
c     
c     O arquivo vars_estado.xlsx na pasta DOCS contem a organização atual
c     das variáveis de estado.
c     
c     
c     As subrotinas aqui declaradas tem nome est_xxxxxxx()
c
c
c
c
c     Eduardo Pagnussat Titello, 2019
c
c*************************************************************************
      
      

c-------------------------------------------------------------------------
c     
c     Importa propriedades de entrada para variáveis de estado
c     O mesmo indice deve ser mantido!
c         Assim apenas com nStatev, ustatev temos as props de entrada
c
c     * ESSA ROTINA DEVE SER CHAMADA NO INICIO DA USERMAT *
c
      SUBROUTINE est_imp_props(nProp, prop, nStatev, ustatev)
c         Parametros:
c         Nomeados igual da usermat, não precisa explicar...    
          INTEGER             nProp,       nStatev
          DOUBLE PRECISION    prop(nProp), ustatev(nStatev)
      
c         Variáveis locais:
c         TestePropConc   (dp,  sc) - modelo*fcm*fct*Eci
          DOUBLE PRECISION TestePropConc
          
c         Importa alguns handlers do ANSYS
          EXTERNAL vmove
          
c         Verifica se já foi preenchido, caso não, segue
          IF(ustatev(1) .EQ. 0.d0) THEN
              TestePropConc = prop(1)*prop(2)*prop(3)*prop(4)
c             Se uma das variáveis for nula vai anular o produto ;D
              IF(TestePropConc .LE. 0.d0) THEN
                  CALL DBG_erro(1)
              ELSE
c                 Passou no(s) critérios então pode seguir a execução.
c                 Copia props para vetor de vars de estado.
                  CALL vmove(prop, ustatev, nProp)
              END IF
          END IF
      END SUBROUTINE
      
c-------------------------------------------------------------------------



c-------------------------------------------------------------------------
c     
c     Rotina de controle de lambda_k p/ NewtonRaphson
c
      SUBROUTINE est_lambda_k(ac, lambda_k, nStatev, ustatev)
c         Parametros:
c         ac                  (int, sc,  in)  - Ação: 0=Retorna, 1=Salva
c         thetaTS             (dp,  sc,  io)  - Angulo theta
          DOUBLE PRECISION lambda_k
          INTEGER ac
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica e executa ação
          IF(ac .EQ. 0) THEN
c             Se valor está zerado é pq nunca foi preenchido
              IF(ustatev(51) .LE. 0.d0) ustatev(51) = 0.000001d0
              lambda_k = ustatev(51)
          ELSE
              ustatev(51) = lambda_k
          END IF
      END SUBROUTINE
      
c-------------------------------------------------------------------------

      
      
      
      
      
c-------------------------------------------------------------------------
c     
c     Define estado do ponto de integração avaliado
c
      SUBROUTINE est_DefEstPto(estado, nStatev, ustatev)
c         Parametros:
c         estado              (dp,  sc, inout)  - Estado do ponto de integração
c                 Estados são:  0 = Neutro
c                            9999 = Retorna atual
c                             -10 = Esmagado
c                              11 = Fissurado-1d
c                              12 = Fissurado-2d
          DOUBLE PRECISION estado
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica ação a ser realizada
          IF(estado .EQ. 9999.d0) THEN
              estado = ustatev(40)
          ELSE
              ustatev(40) = estado
          END IF
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      
      
c-------------------------------------------------------------------------
c     
c     Retorna estado do ponto de integração avaliado
c
       DOUBLE PRECISION FUNCTION est_EstPto(nStatev, ustatev)
c         Parametros:
c         estado              (dp,  sc, inout)  - Estado do ponto de integração
c                 Estados são:  0 = Neutro
c                            9999 = Retorna atual
c                              -1 = Comprimido
c                             -10 = Esmagado
c                              11 = Fissurado-1d
c                              12 = Fissurado-2d
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica ação a ser realizada
          est_EstPto = ustatev(40)
          
      END
      
c-------------------------------------------------------------------------
      

      
c-------------------------------------------------------------------------
c     
c     Retorna modelo de comportamento em tração a ser adotado (com fibras)
c
       DOUBLE PRECISION FUNCTION est_modTracf(nStatev, ustatev)
c         Parametros:
c         est_modTracf   (dp,  sc, out)  - Número do modelo   
          DOUBLE PRECISION est_modelo, modelo 
c         Variáveis internas
c         nd1, nd2, nd3   (dp, sc)    - não usados
c         Vf,lf, df       (dp, sc)    - propriedades das fibras
          DOUBLE PRECISION nd1, nd2, nd3, Vf, lf, df
          
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
          est_modTracf = ustatev(65)
          
c         Adotar modelo padrão? 
          IF(est_modTracf == 0.d0) THEN
              modelo = est_modelo(nStatev, ustatev)
              IF(modelo == 1.d0) ustatev(65) = -1.d0
              IF(modelo == 2.d0) ustatev(65) = -1.d0
              IF(modelo == 3.d0) ustatev(65) =  1.d0
              IF(modelo == 4.d0) ustatev(65) =  1.d0
              est_modTracf = ustatev(65)
          END IF
      
c         ATENÇÃO AQUI:
c         Verifica se existem mesmo fibras, se Vf*l*df=0 alguma é zero, então não tem ação
          CALL est_propCFibr(nd1, nd2, Vf, lf, nd3, df, 
     &                         nStatev, ustatev)
          IF(Vf*lf*df .LE. 0.d0) est_modTracf = -1
          
      END
      
c-------------------------------------------------------------------------


c-------------------------------------------------------------------------
c     
c     Retorna modelo de TS do concreto
c
       DOUBLE PRECISION FUNCTION est_modTS(nStatev, ustatev)
c         Parametros:
c         est_modTS   (dp,  sc, out)  - Número do modelo   
          DOUBLE PRECISION est_modelo, modelo 
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
          est_modTS = ustatev(70)
          
c         Adotar modelo padrão? 
          IF(est_modTS == 0.d0) THEN
              modelo = est_modelo(nStatev, ustatev)
              IF(modelo == 1.d0) ustatev(70) =  1.d0
              IF(modelo == 2.d0) ustatev(70) =  1.d0
              IF(modelo == 3.d0) ustatev(70) =  1.d0
              IF(modelo == 4.d0) ustatev(70) = -1.d0
              est_modTS = ustatev(70)
          END IF
      END
      
c-------------------------------------------------------------------------

      
c-------------------------------------------------------------------------
c     
c     Retorna modelo de Plasticidade a ser adotado
c
       DOUBLE PRECISION FUNCTION est_modPlast(nStatev, ustatev)
c         Parametros:
c         est_modPlast   (dp,  sc, out)  - Numero do modelo   
       
c         Variáveis locais:
c         modelo         (dp,  sc) - modelo
          DOUBLE PRECISION est_modelo, modelo
       
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Os modelos plasticidade são:
c         1) Ottosen (SEM fibras)   com fluxo plástico de Von-Mises
c         2) Willam-Warnke SEM fibras e fluxo plástico de Von-Mises 
c         3) Willam-Warnke COM fibras e fluxo plástico de Von-Mises
          
c         Importa modelo de entrada da usermat
          modelo = est_modelo(nStatev, ustatev)
          IF(modelo == 1.d0) est_modPlast = 1.d0
          IF(modelo == 2.d0) est_modPlast = 2.d0
          IF(modelo == 3.d0) est_modPlast = 3.d0
          IF(modelo == 4.d0) est_modPlast = 3.d0
          
      END
      
c-------------------------------------------------------------------------
    
      
c-------------------------------------------------------------------------
c     
c     Retorna modelo adotado (GERAL, PROP 1)
c
       DOUBLE PRECISION FUNCTION est_modelo(nStatev, ustatev)
c         Parametros:
c         est_modelo   (dp,  sc, out)  - Numero do modelo   
       
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Os modelos de entrada da usermat são:
c                 [TS=Tension Stiffening]
c         1= Ottosen SEM fibras; TS de Martinelli;
c         2= Willam-Warnke SEM fibras; TS de Martinelli;
c         3= Willam-Warnke COM fibras; Tração modelo VEM; COM TS de Martinelli;
c         4= Willam-Warnke COM fibras; Tração modelo VEM; SEM TS;

c         Importa modelo de entrada da usermat
          est_modelo = ustatev(1)

c         Se tu procuras onde montar um novo modelo é o seguinte:
c          1) Define e anota aqui o modelo plástico, TS, tracf
c          2) Entra nas rotinas est_modPlast, est_modTS, est_modTracf
c             e adiciona uma linha de condição pro teu modelo,
c             se modelo=X então plast=Y, TS=Z, tracf=W.
c          3) Se for um novo modelo de plasticidade, endurecimento,
c             TS ou tração de fibras precisa criar as rotinas e por
c             condições nas funções de plasticidade e tração.
      END
      
c-------------------------------------------------------------------------
      

      
c-------------------------------------------------------------------------
c     
c     Retorna os parametros da sup. de Ottosen 
c
      SUBROUTINE est_parOttosen(alpha, beta, lambdaC, lambdaT, 
     &                          nStatev, ustatev)
c         Parametros:
c         alpha, beta         (dp,  sc, out)  - parametros de Ottossen
c         lambdaT, lambdaC    (dp,  sc, out)  - parametros de Ottossen
          DOUBLE PRECISION alpha, beta, lambdaT, lambdaC
          
c         Variáveis internas:
c         calc                (dp,  sc)       - Parametros conhecidos?
c         fcm e fct           (dp,  sc)       - Props a tração e comp do concreto
c         nd                  (dp,  sc)       - P/ valores não usados
          DOUBLE PRECISION calc, fcm, fct, nd
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Está sendo usado o modelo de Ottosen?
          IF(ustatev(1) .EQ. 1.d0) THEN
c             Os parametros já foram determinados?
              calc = ustatev(52)
              IF(calc .EQ. 0.d0) THEN
                  CALL est_propConc(fcm, fct, nd, nd, nStatev, ustatev)
                  CALL conc_parOttosen(fcm, fct, ustatev(53:56))
                  ustatev(52) = 1.d0
              END IF
              alpha   = ustatev(53)
              beta    = ustatev(54)
              lambdaC = ustatev(55)
              lambdaT = ustatev(56)
          ELSE
c             Isso não pode acontecer!!
              CALL DBG_erro(2)
          END IF

      END SUBROUTINE
      
c-------------------------------------------------------------------------

      
      
      
      
c-------------------------------------------------------------------------
c     
c     Retorna os parametros da sup. de Willam-Warnke (pura) 
c
      SUBROUTINE est_parWW(TPrinc1, a0, a1, a2, b0, b1, b2, 
     &                   nStatev, ustatev)
c         Parametros:
c         TPrinc1             (dp,  sc,  in)  - Primeira (maior) tensão principal
c         a0, a1, a2          (dp,  sc, out)  - Constantes do meridiano de tração
c         b0, b1, b2          (dp,  sc, out)  - Constantes do meridiano de compressão
          DOUBLE PRECISION TPrinc1, a0, a1, a2, b0, b1, b2
          
c         Variáveis internas:
c         calc                (dp,  sc)       - Parametros conhecidos?
c         fcm e fct           (dp,  sc)       - Props a tração e comp do concreto
c         alpu                (dp,  sc)       - Alpha p/ constantes em caso de tração
c         nd                  (dp,  sc)       - P/ valores não usados
          DOUBLE PRECISION calc, fcm, fct, nd, alpu
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------

c         Importa propriedades do concreto
          CALL est_propConc(fcm, fct, nd, nd, nStatev, ustatev)
          
c         Sem fibras de aço
c         Verifica se existe uma tensão principal de tração
          IF(TPrinc1 .GT. 0.0001d0) THEN
c             Existe uma tensão principal de tração, verifica se os parametros foram determinados
              calc = ustatev(52)
              IF(calc .EQ. 0.d0) THEN
c                 Parametros desconhecidos
                  alpu = fct/fcm
                  a0 = -(19.68d0*alpu + 1.82d0*alpu**2.d0)/
     &                 (-12.62d0 + 13.49d0*alpu -2.48d0*alpu**2.d0)
                  a1 = (1.52d0 + 0.95d0*alpu + 0.54d0*alpu**2.d0)/
     &                 (-1.26d0 + 1.35d0*alpu - 0.25d0*alpu**2.d0)
                  a2 = -(-2.73d0 + 8.73d0*alpu)/
     &                 (-12.62d0 + 13.49d0*alpu -2.48d0*alpu**2.d0)
                  
                  b0 = a0
                  b1 = (0.62d0 + 2.44d0*alpu + 0.42d0*alpu**2.d0)/
     &                 (-1.26d0 + 1.35d0*alpu - 0.25d0*alpu**2.d0)
                  b2 = -(-2.86d0 + 10.76d0*alpu + 0.017d0*alpu**2.d0)/
     &                 (-12.62d0 + 13.49d0*alpu - 2.48d0*alpu**2.d0)
                  
c                 Grava constantes
                  ustatev(53) = a0
                  ustatev(54) = a1
                  ustatev(55) = a2
                  ustatev(56) = b1
                  ustatev(57) = b2
                  
c                 Marca parametros como conhecidos
                  ustatev(52) = 1.d0
              ELSE
c                 Parametros já foram cálculados
                  a0 = ustatev(53)
                  a1 = ustatev(54)
                  a2 = ustatev(55)
                  b0 = a0
                  b1 = ustatev(56)
                  b2 = ustatev(57)
              END IF
          ELSE 
c             Tensão de compressão, constantes fixas
              a0 =  0.1775d0
              a1 = -1.4554d0
              a2 = -0.1576d0
              b0 =  a0
              b1 = -0.7807d0
              b2 = -0.1763d0
          END IF
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------    

      

      
c-------------------------------------------------------------------------
c     
c     Retorna os parametros da sup. de Willam-Warnke com FIBRAS 
c
      SUBROUTINE est_parWWf(TPrinc1, a0, a1, a2, b0, b1, b2, k,
     &                   nStatev, ustatev)
c         Parametros:
c         TPrinc1             (dp,  sc,  in)  - Primeira (maior) tensão principal
c         a0, a1, a2          (dp,  sc, out)  - Constantes do meridiano de tração
c         b0, b1, b2          (dp,  sc, out)  - Constantes do meridiano de compressão
c         k                   (dp,  sc, out)  - k para alteração do meridiano de tração
          DOUBLE PRECISION TPrinc1, a0, a1, a2, b0, b1, b2, k
          
c         Variáveis internas:
c         calc                (dp,  sc)       - Parametros conhecidos?
c         fcm e fct           (dp,  sc)       - Props a tração e comp do concreto
c         fcc                 (dp,  sc)       - Resistencia a compressão biaxial
c         sig_tu              (dp,  sc)       - Tensão de confinamento gerada pelas fibras
c         alpu                (dp,  sc)       - Alpha p/ constantes em caso de tração
c         nd1,nd2,nd3,nd4,nd5 (dp,  sc)       - P/ valores não usados
c         bskA, bskB, bskC    (dp,  sc)       - Termos da equação de bhaskara p/ fcc
          DOUBLE PRECISION calc, fcm, fct, nd, alpu, sig_tu, fcc, 
     &                     bskA, bskB, bskC, bskDt, nd1,nd2,nd3,nd4,nd5
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------

c         Importa propriedades do concreto
          CALL est_propConc(fcm, fct, nd, nd, nStatev, ustatev)
          CALL est_propCFibr(sig_tu, nd1,nd2,nd3,nd4,nd5, 
     &                       nStatev, ustatev)
          
c         Determina constantes de WW
c         Verifica se existe uma tensão principal de tração
          IF(TPrinc1 .GT. 0.0001d0) THEN
c             Existe uma tensão principal de tração, verifica se os parametros foram determinados
              calc = ustatev(52)
              IF(calc .EQ. 0.d0) THEN
c                 Parametros desconhecidos
                  alpu = fct/fcm
                  a0 = -(19.68d0*alpu + 1.82d0*alpu**2.d0)/
     &                 (-12.62d0 + 13.49d0*alpu -2.48d0*alpu**2.d0)
                  a1 = (1.52d0 + 0.95d0*alpu + 0.54d0*alpu**2.d0)/
     &                 (-1.26d0 + 1.35d0*alpu - 0.25d0*alpu**2.d0)
                  a2 = -(-2.73d0 + 8.73d0*alpu)/
     &                 (-12.62d0 + 13.49d0*alpu -2.48d0*alpu**2.d0)
                  
                  b0 = a0
                  b1 = (0.62d0 + 2.44d0*alpu + 0.42d0*alpu**2.d0)/
     &                 (-1.26d0 + 1.35d0*alpu - 0.25d0*alpu**2.d0)
                  b2 = -(-2.86d0 + 10.76d0*alpu + 0.017d0*alpu**2.d0)/
     &                 (-12.62d0 + 13.49d0*alpu - 2.48d0*alpu**2.d0)
                  
c                 Grava constantes
                  ustatev(53) = a0
                  ustatev(54) = a1
                  ustatev(55) = a2
                  ustatev(56) = b1
                  ustatev(57) = b2
                  
c                 Marca parametros como conhecidos
                  ustatev(52) = 1.d0
              ELSE
c                 Parametros já foram cálculados
                  a0 = ustatev(53)
                  a1 = ustatev(54)
                  a2 = ustatev(55)
                  b0 = a0
                  b1 = ustatev(56)
                  b2 = ustatev(57)
              END IF
          ELSE 
c             Tensão de compressão, constantes fixas
              a0 =  0.1775d0
              a1 = -1.4554d0
              a2 = -0.1576d0
              b0 =  a0
              b1 = -0.7807d0
              b2 = -0.1763d0
          END IF
          
c         Importa fcc
          fcc = ustatev(61)
          k = ustatev(62)
          
          IF(fcc .LE. 0.d0) THEN
c             Determina fcc por baskara
              bskA = 2.d0/3.d0*a2/fcm**2.d0
              bskB = (2.d0+DSQRT(2.d0)*a1)/(DSQRT(3.d0)*fcm) 
     &               - 4.d0/3.d0*a2*sig_tu/fcm**2.d0
              bskC = 2.d0/3.d0*a2*sig_tu**2.d0/fcm**2.d0 + 
     &               sig_tu/fcm*(1.d0-a1*DSQRT(2.d0))/DSQRT(3.d0) + a0
          
              bskDt = bskB**2.d0 - 4.d0*bskA*bskC
          
              IF(bskDt .GE. 0.d0) THEN 
c             Curiosidade: aqui é + ou -??
                  fcc = (-bskB-DSQRT(bskDt))/(2.d0*bskA)
                  nd = (-bskB+DSQRT(bskDt))/(2.d0*bskA)
              ELSE
                  fcc = 0.d0
              END IF
              ustatev(61) = fcc
          END IF
          
          IF(k .LE. 0.d0) THEN    
c             Determina k
              k = a0 + 2.d0/DSQRT(3.d0)*fcc/fcm
              k = a1**2.d0 - 4.d0*a2*k
              k = -a1 - DSQRT(k)
              k = k/(a2*DSQRT(8.d0/3.d0)*fcc/fcm)
              ustatev(62) = k
          END IF
              
      END SUBROUTINE
      
c-------------------------------------------------------------------------    
      



c-------------------------------------------------------------------------
c     
c     Retorna propriedades gerais do concreto
c
      SUBROUTINE est_propConc(fcm, fct, Eci, poisson, 
     &                        nStatev, ustatev)
c         Parametros:
c         fcm             (dp,  sc, out) - resistencia média a compressão
c         fct             (dp,  sc, out) - resistencia média a tração
c         Eci             (dp,  sc, out) - Modulo de elasticidade Eci do concreto
c         poisson         (dp,  sc, out) - Coeficiente de Poisson
          DOUBLE PRECISION fcm, fct, Eci, poisson
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Estes parametros ficam nos espaços (5:9)
          fcm     = ustatev(2)
          fct     = ustatev(3)
          Eci     = ustatev(4)
          poisson = ustatev(5)
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      

c-------------------------------------------------------------------------
c     
c     Retorna propriedades gerais do concreto
c
      SUBROUTINE est_propCFibr(sig_tu, tau_u, Vf, lf, lfi, df, 
     &                         nStatev, ustatev)
c         Parametros:
c         sig_tu          (dp,  sc, out)  - Tensão de confinamento gerada pelas fibrasEci do concreto
c         tau_u           (dp,  sc, out)  - Tensão última de aderencia (aqui na verdade é média)
c         Vf              (dp,  sc, out)  - Volume de fibras
c         lf              (dp,  sc, out)  - Comprimento das fibras
c         lfi             (dp,  sc, out)  - Comprimento reto da fibra (centro)
c         df              (dp,  sc, out)  - Diametro das fibras
          DOUBLE PRECISION tau_u, sig_tu, Vf, lf, lfi, df
          
c         Variáveis internas:
c         efic        (dp,  sc)   - Fator de eficiencia baseado no tipo de fibra
c         fcm         (dp,  sc)   - Resistência a compressão do concreto
c         fct         (dp,  sc)   - Tensão de ruptura do concreto puro
c         sig_fu      (dp,  sc)   - Tensão última de tração da fibra
c         lfc         (dp,  sc)   - Comprimento critico das fibras
c         eta_l       (dp,  sc)   - Correção do comprimento das fibras
c         nd          (dp,  sc)   - Não usado
          DOUBLE PRECISION fcm, fct, nd, efic, sig_fu,
     &                     lfc, eta_l
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Importa propriedades do concreto puro
          CALL est_propConc(fcm, fct, nd, nd, nStatev, ustatev)

c         Lembrete pro Eduardo do futuro:
c         Nessa função surgiu o problema de endereço de memoria:
c         se colocar nd em todos os campos que não vou usar,aqui
c         dentro ficam no mesmo endereço e não posso trabalhar 
c         internamente...
          
c         Pega valores das variáveis de estado
          sig_tu  = ustatev(58)
          tau_u   = ustatev(60)
          sig_fu  = ustatev(6)
          Vf      = ustatev(7)
          lf      = ustatev(8)
          lfi     = ustatev(9)
          df      = ustatev(10)
          
          
c         Verifica se propriedades essencias não são conhecidas
          IF(sig_tu*tau_u .LE. 0.d0) THEN
c             Determina fator de eficiencia
              IF(lf .NE. lfi) THEN
c                 Fibra COM ancoragem
                  efic = 1.0d0
              ELSE
c                 Fibra SEM ancoragem
                  efic = 0.48d0
              END IF
              
c             Calcula tau_u (Conforme em Voo e Foster 2003)
              IF(tau_u .LE. 0.d0) THEN
                  tau_u = 2.5d0*efic*fct
                  ustatev(60) = tau_u
              END IF
          
c             Calcula sig_tu
              IF(sig_tu .LE. 0.d0) THEN
c                 Determina comprimento critico e eta_l
                  lfc = 0.5d0*sig_fu*df/tau_u
                  ustatev(63) = lfc
                  eta_l = 0.5d0
c                 sig_tu:
                  IF(lf .GT. lfc) eta_l = 1.d0 - 0.5d0*lfc/lf
                  sig_tu = 0.405d0*eta_l*Vf*lf*tau_u*2.d0/df
                  ustatev(58) = sig_tu
              END IF

          END IF
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      
    
      
c-------------------------------------------------------------------------
c     
c     Retorna propriedades do TS do concreto
c
      SUBROUTINE est_propTS(props, nprops, nStatev, ustatev)
c         Parametros:
c         props           (dp, nx1, out) - propriedades de tamanho nprops
c         nprops          (int, sc,  in) - Número de propriedades a serem retornadas
          INTEGER nprops, i
          DOUBLE PRECISION props(nprops)
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Estes parametros ficam nos espaços 71 a 74 (4 props)
          props = 0.d0
          IF(nprops .GT. 4) nprops = 4          
          
c         Verifica se valores estão preenchidos, caso contrário define padrão
          IF(ustatev(70) .EQ. 1.d0) THEN
c             Modelo 1: Martineli
              IF(ustatev(71) == 0.d0) ustatev(71) = 0.6000d0  ! alfac = 0.60
              IF(ustatev(72) == 0.d0) ustatev(72) = 0.0025d0  ! eps_cu = 50/20000 = 0.0025
              
          ELSEIF(ustatev(70) .EQ. 2.d0) THEN
c             Modelo 2: exponencial para concreto
              IF(ustatev(71) == 0.d0) ustatev(71) = 150.d0    ! c = 150 (para centimetros)
          END IF

c         Traz propriedades para vetor
          DO i=1,nprops
              props(i) = ustatev(70+i)
          END DO
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      
      
      
c-------------------------------------------------------------------------
c     
c     Retorna propriedades do concreto com fibras em tração
c
      SUBROUTINE est_propTracf(props, nprops, nStatev, ustatev)
c         Parametros:
c         props           (dp, nx1, out) - propriedades de tamanho nprops
c         nprops          (int, sc,  in) - Número de propriedades a serem retornadas
          INTEGER nprops, i
          DOUBLE PRECISION props(nprops)
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Estes parametros ficam nos espaços 66 a 69 (4 props)
          props = 0.d0
          IF(nprops .GT. 4) nprops = 4          
          
          IF(ustatev(65) .EQ. 2.d0) THEN
c             Modelo 2: SDEM p/ fibras
              IF(ustatev(66) == 0.d0) ustatev(66) = 150.0d0   ! c = 150 (para centimetros)
              IF(ustatev(67) == 0.d0) ustatev(67) = 0.001d0   ! s_f = 0.001 cm
              IF(ustatev(68) == 0.d0) ustatev(68) = 0.010d0   ! s_eh = 0.01 cm
          
          ELSEIF(ustatev(65) .EQ. 1.d0) THEN
c             Modelo 1: VEM p/ fibras
              IF(ustatev(66) == 0.d0) ustatev(66) = 150.0d0   ! c = 150 (para centimetros)
          END IF    
          
c         Traz propriedades para vetor
          DO i=1,nprops
              props(i) = ustatev(65+i)
          END DO
          
      END SUBROUTINE
      
c------------------------------------------------------------------------- 
      
      
      
 
      
      
c-------------------------------------------------------------------------
c     
c     Propriedades da Curva uniaxial do MC2010
c
      SUBROUTINE est_propConc_UnMC2010(Ec1, eps_clim, k, eps_c1, 
     &                                 nStatev, ustatev)
c         Parametros:
c         eps_c1      (dp,  sc)   - deformação na maxima tensão de compressão
c         Ec1         (dp,  sc)   - modulo Ec correspondente a eps1
c         eps_clim    (dp,  sc)   - deformação última de compressão
c         k           (dp,  sc)   - número plástico Eci/Ec1
          DOUBLE PRECISION Ec1, eps_clim, k, eps_c1
          
c         Variáveis internas:
c         fcm         (dp,  sc)   - Resistência a compressão do concreto
c         Eci         (dp,  sc)   - Modulo de elasticidade Eci do concreto
c         nd          (dp,  sc)   - Não usado
          DOUBLE PRECISION fcm, Eci, nd
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica se valores não foram cálculados
          Ec1 = ustatev(32) 
          IF(Ec1 .EQ. 0.d0) THEN
c             Importa propriedades do Concreto
              CALL est_propConc(fcm, nd, Eci, nd, nStatev, ustatev)
          
c             Valores de Ec1 e eps_clim aproximados pelos valores do MC2010
c             as expressões tem R²>=0.99 e estão em função de fcm=fck+8MPa
c             Na pasta DOCS está o arquivo do Excel com as aproximações

c             Ec1 é aproximado linearmente
              Ec1 = 288.53d0*fcm + 578.51d0
          
c             eps_clim é quadratico em: 4.5<=fck<=9kN/cm² ou 5.3<=fcm<=9.8kN/cm²
c                 sendo limitado ao invervalo: 3<=eps_clim<=3.5
              eps_clim = 0.0035d0*fcm**2.d0 - 0.1613d0*fcm + 4.2474d0
              IF(eps_clim > 3.5d0) eps_clim = 3.5d0
              IF(eps_clim < 3.0d0) eps_clim = 3.0d0
c             Na verdade isso é negativo e /1000  xD
              eps_clim = -eps_clim/1000.d0
          
c             Demais parametros a serem calculados
              k = Eci/Ec1
              eps_c1 = fcm/Ec1
              
              ustatev(32) = Ec1
              ustatev(33) = eps_clim
              ustatev(34) = k
              ustatev(35) = eps_c1
              
          ELSE
c             Armazena valores das variáveis de estado
              Ec1         = ustatev(32) 
              eps_clim    = ustatev(33) 
              k           = ustatev(34) 
              eps_c1      = ustatev(35) 
          END IF
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      
      
      
c-------------------------------------------------------------------------
c     
c     Sinaliza para interromper solução
c
      SUBROUTINE est_interromper(cont, nStatev, ustatev)
c         Parametros:
c         cont    (int, sc, inout)  - Controle: 9999=Consulta, <>0=Interromper=Codigo
          DOUBLE PRECISION cont
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica se deve retornar estado ou sinalizar p/ interromper
          IF(cont .EQ. 9999.d0) THEN
              cont = ustatev(31)
          ELSE
              ustatev(31) = cont
          END IF
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      

c-------------------------------------------------------------------------
c     
c     Verifica sinais de Interrupção de est_interromper()
c
      SUBROUTINE est_ver_interp(keycut, cutFactor, nStatev, ustatev)
c         Parametros:
c         keycut    (int, sc, out)  - Variavel do ANSYS p. ativar bissecao
c         cutFactor (dp,  sc, out)  - Variavel do ANSYS com controle de bissec
c     
c         Var internas:
c         cont      (dp,  sc)       - Retorno de est_interromper()
          INTEGER keycut
          DOUBLE PRECISION cutFactor, cont 
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica se deve interromper
          cont = 9999.d0
          CALL est_interromper(cont, nStatev, ustatev)
          
          IF(cont .NE. 0.d0) THEN
              keycut = 1
              cutFactor = 0.d0
          END IF
      END SUBROUTINE
      
c-------------------------------------------------------------------------      

      

      
c-------------------------------------------------------------------------
c     
c     Define e Retorna angulo theta de orientação da fissura
c
      SUBROUTINE est_ThetaEPT(ac, thetaTS, nStatev, ustatev)
c         Parametros:
c         ac                  (int, sc,  in)  - Ação: 0=Retorna, 1=Salva
c         thetaTS             (dp,  sc,  io)  - Angulo theta
          DOUBLE PRECISION thetaTS
          INTEGER ac
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica e executa ação
          IF(ac .EQ. 0) THEN
              thetaTS = ustatev(41) 
          ELSE
              ustatev(41) = thetaTS
          END IF
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      

      
    
c-------------------------------------------------------------------------
c     
c     Define e retorna tensões e deformações máximas nas fissuras (1 ou 2)
c
      SUBROUTINE est_Fiss_DT(ac, nfis, stress, strain, nStatev, ustatev)
c         Parametros:
c         ac                  (int, sc,  in)  - Ação: 0=Retorna, 1=Salva
c         nfis                (int, sc,  in)  - Fissura numero? (1 ou 2?)
c         stress              (dp,  sc,  io)  - Tensão máxima
c         strain              (dp,  sc,  io)  - Deformação máxima      
          DOUBLE PRECISION stress, strain
          INTEGER ac, nfis
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica e executa ação
          IF(ac .EQ. 0) THEN
              IF(nfis .EQ. 1) THEN
                  stress = ustatev(44)
                  strain = ustatev(45)
              ELSEIF(nfis .EQ. 2) THEN
                  stress = ustatev(46)
                  strain = ustatev(47)    
              END IF
          ELSE
              IF(nfis .EQ. 1) THEN
                  ustatev(44) = stress
                  ustatev(45) = strain
              ELSEIF(nfis .EQ. 2) THEN
                  ustatev(46) = stress
                  ustatev(47) = strain                  
              END IF
          END IF
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      

      
c-------------------------------------------------------------------------
c     
c     Determina e retorna comprimento característico do elemento (p/ EPT)
c
      SUBROUTINE est_EPT_lc(lc, nStatev, ustatev)
c         Parametros:
c         lc      (dp,  sc,  out)  - Comprimento caracteristico do ponto de integração
          DOUBLE PRECISION lc
          
c         Variáveis internas:
c         k           (int, sc)       - Parametro da função =1
c         kDomIntPt   (int, sc)       - ponto de integração
c         elemId      (int, sc)       - elemento
          INTEGER k, kDomIntPt, elemId
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          EXTERNAL get_ElmData1
          
c         Retorna comprimento da memoria
          lc = ustatev(88)
          
          IF(lc .EQ. 0.d0) THEN
c             Pega elemId e kDomIntPt
              CALL est_elem_kdom(0, elemId, kDomIntPt, nStatev, ustatev)
c             Determina área do ponto de integração (pq o elemento é plano!).
c             Deve ser usado uma variável tipo k, igual a 1, para chamar a subrotina,
c             ela não está documentada, se usar direto 1 buga, deve ser variável IO.
              k = 1
              CALL get_ElmData1(10, elemId, kDomIntPt, k, lc) 
c             Agora transforma área em comprimento caracteristico
              lc = DSQRT(lc)
              ustatev(88) = lc
          END IF
      
!   ---------------
!         http://xansys.org/forum/viewtopic.php?p=63375&sid=af48e0820be726cfb056e94128308ba4
!         If you use ansys 81 or up, you can use following API to get the volume fraction 
!         at the integration point of the current element from usermat for element 
!         182, 183, 185, 186, 187.
!
!         i = 1
!         call get_ElmData1(10, elemId, kDomIntPt, i, value)
!
!         Guoyu
!   --------------
          
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      
      

c-------------------------------------------------------------------------
c     
c     Grava ElemId e kDomIntPt nas variáveis de estado
c         (util para debug)
c
      SUBROUTINE est_elem_kdom(ac, elem, kdom, nStatev, ustatev)
c         Parametros:
c         ac                  (int, sc,  in)  - Ação: 0=Retorna, 1=Salva
c         elem                (int, sc,  io)  - ElemId
c         kdom                (int, sc,  io)  - kDomIntPt
          INTEGER ac, elem, kdom
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica e executa ação
          IF(ac .EQ. 0) THEN
              elem = nint(ustatev(85))
              kdom = nint(ustatev(86))
          ELSE
              ustatev(85) = elem
              ustatev(86) = kdom
          END IF
      END SUBROUTINE
      
c-------------------------------------------------------------------------
           
      
c-------------------------------------------------------------------------
c     
c     Grava coordenadas iniciais dos nós nas variáveis de estado
c
      SUBROUTINE est_coords0(ac, coords, nStatev, ustatev)
c         Parametros:
c         ac                  (int, sc,  in)  - Ação: 0=Retorna, 1=Salva
c         coords              (dp, 3x1,  io)  - Vetor de coordenadas
          INTEGER ac
          DOUBLE PRECISION coords(3)
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Verifica e executa ação
          IF(ac .EQ. 0) THEN
              coords(1) = ustatev(89)
              coords(2) = ustatev(90)
              coords(3) = ustatev(91)
          ELSE
              ustatev(89) = coords(1)
              ustatev(90) = coords(2)
              ustatev(91) = coords(3)
          END IF
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      
  
      
c-------------------------------------------------------------------------
c     
c     Retorna região e angulo de aplicabilidade do TS
c
      SUBROUTINE est_limitesTS(angl, xi, xf, yi, yf, nStatev, ustatev)
c         Parametros:
c         angl    (dp, sc, out)  - angulo limite
c         xi      (dp, sc, out)  - x inicial
c         xf      (dp, sc, out)  - x final
c         yi      (dp, sc, out)  - y inicial
c         yf      (dp, sc, out)  - y final

          DOUBLE PRECISION angl, xi, xf, yi, yf
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Estes parametros ficam nos espaços (75:79)
          angl = ustatev(75)
          xi = ustatev(76)
          xf = ustatev(77)
          yi = ustatev(78)
          yf = ustatev(79)
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      
      
            
      
      
c-------------------------------------------------------------------------
c     
c     Rotina de exemplo
c
      SUBROUTINE est_exemplo(alpha, beta, lambdaT, lambdaC, 
     &                          nStatev, ustatev)
c         Parametros:
c         alpha, beta         (dp,  sc, out)  - parametros de Ottossen
c         lambdaT, lambdaC    (dp,  sc, out)  - parametros de Ottossen
          DOUBLE PRECISION alpha, beta, lambdaT, lambdaC
          
c         Variáveis internas:
c         calc                (dp,  sc)       - Parametros conhecidos?
          DOUBLE PRECISION calc
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c
c         Estes parametros ficam nos espaços (5:9)
          calc    = ustatev(31)
          alpha   = ustatev(32)
          beta    = ustatev(33)
          lambdaT = ustatev(34)
          lambdaC = ustatev(35)
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------
      
      
      
      