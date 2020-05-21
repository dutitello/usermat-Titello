c*************************************************************************
c     
c     Subrotinas relativas ao concreto - conc_xxxx
c
c
c     Eduardo Pagnussat Titello, 2019
c
c*************************************************************************
      
      
c-----------------------------------------------------------------------
c     
c     Determina os parametros da superfície de Ottosen conforme
c     MC2010 da fib em função das props do concreto
c     
      SUBROUTINE conc_parOttosen(fcm, fct, parOttosen)
c         Parametros da superfície de Ottosen
c     
c         Parametros:
c         fcm             (dp,  sc,  in) - resistencia média a compressão
c         fct             (dp,  sc,  in) - resistencia média a tração
c         parOttosen      (dp, 4x1, out) - 4 parametros de Ottosen 
c             #1=alphaOtt, #2=betaOtt, #3=lambdaCOtt, #4=lambdaTOtt
          DOUBLE PRECISION fcm, fct, parOttosen(4)
c
c         Variáveis internas:
c         pelo MC2010:
c         f2c, fc2cm, Scom, Tcom, hOtt, kOtt, xOtt,  
c         yOtt, betaOtt, lambdaCOtt, lambdaTOtt
          DOUBLE PRECISION f2c, fc2cm, Scom, Tcom, hOtt, kOtt, xOtt,
     &                     yOtt, betaOtt, lambdaCOtt, lambdaTOtt, 
     &                     alphaOtt

c         Anula vetor de parametros
          parOttosen = 0.0D0

c         Conforme MC2010
          Scom = -24.0D0
          Tcom = (18.5D0 - 18.D0*fcm/10.D0 + 26.D0*(fcm/10.D0)**2.D0
     &        - 8.4D0*(fcm/10.D0)**3.D0)
      
          fc2cm = (1.2D0-fcm/100.D0)*fcm
          f2c = fc2cm/fcm
      
          kOtt = fct/fcm
          xOtt = Scom/fcm
          yOtt = Tcom/fcm
      
          hOtt = -(DSQRT(2.0D0)*xOtt + yOtt)/(yOtt/(DSQRT(2.0D0)) 
     &        - 1.0D0/3.0D0)
      
          betaOtt = (DSQRT(2.0D0) - (3.0D0*yOtt/(kOtt*f2c)))/
     &       (hOtt - 9.0D0*yOtt/(f2c-kOtt))
      
          alphaOtt = (hOtt*betaOtt - DSQRT(2.0D0))/yOtt
      
          lambdaCOtt = (1.0D0 -hOtt/(3.0D0*yOtt))*DSQRT(3.0D0)*betaOtt+
     &                  DSQRT(3.0D0) + DSQRT(2.0D0)/(DSQRT(3.0D0)*yOtt)
      
          lambdaTOtt =    (2.0D0*DSQRT(3.0D0) 
     &                - f2c*hOtt/(DSQRT(3.0D0)*yOtt))*betaOtt
     &                + DSQRT(3.0D0)/f2c + 
     &                DSQRT(2.0D0)*f2c/(DSQRT(3.0D0)*yOtt)
      
          parOttosen(1) = alphaOtt
          parOttosen(2) = betaOtt
          parOttosen(3) = lambdaCOtt
          parOttosen(4) = lambdaTOtt
      
      END SUBROUTINE
c-------------------------------------------------------------------------


c-------------------------------------------------------------------------
c
c     Regra de endurecimento conforme curva uniaxial do MC2010 da fib
c
      SUBROUTINE conc_End_MC2010(eps_cp, TConc, esmagado, 
     &                           nStatev, ustatev)
c     SUBROUTINE conc_End_MC2010(eps_cp, TConc, eps_c, nStatev, ustatev)
c         Regra de endurecimento do concreto extrapolada da curva
c         uniaxial do MC2010, é necessário transformar a euqação
c         em função da deformação plástica (ver Lazzari 2016 e outros).
      
c         Parametros:
c         eps_cp      (dp,  sc,  in)  - Deformação plástica equivalente
c         TConc       (dp,  sc, out)  - Tensão equivalente obtida no concreto
c         eps_c       (dp,  sc, out)  - Deformação total equivalente no concreto
c         esmagado    (dp,  sc, out)  - Controle de esmagamento
          DOUBLE PRECISION eps_cp, TConc, eps_c, esmagado
      
c         Variáveis internas:
c         eps_c1      (dp,  sc)   - deformação na maxima tensão de compressão
c         Ec1         (dp,  sc)   - modulo Ec correspondente a eps1
c         eps_clim    (dp,  sc)   - deformação última de compressão
c         k           (dp,  sc)   - número plástico Eci/Ec1
c         fcm         (dp,  sc)   - Resistência a compressão do concreto
c         Eci         (dp,  sc)   - Modulo de elasticidade Eci do concreto
c         nd          (dp,  sc)   - Não usado
c         ba,bb,bc,bd (dp,  sc)   - parametros de Baskhara
c         TConc_lim   (dp,  sc)   - Tensão uniaxial na deformação limite
c         Ec_fic      (dp,  sc)   - Modulo de elasticidade ficticio p/ reduzir diagrama a zero
c         ficR        (dp,  sc)   - Fator de tamanho do trecho para anular concreto
c
          DOUBLE PRECISION eps_c1, Ec1, eps_clim, k, ba, bb, bc, bd,
     &                     fcm, Eci, nd, ficR, Ec_fic, TConc_lim
c
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c         
c         Importa as propriedades do concreto
          CALL est_propConc(fcm, nd, Eci, nd, nStatev, ustatev)
          CALL est_propConc_UnMC2010(Ec1, eps_clim, k, eps_c1, 
     &                               nStatev, ustatev)


c         Determina parametros para aplicar Baskhara
c             (é assim que escreve?)
          ba = (k-2.d0)/fcm + 1.d0/(eps_c1*Eci)
          ba = ba / (eps_c1*Eci)
          
          bb = 1.d0/fcm*(1.d0 + eps_cp/eps_c1*(k-2.d0))
          bb = bb - (k - 2.d0*eps_cp/eps_c1)/(eps_c1*Eci)
          
          bc = -eps_cp/eps_c1*(k-eps_cp/eps_c1)
          
c         Delta p/ verificar existência de raizes
          bd = bb**2.d0 - 4.d0*ba*bc
          IF(bd .GE. 0.d0) THEN
              TConc = (-bb + DSQRT(bd))/2.d0/ba
          ELSE
              TConc = 0.d0
          END IF
          
c         Garantia que TConc >= 0
          IF(TConc .LT. 0.d0) TConc = 0.d0
          
c         Deformação equivalente total
          eps_c = TConc/Eci + eps_cp
          
c         Critérios de esmagamento:     
c          Inicializa variáveis
           esmagado = 0.d0
           Ec_fic   = 0.d0
          
c         Critério de esmagamento definido conforme ficR:
c             Se ficR >  0.0: reta linear após eps_clim até (1+R)*eps_clim baixando tensão -> Não faz mais sentido usar isso!!!!
c             Se ficR = -1.0: Após eps_clim anula tensão diretamente -> Schimitz (FAZ MAIS SENTIDO)
c             Se ficR = -2.0: Diagrama tens-def até anular tensão -> Lazzari
          ficR = -1.d0   

          
c         Muda sinal de eps_clim para positivo
          eps_clim = -eps_clim
          
c         Critérios de esmagamento:
          IF(ficR .GT. 0.d0) THEN
c         Meu critério (uma reta para zerar diagrama de extensão R*eps_clim)
              IF((eps_c .GT. eps_clim) .AND.
     &            (eps_c .LT. (1.d0+ficR)*eps_clim)) THEN
c                 Se está no invervalo de amolecimento pós esmagamento
c                 Tensão uniaxial para deformação limite
                  TConc_lim = (k*eps_clim/eps_c1 - 
     &                         (eps_clim/eps_c1)**2.d0)
                  TConc_lim = TConc_lim/(1.d0 + (k - 2.d0)
     &                         *eps_clim/eps_c1)
                  TConc_lim = fcm*TConc_lim
c                 Modulo de elasticidade fictício  (negativo p/ descer)
                  Ec_fic = -TConc_lim/(ficR*eps_clim)
c                 Tensão linear na descida
                  TConc = TConc_lim + Ec_fic*(eps_c - eps_clim)
              
              ELSEIF(eps_c .GE. (1.d0+ficR)*eps_clim) THEN
c                 Ou se está esmagado já
                  TConc = 0.d0
                  esmagado = 1.d0
              END IF
          ELSEIF(ficR .EQ. -1.d0) THEN
c             Conforme Schmitz - Após eps_clim anula
              IF((DABS(eps_c) .GE. DABS(eps_clim)) .OR. 
     &          ((DABS(eps_cp) .GT. 0.d0) .AND. (TConc .LE. 0.d0))) THEN
                  TConc = 0.d0
                  esmagado = 1.d0
              END IF
          ELSE
c          ELSEIF(ficR .EQ. -2.d0) THEN
c             Conforme Lazzari (?) - Curva tens-def até anular
              IF((DABS(eps_cp) .GT. 0.d0) .AND. (TConc .LE. 0.d0)) THEN
                  TConc = 0.d0
                  esmagado = 1.d0
              END IF
          END IF        

c         Restaura sinal de eps_clim para negativo
          eps_clim = -eps_clim


      END SUBROUTINE
c-------------------------------------------------------------------------





c-------------------------------------------------------------------------
c
c     Curva de compressão uniaxial do concreto conforme MC2010
c
      SUBROUTINE conc_Uniax_MC2010(eps_cp, TConc, nStatev, ustatev)
c         Regra de endurecimento do concreto uniaxial conforme MC2010.
      
c         Parametros:
c         eps_cp      (dp,  sc,  in)  - Deformação no concreto
c         TConc       (dp,  sc, out)  - Tensão equivalente obtida no concreto
          DOUBLE PRECISION eps_cp, TConc
      
c         Variáveis internas:
c         eps_c1      (dp,  sc)   - deformação na maxima tensão de compressão
c         Ec1         (dp,  sc)   - modulo Ec correspondente a eps1
c         eps_clim    (dp,  sc)   - deformação última de compressão
c         k           (dp,  sc)   - número plástico Eci/Ec1
c         fcm         (dp,  sc)   - Resistência a compressão do concreto
c         Eci         (dp,  sc)   - Modulo de elasticidade Eci do concreto
c         nd          (dp,  sc)   - Não usado
c
          DOUBLE PRECISION eps_c1, Ec1, eps_clim, k, fcm, Eci, nd, fct
c
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
c         
c         Importa as propriedades do concreto
          CALL est_propConc(fcm, fct, Eci, nd, nStatev, ustatev)
          CALL est_propConc_UnMC2010(Ec1, eps_clim, k, eps_c1, 
     &                               nStatev, ustatev)

          !IF(TConc .LT. -fcm) TConc = -fcm
          !IF(TConc .GT.  fct) TConc = 0.01*fct
          
          IF(eps_cp .GT. 0.d0) THEN
c             Ponto está tracionado, nem deveria ser necessário verificar isso!
              !TConc = Eci*eps_cp
              IF(TConc .GT. fct) TConc = 0.01d0*fct
              
          ELSEIF(eps_cp .GE. eps_clim) THEN
c             Tensão uniaxial para deformação limite
              eps_c1 = -eps_c1
              TConc = (k*eps_cp/eps_c1 - (eps_cp/eps_c1)**2.d0)
              TConc = TConc/(1.d0 + (k-2.d0)*eps_cp/eps_c1)
              TConc = -fcm*TConc
          ELSE
c             Nem um outro?
              TConc = -0.01d0*fcm
          END IF

      END SUBROUTINE
c-------------------------------------------------------------------------




c-------------------------------------------------------------------------
c
c     Tensão efetiva pelo critério de plastificação de Ottosen
c
      SUBROUTINE conc_TEf_Ottosen(Invars, TEf, nStatev, ustatev)
c         Tensão efetiva pelo critério de Ottosen 
c     
c         Parametros:
c         Invars          (dp, 4x1,  in)  - Resultado da rotina EPT_Invars -> I1, J2, J3, theta
c         TEf             (dp,  sc, out)  - Tensão efetiva pela superfície
          DOUBLE PRECISION Invars(4), TEf
c
c         Variáveis internas:
c         Pi              (dp, sc)    - Precisa mesmo explicar isso?
c         C1, C2, lambda  (dp, sc)    - Variáveis da sup. dependentes das tensões
c         c3theta         (dp, sc)    - Cosseno de 3theta
          DOUBLE PRECISION  Pi, C1, C2, lambda, c3theta
          
c         Bagunça para armazenar localmente vars dos vetores
c         I1, J2, J3       (dp, sc)  - Armazena invariantes nas var locais
c         alphaOtt, betaOtt, lambdaCOtt, lambdaTOtt (dp, sc) - Props de Ottosen
          DOUBLE PRECISION alphaOtt, betaOtt, lambdaCOtt, 
     &                     lambdaTOtt, I1, J2, J3, theta

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
c         Constantes locais
          Pi = 4.d0*DATAN(1.d0)        
          
c         Copia Invariantes para variáveis locais para facilitar entendimento
          I1  = Invars(1)
          J2  = Invars(2)
          J3  = Invars(3)
          theta = Invars(4)
          c3theta = DCOS(3*theta)
          
c         Importa props de Ottossen
          CALL est_parOttosen(alphaOtt, betaOtt, lambdaCOtt, lambdaTOtt,
     &                        nStatev, ustatev)
          
c         Determina C1 e C2
          IF(lambdaCOtt/lambdaTOtt .LE. 0.5d0) THEN
              C1 = (2.d0*DCOS(theta)-1.d0)*lambdaTOtt
              C1 = C1 + 4.d0*lambdaCOtt*(1.d0-DCOS(theta))
              C2 = 1.d0
          ELSE
              C2 = 2.d0*lambdaCOtt/lambdaTOtt - 1.d0
              C2 = 3.d0*DATAN(C2/DSQRT(3.d0))
              C1 = lambdaCOtt/DCOS(Pi/3.d0-1.d0/3.d0*C2)
              C2 = DCOS(C2)
          END IF
          
c         Determina lambda
          lambda = C1*DCOS(DACOS(C2*c3theta)/3.d0)
          
c         Tensão efetiva
          TEf = lambda*DSQRT(J2)+betaOtt*I1
          TEf = TEf**2.d0 + 4.d0*alphaOtt*J2
          TEf = DSQRT(TEf) + lambda*DSQRT(J2) + betaOtt*I1
          TEf = TEf/2.d0
          
      END SUBROUTINE
c-------------------------------------------------------------------------


c-------------------------------------------------------------------------
c
c     Tensão efetiva pelo critério de plastificação de Willam-Warnke (PURO)
c
      SUBROUTINE conc_TEf_WW(Invars, TensPrinc, TEf, nStatev, ustatev)
c         Tensão efetiva pelo critério de Wilaam-Warnke 
c     
c         Parametros:
c         Invars          (dp, 4x1,  in)  - Resultado da rotina EPT_Invars -> I1, J2, J3, theta
c         TensPrinc       (dp, 2x1,  in)  - Tensões principais - EPT
c         TEf             (dp,  sc, out)  - Tensão efetiva pela superfície
          DOUBLE PRECISION Invars(4), TensPrinc(2), TEf
c
c         Variáveis internas:
c         fcm                     (dp, sc)    - Propriedades do concreto
c         qsi                     (dp, sc)    - qsi diferentão
c         rho, theta              (dp, sc)    - Variáveis em Haigh-Westergard
c         rhoc, rhot              (dp, sc)    - Meridianos de Compressão e Tração
c         a0, a1, a2              (dp, sc)    - Coeficientes da Superfície p/ Tração
c         b0, b1, b2              (dp, sc)    - Coeficientes da Superfície p/ Compressão
c         delt                    (dp, sc)    - Delta de Bhaskara
c         rhot2, rhoc2            (dp, sc)    - Quadrado de rhot e rhoc
c         cost, cost2             (dp, sc)    - Cosseno e Cosseno**2 de theta
c         rhosup                  (dp, sc)    - rho pela superfície
c         nd                      (dp, sc)    - não usado
c         s,t,u,v                 (dp, sc)    - pedaços de rho    
c     
          DOUBLE PRECISION qsi, rho, theta, rhoc, rhot, a0, a1, a2,
     &                     b0, b1, b2, fcm, delt, rhot2, rhoc2,
     &                     cost, cost2, rhosup, nd, s, t, u, v

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
      
          
c         Solicita parametros da superfície (a's, b's) precisa de TPrinc(1)
c             para saber se ponto está tracionado em alguma direção
          CALL est_parWW(TensPrinc(1), a0, a1, a2, b0, b1, b2, 
     &                   nStatev, ustatev)
          
          CALL est_propConc(fcm, nd, nd, nd, nStatev, ustatev)
          
c         Calcula variáveis em Haigh-Westergard
          qsi = Invars(1)/DSQRT(3.d0)
          !qsi = Invars(1)/3.d0
          rho = DSQRT(2.d0*Invars(2))
          theta = Invars(4) ! Pra theta é apresentada uma outra formulação por Pasa, mas deve ser mesma coisa
          
c         Determina rhot por bhaskara
          delt = a1**2.d0 - 4.d0*a2*(a0 - qsi/fcm)
          !IF(delt .LE. 0.9d0*a1**2.d0) CALL DBG_erro(3)
          IF(delt .LT. 0.d0) THEN
              rhot = 0.d0
          ELSE
              rhot = -0.5d0/a2*(a1+DSQRT(delt))
              rhot = rhot
          END IF
          
c         Determina rhoc por bhaskara
          delt = b1**2.d0 - 4.d0*b2*(b0 - qsi/fcm)
          !IF(delt .LE. 0.9d0*b1**2.d0) CALL DBG_erro(3)
          IF(delt .LT. 0.d0) THEN
              rhoc = 0.d0
          ELSE
              rhoc = -0.5d0/b2*(b1+DSQRT(delt))
          END IF
          
c         Determina pedaços da função rhosup
          rhot2 = rhot**2.d0
          rhoc2 = rhoc**2.d0
          cost  = DCOS(theta)
          cost2 = cost**2.d0
          
          s = 2.d0*rhoc*(rhoc2-rhot2)*cost
          u = 4.d0*(rhoc2-rhot2)*cost2 + 5.d0*rhot2 - 4.d0*rhot*rhoc
          t = rhoc*(2.d0*rhot-rhoc)*DSQRT(u)
          v = 4.d0*(rhoc2-rhot2)*cost2 + (rhoc - 2.d0*rhot)**2.d0
          
          rhosup = (s+t)/v
          
          nd = DSQRT(4.d0*(rhoc2-rhot2)*cost2 + 5.d0*rhot2 
     &                - 4.d0*rhot*rhoc)
          nd = 2.d0*rhoc*(rhoc2-rhot2)*cost + 
     &                rhoc*(2.d0*rhot-rhoc)*nd
          nd = nd/(4.d0*(rhoc2-rhot2)*cost2 + (rhoc-2.d0*rhot)**2.d0)
          
c         Como os valores de rho foram normalizados por fcm agora precisa ~desnormalizar~
          rhosup = rhosup*fcm
          
c         A tensão efetiva é obtida por fcm*(rho_atuante/rho_sup)**3
          TEf = fcm*(rho/rhosup)**3.d0
          
      END SUBROUTINE
c-------------------------------------------------------------------------


          
c-------------------------------------------------------------------------
c
c     Tensão efetiva pelo critério de plastificação de Willam-Warnke c/ FIBRAS (Seow 2005) 
c
      SUBROUTINE conc_TEf_WWf(Invars, TensPrinc, TEf, nStatev, ustatev)
c         Tensão efetiva pelo critério de Wilaam-Warnke 
c     
c         Parametros:
c         Invars          (dp, 4x1,  in)  - Resultado da rotina EPT_Invars -> I1, J2, J3, theta
c         TensPrinc       (dp, 2x1,  in)  - Tensões principais - EPT
c         TEf             (dp,  sc, out)  - Tensão efetiva pela superfície
          DOUBLE PRECISION Invars(4), TensPrinc(2), TEf
c
c         Variáveis internas:
c         fcm                     (dp, sc)    - Propriedades do concreto
c         qsi                     (dp, sc)    - qsi diferentão
c         rho, theta              (dp, sc)    - Variáveis em Haigh-Westergard
c         rhoc, rhot              (dp, sc)    - Meridianos de Compressão e Tração
c         a0, a1, a2, k           (dp, sc)    - Coeficientes da Superfície p/ Tração
c         b0, b1, b2              (dp, sc)    - Coeficientes da Superfície p/ Compressão
c         delt                    (dp, sc)    - Delta de Bhaskara
c         rhot2, rhoc2            (dp, sc)    - Quadrado de rhot e rhoc
c         cost, cost2             (dp, sc)    - Cosseno e Cosseno**2 de theta
c         rhosup                  (dp, sc)    - rho pela superfície
c         nd                      (dp, sc)    - não usado
c         s,t,u,v                 (dp, sc)    - pedaços de rho    
c     
          DOUBLE PRECISION qsi, rho, theta, rhoc, rhot, a0, a1, a2, k,
     &                     b0, b1, b2, fcm, delt, rhot2, rhoc2,
     &                     cost, cost2, rhosup, nd, s, t, u, v

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
      
          
c         Solicita parametros da superfície (a's, b's) precisa de TPrinc(1)
c             para saber se ponto está tracionado em alguma direção
          CALL est_parWWf(TensPrinc(1), a0, a1, a2, b0, b1, b2, k,
     &                   nStatev, ustatev)
          CALL est_propConc(fcm, nd, nd, nd, nStatev, ustatev)
          
c         Calcula variáveis em Haigh-Westergard
          qsi = Invars(1)/DSQRT(3.d0)
          rho = DSQRT(2.d0*Invars(2))
          theta = Invars(4) ! Pra theta é apresentada uma outra formulação por Pasa, mas deve ser mesma coisa
          
c         Determina rhot por bhaskara
          delt = a1**2.d0 - 4.d0*a2*(a0 - qsi/fcm)
          
c         Essa verificação garante que os meridianos não sejam negativos 
c         quando o passo de tensão é muito grande e a solução diverge.
          !IF(delt .LE. 0.9d0*a1**2.d0) CALL DBG_erro(3)
          
          IF(delt .LT. 0.d0) THEN
              rhot = 0.d0
          ELSE
              rhot = -0.5d0/a2*(a1+DSQRT(delt))
              rhot = rhot/k
          END IF
          
c         Determina rhoc por bhaskara
          delt = b1**2.d0 - 4.d0*b2*(b0 - qsi/fcm)

c         Essa verificação garante que os meridianos não sejam negativos 
c         quando o passo de tensão é muito grande e a solução diverge.
          !IF(delt .LE. 0.9d0*b1**2.d0) CALL DBG_erro(3)
          
          IF(delt .LT. 0.d0) THEN
              rhoc = 0.d0
          ELSE
              rhoc = -0.5d0/b2*(b1+DSQRT(delt))
          END IF
          
c         Determina pedaços da função rhosup
          rhot2 = rhot**2.d0
          rhoc2 = rhoc**2.d0
          cost  = DCOS(theta)
          cost2 = cost**2.d0
          
          s = 2.d0*rhoc*(rhoc2-rhot2)*cost
          u = 4.d0*(rhoc2-rhot2)*cost2 + 5.d0*rhot2 - 4.d0*rhot*rhoc
          t = rhoc*(2.d0*rhot-rhoc)*DSQRT(u)
          v = 4.d0*(rhoc2-rhot2)*cost2 + (rhoc - 2.d0*rhot)**2.d0
          
          rhosup = (s+t)/v
          
          nd = DSQRT(4.d0*(rhoc2-rhot2)*cost2 + 5.d0*rhot2 
     &                - 4.d0*rhot*rhoc)
          nd = 2.d0*rhoc*(rhoc2-rhot2)*cost + 
     &                rhoc*(2.d0*rhot-rhoc)*nd
          nd = nd/(4.d0*(rhoc2-rhot2)*cost2 + (rhoc-2.d0*rhot)**2.d0)
          
c         Como os valores de rho foram normalizados por fcm agora precisa ~desnormalizar~
          rhosup = rhosup*fcm
          
c         A tensão efetiva é obtida por fcm*(rho_atuante/rho_sup)**3
          TEf = fcm*(rho/rhosup)**3.d0
          
      END SUBROUTINE
c-------------------------------------------------------------------------



c-------------------------------------------------------------------------
c
c     Sobrotina para determinar o valor de TfConc (f de plasticidade)
c         através de um lambda fornecido
c
      SUBROUTINE conc_TfConc_EPT(lambda, TfConc, Stress, dStrain, 
     &                dsdeEl, TStress, dStrainPl, TepsPlEq, 
     &                epsEq, esmagado, nStatev, ustatev)
c         Parametros:
c         lambda  (dp,  sc,  in)  - Multiplicador lambda a ser testado
c         TfConc  (dp,  sc, out)  - Valor de TfConc obtido 
c         Stress  (dp, 3x1,  in)  - Vetor de tensões antes do incremento
c         dStrain (dp, 3x1,  in)  - Incremento total de deformação
c         dsdeEl  (dp, 3x3,  in)  - Matriz constitutiva EPT
c         epsEq   (dp,  sc,  in)  - Variável epsEq do ANSYS - Deformação plastica equivalente acumulada
c         TStress (dp, 3x1, out)  - TrialStress = Stress+dStress
c         TepsPlEq(dp,  sc, out)  - Trial Def. Plast. Eq. no concreto       
c         dStrainPl(dp, 3x1,out)  - Vetor de incremento de deformações plásticas
c         esmagado(dp,  sc, out)  - Critério de esmagamento do ponto
      
          DOUBLE PRECISION lambda, TfConc, Stress(3), dStrain(3),
     &                     dsdeEl(3,3), TStress(3), dStrainPl(3), 
     &                     TepsPlEq, esmagado, epsEq
c     
c         Variáveis internas
c         fluxPlast   (dp, 3x1)   - Vetor de fluxo plástico
c         dStrainEl   (dp, 3x1)   - Parte elástica do incremento de deformação
c         dStress     (dp, 3x1)   - Incremento de tensão na tentativa atual
c         Invars      (dp, 4x1)   - Invariantes de Tensão
c         TensPrinc   (dp, 2x1)   - Tensões principais EPT
c         TPtheta     (dp,  sc)   - Angulo das tensões principais
c         TEfConc     (dp,  sc)   - Tensão efeitva no concreto (YIELD)
c         TUnxConc    (dp,  sc)   - Tensão equivalnete uniaxial no concret (PREYS)
          
          DOUBLE PRECISION fluxPlast(3), dStrainEl(3), est_modPlast,
     &                     dStress(3), Invars(4), TPtheta, modPlast,
     &                     TEfConc, TUnxConc, TensPrinc(2)
          

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
c         Importa subrotinas do ANSYS
          EXTERNAL vamb, maxv, vapb, vmult1, vmove, vzero, vdot
          
c         O concreto não deve começar esmagado!
          esmagado = 0.d0

c         Aplica incremento como 100% elástico
          CALL vzero(dStrainPl, 3)
          CALL maxv(dsdeEl,  dStrain, dStress, 3, 3)
          CALL vapb(Stress,  dStress, TStress, 3)       
                   
          !Incremento de deformação plástico não pode ser negativo!
          !*ATENÇÃO:* O fato de lambda ser editado aqui pode bugar a solução externamente
          IF(lambda .LT. 0.0d0) lambda = 0.0d0
          ! Fim do trecho de *ATENÇÃO*
          
c         Define que Trial Def. Plast. Eq. no concreto é a já acumulada
          TepsPlEq = epsEq
          
c         Verifica se lambda > 0, caso contrário não tem fluxo plástico!
          IF(lambda .GT. 0.d0) THEN
c             Determina direção do fluxo plastico 
              CALL EPT_Flux_VM(TStress, fluxPlast) 
          
c             Determina incrementos de deformação plástica 
              CALL vmove(fluxPlast, dStrainPl, 3)
              CALL vmult1(dStrainPl, 3, lambda)
              TepsPlEq = TepsPlEq + lambda

c             Determina parte elástica do incremento de def e tensão elástica gerada
              CALL vamb(dStrain, dStrainPl, dStrainEL, 3)
              CALL maxv(dsdeEl,  dStrainEL, dStress,   3, 3)
              CALL vapb(Stress,  dStress,   TStress,   3)
          END IF
              
c         Pelo preditor determina Invariantes de tensão, tensão efetiva e
c         tensão equivalente uniaxial do concreto
          CALL EPT_Invars(TStress, Invars)
          CALL EPT_TensPrinc(TStress, TensPrinc, TPtheta)
          
c         Determina modelo a ser adotado: superfície e endurecimento
          modPlast = est_modPlast(nStatev, ustatev)
          
          IF(modPlast .EQ. 1.d0) THEN
c             Modelo de Ottosen
              CALL conc_TEf_Ottosen(Invars,  TEfConc,  nStatev, ustatev)
              CALL conc_End_MC2010(TepsPlEq, TUnxConc, esmagado, 
     &                         nStatev, ustatev) 
              
          ELSEIF(modPlast .EQ. 2.d0) THEN
c             Modelo de Willam-Warnke
              CALL conc_TEf_WW(Invars, TensPrinc, TEfConc, 
     &                         nStatev, ustatev)
              CALL conc_End_MC2010(TepsPlEq, TUnxConc, esmagado, 
     &                         nStatev, ustatev) 
              
          ELSEIF(modPlast .EQ. 3.d0) THEN
c             Modelo de Willam-Warnke com Fibras (Seow 2005)
              CALL conc_TEf_WWf(Invars, TensPrinc, TEfConc, 
     &                         nStatev, ustatev)
              CALL conc_End_MC2010(TepsPlEq, TUnxConc, esmagado, 
     &                         nStatev, ustatev) 
          END IF


c         Determina se houve plastificação
          TfConc = TEfConc - TUnxConc          
          
      END SUBROUTINE      
c-------------------------------------------------------------------------

          
c-------------------------------------------------------------------------
c
c     Sobrotina para aplicar TensionStiffening pelo modelo de Martinelli
c
      SUBROUTINE conc_Trac_Martinelli(nfis, Strain, dStrain, TStress, 
     &                Etan, nStatev, ustatev)
c         Parametros:
c         nfis    (int, sc,  in)  - Fissura (1 ou 2) - Primeira ou segunda direção?
c         Strain  (dp,  sc,  in)  - Deformação na direção da fissura
c         dStrain (dp,  sc,  in)  - Incremento de deformação na direção da fissura
c         TStress (dp,  sc,  io)  - Tensão atuante na fissura
c         Etan    (dp,  sc, out)  - Modulo de elasticidade tangente p/ usar em dsdePl      
          DOUBLE PRECISION Strain, dStrain, TStress, Etan
          INTEGER nfis
c     
c         Variáveis internas
c         eps_tot     (dp,  sc)   - Deformação total (Strain+dStrain)
c         eps_ctu     (dp,  sc)   - Deformação ultima do concreto tracionado
c         alphac      (dp,  sc)   - Coeficiente alfa aplicado sobre fct
c         fct, Eci    (dp,  sc)   - Propriedades do Concreto
c         stress_ult  (dp,  sc)   - Última tensão observada na fissura
c         strain_ult  (dp,  sc)   - Última deformação observada na fissura
c         martiref    (dp,  sc)   - Tensão após fissuração por essa curva (nome besta né?)
c         propsTS     (dp, 2x1)   - Vetor de leitura das props de TS
c         nd          (dp,  sc)   - não usado
          DOUBLE PRECISION eps_ctu, fct, Eci, alphac, eps_tot,
     &                     nd, strain_ult, stress_ult, propsTS(2), 
     &                     martiref, atana, Etan2
          

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
c         Importa propriedades do concreto e do TS
          CALL est_propConc(nd, fct, Eci, nd, nStatev, ustatev)
          CALL est_propTS(propsTS, 2, nStatev, ustatev)
          alphac   = propsTS(1)
          eps_ctu  = propsTS(2)
c         Importa estado da fissuração
          CALL est_Fiss_DT(0, nfis, stress_ult, strain_ult, nStatev, 
     &                            ustatev)
          
c         Determina deformação total
          eps_tot = Strain + dStrain 
          
c         Verificação interessante: Quando o elemento fissura eu digo que a tensão
c         atuante nele é fct, no caso desse modelo ela é limitada a
c         alphac*fct*(1-fct/Eci/eps_ctu) e então precisa descer até
c         a curva de Martinelli.
          martiref = alphac*fct*(1.d0 - fct/Eci/eps_ctu)
          
c         Verifica limites da curva
          IF(strain_ult .GE. eps_ctu) THEN
c             Rompeu anteriormente
              TStress = 0.0d0
              Etan = alphac*fct/eps_ctu
          ELSEIF(eps_tot .GE. eps_ctu) THEN
c             Fissura rompeu por tração - abriu demais
              TStress = 0.0d0
              Etan = alphac*fct/eps_ctu
              CALL est_Fiss_DT(1, nfis, 0.d0, eps_tot, nStatev, ustatev)
          ELSEIF(eps_tot .LE. 0.d0) THEN
c             Fissura está comprimida!
              CALL conc_Uniax_MC2010(eps_tot, TStress, nStatev, ustatev)
              Etan = Eci
          ELSEIF(eps_tot .GE. strain_ult) THEN
c             Está fissurando mais, amolecimento
c             Duas curvas de amolecimento, usa sempre a maior
c             -Amolecimento original
              Etan = -alphac*fct/eps_ctu
c             -Amolecimento inicial extra
              Etan2 = -2.d0*Eci
              !!TStress = stress_ult + Etan*(eps_tot-strain_ult) !!alphac*fct*(1.d0 - eps_tot/eps_ctu)
              TStress = MAX(stress_ult + Etan2*(eps_tot-strain_ult),
     &                      alphac*fct*(1.d0 - eps_tot/eps_ctu))
              CALL est_Fiss_DT(1, nfis, TStress, eps_tot, nStatev, 
     &                            ustatev)
          ELSEIF(eps_tot .LT. strain_ult) THEN
c             Descarga ou recarga
              Etan = stress_ult/strain_ult
              TStress = TStress + Etan*dStrain 
          END IF

          
c         No fim alterar Etan é uma péssima ideia... o resultado
c         converge melhor com a matriz elástica!
          Etan = Eci

      END SUBROUTINE      
c-------------------------------------------------------------------------

          
c-------------------------------------------------------------------------
c
c     Sobrotina para aplicar ruptura "brusca" com xEci de queda
c
      SUBROUTINE conc_Trac_RBrusca(nfis, Strain, dStrain, TStress, 
     &                Etan, nStatev, ustatev)
c         Parametros:
c         nfis    (int, sc,  in)  - Fissura (1 ou 2) - Primeira ou segunda direção?
c         Strain  (dp,  sc,  in)  - Deformação na direção da fissura
c         dStrain (dp,  sc,  in)  - Incremento de deformação na direção da fissura
c         TStress (dp,  sc,  io)  - Tensão atuante na fissura
c         Etan    (dp,  sc, out)  - Modulo de elasticidade tangente p/ usar em dsdePl        
          DOUBLE PRECISION Strain, dStrain, TStress, Etan
          INTEGER nfis
c     
c         Variáveis internas
c         eps_tot     (dp,  sc)   - Deformação total (Strain+dStrain)
c         eps_ctu     (dp,  sc)   - Deformação ultima do concreto tracionado
c         fct, Eci    (dp,  sc)   - Propriedades do Concreto
c         stress_ult  (dp,  sc)   - Última tensão observada na fissura
c         strain_ult  (dp,  sc)   - Última deformação observada na fissura
c         nd          (dp,  sc)   - não usado
          DOUBLE PRECISION eps_ctu, fct, Eci, eps_tot,
     &                     nd, strain_ult, stress_ult
          

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
c         Importa propriedades do concreto
          CALL est_propConc(nd, fct, Eci, nd, nStatev, ustatev)
 
c         Importa estado da fissuração
          CALL est_Fiss_DT(0, nfis, stress_ult, strain_ult, nStatev, 
     &                            ustatev)
          
c         Determina deformação total
          eps_tot = Strain + dStrain 
          
c         Modulo de amolecimento
          Etan = -3.d0*Eci
          
c         Tensão última
          eps_ctu = fct/Eci + fct/-Etan
          
c         Verifica limites da curva
          IF(strain_ult .GE. eps_ctu) THEN
c             Rompeu anteriormente
              TStress = 0.0d0
          ELSEIF(eps_tot .GE. eps_ctu) THEN
c             Fissura rompeu por tração - abriu demais
              TStress = 0.0d0
              CALL est_Fiss_DT(1, nfis, 0.d0, eps_tot, nStatev, ustatev)
          ELSEIF(eps_tot .LE. 0.d0) THEN
c             Fissura está comprimida!
              CALL conc_Uniax_MC2010(eps_tot, TStress, nStatev, ustatev)
              Etan = Eci
          ELSEIF(eps_tot .GE. strain_ult) THEN
c             Está fissurando mais, amolecimento
              TStress = stress_ult + Etan*(eps_tot-strain_ult)
              CALL est_Fiss_DT(1, nfis, TStress, eps_tot, nStatev, 
     &                            ustatev)
          ELSEIF(eps_tot .LT. strain_ult) THEN
c             Descarga ou recarga
              Etan = stress_ult/strain_ult 
              TStress = TStress + Etan*dStrain 
          END IF
          
          
c         No fim alterar Etan é uma péssima ideia... o resultado
c         converge melhor com a matriz elástica!
          Etan = Eci

      END SUBROUTINE      
c-------------------------------------------------------------------------          
          
          
          
c-------------------------------------------------------------------------
c
c     Sobrotina para aplicar comportamento exponencial em tração do concreto 
c
      SUBROUTINE conc_Trac_Exp(nfis, Strain, dStrain, TStress, 
     &                Etan, nStatev, ustatev)
c         Parametros:
c         nfis    (int, sc,  in)  - Fissura (1 ou 2) - Primeira ou segunda direção?
c         Strain  (dp,  sc,  in)  - Deformação na direção da fissura
c         dStrain (dp,  sc,  in)  - Incremento de deformação na direção da fissura
c         TStress (dp,  sc,  io)  - Tensão atuante na fissura
c         Etan    (dp,  sc, out)  - Modulo de elasticidade tangente p/ usar em dsdePl      
          DOUBLE PRECISION Strain, dStrain, TStress, Etan
          INTEGER nfis
c     
c         Variáveis internas
c         eps_tot     (dp,  sc)   - Deformação total (Strain+dStrain)
c         eps_crack   (dp,  sc)   - Deformação de fissuração (fct/Eci)
c         fct         (dp,  sc)   - Tensão de fissuração do concreto
c         Eci         (dp,  sc)   - Modulo de elasticidade do concreteo
c         stress_ult  (dp,  sc)   - Última tensão observada na fissura
c         strain_ult  (dp,  sc)   - Última deformação observada na fissura
c         propsTS     (dp, 1x1)   - Vetor de leitura das props do TS
c         nd          (dp,  sc)   - não usado
c         constC      (dp,  sc)   - Fator do exponencial (padrão = 150/cm)
c         lc          (dp,  sc)   - Comprimento caracteristico do elemento
c         wcr         (dp,  sc)   - Tamanho total das fissuras somadas (lc*eps)
c         StLimInf    (dp,  sc)   - Tensão limite inferior (1%*fct)
c         eps_ctu     (dp,  sc)   - Deformação onde tensão atinge limite inf
          DOUBLE PRECISION fct, eps_tot, nd, strain_ult, stress_ult,  
     &                     propsTS(1), constC, lc, wcr, eps_crack,
     &                     StLimInf, eps_ctu, Eci

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
c         Importa propriedades do concreto
          CALL est_propConc(nd, fct, Eci, nd, nStatev, ustatev)
          CALL est_propTS(propsTS, 1, nStatev, ustatev)
          constC = propsTS(1) 
c         Importa estado da fissuração
          CALL est_Fiss_DT(0, nfis, stress_ult, strain_ult, nStatev, 
     &                            ustatev)
          
c         Determina comprimento característico do elemento
          CALL est_EPT_lc(lc, nStatev, ustatev)  
          
c         Determina deformação total
          eps_tot = Strain + dStrain
c         Determina deformação de fissuração
          eps_crack = fct/Eci
c         Determina tamanho de wcr
          wcr = lc*(eps_tot-eps_crack)
c         Determina tensao limite inferior e sua deformação
          StLimInf = 0.01d0*fct
          eps_ctu = (DLOG(StLimInf/fct)/-constC)/lc

c         Verifica limites da curva
          IF(MAX(strain_ult, eps_tot) .GE. eps_ctu) THEN
c             Rompeu agora ou anteriormente
              TStress = 0.d0
              CALL est_Fiss_DT(1, nfis, 0.d0, eps_tot, nStatev, ustatev)
          ELSEIF(eps_tot .LE. 0.d0) THEN
c             Fissura está comprimida!
              CALL conc_Uniax_MC2010(eps_tot, TStress, nStatev, ustatev)
              Etan = Eci
          ELSEIF(eps_tot .GE. strain_ult) THEN
c             Está fissurando mais: amolecimento
              TStress = fct*DEXP(-constC*wcr)
              CALL est_Fiss_DT(1, nfis, TStress, eps_tot, nStatev, 
     &                            ustatev)
          ELSEIF(eps_tot .LT. strain_ult) THEN
c             Descarga ou recarga
              Etan = stress_ult/strain_ult
              TStress = TStress + Etan*dStrain 
          END IF

          Etan = Eci

      END SUBROUTINE      
c-------------------------------------------------------------------------

          
c-------------------------------------------------------------------------
c
c     Sobrotina para aplicar modelo VEM ao concreto tracionado com fibras
c
      SUBROUTINE conc_Trac_VEM(nfis, Strain, dStrain, TStress, 
     &                Etan, nStatev, ustatev)
c         Parametros:
c         nfis    (int, sc,  in)  - Fissura (1 ou 2) - Primeira ou segunda direção?
c         Strain  (dp,  sc,  in)  - Deformação na direção da fissura
c         dStrain (dp,  sc,  in)  - Incremento de deformação na direção da fissura
c         TStress (dp,  sc,  io)  - Tensão atuante na fissura
c         Etan    (dp,  sc, out)  - Modulo de elasticidade tangente p/ usar em dsdePl      
          DOUBLE PRECISION Strain, dStrain, TStress, Etan
          INTEGER nfis
c     
c         Variáveis internas
c         eps_tot     (dp,  sc)   - Deformação total (Strain+dStrain)
c         eps_crack   (dp,  sc)   - Deformação de fissuração (fct/Eci)
c         fct         (dp,  sc)   - Tensão de fissuração do concreto
c         Eci         (dp,  sc)   - Modulo de elasticidade do concreteo
c         stress_ult  (dp,  sc)   - Última tensão observada na fissura
c         strain_ult  (dp,  sc)   - Última deformação observada na fissura
c         propsTrac   (dp, 1x1)   - Vetor de leitura das props de tração
c         nd          (dp,  sc)   - não usado
c         constC      (dp,  sc)   - Fator do exponencial (padrão = 150/cm)
c         lc          (dp,  sc)   - Comprimento caracteristico do elemento
c         wcr         (dp,  sc)   - Tamanho total das fissuras somadas (lc*eps)
c         StLimInf    (dp,  sc)   - Tensão limite inferior (1%*fct)
c         eps_ctu     (dp,  sc)   - Deformação onde tensão atinge limite inf
c         TStressC    (dp,  sc)   - Parcela de tensão resistida pelo concreto
c         TStressF    (dp,  sc)   - Parcela de tensão resistida pelas fibras
c         lf, df, Vf  (dp,  sc)   - Propriedades das fibras: comp, diam, Vol
c         tau_u       (dp,  sc)   - Tensão média de aderencia
          DOUBLE PRECISION fct, eps_tot, nd, strain_ult, stress_ult,  
     &                     propsTrac(1), constC, lc, wcr, eps_crack,
     &                     StLimInf, eps_ctu, Eci, TStressC, TStressF,
     &                     lf, df, Vf, tau_u, lfi

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
c         Importa propriedades do concreto
          CALL est_propConc(nd, fct, Eci, nd, nStatev, ustatev)
          CALL est_propTracf(propsTrac, 1, nStatev, ustatev)
          constC = propsTrac(1)
          
c         Propriedadades das fibras
          CALL est_propCFibr(nd, tau_u, Vf, lf, lfi, df, 
     &                         nStatev, ustatev)
          
c         Importa estado da fissuração
          CALL est_Fiss_DT(0, nfis, stress_ult, strain_ult, nStatev, 
     &                            ustatev)
          
c         Determina comprimento característico do elemento
          CALL est_EPT_lc(lc, nStatev, ustatev)  
          
c         Determina deformação total
          eps_tot = Strain + dStrain
c         Determina deformação de fissuração
          eps_crack = fct/Eci
c         Determina tamanho de wcr
          wcr = lc*(eps_tot-eps_crack)
c         Determina tensao limite inferior e sua deformação
          StLimInf = 0.01d0*fct
          eps_ctu = 1.d0 !(DLOG(StLimInf/fct)/-constC)/lc

c         Verifica limites da curva
          IF((eps_tot .GE. 3.0d0*eps_crack).AND.
     &                (TStress .LE. StLimInf)) THEN
c             Rompeu agora ou anteriormente
              TStress = 0.d0
              CALL est_Fiss_DT(1,nfis,TStress,eps_tot,nStatev,ustatev)
          ELSEIF(eps_tot .LE. 0.d0) THEN
c             Fissura está comprimida!
              CALL conc_Uniax_MC2010(eps_tot, TStress, nStatev, ustatev)
              Etan = Eci
          ELSEIF(eps_tot .GE. strain_ult) THEN
c             Está fissurando mais: 
              TStressC = fct*DEXP(-constC*wcr)
              TStressF = Vf*lf/df*tau_u*DATAN(wcr/df*3.5d0)
     &                    /3.1416d0*(1.d0-2.d0*wcr/lf)**2.d0
              TStress = TStressC + TStressF
                  
              CALL est_Fiss_DT(1, nfis, TStress, eps_tot, nStatev, 
     &                            ustatev)
          ELSEIF(eps_tot .LT. strain_ult) THEN
c             Descarga ou recarga
              Etan = stress_ult/strain_ult
              TStress = TStress + Etan*dStrain 
          END IF
          
          Etan = Eci

      END SUBROUTINE      
c-------------------------------------------------------------------------

     


          
c-------------------------------------------------------------------------
c
c     Sobrotina para aplicar modelo de Lim (usado pela Vanessa)
c
      SUBROUTINE conc_Trac_Lim(nfis, Strain, dStrain, TStress, 
     &                Etan, nStatev, ustatev)
c         Parametros:
c         nfis    (int, sc,  in)  - Fissura (1 ou 2) - Primeira ou segunda direção?
c         Strain  (dp,  sc,  in)  - Deformação na direção da fissura
c         dStrain (dp,  sc,  in)  - Incremento de deformação na direção da fissura
c         TStress (dp,  sc,  io)  - Tensão atuante na fissura
c         Etan    (dp,  sc, out)  - Modulo de elasticidade tangente p/ usar em dsdePl        
          DOUBLE PRECISION Strain, dStrain, TStress, Etan
          INTEGER nfis
c     
c         Variáveis internas
c         eps_tot     (dp,  sc)   - Deformação total (Strain+dStrain)
c         fct, Eci    (dp,  sc)   - Propriedades do Concreto
c         stress_ult  (dp,  sc)   - Última tensão observada na fissura
c         strain_ult  (dp,  sc)   - Última deformação observada na fissura
c         sig_tu      (dp,  sc)   - Tensão de confinamento gerada pelas fibras
c         lf, df, Vf  (dp,  sc)   - Propriedades das fibras: comp, diam, Vol
c         nd          (dp,  sc)   - não usado
          DOUBLE PRECISION eps_ctu, fct, Eci, eps_tot, Vf, lf, df,
     &                     nd, strain_ult, stress_ult, sig_tu
          

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
c         Importa propriedades do concreto
          CALL est_propConc(nd, fct, Eci, nd, nStatev, ustatev)
 
c         Importa estado da fissuração
          CALL est_Fiss_DT(0, nfis, stress_ult, strain_ult, nStatev, 
     &                            ustatev)
          
c         Propriedadades das fibras
          CALL est_propCFibr(sig_tu, nd, Vf, lf, nd, df, 
     &                         nStatev, ustatev)    
          
c         Determina deformação total
          eps_tot = Strain + dStrain 
          
c         Modulo de amolecimento
          Etan = -3.d0*Eci
          
c         Verifica limites da curva
          IF(eps_tot .LE. 0.d0) THEN
c             Fissura está comprimida!
              CALL conc_Uniax_MC2010(eps_tot, TStress, nStatev, ustatev)
              Etan = Eci
          ELSEIF(eps_tot .GE. strain_ult) THEN
c             Está fissurando mais, amolecimento
              TStress = stress_ult + Etan*(eps_tot-strain_ult)
              TStress = MAX(TStress, sig_tu)
              CALL est_Fiss_DT(1, nfis, TStress, eps_tot, nStatev, 
     &                            ustatev)
          ELSEIF(eps_tot .LT. strain_ult) THEN
c             Descarga ou recarga
              Etan = stress_ult/strain_ult 
              TStress = TStress + Etan*dStrain 
          END IF
          
          
c         No fim alterar Etan é uma péssima ideia... o resultado
c         converge melhor com a matriz elástica!
          Etan = Eci
      END SUBROUTINE      
c-------------------------------------------------------------------------


          
c-------------------------------------------------------------------------
c
c     Sobrotina para aplicar comportamento em tração do concreto com fibras pelo modelo SDEM (Lee, Cho e Vecchio; 2013)
c
      SUBROUTINE conc_Trac_SDEM(nfis, Strain, dStrain, TStress, 
     &                Etan, nStatev, ustatev)
c         Parametros:
c         nfis    (int, sc,  in)  - Fissura (1 ou 2) - Primeira ou segunda direção?
c         Strain  (dp,  sc,  in)  - Deformação na direção da fissura
c         dStrain (dp,  sc,  in)  - Incremento de deformação na direção da fissura
c         TStress (dp,  sc,  io)  - Tensão atuante na fissura
c         Etan    (dp,  sc, out)  - Modulo de elasticidade tangente p/ usar em dsdePl      
          DOUBLE PRECISION Strain, dStrain, TStress, Etan
          INTEGER nfis
c     
c         Variáveis internas
c         eps_tot     (dp,  sc)   - Deformação total (Strain+dStrain)
c         eps_ctu     (dp,  sc)   - Deformação ultima do concreto tracionado
c         fct, Eci    (dp,  sc)   - Propriedades do Concreto puro
c         fctf        (dp,  sc)   - Tensão ultima do concreto com fibras (estimativa grosseira)
c         Vf          (dp,  sc)   - Volumes de fibras (Crítico e Empregado)
c         stress_ult  (dp,  sc)   - Última tensão observada na fissura
c         strain_ult  (dp,  sc)   - Última deformação observada na fissura
c         sig_tu      (dp,  sc)   - Tensão de confinamento ou residual em tração (Vf<Vfc)
c         propsTrac   (dp, 1x1)   - Vetor de leitura das props de traçãi
c         nd          (dp,  sc)   - não usado
          DOUBLE PRECISION eps_ctu, fct, Eci, eps_tot, fctf,
     &                     nd, strain_ult, stress_ult, propsTrac(1), 
     &                     sig_tu, atana, Etan2, Vf, lc
          

c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
c         Importa propriedades do concreto
          CALL est_propConc(nd, fct, Eci, nd, nStatev, ustatev)
!          CALL est_propCFibr(sig_tu, tau_u, Vf, lf, lfi, df, 
!     &                         nStatev, ustatev)
          CALL est_propTracf(propsTrac, 1, nStatev, ustatev)
          eps_ctu = propsTrac(1) 
c         Importa estado da fissuração
          CALL est_Fiss_DT(0, nfis, stress_ult, strain_ult, nStatev, 
     &                            ustatev)
          
c         Determina comprimento característico do elemento
          CALL est_EPT_lc(lc, nStatev, ustatev)  



      END SUBROUTINE      
c-------------------------------------------------------------------------

          
          
c-------------------------------------------------------------------------
c
c     Sobrotina para aplicar comportamento em tração do concreto - TS ou fibras
c
      SUBROUTINE conc_Trac(nfis, Strain, dStrain, TStress, 
     &                Etan, nStatev, ustatev)
c         Parametros:
c         nfis    (int, sc,  in)  - Fissura (1 ou 2) - Primeira ou segunda direção?
c         Strain  (dp,  sc,  in)  - Deformação na direção da fissura
c         dStrain (dp,  sc,  in)  - Incremento de deformação na direção da fissura
c         TStress (dp,  sc,  io)  - Tensão atuante na fissura
c         Etan    (dp,  sc, out)  - Modulo de elasticidade tangente p/ usar em dsdePl     
c
c         Variáveis internas:
c         modTracf (dp,  sc)   - Modelo de Tração com fibras adotao
c         modTS    (dp,  sc)   - Modelo de TS
c         TStressTS(dp,  sc)   - TStress por TensionStiffening
c         TStressf (dp,  sc)   - TStress pelas fibras
          DOUBLE PRECISION Strain, dStrain, TStress, Etan, modTracf, 
     &                     est_modTracf, modTS, est_modTS, TStressf,
     &                     TStressTS, fct, nd, Eci
          INTEGER nfis
      
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
          ! Pega modelos
          modTracf = est_modTracf(nStatev, ustatev)
          modTS    = est_modTS(nStatev, ustatev)
          
          ! Salva duas copias de TStress
          TStressTS = TStress
          TStressf  = TStress
          
          ! Determina fct 
          CALL est_propConc(nd, fct, Eci, nd, nStatev, ustatev)
          
          ! Antes de aplicar o TS verifica se é aplicável (angulo e região),
          ! se estiver fora assume que TS=-1 e desativa.
          CALL conc_validaTS(nfis, modTS, nStatev, ustatev)
          
          ! Tension Stiffening
          IF(modTS .EQ. 1.d0) THEN
c             Modelo 1: Martinelli
              CALL conc_Trac_Martinelli(nfis, Strain, dStrain,TStressTS,
     &                Etan, nStatev, ustatev)
          ELSEIF(modTS .EQ. 2.d0) THEN
c             Modelo 2: Exponencial 
              CALL conc_Trac_Exp(nfis, Strain, dStrain, TStressTS, 
     &                Etan, nStatev, ustatev)
          ELSE
c             Modelo indefinido: ruptura "brusca" com queda =-3Eci
              CALL conc_Trac_RBrusca(nfis, Strain, dStrain, TStressTS, 
     &                Etan, nStatev, ustatev)
              !TStressTS = 0.0d0
              END IF

          ! Tração do concreto com fibras
          !   Se vf*lf*df for zero então não existe contribuição das fibras (-1)
          IF(modTracf .EQ. 1.d0) THEN
c             Modelo 1: VEM
              CALL conc_Trac_VEM(nfis, Strain, dStrain, TStressf, 
     &                Etan, nStatev, ustatev)
          ELSEIF(modTracf .EQ. 2.d0) THEN
c             Modelo 2: SDEM
              CALL conc_Trac_SDEM(nfis, Strain, dStrain, TStressf, 
     &                Etan, nStatev, ustatev)
          ELSEIF(modTracf .EQ. 3.d0) THEN
c             Modelo 3: Lim (acho horrivel)
              CALL conc_Trac_Lim(nfis, Strain, dStrain, TStressf, 
     &                Etan, nStatev, ustatev)
          ELSE
              TStressf = 0.0d0
          END IF

          ! Pega maior dos dois
          TStress = MAX(TStressTS, TStressf)
          ! Verifica limite inferior da tensão
          IF(((Strain+dStrain).GT.fct/Eci).AND.
     &       (TStress .LT. 0.01d0*fct)) TStress = 0.01d0*fct
          
      END SUBROUTINE
c-------------------------------------------------------------------------
          
          
c-------------------------------------------------------------------------
c     
c     Determina se ponto de integração atual apresenta TS
c         (é rotina de concreto pq não é criadora/resutaradora de ustatevs)
c
      SUBROUTINE conc_validaTS(nfis, modTS, nStatev, ustatev)
c         Parametros:
c         modTS       (dp,  sc,  io)  - modelo de TS aplicado
c         nfis        (int, sc,  in)  - Fissura (1 ou 2) - Primeira ou segunda(+90) direção? 
          DOUBLE PRECISION modTS
          INTEGER          nfis
          
c         Variáveis internas:
c         crds          (dp, 3x1) - Vetor de coordenadas iniciais do ponto
c         theta         (dp,  sc) - Angulo da fissura
c         angl          (dp,  sc) - Angulo limite
c         xi            (dp,  sc) - x inicial
c         xf            (dp,  sc) - x final
c         yi            (dp,  sc) - y inicial
c         yf            (dp,  sc) - y final
c         pi            (dp,  sc) - aquele número mágico
c         iA, iX, iY    (log, sc) - Controle dos intervalos (Angulo, X, Y)
          DOUBLE PRECISION crds(3), theta, angl, xi, xf, yi, yf, pi
          LOGICAL iA, iX, iY
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------

c         Calcula o número mágico
          pi = 4.d0*DATAN(1.d0)

c         Importa theta, coords0 e limites
          CALL est_ThetaEPT(0, theta, nStatev, ustatev)
          CALL est_coords0(0, crds, nStatev, ustatev) 
          CALL est_limitesTS(angl, xi, xf, yi, yf, nStatev, ustatev)
          
c         Se angulo é negativo obtem modulo do valor
          IF(angl .LT. 0.d0) angl = DABS(angl)
          
c         Se angulo limite maior que 90 limita a 90
          IF(angl .GT. 90.d0) angl = 90.d0
      
c         Lembrando que TS só vale se, ao mesmo tempo:
c               -angl <=  theta  <= +angl
c                  xi <= crds(1) <= xf
c                  yi <= crds(2) <= yf
c           e: para nfis=1 -> theta=theta
c              para nfis=2 -> theta=theta+90graus
c           sendo angl em relação à vertical (ver figura no excel de vars estado)
c           Se yi=yf=0 ou xi=xf=0 vale tudo nessa direção

c         Inicialmente define todos controles como falso
c             assim não precisa definir depois, só marcar as que passam
          iA = .FALSE.
          iX = .FALSE.
          iY = .FALSE.
          
c         Restrições por angulo 
          IF(angl == 0.d0) THEN
c             Sem restrição: está ok
              iA = .TRUE.
          ELSE
c             Com restrição:

c             Passa angulo limite para radianos
              angl = angl*pi/180.d0
              
c             Verifica se o angulo limite é menor que 0.01 graus, caso sim =90graus
              IF(angl .LE. 0.0001d0) angl = pi/2.d0  
              
c             Segunda fissura está defasada 90 graus da primeira
              IF(nfis .EQ. 2) theta = theta + pi/2.d0
              
c             Retorna sempre pros quadrantes I e IV
              IF(theta .GT. pi/2) theta = theta - pi          
          
c             Verifica se theta está no intervalo
              IF((-angl <= theta).AND.(theta <= angl)) iA = .TRUE.
          END IF

c         Restrições em X
          IF((xi == 0.d0).AND.(xf == 0.d0)) THEN
c             Não há restrição nessa direção
              iX = .TRUE.
          ELSE
c             Existe restrição, verificar intervalo
              IF((xi <= crds(1)).AND.(crds(1) <= xf)) iX = .TRUE.
          END IF
          
          
c         Restrições em Y
          IF((yi == 0.d0).AND.(yf == 0.d0)) THEN
c             Não há restrição nessa direção
              iY = .TRUE.
          ELSE
c             Existe restrição, verificar intervalo
              IF((yi <= crds(2)).AND.(crds(2) <= yf)) iY = .TRUE.
          END IF          
          
c         Verifica se alguma é falsa, então desliga TS
          IF(.NOT.((iA.AND.iX).AND.(iY))) modTS = -1.d0
          
      END SUBROUTINE
      
c-------------------------------------------------------------------------