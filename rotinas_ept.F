c*************************************************************************
c     
c     Subrotinas para estado plano de tensões, nomeadas EPT_xxx.
c     
c
c
c     Eduardo Pagnussat Titello, 2019
c
c*************************************************************************
      
c-------------------------------------------------------------------------
c
c     Tensões principais
c
      SUBROUTINE EPT_TensPrinc(stress, TensPrinc, theta)
c         Cálcula as tensões principais e o angulo formado por estas em EPT
c     
c         Parametros:
c         stress          (dp, 3x1,  in)  - Estado de tensões
c         TensPrinc       (dp, 2x1, out)  - Tensões principais (1,2)
c         theta           (dp,  sc, out)  - Angulo de rotação (rad)
          DOUBLE PRECISION stress(3), TensPrinc(2), theta
c         Variáveis internas:
c         P1 e P2         (dp, sc)    - Partes 1 e 2 da expressão para tensões principais
c         Pi              (dp, sc)    - Precisa mesmo?
c         cost, sent      (dp, sc)    - Cosseno e Seno de Theta
c         cost2, sent2    (dp, sc)    - Cosseno2 e Seno2 de Theta
          DOUBLE PRECISION P1, P2, Pi, cost, sent, cost2, sent2 
          Pi = 4.d0*DATAN(1.d0)
      
c         Partes que compõem as tensões principais
          P1 = (stress(1) + stress(2))/2.d0
          P2 = (stress(1) - stress(2))/2.d0
      
c         Angulo theta:
c         Condições de contorno para arctan
          IF(DABS(stress(3)) .LE. 10.d0**-4) THEN
              theta = 0.d0
              IF(P2 .LT. 0.d0) theta = Pi/2.d0
          ELSEIF(DABS(P2) .LE. 10.d0**-4) THEN
              theta = Pi/4.d0
              IF(stress(3) .LT. 0.d0) theta = -theta
          ELSE
              theta = stress(3)/P2
              theta = 0.5d0*DATAN(theta)
          END IF
          
c         Tensões máxima e mínima
          P2 = P2**2.d0 + stress(3)**2.d0
          P2 = DSQRT(P2)
          TensPrinc(1) = P1 + P2
          TensPrinc(2) = P1 - P2
          
c         Nova abordagem: usando angulo para determinar tensões principais
          cost = DCOS(theta)
          sent = DSIN(theta)
          cost2 = cost**2.d0
          sent2 = sent**2.d0 
          
c         Tensões principais iniciais
          TensPrinc(1) = stress(1)*cost2 + stress(2)*sent2 + 
     &                    2.d0*stress(3)*sent*cost
          TensPrinc(2) = stress(1)*sent2 + stress(2)*cost2 - 
     &                    2.d0*stress(3)*sent*cost
          
c         Teste se angulo não está errado, vai girando até acertar
          DO WHILE(TensPrinc(1) .LT. TensPrinc(2))
              theta = theta + Pi/2.d0
              
              cost = DCOS(theta)
              sent = DSIN(theta)
              cost2 = cost**2.d0
              sent2 = sent**2.d0 
              
              TensPrinc(1) = stress(1)*cost2 + stress(2)*sent2 + 
     &                        2.d0*stress(3)*sent*cost
              TensPrinc(2) = stress(1)*sent2 + stress(2)*cost2 - 
     &                        2.d0*stress(3)*sent*cost
          END DO
          
      END SUBROUTINE
c-------------------------------------------------------------------------


c-------------------------------------------------------------------------
c
c     "Invariantes de tensão" -> I1, J2, J3, theta
c
      SUBROUTINE EPT_Invars(stress, Invars)
c         Alguns invariantes e semelhantes úteis
c     
c         Parametros:
c         stress          (dp, 3x1,  in)  - Estado de tensões
c         Invars          (dp, 4x1, out)  - Alguns invariantes (uteis) + ang.theta
c             Sendo #1=I1, #2=J2, #3=J3, #4=theta
          DOUBLE PRECISION stress(3), Invars(4), p
      
c         Primeiro invariante de tensões principais I1
          Invars(1) = stress(1) + stress(2)
      
c         Segundo invariante de tensoes desviadoras J2
          Invars(2) = (stress(1) -  stress(2))**2.d0
          Invars(2) = Invars(2) + ( stress(2))**2.d0
          Invars(2) = Invars(2) + (-stress(1))**2.d0
          Invars(2) = 1.d0/6.d0*Invars(2)
          Invars(2) = Invars(2) + (stress(3))**2.d0
      
c         Terceiro Invariante de Tensões desviadoras J3
          p = Invars(1)/3.d0
          Invars(3) = (stress(1)-p)*(stress(2)-p)*(-p)
          Invars(3) = Invars(3) + p*(stress(3))**2.d0
      
c         Não é invariante, mas é útil
c         cos(3theta)
          IF(Invars(2) .NE. 0.d0) THEN
              Invars(4) = 1.5d0*DSQRT(3.d0)*Invars(3)/(Invars(2)**1.5d0)
              IF(Invars(4) .LT. -1.d0) Invars(4) = -1.d0
              IF(Invars(4) .GT.  1.d0) Invars(4) =  1.d0
c             theta = 1/3*acos(cos(3theta))
              Invars(4) = 1.d0/3.d0*DACOS(Invars(4))
          ELSE
              Invars(4) = 0.d0
          END IF
          
      END SUBROUTINE
c-------------------------------------------------------------------------




c-------------------------------------------------------------------------
c
c     Vetor fluxo plástico de Von-Mises
c
      SUBROUTINE EPT_Flux_VM(stress, flux)
c         Parametros:
c         stress      (dp, 3x1,  in)  - Vetor com Tensões
c         flux        (dp, 3x1, out)  - Vetor de fluxo
          DOUBLE PRECISION stress(3), flux(3)
c         Variáveis internas
c         Sxx, Syy, Sxy  (dp, sc) - Tensões em X, Y e XY
c         Const (dp, sc) - Constante que pré multiplica a matriz conforme Schmitz
          DOUBLE PRECISION Sxx, Syy, Sxy, Const

          Sxx = stress(1)
          Syy = stress(2)
          Sxy = stress(3)
          
c         Cálculo da constante
          Const = (Sxx**2.d0 + Syy**2.d0) - (Sxx*Syy)
          Const = Const + 3.d0*(Sxy**2.d0)
          
          flux    = 0.d0
          
          IF(Const .GT. 0.d0) THEN
              Const = 1.d0/(2.d0*DSQRT(Const))
              flux(1) = Const*(2.d0*Sxx - Syy)
              flux(2) = Const*(2.d0*Syy - Sxx)
              flux(3) = Const*(6.d0*Sxy)
          END IF

      END SUBROUTINE      
c-------------------------------------------------------------------------


c-------------------------------------------------------------------------
c
c     Matriz constitutiva elástica p/ EPT
c
      SUBROUTINE EPT_MatrizD(E, poisson, D)
c         Parametros:
c         E       (dp,  sc,  in)  - Modulo de elasticidade
c         poisson (dp,  sc,  in)  - Coeficiente de Poisson
c         D       (dp, 3x3, out)  - Matriz constitutiva D
          DOUBLE PRECISION E, poisson, D(3,3)
c     
c         Variáveis internas
c         C1, C2, C3  (dp, sc)    - Coeficientes de construção de D
          DOUBLE PRECISION C1, C2, C3
         
c         Cálcula coeficientes
          C1 = E/(1.d0 - poisson**2.d0)
          C2 = C1*poisson
          C3 = C1*(1.d0 - poisson)/2.d0
          
c         Monta matriz
          D      = 0.d0
          D(1,1) = C1
          D(2,2) = C1
          D(1,2) = C2
          D(2,1) = C2
          D(3,3) = C3
          
      END SUBROUTINE      
c-------------------------------------------------------------------------

      
c-------------------------------------------------------------------------
c
c     Rotaciona tensões em EPT
c
      SUBROUTINE EPT_Rot_Tens(Stress, theta)
c         Parametros:
c         Stress  (dp, 3x1,  io)  - Vetor de tensões a ser rotacionado e retorno
c         theta   (dp,  sc,  in)  - Angulo de rotação
          DOUBLE PRECISION Stress(3), theta
c     
c         Variáveis internas
c         
c         cost, sent  (dp, sc)    - Seno e Cosseno de theta
c         cos2t, sen2t(dp, sc)    - Seno**2 e Cosseno**2 de theta
c         StressRot   (dp 3x1)    - Vetor de tensões rotacionadas
          DOUBLE PRECISION cost, sent, StressRot(3), cos2t, sen2t
         
c         Cálcula coeficientes
          cost = DCOS(theta)
          sent = DSIN(theta)
          cos2t = cost**2.d0
          sen2t = sent**2.d0
          
c         Monta vetor rotacionado
          StressRot(1) = Stress(1)*cos2t + Stress(2)*sen2t +
     &                    2.d0*Stress(3)*cost*sent
          StressRot(2) = Stress(1)*sen2t + Stress(2)*cos2t -
     &                    2.d0*Stress(3)*cost*sent
          StressRot(3) = (Stress(2) - Stress(1))*cost*sent +
     &                    Stress(3)*(cos2t - sen2t)
          
c         Passa vetor rotacionado para entrada/saida
          Stress(1) = StressRot(1)
          Stress(2) = StressRot(2)
          Stress(3) = StressRot(3)
          
      END SUBROUTINE      
c-------------------------------------------------------------------------


          

c-------------------------------------------------------------------------
c
c     Rotaciona deformações em EPT
c
      SUBROUTINE EPT_Rot_Def(Strain, theta)
c         Parametros:
c         Strain  (dp, 3x1,  io)  - Vetor de deformações a ser rotacionado e retorno
c         theta   (dp,  sc,  in)  - Angulo de rotação
          DOUBLE PRECISION Strain(3), theta
   
c        Altera Strain(3) e usa rotina de tensões ;D
         Strain(3) = Strain(3)/2.d0
         CALL EPT_Rot_Tens(Strain, theta)
         Strain(3) = Strain(3)*2.d0

      END SUBROUTINE      
c-------------------------------------------------------------------------

          
      
      
c-------------------------------------------------------------------------
c
c     Rotaciona matriz constitutiva EPT
c
      SUBROUTINE EPT_Rot_Const(dsdePl,  theta)
c         Parametros:
c         dsdePl  (dp, 3x3,  io)  - Matriz constitutiva EPT
c         theta   (dp,  sc,  in)  - Angulo de rotação
          DOUBLE PRECISION dsdePl(3,3), theta
c     
c         Variáveis internas
c         cost, sent  (dp, sc)    - Seno e Cosseno de theta
c         mrot        (dp 3x3)    - Matriz de rotação
          DOUBLE PRECISION cost, sent, mrot(3,3), dsdePlRot(3,3)
          
c         Cálcula coeficientes e monta matriz de rotação
          cost = DCOS(theta)
          sent = DSIN(theta)
          
c         Monta matriz rotacionada
          dsdePlRot(1,1) = sent*(dsdePl(2,2)*sent+dsdePl(1,2)*cost)
     &                    +cost*(dsdePl(1,2)*sent+dsdePl(1,1)*cost)
          dsdePlRot(1,2) = sent*(dsdePl(2,2)*cost-dsdePl(1,2)*sent)
     &                    +cost*(dsdePl(1,2)*cost-dsdePl(1,1)*sent)
          dsdePlRot(1,3) = dsdePl(2,3)*sent+dsdePl(1,3)*cost

          dsdePlRot(2,1) = cost*(dsdePl(2,2)*sent+dsdePl(1,2)*cost)
     &                    -sent*(dsdePl(1,2)*sent+dsdePl(1,1)*cost)
          dsdePlRot(2,2) = cost*(dsdePl(2,2)*cost-dsdePl(1,2)*sent)
     &                    -sent*(dsdePl(1,2)*cost-dsdePl(1,1)*sent)
          dsdePlRot(2,3) = dsdePl(2,3)*cost-dsdePl(1,3)*sent

          dsdePlRot(3,1) = dsdePl(2,3)*sent+dsdePl(1,3)*cost
          dsdePlRot(3,2) = dsdePl(2,3)*cost-dsdePl(1,3)*sent
          dsdePlRot(3,3) = dsdePl(3,3)
          
c         Passa para saida
          dsdePl(1,1) = dsdePlRot(1,1)
          dsdePl(1,2) = dsdePlRot(1,2)
          dsdePl(1,3) = dsdePlRot(1,3)
          dsdePl(2,1) = dsdePlRot(2,1)
          dsdePl(2,2) = dsdePlRot(2,2)
          dsdePl(2,3) = dsdePlRot(2,3)
          dsdePl(3,1) = dsdePlRot(3,1)
          dsdePl(3,2) = dsdePlRot(3,2)
          dsdePl(3,3) = dsdePlRot(3,3)
          
          
      END SUBROUTINE      
c-------------------------------------------------------------------------

              