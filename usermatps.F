*deck,usermatps    USERDISTRIB  parallel                                gal
      subroutine usermatps(
     &                   matId, elemId,kDomIntPt, kLayer, kSectPt,
     &                   ldstep,isubst,keycut,
     &                   nDirect,nShear,ncomp,nStatev,nProp,
     &                   Time,dTime,Temp,dTemp,
     &                   stress,ustatev,dsdePl,sedEl,sedPl,epsEq,
     &                   Strain,dStrain, epsPl, prop, coords, 
     &                   var0, defGrad_t, defGrad,
     &                   tsstif, epsZZ, cutFactor, 
     &                   var1, var2, var3, var4, var5, 
     &                   var6, var7)
c*************************************************************************
c     
c     Usermat TPS para concreto e concreto refor�ado com fibras de a�o
c
c         Existem vari�veis declaradas n�o usadas atualmente que poderiam 
c             ser apagadas, tamb�m algumas rotinas nos arquivos extras,
c             por�m, estoy sem paciencia e tempo pra isso.
c      
c         Usar c no come�o da linha para criar coment�rio fica bonito, 
c             fica cl�ssico, mas no fim do prazo � um inferno, ent�o,
c             como ninguem usa mais F77 e a Intel ajuda com o ! agora 
c             temos v�rios coment�rios por ! em qualquer lugar e azar.
c     
          ! Sauda��es e um abra�o
c
c     Eduardo Pagnussat Titello, 2019/2020
c
c*************************************************************************
c
c     Entrada/sa�da de dados da usermat (Original do ANSYS)
c
c     input arguments
c     ===============
c      matId     (int,sc,i)               material #
c      elemId    (int,sc,i)               element #
c      kDomIntPt (int,sc,i)               "k"th domain integration point
c      kLayer    (int,sc,i)               "k"th layer
c      kSectPt   (int,sc,i)               "k"th Section point
c      ldstep    (int,sc,i)               load step number
c      isubst    (int,sc,i)               substep number
c      nDirect   (int,sc,in)              # of direct components
c      nShear    (int,sc,in)              # of shear components
c      ncomp     (int,sc,in)              nDirect + nShear
c      nStatev   (int,sc,l)               Number of state variables
c      nProp     (int,sc,l)               Number of material ocnstants
c
c      Temp      (dp,sc,in)               temperature at beginning of
c                                         time increment
c      dTemp     (dp,sc,in)               temperature increment 
c      Time      (dp,sc,in)               time at beginning of increment (t)
c      dTime     (dp,sc,in)               current time increment (dt)
c
c      Strain   (dp,ar(ncomp),i)          Strain at beginning of time increment
c      dStrain  (dp,ar(ncomp),i)          Strain increment
c      prop     (dp,ar(nprop),i)          Material constants defined by TB,USER
c      coords   (dp,ar(3),i)              current coordinates
c      defGrad_t(dp,ar(3,3),i)            Deformation gradient at time t
c      defGrad  (dp,ar(3,3),i)            Deformation gradient at time t+dt
c
c     input output arguments              
c     ======================             
c      stress   (dp,ar(nTesn),io)         stress
c      ustatev   (dp,ar(nStatev),io)      user state variables
c      sedEl    (dp,sc,io)                elastic work
c      sedPl    (dp,sc,io)                plastic work
c      epsEq    (dp,sc,io)                equivalent plastic strain
c      epsPl    (dp,ar(ncomp),io)         plastic strains       
c      tsstif   (dp,ar(2),io)             transverse shear stiffness
c                                         tsstif(1) - Gxz
c                                         tsstif(2) - Gyz
c                                         tsstif(1) is also used to calculate hourglass
c                                         stiffness, this value must be defined when low
c                                         order element, such as 181, 182, 185 with uniform 
c                                         integration is used.
c      var?     (dp,sc,io)                not used, they are reserved arguments 
c                                         for further development
c
c     output arguments
c     ================
c      keycut   (int,sc,io)               loading bisect/cut control
c                                         0 - no bisect/cut
c                                         1 - bisect/cut 
c                                         (factor will be determined by ANSYS solution control)
c      dsdePl   (dp,ar(ncomp,ncomp),io)   material jacobian matrix
c      epsZZ    (dp,sc,o)                 strain epsZZ for plane stress, 
c                                         define it when accounting for thickness change 
c                                         in shell and plane stress states
c      cutFactor(dp,sc,o)                 time step size cut-back factor 
c                                         define it if a smaller step size is wished
c                                         recommended value is 0~1
c
c*************************************************************************
c
c      ncomp   3   for plane stress (nShear = 1)
c
c      stresss and strains, plastic strain vectors
c          11, 22, 12                for plane stress
c
c      material jacobian matrix
c        plane stress (11, 22, 12)
c           dsdePl    |  1111   1122   1112 |
c           dsdePl    |  2211   2222   2212 |
c           dsdePl    |  1211   1222   1212 |
c
c*************************************************************************
#include "impcom.inc"
c
      INTEGER          
     &                 matId, elemId,
     &                 kDomIntPt, kLayer, kSectPt,
     &                 ldstep,isubst,keycut,
     &                 nDirect,nShear,ncomp,nStatev,nProp
      DOUBLE PRECISION 
     &                 Time,    dTime,   Temp,    dTemp,
     &                 sedEl,   sedPl,   epsEq,   epsZZ, cutFactor
      DOUBLE PRECISION 
     &                 stress  (ncomp  ), ustatev (nStatev),
     &                 dsdePl  (ncomp,ncomp), sigi(ncomp),
     &                 Strain  (ncomp  ), dStrain (ncomp  ), 
     &                 epsPl   (ncomp  ), prop    (nProp  ), 
     &                 coords  (3),        
     &                 defGrad (3,3),     defGrad_t(3,3),
     &                 tsstif  (2), var0, var1, var2, var3,
     &                 var4, var5, var6, var7
c
c*************************************************************************
c     
c     Declara��o das vari�veis internas:
c    -Contadores:      
c     i, j, k         (in,  sc)   - contadores
c
c     estPto          (dp,  sc)   - Estado do Ponto de integra��o
c     
c    -Materiais:
c     fcm, fct, Eci   (dp,  sc)   - Propriedades do Concreto
c     poisson         (dp,  sc)   - Coeficiente de Poisson do Concreto
c     esmagado        (dp,  sc)   - Controle de esmagamento do concreto
c     
c    -Deforma��es e tens�es:
c     dStrainEl       (dp, 3x1)   - Parte el�stica do incremento de deforma��o
c     dStrainPl       (dp, 3x1)   - Parte pl�stica do incremento de deforma��o
c     TStress         (dp, 3x1)   - TrialStress = Stress+dStress
c     dStress         (dp, 3x1)   - Incremento de tens�o na tentativa atual
c     StrainEl        (dp, 3x1)   - Deforma��o el�stica total (+inc.)     
c     TepsPlEq        (dp,  sc)   - Trial Def. Plast. Eq. no concreto  
c     depsPlEq        (dp,  sc)   - Incremento de deforma��o pl�stica eq. conc.
c     fluxPlast       (dp, 3x1)   - Vetor de fluxo pl�stico (ou AVECT)
c     Invars          (dp, 4x1)   - Invariantes de Tens�o
c     TEfConc         (dp,  sc)   - Tens�o efeitva no concreto (YIELD)
c     TUnxConc        (dp,  sc)   - Tens�o equivalnete uniaxial no concret (PREYS)
c     TfConc_k        (dp,  sc)   - Diferen�a entre tens�o atuante e de escoamento (f) - ATUAL
c     TfConc_i        (dp,  sc)   - Diferen�a entre tens�o atuante e de escoamento (f) - LIMITE INFERIOR
c     TfConc_s        (dp,  sc)   - Diferen�a entre tens�o atuante e de escoamento (f) - LIMITE SUPERIOR
c     razTf           (dp,  sc)   - Raz�o TfConc_i/TfConc_k
c     lambda_k        (dp,  sc)   - Multiplicador da fun��o de fluxo - ATUAL   
c     lambda_i        (dp,  sc)   - Multiplicador da fun��o de fluxo - LIMITE INFERIOR
c     lambda_s        (dp,  sc)   - Multiplicador da fun��o de fluxo - LIMITE SUPERIOR
c     bis_incl        (dp,  sc)   - Bisse��o: inclina��o
c
c     itLim           (int, sc)   - Limite de itera��es no total
c     itLimNR         (int, sc)   - Limite de itera��es de Newton-Raphson - Plasticidade
c     passoNR         (dp,  sc)   - Tamanho do passo de NR determina pelo line-search
c     direcNR         (dp,  sc)   - Dire��o apontada por NR para dar passo
c     tolfConc        (dp,  sc)   - Tolerancia da fun��o de escoamento f
c     
c     TensPrinc       (dp, 2x1)   - Tens�es principais
c     thetaTS         (dp,  sc)   - Angulo theta das tens�es principais
c     nd              (dp,  sc)   - Escalar para armazenar valores n�o usados...
c     BetaG           (dp,  sc)   - Fator de redu��o de rigidez transversal p/ fissuras
c     BetaMin         (dp,  sc)   - Valor minimo da redu��o da rigidez
c     lc              (dp,  sc)   - Comprimento caracteristico do elemento (obtido pela �rea)

      
      DOUBLE PRECISION estPto, fcm, fct, Eci, poisson, dStrainEl(3),
     &                 dStrainPl(3), TStress(3), dStress(3), TepsPlEq,
     &                 fluxPlast(3), Invars(4), TEfConc, depsPlEq,
     &                 TUnxConc, TfConc_k, const, lambda_k, BetaMin,
     &                 TfConc_i, TfConc_s, lambda_i, lambda_s,
     &                 bis_incl, ModdStr, passoNR, esmagado, tolfConc,
     &                 direcNR, est_EstPto, nd, TensPrinc(2), thetaTS,
     &                 StrainEl(3), BetaG, razTf, const0, itLim, lc
      
      INTEGER i, j, k, itLimNR
      
c     Importa subrotinas do ANSYS     
      EXTERNAL vmove, vzero, vamb, maxv, vapb, vapcb1, vmult1, vdot,
     &         vapb1, get_ElmData1

      
c     Importa propriedades para vari�veis de estado 
      CALL est_imp_props(nProp, prop, nStatev, ustatev)
      
c     Verifica se a itera��o anterior convergiu com sinal de interrup��o -> CANCELA SOLVER <-
      !! CALL est_ver_interp(keycut, cutFactor, nStatev, ustatev)      
      
c     Importa propriedades do concreto p/ vari�veis locais
      CALL est_propConc(fcm, fct, Eci, poisson, nStatev, ustatev)
      
      
c     Se time=0 (nos passos iniciais)
      IF(time .EQ. 0.d0) THEN
c        Grava elemId e kDomIntPt nas vari�veis de estado para facilitar o controle
         CALL est_elem_kdom(1, elemId, kDomIntPt, nStatev, ustatev)
         
c        Grava posi��o inicial dos pontos de integra��o
         CALL est_coords0(1, coords, nStatev, ustatev)
      END IF

c     Matriz constitutiva el�stica
c     CALL EPT_MatrizD(Eci, poisson, dsdePl)
      

      !CALL EPT_TensPrinc(Stress, TensPrinc, thetaTS)
      !IF(TensPrinc(1) .GT. fct*1.05) THEN
      !    nd = 1.d0
      !END IF

      IF(est_EstPto(nStatev, ustatev) .EQ.  0.d0) THEN    
c         PONTO EST� COMPRIMIDO / N�O FISSURADO / NEUTRO
          
c         Para n�o ter que declarar uma outra matriz e copiar essa dentro vou
c         armazenar a matriz const. el�stica na matriz da pl�stica. ;)
          CALL EPT_MatrizD(Eci, poisson, dsdePl)
          
c         Zerando vetores e vari�veis a serem usados
          CALL vzero(dStrainPl, 3)
          CALL vzero(TStress,   3)
    
c         C�lculo das condi��es de carga:
c         - Indice i: tra��o/fissura��o
c         Se f<=0: El�stico / f>0: Fissura��o
c         Nesse caso "ainda n�o existe" deforma��o plastica ent�o lambda e epsEq s�o nulos
c         isso faz com que TfConc_i seja na verdade a tens�o Efetiva pelo superf�cie adotada!!
          CALL conc_TfConc_EPT(0.d0, TfConc_i, Stress,
     &            dStrain, dsdePl, TStress, dStrainPl, 
     &            TepsPlEq, 0.d0, nd, nStatev, ustatev)
c         Para fissura��o determina tens�es principais
          CALL EPT_TensPrinc(TStress, TensPrinc, thetaTS)
          
          
c         Agora entra o crit�rio das usermats que n�o est� escrito em nenhum trabalho:
c             Se a tens�o principal de compress�o normalizada por fcm for maior que 
c                a tens�o principal de tra��o normalizada por fct
c             � considerado como comprimido. Sendo o incremento el�stico.
          IF(((TensPrinc(1)/fct) .GT. DABS(TensPrinc(2)/fcm))) THEN 
c             Ponto est� relativamente mais tracionado que comprimido
c
c             Vamos considerar como tracionado e trabalhar EL�STICAMENTE.
c             O incremento feito em TfConc_i j� � el�stico, basta ent�o verificar 
c             quanto a fissura��o.
              
c             Verifica se atingiu superf�cie de ruptura por tra��o
              IF((TfConc_i .GE. fcm) .AND. 
     &           (TensPrinc(1) .GE. fct/2.d0)) THEN
c                 Quando o concreto rompe por tra��o passa a ser um material
c                 ortotr�pico, ao meu ver (hoje kkk) como a carga de tra��o
c                 at� a ruptura � el�stica as tens�es atuantes devem ser 
c                 determinadas de forma ortotr�pica ap�s o surgimento da 
c                 primeira fissura. Basicamente o surgimento da fissura
c                 vai alterar o estado de tens�es pois poisson deixa de
c                 afetar a solu��o.
c
c                 Para uma tra��o simples at� a fissura n�o muda muito,
c                 por�m no caso de tra��o biaxial d� uma grande diferen�a!
c
c                 O que vai ser feito: vai ser marcado se a ruptura foi 
c                 em uma ou duas dire��es. A altera��o do estado de tensoes
c                 vai ser ignorada, por motivos de for�a maior, tentei e n�o
c                 consegui manter da forma correta... Ent�o ap�s o surgimento
c                 da/das fissuras vou rotacionar o estado atual de tens�es e 
c                 deforma��es pra informar a rotina de fissura��o o ultimo 
c                 estado isotr�pico, l�, no fim do c�digo, quando considerar
c                 a fissura��o vou fazer incrementos ortotr�picos.
c                 Acredito que manter o estado de tens�es pode levar a 
c                 um aumento na estabilidade do solver, memso introduzindo
c                 algum erro, pois n�o vai criar um dente no estado de tens�es,
c                 precisa ver se isso vai estragar muito a solu��o.
c                 
c                                  
c
c                 Guarda angulo da fissura
                  CALL est_ThetaEPT(1, thetaTS, nStatev, ustatev)
                  
c                 !Copia tens�es originais de Stress p/ TStress que ser� depois 
c                 !    colocado novamente em Stress (que volta n�?)
                  CALL vmove(Stress, TStress, 3)
                  CALL EPT_Rot_Tens(TStress, thetaTS)
                  
c                 Rotaciona deforma��es el�sticais atuais (sem incremento) para
c                 angulo da fissura.
                  CALL vamb(Strain, epsPl, StrainEl, 3)
                  CALL EPT_Rot_Def(StrainEl, thetaTS)
                  
c                 Determina se a fissura��o ocorreu em UMA ou DUAS dire��es
c                 aqui temos o problema da solu��a num�rica ter algumas 
c                 instabilidades, por isso se atingir a superf�cie e as tens�es
c                 forem semelhantes e proximas da tens�o de ruptura o
c                 ponto vai ser considerado "birrompido" (hsauhshahus)
                  IF((TensPrinc(1)/TensPrinc(2) .LT. 1.10d0) .AND.
     &               (TensPrinc(2)/fct .GT. 0.90d0)) THEN
c                     Rompido em duas dire��es - 12 = 2 fissuras
                      CALL est_DefEstPto(12.d0, nStatev, ustatev)
                      CALL est_Fiss_DT(1, 1, TStress(1), StrainEl(1), 
     &                                    nStatev, ustatev)
                      CALL est_Fiss_DT(1, 2, TStress(2), StrainEl(2), 
     &                                    nStatev, ustatev)
                  ELSE
c                     Rompido em uma dire��o - 11 = 1 fissura 
                      CALL est_DefEstPto(11.d0, nStatev, ustatev)
                      CALL est_Fiss_DT(1, 1, TStress(1), StrainEl(1), 
     &                                    nStatev, ustatev)
                  END IF

c                 Rotaciona tens�es para dire��o global 
                  CALL EPT_Rot_Tens(TStress, -thetaTS) 

              ELSEIF((TfConc_i .GE. fcm) .AND. 
     &           (TensPrinc(1) .LT. fct/2.d0)) THEN
c                 Apenas uma verifica��o de seguran�a: elemento tracionado 
c                 atinge superf�cie de ruptura por compress�o? pode isso Galv�o?
                  
                 WRITE(*,*) 'Seguinte: um elemento que eh considerado
     &            tracionado esta rompendo por compressao! (p. ENTER)
     &            ISSO ESTA MUITO ERRADO !!!'
                 READ(*,*) 
              END IF

          ELSE
c             Ponto est� relativamente mais comprimido que tracionado
c
c             - Indice k: compress�o/plasticidade/el�stico em descarga e recarga
c             Se f<=0: El�stico / f>0: Plastico
c             Para determinar f com def.pl�stica atual e incremento totalmente 
c             elastico usa lambda=0.0.
              CALL conc_TfConc_EPT(0.d0, TfConc_k, Stress,
     &            dStrain, dsdePl, TStress, dStrainPl, 
     &            TepsPlEq, epsEq, esmagado, nStatev, ustatev)

c             Tolerancia da fun��o de plasticidade/escoamento/ruptura f
              tolfConc = fcm/500.d0 !0.00001d0 
          
c             Verifica se h� deforma��o pl�stica e chama NR
              IF(TfConc_k .GE. tolfConc) THEN
c                 Elemento n�o pode come�ar esmagado 
                  esmagado = 0.d0
          
c                 M�todo de Newton-Raphson
c                 Loop de solu��o de lambda/deforma��o pl�stica inicial
                  !lambda_k = 0.000001d0  ! Bom p/ modelo 1 
                  CALL est_lambda_k(0, lambda_k, nStatev, ustatev)
c                 Const � o incremento da derivada num�rica 5%.lambda_k
                  const0  = lambda_k/20.d0
                  const   = const0
                  I = 0
c                 Limite de itera��es
                  itLim   = 300
                  itLimNR = 300
                  passoNR = 1.d0
                  direcNR = 0.d0
          
c                 Usei um DO sem while e contador interno para garantir a sa�da pelo num de iters
                  DO
c                     Contabiliza passo
                      I = I + 1
      
c                     Determina f (TfConc_k) correspondente a lambda_k
                      CALL conc_TfConc_EPT(lambda_k,TfConc_k,Stress,
     &                  dStrain, dsdePl, TStress, dStrainPl, 
     &                  TepsPlEq, epsEq, esmagado, nStatev, ustatev)   
              
c                     Crit�rio de sa�da/execu��o
                      IF(DABS(TfConc_k) .LT. tolfConc) THEN
c                         Convergiu - Sai do loop
                          CALL est_lambda_k(1,lambda_k,nStatev,ustatev)
                          EXIT
                      ELSEIF(I .GE. itLim) THEN
c                         Limite de itera��es excedido - Sai do loop - Sinal:2
c                         Se n�o � capaz de convergir mesmo ap�s relaxar solu��o vamos 
c                         considerar que o ponto est� esmagado.
                          TStress = Stress
                          dStrainPl = 0.d0
                          TepsPlEq = epsEq
c                         Informa rotina de interrup��o do solver que se esse passo for ADOTADO o passo seguinte pode interromper solu��o 
                          CALL est_interromper(2.d0, nStatev, ustatev)
c                         Marca ponto como esmagado (-10)
                          CALL est_DefEstPto(-10.d0, nStatev, ustatev)
                          EXIT
                      ELSEIF(I .LE. itLimNR) THEN
c                         M�todo de Newton-Raphson com Line-Search (deriva��o num�rica)
c                         lambda_i=lambdak+incremento (const)
                          lambda_i = lambda_k + const
c                         Determina f (TfConc_k) correspondente a lambda_k
                          CALL conc_TfConc_EPT(lambda_i,TfConc_i,
     &                        Stress,dStrain,dsdePl,TStress,dStrainPl, 
     &                        TepsPlEq,epsEq,esmagado,nStatev,ustatev)
      
c                         Dire��o � x(k+1)-x(k)
                          direcNR = -TfConc_k*const/(TfConc_i-TfConc_k)
                  
                          DO J=0,10
c                             Passo = 0.5^J -> 0, 0.5, 0.25, 0.125....
                              passoNR = 0.5d0**J
                              lambda_i = lambda_k + passoNR*direcNR
                      
c                             Determina f (TfConc_k) correspondente a lambda_k
                              CALL conc_TfConc_EPT(lambda_i,TfConc_i,
     &                         Stress,dStrain,dsdePl,TStress,dStrainPl,
     &                         TepsPlEq,epsEq,esmagado,nStatev,ustatev)
                      
c                             Verifica se reduziu valor 
                              ! Dependente da escala
                              IF(DABS(TfConc_i) .LT. DABS(TfConc_k))EXIT
                      
                      
c                             Aqui deveria ser usada uma regra melhor tipo Armijo, mas assim j� serve
                              
                              !razTf = TfConc_i/TfConc_k
                              !IF(ISNAN(razTf)) razTf = 0.d0
c                             Achou maior passo com redu��o, pronto
                              !IF(DABS(razTf) .LE. (1.d0)) EXIT
                              !IF(DABS(razTf) .LE. 1.d0) EXIT
                          END DO
c                         Verifica como saiu do linesearch      
                          IF(J .GE. 10) THEN
                              passoNR = 0.5d0
                              IF(const .LE. 10.d0*const0) THEN
c                                 Se n�o convergiu tenta aumentar o passa da derivada - at� 10x
                                  const = 2.d0 * const
                              END IF
                          END IF
                          lambda_k = lambda_k + passoNR*direcNR
                      ELSE
c                         M�todo da bisse��o - ou algo semelhante da minha cabe�a
c                         At� itlimNR usa NR, depois entra aqui
                          IF(I .EQ. itLimNR) THEN
c                             Entrou agora no m�todo? determinar intervalo
                              !IF(
                              continue
                          END IF
                      END IF
                  END DO
              END IF
c             Passa valores para vetores do ANSYS
              epsEq = TepsPlEq
              CALL vapb1(epsPl, dStrainPl, 3)    
          END IF

c         Passa TStress para vetor de tens�es do ANSYS
          CALL vmove(TStress, Stress, 3)
      END IF
          

c              
c     Ponto est� esmagado??
c
      IF(est_EstPto(nStatev, ustatev) .EQ. -10.d0) THEN
c         Ponto foi esmagado por compress�o - vamos tirar poisson
          CALL EPT_MatrizD(Eci, poisson, dsdePl) ! Mas aqui n�o JK
          
          Stress(1) = Eci*(Strain(1)+dStrain(1)-epsPl(1))
          Stress(2) = Eci*(Strain(2)+dStrain(2)-epsPl(2))
          Stress(3) = Eci/2.d0*(Strain(3)+dStrain(3)-epsPl(3))
          ! Determina como limite superiro 0.01fcm e conserva sinal
          IF(DABS(Stress(1)) .GT. 0.01d0*fcm) 
     &                Stress(1) = DSIGN(0.01d0*fcm, Stress(1))
          IF(DABS(Stress(2)) .GT. 0.01d0*fcm) 
     &                Stress(2) = DSIGN(0.01d0*fcm, Stress(2))
          IF(DABS(Stress(3)) .GT. 0.005d0*fcm) 
     &                Stress(3) = DSIGN(0.005d0*fcm, Stress(3))
      END IF        
              
            
              
              
c     Verifica estado de fissura��o, n�o pode usar ELSEIF da sequencia anterior!
      IF(est_EstPto(nStatev, ustatev) .GE. 11.d0) THEN
c         Ponto est� fissurado em UMA ou DUAS dire��es
c
c         Aqui o comportamento � ortotr�pico ent�o n�o � necess�rio usar
c         a matriz constitutiva e etc, por�m ela � necess�ria para o solver
c         do ANSYS se encontrar nessa bagun�a que � a vida adulta.
c         O que acontece com a matriz elastoplastica? Bom...
c         -Pelo fato de ser ortotr�pico e sem Poisson � s� declarar a diagonal;
c         -Se apenas uma dire��o est� fissurada ela tem Eci na segunda dire��o 
c         um "E" tangente na primeira dire��o;
c         -Se estiver fissurado em duas dire��es tem modulos "E" tangentes em 
c         duas dire��es que n�o s�o necessariamente iguais, depende do TS aplicado;
c         -Em ambos os casos a matriz deve ser rotacionada conforme sentido da fissura!
c         
c         
c         
c         -----
c         Est�gio inicial, determina valores a serem rotacionados para dir. da fissura
c         e outros que s�o independentes de dire��o.
c     
c         Determina deforma��es el�sticas atuais (sem incremento)
          CALL vamb(Strain, epsPl, StrainEl, 3)
c         Copia incremento de deforma��o (n�o � el�stico, mas vamos fazer de conta que seja)
          CALL vmove(dStrain, dStrainEl, 3)
c         Define tens�es atuais como TStress p/ rotacionar sem problemas (sei l� n�)
          CALL vmove(Stress, TStress, 3)
          
c         Angulo da primeira fissura -> a segunda � ortogonal
          CALL est_ThetaEPT(0, thetaTS, nStatev, ustatev)
          
          
c         -----
c         Rotacionando para plano XY das fissuras
          CALL EPT_Rot_Def(StrainEl,  thetaTS)
          CALL EPT_Rot_Def(dStrainEl, thetaTS)
          CALL EPT_Rot_Tens(TStress,  thetaTS)
          
          
c         -----
c         A partir de agora estamos trabalhando na dire��o da fissura onde X � a
c         dir. da primeira fissura e Y da segunda (exista ou n�o, vai ser 90� de X)
          
c         A matriz pl�stica � obtida em etapas e diretamente na dire��o das fissuras
c         Gera matriz constitutiva inicial sem Poisson e com Eci
          CALL EPT_MatrizD(Eci, 0.d0, dsdePl)
          
c         Verifica se a segunda fissura � formada nesse passo de deforma��o
          IF(est_EstPto(nStatev, ustatev) .EQ. 11.d0) THEN
              TStress(2) = TStress(2) + Eci*dStrainEl(2)
              IF(TStress(2) .GT. 0.98d0*fct) THEN
c                 Rompe na segunda dire��o
c                 Desfaz essa altera��o no TStress para fazer incremento depois
                  TStress(2) = TStress(2) - Eci*dStrainEl(2)
                  CALL est_DefEstPto(12.d0, nStatev, ustatev)
                  CALL est_Fiss_DT(1, 2, TStress(2), StrainEl(2), 
     &                                nStatev, ustatev)
              !ELSEIF(TStress(2) .LT. 0.1d0) THEN
              ELSE
c                 Ponto est� comprimido nessa dire��o 
                  CALL conc_Uniax_MC2010(StrainEl(2)+dStrainEl(2), 
     &                        TStress(2), nStatev, ustatev)
                  dsdePl(2,2) = Eci
              END IF  
          END IF

c         Atrav�s da fun��o de Tra��o determina tens�es e rigidez para primeira fissura
          CALL conc_Trac(1, StrainEl(1), dStrainEl(1), TStress(1),   
     &                    dsdePl(1,1), nStatev, ustatev)
              
c         Atrav�s da fun��o de Tra��o determina tens�es e rigidez para segunda fissura, se existir!
          IF(est_EstPto(nStatev, ustatev) .EQ. 12.d0) THEN
              CALL conc_Trac(2, StrainEl(2), dStrainEl(2), TStress(2), 
     &                    dsdePl(2,2), nStatev, ustatev)
          END IF
              
          
c         Determina tens�o de cisalhamento atuante
c         Usa maior fissura.    
          ! Essa � a maneira correta sim, quando coloquei DABS anteriormente
          ! pegava fissura fechando e colocava na transversal (poderia ao menos)
          ! depois, quando tirei e n�o limitei >0 havia o problema de elevar
          ! um numero negativo a 0.3, ent�o isso virava um numero complexo...
          ! Agora sem DABS e com a verifica��o posterior fica tudo na paz!
          !
          ! -Aqui betaG � a deforma��o no plano da fissura, n�o beta de fato!
          IF(est_EstPto(nStatev, ustatev) .EQ. 12.d0) THEN
              BetaG = MAX(dStrainEl(1) + StrainEl(1),
     &                    dStrainEl(2) + StrainEl(2))
          ELSE
              BetaG = (dStrainEl(1) + StrainEl(1))
          END IF

          ! Se estiver comprimida
          IF(BetaG .LT. 0.d0) BetaG = 0.d0
          
          ! Calcula de fato BetaG
          BetaG = 1.d0 - (BetaG/0.005d0)**0.3d0
          
c         Imp�em limites a essa zoeira
          !IF(BetaG .LT. 0.001d0) BetaG = 0.001d0
          !IF(BetaG .LT. 0.000001d0) BetaG = 0.000001d0
          ! TEMPORARIO
          BetaMin = ustatev(48)
          IF(BetaMin .EQ. 0.d0) BetaMin = 0.00001d0
          IF(BetaG .LT. BetaMin) BetaG = BetaMin
          IF(BetaG .GT. 1.0d0) BetaG = 1.0d0

          
c         Aplica Beta sobre a rigidez transversal
          TStress(3) = BetaG*Eci/2.d0*(dStrainEl(3) + StrainEl(3))
          
          
c         -----
c         Rotacionando para plano XY original (-theta)
          CALL EPT_Rot_Tens(TStress,  -thetaTS)
          !CALL EPT_Rot_Const(dsdePl,  -thetaTS)
          
c         -----
c         Trabalhos finais a serem realizados no plano XY   
          
c         Passa TStress para vetor de tens�es do ANSYS
          CALL vmove(TStress, Stress, 3)
              
          ! Ignora e adota matriz el�stica inicial ! SEMPRE
          !CALL EPT_MatrizD(Eci, poisson, dsdePl)
          
          
      END IF              
      
c     Matriz elastopl�stica sempre inicial
      CALL EPT_MatrizD(Eci, poisson, dsdePl)
              
              
c              
c     Deforma��o em Z conforme estado do ponto
c
      IF(est_EstPto(nStatev, ustatev) .EQ. 0.d0) THEN
c         Ponto est� neutro
c         (copiei da usermat original ;)
          epsZZ = -poisson/Eci*(Stress(1) + Stress(2)) 
          epsZZ = epsZZ - (epsPl(1) + epsPl(2))
      ELSE
c         Nesses casos poisson=0, ent�o:
          epsZZ = 0.d0
      END IF       
              
      !epsZZ = -poisson/Eci*(Stress(1) + Stress(2)) 
      !epsZZ = epsZZ - (epsPl(1) + epsPl(2))  


c     Rigidez transversal tsstif, acho que n�o � usado em nada...
      tsstif(1) = dsdePl(3,3)
      !tsstif(2) = dsdePl(3,3)
          

      
c     Trabalhos de deforma��o        
c         Vamos fazer de conta que hoje � feriado.
c             De acordo com o ANSYS: for output purposes only
          
      
c     CONTROLES, DEBUG E FISSURA��O
      CALL DBG_EstPontos(Time, nStatev, ustatev)
      
              
      END

