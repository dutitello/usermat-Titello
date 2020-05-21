c*************************************************************************
c     
c     Subrotinas para controle e debug da solução, nomeadas DBG_xxx.
c     
c
c
c     Eduardo Pagnussat Titello, 2019
c
c*************************************************************************
      
c-------------------------------------------------------------------------
c
c     Grava arquivo de estado dos pontos, se solicitado
c
      SUBROUTINE DBG_EstPontos(Time, nStatev, ustatev)
c         Parametros:

c         Time            (dp,  sc,  in)  - Tempo 
          DOUBLE PRECISION Time
          INTEGER          elemId, kDomIntPt
          
c         Variáveis internas:
c         elemId          (int, sc)   - Numero do elemento
c         kDomIntPt       (int, sc)   - Ponto de gauss
c         coords          (dp, 3x1)   - Coordendas do ponto de integração
c         ungr            (int, sc)   - Unidade de gravação = Variável de estado de controle = 80
c         theta           (dp,  sc)   - Angulo theta da fissura
c         eps1, eps2      (dp,  sc)   - Deformações nas fissuras 1 e 2
c         nd              (dp,  sc)   - nada
c         estado          (dp,  sc)   - Estado do ponto
c         opened          (log, sc)   - Controla se arquivo aberto
c         fname           (chr, 32)   - Nome do arquivo aberto, se aberto
c
          DOUBLE PRECISION coords(3), theta, eps1, eps2, nd, estado, 
     &                     est_EstPto
          INTEGER          ungr
          LOGICAL          fabert
          CHARACTER        fname*32
          
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          INTEGER          nStatev
          DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------   
          
c         Define unidade de gravação e nome do arquivo
          fname = '_relpts.csv' 
          ungr  = 80
          
c         Retorna elemId e kDomIntPt
          CALL est_elem_kdom(0, elemId, kDomIntPt, nStatev, ustatev)
          CALL est_coords0(0, coords, nStatev, ustatev)
          
c         Verifica se rotina deve ser usada
          IF(ustatev(ungr) .EQ. 1.d0) THEN
c             Verifica se unidade está aberta, se não estiver abre
              INQUIRE(unit=ungr, opened=fabert)
              IF(fabert .EQV. .FALSE.) THEN
                  CALL DBG_erro(21)
              ! Isso era feito aqui, agora basta executaro o comando usr1 
              !   antes e depois da solução para abrir e fechar o arquivo, 
              !   evitando a gravação errada do cabeçalho pelos diversos threads
              
                  
c                 Arquivo fechado: Abre
!                  OPEN(unit=ungr, file=fname, action='write')
c                 Grava cabeçalho
!                  WRITE(ungr, *) 'sep=,'
!                  WRITE(ungr, *) 'Time,elemId,kDomIntPt,',
!     &                        'coordsX,coordsY,coordsZ,',
!     &                        'estado,theta,eps1,eps2'
              END IF

c             Importa dados dos pontos
              estado = est_EstPto(nStatev, ustatev)
              CALL est_ThetaEPT(0, theta, nStatev, ustatev)
              CALL est_Fiss_DT(0, 1, nd, eps1, nStatev, ustatev)
              CALL est_Fiss_DT(0, 2, nd, eps2, nStatev, ustatev)
              
c             Grava conteudo no arquivo
              WRITE(ungr, 180) Time, elemId, kDomIntPt,
     &                        coords(1), coords(2), coords(3), 
     &                        estado, theta, eps1, eps2
              
              END IF
              
c     Cria formato a ser usado, sendo a ordem igual do cabeçalho, obvio
  180 FORMAT(F10.6,',',I4,',',I2,',',F14.6,',',F14.6,
     &               ',', F14.6,',', F4.0,',', 
     &               F10.6,',',E15.7E3,',',E15.7E3)
              
     
      END SUBROUTINE
c-------------------------------------------------------------------------


          
          
c-------------------------------------------------------------------------
c
c     Facilita uso de erros! Aqui estão cadastrados os erros usados
c
      SUBROUTINE DBG_erro(msgid)
c         Emite erro pela função de erro do ANSYS (erhandler)
c     
c         Parametros:
c         msgid           (int, sc,  in)  - id do erro chamado
          INTEGER msgid
          
c         Variáveis internas
c         filein              (cha, 40)   - Arquivo que gerou erro
c         msglvl              (int, sc)   - Gravidade do erro 
c                                             (1=note, 2=warning, 
c                                               3=error, 4=fatal
c                                              -3=error w/tech supp note
c                                              -4=fatal w/tech supp note
c         lngstrng            (cha, 300)  - Mensagem de erro 
          INTEGER     msglvl
          CHARACTER   filein*40, lngstrng*300
          
c         Importa rotina de erro
          EXTERNAL erhandler
          
c         Inicialmente "zera" variáveis para garantir que funcionará
          filein = "USERMAT.F (geral)"
          msglvl = 3  ! Padrão é erro
          lngstrng = "UM ERRO NAO DECLARADO OCORREU!"

          
c         Agora conforme código do erro altera saída
          SELECT CASE (msgid)
              CASE(1)
                  filein = "rotinas_estado.F/est_imp_props"
                  msglvl = 4
                  lngstrng = "ATENÇÃO: Uma das 4 primeiras propriedades
     & de entrada da USERMAT está problemática. Finalizando solução."
              CASE(2)
                  filein = "rotinas_estado.F/est_parOttosen"
                  lngstrng = "ATENÇÃO: As propriedades de Ottossen estão
     & sendo solicitados por um modelo diferente ao de Ottossen."
              CASE(3) 
                  filein = "rotinas_concreto.F/conc_TEf_WW/WWf"
                  lngstrng = "ATENÇÃO: O número de passos da solução 
     & é insuficiente. Caso a solução seja concluída assim o concreto 
     & poderá apresentar comportamento elástico linear para qualquer 
     & tensão."
              CASE(21)
                  filein = "rotinas_debug.F/DBG_EstPontos"
                  lngstrng = "ATENÇÃO: A exportação do estado dos pontos
     & de integração está ativada, porém o arquivo de não foi 
     & aberto/inicializado, antes e após e executar a solução o comando
     & usr1 deve ser executado."
          END SELECT
          
c         Emite erro
          CALL erhandler(filein, msgid, msglvl, lngstrng, 0.0d0,' ')
          
      END SUBROUTINE
c-------------------------------------------------------------------------

          

          
c-------------------------------------------------------------------------
c
c     Abre unidade de gravação
c
      SUBROUTINE DBG_TensPrinc(stress, TensPrinc, theta)
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

   
c     -----------------------------------------------------------
c         Para uso das subrotinas est_xx()
c         nStatev e ustatev -> variáveis de estado do ANSYS-INOUT
          !INTEGER          nStatev
          !DOUBLE PRECISION ustatev(nStatev)
c     ------------------------------------------------------------
          
          
c         Partes que compõem as tensões principais

          
      END SUBROUTINE
c-------------------------------------------------------------------------

