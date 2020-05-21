*deck,user01       USERDISTRIB                                          ANSYS,INC
      function  user01()
c *** primary function:    user routine number  01

c         *** Copyright ANSYS.  All Rights Reserved.
c         *** ansys, inc.
c *** Notice - This file contains ANSYS Confidential information ***

c  /*******************************************************************\
c  | this is a user routine that may be used by users to include their |
c  | special coding.  accesss to this routine is by the command usr1.  |
c  | usr1 may be changed by the user using the command /ucmd.  the     |
c  | user may then use this routine to call his/her special routines.  |
c  | ansys routines to access information in the ansys database may be |
c  | found in the "ansys programmer's manual", available from ansys,inc|
c  | see user02 for a simple example usage.                            |
C  | routines user03 to user10 are also available.                     |
C  \*******************************************************************/

c  input arguments:  none

c  output arguments:
c     user01   (int,sc,out)      - result code (should be zero)
c                                   (which is ignored for now)

c    **************************************************************
c    Functions for accessing data on the command line
c    integer function  intinfun(iField) - gets an integer from field iField
c    double precision function dpinfun(iField) - gets double precision
c    character*4 ch4infun(iField) - gets (upper case) 4 characters
c    character*8 ch8infun(iField) - gets (mixed case) 8 characters
c    character*32  ch32infun(iField) - gets (mixed case) 32 characters
c    **************************************************************
c
#include "impcom.inc"
#include "ansysdef.inc"

      INTEGER          user01, ungr
      LOGICAL          fabert
      CHARACTER        fname*32
      
c-----------------------------------------
c     
c     Usado para abrir e fechar unidade 80
c     de gravação do arquivo de pontos
c     
c-----------------------------------------
      
c     Define unidade de gravação e nome do arquivo
      fname = '_relpts.csv' 
      ungr  = 80
          
      INQUIRE(unit=ungr, opened=fabert)
      IF(fabert .EQV. .TRUE.) THEN
c         Arquivo aberto: fechar
          CLOSE(unit=ungr)
      ELSE
c         Arquivo fechado: abrir e por cabeçalho
          OPEN(unit=ungr, file=fname, action='write')
c         Grava cabeçalho
          WRITE(ungr, '(A)') 'sep=,'
          WRITE(ungr, '(A)') 'Time,elemId,kDomIntPt,
     &coordsX,coordsY,coordsZ,estado,theta,eps1,eps2'
      END IF
      
      user01 = 0

      return
      end
