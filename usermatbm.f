*deck,usermatbm    USERDISTRIB  parallel                                gal
      subroutine usermatbm(
     &                   matId, elemId,kDomIntPt, kLayer, kSectPt,
     &                   ldstep,isubst,keycut,
     &                   nDirect,nShear,ncomp,nStatev,nProp,
     &                   Time,dTime,Temp,dTemp,
     &                   stress,ustatev,dsdePl,sedEl,sedPl,epseq,
     &                   Strain,dStrain, epsPl, prop, coords, 
     &                   var0, defGrad_t, defGrad,
     &                   tsstif, epsZZ, cutFactor, 
     &                   var1, var2, var3, var4, var5,
     &                   var6, var7)
c*************************************************************************
c     *** primary function ***
c
c           user defined material constitutive model
c
c      Attention:
c           User must define material constitutive law properly
c           according to the stress state such as 3D, plane strain
c           and axisymmetry, plane stress and beam.
c
c           a 3D material constitutive model can use for
c           plane strain and axisymmetry cases.
c
c           When using shell elements, a plane stress algorithm
c           must be use.
c
c                                             gal July, 1999
c
c       The following demonstrates a USERMAT subroutine for 
c       a plasticity model in 3D beam(188, 189). The plasticity
c       model is the same as TB, BISO.
c       See "ANSYS user material subroutine USERMAT" for detailed
c       description of how to write a USERMAT routine.
c
c*************************************************************************
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
c      ustatev   (dp,ar(nStatev),io)       statev
c           ustatev(1)                     - equivalent plastic strain
c           ustatev(2) - ustatev(1+ncomp)  - plastic strain vector
c           ustatev(nStatev)               - von-Mises stress
c      sedEl    (dp,sc,io)                elastic work
c      sedPl    (dp,sc,io)                plastic work
c      epseq    (dp,sc,io)                equivalent plastic strain
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
c      ncomp   6   for 3D
c      ncomp   4   for plane strain, axisymmetric (nShear = 1)
c      ncomp   3   for plane stress (nShear = 1)
c      ncomp   3   for 3D beam (nShear = 2), beam188/189
c      ncomp   1   for 1D beam, link180
c
c      stresss and strains, plastic strain vectors
c          11, 22, 33, 12, 23, 13    for 3D
c          11, 22, 33, 12            for Plane strain and axisymmetry
c          11, 22, 12                for Plane stress
c          11, 13, 12                for 3d beam
c          11                        for 1D
c
c      material jacobian matrix
c        3D
c           dsdePl    |  1111   1122   1133   1112   1123   1113 |
c           dsdePl    |  2211   2222   2233   2212   2223   2213 |
c           dsdePl    |  3311   3322   3333   3312   3323   3313 |
c           dsdePl    |  1211   1222   1233   1212   1223   1213 |
c           dsdePl    |  2311   2322   2333   2312   2323   2313 |
c           dsdePl    |  1311   1322   1333   1312   1323   1313 |
c        plane strain, axisymmetric
c           dsdePl    |  1111   1122   1133   1112 |
c           dsdePl    |  2211   2222   2233   2212 |
c           dsdePl    |  3311   3322   3333   3312 |
c           dsdePl    |  1211   1222   1233   1212 |
c        plane stress
c           dsdePl    |  1111   1122   1112 |
c           dsdePl    |  2211   2222   2212 |
c           dsdePl    |  1211   1222   1212 |
c        3d beam plasticity
c           dsdePl    |  1111   1113   1112 |
c           dsdePl    |  1311   1313   1312 |
c           dsdePl    |  1211   1213   1212 |
c        1d
c           dsdePl    |  1111 |
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
     &                 sedEl,   sedPl,   epseq,   epsZZ,  cutFactor
      DOUBLE PRECISION 
     &                 stress  (ncomp  ), ustatev (nStatev),
     &                 dsdePl  (ncomp,ncomp), sigi(ncomp),
     &                 Strain  (ncomp  ), dStrain (ncomp  ), 
     &                 epsPl   (ncomp  ), prop    (nProp  ), 
     &                 coords  (3),       
     &                 defGrad (3,3),     defGrad_t(3,3),
     &                 tsstif  (2)
c
c***************** User defined part *************************************
c
c --- parameters
c
      INTEGER          NEWTON, mcomp
      DOUBLE PRECISION HALF, ONE, TWO, SMALL, SQTWOTHIRD,
     &                 ZERO, TWOTHIRD, ONEDM02, ONEDM05, sqTiny
      PARAMETER       (ZERO       = 0.d0,
     &                 HALF       = 0.5d0,
     &                 ONE        = 1.d0,
     &                 TWO        = 2.d0,
     &                 SMALL      = 1.d-08,
     &                 sqTiny     = 1.d-20,
     &                 ONEDM02    = 1.d-02,
     &                 ONEDM05    = 1.d-05,
     &                 TWOTHIRD   = 2.0d0/3.0d0,
     &                 SQTWOTHIRD = 0.816496580927726030d0,
     &                 NEWTON     = 20,
     &                 mcomp      = 3
     &                 )
c
c --- local variables
c
c      sigElp   (dp,ar(3  ),l)            trial stress
c      dsdeEl   (dp,ar(3,3),l)            elastic moduli
c      pleq_t   (dp,sc     ,l)            equivalent plastic strain at beginnig of time increment
c      pleq     (dp,sc     ,l)            equivalent plastic strain at end of time increment
c      dpleq    (dp,sc     ,l)            incremental equivalent plastic strain
c      gamma    (dp,sc     ,l)            variable for solving incremental equivalent plastic strain
c      dgamma   (dp,sc     ,l)            correction of gamma
c      sigy_t   (dp,sc     ,l)            yield stress at beginnig of time increments
c      sigy     (dp,sc     ,l)            yield stress at end of time increment
c      young    (dp,sc     ,l)            Young's modulus
c      posn     (dp,sc     ,l)            Poiss's ratio
c      sigy0    (dp,sc     ,l)            initial yield stress
c      dsigdep  (dp,sc     ,l)            plastic slop
c      twoG     (dp,sc     ,l)            two time of shear moduli
c      funcf    (dp,sc     ,l)            nonlinear function to be solved for gamma
c      dFdep    (dp,sc     ,l)            derivative of nonlinear function over gamma
c
c --- temperary variables for solution purpose
c      i, j
c      c1, c2, c3, fratio
c      wk1(3), wk2(3), wk3(3), wk4(3) vector working arrays
c
      EXTERNAL         vmove, vzero, vapb1, vamb1,get_ElmData
      DOUBLE PRECISION sigElp(mcomp), dsdeEl(mcomp,mcomp), 
     &                 wk1(3), wk2(3), wk3(3), wk4(3)

      DOUBLE PRECISION var0, var1, var2, var3, var4, var5,
     &                 var6, var7

      INTEGER          i, j, k
      DOUBLE PRECISION pleq_t,  sigy_t , sigy,
     &                 cpleq, dpleq,   pleq,    twoG,    et,
     &                 young, posn,    sigy0,   dsigdep, 
     &                 gamma, dgamma,  dfdga,   dplga,   fratio,
     &                 funcFb,funcFb2, funcf,   dFdep,
     &                 c1, c2, c3, c4, c5
      DOUBLE PRECISION pv(3)
      data pv/TWOTHIRD, TWO, TWO/
c*************************************************************************
c
      keycut   = 0
      cutFactor = 0.d0
c *** equivalent plastic strain at beginning of time step
      pleq_t   = ustatev(ncomp+1)
      pleq     = pleq_t
c *** get Young's modulus and Poisson's ratio, initial yield stress and slope of stress-strain
      young    = prop(1)
      posn     = prop(2)
      sigy0    = prop(3)
      et       = prop(4)
c *** calculate plastic slope
      dsigdep  = young * et/(young - et)
      twoG     = young / (ONE+posn)
c *** define tsstif(1) since it is used for calculation of hourglass stiffness
      tsstif(1) = HALF * twoG
c
c *** calculate elastic stiffness matrix
c
      call vzero(dsdeEl(1,1), ncomp * ncomp)
      c1 = twoG * HALF
      dsdeEl (1,1) = young
      dsdeEl (2,2) = c1
      dsdeEl (3,3) = c1
      DO i = 1, ncomp
         wk3(i) = dsdeEl(i,i)
      END DO
c *** calculate predicted strain 
      call vmove(Strain(1), wk1(1), ncomp)
      call vapb1(wk1(1), dStrain(1), ncomp)
      call vamb1(wk1(1), ustatev(1), ncomp)

c
c *** get initial stress
      call vzero(sigi(1),ncomp)
      i = ncomp
      call get_ElmData ('ISIG', elemId,kDomIntPt, i, sigi)

c
c *** calculate the trial stress and 
c     copy elastic moduli dsdeEl to material Jacobian matrix
      call vmove(dsdeEl(1,1), dsdePl(1,1), ncomp * ncomp)
      do i=1,ncomp
         sigElp(i) = wk3(i) * wk1(i) + sigi(i)
      end do
c
      funcFb2 = ZERO
      DO i = 1, ncomp
        funcFb2 = funcFb2 + pv(i) * sigElp(i) * sigElp(i)
      END DO
      funcFb = sqrt(funcFb2)

c *** compute current yield stress
      sigy    = sigy0 + dsigdep * pleq
c
      fratio = funcFb/sigy - SQTWOTHIRD
c *** check for yielding
      IF (fratio .LE. -SMALL) GO TO 500
      sigy_t  = sigy

      DO i = 1, ncomp
         wk3(i) = wk3(i) * pv(i)
      END DO

      gamma    = ZERO
      dplga    = ZERO
      dfdga    = ZERO
      dpleq    = ZERO
      pleq     = pleq_t 

c *** Local New-Raphson procedure for solving the gamma and 
c     thus the incremental equivalent plastic strain
      DO k=1,NEWTON
         funcFb2 = ZERO
         dfdga   = ZERO
         DO j = 1 , ncomp
            c1 = ONE + gamma * wk3(j)
            c1 = ONE / c1
            c2 = sigElp(j) * c1
            wk4(j) = c2
            funcFb2 = funcFb2 + pv(j) * c2 * c2
            c2 = c2 * c2 * c1 * wk3(j) * pv(j)
            dfdga   = dfdga - c2
         END DO
         funcFb   = sqrt(funcFb2)
c ***    derivative of funcFb w.r.t. gamma
         dfdga   = dfdga / funcFb

c ***    calculate the incremental equivalent plastic strain
         dpleq    = gamma * SQTWOTHIRD * funcFb
c ***    update the total equivalent plastic strain
         pleq     = pleq_t + dpleq
c ***    current yield stress
         sigy     = sigy0 + dsigdep * pleq
c ***    calculate the residual
         funcf    = funcFb - SQTWOTHIRD * sigy
c ***    derivative of incremental equivalent plastic strain w.r.t. gamma
         dplga    = SQTWOTHIRD * (gamma * dfdga + funcFb)
c ***    derivative of residual function w.r.t. gamma
         dFdep    = dfdga - SQTWOTHIRD * dsigdep * dplga
c ***    correction of gamma
         dgamma   = -funcf / dFdep 
         gamma    = gamma   + dgamma
c ***    check for negative gamma
         gamma    = max (gamma, sqTiny)
         fratio   = funcf/ sigy
c
c ***    Check for convergence of local New-Raphson iteration
         IF (((abs(fratio) .LT. ONEDM05 ) .AND.
     &        (abs(dgamma) .LT. ONEDM02*gamma)) .OR.
     &       ((abs(fratio) .LT. ONEDM05 ) .AND.
     &        (dgamma      .LE. sqTiny  ) .AND.
     &        ( gamma      .LE. sqTiny  )))  GO TO 100

      END DO
c
c *** Uncovergence, set keycut to 1 for bisection/cutback the load increment
      keycut   = 1
      GO TO 990
 100  CONTINUE
c
c *** update stresses
      call vmove(wk4(1), stress(1), ncomp)

c *** calculate incremental plastic strain 
      DO j = 1, ncomp
         wk2(j) = gamma * pv(j) * wk4(j)
      END DO
c *** update plastic strains
      call vapb1(epsPl(1),wk2(1),ncomp)

c *** Update state variables
      ustatev(ncomp+1) = pleq
      do i=1,ncomp
         ustatev(i) = epsPl(i)
      end do

c *** update plastic work
      sedPl     = sedPl + HALF * (sigy_t+sigy) * dpleq

      c1     = TWOTHIRD * dsigdep
      c3     = c1 * funcFb2 / (ONE - c1 * gamma)
      DO j = 1 , ncomp
         c1 = ONE / (ONE + gamma * wk3(j))
         wk3(j) = wk3(j) * c1 / pv(j)
      END DO
      DO j = 1 , ncomp
         wk4(j) = wk4(j) * pv(j)
      END DO
      DO j = 1 , ncomp
         c3 = c3 + wk4(j) * wk4(j) * wk3(j)
      END DO
      DO j = 1 , ncomp
         wk4(j) = wk4(j) * wk3(j)
      END DO

      c3     = ONE / c3
      DO i=1,ncomp
         dsdePl(i,i) = wk3(i)
      END DO

c *** Calculate the plastic Jacobian

      DO i=1,ncomp
         DO j=1,ncomp
            dsdePl(i,j) =    dsdePl(i,j) - c3 * wk4(i) * wk4(j)
         END DO
      END DO

      goto 600

  500 continue

c *** Update stress in case of elastic/unloading
      call vmove(sigElp(1),stress(1),ncomp)

  600 continue
c *** elastic strain energy
      sedEl = ZERO
      DO i = 1 , ncomp
         sedEl = sedEl + stress(i)*(Strain(i)+dStrain(i)-epsPl(i))
      END DO
      sedEl = sedEl * HALF
      ustatev(nStatev) = funcFb / SQTWOTHIRD

 990  CONTINUE
c
      return
      end