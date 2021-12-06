PROGRAM mos
        IMPLICIT NONE
        ! parameter declaration
      REAL, PARAMETER ::q=1.6e-19
      REAL, PARAMETER::Epsilon0=8.854e-14
      REAL, PARAMETER::EpsilonS=11.4*Epsilon0
      REAL, PARAMETER::Epsilonox=3.9*Epsilon0
      REAL, PARAMETER:: W=5.0e-4
      REAL, PARAMETER::L=2.0e-4
      REAL, PARAMETER::tox=2.0e-7
      REAL, PARAMETER::Na=1.0E+17
      REAL, PARAMETER::ni=1.0E+10
      REAL, PARAMETER::kt=0.026
      REAL, PARAMETER::MU=1000
      REAL:: Cox,Vsb,Vfb,Vgs,Vds,dVd,gamma,phi0,Vt,dVg
      INTEGER::u,i,j,ios
      INTEGER::N

      ! Id Vd and Vg array

      REAL, DIMENSION(:), allocatable ::Vd,Id
      REAL, DIMENSION(0:4)::Vg_Array
      CHARACTER(len=8), DIMENSION(0:4)::FILE_NAME
      INTEGER, DIMENSION(0:4):: UNITS
      UNITS=(/1,2,3,4,7/)
      FILE_NAME=(/'IV1.dat','IV2.dat','IV3.dat','IV4.dat','IV5.dat'/)
      print*,'Enter the value of N, Vgs, Vds'
      read(*,*) N,Vgs,Vds
      allocate(Vd(0:N),Id(0:N))
      Cox=Epsilonox/tox
      gamma=sqrt(2*q*EpsilonS*Na)/(Cox)
      Vsb=0.0
      Vfb=0.21
      phi0 = 2*kt*log(Na/ni)
      dVd = Vds/(N-1)
      dVg = Vgs/4
      
        !filling Vd array

     Vd_fill: do i=0,N,1
        Vd(i)=0.0
        Vd(i)=dVd*i
      end do Vd_fill

      print'(/,10x,"Gamma = ",f10.6)',gamma
      
      Vt = Vfb + phi0 + gamma*sqrt(phi0 + Vsb)
      print '("Threshold voltage, Vt :", 5x,f10.5 )',Vt

      !filling Vg array  

     Vg_fill: do i=0,4,1
         Vg_Array(i)=0.0
        Vg_Array(i)=dVg*i + Vt + 0.1
      end do Vg_fill
      ! Id Vd calculation for a particular Vg  

    Calc_Id_Vd_Vg:do j=0,4,1

           Calc_Id_Vd: do i=0,N,1
                Id(i)=0.0
                Vd(i)=Vd(i)
        Id_Calc:if (Vd(i) < (Vg_Array(j) - Vt)) then
                         Id(i) = mu*Cox*(W/L)*((Vg_Array(j) - Vt)*Vd(i) - (Vd(i)**2)/2 )
                else

                         Id(i) = mu*Cox*(W/(2*L))*((Vg_Array(j) - Vt)**2)
                end if Id_Calc
          end do Calc_Id_Vd
                print*,'file:',FILE_NAME(j)
                open(UNITS(j),file=FILE_NAME(j),iostat=ios,status='new',action='write', form='formatted')
        if (ios .eq. 0) then
              do i=0,N,1
                write(UNITS(j),*) Vd(i), Id(i) ! unformatted
              end do
              close(UNITS(j))
        end if
      end do Calc_Id_Vd_Vg
 deallocate(Vd,Id)
 print*,"Done with the program!!!!!"
end PROGRAM mos

