module InitCond
        implicit none
        real*8, private, parameter::Epsilon0=8.854e-14
        real*8, private, parameter::Epsilonox=Epsilon0*3.9
        real*8, private, parameter::EpsilonS=Epsilon0*11.4
        real*8, private, parameter::tox=2e-7
        real*8, private, parameter::Na=1e+17
        real*8, private, parameter::ni=1e+10
        real*8, private, parameter::q=1.6e-19
        real*8, private, parameter::Cox=Epsilonox/tox
        
        real*8, public, parameter::kt=0.026
        real*8, public::Vfb=0.21
        real*8, public, parameter::phif=kt*log(Na/ni)
        real*8, public, parameter::gamma=sqrt(2*q*Na*EpsilonS)/Cox
        public::IC
        contains
                real*8 function IC(Vg)
                        implicit none
                        real*8, intent(in)::Vg
                        real*8::Ig1,Ig2,Ig3
                        if(Vg .lt. 0) then
                            Ig1= ((-gamma/2) + sqrt(abs( ((gamma/2)**2) - (Vfb - abs(Vg)) )) )**2
                            Ig3=-2*kt*log(abs(abs(Vg) - Vfb)/(gamma*sqrt(kt)))
                            IC=min(Ig1,Ig3)
                else
                            Ig2=2*( phif + kt*log(abs(abs(Vg) - Vfb)/(gamma*sqrt(kt)) )  )
                            Ig1= ((-gamma/2) + sqrt(abs( ((gamma/2)**2) - (Vfb - abs(Vg)) )) )**2
                            IC=min(Ig1,Ig2)
                    end if

                end function IC
end module InitCond

