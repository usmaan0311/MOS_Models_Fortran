module Psi
        use InitCond
      implicit none
      public::fun
      contains
             real*8 function fun(x,Vg)
                      implicit none
                     real*8::Vg
                      real*8, intent(in):: x
                      fun=Vfb + x + gamma*sqrt(abs(x) + kt*exp(-x/kt) - kt  + &
                              exp(-2*phif/kt)*(-abs(x) + kt*exp(x/kt) - kt)) - abs(Vg)
                end function fun
end module Psi
