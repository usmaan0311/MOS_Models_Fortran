module Differentiation
      implicit none
      contains
             real*8 function diff(f,Vg,x,h)
                      implicit none
                      real*8,intent(in)::x,h,Vg
                      real*8::f
                      diff=( f(x + h,Vg) - f(x - h,Vg) )/(2*h)
                  end function diff
      end module Differentiation
