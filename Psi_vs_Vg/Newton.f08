module Newton      
        use Differentiation
        implicit none
        contains
         subroutine NR(f,Vg,ig,h, maxiter,error,iter,root)
                        implicit none
                        real*8::f,h,error,err,xr,Vg
                        real*8, intent(inout)::ig 
                        integer:: maxiter
                        real*8, intent(out)::root
                        integer, intent(out)::iter

                        if(f(ig,Vg).eq.0) then
                                print'(f10.8, "is the root of function")',ig
                        else
                                iter=0
                                err=error+0.5
                                do while((err>error) .and. (iter<maxiter))
                                
                                xr = ig - f(ig,Vg)/diff(f,Vg,ig,h)
                                iter=iter+1
                                err = abs( f(xr,Vg) - f(ig,Vg) )
                                ig=xr
                                end do
                        end if
                        root=xr
                        print '("value of root is ",f18.18," Converged in",i10," iterations ")',root,iter
                        print*,"Root is", xr
                        end subroutine NR

end module Newton

