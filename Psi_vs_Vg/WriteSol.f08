module WriteSol
      implicit none
      contains
              subroutine WriteF(psi,v,n,u,fname)
                      implicit none
                      integer, intent(in)::n
                      integer::u, ios,i
                      real*8, dimension(0:n)::psi,v
                      character(len=*)::fname
                      ios=3
                      open(unit=u,file=fname,action='write', iostat=ios)
                      if(ios .eq. 0) then
                      do i=0,n,1
                      write(u,*) v(i),psi(i)
                      end do
                      close(u)
              end if
              end subroutine WriteF
end module WriteSol


