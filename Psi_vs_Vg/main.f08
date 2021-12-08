program main
        use InitCond
      use Psi
      use Differentiation
      use Newton
      use WriteSol
      implicit none
      real*8:: h,error,Vg,dVg
      integer:: maxiter,Nh,Nl,i,u
      real*8, dimension(:), allocatable::Vgs,root,ig
      integer, dimension(:),allocatable::iter
      print*,"Enter value of Nh, Nl and Vg"
      read*,Nh,Nl,Vg
      allocate(Vgs(Nl:Nh),root(Nl:Nh),ig(Nl:Nh),iter(Nl:Nh))
        u=7
      h=1e-6
      error=1e-10
      maxiter=200000
      dVg=Vg/((Nh - Nl)-1)
      do i=Nl,Nh,1
      Vgs(i)=0.0
      Vgs(i)=i*dVg
      print*,"Vgs = ",Vgs(i)

      ig(i)=0.0
      ig(i)=IC(Vgs(i))

      call NR(fun,Vgs(i),ig(i),h,maxiter,error,iter(i),root(i))
      end do

      call WriteF(root,Vgs,(Nh-Nl)+1,u,"Psi.dat")
      call WriteF(real(iter,8),Vgs,(Nh-Nl)+1,u,"iter.dat")
      call WriteF(ig,Vgs,(Nh-Nl)+1,u,"InitGuess.dat")
      deallocate(Vgs,root,ig,iter)
      print*,"Successfully done!!!"
end program main

