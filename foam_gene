      program foamgen
c234567
      implicit none
C
      integer*2 idim, idir, iat, nat, itau, ntau
      integer*2 ia1, ia2, ia3, na1, na2, na3, i, m,n,j
      real*4 a(3,3), tau(3,100), crd(3,5000)
      real*4  x_c, y_c, z_c
      real*4 d, scal12, scal3, dxy, dz
c
c  C-C interatomic distance
c      d = 1.42
c
c  specify number of unit cells along the a1, a2, a3 direction
      write (6,200) 
 200  format (2x,'Enter number of unit cells ',
     &  'in the a1, a2, a3 direction'/)
      read (5,*) na1, na2, na3

      ntau = 15 
	open(1,file='matrix.txt')
        read(1,*) a(1,1),a(2,1),a(3,1) 
        read(1,*) a(1,2),a(2,2),a(3,2)
        read(1,*) a(1,3),a(2,3),a(3,3) 
        read(1,*) ntau 
        do i=1, ntau
	read(1,*)  x_c, y_c, z_c
                tau(1,i) = x_c
                tau(2,i) = y_c
                tau(3,i) = z_c
	end do
	close(1)

c
      nat = 0
c  generate lattice coordinates
      do 10 ia1=1,na1
      do 10 ia2=1,na2
      do 10 ia3=1,na3
c
      do 20 itau=1,ntau
      nat = nat + 1
c
      do 30 idir=1,3
      crd(idir,nat) = (ia1-1)*a(idir,1)
     &            +(ia2-1)*a(idir,2)
     &            +(ia3-1)*a(idir,3)
     &            +tau(idir,itau)
 30   continue
c
 20   continue
c
 10   continue
c
      open(unit=20,file='foam.xyz',form='formatted',status='unknown')
      write (20,*) nat
      write (20,*)
c 100  format (2x,i5/2x,'foam structure')
      do 50 iat=1,nat
      write (20,110) (crd(idir,iat),idir=1,3)
 110  format (2x,1hC,4x,f10.4,2x,f10.4,2x,f10.4)
c
 50   continue
c
      stop
      end program foamgen
