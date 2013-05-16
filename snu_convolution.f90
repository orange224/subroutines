      !> @author 
      !!  orange224@gmail.com
      !!
      !> @todo
      !!  2013-05-16: debugged
      !!
      !> @todo
      !!  2012-03-06: created
      !!
      !> @brief
      !!  Basically, convolution of signals has three components: 
      !!  the input singal     (e.g., the source signature, in the context of petroleum seismology), 
      !!  the impulse response (e.g., the earth response to the delta-function source signature), 
      !!  and the output       (e.g., the seismic data).
      !!
      !> @see
      !!  Luc T. Ikelle and Lasse Amundsen., Introduction to Petroleum Seismology. 
      !!  Chapter 4. The Fourier representation of seismic signals. page 150-156. Convolution
      !! 
      !> @see 
      !!  E. Oran Brigham., The Fast Fourier Transform and its Applications.
      !!  Chapter 7. Discrete convolution and correlation. page 122. equation (7.6)
      !!
      !!  yy(1:nh+nx-1) = xx(1:nx)*hh(1:nh) 
      !!                = hh(1:nh)*xx(1:nx)
      !!
      !! @param yy(1:ny): output
      !! @param xx(1:nx): time-invariant system to its input
      !! @param hh(1:nh): impulse response
      
      module snu_convolution
      implicit none
      interface tconvolv
        module procedure tconvolv_1d
        module procedure tconvolv_1d_ierr
      end interface tconvolv
        
      contains
      
      subroutine  tconvolv_1d (yy, ny, xx, nx, hh, nh)
      implicit none
        real(kind=8), dimension(:), intent(out) :: yy
        real(kind=8), dimension(:), intent(in)  :: xx
        real(kind=8), dimension(:), intent(in)  :: hh
        
        integer(kind=4),            intent(in)  :: ny
        integer(kind=4),            intent(in)  :: nx
        integer(kind=4),            intent(in)  :: nh
        
        integer(kind=4) :: ii, jj, jint, jend
        real(kind=8)    :: sum
        integer(kind=4) :: nxh
        
        yy(1:ny) = 0.d0
        if (nx .ge. nh) then ! nh -> slide function
          jint = 1
          do ii = 1, nh
            jend = ii
            
            sum = 0.d0
            do jj = jint, jend
              sum = sum + hh(jj)*xx(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do ! do ii = 1, nh
          
          jend = nh
          do ii = nh+1, nx
            sum = 0.d0
            do jj = jint, jend
              sum = sum + hh(jj)*xx(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do ! do ii = 1, nh
          
          do  ii = nx+1, ny
            jint = ii-nx+1
            sum = 0.d0
            do jj = jint, jend
              sum = sum + hh(jj)*xx(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do
          
        else if (nx .lt. nh) then ! nx -> slide function
          jint = 1
          do ii = 1, nx
            jend = ii
            
            sum = 0.d0
            do jj = jint, jend
              sum = sum + xx(jj)*hh(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do ! do ii = 1, nh
          
          jend = nx
          do ii = nx+1, nh
            sum = 0.d0
            do jj = jint, jend
              sum = sum + xx(jj)*hh(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do ! do ii = 1, nh
          
          do  ii = nh+1, ny
            jint = ii-nh+1
            sum = 0.d0
            do jj = jint, jend
              sum = sum + xx(jj)*hh(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do
          
        end if
        
      return
      end subroutine tconvolv_1d
      
      subroutine  tconvolv_1d_ierr (yy, ny, xx, nx, hh, nh, ierr)
      implicit none
        real(kind=8), dimension(:), intent(out) :: yy
        real(kind=8), dimension(:), intent(in)  :: xx
        real(kind=8), dimension(:), intent(in)  :: hh
        
        integer(kind=4),            intent(in)  :: ny
        integer(kind=4),            intent(in)  :: nx
        integer(kind=4),            intent(in)  :: nh
        integer(kind=4),            intent(in)  :: ierr
        
        integer(kind=4) :: ii, jj, jint, jend, count
        real(kind=8)    :: sum
        integer(kind=4) :: nxh
        
        count = 0
        yy(1:ny) = 0.d0
        if (nx .ge. nh) then ! nh -> slide function
          print '(a)', 'slice function: hh(1:nh)'
          print '(a)', 'WeThePeople1'
          jint = 1
          do ii = 1, nh
            jend = ii
            
            sum = 0.d0
            do jj = jint, jend
              count = count+1
              print '(a,4(1x,i3))', 'count, y[ii], x[ii-jj+1]h[jj]', count, ii, ii-jj+1, jj
              sum = sum + hh(jj)*xx(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do ! do ii = 1, nh
          
          jend = nh
          print '(a)', 'WeThePeople2'
          do ii = nh+1, nx
            sum = 0.d0
            do jj = jint, jend
              count = count+1
              print '(a,4(1x,i3))', 'count, y[ii], x[ii-jj+1]h[jj]', count, ii, ii-jj+1, jj
              sum = sum + hh(jj)*xx(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do ! do ii = 1, nh
          
          print '(a)', 'WeThePeople3'
          do  ii = nx+1, ny
            jint = ii-nx+1
            sum = 0.d0
            do jj = jint, jend
              count = count+1
              print '(a,4(1x,i3))', 'count, y[ii], x[ii-jj+1]h[jj]', count, ii, ii-jj+1, jj
              sum = sum + hh(jj)*xx(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do
          
        else if (nx .lt. nh) then ! nx -> slide function
          print '(a)', 'slice function: xx(1:nx)'
          print '(a)', 'WeThePeople11'
          jint = 1
          do ii = 1, nx
            jend = ii
            
            sum = 0.d0
            do jj = jint, jend
              count = count+1
              print '(a,4(1x,i3))', 'count, y[ii], x[jj]h[ii-jj+1]', count, ii, ii-jj+1, jj
              sum = sum + xx(jj)*hh(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do ! do ii = 1, nh
          
          jend = nx
          print '(a)', 'WeThePeople12'
          do ii = nx+1, nh
            sum = 0.d0
            do jj = jint, jend
              count = count+1
              print '(a,4(1x,i3))', 'count, y[ii], x[jj]h[ii-jj+1]', count, ii, ii-jj+1, jj
              sum = sum + xx(jj)*hh(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do ! do ii = 1, nh
          
          print '(a)', 'WeThePeople13'
          do  ii = nh+1, ny
            jint = ii-nh+1
            sum = 0.d0
            do jj = jint, jend
              count = count+1
              print '(a,4(1x,i3))', 'count, y[ii], x[jj]h[ii-jj+1]', count, ii, ii-jj+1, jj
              sum = sum + xx(jj)*hh(ii-jj+1) ! convolve: multiply and accumulate
            end do
            yy(ii) = sum
          end do
          
        end if
        
      return
      end subroutine tconvolv_1d_ierr
      
      !> @author 
      !!  orange224@gmail.com
      !!
      !> @todo
      !!  2012-03-06: created
      !!
      ! subroutine tconvolv (y, ny, x, nx, h, nh)
      ! implicit none
      !   real(kind=8), dimension(:), intent(out) :: y
      !   real(kind=8), dimension(:), intent(in)  :: x
      !   real(kind=8), dimension(:), intent(in)  :: h
      !   integer(kind=4),            intent(in)  :: ny
      !   integer(kind=4),            intent(in)  :: nx
      !   integer(kind=4),            intent(in)  :: nh
      !   
      !   integer(kind=4) :: i, j, jint, jend
      !   real(kind=8)    :: sum
      !   
      !   y(1:ny) = 0.0d0
      !   do i = 1, ny
      !     jint = i - nh
      !     if (jint .lt. 1) then
      !       jint = 1
      !     end if
      !     
      !     jend = i
      !     if (jend .gt. nx) then
      !       jend = nx
      !     end if
      !     
      !     sum = 0.0d0
      !     do j = jint, jend
      !       sum = sum + x(j)*h(i-j+1)
      !     end do
      !     y(i) = sum
      !   end do ! do i = 1, ny
      !   
      !   return
      ! end subroutine tconvolv
      
      end module snu_convolution
      
      
      
      
      program main
        
        use snu_convolution
        
      implicit none
        
        ! example01       
        ! hh[nh] = 2, 1   
        ! xx[nx] = 3, 4, 5
        ! 
        !              ix=1         ix=2         ix=3   ix=4                                    
        ! yy[1] = hh[1]xx[1]           .            .     .   =  2*3    .     .   . =  6  ii=1  
        ! yy[2] = hh[2]xx[1] + hh[1]xx[2]           .     .   =  1*3 + 2*4    .   . = 11  ii=nh 
        ! yy[3] =         .  + hh[2]xx[2] + hh[1]xx[3]    .   =    .   1*4 + 2*5  , = 14  ii=nx 
        ! yy[4] =         .  +         .  + hh[2]xx[3]    .   =    .    ,    1*5  . =  5  ii=ny 
        integer(kind=4),parameter :: nx = 3
        integer(kind=4),parameter :: nh = 2
        real(kind=8),   dimension(1:nx) :: xx = (/ 3.d0, 4.d0, 5.d0 /)
        real(kind=8),   dimension(1:nh) :: hh = (/ 2.d0, 1.d0 /)
        
        ! integer(kind=4),parameter :: nx = 2
        ! integer(kind=4),parameter :: nh = 3
        ! real(kind=8),   dimension(1:nx) :: xx = (/ 2.d0, 1.d0 /)
        ! real(kind=8),   dimension(1:nh) :: hh = (/ 3.d0, 4.d0, 5.d0 /)
        ! ----------------------------------------------------------------------------------------------- example 01
        
        ! ----------------------------------------------------------------------------------------------- example 02
        ! example02
        ! hh[nh] = 3, 2, 1      
        ! xx[nx] = 1, 2, 2, 1, 1
        ! 
        !              ix=1         ix=2         ix=3         ix=4         ix=5   ix=6  ix=7             
        ! yy[1] = hh[1]xx[1]          .            .            .            .      .     .   =  3 ii=1  
        ! yy[2] = hh[2]xx[1] + hh[1]xx[2]          .            .            .      .     .   =  8       
        ! yy[3] = hh[3]xx[1] + hh[2]xx[2] + hh[1]xx[3]          .            .      .     .   = 11 ii=nh 
        ! yy[4] =    .         hh[3]xx[2] + hh[2]xx[3] + hh[1]xx[4]          .      .     .   =  9       
        ! yy[5] =    .            .         hh[3]xx[3] + hh[2]xx[4] + hh[1]xx[5]    .     .   =  7 ii=nx 
        ! yy[6] =    .            .            .         hh[3]xx[4] + hh[2]xx[5]    .     .   =  3       
        ! yy[7] =    .            .            .            .         hh[3]xx[5]    ,     ,   =  1 ii=ny 
        ! integer(kind=4),parameter :: nx = 5
        ! integer(kind=4),parameter :: nh = 3
        ! real(kind=8),   dimension(1:nx) :: xx = (/ 1.d0, 2.d0, 2.d0, 1.d0, 1.d0/)
        ! real(kind=8),   dimension(1:nh) :: hh = (/ 3.d0, 2.d0, 1.d0 /)
        
        ! integer(kind=4),parameter :: nx = 3
        ! integer(kind=4),parameter :: nh = 5
        ! real(kind=8),   dimension(1:nx) :: xx = (/ 3.d0, 2.d0, 1.d0 /)
        ! real(kind=8),   dimension(1:nh) :: hh = (/ 1.d0, 2.d0, 2.d0, 1.d0, 1.d0/)
        ! ----------------------------------------------------------------------------------------------- example 02
        
        integer(kind=4),parameter :: ny = nx+nh-1
        real(kind=8),   dimension(1:ny) :: yy = 0.d0
        
        call tconvolv (yy(1:ny), ny, xx(1:nx), nx, hh(1:nh), nh)
        
        ! for debugging
        ! call tconvolv (yy(1:ny), ny, xx(1:nx), nx, hh(1:nh), nh, 8807224)
        
        print '(8(1x,f6.3))', yy(1:ny)
        
        stop
      end program main
      
      
