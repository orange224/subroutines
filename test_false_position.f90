   
   module your_function
   implicit none
      
      interface           given_function
         module procedure given_function
      end interface       given_function
      
   contains
      
      function given_function (cc, f0) result (ff)
      implicit none
         real(kind=8), parameter :: gg = 980.0D0
         real(kind=8), parameter :: mm = 75000.0D0
         real(kind=8), parameter :: tt = 6.0D0
         real(kind=8), intent(in) :: cc
         real(kind=8), intent(in), optional :: f0
         real(kind=8) :: ff
         
         if (present(f0)) then
            ff = (gg*mm/cc)*(1.0D0 - dexp(-1.0D0*cc*tt/mm)) - f0
            return
         else 
            ff = (gg*mm/cc)*(1.0D0 - dexp(-1.0D0*cc*tt/mm))
            return
         end if
      end function
      
   end module your_function
   
   
   
   
   program main
      
      use your_function
      
   implicit none
      
      real(kind=8) :: cc_int, cc_tmp, cc_end
      real(kind=8) :: ff_int, ff_tmp, ff_end, f0
      real(kind=8) :: es
      integer(kind=8) :: sgn
      
      
      !> cc_int: end points, lower bound
      !> cc_end: end points, upper bound
      cc_int = 1.0D0
      cc_end = 100000.0D0
      cc_tmp = 0.0D0
      
      f0 = 3600.0D0
      ff_int = given_function (cc_int, f0)
      ff_end = given_function (cc_end, f0)
      
      es = 1.0D-9
      sgn = 0
      do
         cc_tmp = (ff_int*cc_end - ff_end*cc_int)/(ff_int - ff_end)
         ff_tmp = given_function (cc_tmp, f0)
         
         print "(6(1x,f24.12))", cc_int, cc_tmp, cc_end, ff_int, ff_tmp, ff_end
         
         if (dabs(cc_end-cc_int) .le. es*dabs(cc_end+cc_int)) then
            exit
         end if
         
         if (ff_tmp*ff_end .gt. 0.0D0) then
            cc_end = cc_tmp
            ff_end = ff_tmp
            if (sgn .eq. -1) ff_int = 0.5D0*ff_int
            sgn = -1
            
         else if (ff_tmp*ff_int .gt. 0.0D0) then
            cc_int = cc_tmp
            ff_int = ff_tmp
            
            if (sgn .eq. 1) ff_end = 0.5D0*ff_end
            sgn = 1
         else
            exit
         end if
      end do
      
   stop
   end program main
   
   
