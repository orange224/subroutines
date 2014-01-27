








      !> @see 
      !!  George B. Arfken and Hans J. Weber, Mathematical Methods For Physicists
      !!  9.7 Nonhomogeneous Equation - Green's Function
      !!  Table 9.5 Green's functions. page 598
      !!  
      !> @see 
      !!  Duffy, D.G. 2001. Green's Functions with Applications
      !!  Chapter 5. Green's Functions for the Helmholtz Equation
      !!  
      !> @see 
      !!  Sommerfeld, A., 1912: Die Greensche Funktion der Schwingungsgleichung. 
      !!  Jahresber. Deutschen Math. - Veeereinung, 21. 309-353
      !!  
      !> @brief
      !!  g(rr|ss)
      !!  we have taken the temporal forcing to be $e^{-j \omega t}$ and $k_{0} = \omega/c$.
      
      function helm_2d (vp, sx, sy, sz, rx, ry, rz, omega) result (zg)
         use ifport
      
      implicit none
         real(kind=8), intent(in)  :: vp
         real(kind=8), intent(in)  :: sx, sy, sz
         real(kind=8), intent(in)  :: rx, ry, rz
         real(kind=8), intent(in)  :: omega
         
         include 'constants_pi.f90'
         complex(kind=8), parameter ::  im = dcmplx(0.d0,1.d0)
         complex(kind=8) :: zg;   real(kind=8) :: rr, k0
         
         ! omega .eq. 0 -> Poisson's equation
         if (omega .eq. 0.d0) then
            rr = dsqrt((rx-sx)**2 + (ry-sy)**2+ (rz-sz)**2)
            
            zg = -dlog(rr)/epi
            
            return
         
         ! omega .ne. 0 -> Helmholtz's equation
         else if (omega .gt. 0.d0) then
            rr = dsqrt((rx-sx)**2 + (ry-sy)**2+ (rz-sz)**2)
            
            k0 = omega/vp
            
            ! H^{1}_{0} () = J_{0}() + j Y_{0}()
            zg = dcmplx(dbesj0(rr*k0), dbesy0(rr*k0))
            
            zg = 0.25d0*im*zg
            
            return
         end if
         
      end function helm_2d
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
