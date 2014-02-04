      !> @see 
      !!  Duffy, D.G. 2001. Green's Functions with Applications
      !!  Chapter 3. Green's Functions for the Wave Equation
      !!  equation (3.5.11)
      
      function green3d (vp, sx, sy, sz, rx, ry, rz, tt, dt, t0) result (dg)
      
      implicit none
         real(kind=8), intent(in) :: vp
         real(kind=8), intent(in) :: sx, sy, sz
         real(kind=8), intent(in) :: rx, ry, rz
         real(kind=8), intent(in) :: tt, dt, t0
         
         include 'constants_pi.f90'
         real(kind=8) :: rr, dg
         
         rr = dsqrt((rx-sx)**2 + (ry-sy)**2 + (rz-sz)**2)
         dg = 0.d0
         
         if (tt-t0 .eq. rr/vp) then
            dg = 1.d0/(fpi*rr)
            return
            
         else if (tt-t0-0.99d0*dt .le. rr/vp .and. tt-t0+0.99d0*dt .gt. rr/vp) then
            dg = 1.d0/(fpi*rr)
            return
         end if
         
      end function green3d
      
