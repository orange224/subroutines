      !> @see 
      !!  Duffy, D.G. 2001. Green's Functions with Applications
      !!  Chapter 3. Green's Functions for the Wave Equation
      !!  equation (3.5.11)
      
      FUNCTION TD3DGG (vp, sx, sy, sz, gx, gy, gz, tt, t0, dt) RESULT (gg)
      IMPLICIT NONE
         REAL(KIND=8), INTENT(IN) :: vp
         REAL(KIND=8), INTENT(IN) :: sx, sy, sz
         REAL(KIND=8), INTENT(IN) :: gx, gy, gz
         REAL(KIND=8), INTENT(IN) :: tt, t0, dt
         OPTIONAL                 :: dt
         
         INCLUDE 'uwa_pi.f90'
         REAL(KIND=8) :: rr, r0, r1, gg
         
         rr = DSQRT((gx-sx)**2 + (gy-sy)**2)
         r0 = DSQRT((gz-sz)**2 + rr**2)      ! real source
         
         gg = 0.0D+0
         IF (vp*(tt-t0) .LT. r0) gg = 0.0D+0
         IF (vp*(tt-t0) .GT. r0) gg = 0.0D+0
         IF (vp*(tt-t0) .EQ. r0) gg = 1.0D+0
         
         IF (PRESENT(dt)) THEN
            gg = 0.0D+0
            IF (r0/vp .GT. tt-t0) gg = 0.0D+0
            IF (r0/vp .LT. tt-t0) gg = 0.0D+0
            IF (r0/vp .GT. tt-t0-0.5D+0*dt .AND. r0/vp .LE. tt-t0+0.5D+0*dt) gg = 1.0D+0
         END IF
         gg = gg*(1.0D+0/(fpi*r0))
         
         RETURN
      END FUNCTION TD3DGG
      
