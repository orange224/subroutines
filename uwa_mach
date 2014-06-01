      !> Machine epsilon gives an upper bound on the relative error due to rounding in floating point arithmetic
      !> The quantity is also called machine epsilon or unit roundoff
      
      MODULE uwa_mach
      IMPLICIT NONE
         
         INTEGER(KIND=4), PARAMETER, PRIVATE :: DP = SELECTED_REAL_KIND(15,307)
         
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I01MACH =          5   ! I1MACH(01) = the standard input unit.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I02MACH =          6   ! I1MACH(02) = the standard output unit.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I03MACH =          7   ! I1MACH(03) = the standard punch unit.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I04MACH =          6   ! I1MACH(04) = the standard error message unit.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I05MACH =         32   ! I1MACH(05) = the number of bits per integer storage unit.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I06MACH =          4   ! I1MACH(06) = the number of characters per integer storage unit.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I07MACH =          2   ! I1MACH(07) = A, the base.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I08MACH =         31   ! I1MACH(08) = S, the number of base A digits.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I09MACH = 2147483647   ! I1MACH(09) = A**S-1, the largest integer.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I10MACH =          2   ! I1MACH(10) = B, the base.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I14MACH =         53   ! I1MACH(14) = T, the number of base B digits.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I15MACH =      -1021   ! I1MACH(15) = EMIN, the smallest exponent E.
         INTEGER(KIND=4), PARAMETER, PUBLIC :: I16MACH =       1024   ! I1MACH(16) = EMAX, the largest exponent E.
         
         REAL(KIND=8), PARAMETER, PRIVATE :: BB = RADIX(1.0D+0)
         REAL(KIND=8), PARAMETER, PUBLIC  :: D1MACH = BB**(MINEXPONENT(1.0D+0)-1.0D+0)                             ! D1MACH(1) = BB**(EMIN-1),               the smallest positive magnitude.
         REAL(KIND=8), PARAMETER, PUBLIC  :: D2MACH = BB**(MAXEXPONENT(1.0D+0)*(1.0D+0 - BB**(-DIGITS(1.0D+0))))   ! D1MACH(2) = BB**(EMAX*(1 - BB**(-TT))), the largest magnitude.
         REAL(KIND=8), PARAMETER, PUBLIC  :: D3MACH = BB**(-DIGITS(1.0D+0))                                        ! D1MACH(3) = BB**(-TT),                  the smallest relative spacing.
         REAL(KIND=8), PARAMETER, PUBLIC  :: D4MACH = BB**(1.0D+0-DIGITS(1.0D+0))                                  ! D1MACH(4) = BB**(1-TT),                 the largest relative spacing.
         REAL(KIND=8), PARAMETER, PUBLIC  :: D5MACH = DLOG10(BB)                                                   ! D1MACH(5) = LOG10(BB)
         
         REAL(KIND=8), PARAMETER, PUBLIC  :: machEPS = EPSILON(1.0_DP)
         
      END MODULE uwa_mach
      
      
      
      PROGRAM main
         USE uwa_mach
      
      IMPLICIT NONE
         REAL(KIND=8) :: EPS = 1.0D+0
         
         REAL(KIND=8) :: D1MA = 4.450147717014403D-308  ! D1MACH(1) = B**(EMIN-1), the smallest positive magnitude.
         REAL(KIND=8) :: D2MA = 8.988465674311579D+307  ! D1MACH(2) = B**EMAX*(1 - B**(-T)), the largest magnitude.
         REAL(KIND=8) :: D3MA = 1.110223024625157D-016  ! D1MACH(3) = B**(-T), the smallest relative spacing.
         REAL(KIND=8) :: D4MA = 2.220446049250313D-016  ! D1MACH(4) = B**(1-T), the largest relative spacing.
         REAL(KIND=8) :: D5MA = 0.301029995663981D+000  ! D1MACH(5) = LOG10(B)
         
         PRINT '(a)', "!> check the machine epsilon ..."
         DO WHILE ((1.0D+0 + EPS / 2.0D+0) .GT. 1.0D+0)
            EPS = EPS / 2.0D+0
         END DO
         PRINT *, EPS
         PRINT *, machEPS
         PRINT *, ' '
         
         
         !> check the smallest positive magnitude
         PRINT '(a)', "!> check the smallest positive magnitude ..."
         PRINT *, D1MACH
         PRINT *, D1MA
         PRINT *, MIN(D1MA, D1MACH)
         PRINT *, ' '
         
         
         ! check the largest positive magnitude
         PRINT '(a)', "!> check the largest positive magnitude ..."
         PRINT *, D2MACH
         PRINT *, D2MA
         PRINT *, MAX(D2MA, D2MACH)
         PRINT *, ' '
         
         
         ! check the smallest relative spacing
         PRINT '(a)', "!> check the smallest relative spacing ..."
         PRINT *, D3MACH
         PRINT *, D3MA
         PRINT *, MIN(D3MA, D3MACH)
         PRINT *, ' '
         
         
         ! check the largest relative spacing
         PRINT '(a)', "!> check the largest relative spacing ..."
         PRINT *, D4MACH
         PRINT *, D4MA
         PRINT *, MIN(D4MA, D4MACH)
         PRINT *, ' '
         
         
         PRINT '(a)', "!> check the LOG10(B) ..."
         PRINT *, D5MACH
         PRINT *, D5MA
         PRINT *, MIN(D5MA, D5MACH)
         PRINT *, ' '
         
         
         PRINT *, HUGE(1) - 2147483647
         PRINT *, MINEXPONENT(1.0D+0)
         PRINT *, MAXEXPONENT(1.0D+0)
         
         STOP
      END PROGRAM main
      
      
      
      
