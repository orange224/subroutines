      module mod_rock; implicit none
         
         ! -------------------------------------------------------------------------------------------- poisson's ratio
         ! vp, vs -> poisson's ratio
         ! gm = 0.5*(vp**2 - 2*vs**2)/(vp**2 - vs**2)
         interface           ps2gm   
            module procedure ps2gm_r0
            module procedure ps2gm_r1
            module procedure ps2gm_r2
            module procedure ps2gm_d0
            module procedure ps2gm_d1
            module procedure ps2gm_d2
         end interface       ps2gm   
         
         
         ! -------------------------------------------------------------------------------------------------- impedence
         ! vp, ds -> p-wave impedence
         ! zp = ds*vp
         interface           vp2zp   
            module procedure vp2zp_r0
            module procedure vp2zp_r1
            module procedure vp2zp_r2
            module procedure vp2zp_d0
            module procedure vp2zp_d1
            module procedure vp2zp_d2
         end interface       vp2zp   
         
         ! p-wave impedence, ds -> vp
         ! vp = zp/ds
         interface           zp2vp   
            module procedure zp2vp_r0
            module procedure zp2vp_r1
            module procedure zp2vp_r2
            module procedure zp2vp_d0
            module procedure zp2vp_d1
            module procedure zp2vp_d2
         end interface       zp2vp   
         
         ! vs, ds -> s-wave impedence
         ! zs = ds*vs
         interface           vs2zp   
            module procedure vp2zp_r0
            module procedure vp2zp_r1
            module procedure vp2zp_r2
            module procedure vp2zp_d0
            module procedure vp2zp_d1
            module procedure vp2zp_d2
         end interface       vs2zp   
         
         ! s-wave impedence, ds -> vs
         ! vs = zs/ds
         interface           zs2vs   
            module procedure zp2vp_r0
            module procedure zp2vp_r1
            module procedure zp2vp_r2
            module procedure zp2vp_d0
            module procedure zp2vp_d1
            module procedure zp2vp_d2
         end interface       zs2vs
         
         
         
         ! ---------------------------------------------------------------------------------------------------- modulus
         ! vp, ds -> p-wave modulus
         ! pm = ds*(vp**2)
         interface           vp2pm   
            module procedure vp2pm_r0
            module procedure vp2pm_r1
            module procedure vp2pm_r2
            module procedure vp2pm_d0
            module procedure vp2pm_d1
            module procedure vp2pm_d2
         end interface       vp2pm   
         
         ! p-wave modulus, ds -> vp
         ! vp = sqrt(pm/ds)
         interface           pm2vp   
            module procedure pm2vp_r0
            module procedure pm2vp_r1
            module procedure pm2vp_r2
            module procedure pm2vp_d0
            module procedure pm2vp_d1
            module procedure pm2vp_d2
         end interface       pm2vp   
         
         ! vs, ds -> s-wave modulus, or shear modulus
         ! sm = ds*(vs**2)
         interface           vs2sm   
            module procedure vp2pm_r0
            module procedure vp2pm_r1
            module procedure vp2pm_r2
            module procedure vp2pm_d0
            module procedure vp2pm_d1
            module procedure vp2pm_d2
         end interface       vp2pm   
         
         ! s-wave modulus or shear modulus, ds -> vs
         ! vs = sqrt(sm/ds)
         interface           sm2vs   
            module procedure pm2vp_r0
            module procedure pm2vp_r1
            module procedure pm2vp_r2
            module procedure pm2vp_d0
            module procedure pm2vp_d1
            module procedure pm2vp_d2
         end interface       sm2vs   
         
         
         
         
         
         ! ------------------------------------------------------------ bulk and shear modulus from vp, vs, and density
         !> @see: http://en.wikzpedia.org/wiki/shear_modulus
         !!  shear modulus or modulus of regidity
         !!  defined as the ratio of shear stress and the shear strain.
         !!
         !> @see: http://en.wikzpedia.org/wiki/bulk_modulus
         !!  the inverse of the bulk modulus gives a substance's compressibility.
         
         ! vp, vs, ds -> bulk and shear modulus
         ! gg = ds*vs**2
         ! kk = ds*vp**2 - 4*gg/3
         interface           ps2bs   
            module procedure ps2bs_r0
            module procedure ps2bs_r1
            module procedure ps2bs_r2
            module procedure ps2bs_d0
            module procedure ps2bs_d1
            module procedure ps2bs_d2
         end interface       ps2bs   
         
         interface           bs2ps   
            module procedure bs2ps_r0
            module procedure bs2ps_r1
            module procedure bs2ps_r2
            module procedure bs2ps_d0
            module procedure bs2ps_d1
            module procedure bs2ps_d2
         end interface       bs2ps   
         
      contains
         
         
         ! -------------------------------------------------------------------------------------------- poisson's ratio
         !> @see:
         !!  http://www.glossary.oilfield.slb.com/en/Terms/p/poissons_ratio.aspx
         !> @param[vp]: p-wave velocity
         !> @param[vs]: s-wave velocity
         !> @param[gm]: poisson's ratio
         !! gas reservoir: gm ~ 0.
         !! carbonate rocks: gm ~ 0.3
         !! sandstone : gm ~ 0.2
         !! shale : gm > 0.3
         !! coal : gm ~ 0.4
         subroutine ps2gm_r0 (vp, vs, gm)
         implicit none
            real(kind=4), intent(in)  :: vp
            real(kind=4), intent(in)  :: vs
            real(kind=4), intent(out) :: gm
            
            gm = 0.e0
            if (vp .ne. vs) gm = 0.5e0*(vp**2 - 2.e0*vs**2)/(vp**2 - vs**2)
            return
         end subroutine ps2gm_r0
         
         subroutine ps2gm_r1 (vp, vs, gm, nz)
         implicit none
            real(kind=4), dimension(:), intent(in)  :: vp
            real(kind=4), dimension(:), intent(in)  :: vs
            real(kind=4), dimension(:), intent(out) :: gm
            integer(kind=4),            intent(in)  :: nz
            integer(kind=4) :: iz
            
            do iz = 1, nz
               call ps2gm_r0 (vp(iz), vs(iz), gm(iz))
            end do
            
            return
         end subroutine ps2gm_r1
         
         subroutine ps2gm_r2 (vp, vs, gm, nz, nx)
         implicit none
            real(kind=4), dimension(:,:), intent(in)  :: vp
            real(kind=4), dimension(:,:), intent(in)  :: vs
            real(kind=4), dimension(:,:), intent(out) :: gm
            integer(kind=4),              intent(in)  :: nz, nx
            integer(kind=4) :: ix
            
            do ix = 1, nx
               call ps2gm_r1 (vp(1:nz,ix), vs(1:nz,ix), gm(1:nz,ix), nx)
            end do
            
            return
         end subroutine ps2gm_r2
         
         
         subroutine ps2gm_d0 (vp, vs, gm)
         implicit none
            real(kind=8), intent(in)  :: vp
            real(kind=8), intent(in)  :: vs
            real(kind=8), intent(out) :: gm
            
            gm = 0.e0
            if (vp .ne. vs) gm = 0.5d0*(vp**2 - 2.d0*vs**2)/(vp**2 - vs**2)
            return
         end subroutine ps2gm_d0
         
         subroutine ps2gm_d1 (vp, vs, gm, nz)
         implicit none
            real(kind=8), dimension(:), intent(in)  :: vp
            real(kind=8), dimension(:), intent(in)  :: vs
            real(kind=8), dimension(:), intent(out) :: gm
            integer(kind=4),            intent(in)  :: nz
            integer(kind=4) :: iz
            
            do iz = 1, nz
               call ps2gm_d0 (vp(iz), vs(iz), gm(iz))
            end do
            
            return
         end subroutine ps2gm_d1
         
         subroutine ps2gm_d2 (vp, vs, gm, nz, nx)
         implicit none
            real(kind=8), dimension(:,:), intent(in)  :: vp
            real(kind=8), dimension(:,:), intent(in)  :: vs
            real(kind=8), dimension(:,:), intent(out) :: gm
            integer(kind=4),              intent(in)  :: nz, nx
            integer(kind=4) :: ix
            
            do ix = 1, nx
               call ps2gm_d1 (vp(1:nz,ix), vs(1:nz,ix), gm(1:nz,ix), nx)
            end do
            
            return
         end subroutine ps2gm_d2
         
         
         
         
         
         ! -------------------------------------------------------------------------------------------------- impedence
         !> @see:
         ! p-wave impedance
         ! s-wave impedance
         subroutine vp2zp_r0 (vp, ds, zp)
         implicit none
            real(kind=4), intent(in)  :: vp
            real(kind=4), intent(in)  :: ds
            real(kind=4), intent(out) :: zp
            
            zp = ds*vp
            return
         end subroutine vp2zp_r0
         
         subroutine vp2zp_r1 (vp, ds, zp, nz)
         implicit none
            real(kind=4), dimension(:), intent(in)  :: vp
            real(kind=4), dimension(:), intent(in)  :: ds
            real(kind=4), dimension(:), intent(out) :: zp
            integer(kind=4),            intent(in)  :: nz
            
            zp(1:nz) = ds(1:nz)*vp(1:nz)
            return
         end subroutine vp2zp_r1
         
         subroutine vp2zp_r2 (vp, ds, zp, nz, nx)
         implicit none
            real(kind=4), dimension(:,:), intent(in)  :: vp
            real(kind=4), dimension(:,:), intent(in)  :: ds
            real(kind=4), dimension(:,:), intent(out) :: zp
            integer(kind=4),              intent(in)  :: nz,nx
            
            zp(1:nz,1:nx) = ds(1:nz,1:nx)*vp(1:nz,1:nx)
            return
         end subroutine vp2zp_r2
         
         subroutine vp2zp_d0 (vp, ds, zp)
         implicit none
            real(kind=8), intent(in)  :: vp
            real(kind=8), intent(in)  :: ds
            real(kind=8), intent(out) :: zp
            
            zp = ds*vp
            return
         end subroutine vp2zp_d0
         
         subroutine vp2zp_d1 (vp, ds, zp, nz)
         implicit none
            real(kind=8), dimension(:), intent(in)  :: vp
            real(kind=8), dimension(:), intent(in)  :: ds
            real(kind=8), dimension(:), intent(out) :: zp
            integer(kind=4),            intent(in)  :: nz
            
            zp(1:nz) = ds(1:nz)*vp(1:nz)
            return
         end subroutine vp2zp_d1
         
         subroutine vp2zp_d2 (vp, ds, zp, nz, nx)
         implicit none
            real(kind=8), dimension(:,:), intent(in)  :: vp
            real(kind=8), dimension(:,:), intent(in)  :: ds
            real(kind=8), dimension(:,:), intent(out) :: zp
            integer(kind=4),              intent(in)  :: nz,nx
            
            zp(1:nz,1:nx) = ds(1:nz,1:nx)*vp(1:nz,1:nx)
            return
         end subroutine vp2zp_d2
         
         
         
         !> @see:
         ! p-wave impedance
         ! s-wave impedance
         subroutine zp2vp_r0 (vp, ds, zp)
         implicit none
            real(kind=4), intent(out) :: vp
            real(kind=4), intent(in)  :: ds
            real(kind=4), intent(in)  :: zp
            
            vp = zp/ds
            return
         end subroutine zp2vp_r0
         
         subroutine zp2vp_r1 (vp, ds, zp, nz)
         implicit none
            real(kind=4), dimension(:), intent(out) :: vp
            real(kind=4), dimension(:), intent(in)  :: ds
            real(kind=4), dimension(:), intent(in)  :: zp
            integer(kind=4),            intent(in)  :: nz
            
            vp(1:nz) = zp(1:nz)/ds(1:nz)
            return
         end subroutine zp2vp_r1
         
         subroutine zp2vp_r2 (vp, ds, zp, nz, nx)
         implicit none
            real(kind=4), dimension(:,:), intent(out) :: vp
            real(kind=4), dimension(:,:), intent(in)  :: ds
            real(kind=4), dimension(:,:), intent(in)  :: zp
            integer(kind=4),              intent(in)  :: nz,nx
            
            vp(1:nz,1:nx) = zp(1:nz,1:nx)/ds(1:nz,1:nx)
            return
         end subroutine zp2vp_r2
         
         subroutine zp2vp_d0 (vp, ds, zp)
         implicit none
            real(kind=8), intent(out) :: vp
            real(kind=8), intent(in)  :: ds
            real(kind=8), intent(in)  :: zp
            
            vp = zp/ds
            return
         end subroutine zp2vp_d0
         
         subroutine zp2vp_d1 (vp, ds, zp, nz)
         implicit none
            real(kind=8), dimension(:), intent(out) :: vp
            real(kind=8), dimension(:), intent(in)  :: ds
            real(kind=8), dimension(:), intent(in)  :: zp
            integer(kind=4),            intent(in)  :: nz
            
            vp(1:nz) = zp(1:nz)/ds(1:nz)
            return
         end subroutine zp2vp_d1
         
         subroutine zp2vp_d2 (vp, ds, zp, nz, nx)
         implicit none
            real(kind=8), dimension(:,:), intent(out) :: vp
            real(kind=8), dimension(:,:), intent(in)  :: ds
            real(kind=8), dimension(:,:), intent(in)  :: zp
            integer(kind=4),              intent(in)  :: nz, nx
            
            vp(1:nz,1:nx) = zp(1:nz,1:nx)/ds(1:nz,1:nx)
            return
         end subroutine zp2vp_d2
         
         
         
         
         
         
         
         
         
         
         ! ---------------------------------------------------------------------------------------------------- modulus
         !> @see: http://en.wikzpedia.org/wiki/shear_modulus
         ! shear modulus or modulus of regidity
         ! defined as the ratio of shear stress and the shear strain
         subroutine vp2pm_r0 (vp, ds, pm)
         implicit none 
            real(kind=4), intent(in)  :: vp
            real(kind=4), intent(in)  :: ds
            real(kind=4), intent(out) :: pm
            
            pm = ds*(vp**2)
            return
         end subroutine vp2pm_r0
         
         subroutine vp2pm_r1 (vp, ds, pm, nz)
         implicit none 
            real(kind=4), dimension(:), intent(in)  :: vp
            real(kind=4), dimension(:), intent(in)  :: ds
            real(kind=4), dimension(:), intent(out) :: pm
            integer(kind=4),            intent(in)  :: nz
            
            pm(1:nz) = ds(1:nz)*(vp(1:nz)**2)
            return
         end subroutine vp2pm_r0
         
         subroutine vp2pm_r2 (vp, ds, pm, nz, nx)
         implicit none
            real(kind=4), dimension(:,:), intent(in)  :: vp
            real(kind=4), dimension(:,:), intent(in)  :: ds
            real(kind=4), dimension(:,:), intent(out) :: pm
            integer(kind=4),              intent(in)  :: nz,nx
            
            pm(1:nz,1:nx) = ds(1:nz,1:nx)*(vp(1:nz,1:nx)**2)
            return
         end subroutine vp2pm_r2
         
         subroutine vp2pm_d0 (vp, ds, pm)
         implicit none
            real(kind=8), intent(in)  :: vp
            real(kind=8), intent(in)  :: ds
            real(kind=8), intent(out) :: pm
            
            pm = ds*(vp**2)
            return
         end subroutine vp2pm_d0
         
         subroutine vp2pm_d1 (vp, ds, pm, nz)
         implicit none
            real(kind=8), dimension(:), intent(in)  :: vp
            real(kind=8), dimension(:), intent(in)  :: ds
            real(kind=8), dimension(:), intent(out) :: pm
            integer(kind=4),            intent(in)  :: nz
            
            pm(1:nz) = ds(1:nz)*(vp(1:nz)**2)
            return
         end subroutine vp2pm_d1
         
         subroutine vp2pm_d2 (vp, ds, pm, nz, nx)
         implicit none
            real(kind=8), dimension(:,:), intent(in)  :: vp
            real(kind=8), dimension(:,:), intent(in)  :: ds
            real(kind=8), dimension(:,:), intent(out) :: pm
            integer(kind=4),              intent(in)  :: nz,nx
            
            pm(1:nz,1:nx) = ds(1:nz,1:nx)*(vp(1:nz,1:nx)**2)
            return
         end subroutine vp2pm_d2
         
         
         
         subroutine pm2vp_r0 (vp, ds, pm)
         implicit none
            real(kind=4), intent(out) :: vp
            real(kind=4), intent(in)  :: ds
            real(kind=4), intent(in)  :: pm
            
            vp = sqrt(pm/ds)
            return
         end subroutine pm2vp_r0
         
         subroutine pm2vp_r1 (vp, ds, pm, nz)
         implicit none
            real(kind=4), dimension(:), intent(out) :: vp
            real(kind=4), dimension(:), intent(in)  :: ds
            real(kind=4), dimension(:), intent(in)  :: pm
            integer(kind=4),            intent(in)  :: nz
            
            vp(1:nz) = sqrt(pm(1:nz)/ds(1:nz))
            return
         end subroutine pm2vp_r1
         
         subroutine pm2vp_r2 (vp, ds, pm, nz, nx)
         implicit none
            real(kind=4), dimension(:,:), intent(out) :: vp
            real(kind=4), dimension(:,:), intent(in)  :: ds
            real(kind=4), dimension(:,:), intent(in)  :: pm
            integer(kind=4),              intent(in)  :: nz,nx
            
            vp(1:nz,1:nx) = sqrt(pm(1:nz,1:nx)/ds(1:nz,1:nx))
            return
         end subroutine pm2vp_r2
         
         subroutine pm2vp_d0 (vp, ds, pm)
         implicit none
            real(kind=8), intent(out) :: vp
            real(kind=8), intent(in)  :: ds
            real(kind=8), intent(in)  :: pm
            
            vp = dsqrt(pm/ds)
            return
         end subroutine pm2vp_d0
         
         subroutine pm2vp_d1 (vp, ds, pm, nz)
         implicit none
            real(kind=8), dimension(:), intent(out) :: vp
            real(kind=8), dimension(:), intent(in)  :: ds
            real(kind=8), dimension(:), intent(in)  :: pm
            integer(kind=4),            intent(in)  :: nz
            
            vp(1:nz) = dsqrt(pm(1:nz)/ds(1:nz))
            return
         end subroutine pm2vp_d1
         
         subroutine pm2vp_d2 (vp, ds, pm, nz, nx)
         implicit none
            real(kind=8), dimension(:,:), intent(out) :: vp
            real(kind=8), dimension(:,:), intent(in)  :: ds
            real(kind=8), dimension(:,:), intent(in)  :: pm
            integer(kind=4),              intent(in)  :: nz, nx
            
            vp(1:nz,1:nx) = dsqrt(pm(1:nz,1:nx)/ds(1:nz,1:nx))
            return
         end subroutine pm2vp_d2
         
         
         
         
         
         
         
         
         
         
         ! ------------------------------------------------------------------------------------- bulk and shear modulus
         !> @see: http://en.wikzpedia.org/wiki/shear_modulus
         !!  shear modulus or modulus of regidity
         !!  defined as the ratio of shear stress and the shear strain.
         !!
         !> @see: http://en.wikzpedia.org/wiki/bulk_modulus
         !!  the inverse of the bulk modulus gives a substance's compressibility.
         ! gg = ds*vs**2
         ! kk = ds*vp**2 - 4*gg/3
         subroutine ps2bs_r0 (vp, vs, ds, kk, gg)
         implicit none
            real(kind=4), intent(in)  :: vp
            real(kind=4), intent(in)  :: vs
            real(kind=4), intent(in)  :: ds
            real(kind=4), intent(out) :: kk
            real(kind=4), intent(out) :: gg
            
            gg = ds*(vs**2)
            kk = ds*(vp**2) - (4.e0*gg/3.e0)
            return
         end subroutine ps2bs_r0
         
         subroutine ps2bs_r1 (vp, vs, ds, kk, gg, nz)
         implicit none
            real(kind=4), dimension(:), intent(in)  :: vp
            real(kind=4), dimension(:), intent(in)  :: vs
            real(kind=4), dimension(:), intent(in)  :: ds
            real(kind=4), dimension(:), intent(out) :: kk
            real(kind=4), dimension(:), intent(out) :: gg
            integer(kind=4),            intent(in)  :: nz
            
            gg(1:nz) = ds(1:nz)*(vs(1:nz)**2)
            kk(1:nz) = ds(1:nz)*(vp(1:nz)**2) - (4.e0*gg(1:nz)/3.e0)
            return
         end subroutine ps2bs_r1
         
         subroutine ps2bs_r2 (vp, vs, ds, kk, gg, nz, nx)
         implicit none
            real(kind=4), dimension(:,:), intent(in)  :: vp
            real(kind=4), dimension(:,:), intent(in)  :: vs
            real(kind=4), dimension(:,:), intent(in)  :: ds
            real(kind=4), dimension(:,:), intent(out) :: kk
            real(kind=4), dimension(:,:), intent(out) :: gg
            integer(kind=4),              intent(in)  :: nz, nx
            
            gg(1:nz,1:nx) = ds(1:nz,1:nx)*(vs(1:nz,1:nx)**2)
            kk(1:nz,1:nx) = ds(1:nz,1:nx)*(vp(1:nz,1:nx)**2) - (4.e0*gg(1:nz,1:nx)/3.e0)
            return
         end subroutine ps2bs_r2
         
         subroutine ps2bs_d0 (vp, vs, ds, kk, gg)
         implicit none
            real(kind=8), intent(in)  :: vp
            real(kind=8), intent(in)  :: vs
            real(kind=8), intent(in)  :: ds
            real(kind=8), intent(out) :: kk
            real(kind=8), intent(out) :: gg
            
            gg = ds*(vs**2)
            kk = ds*(vp**2) - (4.d0*gg/3.d0)
            return
         end subroutine ps2bs_d0
         
         subroutine ps2bs_d1 (vp, vs, ds, kk, gg, nz)
         implicit none
            real(kind=8), dimension(:), intent(in)  :: vp
            real(kind=8), dimension(:), intent(in)  :: vs
            real(kind=8), dimension(:), intent(in)  :: ds
            real(kind=8), dimension(:), intent(out) :: kk
            real(kind=8), dimension(:), intent(out) :: gg
            integer(kind=4),            intent(in)  :: nz
            
            gg(1:nz) = ds(1:nz)*(vs(1:nz)**2)
            kk(1:nz) = ds(1:nz)*(vp(1:nz)**2) - (4.d0*gg(1:nz)/3.d0)
            return
         end subroutine ps2bs_d1
         
         subroutine ps2bs_d2 (vp, vs, ds, kk, gg, nz, nx)
         implicit none
            real(kind=8), dimension(:,:), intent(in)  :: vp
            real(kind=8), dimension(:,:), intent(in)  :: vs
            real(kind=8), dimension(:,:), intent(in)  :: ds
            real(kind=8), dimension(:,:), intent(out) :: kk
            real(kind=8), dimension(:,:), intent(out) :: gg
            integer(kind=4),              intent(in)  :: nz, nx
            
            gg(1:nz,1:nx) = ds(1:nz,1:nx)*(vs(1:nz,1:nx)**2)
            kk(1:nz,1:nx) = ds(1:nz,1:nx)*(vp(1:nz,1:nx)**2) - (4.d0*gg(1:nz,1:nx)/3.d0)
            return
         end subroutine ps2bs_d2
         
         
         subroutine bs2ps_r0 (vp, vs, ds, kk, gg)
         implicit none
            real(kind=4), intent(out) :: vp
            real(kind=4), intent(out) :: vs
            real(kind=4), intent(in)  :: ds
            real(kind=4), intent(in)  :: kk
            real(kind=4), intent(in)  :: gg
            
            vs = sqrt(gg/ds)
            vp = sqrt((kk+(4.e0*gg/3.e0))/ds)
            return
         end subroutine bs2ps_r0
         
         subroutine bs2ps_r1 (vp, vs, ds, kk, gg, nz)
         implicit none
            real(kind=4), dimension(:), intent(out) :: vp
            real(kind=4), dimension(:), intent(out) :: vs
            real(kind=4), dimension(:), intent(in)  :: ds
            real(kind=4), dimension(:), intent(in)  :: kk
            real(kind=4), dimension(:), intent(in)  :: gg
            integer(kind=4),            intent(in)  :: nz
            
            vs(1:nz) = sqrt(gg(1:nz)/ds(1:nz))
            vp(1:nz) = sqrt((kk(1:nz)+(4.e0*gg(1:nz)/3.e0))/ds(1:nz))
            return
         end subroutine bs2ps_r1
         
         subroutine bs2ps_r2 (vp, vs, ds, kk, gg, nz, nx)
         implicit none
            real(kind=4), dimension(:,:), intent(out) :: vp
            real(kind=4), dimension(:,:), intent(out) :: vs
            real(kind=4), dimension(:,:), intent(in)  :: ds
            real(kind=4), dimension(:,:), intent(in)  :: kk
            real(kind=4), dimension(:,:), intent(in)  :: gg
            integer(kind=4),              intent(in)  :: nz, nx
            
            vs(1:nz,1:nx) = sqrt(gg(1:nz,1:nx)/ds(1:nz,1:nx))
            vp(1:nz,1:nx) = sqrt((kk(1:nz,1:nx)+(4.e0*gg(1:nz,1:nx)/3.e0))/ds(1:nz,1:nx))
            return
         end subroutine bs2ps_r2
         
         subroutine bs2ps_d0 (vp, vs, ds, kk, gg)
         implicit none
            real(kind=8), intent(out) :: vp
            real(kind=8), intent(out) :: vs
            real(kind=8), intent(in)  :: ds
            real(kind=8), intent(in)  :: kk
            real(kind=8), intent(in)  :: gg
            
            vs = dsqrt(gg/ds)
            vp = dsqrt((kk+(4.d0*gg/3.d0))/ds)
            return
         end subroutine bs2ps_d0
         
         subroutine bs2ps_d1 (vp, vs, ds, kk, gg, nz)
         implicit none
            real(kind=8), dimension(:), intent(out) :: vp
            real(kind=8), dimension(:), intent(out) :: vs
            real(kind=8), dimension(:), intent(in)  :: ds
            real(kind=8), dimension(:), intent(in)  :: kk
            real(kind=8), dimension(:), intent(in)  :: gg
            integer(kind=4),            intent(in)  :: nz
            
            vs(1:nz) = dsqrt(gg(1:nz)/ds(1:nz))
            vp(1:nz) = dsqrt((kk(1:nz)+(4.d0*gg(1:nz)/3.d0))/ds(1:nz))
            return
         end subroutine bs2ps_d1
         
         subroutine bs2ps_d2 (vp, vs, ds, kk, gg, nz, nx)
         implicit none
            real(kind=8), dimension(:,:), intent(out) :: vp
            real(kind=8), dimension(:,:), intent(out) :: vs
            real(kind=8), dimension(:,:), intent(in)  :: ds
            real(kind=8), dimension(:,:), intent(in)  :: kk
            real(kind=8), dimension(:,:), intent(in)  :: gg
            integer(kind=4),              intent(in)  :: nz, nx
            
            vs(1:nz,1:nx) = dsqrt(gg(1:nz,1:nx)/ds(1:nz,1:nx))
            vp(1:nz,1:nx) = dsqrt((kk(1:nz,1:nx)+(4.d0*gg(1:nz,1:nx)/3.d0))/ds(1:nz,1:nx))
            return
         end subroutine bs2ps_d2
         
      end module mod_rock
      
      
      
      
      
      program main
         
         use mod_rock
         
         
         real(kind=4), dimension(1:10) :: vp 
         real(kind=4), dimension(1:10) :: vs
         real(kind=4), dimension(1:10) :: gm
         
         vp(1:10) = 2.e0
         vs(1:10) = 1.e0
         vs(1:5)  = 0.e0
         vs( 6) = 2.e0
         vs(10) = 2.e0
         
         call ps2gm (vp, vs, gm, 10)
         
         print '(1f12.9)', gm
         print '(a)', ' '
         
         
         do i = 1, 10
            call ps2gm (vp(i), vs(i), gm(i))
            print '(1f12.9)', gm(i)
         end do
            
         
      end program main
