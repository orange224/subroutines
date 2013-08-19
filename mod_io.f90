      module mod_io
      implicit none

         integer(kind=4), public :: nrecl  
         integer(kind=4), public :: nrecl4 
         integer(kind=4), public :: nrecl8 
         integer(kind=4), public :: nrecl16
         
         integer(kind=4), parameter, public :: iu01 =  1
         integer(kind=4), parameter, public :: iu02 =  2
         integer(kind=4), parameter, public :: iu03 =  3
         integer(kind=4), parameter, public :: iu04 =  4
         integer(kind=4), parameter, public :: iu05 =  5
         integer(kind=4), parameter, public :: iu06 =  6
         integer(kind=4), parameter, public :: iu07 =  7
         integer(kind=4), parameter, public :: iu08 =  8
         integer(kind=4), parameter, public :: iu09 =  9
         
         integer(kind=4), parameter, public :: iu10 = 10
         integer(kind=4), parameter, public :: iu11 = 11
         integer(kind=4), parameter, public :: iu12 = 12
         integer(kind=4), parameter, public :: iu13 = 13
         integer(kind=4), parameter, public :: iu14 = 14
         integer(kind=4), parameter, public :: iu15 = 15
         integer(kind=4), parameter, public :: iu16 = 16
         integer(kind=4), parameter, public :: iu17 = 17
         integer(kind=4), parameter, public :: iu18 = 18
         integer(kind=4), parameter, public :: iu19 = 19
         
         integer(kind=4), parameter, public :: iu20 = 20
         integer(kind=4), parameter, public :: iu21 = 21
         integer(kind=4), parameter, public :: iu22 = 22
         integer(kind=4), parameter, public :: iu23 = 23
         integer(kind=4), parameter, public :: iu24 = 24
         integer(kind=4), parameter, public :: iu25 = 25
         integer(kind=4), parameter, public :: iu26 = 26
         integer(kind=4), parameter, public :: iu27 = 27
         integer(kind=4), parameter, public :: iu28 = 28
         integer(kind=4), parameter, public :: iu29 = 29
         
         integer(kind=4), parameter, public :: iu30 = 30
         integer(kind=4), parameter, public :: iu31 = 31
         integer(kind=4), parameter, public :: iu32 = 32
         integer(kind=4), parameter, public :: iu33 = 33
         integer(kind=4), parameter, public :: iu34 = 34
         integer(kind=4), parameter, public :: iu35 = 35
         integer(kind=4), parameter, public :: iu36 = 36
         integer(kind=4), parameter, public :: iu37 = 37
         integer(kind=4), parameter, public :: iu38 = 38
         integer(kind=4), parameter, public :: iu39 = 39
         
         integer(kind=4), parameter, public :: iu40 = 40
         integer(kind=4), parameter, public :: iu41 = 41
         integer(kind=4), parameter, public :: iu42 = 42
         integer(kind=4), parameter, public :: iu43 = 43
         integer(kind=4), parameter, public :: iu44 = 44
         integer(kind=4), parameter, public :: iu45 = 45
         integer(kind=4), parameter, public :: iu46 = 46
         integer(kind=4), parameter, public :: iu47 = 47
         integer(kind=4), parameter, public :: iu48 = 48
         integer(kind=4), parameter, public :: iu49 = 49
         
         integer(kind=4), parameter, public :: iu50 = 50
         integer(kind=4), parameter, public :: iu51 = 51
         integer(kind=4), parameter, public :: iu52 = 52
         integer(kind=4), parameter, public :: iu53 = 53
         integer(kind=4), parameter, public :: iu54 = 54
         integer(kind=4), parameter, public :: iu55 = 55
         integer(kind=4), parameter, public :: iu56 = 56
         integer(kind=4), parameter, public :: iu57 = 57
         integer(kind=4), parameter, public :: iu58 = 58
         integer(kind=4), parameter, public :: iu59 = 59
         
         integer(kind=4), parameter, public :: iu60 = 60
         integer(kind=4), parameter, public :: iu61 = 61
         integer(kind=4), parameter, public :: iu62 = 62
         integer(kind=4), parameter, public :: iu63 = 63
         integer(kind=4), parameter, public :: iu64 = 64
         integer(kind=4), parameter, public :: iu65 = 65
         integer(kind=4), parameter, public :: iu66 = 66
         integer(kind=4), parameter, public :: iu67 = 67
         integer(kind=4), parameter, public :: iu68 = 68
         integer(kind=4), parameter, public :: iu69 = 69
         
         integer(kind=4), parameter, public :: iu70 = 70
         integer(kind=4), parameter, public :: iu71 = 71
         integer(kind=4), parameter, public :: iu72 = 72
         integer(kind=4), parameter, public :: iu73 = 73
         integer(kind=4), parameter, public :: iu74 = 74
         integer(kind=4), parameter, public :: iu75 = 75
         integer(kind=4), parameter, public :: iu76 = 76
         integer(kind=4), parameter, public :: iu77 = 77
         integer(kind=4), parameter, public :: iu78 = 78
         integer(kind=4), parameter, public :: iu79 = 79
         
         integer(kind=4), parameter, public :: iu80 = 80
         integer(kind=4), parameter, public :: iu81 = 81
         integer(kind=4), parameter, public :: iu82 = 82
         integer(kind=4), parameter, public :: iu83 = 83
         integer(kind=4), parameter, public :: iu84 = 84
         integer(kind=4), parameter, public :: iu85 = 85
         integer(kind=4), parameter, public :: iu86 = 86
         integer(kind=4), parameter, public :: iu87 = 87
         integer(kind=4), parameter, public :: iu88 = 88
         integer(kind=4), parameter, public :: iu89 = 89
         
         integer(kind=4), parameter, public :: iu90 = 90
         integer(kind=4), parameter, public :: iu91 = 91
         integer(kind=4), parameter, public :: iu92 = 92
         integer(kind=4), parameter, public :: iu93 = 93
         integer(kind=4), parameter, public :: iu94 = 94
         integer(kind=4), parameter, public :: iu95 = 95
         integer(kind=4), parameter, public :: iu96 = 96
         integer(kind=4), parameter, public :: iu97 = 97
         integer(kind=4), parameter, public :: iu98 = 98
         integer(kind=4), parameter, public :: iu99 = 99
         
         character(len=1), public :: inum1
         character(len=2), public :: inum2
         character(len=3), public :: inum3
         character(len=4), public :: inum4
         character(len=5), public :: inum5
         character(len=6), public :: inum6
         
         
         interface           file_open                                    
           module procedure  file_open_nrecl4 !> integer(kind=4) :: nrecl4
           module procedure  file_open_nrecl8 !> integer(kind=8) :: nrecl8
         end interface       file_open                                    
         
         
         interface            read_irec4    
            module procedure  read_irec4_si0
            module procedure  read_irec4_si1
            module procedure  read_irec4_ii0
            module procedure  read_irec4_ii1
            module procedure  read_irec4_li0
            module procedure  read_irec4_li1
            
            module procedure  read_irec4_r0 
            module procedure  read_irec4_r1 
            module procedure  read_irec4_d0 
            module procedure  read_irec4_d1 
            
            module procedure  read_irec4_c0 
            module procedure  read_irec4_c1 
            module procedure  read_irec4_z0 
            module procedure  read_irec4_z1 
         end interface        read_irec4    
         
         
         interface            read_irec8    
            module procedure  read_irec8_si0
            module procedure  read_irec8_si1
            module procedure  read_irec8_ii0
            module procedure  read_irec8_ii1
            module procedure  read_irec8_li0
            module procedure  read_irec8_li1
            
            module procedure  read_irec8_r0 
            module procedure  read_irec8_r1 
            module procedure  read_irec8_d0 
            module procedure  read_irec8_d1 
            
            module procedure  read_irec8_c0 
            module procedure  read_irec8_c1 
            module procedure  read_irec8_z0 
            module procedure  read_irec8_z1 
         end interface        read_irec8    
         
         
         interface            read_irec     
            module procedure  read_irec4_si0
            module procedure  read_irec4_si1
            module procedure  read_irec4_ii0
            module procedure  read_irec4_ii1
            module procedure  read_irec4_li0
            module procedure  read_irec4_li1
            
            module procedure  read_irec4_r0 
            module procedure  read_irec4_r1 
            module procedure  read_irec4_d0 
            module procedure  read_irec4_d1 
            
            module procedure  read_irec4_c0 
            module procedure  read_irec4_c1 
            module procedure  read_irec4_z0 
            module procedure  read_irec4_z1 
            
            module procedure  read_irec8_si0
            module procedure  read_irec8_si1
            module procedure  read_irec8_ii0
            module procedure  read_irec8_ii1
            module procedure  read_irec8_li0
            module procedure  read_irec8_li1
            
            module procedure  read_irec8_r0 
            module procedure  read_irec8_r1 
            module procedure  read_irec8_d0 
            module procedure  read_irec8_d1 
            
            module procedure  read_irec8_c0 
            module procedure  read_irec8_c1 
            module procedure  read_irec8_z0 
            module procedure  read_irec8_z1 
         end interface        read_irec     
         
         
         interface            write_irec4    
            module procedure  write_irec4_si0
            module procedure  write_irec4_si1
            module procedure  write_irec4_ii0
            module procedure  write_irec4_ii1
            module procedure  write_irec4_li0
            module procedure  write_irec4_li1
            
            module procedure  write_irec4_r0 
            module procedure  write_irec4_r1 
            module procedure  write_irec4_d0 
            module procedure  write_irec4_d1 
            
            module procedure  write_irec4_c0 
            module procedure  write_irec4_c1 
            module procedure  write_irec4_z0 
            module procedure  write_irec4_z1 
         end interface        write_irec4    
         
         
         interface            write_irec8    
            module procedure  write_irec8_si0
            module procedure  write_irec8_si1
            module procedure  write_irec8_ii0
            module procedure  write_irec8_ii1
            module procedure  write_irec8_li0
            module procedure  write_irec8_li1
            
            module procedure  write_irec8_r0 
            module procedure  write_irec8_r1 
            module procedure  write_irec8_d0 
            module procedure  write_irec8_d1 
            
            module procedure  write_irec8_c0 
            module procedure  write_irec8_c1 
            module procedure  write_irec8_z0 
            module procedure  write_irec8_z1 
         end interface        write_irec8    
         
         
         interface            write_irec     
            module procedure  write_irec4_si0
            module procedure  write_irec4_si1
            module procedure  write_irec4_ii0
            module procedure  write_irec4_ii1
            module procedure  write_irec4_li0
            module procedure  write_irec4_li1
            
            module procedure  write_irec4_r0 
            module procedure  write_irec4_r1 
            module procedure  write_irec4_d0 
            module procedure  write_irec4_d1 
            
            module procedure  write_irec4_c0 
            module procedure  write_irec4_c1 
            module procedure  write_irec4_z0 
            module procedure  write_irec4_z1 
            
            module procedure  write_irec8_si0
            module procedure  write_irec8_si1
            module procedure  write_irec8_ii0
            module procedure  write_irec8_ii1
            module procedure  write_irec8_li0
            module procedure  write_irec8_li1
            
            module procedure  write_irec8_r0 
            module procedure  write_irec8_r1 
            module procedure  write_irec8_d0 
            module procedure  write_irec8_d1 
            
            module procedure  write_irec8_c0 
            module procedure  write_irec8_c1 
            module procedure  write_irec8_z0 
            module procedure  write_irec8_z1 
         end interface        write_irec     
         
         
         
         
         interface           file_close
            module procedure file_close
         end interface       file_close
         
      contains
         
         
         
         
         
         !!                                                                                                   file_open
         !> @author 
         !!  orange224
         !! 
         !> @version 
         !!  1.0.0
         !! 
         !> @todo 
         !!  2013-05-13: open_file -> file_open 
         !! 
         !> @todo 
         !!  2013-04-24: open_file4, open_file8
         !! 
         !> @todo 
         !!  2013-03-24: if (present(endian)) -> select case(present(endian))
         !! 
         !> @todo 
         !!  2008-03-20: add comments
         !! 
         !> @todo 
         !!  2007-09-27: add 'endian' to specify the conversion.
         !! 
         !> @param[in] iu    
         !> @param[in] fn    
         !> @param[in] nrecl 
         !> @param[in] endian
         !! 
         !> @see convert specifier 'endian', it must be on of following; 
         !! 
         !> @see little_endian: If the file is connected with little endian integer and IEEE floating-point data conversion in effect                                           
         !> @see    big_endian: If the file is connected with    big endian integer and IEEE floating-point data conversion in effect                                           
         !> @see          cray: If the file is connected with    big endian integer and CRAY floating-point data conversion in effect                                           
         !> @see           fdx: If the file is connected with little endian integer and and VAX processor F_floating, D_floating, and IEEE X_floating data conversion in effect 
         !> @see           fgx: If the file is connected with little endian integer and and VAX processor F_floating, G_floating, and IEEE X_floating data conversion in effect 
         !> @see           ibm: If the file is connected with    big endian integer and IBM System\370 floating-point data conversion in effect                                 
         !> @see          vaxd: If the file is connected with little endian integer and VAX processor F_floating, D_floating, and H_floating in effect                          
         !> @see          vaxg: If the file is connected with little endian integer and VAX processor F_floating, G_floating, and H_floating in effect                          
         !> @see        native: If the file is connected with no data conversion in effect                                                                                      
         !> @see       unknown: If the file or unit is not connected for unformatted data transfer                                                                              
         
         subroutine file_open_nrecl4 (iu, fn, nrecl, endian)                                          ! file_open_irec4
         implicit none 
            integer(kind=4),  intent(in) :: iu
            integer(kind=4),  intent(in) :: nrecl
            character(len=*), intent(in) :: fn
            character(len=*), intent(in) :: endian
            optional                     :: endian
            
            select case (present(endian)) 
            case (.true.)
               open (unit=iu, file=fn, form='unformatted', access='direct', recl=nrecl, convert=endian)
            case (.false.)
               open (unit=iu, file=fn, form='unformatted', access='direct', recl=nrecl)
            end select
            
            return
         end subroutine file_open_nrecl4
         
         
         subroutine file_open_nrecl8 (iu, fn, nrecl8, endian)                                         ! file_open_irec8
         implicit none 
            integer(kind=4),  intent(in) :: iu
            integer(kind=8),  intent(in) :: nrecl8
            character(len=*), intent(in) :: fn
            character(len=*), intent(in) :: endian
            optional                     :: endian
            
            select case (present(endian)) 
            case (.true.)
               open (unit=iu, file=fn, form='unformatted', access='direct', recl=nrecl8, convert=endian)
            case (.false.)
               open (unit=iu, file=fn, form='unformatted', access='direct', recl=nrecl8)
            end select
            
            return
         end subroutine file_open_nrecl8
         
         
         
         
         
         
         
         
         
         
         !!                                                                                                  read_irec
         !> @author
         !!  orange224
         !!
         !> @todo
         !!  2013-06-14: read_irec4_i1
         !!  2013-06-14: read_irec4_i2
         !!  2013-06-14: read_irec8_i1
         !!  2013-06-14: read_irec8_i2
         !!
         !> @todo
         !!  2013-05-13: read_profile -> read_irec
         !!
         !> @todo
         !!  2013-05-05: extract_profile -> read_profile
         !!
         !> @todo
         !!  2013-04-24: add extract_profile_r8 for large file
         !!
         !> @todo
         !!  2013-04-24: add extract_profile_r4
         !!
         !> @todo
         !!  2013-04-05: created
         
         ! -------------------------------------------------------------------------------------------------- integer*2
         subroutine read_irec4_si0 (iu, irec, profile)
         implicit none
            integer*2,       intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=4), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec4_si0
         
         
         subroutine read_irec4_si1 (iu, irec, profile, nze)
         implicit none
            integer*2,      dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=4),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec4_si1
         
         
         ! -------------------------------------------------------------------------------------------- integer(kind=4)
         subroutine read_irec4_ii0 (iu, irec, profile)
         implicit none
            integer(kind=4), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=4), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec4_ii0
         
         
         subroutine read_irec4_ii1 (iu, irec, profile, nze)
         implicit none
            integer(kind=4),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=4),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec4_ii1
         
         
         ! -------------------------------------------------------------------------------------------- integer(kind=8)
         subroutine read_irec4_li0 (iu, irec, profile)
         implicit none
            integer(kind=8), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=4), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec4_li0
         
         
         subroutine read_irec4_li1 (iu, irec, profile, nze)
         implicit none
            integer(kind=8),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=4),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec4_li1
         
         
         ! ----------------------------------------------------------------------------------------------- real(kind=4)
         subroutine read_irec4_r0 (iu, irec, profile)
         implicit none
               real(kind=4), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=4), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec4_r0
         
         
         subroutine read_irec4_r1 (iu, irec, profile, nze)
         implicit none
               real(kind=4),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=4),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec4_r1
         
         
         ! ----------------------------------------------------------------------------------------------- real(kind=8)
         subroutine read_irec4_d0 (iu, irec, profile)
         implicit none
               real(kind=8), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=4), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec4_d0
         
         
         subroutine read_irec4_d1 (iu, irec, profile, nze)
         implicit none
               real(kind=8),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=4),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec4_d1
         
         
         ! -------------------------------------------------------------------------------------------- complex(kind=4)
         subroutine read_irec4_c0 (iu, irec, profile)
         implicit none
            complex(kind=4), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=4), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec4_c0
         
         
         subroutine read_irec4_c1 (iu, irec, profile, nze)
         implicit none
            complex(kind=4),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=4),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec4_c1
         
         
         ! -------------------------------------------------------------------------------------------- complex(kind=8)
         subroutine read_irec4_z0 (iu, irec, profile)
         implicit none
            complex(kind=8), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=4), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec4_z0
         
         
         subroutine read_irec4_z1 (iu, irec, profile, nze)
         implicit none
            complex(kind=8),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=4),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec4_z1
         
         
         ! -------------------------------------------------------------------------------------------------- integer*2
         subroutine read_irec8_si0 (iu, irec, profile)
         implicit none
            integer*2,       intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=8), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec8_si0
         
         
         subroutine read_irec8_si1 (iu, irec, profile, nze)
         implicit none
            integer*2,      dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=8),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec8_si1
         
         
         ! -------------------------------------------------------------------------------------------- integer(kind=4)
         subroutine read_irec8_ii0 (iu, irec, profile)
         implicit none
            integer(kind=4), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=8), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec8_ii0
         
         
         subroutine read_irec8_ii1 (iu, irec, profile, nze)
         implicit none
            integer(kind=4),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=8),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec8_ii1
         
         
         ! -------------------------------------------------------------------------------------------- integer(kind=8)
         subroutine read_irec8_li0 (iu, irec, profile)
         implicit none
            integer(kind=8), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=8), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec8_li0
         
         
         subroutine read_irec8_li1 (iu, irec, profile, nze)
         implicit none
            integer(kind=8),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=8),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec8_li1
         
         
         ! ----------------------------------------------------------------------------------------------- real(kind=4)
         subroutine read_irec8_r0 (iu, irec, profile)
         implicit none
               real(kind=4), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=8), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec8_r0
         
         
         subroutine read_irec8_r1 (iu, irec, profile, nze)
         implicit none
               real(kind=4),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=8),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec8_r1
         
         
         ! ----------------------------------------------------------------------------------------------- real(kind=8)
         subroutine read_irec8_d0 (iu, irec, profile)
         implicit none
               real(kind=8), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=8), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec8_d0
         
         
         subroutine read_irec8_d1 (iu, irec, profile, nze)
         implicit none
               real(kind=8),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=8),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec8_d1
         
         
         ! -------------------------------------------------------------------------------------------- complex(kind=4)
         subroutine read_irec8_c0 (iu, irec, profile)
         implicit none
            complex(kind=4), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=8), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec8_c0
         
         
         subroutine read_irec8_c1 (iu, irec, profile, nze)
         implicit none
            complex(kind=4),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=8),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec8_c1
         
         
         ! -------------------------------------------------------------------------------------------- complex(kind=8)
         subroutine read_irec8_z0 (iu, irec, profile)
         implicit none
            complex(kind=8), intent(out) :: profile
            integer(kind=4), intent(in)  :: iu
            integer(kind=8), intent(in)  :: irec
            
            read (unit=iu, rec=irec) profile
            
            return
         end subroutine read_irec8_z0
         
         
         subroutine read_irec8_z1 (iu, irec, profile, nze)
         implicit none
            complex(kind=8),dimension(:), intent(out) :: profile
            integer(kind=4),              intent(in)  :: iu
            integer(kind=8),              intent(in)  :: irec
            integer(kind=4),              intent(in)  :: nze
            
            read (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine read_irec8_z1
         
         
         
         
         
         
         
         
         
         
         ! -------------------------------------------------------------------------------------------------- integer*2
         subroutine write_irec4_si0 (iu, irec, profile)
         implicit none
            integer*2,       intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=4), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec4_si0
         
         
         subroutine write_irec4_si1 (iu, irec, profile, nze)
         implicit none
            integer*2,      dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=4),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec4_si1
         
         
         ! -------------------------------------------------------------------------------------------- integer(kind=4)
         subroutine write_irec4_ii0 (iu, irec, profile)
         implicit none
            integer(kind=4), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=4), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec4_ii0
         
         
         subroutine write_irec4_ii1 (iu, irec, profile, nze)
         implicit none
            integer(kind=4),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=4),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec4_ii1
         
         
         ! -------------------------------------------------------------------------------------------- integer(kind=8)
         subroutine write_irec4_li0 (iu, irec, profile)
         implicit none
            integer(kind=8), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=4), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec4_li0
         
         
         subroutine write_irec4_li1 (iu, irec, profile, nze)
         implicit none
            integer(kind=8),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=4),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec4_li1
         
         
         ! ----------------------------------------------------------------------------------------------- real(kind=4)
         subroutine write_irec4_r0 (iu, irec, profile)
         implicit none
               real(kind=4), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=4), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec4_r0
         
         
         subroutine write_irec4_r1 (iu, irec, profile, nze)
         implicit none
               real(kind=4),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=4),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec4_r1
         
         
         ! ----------------------------------------------------------------------------------------------- real(kind=8)
         subroutine write_irec4_d0 (iu, irec, profile)
         implicit none
               real(kind=8), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=4), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec4_d0
         
         
         subroutine write_irec4_d1 (iu, irec, profile, nze)
         implicit none
               real(kind=8),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=4),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec4_d1
         
         
         ! -------------------------------------------------------------------------------------------- complex(kind=4)
         subroutine write_irec4_c0 (iu, irec, profile)
         implicit none
            complex(kind=4), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=4), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec4_c0
         
         
         subroutine write_irec4_c1 (iu, irec, profile, nze)
         implicit none
            complex(kind=4),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=4),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec4_c1
         
         
         ! -------------------------------------------------------------------------------------------- complex(kind=8)
         subroutine write_irec4_z0 (iu, irec, profile)
         implicit none
            complex(kind=8), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=4), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec4_z0
         
         
         subroutine write_irec4_z1 (iu, irec, profile, nze)
         implicit none
            complex(kind=8),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=4),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec4_z1
         
         
         ! -------------------------------------------------------------------------------------------------- integer*2
         subroutine write_irec8_si0 (iu, irec, profile)
         implicit none
            integer*2,       intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=8), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec8_si0
         
         
         subroutine write_irec8_si1 (iu, irec, profile, nze)
         implicit none
            integer*2,      dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=8),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec8_si1
         
         
         ! -------------------------------------------------------------------------------------------- integer(kind=4)
         subroutine write_irec8_ii0 (iu, irec, profile)
         implicit none
            integer(kind=4), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=8), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec8_ii0
         
         
         subroutine write_irec8_ii1 (iu, irec, profile, nze)
         implicit none
            integer(kind=4),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=8),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec8_ii1
         
         
         ! -------------------------------------------------------------------------------------------- integer(kind=8)
         subroutine write_irec8_li0 (iu, irec, profile)
         implicit none
            integer(kind=8), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=8), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec8_li0
         
         
         subroutine write_irec8_li1 (iu, irec, profile, nze)
         implicit none
            integer(kind=8),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=8),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec8_li1
         
         
         ! ----------------------------------------------------------------------------------------------- real(kind=4)
         subroutine write_irec8_r0 (iu, irec, profile)
         implicit none
               real(kind=4), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=8), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec8_r0
         
         
         subroutine write_irec8_r1 (iu, irec, profile, nze)
         implicit none
               real(kind=4),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=8),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec8_r1
         
         
         ! ----------------------------------------------------------------------------------------------- real(kind=8)
         subroutine write_irec8_d0 (iu, irec, profile)
         implicit none
               real(kind=8), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=8), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec8_d0
         
         
         subroutine write_irec8_d1 (iu, irec, profile, nze)
         implicit none
               real(kind=8),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=8),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec8_d1
         
         
         ! -------------------------------------------------------------------------------------------- complex(kind=4)
         subroutine write_irec8_c0 (iu, irec, profile)
         implicit none
            complex(kind=4), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=8), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec8_c0
         
         
         subroutine write_irec8_c1 (iu, irec, profile, nze)
         implicit none
            complex(kind=4),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=8),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec8_c1
         
         
         ! -------------------------------------------------------------------------------------------- complex(kind=8)
         subroutine write_irec8_z0 (iu, irec, profile)
         implicit none
            complex(kind=8), intent(in) :: profile
            integer(kind=4), intent(in) :: iu
            integer(kind=8), intent(in) :: irec
            
            write (unit=iu, rec=irec) profile
            
            return
         end subroutine write_irec8_z0
         
         
         subroutine write_irec8_z1 (iu, irec, profile, nze)
         implicit none
            complex(kind=8),dimension(:), intent(in) :: profile
            integer(kind=4),              intent(in) :: iu
            integer(kind=8),              intent(in) :: irec
            integer(kind=4),              intent(in) :: nze
            
            write (unit=iu, rec=irec) profile(1:nze)
            
            return
         end subroutine write_irec8_z1
         
         
         
         
         
         
         
         
         
         
         subroutine file_close (iu)                                                                        ! file_close
         implicit none
            integer(kind=4), intent(in) :: iu
            
            close (unit=iu)
            
            return
         end subroutine file_close


         end module mod_io
