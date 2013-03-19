      module mod_io
      implicit none

        integer(kind=8), public :: nrecl
        integer(kind=8), public :: nrecl4
        integer(kind=8), public :: nrecl8
        integer(kind=8), public :: nrecl16

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

!       modified by UGeun Jang 
!       @date : Thursday, March 20, 2008 
!       @note : add converter specifier
!       @added by UGeun Jang 
!       2007. 09. 27
        interface open_file
          module procedure open_file
        end interface open_file



        ! ----------------------------------------------------------- view_file_size
        ! ----------------------------------------------------------- view_file_size
        !  @purpose : This subroutine returns file size about a file.
        !  @create : 2008-03-20
        !  @author : UGeun Jang
        !  @mailto : ugeun.jang@uwa.edu.au
        interface view_file_size
          module procedure view_file_size4, view_file_size8
        end interface view_file_size
        ! ----------------------------------------------------------- view_file_size
        ! ----------------------------------------------------------- view_file_size

      contains



! --- subroutine open_file (iu, fn, nrecl, endian)
!
!       @author : UGeun Jang
!       @mailto : orange224@gmail.com
!       @create : Thursday, March 20, 2008 
!
!       @note : add convert specifier 'endian' and it  must be one of 
!
!         little_endian : If the file is connected with little endian
!                         integer and IEEE floating-point data 
!                         conversion in effect
!
!         big_endian : If the file is connected with big endian 
!                      integer and IEEE floating-point data conversion 
!                      in effect
!
!         cray : If the file is connected with big endian integer and
!                CRAY floating-point data conversion in effect
!
!         fdx : If the file is connected with little endian integer 
!               and VAX processor F_floating, D_floating, and IEEE 
!               X_floating data conversion in effect 
!
!         fgx : If the file is connected with little endian integer 
!               and VAX processor F_floating, G_floating, and IEEE 
!               X_floating data conversion in effect
!
!         ibm : If the file is connected with big endian integer and 
!               IBM System\370 floating-point data conversion in 
!               effect
!
!         vaxd : If the file is connected with little endian integer 
!                and VAX processor F_floating, D_floating, and 
!                H_floating in effect
!
!         vaxg : If the file is connected with little endian integer 
!                and VAX processor F_floating, G_floating, and 
!                H_floating in effect
!
!         native : If the file is connected with no data conversion in 
!                  effect 
!
!         unknown : If the file or unit is not connected for 
!                   unformatted data transfer
!
      subroutine open_file (iu, fn, nrecl, endian)
      implicit none 
        integer(kind=4),  intent(in) :: iu
        character(len=*), intent(in) :: fn
        integer(kind=8),  intent(in) :: nrecl
        character(len=*), intent(in), optional :: endian

      if (present(endian) .eqv. .true.) then
        open (unit=iu, file=fn, form='unformatted', access='direct', recl=nrecl, convert=endian)

      else
        open (unit=iu, file=fn, form='unformatted', access='direct', recl=nrecl)

      end if

      return
      end subroutine open_file
! --- version 2008_03_20 -----------------------------------------------------------






! --- subroutine open_file ---------------------------------------------------------



      ! ------------------------------------------------------------- view_file_size
      ! ------------------------------------------------------------- view_file_size
      !
      !  subroutine view_file_size (fn, fsize)
      !
      !  @create : 2008-03-20
      !  @author : UGeun Jang
      !  @mailto : ugeun.jang@uwa.edu.au
      !
      !  @purpose : This subroutine returns file size about a file.
      !    buff(1)  : Device ID
      !    buff(2)  : Inode number
      !    buff(3)  : File mode
      !    buff(4)  : Number of links
      !    buff(5)  : Owner's uid
      !    buff(6)  : Owner's gid
      !    buff(7)  : ID of device containing directory entry for file
      !               (0 if not available)
      !    buff(8)  : File size (bytes)
      !    buff(9)  : Last access time
      !    buff(10) : Last modification time
      !    buff(11) : Last file status change time
      !    buff(12) : Preferred I/O block size (-1 if not available)
      !    buff(13) : Number of blocks allocated (-1 if not available)
      !
      subroutine view_file_size4 (fn, fsize) ! --------------------- view_file_size4
        use ifport
      implicit none
        character(len=*), intent(in)      :: fn
        integer(kind=4),  intent(out)     :: fsize
        integer(kind=4),  dimension(1:12) :: buff
        integer(kind=4)                   :: status

      status = stat (fn, buff)
      fsize = buff(8)

      return
      end subroutine view_file_size4 ! ----------------------------- view_file_size4

      subroutine view_file_size8 (fn, fsize) ! --------------------- view_file_size8
        use ifport
      implicit none
        character(len=*), intent(in)      :: fn
        integer(kind=8),  intent(out)     :: fsize
        integer(kind=8),  dimension(1:12) :: buff
        integer(kind=8)                   :: status

      status = stat (fn, buff)
      fsize = buff(8)

      return
      end subroutine view_file_size8 ! ----------------------------- view_file_size8


      function file_size (fn) result (fsize)
        use ifport
      implicit none
        character(len=*), intent(in)      :: fn
        integer(kind=8),  dimension(1:12) :: buff
        integer(kind=8) :: status
        integer(kind=8) :: fsize

      status = stat (fn, buff)
      fsize = buff(8)

      return
      end function file_size

      function file_size4 (fn) result (fsize)
        use ifport
      implicit none
        character(len=*), intent(in)      :: fn
        integer(kind=4),  dimension(1:12) :: buff
        integer(kind=4) :: status
        integer(kind=4) :: fsize

      status = stat (fn, buff)
      fsize = buff(8)

      return
      end function file_size4

      function file_size8 (fn) result (fsize)
        use ifport
      implicit none
        character(len=*), intent(in)      :: fn
        integer(kind=8),  dimension(1:12) :: buff
        integer(kind=8) :: status
        integer(kind=8) :: fsize

      status = stat (fn, buff)
      fsize = buff(8)

      return
      end function file_size8
      ! ------------------------------------------------------------- view_file_size
      ! ------------------------------------------------------------- view_file_size



      end module mod_io








! --- module mod_io
!
!     @create : 2010-03-10
!     @author : UGeun Jang
!     @mailto : ugeun.jang@uwa.edu.au
!
!     @history : 2012-03-05
!       nrecl, nrecl4, nrecl8, nrecl16
!
!     @history : 2012-05-04
!       function file_size (fn) result (fsize)
!
!     @history : 2012-05-04
!       integer(kind=8) :: nrecl
!       integer -> integer(kind=4)
!
!     @history : 2013-01-08
!       change the contact information
!
!     @history : 2013-01-08
!       view_file_size -> view_file_size4 & view_file_size8
!
!     @history : 2013-01-08
!       add file_size4 & file_size8
!


! ----------------------------------------------------------------------- 2013_01_08
! ----------------------------------------------------------------------- 2013_01_11
