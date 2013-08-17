      module mod_time
      implicit none
         
         character(len=3), dimension(1:12), parameter :: month = (/ & 
                           'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',& 
                           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)
         
         type time_unix
            integer(kind=4)  :: yy   ! 4-digit year
            integer(kind=4)  :: mm   ! the month of the year
            integer(kind=4)  :: dd   ! the day of the month
            integer(kind=4)  :: hh   ! the hour of the day
            integer(kind=4)  :: nn   ! the minutes of the hour
            integer(kind=4)  :: ss   ! the seconds of the minute
            integer(kind=4)  :: ms   ! the milliseconds of the second
            character(len=5) :: zn   ! time zone
         end type time_unix
         type (time_unix), private :: time_unix99
         type (time_unix), public  :: time_unix00
         
         interface           time_unix_init
            module procedure time_unix_init
         end interface       time_unix_init
         
         interface           time_unix_stmp
            module procedure time_unix_stmp
         end interface       time_unix_stmp
         
         interface           time_stmp
            module procedure time_stmp
         end interface       time_stmp
         
      contains
      
      
      
      !> @author
      !!  ugeun.jang
      !> @todo
      !!  2013-08-12: created
      !> @brief
      !!  return current local time, not UTC time
      !!  http://software.intel.com/sites/products/documentation/doclib/stdxe/2013/composerxe/compiler/fortran-mac/GUID-4EF6D9F2-BF08-46BD-8F0F-75A3B7FDF778.htm
      !!
      subroutine time_unix_init ( )
      implicit none
         character(len= 8) :: date
         character(len=10) :: time
         character(len= 5) :: zone
         integer(kind=4), dimension(1:8) :: values
         
         call date_and_time (date, time, zone, values(1:8))
         time_unix00%yy = values(1)
         time_unix00%mm = values(2)
         time_unix00%dd = values(3)
         time_unix00%hh = values(5)
         time_unix00%nn = values(6)
         time_unix00%ss = values(7)
         time_unix00%ms = values(8)
         time_unix00%zn = zone
         
         return
      end subroutine time_unix_init
      
      
      
      
      
      !> @author
      !!  ugeun.jang
      !> @todo
      !!  2013-08-13: created
      !> @brief
      !!  return current local time, not UTC time
      !!  http://software.intel.com/sites/products/documentation/doclib/stdxe/2013/composerxe/compiler/fortran-mac/GUID-4EF6D9F2-BF08-46BD-8F0F-75A3B7FDF778.htm
      !!
      subroutine time_unix_stmp (iu99)
      implicit none
         integer(kind=4), intent(in), optional :: iu99
         
         if (present(iu99)) then
            write (iu99, fmt='(i4.4, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i3.3, a)')                  &
                               time_unix00%yy, '-',time_unix00%mm, '-',time_unix00%dd,                     &
                           ' ',time_unix00%hh, ':',time_unix00%nn, ':',time_unix00%ss, '.',time_unix00%ms, &
                               time_unix00%zn                                                            
         else
            print '(i4.4, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i3.3, a)',                         &
                               time_unix00%yy, '-',time_unix00%mm, '-',time_unix00%dd,                     &
                           ' ',time_unix00%hh, ':',time_unix00%nn, ':',time_unix00%ss, '.',time_unix00%ms, &
                               time_unix00%zn                                                            
         end if
         
         return
      end subroutine time_unix_stmp
      
      
      
      
      
      !> @author
      !!  ugeun.jang
      !> @todo
      !!  2013-08-13: created
      !> @brief
      !!  return current local time, not UTC time
      !!  http://software.intel.com/sites/products/documentation/doclib/stdxe/2013/composerxe/compiler/fortran-mac/GUID-4EF6D9F2-BF08-46BD-8F0F-75A3B7FDF778.htm
      ! 
      ! subroutine time_stmp (iu99)
      ! implicit none
      !    integer(kind=4), intent(in), optional :: iu99
      !    character(len= 8) :: date
      !    character(len=10) :: time
      !    character(len= 5) :: zone
      !    integer(kind=4), dimension(1:8) :: values
      !    integer(kind=4) :: yy  ! 4-digit year
      !    integer(kind=4) :: mm  ! the month of the year
      !    integer(kind=4) :: dd  ! the day of the month
      !    integer(kind=4) :: hh  ! local time; the hour of the day
      !    integer(kind=4) :: nn  ! local time; the minutes of the hour
      !    integer(kind=4) :: ss  ! local time; the seconds of the minute
      !    integer(kind=4) :: ms  ! local time; the milliseconds of the second
      !    
      !    call date_and_time (date, time, zone, values(1:8))
      !    yy = values(1)
      !    mm = values(2)
      !    dd = values(3)
      !    hh = values(5)
      !    nn = values(6)
      !    ss = values(7)
      !    ms = values(8)
      !    
      !    if (present(iu99)) then
      !       write (iu99, fmt='(i4.4, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i3.3, a)') & 
      !                          yy,   '-',mm, '-',dd, ' ',hh, ':',nn, ':',ss, '.',ms, zone
      !    else
      !       print '(i4.4, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i3.3, a)', & 
      !               yy,   '-',mm, '-',dd, ' ',hh, ':',nn, ':',ss, '.',ms, zone
      !    end if
      !    return
      ! end subroutine time_stmp
      !
      subroutine time_stmp (iu99)
      implicit none
         integer(kind=4), intent(in), optional :: iu99
         
         if (present (iu99)) then
            call time_unix_init ( )
            call time_unix_stmp (iu99)
         else 
            call time_unix_init ( )
            call time_unix_stmp ( )
         end if
         
         return
      end subroutine time_stmp
      
      
      
      
      
      end module mod_time
      
      
      
      
      
      
      
      
      
      
      ! program main
      !    use mod_time
      !    call time_stmp (99)
      !    stop
      ! end program main
      
      
