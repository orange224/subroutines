      module mod_time
      implicit none
         
         character(len=3), dimension(1:12), parameter :: month = (/ & 
                           'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',& 
                           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec' /)
         
         interface           timestamp
            module procedure timestamp
         end interface       timestamp
         
      contains
      
      
      
      !> @author
      !!  ugeun.jang
      !> @history
      !!  2013-08-12: created
      !> @brief
      !!  return current local time, not UTC time
      !!  http://software.intel.com/sites/products/documentation/doclib/stdxe/2013/composerxe/compiler/fortran-mac/GUID-4EF6D9F2-BF08-46BD-8F0F-75A3B7FDF778.htm
      subroutine timestamp (iu99)
      implicit none
         integer(kind=4), intent(in), optional :: iu99
         character(len= 8) :: date
         character(len=10) :: time
         character(len= 5) :: zone
         integer(kind=4), dimension(1:8) :: values
         integer(kind=4) :: yy; character(len=4) :: cyy  ! 4-digit year
         integer(kind=4) :: mm; character(len=2) :: cmm  ! the month of the year
         integer(kind=4) :: dd; character(len=2) :: cdd  ! the day of the month
         integer(kind=4) :: hh; character(len=2) :: chh  ! local time; the hour of the day
         integer(kind=4) :: nn; character(len=2) :: cnn  ! local time; the minutes of the hour
         integer(kind=4) :: ss; character(len=2) :: css  ! local time; the seconds of the minute
         integer(kind=4) :: ms; character(len=3) :: cms  ! local time; the milliseconds of the second
         
         call date_and_time (date, time, zone, values(1:8))
         yy = values(1)
         mm = values(2)
         dd = values(3)
         hh = values(5)
         nn = values(6)
         ss = values(7)
         ms = values(8)
         
         ! yy = values(1);    write (cyy, '(i4.4)') yy ! 4-digit year
         ! mm = values(2);    write (cmm, '(i2.2)') mm ! the month of the year
         ! dd = values(3);    write (cdd, '(i2.2)') dd ! the day of the month
         ! hh = values(5);    write (chh, '(i2.2)') hh ! local time; the hour of the day
         ! nn = values(6);    write (cnn, '(i2.2)') nn ! local time; the minutes of the hour
         ! ss = values(7);    write (css, '(i2.2)') ss ! local time; the seconds of the minute
         ! ms = values(8);    write (cms, '(i3.3)') ms ! local time; the milliseconds of the second
         
         if (present(iu99)) then
            write (iu99, fmt='(i4.4, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i3.3, a)') & 
                                 yy, '-',mm, '-',dd, ' ',hh, ':',nn, ':',ss, '.',ms, zone
         else
            print '(i4.4, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i2.2, a,i3.3, a)', & 
                      yy, '-',mm, '-',dd, ' ',hh, ':',nn, ':',ss, '.',ms, zone
         end if
         return
      end subroutine timestamp
      
      end module mod_time
      
      ! program main
      !    use mod_time
      !    call timestamp (99)
      !    stop
      ! end program main
      
      
