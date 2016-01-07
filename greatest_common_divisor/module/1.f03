!! Mudole containing procedure "gcd",
!! V1P2, Algorithm E.
!! Euclid's algorithm.

module module_greatest_common_divisor
  implicit none
  private

  public :: gcd

  contains

  subroutine gcd(a,b,g)
    implicit none

    integer , intent(in) &
      :: a
    integer , intent(in) &
      :: b
    integer , intent(out) &
      :: g

    integer &
      m , n

!! Unusual return!{{{
    if ( a .lt. 0 .or. b .lt. 0 ) then
      g = -1
      return
    end if
    if ( a .eq. 0 ) then
      g = b
      return
    end if
    if ( b .eq. 0 ) then
      g = a
      return
    end if
!}}}

!! Pre-process!{{{
    if ( a>b ) then
      m = a
      n = b
    else
      m = b
      n = a
    end if
!}}}

!! Main iteration!{{{
    do while ( n .ne. 0 )
      g = mod(m,n)
      m = n
      n = g
    end do

    g = m
!}}}

    return
  end subroutine gcd
end module module_greatest_common_divisor
