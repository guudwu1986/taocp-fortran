!! Mudole containing procedure "bcd",
!! V1P9E4, Algorithm F.
!! Euclid's algorithm without replacement.

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
    if ( a .eq. 0 .or. b .eq. 0 ) then
      g = 0
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
    do
      m = mod(m,n)
      if ( m .eq. 0 ) then
        g = n
        exit
      end if
      n = mod(n,m)
      if ( n .eq. 0 ) then
        g = m
        exit
      end if
    end do
!}}}

    return
  end subroutine gcd
end module module_greatest_common_divisor
