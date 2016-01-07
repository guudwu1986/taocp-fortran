!! Mudole containing procedure "gcd_extend",
!! V1P2, Algorithm E.
!! Extended Euclid's algorithm.

module module_greatest_common_divisor
  implicit none
  private

  public :: gcd_extend

  contains

  subroutine gcd_extend(m,n,g,a,b)
    implicit none

    integer , intent(in) &
      :: m
    integer , intent(in) &
      :: n
    integer , intent(out) &
      :: g
    integer , intent(out) &
      :: a
    integer , intent(out) &
      :: b

    integer &
      :: c
    integer &
      :: d
    integer &
      :: a1
    integer &
      :: b1
    integer &
      :: q
    integer &
      :: r
    integer &
      :: t

!! Unusual return!{{{
    if ( m .lt. 0 .or. n .lt. 0 ) then
      g = -1
      a = 0
      b = 0
      return
    end if
    if ( m .eq. 0 ) then
      g = n
      a = 0
      b = 1
      return
    end if
    if ( n .eq. 0 ) then
      g = m
      a = 1
      b = 0
      return
    end if
!}}}

!! Initialization!{{{
    a = 0
    b1 = 0
    b = 1
    a1 = 1
    c = m
    d = n
!}}}

!! Iteration!{{{

!! Always:
!! c = a1*m + b1*n
!! d = a*m + b*n

    do
      r = mod(c,d)
      if ( r .eq. 0 ) then
        exit
      end if
      q = c/d

      c = d
      d = r
      t = a1
      a1 = a
      a = t - q*a
      t = b1
      b1 = b
      b = t - q*b
    end do
!}}}

    g = d

    return
  end subroutine gcd_extend

end module module_greatest_common_divisor
