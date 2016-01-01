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

    if ( a .lt. 0 .or. b .lt. 0 ) then
      g = -1
      return
    end if
    if ( a .eq. 0 .or. b .eq. 0 ) then
      g = 0
      return
    end if

    if ( a>b ) then
      m = a
      n = b
    else
      m = b
      n = a
    end if

    do while ( n .ne. 0 )
      g = mod(m,n)
      m = n
      n = g
    end do

    g = m


    return
  end subroutine gcd
end module module_greatest_common_divisor
