!! V1P9E6
!! Use "3.f03".
!! Compute average number of iteration
!! for a given "n".

program average
  use module_greatest_common_divisor
  implicit none

  integer , parameter &
    :: T=5

  integer &
    :: i
  integer &
    :: g
  integer &
    :: c
  integer &
    :: total

  total = 0
  do i = 1,T
    call gcd(T,i,g,c)
    write(*,*) c
    total = total + c
  end do

  write(*,*) real(total)/T

  stop
end program average
