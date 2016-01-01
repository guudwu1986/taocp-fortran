program main
  use module_greatest_common_divisor
  implicit none

  integer :: &
    a , b , g

  read(*,*) a , b

  call gcd(a,b,g)

  write(*,*) g

  stop
end program main
