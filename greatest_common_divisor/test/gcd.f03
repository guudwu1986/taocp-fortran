!! Testing program to test
!! procedures of greatest common divisor.

program gcd_test
  use module_greatest_common_divisor
  implicit none

  integer :: &
    a , b , g

  read(*,*) a , b

  call gcd(a,b,g)

  write(*,*) g

  stop
end program gcd_test
