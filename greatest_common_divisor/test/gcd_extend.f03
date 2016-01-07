!! Testing program to test
!! procedures of greatest common divisor
!! "gcd_extend".

program gcd_extend_test
  use module_greatest_common_divisor
  implicit none

  integer :: &
    m , n , g , a , b

  read(*,*) m , n

  call gcd_extend(m,n,g,a,b)

  write(*,*) g
  write(*,*) a,b
  write(*,*) a*m+b*n

  stop
end program gcd_extend_test
