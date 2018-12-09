program example
  use autodiff
  implicit none

  type(dual_number) :: x,y

  x = init_dual(1.0)
  y = f1(x)
  print *,'f1(x)  at x=1:',y%val
  print *,'f1''(x) at x=1:',y%eps

  x = init_dual(2.0)
  y = f2(x)
  print *,'f2(x)  at x=2:',y%val
  print *,'f2''(x) at x=2:',y%eps

contains

  function f1(a) result(r)
    type(dual_number), intent(in) :: a
    type(dual_number) :: r
    r = 2.0*a - a / (-a + 3.0)
  end function f1

  function f2(a) result(r)
    type(dual_number), intent(in) :: a
    type(dual_number) :: r
    r = 2.0*exp(-3.0*a)
  end function f2

end program example
