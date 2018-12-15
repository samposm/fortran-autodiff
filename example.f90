program example
  use autodiff
  implicit none

  type(dual_number) :: x,y

  x = init_dual(1.0)
  y = f1(x)
  print *,'f1(x)  at x=1:',y%val
  print *,'f1''(x) at x=1:',y%eps

contains

  function f1(a) result(r)
    type(dual_number), intent(in) :: a
    type(dual_number) :: r
    r = 2.0*a - a / (-a + 3.0) + exp(3.0 - 2.0*a) - log(2.0 + 1.0/(2.0 * a))
  end function f1

end program example
