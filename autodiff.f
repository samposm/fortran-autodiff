module autodiff
  implicit none
  private
  public :: dual_number,init_dual, &
            operator(+),operator(-),operator(*),operator(/)

  type dual_number
     real :: val ! value
     real :: eps ! differential
  end type dual_number

  interface operator(+)
     module procedure plus_dual_dual
     module procedure plus_dual_real
     module procedure plus_real_dual
  end interface operator(+)

  interface operator(-)
     module procedure minus_dual_dual
     module procedure minus_dual
     module procedure minus_dual_real
     module procedure minus_real_dual
  end interface operator(-)

  interface operator(*)
     module procedure times_dual_dual
     module procedure times_dual_real
     module procedure times_real_dual
  end interface operator(*)

  interface operator(/)
     module procedure division_dual_dual
     module procedure division_dual_real
     module procedure division_real_dual
  end interface operator(/)

contains

  function init_dual(x) result(r)
    real :: x
    type(dual_number) :: r
    r%val = x ; r%eps = 1.0
  end function init_dual

  function plus_dual_dual(a,b) result(r)
    type(dual_number), intent(in) :: a,b
    type(dual_number) :: r
    r%val = a%val + b%val ; r%eps = a%eps + b%eps
  end function plus_dual_dual

  function plus_dual_real(a,x) result(r)
    type(dual_number), intent(in) :: a
    real, intent(in) :: x
    type(dual_number) :: r
    r%val = a%val + x ; r%eps = a%eps
  end function plus_dual_real

  function plus_real_dual(x,a) result(r)
    type(dual_number), intent(in) :: a
    real, intent(in) :: x
    type(dual_number) :: r
    r%val = a%val + x ; r%eps = a%eps
  end function plus_real_dual

  function minus_dual_dual(a,b) result(r)
    type(dual_number), intent(in) :: a,b
    type(dual_number) :: r
    r%val = a%val - b%val ; r%eps = a%eps - b%eps
  end function minus_dual_dual

  function minus_dual(a) result(r)
    type(dual_number), intent(in) :: a
    type(dual_number) :: r
    r%val = -a%val ; r%eps = -a%eps
  end function minus_dual

  function minus_dual_real(a,x) result(r)
    type(dual_number), intent(in) :: a
    real, intent(in) :: x
    type(dual_number) :: r
    r%val = a%val - x ; r%eps = a%eps
  end function minus_dual_real

  function minus_real_dual(x,a) result(r)
    type(dual_number), intent(in) :: a
    real, intent(in) :: x
    type(dual_number) :: r
    r%val = a%val - x ; r%eps = a%eps
  end function minus_real_dual

  function times_dual_dual(a,b) result(r)
    type(dual_number), intent(in) :: a,b
    type(dual_number) :: r
    r%val = a%val * b%val ; r%eps = a%eps * b%val + a%val * b%eps
  end function times_dual_dual

  function times_dual_real(a,x) result(r)
    type(dual_number), intent(in) :: a
    real, intent(in) :: x
    type(dual_number) :: r
    r%val = a%val * x ; r%eps = a%eps * x
  end function times_dual_real

  function times_real_dual(x,a) result(r)
    type(dual_number), intent(in) :: a
    real, intent(in) :: x
    type(dual_number) :: r
    r%val = a%val * x ; r%eps = a%eps * x
  end function times_real_dual

  function division_dual_dual(a,b) result(r)
    type(dual_number), intent(in) :: a,b
    type(dual_number) :: r
    r%val = a%val / b%val ; r%eps = (a%eps * b%val - a%val * b%eps) / b%val**2
  end function division_dual_dual

  function division_dual_real(a,x) result(r)
    type(dual_number), intent(in) :: a
    real, intent(in) :: x
    type(dual_number) :: r
    r%val = a%val / x ; r%eps = a%eps / x
  end function division_dual_real

  function division_real_dual(x,a) result(r)
    type(dual_number), intent(in) :: a
    real, intent(in) :: x
    type(dual_number) :: r
    r%val = 1.0 / a%val ; r%eps = -x * a%eps / a%val**2
  end function division_real_dual

end module autodiff
