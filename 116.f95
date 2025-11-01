function func(x)
implicit none
integer,parameter::dbl=selected_real_kind(p=12)
  real(kind=dbl) :: func 
  real(kind=dbl),intent(in)::x 
  func=10.0_dbl * dsin(20.0_dbl*x)
end function func

subroutine deriv(func,x0,dx,rdf,error)
 implicit none
 integer,parameter::dbl=selected_real_kind(p=12)
 real(kind=dbl),external::func
 real(kind=dbl),intent(in)::x0,dx
 real(kind=dbl),intent(out)::rdf
 integer,intent(out)::error

    if(dx==0.0_dbl)then 
       error=1
    else 
      error=0
       rdf=(func(x0+dx)-func(x0))/dx 
    end if 
end subroutine deriv



program dderiv
use, intrinsic :: iso_fortran_env 
implicit none 
integer,parameter::dbl=selected_real_kind(p=12)
real(kind=dbl),external::func  ! Added external declaration
real(kind=dbl)::x0,dx,rdf
integer::error

x0=0.0_dbl
dx=0.0001_dbl
call deriv(func,x0,dx,rdf,error)
write(*,*)'Result',rdf
end program dderiv