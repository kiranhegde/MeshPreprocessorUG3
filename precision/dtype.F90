module dtype
   implicit none
   !integer,parameter :: dp = kind(1.0d0)
   !integer,parameter :: i4 = kind(1)
   integer, parameter :: i1=selected_int_kind(2)
   integer, parameter :: i2=selected_int_kind(4)
   integer, parameter :: i4=selected_int_kind(9)
   integer, parameter :: i8=selected_int_kind(18)
   integer, parameter :: sp=selected_real_kind(6,37)
   integer, parameter :: dp=selected_real_kind(15,307)
   integer, parameter :: qp=selected_real_kind(31,307)
end module dtype
