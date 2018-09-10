! Ends the program if cond==false after printing message
subroutine assert(cond, message)
#include <petsc/finclude/petscsys.h>
   use petscsys
   implicit none
   logical,intent(in) :: cond
   character(len=*),intent(in) :: message

   if(cond .eqv. .false.)then
      SETERRA(PETSC_COMM_WORLD,1,message)
   endif

end subroutine assert

! Ends the program if cond==false after printing message on rank=0
subroutine assert0(cond, message)
#include <petsc/finclude/petscsys.h>
   use petscsys
   use comdata, only : rank
   implicit none
   logical,intent(in) :: cond
   character(len=*),intent(in) :: message

   if(cond .eqv. .false.)then
      if(rank == 0) print*,message
      SETERRA(PETSC_COMM_WORLD,1,' ')
   endif

end subroutine assert0

! Ends the program
! Print message only on rank=0
subroutine abort0(message)
#include <petsc/finclude/petscsys.h>
   use petscsys
   use comdata, only : rank
   implicit none
   character(len=*) :: message

   if(rank == 0) print*,message
   SETERRA(PETSC_COMM_WORLD,1,' ')

end subroutine abort0

! Ends the program after printing message
! Message is printed on every process.
subroutine abort1(message)
#include <petsc/finclude/petscsys.h>
   use petscsys
   implicit none
   character(len=*) :: message

   SETERRA(PETSC_COMM_WORLD,1,message)

end subroutine abort1

! Print message on rank=0 process only
subroutine print0(message)
   use comdata, only : rank
   implicit none
   character(len=*),intent(in) :: message

   if(rank == 0) print*,trim(message)

end subroutine print0
