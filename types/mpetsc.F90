! Module to store petsc Vec and VecScatter objects
!
! Vectors
!
! v_u   = corresponds to s%u    (ghosted vector)
! v_res = corresponds to s%res
! v_dq  = corresponds to s%dq   (ghosted vector)
! v_dtl = corresponds to s%dtl
!
! Scatters
!
! vs_res = used to apply periodic condition on s%res
! vs_dq  = used to apply periodic condition on s%dq
! vs_dtl = used to apply periodic condition on s%dtl


module mpetsc
#include <petsc/finclude/petscvec.h>
   use petscvec

! Check that we have recent version of PETSc
#if PETSC_VERSION_LT(3,8,0)
#error "PETSc version must be >= 3.8.0"
#endif

   type petsc
      Vec        :: v_u, v_res, v_dq, v_dtl, v_tmp
      VecScatter :: vs_res, vs_dq, vs_dtl
      PetscInt   :: nres, npmax

      PetscLogEvent :: u_ghost, dq_ghost, dobface1, dobface2, &
                       dotet_loc, docell_loc, dotet_ghost, docell_ghost, &
                       edge_loc, edge_ghost, grad_lim, writesol, userpost
   end type petsc

end module mpetsc
