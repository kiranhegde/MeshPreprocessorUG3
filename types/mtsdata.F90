module mtsdata
   use mgrid
   use mpetsc
   implicit none

   integer, parameter :: IOmesh= 24     !Gmshmesh file
   integer, parameter :: IOpara= 25     !Input paramter
   !integer, parameter :: IOmesh= 24     !Gmshmesh file

   type tsdata
      type(grid)  :: g
      type(petsc) :: p
      !integer(kind=i4) :: nv,nc,nelem,nbf,nf,ne,np,no_pf,no_npf
      !integer(kind=i4) :: ntri, nquad, ntet, npris, npyr, nhex
      type(periodic_dir):: xyz

      !integer(kind=i4) :: write_test_vtk=0, write_test_msh=0;
      !integer(kind=i4) :: write_part_msh=0, write_sparse_matrix=0;
      !integer(kind=i4) :: gridtype=1, is_periodic=0, renumber_flag=0,ofile_fmt=1
      !integer(kind=i4) :: npdir=0, msh_periodic=0, npt, is_rans=0, nsolid_walls
   end type tsdata


end module mtsdata
