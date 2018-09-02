module mtsdata
   use mgrid
   use mpetsc
   type tsdata
      type(grid)  :: g
      type(petsc) :: p
      type(periodic_dir):: pd
      integer(kind=i4) :: npt
   end type tsdata


end module mtsdata
