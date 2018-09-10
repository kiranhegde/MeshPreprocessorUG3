module comdata
   use dtype
   implicit none
   ! Command line arguments
   integer :: icheckgrid, iprinttopo
   
   ! nproc = number of parallel processes
   ! rank  = index of this process (starts at 0)
   integer :: nproc, rank,core=0

   ! Directory for grid file
   character(len=128) :: gdir

   ! Type of preprocessor format
   integer :: igridformat
   integer,parameter :: iascii=1, ihdf5=2

   ! Number of digits used in filenames
   integer,parameter :: ndf = 4 ! for time counter
   integer,parameter :: ndr = 4 ! for rank
   
    
   integer(kind=i4) :: write_test_vtk=0, write_test_msh=0 
   integer(kind=i4) :: write_part_msh=0, write_sparse_matrix=0
   integer(kind=i4) :: gridtype=1, is_periodic=0, renumber_flag=0,ofile_fmt=1
   integer(kind=i4) :: npdir=0, msh_periodic=0, npt, is_rans=0, nsolid_walls
   character*64     :: gfile 

end module comdata
