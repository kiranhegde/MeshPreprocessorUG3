module comdata
   use dtype
   implicit none
   ! Command line arguments
   integer :: icheckgrid, iprinttopo
   
   ! nproc = number of parallel processes
   ! rank  = index of this process (starts at 0)
   integer :: nproc, rank

   ! Directory for grid file
   character(len=128) :: gdir

   ! Type of preprocessor format
   integer :: igridformat
   integer,parameter :: iascii=1, ihdf5=2

   ! Number of digits used in filenames
   integer,parameter :: ndf = 4 ! for time counter
   integer,parameter :: ndr = 4 ! for rank

end module comdata
