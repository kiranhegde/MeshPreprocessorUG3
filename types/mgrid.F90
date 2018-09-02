module mgrid
   use dtype

   type grid
      integer(kind=i4) :: nv,nc,nelem,nbf,nf,ne,np,no_pf,no_npf
      integer(kind=i4) :: ntri, nquad, ntet, npris, npyr, nhex
      integer(kind=i4) :: gridtype, is_periodic, renumber_flag, ofile_fmt
      integer(kind=i4) :: npdir, msh_periodic, npt, is_rans, nsolid_walls
      integer(kind=i4) :: mesup, mpsup
      character*100    :: filename
      integer(kind=i4),dimension(:),allocatable :: periodic_dir, slavetag, mastertag, swtag
      integer(kind=i4),dimension(:),allocatable :: esup1, esup2
      integer(kind=i4),dimension(:),allocatable :: psup1, psup2
      integer(kind=i4),dimension(:),allocatable :: pid, otn
      real(kind=dp),dimension(:),allocatable :: volume
   end type grid

   type periodic_dir
      integer(kind=i4),dimension(:),allocatable :: slavetag
      integer(kind=i4),dimension(:),allocatable :: mastertag
      integer(kind=i4),dimension(:),allocatable :: swtag
   end type periodic_dir
        


end module mgrid
