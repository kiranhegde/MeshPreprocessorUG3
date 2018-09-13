module mgrid
   use dtype
   implicit none

   type vertex
      real(kind=dp) :: x,y,z 
   end type vertex
        
   type elements
      integer(kind=i4) :: gmshtype,ntags 
      !real(kind=dp)    :: x,y,z 
   end type elements


   type grid
      integer(kind=i4) :: nv,nc,nelem,nbf,nf,ne,np,no_pf,no_npf
      integer(kind=i4) :: ntri, nquad, ntet, npris, npyr, nhex
      !integer(kind=i4) :: gridtype=1, is_periodic=0, renumber_flag=0,ofile_fmt=1
      !integer(kind=i4) :: npdir=0, msh_periodic=0, npt, is_rans=0, nsolid_walls
      integer(kind=i4) :: mesup, mpsup
      character*100    :: filename
      !integer(kind=i4),dimension(:),allocatable :: periodic_dir, slavetag, mastertag, swtag
      integer(kind=i4),dimension(:),allocatable :: esup1, esup2
      integer(kind=i4),dimension(:),allocatable :: psup1, psup2
      integer(kind=i4),dimension(:),allocatable :: pid, otn
      real(kind=dp),dimension(:),allocatable    :: volume
      type(vertex),dimension(:),allocatable     :: vrt
      type(elements),dimension(:),allocatable     :: elem
   end type grid

   type periodic_dir
      integer(kind=i4),dimension(:),allocatable :: slavetag
      integer(kind=i4),dimension(:),allocatable :: mastertag
      integer(kind=i4),dimension(:),allocatable :: swtag
   end type periodic_dir
        
contains 

FUNCTION is_numeric(string)
    IMPLICIT NONE
    CHARACTER(len=*), INTENT(IN) :: string
    LOGICAL :: is_numeric
    INTEGER :: e
    INTEGER :: x
    READ(string,*,IOSTAT=e) x
    is_numeric = e == 0
END FUNCTION is_numeric


function itoa(i) result(res)
  IMPLICIT NONE
  character(:),allocatable :: res
  integer,intent(in) :: i
  character(range(i)+2) :: tmp
  write(tmp,'(i0)') i
  res = trim(tmp)
end function

subroutine str2int(str,it,stat)
    implicit none
    ! Arguments
    character(len=*),intent(in) :: str
    integer,intent(out)         :: it
    integer,intent(out)         :: stat

    read(str,*,iostat=stat)  it
end subroutine str2int

SUBROUTINE split_string(instring, string1, string2,string3)
    CHARACTER(len=*) :: instring
    CHARACTER(len=*) :: string3
    CHARACTER(len=*),INTENT(OUT):: string1,string2
    INTEGER :: index

    instring = TRIM(instring)
    string3 = TRIM(string3)

    index = SCAN(instring,string3)
    string1 = instring(1:index-1)
    string2 = instring(index+1:)

END SUBROUTINE split_string



end module mgrid
