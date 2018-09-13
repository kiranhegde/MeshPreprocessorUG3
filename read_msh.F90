subroutine read_msh(g,xyz)
#include <petsc/finclude/petscsys.h>
   use petscsys
   use comdata
   use mgrid
   use mtsdata
   implicit none
   !
   ! there are 19 gmsh element types: \
   !
   !    1 : 2-node line
   !    2 : 3-node triangle (face)
   !    3 : 4-node quadrangle (face)
   !    4 : 4-node tetrahedron
   !    5 : 8-node hexahedron (eg cube)
   !    6 : 6-node triangular-prism
   !    7 : 5-node pyramid
   !
   !  8-14: 'second-order' elements.  Ref Gmsh manual.
   !   15 : 1-node point
   ! 16-19: more second-order FEM elements
   !
   ! the nodes/vertices for each element are read into the
   ! v array.
   !
   ! each element can have several tags.
   ! the first tag gives the physical_group number.
   ! all elements on the same boundary have the same physical_group number.
   !
   integer(kind=i4),parameter :: elem_type(19) = &
                      (/ 2,3,4,4,8,6,5,3,6,9,10,27,18,14,1,8,20,15,13 /)
   integer(kind=i4) :: tags(64), v(27)
   integer(kind=i4) :: coun,tag(3),it,i,j 
   integer(kind=i4) :: ptags(3*npdir),gmshtype, ntags 
   integer(kind=i4) :: ntri, nquad, ntet, npris, npyr, nhex,nn
   integer          :: ivs = 0,lens
   real(kind=dp)    :: start,finish 
   character(len=72):: c_input1, c_input2, c_input3,string
   logical          :: exists

   type(grid)        :: g
   type(periodic_dir):: xyz

   call print0('Reading grid file in msh format:'//gfile)

   do i=1,npdir
      ptags(i)=xyz%slavetag(i)
      ptags(i+1)=xyz%mastertag(i)
      call print0("periodic :"//itoa(ptags(i+1)))
   enddo 

   ntri  = 0  
   nquad = 0 
   ntet  = 0
   npris = 0 
   npyr  = 0 
   nhex  = 0

   inquire(file=gfile,exist=exists)
   if( .not. exists )then
      call print0("*** Error: File does not exist"//gfile)
      stop
   endif

   open(IOmesh,file=gfile)
   read(IOmesh,*) c_input1
   call check_input_character(c_input1,'$MeshFormat')

   read(IOmesh,*) c_input1,c_input2,c_input3
   if( c_input1 == '2.2' )then
     ivs = 22
   else if( c_input1 == '2.1' )then
     ivs = 21
   else if( c_input1 == '2' )then
     ivs = 20
   else
     call print0('*** WARNING: unknown Gmsh version') 
     call print0('*** Unexpected results might happen')
     ivs = 21
   endif


   if( ivs == 20 )then
     call check_input_character(c_input1,'2')
   else if( ivs == 21 )then
     call check_input_character(c_input1,'2.1')
   else if( ivs == 22 )then
     call check_input_character(c_input1,'2.2')
   else
     call print0('*** Version found '//c_input1)
   endif

   call check_input_character(c_input2,'0')
   call check_input_character(c_input3,'8')

   string=c_input1(1:lens(c_input1))//' '//c_input2(1:lens(c_input2))//' '//c_input3(1:lens(c_input3))
   call print0('MeshFormat: '//string)  

   read(IOmesh,*) c_input1
   call check_input_character(c_input1,'$EndMeshFormat')
   !
   ! read the Gmsh PhysicalNames
   !
   call print0('Reading PhysicalNames')
   read(IOmesh,*) c_input1
   call check_input_character(c_input1,'$PhysicalNames')
   read(IOmesh,*) nn 
   if( nn <= 0 )then
     call print0('error: number of names must be a positive number')
     stop
   endif

   do i=1,nn
      read(IOmesh,*) 
   end do

   read(IOmesh,*) c_input1
   call check_input_character(c_input1,'$EndPhysicalNames')
   !
   ! read the nodes from the .msh file 
   !
   call print0('Reading Nodes')
   read(IOmesh,*) c_input1
   call check_input_character(c_input1,'$Nodes')

   read(IOmesh,*) g%nv
   if( g%nv <= 0 )then
     call print0('error: number of nodes must be a positive number')
     stop
   else
     call print0('Number of nodes : '//itoa(g%nv) )
     allocate(g%vrt(g%nv))
   endif

   nodes: do i=1,g%nv
     read(IOmesh,*) it,g%vrt(i)%x,g%vrt(i)%y,g%vrt(i)%z 
   enddo nodes
 
   read(IOmesh,*) c_input1
   call check_input_character(c_input1,'$EndNodes')
   !
   ! read the elements from the .msh file 
   !
   call print0('Reading Elements')
   read(IOmesh,*) c_input1
   call check_input_character(c_input1,'$Elements')

   read(IOmesh,*) g%nelem
   if( g%nelem <= 0 )then
     write(*,*) 'error: number of elements must be a positive number'
     stop
   else
     call print0('Number of Elements : '//itoa(g%nelem) )
     allocate(g%elem(g%nelem))
   endif

   do j=1,g%nelem
     read(IOmesh,*) it, gmshtype, ntags
     if( ivs <= 21 )then
       string=itoa(it)//" "//itoa(ntags)
       if( ntags /= 3 ) call print0('tag error n_tags /= 3:' //string)
     else
       string=itoa(it)//" "//itoa(ntags)
       if( ntags /= 2 )call print0('tag error n_tags /= 2:' //string) 
     endif
     call check_element_type(gmshtype,elem_type)
     call check_n_tags(ntags,tags)
     backspace(IOmesh)

     read(IOmesh,*) it, gmshtype,ntags,(tags(i),i=1,ntags),(v(i),i=1,elem_type(gmshtype))
     g%elem(j)%ntags=ntags
     g%elem(j)%gmshtype=gmshtype
 
   enddo 



contains

subroutine check_input_character(c1,c2)
     implicit none
     character (len=*) :: c1, c2

     if( c1(1:len(c2)) /= c2 )then
       call print0('error reading Gmsh input file: ')
       !call print0('the following two characters should be the ') 
       !call print0('same but differ '//c1(1:len(c2)),c2)
       stop
     endif

end subroutine

subroutine check_element_type(ielement_type,element_type)

     implicit none
     integer ielement_type
     integer element_type(:)

     if( ielement_type < 0 )then
       write(*,*) 'error reading Gmsh file: element type must be positive'
       write(*,*) 'element type = ',ielement_type
       stop
     endif

     if( ielement_type > size(element_type) )then
       write(*,*) 'error reading Gmsh file: unrecognised element type'
       write(*,*) 'element type ',ielement_type
       write(*,*) 'max recognised element type ',size(element_type)
       stop
     endif

end subroutine

subroutine check_n_tags(ntags,itags)

     implicit none

     integer ntags
     integer itags(:)

     if( ntags > size(itags) )then
       write(*,*) 'error: The Gmsh file contains ',ntags,' tags per element'
       write(*,*) 'Gmsh2ug3 is hard-wired for a maximum of ',size(itags),&
                  'tags.  The dimension of this array needs to be increased.'
       stop
     endif

end subroutine





end subroutine read_msh

integer function lens(string)

   character(len=*) string

   do i=len(string),0,-1
     if( string(i:i) .ne. ' ') goto 10
   end do
   i = 0
10 continue

   lens = i

end function lens

