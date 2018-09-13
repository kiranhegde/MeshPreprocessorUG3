subroutine read_input_parameters(xyz)
#include <petsc/finclude/petscsys.h>
   use petscsys
   use comdata
   use mgrid
   use mtsdata
   implicit none 
   PetscErrorCode :: ierr
   PetscBool :: set
   ! set default parameters
   integer          :: io,cc,key,i
   logical          :: file_exists
   character(len=64):: string,str1,str2
   !type(tsdata)       :: ctx
   type(periodic_dir) :: xyz
   PetscInt           :: nunused

   if(rank==0) then
      print*
      print*,'Reading parameters from preproc.in'
      print*
   endif


   call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                              '-format',string,set,ierr); CHKERRQ(ierr)

   if(trim(string) == 'gmsh')then
      gridtype = 1
      call print0("Input grid format : "//string)  
   else if(trim(string) == 'su2')then
      gridtype = 2
      call print0("Input grid format : "//string)  
   else
      call abort1('readparam: unknown grid format :'//string)
   endif
   
   call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                              '-file',string,set,ierr); CHKERRQ(ierr)
   
   if(trim(string) == ''.or.trim(string) == 'none') then
     gfile='grid.msh'
     call print0("Input grid file  : grid.msh")  
   else
     gfile=trim(string)
     call print0("Input grid file   : "//string)  
   endif 

   cc=0
   call PetscOptionsGetint(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                              '-npdir',cc,set,ierr); CHKERRQ(ierr)

   call print0(" npdir   : "//itoa(cc))

   if(cc>0)  then 
     allocate(xyz%slavetag(cc),stat=io)
     allocate(xyz%mastertag(cc),stat=io)
     if(io/=0) call print0("!!!!!!!!!!!!!!!ERROR: could not allocate xyz")  

     npdir = cc 

     string=""
     call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                                     '-x',string,set,ierr); CHKERRQ(ierr)
     i=0
     if(string/='') then 
       i=i+1
       call split_string(trim(string), str1, str2,'-')
       call str2int(str1,cc,io)
       xyz%slavetag(i)=cc
       call str2int(str2,cc,io)
       xyz%mastertag(i)=cc
       call print0("X-direction periodic :"//string)  
     endif  

     string=""
     call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                                     '-y',string,set,ierr); CHKERRQ(ierr)
     if(string/='') then 
       i=i+1
       call split_string(trim(string), str1, str2,'-')
       call str2int(str1,cc,io)
       xyz%slavetag(i)=cc
       call str2int(str2,cc,io)
       xyz%mastertag(i)=cc
       call print0("Y-direction periodic :"//string)  
     endif
 
     string=""
     call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                                     '-z',string,set,ierr); CHKERRQ(ierr)
     if(string/='') then 
       i=i+1
       call split_string(trim(string), str1, str2,'-')
       call str2int(str1,cc,io)
       xyz%slavetag(i)=cc
       call str2int(str2,cc,io)
       xyz%mastertag(i)=cc
       call print0("Z-direction periodic :"//string)  
     endif
   endif

   call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                              '-type',string,set,ierr); CHKERRQ(ierr)

   if(trim(string) == 'ascii')then
      ofile_fmt = 1
      call print0("Output file format : "//string)  
   else if(trim(string) == 'hdf5')then
      ofile_fmt = 2
      call print0("Output file format : "//string)  
   else
      call abort1('type: unknown output file format'//string)
   endif

   call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                              '-renumber',string,set,ierr); CHKERRQ(ierr)
   if(trim(string) == 'no')then
      renumber_flag=0
      call print0("Whether to renumber grid : "//string)  
   else if(trim(string) == 'yes')then
      renumber_flag=1
      call print0("Whether to renumber grid : "//string)  
   else
      call abort1('Unknown renumber option:'//string)
   endif

   call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                              '-gmsh',string,set,ierr); CHKERRQ(ierr)
   if(trim(string) == 'no')then
      msh_periodic = 0
      call print0("Use gmsh periodic info : "//string)  
   else if(trim(string) == 'yes')then
      msh_periodic = 0
      call print0("Use gmsh periodic info : "//string)  
   else
      call abort1('Unknown option for msh_periodic :'//string)
   endif

   cc=0
   call PetscOptionsGetInt(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                              '-walls',cc,set,ierr); CHKERRQ(ierr)
   if(cc>0)  then 
     !allocate(xyz%swtag(cc))
     nsolid_walls=cc
     is_rans=1

     do i=1,cc
       !xyz%swtag(i)=
       xyz%swtag  =1
     enddo 
       call print0("Number of solid walls (for RANS) = "//itoa(cc))  

   endif

! Check options only on root process.
   ! TODO: After this, abort is called on on root process, need to fix this.
   if(rank > 0) return



end  subroutine read_input_parameters

