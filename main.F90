program main
#include <petsc/finclude/petscsys.h>
   use petscsys
   use comdata
   use mtsdata
   implicit none

   integer(kind=i4) :: argc,i
!   integer(kind=i4) :: write_test_vtk=0, write_test_msh=0;
!   integer(kind=i4) :: write_part_msh=0, write_sparse_matrix=0;
   real(kind=dp)    :: ug3pre_start, ug3pre_finish, ug3pre_time;
   real(kind=dp)    :: fwrite_start, fwrite_finish, fwrite_time;
   real(kind=dp)    :: readmsh_time=0, readsu2_time=0, metis_time=0;
   real(kind=dp)    :: esup_time=0, psup_time=0, renumber_time=0;
   real(kind=dp)    :: bface_time=0, anorm_time=0, volume_tets_time=0;
   real(kind=dp)    :: edges_time=0, partition_time=0;
   real(kind=dp)    :: periodic_gen_time=0, periodic_ds_time=0;
   real(kind=dp)    :: dist_to_wall_time=0;
   real(kind=dp)    :: tstart,tend
   character*64     :: arg
   logical :: file_exists
 
   type(tsdata)       :: ctx
 
   PetscErrorCode     :: ierr 


   if(rank==0) then 
   INQUIRE(FILE="preproc.in", EXIST=file_exists)
   endif

   if (file_exists) then
      call PetscInitialize('preproc.in', ierr); CHKERRQ(ierr)
   else 
     print*
     print*,'preproc.in file missing....'
     print*
     print*,'The contents of preproc.in file should be like this'
     print*,''
     print*,'format   gmsh|su2                Input grid file format,default=gmsh'
     print*,'file     <path to file>          Input grid file'
     print*,'type     ascii|hdf5              Output format, default=ascii'
     print*,'renumber yes|no                  Whether to renumber, default=no'
     print*,'gmsh     yes|no                  Use gmsh periodic info,default=no'
     print*,'npdir    <no. of periodic dir>'
     print*,'For each periodic dir, give'
     print*,'dir   face_tag1   face_tag2'
     print*,''
     print*,'It is not necessary to specify default values. If there are no periodic'
     print*,'directions, then nothing needs to be specified.'
     print*
     stop "Unable to initialize PETSc"
   endif

   call MPI_Comm_rank(PETSC_COMM_WORLD, rank, ierr); CHKERRQ(ierr)
   call MPI_Comm_size(PETSC_COMM_WORLD, nproc, ierr); CHKERRQ(ierr)



   if(rank==0) then 
      call cpu_time(tstart) 
      call parse_cmdline(ctx) 
      print*,'Number of processes: ', nproc
   endif

   call read_input_parameters(ctx%xyz)

!  read grid file in msh format
   if(gridtype==1) call read_msh(ctx%g,ctx%xyz);  
   !if(grid.gridtype==2) readsu2_time=grid.read_su2();   // read grid file in su2 format

   if(rank==0) then 
      call cpu_time(tend) 
      print '("Time ",f6.3,"seconds")',tend-tstart
   endif
   call PetscFinalize(ierr); CHKERRQ(ierr)
end program main  


subroutine  parse_cmdline(ctx)
#include <petsc/finclude/petscsys.h>
   use petscsys
   use comdata
   use mtsdata
   implicit none

!  character(len=*), parameter :: version = '1.0'
   character(len=64) :: arg
   character(len=8) :: date
   character(len=10) :: time
   character(len=15) :: zone
   logical :: do_time = .false.
   integer(kind=i4) :: i
  
   type(tsdata)   :: ctx

  PetscErrorCode :: ierr
  PetscBool      :: flag

  call PetscInitialize(PETSC_NULL_CHARACTER,ierr)

  core = 0
  call PetscOptionsGetint(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                              '-c',core,flag,ierr); CHKERRQ(ierr)

  if(core > 0) then 
     core = core
    call print0("No. of partitions = "//itoa(core))
  else 
    print*
    call print0("No. of partitions should be > 0")
    call print0("No. of partitions = "//itoa(core))
    print '(a,a,/)', 'Check Command-line option: ' 
    call print_help()
    stop
    !call print0("No. of partitions = "//itoa(core))
  endif

!stop
  call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
                              '-w',arg,flag,ierr); CHKERRQ(ierr)

         if (trim(arg) == "write_test_vtk") then
            write_test_vtk = 1
            call print0("write option: write_test_vtk")
         elseif (trim(arg) == "write_test_msh") then
            write_test_msh = 1
            call print0("write option: write_test_msh")
         elseif (trim(arg) == "write_part_msh") then
            write_part_msh = 1
            call print0("write option: write_part_msh")
         elseif (trim(arg) == "write_sparse_matrix") then
            write_sparse_matrix = 1 
            call print0("write option: write_sparse_matrix")
    !     else
    !           print '(a,a,/)', ' Unrecognized command-line option: ', arg
    !           call print_help()
    !           stop
         endif  

   return

!  print*
!  print*
!  print*,'Reading parameters from commandline'
!  if ( command_argument_count()  == 0)  then 
!      core = 1
!  else
!       do i = 1, command_argument_count()
!          call get_command_argument(i, arg)
!          select case (arg)
!          case ('-write_test_vtk')
!             print '(2a)', arg
!             write_test_vtk = 1
!          case ('-write_test_msh')
!             print '(2a)',arg
!             write_test_msh = 1
!          case ('-write_part_msh')
!             print '(2a)',arg
!             write_part_msh = 1
!          case ('-write_sparse_matrix')
!             print '(2a)',arg
!             write_sparse_matrix = 1 
!          case default
!             if(is_numeric(arg)) then 
!                core = core
!             else           
!                print '(a,a,/)', ' Unrecognized command-line option: ', arg
!               call print_help()
!               stop
!             endif  
!          end select
!       end do
!  endif
!  print*
!  print*

  ! Print the date and, optionally, the time
  !call date_and_time(DATE=date, TIME=time, ZONE=zone)
  !write (*, '(a,"-",a,"-",a)', advance='no') date(1:4), date(5:6), date(7:8)
  !if (do_time) then
  !   write (*, '(x,a,":",a,x,a)') time(1:2), time(3:4), zone
  !else
  !   write (*, '(a)') ''
  !end if

contains

  subroutine print_help()
      implicit none 
      print*,'Input argument format'
      print*
      print*,'ug3pre No_of_partition  optional_write '
      print*
      print*,'No_of_partition : no. of cpu cores or partions for parallel run > 0'
      print*,'optional_write  : write_test_vtk' 
      print*,'                  write_test_msh'
      print*,'                  write_part_msh'
      print*,'                  write_sparse_matrix'
      print*   
      print*,'Eg.  mpirun -np 5  ./ug3preF90 -c 6 -w write_test_vtk'   
      print*   
  end subroutine print_help

end subroutine  parse_cmdline

