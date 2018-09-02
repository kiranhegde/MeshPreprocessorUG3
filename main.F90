program main
#include <petsc/finclude/petscsys.h>
   use petscsys
   use comdata
   use mtsdata

   implicit none

   integer          :: core
   integer(kind=i4) :: argc,i
   integer(kind=i4) :: write_test_vtk=0, write_test_msh=0;
   integer(kind=i4) :: write_part_msh=0, write_sparse_matrix=0;
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
 
   type(tsdata)       :: ctx
 
   PetscErrorCode     :: ierr 

   call cpu_time(tstart) 
!   argc = iargc()

   if(rank==0) then 
      print*,'Reading parameters from commandline'

!   if (argc == 0) then
!      ctx%npt = 1
!      do i = 1, argc
!         CALL getarg(i, arg)


!         call PetscOptionsGetString(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,&
!                              '-tscheme',string,set,ierr); CHKERRQ(ierr)
!   if(trim(string) == 'ssprk3')then


    endif

   !if(write_test_vtk==1) print*,'vtk'
   !if(write_test_msh==1) print*,'msh'
   !if(write_part_msh==1) print*,'part-msh'
   !if(write_sparse_matrix==1) print*,'sm'

   !call read_input_parameters()
   endif


   call cpu_time(tend) 
   print '("Time ",f6.3,"seconds")',tend-tstart
end program main  


subroutine  parse_cmdline
  implicit none

!  character(len=*), parameter :: version = '1.0'
  character(len=32) :: arg
  character(len=8) :: date
  character(len=10) :: time
  character(len=5) :: zone
  logical :: do_time = .false.
  integer :: i


         if (trim(arg) == "-write_test_vtk") then
            write_test_vtk = 1
         elseif (trim(arg) == "-write_test_msh") then
            write_test_msh = 1
         elseif (trim(arg) == "-write_part_msh") then
            write_part_msh = 1
         elseif (trim(arg) == "-write_sparse_matrix") then
            write_sparse_matrix = 1 
         else
               read(arg,*)core  
               ctx%npt = core
               print*,'Total partitions :',core
         endif
      enddo


  do i = 1, command_argument_count()
     call get_command_argument(i, arg)

     select case (arg)
     case ('-v', '--version')
        print '(2a)', 'cmdline version ', version
        stop
     case ('-h', '--help')
        call print_help()
        stop
     case ('-t', '--time')
        do_time = .true.
     case default
        print '(a,a,/)', 'Unrecognized command-line option: ', arg
        call print_help()
        stop
     end select
  end do

  ! Print the date and, optionally, the time
  call date_and_time(DATE=date, TIME=time, ZONE=zone)
  write (*, '(a,"-",a,"-",a)', advance='no') date(1:4), date(5:6), date(7:8)
  if (do_time) then
     write (*, '(x,a,":",a,x,a)') time(1:2), time(3:4), zone
  else
     write (*, '(a)') ''
  end if

contains

  subroutine print_help()
      print*,'Input argument format'
      print*
      print*,'ug3pre No_of_partition  optional_write '
      print*
      print*,'No_of_partition : no. of cpu cores or partions for parallel run'
      print*,'optional_write  : -write_test_vtk' 
      print*,'                  -write_test_msh'
      print*,'                  -write_part_msh'
      print*,'                  -write_sparse_matrix'
      print*   
  end subroutine print_help

end subroutine  parse_cmdline


