subroutine read_input_parameters(g,prdir)
   use dtype
   use meshdata
 
   ! set default parameters
   integer          :: io,cc
   integer(kind=i4) :: gridtype     = 1  !  gmsh
   integer(kind=i4) :: is_periodic  = 0 
   integer(kind=i4) :: npdir        = 0  !  no. of periodic directions
   integer(kind=i4) :: msh_periodic = 0 
   integer(kind=i4) :: ofile_fmt    = 1  !  output format is ascii
   integer(kind=i4) :: renumber_flag= 0  !  whether to renumber, default=no
   integer(kind=i4) :: is_rans      = 0  !  whether needed RANS modeling, default=no
   logical          :: file_exists
   character*64     :: fname,gfile,input1,input2

   type(periodic_dir) :: prdir

   inquire(file="preproc.in", exist=file_exists)
   if(.not.file_exists) then
     print*
     print*,'preproc.in file missing....'
     print*
     print*,'The contents of preproc.in file should be like this'
     print*,''
     print*,'format   gmsh|su2                Input grid file format, default=gmsh'
     print*,'file     <path to file>          Input grid file'
     print*,'type     ascii|hdf5              Output format, default=ascii'
     print*,'renumber yes|no                  Whether to renumber, default=no'
     print*,'gmsh     yes|no                  Use gmsh periodic info, default=no'
     print*,'npdir    <no. of periodic dir>'
     print*,'For each periodic dir, give'
     print*,'dir   face_tag1   face_tag2'
     print*,''
     print*,'It is not necessary to specify default values. If there are no periodic'
     print*,'directions, then nothing needs to be specified.'
     print*
     stop
   endif

   open(3,file='preproc.in')
      do
        read(3,*,iostat=io)input1,input2 
        if (io /= 0 ) exit   

        if(len_trim(input1)=="format") then
          if(len_trim(input2)=="gmsh") then 
             gridtype = 1
             print*,"Input grid format = ",len_trim(input2)  
          elseif(len_trim(input2)=="su2") then 
             gridtype = 2
             print*,"Input grid format = ",len_trim(input2)  
          else
            print*, "Unknown grid format >>>>>>> " ,len_trim(input2)
            stop
          endif

        elseif(len_trim(input1)=="file") then
          print*,"Input grid file   = ",len_trim(input2)  
          gfile=len_trim(input2) 

        elseif(len_trim(input1)=="type") then
          if(len_trim(input2)=="ascii") then 
             ofile_fmt= 1
             print*,"Output file  format = ",len_trim(input2)  
          elseif(len_trim(input2)=="su2") then 
             ofile_fmt= 2
             print*,"Output file  format = ",len_trim(input2)  
          else
            print*, "Unknown output file  format >>>>>>> " ,len_trim(input2)
            stop
          endif

        elseif(len_trim(input1)=="renumber") then
          if(len_trim(input2)=="yes") then 
             renumber_flag = 1
             print*,"Whether to renumber grid =",len_trim(input2)  
          elseif(len_trim(input2)=="no") then 
             ofile_fmt= 2
             print*,"Whether to renumber grid =",len_trim(input2)  
          else
            print*, "Unknown renumber option    >>>>>>> " ,len_trim(input2)
            stop
          endif

        elseif(len_trim(input1)=="gmsh") then
          if(len_trim(input2)=="yes") then 
             msh_periodic = 1 
             print*,"Whether to renumber grid =",len_trim(input2)  
          elseif(len_trim(input2)=="no") then 
             msh_periodic = 0 
             print*,"Whether to renumber grid =",len_trim(input2)  
          else
            print*, "Unknown option for msh_periodic   >>>>>>> " ,len_trim(input2)
            stop
          endif

        elseif(len_trim(input1)=="npdir") then
          read(input2,*)cc
          g%npdir=cc  
          allocate(prdir%periodic_dir(npdir),prdir%slavetag(npdir),prdir%mastertag(npdir))
          if(g%npdir > 0) then 
            is_periodic = 1
            do i=1,g%npdir
               read(3,*,iostat=io)prdir%periodic_dir(i),prdir%slavetag(i),prdir%mastertag(i)  
               if (io /= 0 ) exit   
               write(*,*)prdir%periodic_dir(i),prdir%slavetag(i),prdir%mastertag(i) 
            enddo 
          else
            is_periodic = 0
          endif

        endif 
     enddo 
   close(3)


end  subroutine read_input_parameters



