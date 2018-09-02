program test
implicit none
integer, parameter   :: nels=2, nnds=6, npel=4
integer              :: eptr(nels+1), nodes(nels*npel), epart(nels),npart(nnds), n
integer, pointer     :: vwgt=>null(), vsize=>null(), mopts=>null()
real(8), pointer     :: tpwgts=>null()
eptr=(/0,4,8/)
nodes=(/0,1,2,3,1,4,5,2/) ! Element 1 has nodes 0 1 2 3
                            ! Element 2 has nodes 1 4 5 2
call METIS_PartMeshNodal(nels,nnds,eptr,nodes,vwgt,vsize,2,tpwgts,mopts,n,epart,npart) 
  print*, npart; print*, epart
end program test
