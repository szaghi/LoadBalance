program loadbalance
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision
USE Data_Type_Block
USE Data_Type_Command_Line_Interface
USE Data_Type_OS
USE Data_Type_Proc
USE Lib_Balance
USE Lib_INI_IO
USE Lib_IO_Misc
USE Lib_Strings
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
type(Type_OS):: OS !< Running architecture.
! main data structures
type(Type_Block), pointer::     leaf=>null() !< Leafs list.
integer(I4P)::                  Nb=0         !< Number of blocks.
type(Type_Block), pointer::     block(:)     !< Blocks data [1:Nb].
integer(I4P)::                  Np=8         !< Number of processors.
type(Type_Proc),  allocatable:: proc(:)      !< Processors data [1:Np].
! files handling variables
character(Nc)::       fnamein=''                !< Input file name.
character(Nc)::       froot=''                  !< Root file name.
integer(I4P)::        uin=0                     !< Input file logic unit.
character(13)::       endianism='LITTLE_ENDIAN' !< Bit ordering of binary files.
logical::             save_blk_his=.false.      !< Flag for saving the splitting history of each original block.
logical::             save_lvl_lis=.false.      !< Flag for saving levele list.
logical::             save_blk_growth=.false.   !< Flag for saving blocks growth history.
logical::             is_file=.false.           !< Flag for inquiring the existance of input file.
character(Nc)::       fblkgroups=''             !< Blocks groups file.
character(Nc)::       cc_par=''                 !< cc.par file.
type(Type_File_INI):: fini                      !< Blocks groups file (INI) file handler.
! balancing parameters
integer(I4P)::               Nbs=0              !< Number of blocks after splitting.
real(R8P)::                  bal=3._R8P         !< Tollerance of maximum balancing.
integer(I8P)::               Wi=0               !< Ideal work load per processor.
integer(I8P)::               Wmax=0             !< Maximum work load over blocks.
integer(I4P)::               bmax=0             !< Index of heaviest block.
integer(I4P),  allocatable:: pf(:)              !< Prime factors for block splitting [...,9,7,5,3,2].
integer(I4P)::               mgl=4              !< Number of levels of multi-grid.
integer(I4P)::               imax=1000          !< Maximum number of balancing iterations.
real(R8P)::                  Wprmax=0._R8P      !< Maximum Wpr.
real(R8P)::                  thresh             !< Threshold of splitting.
real(R8P)::                  threshMin=1._R8P   !< Minimum split threshold.
! real(R8P)::                  threshMax=1._R8P   !< Maximum split threshold.
integer(I4P)::               threshNum=1_I4P    !< Number of split threshold.
real(R8P)::                  threshDelta=0._R8P !< Delta split threshold.
! auxiliary variables
integer(I4P):: i,j,k,b,l,p,t     !< Counters.
logical::      verbose = .false. !< Flag for activating verbose outputs.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
call command_line_interface
froot = OS%basename(trim(fnamein))                                      ! basename of fnamein
if (index(froot,'.grd')>0) froot = trim(froot(1:index(froot,'.grd')-1)) ! trimming .grd
do l=1,99 ! loop over admissible levels
  if (index(froot,'.'//strz(2,l))>0) then
    froot = trim(froot(1:index(froot,'.'//strz(2,l))-1))                ! trimming .#grl
  endif
enddo
inquire(file=trim(fnamein),exist=is_file)
if (.not.is_file) then
  write(stderr,'(A)')' Error: the input file "'//trim(fnamein)//'" does not exist!'
  stop
endif

if (.not.allocated(pf)) then
  allocate(pf(1:2)) ; pf = [3,2]
endif
! printing parameters of the balancing
call print_defaults
! allocating processors data
allocate(proc(1:Np))
! loading input data
#ifdef cXLF
open(unit = Get_Unit(uin), file = trim(fnamein), form = 'UNFORMATTED')
#else
open(unit = Get_Unit(uin), file = trim(fnamein), form = 'UNFORMATTED', convert = trim(endianism))
#endif
read(uin)Nb
! allocating blocks data
allocate(block(1:Nb))
! loading data of original blocks
do b=1,Nb
  read(uin)i,j,k ; call block(b)%set(a=b,l=0,ba=b,b=(/b/),Ni=i,Nj=j,Nk=k)
enddo
close(uin)
! parsing eventaul blocks groups file mapping
call load_map_blk_groups(filename=fblkgroups)
! parsing eventaul cc.par
call load_cc_par(filename=cc_par)
! computing global variables
Wi=int(sum(block%Wb)/Np)
Wmax=maxval(block%Wb)
bmax=maxloc(block%Wb,dim=1)
! initial multigrid check
do b=1,Nb
  if(.not.(block(b)%mgl_check(mgl=mgl))) then
    write(stderr,'(A)')'The block a:'//trim(str(.true.,block(b)%a))//&
                                ' l:'//trim(str(.true.,block(b)%l))//&
                                ' ba:'//trim(str(.true.,block(b)%ba))//&
                                ' i => Ni: '//trim(str(.true.,block(b)%Ni))//&
                                ' j => Nj: '//trim(str(.true.,block(b)%Nj))//&
                                ' k => Nk: '//trim(str(.true.,block(b)%Nk))//&
                                ' does not have enough levels of MG!'
    write(stderr,'(A)')'MG constraint violation! Sorry, I am exiting...'
    stop
  endif
enddo
! intialize leafs list
call init_leaf(check=.true.,block=block,leaf=leaf)
! splitting blocks heavier than Wi
call split_block_gt_Wi(mgl=mgl,Wi=Wi,leaf=leaf)
Nbs=0
do b=1,Nb
  call block(b)%print
  call block(b)%get_Nbs(Nbs=Nbs)
enddo
! balancing
i = 0
balance: do
  i = i + 1
  ! initializing processors
  do p=1,Np
    call proc(p)%init
  enddo
  ! assigning blocks
  call assign_block_leaf(leaf=leaf,Wi=Wi,bal=bal,proc=proc)
  ! checking if all blocks have been assigned to a processor
  if (Nbs/=sum(proc%Nb)) then
    write(stderr,'(A)')"Error: some blocks have not been assigned to a processor Nb:"//trim(str(.true.,Nbs))//"/="//&
                       trim(str(.true.,sum(proc%Nb)))
    stop
  endif
  Wprmax = maxval(abs(proc(:)%Wpr-100._R8P))
  write(stdout,'(A)')'Balancing Step i: '//trim(str(.false.,i))//' ; Maximum un-balance '//trim(str('(F5.2)',Wprmax))&
                     //'% ; Number of blocks:'//trim(str(.false.,Nbs))
  ! checking balancing
  if (all(proc(:)%balanced,dim=1).OR.(i>=imax)) exit balance
  ! some processors are not balanced their blocks (some of) must be split
  do t=1,threshNum
    thresh = threshMin + (t-1)*threshDelta
    call split_block_leaf(mgl=mgl,Wi=Wi,Thresh=thresh,leaf=leaf)
!    call split_block_leaf(mgl=mgl,Wi=Wi,Thresh=real(t/(tmax*1._R8P),R8P),leaf=leaf)
  enddo
  ! check if the procedure is converging
  call converging_check(block=block,leaf=leaf,save_blk_growth=save_blk_growth,Nbs=Nbs)
enddo balance
! initializing processors
do p=1,Np
  call proc(p)%init
enddo
! assigning blocks for the last configuration
call assign_block_leaf(leaf=leaf,Wi=Wi,bal=bal,proc=proc)
! printing stats
call print_stats
! writing output files
call save_output(block=block,froot=trim(froot),mgl=mgl,save_blk_his=save_blk_his,save_lvl_lis=save_lvl_lis,leaf=leaf)
! saving eventaul blocks groups file mapping
call save_map_blk_groups(filename=fblkgroups)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine command_line_interface()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Create and parse command line interface.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Command_Line_Interface):: cli     !< Command Line Interface (CLI).
  integer(I4P)::                      error   !< Error trapping flag.
  logical::                           bendian !< Input file is big endian.
  logical::                           lendian !< Input file is little endian.
  character(100)::                    ch_file !< Eventual check-file for testing algorithm.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call cli%init(progname='loadbalance', &
                version ='v0.0.1', &
                examples=[character(45)::                                  &
                          'loadbalance -check check.dat',                  &
                          'loadbalance -i cc.01',                          &
                          'loadbalance -i cc.01 -np 16 -be',               &
                          'loadbalance -i cc.01 -np 32 -b 2.',             &
                          'loadbalance -i cc.01 -np 16 -imax 100',         &
                          'loadbalance -i cc.01 -np 32 -save_blk_his',     &
                          'loadbalance -i cc.01 -np 32 -thresh 0.2-0.8-3'])
  call cli%add(pref='|-->',switch='--input',switch_ab='-i', &
               help='File name of input, either grd or icc',&
               required=.false.,act='store',def='unset',error=error)
  call cli%add(pref='|-->',switch='--check',switch_ab='-ch',&
               help='File name of parameters checking',     &
               required=.false.,act='store',def='unset',error=error)
  call cli%add(pref='|-->',switch='--processors',switch_ab='-np',&
               help='Number of processors',                      &
               required=.false.,act='store',def='8',error=error)
  call cli%add(pref='|-->',switch='--prime_factors',switch_ab='-pf',&
               help='Prime factors used for splitting',             &
               required=.false.,nargs='2',act='store',def='3 2',error=error)
  call cli%add(pref='|-->',switch='--big_endian',switch_ab='-be',&
               help='Big endian bits ordering',                  &
               required=.false.,act='store_true',def='.false.',error=error)
  call cli%add(pref='|-->',switch='--little_endian',switch_ab='-le',&
               help='Little endian bits ordering',                  &
               required=.false.,act='store_true',def='.false.',error=error)
  call cli%add(pref='|-->',switch='--bal_tol',switch_ab='-b',&
               help='Balancing tolerance',                   &
               required=.false.,act='store',def='3.0',error=error)
  call cli%add(pref='|-->',switch='--iter_max',switch_ab='-imax',&
               help='Maximum iteration of balancing algorithm',  &
               required=.false.,act='store',def='1000',error=error)
  call cli%add(pref='|-->',switch='--multi_grid_levels',switch_ab='-mgl',&
               help='Number of multigrid levels',                        &
               required=.false.,act='store',def='4',error=error)
  call cli%add(pref='|-->',switch='--threshold',switch_ab='-thresh',&
               help='Manual tuning of splitting, min/max',          &
               required=.false.,act='store',nargs='2',def='1.0 1.0',error=error)
  call cli%add(pref='|-->',switch='--save_blocks_history',switch_ab='-save_blk_his',&
               help='Save splitting history of each original block',                &
               required=.false.,act='store_true',def='.false.',error=error)
  call cli%add(pref='|-->',switch='--save_levels_history',switch_ab='-save_lvl_lis',&
               help='Save level-blocks-list of each splitting level',               &
               required=.false.,act='store_true',def='.false.',error=error)
  call cli%add(pref='|-->',switch='--save_blocks_growth',switch_ab='-save_blk_grw',&
               help='Save blocks growth history',                                  &
               required=.false.,act='store_true',def='.false.',error=error)
  call cli%add(pref='|-->',switch='--blocks_groups_map',switch_ab='-blk_groups',&
               help='File name of blocks to groups map',                        &
               required=.false.,act='store',def='blk_groups.ini',error=error)
  call cli%add(pref='|-->',switch='--cc_par',switch_ab='-c',&
               help='File name of overset parameters',      &
               required=.false.,act='store',def='cc.par',error=error)
  write(stdout,'(A)')'+--> Parsing Command Line Arguments'
  call cli%parse(error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-i',val=fnamein,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-ch',val=ch_file,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-np',val=Np,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-be',val=bendian,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-le',val=lendian,error=error,pref='|-->') ; if (error/=0) stop
  ! call cli%get(switch='-pf',val=pf,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-b',val=bal,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-imax',val=imax,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-mgl',val=mgl,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-save_blk_his',val=save_blk_his,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-save_lvl_lis',val=save_lvl_lis,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-save_blk_grw',val=save_blk_growth,error=error,pref='|-->') ; if (error/=0) stop
  ! call cli%get(switch='-thresh',val=save_blk_grw,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-blk_groups',val=fblkgroups,error=error,pref='|-->') ; if (error/=0) stop
  call cli%get(switch='-c',val=cc_par,error=error,pref='|-->') ; if (error/=0) stop
  if (trim(adjustl(ch_file))/='unset') fnamein = check(fnamei=trim(adjustl(ch_file)))
  if (bendian) endianism = 'BIG_ENDIAN'
  if (lendian) endianism = 'LITTLE_ENDIAN'
  if (trim(fnamein)=='') then
    write(stderr,'(A)')' Error: a valid file name for input file must be provided!'
    stop
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine command_line_interface

  subroutine print_defaults()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for printing values of defaults.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Input file :'//trim(fnamein)
  write(stdout,'(A)')' Number of processors :'//trim(str(.true.,Np))
  write(stdout,'(A)')' Bit ordering :'//trim(endianism)
  write(stdout,'(A)')' Balancing tolerance :'//trim(str('(F10.3)',bal))//'%'
  write(stdout,'(A)')' Maximum number of iterations :'//trim(str(.true.,imax))
  write(stdout,'(A)')' Number of multigrid levels :'//trim(str(.true.,mgl))
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_defaults

  subroutine print_stats()
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for printing balancing statitisc.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(1000):: sbuffer !< String buffer.
  integer(I4P)::    b,p     !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Number of original blocks '//trim(str(.true.,Nb))
  Nbs=0
  do b=1,Nb
    call block(b)%print
    call block(b)%get_Nbs(Nbs=Nbs)
  enddo
  Wprmax = MinR8P
  do p=1,Np
    sbuffer='   Processor:'//trim(str('(I7)',p))//', Work Load:'//trim(str('(I8)',proc(p)%Wp))//&
            ', Balance:'//trim(str('(F8.2)',proc(p)%Wpr-100._R8P))//'%'
    write(stdout,'(A)')trim(sbuffer)
    Wprmax = max(Wprmax,abs(proc(p)%Wpr-100._R8P))
  enddo
  write(stdout,'(A)')' Total Work Load '//trim(str(.true.,sum(block%Wb)))
  write(stdout,'(A)')' Ideal Work Load per processor '//trim(str(.true.,Wi))
  write(stdout,'(A)')' Maximum Work Load over ancestor blocks '//trim(str(.true.,Wmax))//' found in block '//trim(str(.true.,bmax))
  write(stdout,'(A)')' Number of split blocks '//trim(str(.true.,Nbs))
  write(stdout,'(A)')' Number of balancing iterations '//trim(str(.true.,i))
  write(stdout,'(A)')' Number of processors '//trim(str(.true.,Np))
  write(stdout,'(A)')' Maximum un-balance '//trim(str('(F8.2)',Wprmax))//'%'
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_stats

  function check(fnamei) result(fnameo)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for checking the algorithm with simple blocks structure described into ASCII file parameters.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN):: fnamei  !< File name of check parameter.
  character(Nc)::            fnameo  !< File name of output grd file.
  integer(I4P)::             ui,uo   !< Units file.
  integer(I4P)::             Nb      !< Number of block.
  integer(I4P)::             b,i,j,k !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  open(unit=Get_Unit(ui),file=trim(fnamei),form = 'FORMATTED')
  read(ui,*)fnameo
  open(unit=Get_Unit(uo),file=trim(fnameo),form = 'UNFORMATTED')
  read(ui,*)Nb ; write(uo)Nb
  do b=1,Nb
    read(ui,*)i,j,k ; write(uo)i,j,k
  enddo
  close(ui)
  close(uo)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction check

  subroutine load_cc_par(filename)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Load cc.par parameters file.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::      filename !< File name containing the cc.par.
  character(len=:), allocatable:: fname    !< Dummy file name containing the cc.par.
  integer(I4P)::                  freeunit !< Unit file.
  logical::                       is_file  !< Flag for inquiring the existance of input file.
  integer(I4P)::                  Nb,b,d   !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  fname = trim(adjustl(filename)) ; if (trim(adjustl(filename)) == '') fname = 'cc.par'
  inquire(file=trim(adjustl(fname)),exist=is_file)
  if (is_file) then
    print "(A)", "Found parameters file: "//fname
    open(unit=Get_Unit(freeunit),file=fname)
    read(freeunit,*)
    read(freeunit,*)
    read(freeunit,*)
    read(freeunit,*)
    read(freeunit,*)
    read(freeunit,*)
    read(freeunit,*)
    read(freeunit,*)
    read(freeunit,*)
    read(freeunit,*)
    read(freeunit,*)Nb
    if (verbose) print "(A)", "  Ancestor blocks number: "//trim(str(.true.,Nb))
    do b=1,Nb
      read(freeunit,*)d,block(b)%g,d
      if (verbose) print "(A)", "  Ancestor block "//trim(str(.true.,block(b)%a))//" has group:"//trim(str(.true.,block(b)%g))
    enddo
    close(freeunit)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine load_cc_par

  subroutine load_map_blk_groups(filename)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Load blocks groups map.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::      filename  !< File name containing the map.
  character(len=:), allocatable:: fname     !< Dummy file name containing the map.
  logical::                       is_file   !< Flag for inquiring the existance of input file.
  integer(I8P), allocatable::     blocks(:) !< Blocks index.
  integer(I4P)::                  Nb,b,g    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  fname = trim(adjustl(filename)) ; if (trim(adjustl(filename)) == '') fname = 'blk_groups.ini'
  inquire(file=trim(adjustl(fname)),exist=is_file)
  if (is_file) then
    call fini%load(filename=trim(adjustl(fname)))
    print "(A)", " Found blocks grouping map"
    if (verbose) call fini%print(pref='   ',unit=stdout)
    if (fini%Ns>0) then
      do g=1,fini%Ns ! loop over bloks groups
        Nb = fini%count_values(section=fini%section(g),option='blocks',delimiter=' ')
        if (allocated(blocks)) deallocate(blocks) ; allocate(blocks(1:Nb))
        call fini%get(section=fini%section(g),option='blocks',val=blocks)
        do b=1,size(blocks)
          block(blocks(b))%gt = trim(adjustl(fini%section(g)))
          if (verbose) print "(A)", " Ancestor block "//str(.true.,block(blocks(b))%a)//' associated to group tag "'//&
                             block(blocks(b))%gt//'"'
        enddo
      enddo
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine load_map_blk_groups

  subroutine save_map_blk_groups(filename)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Save blocks groups map.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(*), intent(IN)::      filename !< File name containing the map.
  character(len=:), allocatable:: fname    !< Dummy file name containing the map.
  type(Type_File_INI)::           bfini    !< New balanced ini file.
  type(Type_Block), pointer::     cur      !< Pointer for scanning leafs list.
  character(len=:), allocatable:: blocks   !< Blocks list of each group.
  integer(I4P)::                  g        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (fini%Ns>0) then
    fname = 'balanced-'//trim(adjustl(filename)) ; if (trim(adjustl(filename)) == '') fname = 'balanced-blk_groups.ini'
    bfini = fini
    bfini%filename = fname
    call bfini%free_options
    do g=1,bfini%Ns
      cur => leaf
      blocks = ''
      do while(associated(cur))
          if (allocated(cur%gt)) then
            if (cur%gt == bfini%section(g)) blocks = trim(blocks)//' '//trim(str(.true.,cur%ba))
          endif
          cur => cur%bnl
      enddo
      blocks = trim(adjustl(blocks))
      call bfini%add(section=bfini%section(g),option='blocks',val=blocks)
    enddo
    call bfini%save
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine save_map_blk_groups
endprogram loadbalance
