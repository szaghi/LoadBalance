!> @brief Module containing derived types and procedures for performing load balancing.
module Lib_Balancing
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision
USE Data_Type_SL_List
USE Data_Type_Time
USE Lib_IO_Misc
USE Lib_Math
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public:: Nc
public:: assign_block
public:: save_output
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
integer(I4P), parameter:: Nc=100 !< Number of character of prototype string.

type, public:: Type_Block
  ! block indexes
  integer(I4P)::              a=0  !< Index of ancestor block.
  integer(I4P)::              l=0  !< Level of splitting.
  integer(I4P), allocatable:: b(:) !< Blocks map, history of splitting [1:Nl].
  integer(I4P)::              p=-1 !< Processor assigned.
  ! block data
  integer(I4P)::              Ni=0,Nj=0,Nk=0       !< Number of cells along each directions.
  integer(I4P), allocatable:: pfi(:),pfj(:),pfk(:) !< Prime factors of Ni, Nj and Nk.
  integer(I8P)::              Wb=0                 !< Work load of each block.
  ! children blocks
  logical::                   split=.false. !< Flag for checking if the block is (or must be) splitted.
  integer(I4P)::              dir=0         !< Direction of split.
  integer(I4P)::              Ns=0          !< Number of splitted (children) blocks.
  type(Type_Block), pointer:: bs(:)=>null() !< Splitted (children) blocks [1:Ns].
  contains
    procedure:: init => init_block          ! Procedure for initializing block data.
    procedure:: splitt => split_block       ! Procedure for splitting (recursive) block.
    procedure:: get_Nbs => get_sblock_Nbs   ! Procedure for computing the number of (splitted) sub-blocks.
    procedure:: get_lmax => get_sblock_lmax ! Procedure for computing the maximum level of splits.
    procedure:: print => print_block        ! Procedure for printing information of block.
    procedure:: save => save_block          ! Procedure for saving block splitting history.
endtype Type_Block

type, public:: Type_Proc
  ! processor data
  real(R8P)::    Wpr=0._R8P       !< Work load ratio of the processor.
  integer(I8P):: Wp=0             !< Work load of the processor.
  logical::      balanced=.false. !< Flag for checking if the processor is balanced.
  ! blocks lists
  integer(I4P)::               Nb=0      !< Number of blocks of the processor.
  type(Type_SL_List)::         blist     !< List of blocks (linked list form).
  character(Nc), allocatable:: blistc(:) !< List of blocks (array list form).
  contains
    procedure:: init => init_proc                  ! Procedure for initializing processor.
    procedure:: split_block => split_block_of_proc ! Procedure for splitting blocks of processor.
endtype Type_Proc
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! type bound procedures of Type_Block
  !> @brief Subroutine for initializing block data.
  subroutine init_block(block,a,l,b,dir,Ni,Nj,Nk,Wi)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Block),      intent(INOUT):: block    !< Block.
  integer(I4P), optional, intent(IN)::    a        !< Index of ancestor block.
  integer(I4P), optional, intent(IN)::    l        !< Level of splitting.
  integer(I4P), optional, intent(IN)::    b(:)     !< Blocks map, history of splitting [1:l].
  integer(I4P), optional, intent(IN)::    dir      !< Direction of split.
  integer(I4P), optional, intent(IN)::    Ni,Nj,Nk !< Number of cells along each directions.
  integer(I8P), optional, intent(IN)::    Wi       !< Ideal work load per processor.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(a )) block%a  = a
  if (present(l )) block%l  = l
  if (present(b )) then
    if (allocated(block%b)) deallocate(block%b) ; allocate(block%b(1:size(b,dim=1))) ; block%b = b
  endif
  if (present(dir)) block%dir = dir
  if (present(Ni)) block%Ni = Ni
  if (present(Nj)) block%Nj = Nj
  if (present(Nk)) block%Nk = Nk
  block%Wb = block%Ni*block%Nj*block%Nk
  call prime(n=block%Ni,p=block%pfi)
  call prime(n=block%Nj,p=block%pfj)
  call prime(n=block%Nk,p=block%pfk)
  if (present(Wi)) then
    if (block%Wb>Wi) block%split =.true.
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine init_block

  !> @brief Subroutine for splitting block. The split is done along the direction (i,j,k) with highest number of cells and the
  !> number of split is driven by the prime factor list "pf" passed [...,9,7,5,3,2]: the pf list is scanned from the highest to
  !> lowest (that should be always 2) and is applied to first pf found.
  !> @note If the ideal work load "Wi" is passed the block is recursively splitted accordingly to Wi.
  !> @note If the number of levels of multi-grid "mgl" is passed the new splitted block preserve it.
  recursive subroutine split_block(block,Wi,mgl,pf)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Block),      intent(INOUT):: block          !< Block.
  integer(I8P), optional, intent(IN)::    Wi             !< Ideal work load per processor.
  integer(I4P),           intent(IN)::    mgl            !< Number of levels of multi-grid to be preserved.
  integer(I4P),           intent(IN)::    pf(1:)         !< Prime factors for block splitting.
  integer(I4P)::                          dir(1:3)       !< Direction selection variable.
  integer(I4P)::                          Nijk(1:3)      !< Direction selection variable.
  integer(I4P)::                          b(1:block%l+1) !< Blocks map, history of splitting.
  integer(I4P), allocatable::             pfd(:)         !< Dummy prime factors list for testing multi-grid levels.
  integer(I4P)::                          s,d,f          !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  b(1:block%l) = block%b
  Nijk(1) = block%Ni ; Nijk(2) = block%Nj ; Nijk(3) = block%Nk
  if     ((block%Ni>=block%Nj).AND.(block%Ni>=block%Nk)) then
    if   ((block%Nj>=block%Nk)) then  !i>j>k
      dir=[1,2,3]
    else                              !i>k>j
      dir=[1,3,2]
    endif
  elseif ((block%Nj>=block%Ni).AND.(block%Nj>=block%Nk)) then
    if   ((block%Ni>=block%Nk)) then  !j>i>k
      dir=[2,1,3]
    else                              !j>k>i
      dir=[2,3,1]
    endif
  elseif ((block%Nk>=block%Ni).AND.(block%Nk>=block%Nj)) then
    if   ((block%Ni>=block%Nj)) then  !k>i>j
      dir=[3,1,2]
    else                              !k>j>i
      dir=[3,2,1]
    endif
  endif
  splitting: do f=1,size(pf,dim=1)
    !direction: do d=1,3
      !select case(dir(d))
      select case(maxloc(Nijk,dim=1))
      case(1)
        if (any(block%pfi==pf(f))) then
          call prime(n=block%Ni/pf(f),p=pfd) ; if (size(pfd,dim=1)<mgl) cycle splitting
          block%split = .true.
          block%dir = 1
          block%Ns = pf(f)
          allocate(block%bs(1:block%Ns))
          do s=1,block%Ns
            b(block%l+1) = s
            if (present(Wi)) then
              call block%bs(s)%init(a=block%a,l=block%l+1,b=b,dir=block%dir,Ni=block%Ni/block%Ns,Nj=block%Nj,Nk=block%Nk,Wi=Wi)
              if (block%bs(s)%split) call split_block(block=block%bs(s),Wi=Wi,mgl=mgl,pf=pf)
            else
              call block%bs(s)%init(a=block%a,l=block%l+1,b=b,dir=block%dir,Ni=block%Ni/block%Ns,Nj=block%Nj,Nk=block%Nk)
            endif
          enddo
          exit splitting
        endif
      case(2)
        if (any(block%pfj==pf(f))) then
          call prime(n=block%Nj/pf(f),p=pfd) ; if (size(pfd,dim=1)<mgl) cycle splitting
          block%split = .true.
          block%dir = 2
          block%Ns = pf(f)
          allocate(block%bs(1:block%Ns))
          do s=1,block%Ns
            b(block%l+1) = s
            if (present(Wi)) then
              call block%bs(s)%init(a=block%a,l=block%l+1,b=b,dir=block%dir,Ni=block%Ni,Nj=block%Nj/block%Ns,Nk=block%Nk,Wi=Wi)
              if (block%bs(s)%split) call split_block(block=block%bs(s),Wi=Wi,mgl=mgl,pf=pf)
            else
              call block%bs(s)%init(a=block%a,l=block%l+1,b=b,dir=block%dir,Ni=block%Ni,Nj=block%Nj/block%Ns,Nk=block%Nk)
            endif
          enddo
          exit splitting
        endif
      case(3)
        if (any(block%pfk==pf(f))) then
          call prime(n=block%Nk/pf(f),p=pfd) ; if (size(pfd,dim=1)<mgl) cycle splitting
          block%split = .true.
          block%dir = 3
          block%Ns = pf(f)
          allocate(block%bs(1:block%Ns))
          do s=1,block%Ns
            b(block%l+1) = s
            if (present(Wi)) then
              call block%bs(s)%init(a=block%a,l=block%l+1,b=b,dir=block%dir,Ni=block%Ni,Nj=block%Nj,Nk=block%Nk/block%Ns,Wi=Wi)
              if (block%bs(s)%split) call split_block(block=block%bs(s),Wi=Wi,mgl=mgl,pf=pf)
            else
              call block%bs(s)%init(a=block%a,l=block%l+1,b=b,dir=block%dir,Ni=block%Ni,Nj=block%Nj,Nk=block%Nk/block%Ns)
            endif
          enddo
          exit splitting
        endif
      endselect
    !enddo direction
  enddo splitting
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine split_block

  !> @brief Subroutine for computing the number of splitted sub-blocks.
  recursive subroutine get_sblock_Nbs(block,Nbs)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Block), intent(IN)::    block !< Block data.
  integer(I4P),      intent(INOUT):: Nbs   !< Number of (splitted) sub-blocks.
  integer(I4P)::                     s     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (block%split) then
    do s=1,block%Ns
      call get_sblock_Nbs(block=block%bs(s),Nbs=Nbs)
    enddo
  else
    Nbs = Nbs + 1
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_sblock_Nbs

  !> @brief Subroutine for computing the maximum level of splitting.
  recursive subroutine get_sblock_lmax(block,lmax)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Block), intent(IN)::    block !< Block data.
  integer(I4P),      intent(INOUT):: lmax  !< Number of maximum level of splits.
  integer(I4P)::                     s     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (block%split) then
    do s=1,block%Ns
      call get_sblock_lmax(block=block%bs(s),lmax=lmax)
    enddo
  else
    lmax = max(block%l,lmax)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_sblock_lmax

  !> @brief Subroutine for printing information of block.
  subroutine print_block(block)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Block), intent(IN):: block   !< Block to be printed.
  character(1000)::               sbuffer !< String buffer.
  integer(I4P)::                  lmax    !< Maximum level of splits.
  integer(I4P)::                  p       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  lmax = 0
  call block%get_lmax(lmax=lmax)
  sbuffer='   Block:'//trim(str('(I7)',block%a))//', Work Load:'//trim(str('(I8)',block%Wb))//', Dimensions: '
  sbuffer=trim(sbuffer)//' Ni '//trim(str('(I4)',block%Ni))//' PF='
  do p=1,size(block%pfi)-1
    sbuffer=trim(sbuffer)//trim(str(.true.,block%pfi(p)))//'-'
  enddo
  sbuffer=trim(sbuffer)//trim(str(.true.,block%pfi(size(block%pfi))))
  sbuffer=trim(sbuffer)//', Nj '//trim(str('(I4)',block%Nj))//' PF='
  do p=1,size(block%pfj)-1
    sbuffer=trim(sbuffer)//trim(str(.true.,block%pfj(p)))//'-'
  enddo
  sbuffer=trim(sbuffer)//trim(str(.true.,block%pfj(size(block%pfj))))
  sbuffer=trim(sbuffer)//', Nk '//trim(str('(I4)',block%Nk))//' PF='
  do p=1,size(block%pfk)-1
    sbuffer=trim(sbuffer)//trim(str(.true.,block%pfk(p)))//'-'
  enddo
  sbuffer=trim(sbuffer)//trim(str(.true.,block%pfk(size(block%pfk))))
  sbuffer=trim(sbuffer)//', Maximum level of splits:'//trim(str('(I4)',lmax))
  write(stdout,'(A)')trim(sbuffer)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_block

  !> @brief Subroutine for recursively saving block splitting history.
  recursive subroutine save_block(block)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Block), intent(IN):: block   !< Block to be saved.
  integer(I4P)::                  u       !< Logical unit.
  character(1000)::               sbuffer !< String buffer.
  integer(I4P)::                  s       !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  open(unit=Get_Unit(u),file='block'//trim(strz(4,block%a))//'-splits.his')
  sbuffer = trim(str(.true.,block%a))//         &
            ' Ni:'//trim(str(.true.,block%Ni))//&
            ' Nj:'//trim(str(.true.,block%Nj))//&
            ' Nk:'//trim(str(.true.,block%Nk))//&
            ' Ns:'//trim(str(.true.,block%Ns))
  sbuffer = trim(sbuffer)//' PFi:'
  do s=1,size(block%pfi,dim=1)-1
    sbuffer = trim(sbuffer)//trim(str(.true.,block%pfi(s)))//'-'
  enddo
  sbuffer = trim(sbuffer)//trim(str(.true.,block%pfi(size(block%pfi,dim=1))))
  sbuffer = trim(sbuffer)//' PFj:'
  do s=1,size(block%pfj,dim=1)-1
    sbuffer = trim(sbuffer)//trim(str(.true.,block%pfj(s)))//'-'
  enddo
  sbuffer = trim(sbuffer)//trim(str(.true.,block%pfj(size(block%pfj,dim=1))))
  sbuffer = trim(sbuffer)//' PFk:'
  do s=1,size(block%pfk,dim=1)-1
    sbuffer = trim(sbuffer)//trim(str(.true.,block%pfk(s)))//'-'
  enddo
  sbuffer = trim(sbuffer)//trim(str(.true.,block%pfk(size(block%pfk,dim=1))))
  write(u,'(A)')trim(sbuffer)
  if (block%split) then
    do s=1,block%Ns
      call save_sblock(block=block%bs(s),i=2,u=u)
    enddo
  endif
  close(u)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    recursive subroutine save_sblock(block,i,u)
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(Type_Block), intent(IN):: block   !< Block to be saved.
    integer(I4P),     intent(IN):: i       !< Indenting white spaces.
    integer(I4P),     intent(IN):: u       !< Logical unit.
    character(1000)::              sbuffer !< String buffer.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    sbuffer = repeat(' ',i)//trim(str(.true.,block%a))//' L:'//trim(str(.true.,block%l))//&
              ' Ni:'//trim(str(.true.,block%Ni))//                                        &
              ' Nj:'//trim(str(.true.,block%Nj))//                                        &
              ' Nk:'//trim(str(.true.,block%Nk))//                                        &
              ' Ns:'//trim(str(.true.,block%Ns))
    sbuffer = trim(sbuffer)//' PFi:'
    do s=1,size(block%pfi,dim=1)-1
      sbuffer = trim(sbuffer)//trim(str(.true.,block%pfi(s)))//'-'
    enddo
    sbuffer = trim(sbuffer)//trim(str(.true.,block%pfi(size(block%pfi,dim=1))))
    sbuffer = trim(sbuffer)//' PFj:'
    do s=1,size(block%pfj,dim=1)-1
      sbuffer = trim(sbuffer)//trim(str(.true.,block%pfj(s)))//'-'
    enddo
    sbuffer = trim(sbuffer)//trim(str(.true.,block%pfj(size(block%pfj,dim=1))))
    sbuffer = trim(sbuffer)//' PFk:'
    do s=1,size(block%pfk,dim=1)-1
      sbuffer = trim(sbuffer)//trim(str(.true.,block%pfk(s)))//'-'
    enddo
    sbuffer = trim(sbuffer)//trim(str(.true.,block%pfk(size(block%pfk,dim=1))))
    write(u,'(A)')trim(sbuffer)
    if (block%split) then
      do s=1,block%Ns
        call save_sblock(block=block%bs(s),i=i+2,u=u)
      enddo
    endif
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine save_sblock
  endsubroutine save_block

  ! type bound procedures of Type_Proc
  !> @brief Subroutine for initializing processor data.
  subroutine init_proc(proc)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Proc), intent(INOUT):: proc !< Processor (self) data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  proc%Wpr=0._R8P
  proc%Wp=0
  proc%balanced=.false.
  proc%Nb=0
  call proc%blist%free()
  if (allocated(proc%blistc)) deallocate(proc%blistc)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine init_proc

  !> @brief Subroutine for splitting blocks assigned to a processor.
  subroutine split_block_of_proc(proc,block,pf,mgl)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Proc), intent(INOUT):: proc      !< Processor data.
  type(Type_Block), intent(INOUT):: block(1:) !< Block data.
  integer(I4P),     intent(IN)::    pf(1:)    !< Prime factors for block splitting.
  integer(I4P),     intent(IN)::    mgl       !< Number of levels of multi-grid to be preserved.
  integer(I4P), allocatable::       bm(:)     !< Blocks map, history of splitting [1:l].
  integer(I8P)::                    Wbmax,Wb  !< Maximum work load.
  integer(I8P)::                    bmax      !< Index of block with maximum work load.
  integer(I4P)::                    a,l,b     !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call proc%blist%array(Nc,proc%blistc)
  Wbmax = MinI8P
  do b=1,size(proc%blistc,dim=1)
    call get_alb(blist=proc%blistc(b),a=a,l=l,b=bm)
    if (l==0) then
      ! current block is an ancestor at root level (0)
      if (block(a)%Wb>Wbmax) then
        Wbmax = block(a)%Wb
        bmax = b
      endif
    else
      ! current block is a child at level > 0
      Wb = get_sblock_Wb(block=block(a)%bs(bm(1)),l=l,bm=bm)
      if (Wb>Wbmax) then
        Wbmax = Wb
        bmax = b
      endif
    endif
  enddo
  ! splitting block with high Wb
  call get_alb(blist=proc%blistc(bmax),a=a,l=l,b=bm)
  if (l==0) then
    ! current block is an ancestor at root level (0)
    call block(a)%splitt(mgl=mgl,pf=pf)
  else
    ! current block is a child at level > 0
    call sblock_split(block=block(a)%bs(bm(1)),l=l,bm=bm,mgl=mgl,pf=pf)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine get_alb(blist,a,l,b)
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    character(*),              intent(IN)::  blist
    integer(I4P),              intent(OUT):: a
    integer(I4P),              intent(OUT):: l
    integer(I4P), allocatable, intent(OUT):: b(:)
    character(len(blist)), allocatable::     tok1(:),tok2(:)
    integer(I4P)::                           s
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    call tokenize(strin=blist,delimiter='-',toks=tok1)
    tok1(1) = trim(tok1(1)(3:)) ; a = cton(trim(tok1(1)),1_I4P)
    tok1(2) = trim(tok1(2)(3:)) ; l = cton(trim(tok1(2)),1_I4P)
    tok1(3) = trim(tok1(3)(3:))
    call tokenize(strin=tok1(3),delimiter=':',toks=tok2)
    if (allocated(b)) deallocate(b) ; allocate(b(1:size(tok2,dim=1)))
    do s=1,size(tok2,dim=1)
      b(s) = cton(trim(tok2(s)),1_I4P)
    enddo
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine get_alb

    recursive function get_sblock_Wb(block,l,bm) result(Wb)
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(Type_Block), intent(IN)::    block   !< Block data.
    integer(I4P),     intent(IN)::    l       !< Level of splitting.
    integer(I4P),     intent(IN)::    bm(1:l) !< Blocks map, history of splitting [1:l].
    integer(I8P)::                    Wb      !< Block work load.
    integer(I4P)::                    s       !< Counter.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (block%l==l) then
      Wb = block%Wb
    else
      Wb = get_sblock_Wb(block=block%bs(bm(block%l+1)),l=l,bm=bm)
    endif
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endfunction get_sblock_Wb

    recursive subroutine sblock_split(block,l,bm,mgl,pf)
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(Type_Block), intent(INOUT):: block   !< Block data.
    integer(I4P),     intent(IN)::    l       !< Level of splitting.
    integer(I4P),     intent(IN)::    bm(1:l) !< Blocks map, history of splitting [1:l].
    integer(I4P),     intent(IN)::    mgl     !< Number of levels of multi-grid to be preserved.
    integer(I4P),     intent(IN)::    pf(1:)  !< Prime factors for block splitting.
    integer(I4P)::                    s       !< Counter.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (block%l==l) then
      call block%splitt(mgl=mgl,pf=pf)
    else
      call sblock_split(block=block%bs(bm(block%l+1)),l=l,bm=bm,mgl=mgl,pf=pf)
    endif
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine sblock_split
  endsubroutine split_block_of_proc

  ! non type bound procedures
  !> @brief Subroutine for assigning blocks to processors.
  recursive subroutine assign_block(block,Wi,bal,proc)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Block), intent(INOUT):: block    !< Block to be assigned.
  integer(I8P),     intent(IN)::    Wi       !< Ideal work load per processor.
  real(R8P),        intent(IN)::    bal      !< Tollerance of maximum balancing.
  type(Type_Proc),  intent(INOUT):: proc(1:) !< Processors data.
  character(Nc)::                   sbuffer  !< String buffer.
  integer(I4P)::                    s,l,b    !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (block%split) then
    do s=1,block%Ns
      call assign_block(block=block%bs(s),Wi=Wi,bal=bal,proc=proc)
    enddo
  else
    l = minloc(proc(:)%Wp,dim=1)
    block%p = l
    sbuffer = 'a_'//trim(str(.true.,block%a))//'-l_'//trim(str(.true.,block%l))//'-b_'
    if (block%l>0) then
      do b=1,block%l-1
        sbuffer = trim(sbuffer)//trim(str(.true.,block%b(b)))//':'
      enddo
      sbuffer = trim(sbuffer)//trim(str(.true.,block%b(block%l)))
    else
      sbuffer = trim(sbuffer)//trim(str(.true.,block%a))
    endif
    call proc(l)%blist%putt(d=sbuffer)
    proc(l)%Wp = proc(l)%Wp + block%Wb
    proc(l)%Wpr = proc(l)%Wp*100._R8P/Wi
    if ((proc(l)%Wpr>(100._R8P-bal)).AND.(proc(l)%Wpr<(100._R8P+bal))) proc(l)%balanced=.true.
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_block

  !> @brief Subroutine for recursively saving output files (blocks list files, bsplit.par of each level and proc.input).
  recursive subroutine save_output(block,froot,mgl,save_lvl_lis)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Block), intent(IN)::    block(1:)    !< Blocks data.
  character(*),     intent(IN)::    froot        !< Root file name.
  integer(I4P),     intent(IN)::    mgl          !< Number of levels of multi-grid.
  logical,          intent(IN)::    save_lvl_lis !< Flag for saving levele list.
  integer(I4P), allocatable::       u(:)         !< Logical units for levels list [0:lmax].
  integer(I4P), allocatable::       ubs(:)       !< Logical units for bsplit#.par [1:lmax-1].
  integer(I4P)::                    up           !< Logical unit for proc.input file.
  type(Type_SL_List), allocatable:: llist(:)     !< Levels list [0:lmax].
  character(Nc),      allocatable:: blist(:)     !< Blocks list.
  integer(I4P)::                    lmax         !< Maximum level of splits.
  character(20)::                   date         !< Actual date.
  integer(I4P)::                    l,b,p        !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  date = Get_Date_String()
  ! computing the maximum level of splits
  lmax = 0
  do b=1,size(block,dim=1)
    call block(b)%get_lmax(lmax=lmax)
  enddo
  ! opening levels list files
  if (save_lvl_lis) then
    allocate(u(0:lmax))
    do l=0,lmax
      open(unit=Get_Unit(u(l)),file='level'//trim(strz(4,l))//'.lis')
    enddo
  endif
  ! opening bsplit#.par files
  allocate(ubs(1:lmax))
  do l=1,lmax
    open(unit=Get_Unit(ubs(l)),file='bsplit'//trim(strz(4,l))//'.par')
    write(ubs(l),'(A)')"'xxx'   radice file soluzione"
    write(ubs(l),'(A)')"'"//trim(froot)//repeat('SPLIT',l-1)//"' radice file reticolo"
    write(ubs(l),'(A)')"'"//trim(froot)//repeat('SPLIT',l-1)//"' radice file cc"
    write(ubs(l),'(A)')trim(str(.true.,mgl))//" n livelli MG"
    write(ubs(l),'(A)')"0  variabili al tempo n"
    write(ubs(l),'(A)')"0  variabili al tempo n-1"
    write(ubs(l),'(A)')"1  stampe di debug"
  enddo
  ! allocating levels list
  allocate(llist(0:lmax))
  ! creating levels list
  do l=0,lmax
    do b=1,size(block,dim=1)
      call get_blocks_of_level(block=block(b),l=l,list=llist(l))
    enddo
  enddo
  ! converting list to character array and saving to output files
  do l=0,lmax-1
    call llist(l)%array(Nc,blist)
    do b=1,size(blist,dim=1)
      call parse_block_string(string=trim(str(.true.,b))//' '//trim(blist(b)),u=ubs(l+1))
      if (save_lvl_lis) then
        write(u(l),'(A)')trim(str(.true.,b))//' '//trim(blist(b))
      endif
    enddo
  enddo
  ! saving proc.input file
  call llist(lmax)%array(Nc,blist)
  open(unit=Get_Unit(up),file='proc.input')
  write(up,'(A)')repeat('-',43)//'generated by loadbalance '//trim(date)//repeat('-',44)
  write(up,'(A)')
  write(up,'(A)')
  write(up,'(A)') trim(str(.true.,size(blist,dim=1)))//' Numero totale blocchi (compresi b.i. e d.a.)'
  write(up,'(A)')
  write(up,'(A)') '    blocco    gruppo     corpo      proc'
  write(up,'(A)')repeat('-',132)
  do b=1,size(blist,dim=1)
    p = cton(trim(blist(b)(1:index(blist(b),' '))),I4P) - 1
    write(up,'(A)')trim(str('(I10)',b))//'         0         0 '//trim(str('(I9)',p))
  enddo
  close(up)
  ! closing output files
  if (save_lvl_lis) then
    do l=0,lmax
      close(u(l))
    enddo
    deallocate(u)
  endif
  do l=1,lmax
    close(ubs(l))
  enddo
  deallocate(ubs)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    !> @brief Subroutine for storing blocks list of each level.
    recursive subroutine get_blocks_of_level(block,l,list)
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(Type_Block),   intent(IN)::    block   !< Block data.
    integer(I4P),       intent(IN)::    l       !< Current level.
    type(Type_SL_List), intent(INOUT):: list    !< Level list.
    character(Nc)::                     sbuffer !< String buffer.
    integer(I4P)::                      s       !< Counter.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    if (block%l==l) then
      sbuffer = trim(str(.true.,block%p))//' '//trim(str(.true.,block%a))//' '//trim(str(.true.,block%Ns))
      select case(block%dir)
      case(1)
        sbuffer = trim(sbuffer)//' '//trim(str(.true.,block%dir))//' '//trim(str(.true.,block%Ni/max(1,block%Ns)))
      case(2)
        sbuffer = trim(sbuffer)//' '//trim(str(.true.,block%dir))//' '//trim(str(.true.,block%Nj/max(1,block%Ns)))
      case(3)
        sbuffer = trim(sbuffer)//' '//trim(str(.true.,block%dir))//' '//trim(str(.true.,block%Nk/max(1,block%Ns)))
      endselect
      call list%putt(sbuffer)
    elseif (block%l<l.AND.block%split) then
      do s=1,block%Ns
        call get_blocks_of_level(block=block%bs(s),l=l,list=llist(l))
      enddo
    else
      sbuffer = trim(str(.true.,block%p))//' '//trim(str(.true.,block%a))//' '//trim(str(.true.,block%Ns))
      call list%putt(sbuffer)
    endif
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine get_blocks_of_level

    !> @brief Subroutine for parsing string data of blocks.
    subroutine parse_block_string(string,u)
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    character(*), intent(IN)::            string  !< Block string data.
    integer(I4P), intent(IN)::            u       !< Logical unit.
    character(len(string)), allocatable:: toks(:) !< String tokens.
    integer(I4P)::                        Ns      !< Number of splits.
    integer(I4P)::                        Nijk    !< Position of splits.
    integer(I4P)::                        s       !< Counter.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    call tokenize(strin=string,delimiter=' ',toks=toks)
    Ns = cton(toks(4),I4P)
    if (Ns>1) then
      Nijk = cton(toks(6),I4P)
      do s=1,Ns-1
        write(u,'(A)')trim(toks(1))//' '//trim(toks(5))//' '//trim(str(.true.,s*Nijk))
      enddo
    endif
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine parse_block_string
  endsubroutine save_output
endmodule Lib_Balancing

!> @brief Program for performing load balancing.
program loadbalance
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision
USE Data_Type_SL_List
USE Lib_Balancing
USE Lib_IO_Misc
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
character(100)::                fnamein=''                !< Input file name.
character(100)::                froot='xxx'               !< Root file name.
integer(I4P)::                  uin=0                     !< Input file logic unit.
character(13)::                 endianism='LITTLE_ENDIAN' !< Bit ordering of binary files.
integer(I4P)::                  Np=8                      !< Number of processors.
integer(I4P)::                  Nb=0                      !< Number of blocks.
integer(I4P)::                  Nbs=0                     !< Number of blocks after splitting.
type(Type_Block), allocatable:: block(:)                  !< Blocks data [1:Nb].
type(Type_Proc),  allocatable:: proc(:)                   !< Processors data [1:Np].
real(R8P)::                     bal=3._R8P                !< Tollerance of maximum balancing.
integer(I8P)::                  Wi=0                      !< Ideal work load per processor.
integer(I8P)::                  Wmax=0                    !< Maximum work load over blocks.
integer(I4P)::                  bmax=0                    !< Index of heaviest block.
integer(I4P),   allocatable::   pf(:)                     !< Prime factors for block splitting [...,9,7,5,3,2].
character(100), allocatable::   pfc(:)                    !< Dummy strings for prime factors parsing.
integer(I4P)::                  mgl=4                     !< Number of levels of multi-grid.
integer(I4P)::                  imax=1000                 !< Maximum number of balancing iterations.
logical::                       save_blk_his=.false.      !< Flag for saving the splitting history of each original block.
logical::                       save_lvl_lis=.false.      !< Flag for saving levele list.
integer(I4P)::                  Nca=0                     !< Command line argument number.
character(100)::                switch,dum                !< Command line argument switches.
integer(I4P)::                  b,c,p,i                   !< Counters.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
! parsing command line arguments
Nca = command_argument_count()
if (Nca==0) then
  call print_usage
  stop
endif
c = 0
do while (c<Nca)
  c = c + 1
  call get_command_argument(c,switch)
  select case(trim(switch))
  case('-i')
    call get_command_argument(c+1,fnamein) ; c = c + 1
    froot = basename(trim(fnamein)) ; froot = trim(froot(1:index(froot,'.grd')-1))
  case('-np')
    call get_command_argument(c+1,dum) ; c = c + 1
    Np = cton(dum,I4P)
  case('-be')
    endianism = 'BIG_ENDIAN'
  case('-le')
    endianism = 'LITTLE_ENDIAN'
  case('-pf')
    call get_command_argument(c+1,dum) ; c = c + 1
    call tokenize(strin=dum,delimiter='-',toks=pfc)
    allocate(pf(1:size(pfc,dim=1)))
    do p=1,size(pfc,dim=1)
      pf(p) = cton(pfc(p),I4P)
    enddo
    if (allocated(pfc)) deallocate(pfc)
  case('-b')
    call get_command_argument(c+1,dum) ; c = c + 1
    bal = cton(dum,1._R8P)
  case('-imax')
    call get_command_argument(c+1,dum) ; c = c + 1
    imax = cton(dum,I4P)
  case('-mgl')
    call get_command_argument(c+1,dum) ; c = c + 1
    mgl = cton(dum,I4P)
  case('-save_blk_his')
    save_blk_his = .true.
  case('-save_lvl_lis')
    save_lvl_lis = .true.
  case default
    write(stderr,'(A)')' Switch "'//trim(switch)//'" unknown!'
    call print_usage
    stop
  endselect
enddo
if (.not.allocated(pf)) then
  allocate(pf(1:2)) ; pf = [3,2]
endif
allocate(proc(1:Np))
! loading input data
open(unit = Get_Unit(uin), file = trim(fnamein), form = 'UNFORMATTED', convert = trim(endianism))
read(uin)Nb
allocate(block(1:Nb))
do b=1,Nb
  read(uin)block(b)%Ni,block(b)%Nj,block(b)%Nk
  call block(b)%init(a=b,l=0,b=(/b/))
enddo
close(uin)
! computing global variables
Wi=int(sum(block%Wb)/Np)
Wmax=maxval(block%Wb)
bmax=maxloc(block%Wb,dim=1)
! splitting blocks higher than Wi
do b=1,Nb
  if (block(b)%Wb>Wi) block(b)%split =.true.
  if (block(b)%split) call block(b)%splitt(Wi=Wi,mgl=mgl,pf=pf)
enddo
! balancing
i = 0
balance: do
  i = i + 1
  ! initializing processors
  do p=1,Np
    call proc(p)%init()
  enddo
  ! assigning blocks
  do b=1,Nb
    call assign_block(block=block(b),Wi=Wi,bal=bal,proc=proc)
  enddo
  ! checking balancing
  if (all(proc(:)%balanced).OR.(i>=imax)) exit balance
  ! some processors are not balanced their blocks (some of) must be splitted
  do p=1,Np
    if (.not.proc(p)%balanced) call proc(p)%split_block(block=block,mgl=mgl,pf=pf)
  enddo
enddo balance
! printing stats
call print_stats()
if (save_blk_his) then
  do b=1,Nb
    call block(b)%save
  enddo
endif
! writing output files
call save_output(block=block,froot=basename(trim(froot)),mgl=mgl,save_lvl_lis=save_lvl_lis)
stop
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  !> @brief Subroutine for printing usage help message.
  subroutine print_usage()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Load balancing for Xnavis'
  write(stdout,'(A)')'   Usage:'
  write(stdout,'(A)')'     loadbalance [-swt [arg]]'
  write(stdout,'(A)')'       -i input_file_name'
  write(stdout,'(A)')'       -np #procs (default 8)'
  write(stdout,'(A)')'       -be => big endian bits ordering'
  write(stdout,'(A)')'       -le => little endian bits ordering (default)'
  write(stdout,'(A)')'       -pf prime_factor_list (default=[3,2]); "-" separated value => e.g. -pf 9-7-5-3-2'
  write(stdout,'(A)')'       -b balancing_tolerance (default 3.0%)'
  write(stdout,'(A)')'       -imax maximum_balancing_iteration (default 1000)'
  write(stdout,'(A)')'       -mgl number_of_multi-grid_levels (default 4)'
  write(stdout,'(A)')'       -save_blk_his => save splitting history of each original block (default no)'
  write(stdout,'(A)')'       -save_lvl_lis => save level block list for each level (default no)'
  write(stdout,'(A)')'   Examples:'
  write(stdout,'(A)')'     loadbalance -i cc.01                      ! little endian input file and 8 procs'
  write(stdout,'(A)')'     loadbalance -i cc.01 -np 64 -pf 7-5-3-2   ! particular prime factors list'
  write(stdout,'(A)')'     loadbalance -i cc.01 -np 16 -be           ! big endian input file and 16 procs'
  write(stdout,'(A)')'     loadbalance -i cc.01 -np 32 -b 2.         ! maximum admissible unbalance factor of 2%'
  write(stdout,'(A)')'     loadbalance -i cc.01 -np 16 -imax 100     ! maximum number of balancing iterations set to 100'
  write(stdout,'(A)')'     loadbalance -i cc.01 -np 32 -save_blk_his ! save block splitting history for each original block'
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_usage

  !> @brief Subroutine for printing balancing statitisc.
  subroutine print_stats()
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(1000):: sbuffer !< String buffer.
  integer(I4P)::    b,p     !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Number of original blocks '//trim(str(.true.,Nb))
  do b=1,Nb
    call block(b)%print
    call block(b)%get_Nbs(Nbs=Nbs)
  enddo
  write(stdout,'(A)')' Ideal Work Load per processor '//trim(str(.true.,Wi))
  write(stdout,'(A)')' Maximum Work Load over blocks '//trim(str(.true.,Wmax))//' found in block '//trim(str(.true.,bmax))
  write(stdout,'(A)')' Number of splitted blocks '//trim(str(.true.,Nbs))
  write(stdout,'(A)')' Number of balancing iterations '//trim(str(.true.,i))
  write(stdout,'(A)')' Number of processors '//trim(str(.true.,Np))
  do p=1,Np
    sbuffer='   Processor:'//trim(str('(I7)',p))//', Work Load:'//trim(str('(I8)',proc(p)%Wp))//&
            ', Balance:'//trim(str('(F8.2)',proc(p)%Wpr-100))//'%'
    write(stdout,'(A)')trim(sbuffer)
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_stats
endprogram loadbalance
