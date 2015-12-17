module Lib_Balance
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision
USE Data_Type_Block
USE Data_Type_Proc
USE Data_Type_SL_List
USE Data_Type_Time
USE Lib_IO_Misc
USE Lib_Strings
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
public:: Nc
public:: split_block_gt_Wi
public:: split_block_leaf
public:: assign_block_leaf
public:: init_leaf
public:: converging_check
public:: save_output
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
integer(I4P), parameter:: Nc=100 !< Number of character of prototype string.
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  function len_list(l) result(leng)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Compute the lenght of a Type_Block linked list.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Block), pointer, intent(IN):: l    !< List.
  integer(I4P)::                          leng !< Length of list.
  type(Type_Block), pointer::             cur  !< Pointer for scanning leafs list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  cur => l ; leng = 0_I4P
  do while(associated(cur))
    leng = leng + 1_I4P ; cur => cur%bnl
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction len_list

  subroutine split_block_gt_Wi(mgl,Wi,leaf)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Split blocks heavier than Wi.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),     intent(IN)::             mgl      !< Number of levels of multi-grid to be preserved.
  integer(I8P),     intent(IN)::             Wi       !< Ideal work load per processor.
  type(Type_Block), intent(INOUT), pointer:: leaf     !< Leafs list.
  integer(I4P)::                             it       !< Counters.
  type(Type_Block), pointer::                cur,next !< Pointers for scanning leafs list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  it = 0_I4P
  do while(leaf%Wb>Wi)
    cur => leaf
    do while(associated(cur))
      it = it + 1_I4P
      next => cur%bnl
      if (cur%Wb > Wi) then
        call cur%split(mgl=mgl,leaf=leaf)
      endif
      cur => next
    enddo
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine split_block_gt_Wi

  subroutine split_block_leaf(mgl,Wi,Thresh,leaf)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Split leafs.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  integer(I4P),              intent(IN)::    mgl                 !< Number of levels of multi-grid to be preserved.
  integer(I8P),              intent(IN)::    Wi                  !< Ideal work load per processor.
  real(R8P),                 intent(IN)::    Thresh              !< Threshold of blocks splitting.
  type(Type_Block), pointer, intent(INOUT):: leaf                !< Leafs list.
  logical::                                  split_performed     !< Split performed or not (when split_mode=prime).
  type(Type_Block), pointer::                bsplit              !< Pointer to block to split.
  type(Type_Block), pointer::                cur,last            !< Pointers for scanning leafs list.
  integer(I8P)::                             Wbmin,Wbmax,Wbsplit !< Work loads for driving the spitting.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  write(stdout,'(A)')' Starting leaf Wb,Wi:'//trim(str(.true.,leaf%Wb))//' '//trim(str(.true.,Wi))
  Wbmax = leaf%Wb
  cur => leaf
  do while(associated(cur%bnl))
    cur => cur%bnl
  enddo
  last => cur
  Wbmin = last%Wb
  Wbsplit = nint(Wbmin + Thresh*(Wbmax-Wbmin),I8P)
  split_performed = .false.
  cur => leaf
  do_bs: do while(associated(cur))
    if (cur%Wb<=Wbsplit) then
      bsplit => cur
      do while(associated(bsplit))
        call bsplit%split(split_performed=split_performed,mgl=mgl,leaf=leaf)
        if (split_performed) return
        bsplit => bsplit%bpl
      enddo
      exit do_bs !return
    else
      cur => cur%bnl
    endif
  enddo do_bs
  if (.not.split_performed) then
    write(stderr,'(A)')' Error: split failed!'
!    write(stderr,'(A)')' Block is =>'// &
!                       ' Ancestor: '//trim(str(.true.,bsplit%a))//&
!                       ' Level: '//trim(str(.true.,bsplit%l))//&
!                       ' Sizes: Ni-'//trim(str(.true.,bsplit%Ni))//&
!                              ' Nj-'//trim(str(.true.,bsplit%Nj))//&
!                              ' Nk-'//trim(str(.true.,bsplit%Nk))
    stop
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine split_block_leaf

  subroutine assign_block_leaf(leaf,Wi,bal,proc)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Assignin blocks to processors.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Block), intent(IN), target:: leaf     !< Leafs list.
  integer(I8P),     intent(IN)::         Wi       !< Ideal work load per processor.
  real(R8P),        intent(IN)::         bal      !< Tollerance of maximum balancing.
  type(Type_Proc),  intent(INOUT)::      proc(1:) !< Processors data.
  type(Type_Block), pointer::            cur      !< Pointer for scanning leafs list.
  integer(I4P)::                         l        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  cur => leaf
  do while(associated(cur))
    l = minloc(proc(:)%Wp,dim=1)
    cur%p = l
    proc(l)%Wp = proc(l)%Wp + cur%Wb
    proc(l)%Wpr = proc(l)%Wp*100._R8P/Wi
    proc(l)%balanced=.false.
    if ((proc(l)%Wpr>(100._R8P-bal)).AND.(proc(l)%Wpr<(100._R8P+bal))) proc(l)%balanced=.true.
    proc(l)%Nb = proc(l)%Nb + 1
    cur => cur%bnl
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_block_leaf

  subroutine init_leaf(check,block,leaf)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize sorted (descending) list of leafs.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  logical, optional,         intent(IN)::    check         !< Check initial leafs list.
  type(Type_Block), target,  intent(IN)::    block(1:)     !< Blocks data.
  type(Type_Block), pointer, intent(INOUT):: leaf          !< Leafs list.
  type(Type_Block), pointer::                cur,prev,last !< Pointers for scanning leafs list.
  integer, allocatable::                     Wb_array(:)   !< For ordering.
  integer::                                  b_Wbmax       !< Temporary index.
  integer::                                  Nb            !< Number of ancestor blocks.
  integer::                                  i,b           !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Nb=size(block)
  allocate(Wb_array(1:Nb))
  do b=1,Nb
    Wb_array(b) = block(b)%Wb
  enddo
  b_Wbmax=maxloc(Wb_array,dim=1) ; Wb_array(b_Wbmax) = -1
  leaf  => block(b_Wbmax)
  cur   => leaf
  prev  => leaf
  do b=2,Nb
    b_Wbmax=maxloc(Wb_array,dim=1) ; Wb_array(b_Wbmax) = -1
    cur%bnl  => block(b_Wbmax)
    cur      => cur%bnl
    cur%bpl  => prev
    prev     => cur
  enddo
  last => cur
  deallocate(Wb_array)
  if (present(check)) then
    if (check) then
      i=0
      cur => leaf
      do while(associated(cur))
         i=i+1
         write(stdout,'(A)')'Initial leafs list (descending): i,Wb:'//trim(str(.true.,i))//' '//trim(str(.true.,cur%Wb))
         cur => cur%bnl
      enddo
      cur => last
      i=Nb
      do while(associated(cur))
         write(stdout,'(A)')'Initial leafs list (ascending): i,Wb:'//trim(str(.true.,i))//' '//trim(str(.true.,cur%Wb))
         cur => cur%bpl
         i=i-1
      enddo
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine init_leaf

  function leaf_to_sort_array(leaf) result(block)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Sort leafs for block%ba (ascending).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Block), pointer, intent(IN):: leaf     !< Leafs list.
  type(Type_Block), allocatable::         block(:) !< Blocks data.
  type(Type_Block), pointer::             cur      !< Pointer for scanning leafs list.
  integer::                               Nb,b     !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Nb = len_list(l=leaf)
  allocate(block(1:Nb))
  cur => leaf
  b = 1
  do while(associated(cur))
    block(b) = cur
    cur => cur%bnl
    b = b + 1
  enddo
  call sort(blk=block)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    pure subroutine sort(blk)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Sort blocks array accordingly to block%ba (ascending).
    !-------------------------------------------------------------------------------------------------------------------------------
    type(Type_Block), intent(INOUT):: blk(:)
    type(Type_Block)::                tmp
    integer::                         i,J
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    do i = 2, size(blk,dim=1)
       j = i - 1
       tmp = blk(i)
       do while (j>=1)
          if (blk(j)%ba>tmp%ba) then
            blk(j+1) = blk(j)
            j = j - 1
          else
            exit
          endif
       end do
       blk(j+1) = tmp
    enddo
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine sort
  endfunction leaf_to_sort_array

  subroutine converging_check(block,leaf,save_blk_growth,Nbs)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check balancing procedure: (a) Nb growth and (b) leafs list.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Block), target,  intent(IN)::    block(1:)       !< Blocks data.
  type(Type_Block), pointer, intent(IN)::    leaf            !< Leafs list.
  logical,                   intent(IN)::    save_blk_growth !< Flag for saving blocks growth history.
  integer(I4P),              intent(INOUT):: Nbs             !< Number of blocks of current iteration.
  type(Type_Block), pointer::                cur             !< Pointer for scanning the list.
  integer(I4P)::                             Nb              !< Number of ancestors blocks.
  integer(I4P)::                             Nbs_old         !< Number of blocks of previous iteration.
  integer(I4P), allocatable::                Nbs_block(:)    !< Array of Number of blocks for each ancestor.
  integer(I4P)::                             b,i             !< Counters.
  integer(I4P)::                             u               !< File unit for growth behaviour.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Nb=size(block,dim=1)
  allocate(Nbs_block(1:Nb))
  Nbs_old=Nbs ; Nbs = 0 ; Nbs_block = 0
  do b=1,Nb
    call block(b)%get_Nbs(Nbs=Nbs)
    call block(b)%get_Nbs(Nbs=Nbs_block(b))
  enddo
  if (save_blk_growth) then
    open(unit=Get_Unit(u),file='blocks_growth.dat',position='append')
    write(u,'('//trim(str(.true.,Nb))//'A)')(' '//str(n=Nbs_block(b)),b=1,Nb)
    close(u)
  endif
  if (Nbs == Nbs_old) then
    write(stderr,'(A)')'Number of split blocks: '//trim(str(.true.,Nbs))
    write(stderr,'(A)')'Largest block is =>'// &
                       ' Ancestor: '//trim(str(.true.,leaf%a))//&
                       ' Level: '//trim(str(.true.,leaf%l))//&
                       ' Sizes: Ni-'//trim(str(.true.,leaf%Ni))//&
                              ' Nj-'//trim(str(.true.,leaf%Nj))//&
                              ' Nk-'//trim(str(.true.,leaf%Nk))
    write(stderr,'(A)')'Cannot split more! Sorry, I am exiting...'
    stop
  endif
  cur => leaf
  i=0
  do while(associated(cur))
    i=i+1
    cur => cur%bnl
  enddo
  if (i/=Nbs) then
    write(stderr,'(A)')'Number of sorted leafs :'//trim(str(.true.,i))//' not equal to split blocks Nbs: '//trim(str(.true.,Nbs))
    write(stderr,'(A)')'Error! I am exiting...'
    stop
  endif
  deallocate(Nbs_block)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine converging_check

  recursive subroutine save_output(block,froot,mgl,save_blk_his,save_lvl_lis,leaf)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Recursively save output files (blocks list files, bsplit.par of each level and proc.input).
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Block), target,  intent(INOUT):: block(1:)    !< Blocks data.
  character(*),              intent(IN)::    froot        !< Root file name.
  integer(I4P),              intent(IN)::    mgl          !< Number of levels of multi-grid.
  logical,                   intent(IN)::    save_blk_his !< Flag for saving the splitting history of each original block.
  logical,                   intent(IN)::    save_lvl_lis !< Flag for saving levele list.
  type(Type_Block), pointer, intent(INOUT):: leaf         !< Leafs list.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (save_blk_his) call save_block_split_his(block=block)
  if (save_lvl_lis) call save_level_lis(block=block)
                    call save_bsplit_par(block=block,froot=froot,mgl=mgl,leaf=leaf)
                    call save_proc_input(leaf=leaf)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    subroutine create_fake_children(leaf,lmax)
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(Type_Block), pointer, intent(INOUT):: leaf    !< List of leafs.
    integer(I4P),              intent(IN)::    lmax    !< Maximum level.
    type(Type_Block), pointer::                cur,tmp !< Pointers for list scanning.
    integer(I4P)::                             l       !< Counter.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    cur => leaf
    do while(associated(cur))
      if (cur%l<lmax) then
        ! creating fake children
        tmp => cur
        do l=1,lmax-cur%l
          tmp%spt=.true. ; tmp%dir=0 ; tmp%Ns=1
          allocate(tmp%bs(1))
          call tmp%bs(1)%set(a=tmp%a,g=tmp%g,l=tmp%l+1,ba=tmp%ba,b=[tmp%b,1],p=tmp%p,Ni=tmp%Ni,Nj=tmp%Nj,Nk=tmp%Nk)
          tmp => tmp%bs(1)
        enddo
        tmp%spt=.false.
      endif
      cur => cur%bnl
    enddo
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine create_fake_children

    subroutine save_block_split_his(block)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Save the split history of each ancestor block.
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(Type_Block), target,  intent(INOUT):: block(1:) !< Blocks data.
    integer(I4P)::                             b         !< Counter.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    do b=1,size(block,dim=1)
      call block(b)%save
    enddo
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine save_block_split_his

    subroutine save_level_lis(block)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Save the list of blocks of each level.
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(Type_Block), target,  intent(INOUT):: block(1:) !< Blocks data.
    integer(I4P), allocatable::                u(:)      !< Logical units for levels list [0:lmax].
    integer(I4P)::                             Nb        !< Number of original (ancestor) blocks.
    integer(I4P)::                             lmax      !< Maximum level of splits.
    type(Type_SL_List), allocatable::          llist(:)  !< Levels list [0:lmax].
    character(Nc),      allocatable::          blist(:)  !< Blocks list.
    integer(I4P)::                             b,l       !< Counters.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    Nb = size(block,dim=1)
    lmax = 0
    do b=1,Nb
      call block(b)%get_lmax(lmax=lmax)
    enddo
    allocate(u(0:lmax))
    do l=0,lmax
      open(unit=Get_Unit(u(l)),file='level'//trim(strz(4,l))//'.lis')
    enddo
    allocate(llist(0:lmax))
    do l=0,lmax
      do b=1,Nb
        call get_blocks_of_level(block=block(b),l=l,list=llist(l))
      enddo
    enddo
    ! converting list to character array and saving to output files
    do l=0,lmax
      call llist(l)%array(Nc,blist)
      do b=1,size(blist,dim=1)
        write(u(l),'(A)')trim(str(.true.,b))//' '//trim(blist(b))
      enddo
    enddo
    do l=0,lmax
      close(u(l))
      call llist(l)%free
    enddo
    deallocate(u,llist,blist)
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine save_level_lis

    subroutine save_bsplit_par(block,froot,mgl,leaf)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Save bsplit####.par files.
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(Type_Block), target,  intent(INOUT):: block(1:) !< Blocks data.
    character(*),              intent(IN)::    froot     !< Root file name.
    integer(I4P),              intent(IN)::    mgl       !< Number of levels of multi-grid.
    type(Type_Block), pointer, intent(INOUT):: leaf      !< Leafs list.
    integer(I4P)::                             Nb        !< Number of original (ancestor) blocks.
    integer(I4P)::                             lmax      !< Maximum level of splits.
    type(Type_SL_List), allocatable::          llist(:)  !< Levels list [0:lmax].
    character(Nc),      allocatable::          blist(:)  !< Blocks list.
    integer(I4P), allocatable::                u(:)      !< Logical units for bsplit####.par files [1:lmax].
    integer(I4P)::                             b,l,lc    !< Counters.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    ! computing the maximum level
    lmax = 0
    do b=1,size(block,dim=1)
      call block(b)%get_lmax(lmax=lmax)
    enddo
    if (lmax==0) then
      write(stdout,'(A)')' No split is necessary thus no bsplit.par is saved!'
      return
    endif
    ! creating fake children blocks if necessary
    call create_fake_children(leaf=leaf,lmax=lmax)
    ! creating levels lists
    allocate(llist(0:lmax))
    do l=0,lmax
      do b=1,size(block,dim=1)
        call get_blocks_of_level(block=block(b),l=l,list=llist(l))
      enddo
    enddo
    ! creating bsplit####.par files
    allocate(u(1:lmax))
    do l=1,lmax
      open(unit=Get_Unit(u(l)),file='bsplit'//trim(strz(4,l))//'.par')
      write(u(l),'(A)')"'xxx'   radice file soluzione"
      write(u(l),'(A)')"'"//trim(froot)//repeat('SPLIT',l-1)//"' radice file reticolo"
      write(u(l),'(A)')"'"//trim(froot)//repeat('SPLIT',l-1)//"' radice file cc"
      write(u(l),'(A)')trim(str(.true.,mgl))//" n livelli MG"
      write(u(l),'(A)')"0  variabili al tempo n"
      write(u(l),'(A)')"0  variabili al tempo n-1"
      write(u(l),'(A)')"1  stampe di debug"
    enddo
    ! saving bsplit####.par data
    do l=0,lmax-1
      call llist(l)%array(Nc,blist)
      do b=1,size(blist,dim=1)
        call parse_block_string(string=trim(str(.true.,b))//' '//trim(blist(b)),u=u(l+1))
      enddo
    enddo
    ! freeing memory
    call llist(0)%free
    do l=1,lmax
      close(u(l))
      call llist(l)%free
    enddo
    deallocate(u,llist,blist)
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine save_bsplit_par

    subroutine save_proc_input(leaf)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Save proc.input file.
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    type(Type_Block), pointer, intent(IN):: leaf        !< Leafs list.
    integer(I4P)::                          Nb          !< Number of blocks.
    integer(I4P)::                          u           !< Logical unit for proc.input file.
    character(20)::                         date        !< Actual date.
    integer(I4P)::                          b,c,g,p     !< Counters.
    type(Type_Block), allocatable::         blk_sort(:) !< Sort blocks array.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    date = Get_Date_String()
    Nb = len_list(l=leaf)
    blk_sort = leaf_to_sort_array(leaf=leaf)
    open(unit=Get_Unit(u),file='proc.input')
    write(u,'(A)')repeat('-',43)//'generated by loadbalance '//trim(date)//repeat('-',44)
    write(u,'(A)')
    write(u,'(A)')
    write(u,'(A)')trim(str(.true.,Nb))//' Numero totale blocchi (compresi b.i. e d.a.)'
    write(u,'(A)')
    write(u,'(A)') '    blocco    gruppo     corpo      proc'
    write(u,'(A)')repeat('-',132)
    do b=1,Nb
        write(u,'(A)')trim(str('(I10)',blk_sort(b)%ba ))//' '//&
                      trim(str('(I10)',blk_sort(b)%g  ))//' '//&
                      trim(str('(I10)',blk_sort(b)%c  ))//' '//&
                      trim(str('(I10)',blk_sort(b)%p-1))//' '//' ! '//trim(str(.true.,blk_sort(b)%Wb))
    enddo
    close(u)
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine save_proc_input

    recursive subroutine get_blocks_of_level(block,l,list)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Store blocks list of each level.
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
      sbuffer = trim(sbuffer)//' '//trim(str(.true.,block%dir))//' '//trim(str(.true.,block%Nijk))
      sbuffer = trim(sbuffer)//' '//trim(str(.true.,block%Ni*block%Nj*block%Nk))
      call list%putt(sbuffer)
    elseif (block%l<l.AND.block%spt) then
      do s=1,block%Ns
        call get_blocks_of_level(block=block%bs(s),l=l,list=list)
      enddo
    else
      sbuffer = trim(str(.true.,block%p))//' '//trim(str(.true.,block%a))//' '//trim(str(.true.,block%Ns))
      call list%putt(sbuffer)
    endif
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine get_blocks_of_level

    subroutine parse_block_string(string,u)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Parse string data of blocks.
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
    Ns = cton(str=toks(4), knd=1_I4P)
    if (Ns>1) then
      Nijk = cton(str=toks(6), knd=1_I4P)
      do s=1,Ns-1
        write(u,'(A)')trim(toks(1))//' '//trim(toks(5))//' '//trim(str(.true.,s*Nijk))
      enddo
    endif
    deallocate (toks)
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endsubroutine parse_block_string
  endsubroutine save_output
endmodule Lib_Balance
