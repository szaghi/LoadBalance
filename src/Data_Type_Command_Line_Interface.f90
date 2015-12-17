!< FLAP, Fortran command Line Arguments Parser for poor people
module Data_Type_Command_Line_Interface
!-----------------------------------------------------------------------------------------------------------------------------------
!< FLAP, Fortran command Line Arguments Parser for poor people
!<{!README-FLAP.md!}
!<
!<### ChangeLog
!<
!<{!ChangeLog-FLAP.md!}
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision                                                                ! Integers and reals precision definition.
USE, intrinsic:: ISO_FORTRAN_ENV, only: stdout=>OUTPUT_UNIT, stderr=>ERROR_UNIT ! Standard output/error logical units.
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, abstract:: Type_Object
  private
  character(len=:), public, allocatable:: progname      !< Program name.
  character(len=:), public, allocatable:: version       !< Program version.
  character(len=:),         allocatable:: help          !< Help message.
  character(len=:), public, allocatable:: description   !< Detailed description.
  character(len=:), public, allocatable:: license       !< License description.
  character(len=:), public, allocatable:: authors       !< Authors list.
  character(len=:), public, allocatable:: epilog        !< Epilog message.
  character(len=:), public, allocatable:: m_exclude     !< Mutually exclude other CLA(s group).
  integer(I4P),     public::              error = 0_I4P !< Error traping flag.
  contains
    procedure:: free_object   !< Free dynamic memory.
    procedure:: errored       !< Trig error occurence and print meaningful message.
    procedure:: print_version !< Print version.
    procedure:: assign_object !< Assignment overloading.
endtype Type_Object

type, extends(Type_Object):: Type_Command_Line_Argument
  !< Command line arguments (CLA).
  !<
  !< @note If not otherwise declared the action on CLA value is set to "store" a value.
  private
  character(len=:), allocatable:: switch             !< Switch name.
  character(len=:), allocatable:: switch_ab          !< Abbreviated switch name.
  logical::                       required  =.false. !< Flag for set required argument.
  logical::                       positional=.false. !< Flag for checking if CLA is a positional or a named CLA.
  integer(I4P)::                  position  = 0_I4P  !< Position of positional CLA.
  logical::                       passed    =.false. !< Flag for checking if CLA has been passed to CLI.
  character(len=:), allocatable:: act                !< CLA value action.
  character(len=:), allocatable:: def                !< Default value.
  character(len=:), allocatable:: nargs              !< Number of arguments consumed by CLA.
  character(len=:), allocatable:: choices            !< List (comma separated) of allowable values for the argument.
  character(len=:), allocatable:: val                !< CLA value.
  character(len=:), allocatable:: envvar             !< Environment variable from which take value.
  contains
    ! public methods
    procedure, public:: free          => free_cla             !< Free dynamic memory.
    procedure, public:: check         => check_cla            !< Check CLA data consistency.
    procedure, public:: check_choices => check_choices_cla    !< Check if CLA value is in allowed choices.
    generic,   public:: get           => get_cla,get_cla_list !< Get CLA value(s).
    procedure, public:: usage         => usage_cla            !< Get correct CLA usage.
    procedure, public:: signature     => signature_cla        !< Get CLA signature for adding to the CLI one.
    ! private methods
    procedure, private:: get_cla                     !< Get CLA (single) value from CLAs list parsed.
    procedure, private:: get_cla_list                !< Get CLA multiple values from CLAs list parsed.
    procedure, private:: assign_cla                  !< CLA assignment overloading.
    generic,   private:: assignment(=) => assign_cla !< CLA assignment overloading.
    final::              finalize_cla                !< Free dynamic memory when finalizing.
endtype Type_Command_Line_Argument

type, extends(Type_Object):: Type_Command_Line_Arguments_Group
  !< Group of CLAs for building nested commands.
  private
  character(len=:), allocatable::                 group               !< Group name (command).
  integer(I4P)::                                  Na          = 0_I4P !< Number of CLA.
  integer(I4P)::                                  Na_required = 0_I4P !< Number of command line arguments that CLI requires.
  integer(I4P)::                                  Na_optional = 0_I4P !< Number of command line arguments that are optional for CLI.
  type(Type_Command_Line_Argument), allocatable:: cla(:)              !< CLA list [1:Na].
  logical::                                       called      =.false.!< Flag for checking if CLAs group has been passed to CLI.
  contains
    ! public methods
    procedure, public:: free              => free_clasg              !< Free dynamic memory.
    procedure, public:: check             => check_clasg             !< Check CLAs data consistency.
    procedure, public:: check_required    => check_required_clasg    !< Check if required CLAs are passed.
    procedure, public:: check_m_exclusive => check_m_exclusive_clasg !< Check if two mutually exclusive CLAs have been passed.
    procedure, public:: add               => add_cla_clasg           !< Add CLA to CLAs group.
    procedure, public:: passed            => passed_clasg            !< Check if a CLA has been passed.
    procedure, public:: defined           => defined_clasg           !< Check if a CLA has been defined.
    procedure, public:: parse             => parse_clasg             !< Parse CLAs group arguments.
    procedure, public:: usage             => usage_clasg             !< Get correct CLAs group usage.
    procedure, public:: signature         => signature_clasg         !< Get CLAs group signature for adding to the CLI one.
    ! private methods
    procedure, private:: assign_clasg                  !< CLAs group assignment overloading.
    generic,   private:: assignment(=) => assign_clasg !< CLAs group assignment overloading.
    final::              finalize_clasg                !< Free dynamic memory when finalizing.
endtype Type_Command_Line_Arguments_Group

type, extends(Type_Object), public:: Type_Command_Line_Interface
  !< Command Line Interface (CLI).
  private
  type(Type_Command_Line_Arguments_Group), allocatable:: clasg(:)            !< CLA list [1:Na].
#ifdef GNU
  character(100  ), allocatable::                        args(:)             !< Actually passed command line arguments.
  character(100  ), allocatable::                        examples(:)         !< Examples of correct usage.
#else
  character(len=:), allocatable::                        args(:)             !< Actually passed command line arguments.
  character(len=:), allocatable::                        examples(:)         !< Examples of correct usage (not work with gfortran).
#endif
  logical::                                              disable_hv = .false.!< Disable automatic 'help' and 'version' CLAs.
  contains
    ! public methods
    procedure, public:: free                                !< Free dynamic memory.
    procedure, public:: init                                !< Initialize CLI.
    procedure, public:: add_group                           !< Add CLAs group CLI.
    procedure, public:: add                                 !< Add CLA to CLI.
    procedure, public:: passed                              !< Check if a CLA has been passed.
    procedure, public:: defined                             !< Check if a CLA has been defined.
    procedure, public:: defined_group                       !< Check if a CLAs group has been defined.
    procedure, public:: set_mutually_exclusive_groups       !< Set two CLAs group as mutually exclusive.
    procedure, public:: run_command => called_group         !< Check if a CLAs group has been runned.
    procedure, public:: parse                               !< Parse Command Line Interfaces.
    generic,   public:: get => get_cla_cli,get_cla_list_cli !< Get CLA value(s) from CLAs list parsed.
    procedure, public:: usage                               !< Get CLI usage.
    procedure, public:: signature                           !< Get CLI signature.
    procedure, public:: print_usage                         !< Print correct usage of CLI.
    procedure, public:: save_man_page                       !< Save man page build on CLI.
    ! private methods
    procedure, private:: check                                !< Check CLAs data consistenc.
    procedure, private:: check_m_exclusive                    !< Check if two mutually exclusive CLAs group have been called.
    procedure, private:: get_clasg_indexes                    !< Get CLAs groups indexes.
    generic,   private:: get_args => get_args_from_string,&   !< Get CLAs from string.
                                     get_args_from_invocation !< Get CLAs from CLI invocation.
    procedure, private:: get_args_from_string                 !< Get CLAs from string.
    procedure, private:: get_args_from_invocation             !< Get CLAs from CLI invocation.
    procedure, private:: get_cla_cli                          !< Get CLA (single) value from CLAs list parsed.
    procedure, private:: get_cla_list_cli                     !< Get CLA multiple values from CLAs list parsed.
    procedure, private:: assign_cli                           !< CLI assignment overloading.
    generic,   private:: assignment(=) => assign_cli          !< CLI assignment overloading.
    final::              finalize                             !< Free dynamic memory when finalizing.
endtype Type_Command_Line_Interface

integer(I4P),     parameter:: max_val_len        = 1000            !< Maximum number of characters of CLA value.
character(len=*), parameter:: action_store       = 'STORE'         !< CLA that stores a value associated to its switch.
character(len=*), parameter:: action_store_true  = 'STORE_TRUE'    !< CLA that stores .true. without the necessity of a value.
character(len=*), parameter:: action_store_false = 'STORE_FALSE'   !< CLA that stores .false. without the necessity of a value.
character(len=*), parameter:: action_print_help  = 'PRINT_HELP'    !< CLA that print help message.
character(len=*), parameter:: action_print_vers  = 'PRINT_VERSION' !< CLA that print version.
character(len=*), parameter:: args_sep           = '||!||'         !< Arguments separator for multiple valued (list) CLA.
! code errors and status
integer(I4P), parameter:: error_cla_optional_no_def        = 1  !< Optional CLA without default value.
integer(I4P), parameter:: error_cla_required_m_exclude     = 2  !< Required CLA cannot exclude others.
integer(I4P), parameter:: error_cla_positional_m_exclude   = 3  !< Positional CLA cannot exclude others.
integer(I4P), parameter:: error_cla_named_no_name          = 4  !< Named CLA without switch name.
integer(I4P), parameter:: error_cla_positional_no_position = 5  !< Positional CLA without position.
integer(I4P), parameter:: error_cla_positional_no_store    = 6  !< Positional CLA without action_store.
integer(I4P), parameter:: error_cla_not_in_choices         = 7  !< CLA value out of a specified choices.
integer(I4P), parameter:: error_cla_missing_required       = 8  !< Missing required CLA.
integer(I4P), parameter:: error_cla_m_exclude              = 9  !< Two mutually exclusive CLAs have been passed.
integer(I4P), parameter:: error_cla_casting_logical        = 10 !< Error casting CLA value to logical type.
integer(I4P), parameter:: error_cla_no_list                = 11 !< Actual CLA is not list-values.
integer(I4P), parameter:: error_cla_nargs_insufficient     = 12 !< Multi-valued CLA with insufficient arguments.
integer(I4P), parameter:: error_cla_unknown                = 13 !< Unknown CLA (switch name).
integer(I4P), parameter:: error_clasg_consistency          = 14 !< CLAs group consistency error.
integer(I4P), parameter:: error_clasg_m_exclude            = 15 !< Two mutually exclusive CLAs group have been called.
integer(I4P), parameter:: error_cli_missing_cla            = 16 !< CLA not found in CLI.
integer(I4P), parameter:: error_cli_missing_group          = 17 !< Group not found in CLI.
integer(I4P), parameter:: error_cli_missing_selection_cla  = 18 !< CLA selection in CLI failing.
integer(I4P), parameter:: error_cli_too_few_clas           = 19 !< Insufficient arguments for CLI.
integer(I4P), parameter:: status_clasg_print_v             = -1 !< Print version status.
integer(I4P), parameter:: status_clasg_print_h             = -2 !< Print help status.
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  ! auxiliary procedures
  elemental function Upper_Case(string)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for converting the lower case characters of a string to upper case one.
  !<
  !< @note This is taken form Lib_Strings.f90: chek into it for any updates.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=*), intent(IN):: string                                        !< String to be converted.
  character(len=len(string))::   Upper_Case                                    !< Converted string.
  integer::                      n1                                            !< Characters counter.
  integer::                      n2                                            !< Characters counter.
  character(len=26), parameter:: upper_alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' !< Upper case alphabet.
  character(len=26), parameter:: lower_alphabet = 'abcdefghijklmnopqrstuvwxyz' !< Lower case alphabet.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  Upper_Case = string
  do n1=1,len(string)
    n2 = index(lower_alphabet,string(n1:n1))
    if (n2>0) Upper_Case(n1:n1) = upper_alphabet(n2:n2)
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction Upper_Case

  pure subroutine tokenize(strin,delimiter,Nt,toks)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for tokenizing a string in order to parse it.
  !<
  !< @note The dummy array containing tokens must allocatable and its character elements must have the same length of the input
  !< string. If the length of the delimiter is higher than the input string one then the output tokens array is allocated with
  !< only one element set to char(0).
  !<
  !< @note This is taken form Lib_Strings.f90: chek into it for any updates.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  character(len=*),          intent(IN)::               strin     !< String to be tokenized.
  character(len=*),          intent(IN)::               delimiter !< Delimiter of tokens.
  integer(I4P),              intent(OUT), optional::    Nt        !< Number of tokens.
  character(len=len(strin)), intent(OUT), allocatable:: toks(:)   !< Tokens.
  character(len=len(strin))::                           strsub    !< Temporary string.
  integer(I4P)::                                        dlen      !< Delimiter length.
  integer(I4P)::                                        c         !< Counter.
  integer(I4P)::                                        n         !< Counter.
  integer(I4P)::                                        t         !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! initialization
  if (allocated(toks)) deallocate(toks)
  strsub = strin
  dlen = len(delimiter)
  if (dlen>len(strin)) then
    allocate(toks(1:1)) ; toks(1) = char(0) ; if (present(Nt)) Nt = 1 ; return
  endif
  ! computing the number of tokens
  n = 1
  do c=1,len(strsub)-dlen ! loop over string characters
    if (strsub(c:c+dlen-1)==delimiter) n = n + 1
  enddo
  allocate(toks(1:n))
  ! tokenization
  do t=1,n ! loop over tokens
    c = index(strsub,delimiter)
    if (c>0) then
      toks(t) = strsub(1:c-1)
      strsub = strsub(c+dlen:)
    else
      toks(t) = strsub
    endif
  enddo
  if (present(Nt)) Nt = n
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine tokenize

  ! Type_Object procedures
  elemental subroutine free_object(obj)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Object), intent(INOUT):: obj !< Object data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(obj%progname   )) deallocate(obj%progname   )
  if (allocated(obj%version    )) deallocate(obj%version    )
  if (allocated(obj%help       )) deallocate(obj%help       )
  if (allocated(obj%description)) deallocate(obj%description)
  if (allocated(obj%license    )) deallocate(obj%license    )
  if (allocated(obj%authors    )) deallocate(obj%authors    )
  if (allocated(obj%epilog     )) deallocate(obj%epilog     )
  if (allocated(obj%m_exclude  )) deallocate(obj%m_exclude  )
  obj%error = 0_I4P
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_object

  subroutine errored(obj,pref,group,switch,val_str,log_value,a1,a2,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Trig error occurence and print meaningful message.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Object),     intent(INOUT):: obj       !< Object data.
  character(*), optional, intent(IN)::    pref      !< Prefixing string.
  ! integer(I4P), optional, intent(IN)::    Na        !< Number of CLA passed.
  character(*), optional, intent(IN)::    group     !< Group name.
  character(*), optional, intent(IN)::    switch    !< CLA switch name.
  character(*), optional, intent(IN)::    val_str   !< Value string.
  character(*), optional, intent(IN)::    log_value !< Logical value to be casted.
  integer(I4P), optional, intent(IN)::    a1,a2     !< CLAs group inconsistent indexes.
  integer(I4P),           intent(IN)::    error     !< Error occurred.
  character(len=:), allocatable::         prefd     !< Prefixing string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  obj%error = error
  if (obj%error/=0) then
    prefd = '' ; if (present(pref)) prefd = pref
    select type(obj)
    class is(Type_Command_Line_Argument)
      select case(obj%error)
      case(error_cla_optional_no_def)
        if (obj%positional) then
          write(stderr,'(A)')prefd//obj%progname//': error: "'//trim(str(n=obj%position))//&
                             '-th" positional option has not a default value!'
        else
          write(stderr,'(A)')prefd//obj%progname//': error: named option "'//obj%switch//'" has not a default value!'
        endif
      case(error_cla_required_m_exclude)
        write(stderr,'(A)')prefd//obj%progname//': error: named option "'//obj%switch//'" cannot exclude others'//&
          ', it being requiredi, only optional ones can!'
      case(error_cla_positional_m_exclude)
        write(stderr,'(A)')prefd//obj%progname//': error: "'//trim(str(n=obj%position))//&
                           '-th" positional option cannot exclude others, only optional named options can!'
      case(error_cla_named_no_name)
        write(stderr,'(A)')prefd//obj%progname//': error: a non positional optiona must have a switch name!'
      case(error_cla_positional_no_position)
        write(stderr,'(A)')prefd//obj%progname//': error: a positional option must have a position number different from 0!'
      case(error_cla_positional_no_store)
        write(stderr,'(A)')prefd//obj%progname//': error: a positional option must have action set to "'//action_store//'"!'
      case(error_cla_m_exclude)
        write(stderr,'(A)')prefd//obj%progname//': error: the options "'//obj%switch//'" and "'//obj%m_exclude//'" are mutually'//&
         ' exclusive, but both have been passed!'
      case(error_cla_not_in_choices)
        if (obj%positional) then
          write(stderr,'(A)')prefd//obj%progname//': error: value of "'//trim(str(n=obj%position))//&
            '-th" positional option must be chosen in:'
        else
          write(stderr,'(A)')prefd//obj%progname//': error: value of named option "'//obj%switch//'" must be chosen in:'
        endif
        write(stderr,'(A)')prefd//'('//obj%choices//')'
        write(stderr,'(A)')prefd//'"'//trim(val_str)//'" has been passed!'
      case(error_cla_missing_required)
        if (.not.obj%positional) then
          write(stderr,'(A)')prefd//obj%progname//': error: named option "'//trim(adjustl(obj%switch))//'" is required!'
        else
          write(stderr,'(A)')prefd//obj%progname//': error: "'//trim(str(.true.,obj%position))//&
            '-th" positional option is required!'
        endif
      case(error_cla_casting_logical)
        write(stderr,'(A)')prefd//obj%progname//': error: cannot convert "'//log_value//'" of option "'//obj%switch//&
                           '" to logical type!'
      case(error_cla_no_list)
        if (.not.obj%positional) then
          write(stderr,'(A)')prefd//obj%progname//': error: named option "'//trim(adjustl(obj%switch))//'" has not "nargs" value'//&
                             ' but an array has been passed to "get" method!'
        else
          write(stderr,'(A)')prefd//obj%progname//': error: "'//trim(str(.true.,obj%position))//'-th" positional option '//&
                                    'has not "nargs" value but an array has been passed to "get" method!'
        endif
      case(error_cla_nargs_insufficient)
        if (.not.obj%positional) then
          write(stderr,'(A)')prefd//obj%progname//': error: named option "'//trim(adjustl(obj%switch))//'" requires '//&
            trim(adjustl(obj%nargs))//' arguments but no enough ones remain!'
        else
          write(stderr,'(A)')prefd//obj%progname//': error: "'//trim(str(.true.,obj%position))//&
            '-th" positional option requires '//&
            trim(adjustl(obj%nargs))//' arguments but no enough ones remain!'
        endif
      case(error_cla_unknown)
        write(stderr,'(A)')prefd//obj%progname//': error: switch "'//trim(adjustl(switch))//'" is unknown!'
      endselect

    class is(Type_Command_Line_Arguments_Group)
      select case(obj%error)
      case(error_clasg_consistency)
        if (obj%group /= '') then
          write(stderr,'(A)')prefd//obj%progname//': error: group (command) name: "'//obj%group//'" consistency error:'
        else
          write(stderr,'(A)')prefd//obj%progname//': error: consistency error:'
        endif
        write(stderr,'(A)')prefd//' "'//trim(str(.true.,a1))//'-th" option has the same switch or abbreviated switch of "'&
                           //trim(str(.true.,a2))//'-th" option:'
        write(stderr,'(A)')prefd//' CLA('//trim(str(.true.,a1)) //') switches = '//obj%cla(a1)%switch //' '//&
                           obj%cla(a1)%switch_ab
        write(stderr,'(A)')prefd//' CLA('//trim(str(.true.,a2))//') switches = '//obj%cla(a2)%switch//' '//&
                           obj%cla(a2)%switch_ab
      case(error_clasg_m_exclude)
        write(stderr,'(A)')prefd//obj%progname//': error: the group "'//obj%group//'" and "'//obj%m_exclude//'" are mutually'//&
         ' exclusive, but both have been called!'
      endselect

    class is(Type_Command_Line_Interface)
      select case(obj%error)
      case(error_cli_missing_cla)
        write(stderr,'(A)')prefd//obj%progname//': error: there is no option "'//trim(adjustl(switch))//'"!'
      case(error_cli_missing_selection_cla)
        write(stderr,'(A)')prefd//obj%progname//&
          ': error: to get an option value one of switch "name" or "position" must be provided!'
      case(error_cli_missing_group)
        write(stderr,'(A)')prefd//obj%progname//': error: ther is no group (command) named "'//trim(adjustl(group))//'"!'
      case(error_cli_too_few_clas)
        ! write(stderr,'(A)')prefd//obj%progname//': error: too few arguments ('//trim(str(.true.,Na))//')'//&
                           ! ' respect the required ('//trim(str(.true.,obj%Na_required))//')'
      endselect
    endselect
    write(stderr,'(A)')
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine errored

  subroutine print_version(obj,pref)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print version.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Object),     intent(IN):: obj   !< Object data.
  character(*), optional, intent(IN):: pref  !< Prefixing string.
  character(len=:), allocatable::      prefd !< Prefixing string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  write(stdout,'(A)')prefd//obj%progname//' version '//obj%version
  if (obj%license /= '') then
    write(stdout,'(A)')prefd//obj%license
  endif
  if (obj%authors /= '') then
    write(stdout,'(A)')prefd//obj%authors
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_version

  elemental subroutine assign_object(lhs,rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Object), intent(INOUT):: lhs !< Left hand side.
  class(Type_Object), intent(IN)::    rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Type_Object members
  if (allocated(rhs%progname   )) lhs%progname    = rhs%progname
  if (allocated(rhs%version    )) lhs%version     = rhs%version
  if (allocated(rhs%help       )) lhs%help        = rhs%help
  if (allocated(rhs%description)) lhs%description = rhs%description
  if (allocated(rhs%license    )) lhs%license     = rhs%license
  if (allocated(rhs%authors    )) lhs%authors     = rhs%authors
  if (allocated(rhs%epilog     )) lhs%epilog      = rhs%epilog
  if (allocated(rhs%m_exclude  )) lhs%m_exclude   = rhs%m_exclude
                                  lhs%error       = rhs%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_object

  ! Type_Command_Line_Argument procedures
  elemental subroutine free_cla(cla)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Argument), intent(INOUT):: cla !< CLA data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Type_Object members
  call cla%free_object
  ! Type_Command_Line_Argument members
  if (allocated(cla%switch   )) deallocate(cla%switch   )
  if (allocated(cla%switch_ab)) deallocate(cla%switch_ab)
  if (allocated(cla%act      )) deallocate(cla%act      )
  if (allocated(cla%def      )) deallocate(cla%def      )
  if (allocated(cla%nargs    )) deallocate(cla%nargs    )
  if (allocated(cla%choices  )) deallocate(cla%choices  )
  if (allocated(cla%val      )) deallocate(cla%val      )
  if (allocated(cla%envvar   )) deallocate(cla%envvar   )
  cla%required   = .false.
  cla%positional = .false.
  cla%position   =  0_I4P
  cla%passed     = .false.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_cla

  elemental subroutine finalize_cla(cla)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free dynamic memory when finalizing.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Command_Line_Argument), intent(INOUT):: cla !< CLA data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call cla%free
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize_cla

  subroutine check_cla(cla,pref)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check CLA data consistency.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Argument), intent(INOUT):: cla   !< CLA data.
  character(*), optional,            intent(IN)::    pref  !< Prefixing string.
  character(len=:), allocatable::                    prefd !< Prefixing string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  if ((.not.cla%required).and.(.not.allocated(cla%def))) then
    call cla%errored(pref=prefd,error=error_cla_optional_no_def)
  endif
  if ((cla%required).and.(cla%m_exclude/='')) then
    call cla%errored(pref=prefd,error=error_cla_required_m_exclude)
  endif
  if ((cla%positional).and.(cla%m_exclude/='')) then
    call cla%errored(pref=prefd,error=error_cla_positional_m_exclude)
  endif
  if ((.not.cla%positional).and.(.not.allocated(cla%switch))) then
    call cla%errored(pref=prefd,error=error_cla_named_no_name)
  elseif ((cla%positional).and.(cla%position==0_I4P)) then
    call cla%errored(pref=prefd,error=error_cla_positional_no_position)
  elseif ((cla%positional).and.(cla%act/=action_store)) then
    call cla%errored(pref=prefd,error=error_cla_positional_no_store)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine check_cla

  subroutine check_choices_cla(cla,val,pref)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if CLA value is in allowed choices.
  !<
  !< @note This procedure can be called if and only if cla%choices has been allocated.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Argument), intent(INOUT):: cla     !< CLA data.
  class(*),                          intent(IN)::    val     !< CLA value.
  character(*), optional,            intent(IN)::    pref    !< Prefixing string.
  character(len=:), allocatable::                    prefd   !< Prefixing string.
  character(len(cla%choices)), allocatable::         toks(:) !< Tokens for parsing choices list.
  integer(I4P)::                                     Nc      !< Number of choices.
  logical::                                          val_in  !< Flag for checking if val is in the choosen range.
  character(len=:), allocatable::                    val_str !< Value in string form.
  character(len=:), allocatable::                    tmp     !< Temporary string for avoiding GNU gfrotran bug.
  integer(I4P)::                                     c       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  val_in = .false.
  val_str = ''
  tmp = cla%choices
  call tokenize(strin=tmp,delimiter=',',Nt=Nc,toks=toks)
  select type(val)
#ifdef r16p
  type is(real(R16P))
    val_str = str(n=val)
    do c=1,Nc
      if (val==cton(str=trim(adjustl(toks(c))),knd=1._R16P)) val_in = .true.
    enddo
#endif
  type is(real(R8P))
    val_str = str(n=val)
    do c=1,Nc
      if (val==cton(str=trim(adjustl(toks(c))),knd=1._R8P)) val_in = .true.
    enddo
  type is(real(R4P))
    val_str = str(n=val)
    do c=1,Nc
      if (val==cton(str=trim(adjustl(toks(c))),knd=1._R4P)) val_in = .true.
    enddo
  type is(integer(I8P))
    val_str = str(n=val)
    do c=1,Nc
      if (val==cton(str=trim(adjustl(toks(c))),knd=1_I8P)) val_in = .true.
    enddo
  type is(integer(I4P))
    val_str = str(n=val)
    do c=1,Nc
      if (val==cton(str=trim(adjustl(toks(c))),knd=1_I4P)) val_in = .true.
    enddo
  type is(integer(I2P))
    val_str = str(n=val)
    do c=1,Nc
      if (val==cton(str=trim(adjustl(toks(c))),knd=1_I2P)) val_in = .true.
    enddo
  type is(integer(I1P))
    val_str = str(n=val)
    do c=1,Nc
      if (val==cton(str=trim(adjustl(toks(c))),knd=1_I1P)) val_in = .true.
    enddo
  type is(character(*))
    val_str = val
    do c=1,Nc
      if (val==toks(c)) val_in = .true.
    enddo
  endselect
  if (.not.val_in) then
    prefd = '' ; if (present(pref)) prefd = pref
    call cla%errored(pref=prefd,error=error_cla_not_in_choices,val_str=val_str)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine check_choices_cla

  subroutine get_cla(cla,pref,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get CLA (single) value.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Argument), intent(INOUT):: cla     !< CLA data.
  character(*), optional,            intent(IN)::    pref    !< Prefixing string.
  class(*),                          intent(INOUT):: val     !< CLA value.
  character(len=:), allocatable::                    prefd   !< Prefixing string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  if (((.not.cla%passed).and.cla%required).or.((.not.cla%passed).and.(.not.allocated(cla%def)))) then
    call cla%errored(pref=prefd,error=error_cla_missing_required)
    return
  endif
  if (cla%act==action_store) then
    if (cla%passed) then
      select type(val)
#ifdef r16p
      type is(real(R16P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%val)),knd=1._R16P)
#endif
      type is(real(R8P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%val)),knd=1._R8P)
      type is(real(R4P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%val)),knd=1._R4P)
      type is(integer(I8P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%val)),knd=1_I8P)
      type is(integer(I4P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%val)),knd=1_I4P)
      type is(integer(I2P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%val)),knd=1_I2P)
      type is(integer(I1P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%val)),knd=1_I1P)
      type is(logical)
        read(cla%val,*,iostat=cla%error)val
        if (cla%error/=0) call cla%errored(pref=prefd,error=error_cla_casting_logical,log_value=cla%val)
      type is(character(*))
        val = cla%val
      endselect
    else ! using default value
      select type(val)
#ifdef r16p
      type is(real(R16P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%def)),knd=1._R16P)
#endif
      type is(real(R8P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%def)),knd=1._R8P)
      type is(real(R4P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%def)),knd=1._R4P)
      type is(integer(I8P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%def)),knd=1_I8P)
      type is(integer(I4P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%def)),knd=1_I4P)
      type is(integer(I2P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%def)),knd=1_I2P)
      type is(integer(I1P))
        val = cton(pref=prefd,error=cla%error,str=trim(adjustl(cla%def)),knd=1_I1P)
      type is(logical)
        read(cla%def,*,iostat=cla%error)val
        if (cla%error/=0) call cla%errored(pref=prefd,error=error_cla_casting_logical,log_value=cla%def)
      type is(character(*))
        val = cla%def
      endselect
    endif
    if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val,pref=prefd)
  elseif (cla%act==action_store_true) then
    if (cla%passed) then
      select type(val)
      type is(logical)
        val = .true.
      endselect
    else
      select type(val)
      type is(logical)
        read(cla%def,*)val
      endselect
    endif
  elseif (cla%act==action_store_false) then
    if (cla%passed) then
      select type(val)
      type is(logical)
        val = .false.
      endselect
    else
      select type(val)
      type is(logical)
        read(cla%def,*)val
      endselect
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_cla

  subroutine get_cla_list(cla,pref,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get CLA (multiple) value.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Argument), intent(INOUT):: cla      !< CLA data.
  character(*), optional,            intent(IN)::    pref     !< Prefixing string.
  class(*),                          intent(INOUT):: val(1:)  !< CLA values.
  integer(I4P)::                                     Nv       !< Number of values.
  character(len=len(cla%val)), allocatable::         valsV(:) !< String array of values based on cla%val.
  character(len=len(cla%def)), allocatable::         valsD(:) !< String array of values based on cla%def.
  character(len=:), allocatable::                    prefd    !< Prefixing string.
  integer(I4P)::                                     v        !< Values counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  if (((.not.cla%passed).and.cla%required).or.((.not.cla%passed).and.(.not.allocated(cla%def)))) then
    call cla%errored(pref=prefd,error=error_cla_missing_required)
    return
  endif
  if (.not.allocated(cla%nargs)) then
    call cla%errored(pref=prefd,error=error_cla_no_list)
    return
  endif
  if (cla%act==action_store) then
    if (cla%passed) then
      call tokenize(strin=cla%val,delimiter=args_sep,Nt=Nv,toks=valsV)
      select type(val)
#ifdef r16p
      type is(real(R16P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsV(v))),knd=1._R16P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
#endif
      type is(real(R8P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsV(v))),knd=1._R8P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(real(R4P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsV(v))),knd=1._R4P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(integer(I8P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsV(v))),knd=1_I8P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(integer(I4P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsV(v))),knd=1_I4P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(integer(I2P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsV(v))),knd=1_I2P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(integer(I1P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsV(v))),knd=1_I1P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(logical)
        do v=1,Nv
          read(valsV(v),*,iostat=cla%error)val(v)
          if (cla%error/=0) then
            call cla%errored(pref=prefd,error=error_cla_casting_logical,log_value=valsD(v))
            exit
          endif
        enddo
      type is(character(*))
        do v=1,Nv
          val(v)=valsV(v)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      endselect
    else ! using default value
      call tokenize(strin=cla%def,delimiter=' ',Nt=Nv,toks=valsD)
      select type(val)
#ifdef r16p
      type is(real(R16P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsD(v))),knd=1._R16P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (error/=0) exit
        enddo
#endif
      type is(real(R8P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsD(v))),knd=1._R8P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(real(R4P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsD(v))),knd=1._R4P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(integer(I8P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsD(v))),knd=1_I8P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(integer(I4P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsD(v))),knd=1_I4P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(integer(I2P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsD(v))),knd=1_I2P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(integer(I1P))
        do v=1,Nv
          val(v) = cton(pref=prefd,error=cla%error,str=trim(adjustl(valsD(v))),knd=1_I1P)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      type is(logical)
        do v=1,Nv
          read(valsD(v),*,iostat=cla%error)val(v)
          if (cla%error/=0) then
            call cla%errored(pref=prefd,error=error_cla_casting_logical,log_value=valsD(v))
            exit
          endif
        enddo
      type is(character(*))
        do v=1,Nv
          val(v)=valsD(v)
          if (allocated(cla%choices).and.cla%error==0) call cla%check_choices(val=val(v),pref=prefd)
          if (cla%error/=0) exit
        enddo
      endselect
    endif
  elseif (cla%act==action_store_true) then
    if (cla%passed) then
      select type(val)
      type is(logical)
        val = .true.
      endselect
    else
      call tokenize(strin=cla%def,delimiter=' ',Nt=Nv,toks=valsD)
      select type(val)
      type is(logical)
        do v=1,Nv
          read(valsD(v),*)val(v)
        enddo
      endselect
    endif
  elseif (cla%act==action_store_false) then
    if (cla%passed) then
      select type(val)
      type is(logical)
        val = .false.
      endselect
    else
      call tokenize(strin=cla%def,delimiter=' ',Nt=Nv,toks=valsD)
      select type(val)
      type is(logical)
        do v=1,Nv
          read(valsD(v),*)val(v)
        enddo
      endselect
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_cla_list

  function usage_cla(cla,pref) result(usage)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get correct CLA usage.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Argument), intent(IN):: cla   !< CLAs group data.
  character(*), optional,            intent(IN):: pref  !< Prefixing string.
  character(len=:), allocatable::                 usage !< Usage string.
  character(len=:), allocatable::                 prefd !< Prefixing string.
  integer(I4P)::                                  a     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  if (cla%act==action_store) then
    if (.not.cla%positional) then
      if (allocated(cla%nargs)) then
        usage = ''
        select case(cla%nargs)
        case('+') ! not yet implemented
        case('*') ! not yet implemented
        case default
          do a=1,cton(str=trim(adjustl(cla%nargs)),knd=1_I4P)
            usage = usage//' value#'//trim(str(.true.,a))
          enddo
        endselect
        if (trim(adjustl(cla%switch))/=trim(adjustl(cla%switch_ab))) then
          usage = '   '//trim(adjustl(cla%switch))//usage//', '//trim(adjustl(cla%switch_ab))//usage
        else
          usage = '   '//trim(adjustl(cla%switch))//usage
        endif
      else
        if (trim(adjustl(cla%switch))/=trim(adjustl(cla%switch_ab))) then
          usage = '   '//trim(adjustl(cla%switch))//' value, '//trim(adjustl(cla%switch_ab))//' value'
        else
          usage = '   '//trim(adjustl(cla%switch))//' value'
        endif
      endif
    else
      usage = '  value'
    endif
    if (allocated(cla%choices)) then
      usage = usage//', value in: ('//cla%choices//')'
    endif
  else
    if (trim(adjustl(cla%switch))/=trim(adjustl(cla%switch_ab))) then
      usage = '   '//trim(adjustl(cla%switch))//', '//trim(adjustl(cla%switch_ab))
    else
      usage = '   '//trim(adjustl(cla%switch))
    endif
  endif
  usage = prefd//usage
  if (cla%positional) usage = usage//new_line('a')//prefd//repeat(' ',10)//trim(str(.true.,cla%position))//'-th argument'
  if (allocated(cla%envvar)) then
    if (cla%envvar /= '') then
      usage = usage//new_line('a')//prefd//repeat(' ',10)//'environment variable name "'//trim(adjustl(cla%envvar))//'"'
    endif
  endif
  if (.not.cla%required) then
    if (cla%def /= '') then
      usage = usage//new_line('a')//prefd//repeat(' ',10)//'default value '//trim(adjustl(cla%def))
    endif
  endif
  if (cla%m_exclude/='') usage = usage//new_line('a')//prefd//repeat(' ',10)//'mutually exclude "'//cla%m_exclude//'"'
  usage = usage//new_line('a')//prefd//repeat(' ',10)//trim(adjustl(cla%help))
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction usage_cla

  function signature_cla(cla) result(signd)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get CLA signature for adding to the CLI one.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Argument), intent(IN):: cla   !< CLA data.
  character(len=:), allocatable::                 signd !< Temporary CLI signature.
  integer(I4P)::                                  nargs !< Number of arguments consumed by CLA.
  integer(I4P)::                                  a     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (cla%act==action_store) then
    if (.not.cla%positional) then
      if (allocated(cla%nargs)) then
        select case(cla%nargs)
        case('+')
          signd = ' value#1 [value#2 value#3...]'
        case('*') ! not yet implemented
          signd = ' [value#1 value#2 value#3...]'
        case default
          nargs = cton(str=trim(adjustl(cla%nargs)),knd=1_I4P)
          signd = ''
          do a=1,nargs
            signd = signd//' value#'//trim(str(.true.,a))
          enddo
        endselect
      else
        signd = ' value'
      endif
      if (cla%required) then
        signd = ' '//trim(adjustl(cla%switch))//signd
      else
        signd = ' ['//trim(adjustl(cla%switch))//signd//']'
      endif
    else
      if (cla%required) then
        signd = ' value'
      else
        signd = ' [value]'
      endif
    endif
  else
    if (cla%required) then
      signd = ' '//trim(adjustl(cla%switch))
    else
      signd = ' ['//trim(adjustl(cla%switch))//']'
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction signature_cla

  elemental subroutine assign_cla(lhs,rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Argument), intent(INOUT):: lhs !< Left hand side.
  type(Type_Command_Line_Argument),  intent(IN)::    rhs !< Rigth hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Type_Object members
  call lhs%assign_object(rhs)
  ! Type_Command_Line_Argument members
  if (allocated(rhs%switch   )) lhs%switch     = rhs%switch
  if (allocated(rhs%switch_ab)) lhs%switch_ab  = rhs%switch_ab
  if (allocated(rhs%act      )) lhs%act        = rhs%act
  if (allocated(rhs%def      )) lhs%def        = rhs%def
  if (allocated(rhs%nargs    )) lhs%nargs      = rhs%nargs
  if (allocated(rhs%choices  )) lhs%choices    = rhs%choices
  if (allocated(rhs%val      )) lhs%val        = rhs%val
  if (allocated(rhs%envvar   )) lhs%envvar     = rhs%envvar
                                lhs%required   = rhs%required
                                lhs%positional = rhs%positional
                                lhs%position   = rhs%position
                                lhs%passed     = rhs%passed
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_cla

  ! Type_Command_Line_Arguments_Group procedures
  elemental subroutine free_clasg(clasg)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(INOUT):: clasg !< CLAs group data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Type_Object members
  call clasg%free_object
  ! Type_Command_Line_Arguments_Group members
  if (allocated(clasg%group)) deallocate(clasg%group)
  if (allocated(clasg%cla)) then
    call clasg%cla%free
    deallocate(clasg%cla)
  endif
  clasg%Na          = 0_I4P
  clasg%Na_required = 0_I4P
  clasg%Na_optional = 0_I4P
  clasg%called      = .false.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free_clasg

  elemental subroutine finalize_clasg(clasg)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Free dynamic memory when finalizing.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Command_Line_Arguments_Group), intent(INOUT):: clasg !< CLAs group data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call clasg%free
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize_clasg

  subroutine check_clasg(clasg,pref)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check CLA data consistency.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(INOUT):: clasg !< CLAs group data.
  character(*), optional,                   intent(IN)::    pref  !< Prefixing string.
  character(len=:), allocatable::                           prefd !< Prefixing string.
  integer(I4P)::                                            a,aa  !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  ! verifing if CLAs switches are unique
  CLA_unique: do a=1,clasg%Na
    if (.not.clasg%cla(a)%positional) then
      do aa=1,clasg%Na
        if ((a/=aa).and.(.not.clasg%cla(aa)%positional)) then
          if ((clasg%cla(a)%switch==clasg%cla(aa)%switch   ).or.(clasg%cla(a)%switch_ab==clasg%cla(aa)%switch   ).or.&
              (clasg%cla(a)%switch==clasg%cla(aa)%switch_ab).or.(clasg%cla(a)%switch_ab==clasg%cla(aa)%switch_ab)) then
            call clasg%errored(pref=prefd,error=error_clasg_consistency,a1=a,a2=aa)
            exit CLA_unique
          endif
        endif
      enddo
    endif
  enddo CLA_unique
  ! updating mutually exclusive relations
  CLA_exclude: do a=1,clasg%Na
    if (.not.clasg%cla(a)%positional) then
      if (clasg%cla(a)%m_exclude/='') then
        if (clasg%defined(switch=clasg%cla(a)%m_exclude,pos=aa)) then
          clasg%cla(aa)%m_exclude = clasg%cla(a)%switch
        endif
      endif
    endif
  enddo CLA_exclude
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine check_clasg

  subroutine check_required_clasg(clasg,pref)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if required CLAs are passed.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(INOUT):: clasg !< CLAs group data.
  character(*), optional,                   intent(IN)::    pref  !< Prefixing string.
  character(len=:), allocatable::                           prefd !< Prefixing string.
  integer(I4P)::                                            a     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  if (clasg%called) then
    do a=1,clasg%Na
      if (clasg%cla(a)%required) then
        if (.not.clasg%cla(a)%passed) then
          call clasg%cla(a)%errored(pref=prefd,error=error_cla_missing_required)
          clasg%error = clasg%cla(a)%error
          write(stdout,'(A)') clasg%usage(pref=prefd)
          return
        endif
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine check_required_clasg

  subroutine check_m_exclusive_clasg(clasg,pref)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if two mutually exclusive CLAs have been passed.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(INOUT):: clasg !< CLAs group data.
  character(*), optional,                   intent(IN)::    pref  !< Prefixing string.
  character(len=:), allocatable::                           prefd !< Prefixing string.
  integer(I4P)::                                            a     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (clasg%called) then
    prefd = '' ; if (present(pref)) prefd = pref
    do a=1,clasg%Na
      if (clasg%cla(a)%passed) then
        if (clasg%cla(a)%m_exclude/='') then
          if (clasg%passed(switch=clasg%cla(a)%m_exclude)) then
            call clasg%cla(a)%errored(pref=prefd,error=error_cla_m_exclude)
            clasg%error = clasg%cla(a)%error
            return
          endif
        endif
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine check_m_exclusive_clasg

  subroutine add_cla_clasg(clasg,pref,cla)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add CLA to CLAs list.
  !<
  !< @note If not otherwise declared the action on CLA value is set to "store" a value that must be passed after the switch name
  !< or directly passed in case of positional CLA.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(INOUT):: clasg           !< CLAs group data.
  character(*), optional,                   intent(IN)::    pref            !< Prefixing string.
  type(Type_Command_Line_Argument),         intent(IN)::    cla             !< CLA data.
  type(Type_Command_Line_Argument), allocatable::           cla_list_new(:) !< New (extended) CLA list.
  character(len=:), allocatable::                           prefd           !< Prefixing string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (clasg%Na>0_I4P) then
    if (.not.cla%positional) then
      allocate(cla_list_new(1:clasg%Na+1))
      cla_list_new(1:clasg%Na)=clasg%cla
      cla_list_new(clasg%Na+1)=cla
    else
      allocate(cla_list_new(1:clasg%Na+1))
      cla_list_new(1:cla%position-1)=clasg%cla(1:cla%position-1)
      cla_list_new(cla%position)=cla
      cla_list_new(cla%position+1:clasg%Na+1)=clasg%cla(cla%position:clasg%Na)
    endif
  else
    allocate(cla_list_new(1:1))
    cla_list_new(1)=cla
  endif
  call move_alloc(from=cla_list_new,to=clasg%cla)
  clasg%Na = clasg%Na + 1
  if (cla%required) then
    clasg%Na_required = clasg%Na_required + 1
  else
    clasg%Na_optional = clasg%Na_optional + 1
  endif
  if (allocated(cla_list_new)) deallocate(cla_list_new)
  prefd = '' ; if (present(pref)) prefd = pref
  call clasg%check(pref=prefd)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_cla_clasg

  pure function passed_clasg(clasg,switch,position) result(passed)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if a CLA has been passed.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(IN):: clasg    !< CLAs group data.
  character(*), optional,                   intent(IN):: switch   !< Switch name.
  integer(I4P), optional,                   intent(IN):: position !< Position of positional CLA.
  logical::                                              passed   !< Check if a CLA has been passed.
  integer(I4P)::                                         a        !< CLA counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  passed = .false.
  if (clasg%Na>0) then
    if (present(switch)) then
      do a=1,clasg%Na
        if ((clasg%cla(a)%switch==switch).or.(clasg%cla(a)%switch_ab==switch)) then
          passed = clasg%cla(a)%passed
          exit
        endif
      enddo
    elseif (present(position)) then
      passed = clasg%cla(position)%passed
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction passed_clasg

  function defined_clasg(clasg,switch,pos) result(defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if a CLA has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(IN)::  clasg   !< CLAs group data.
  character(*),                             intent(IN)::  switch  !< Switch name.
  integer(I4P), optional,                   intent(OUT):: pos     !< CLA position.
  logical::                                               defined !< Check if a CLA has been defined.
  integer(I4P)::                                          a       !< CLA counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  defined = .false.
  if (present(pos)) pos = 0
  if (clasg%Na>0) then
    do a=1,clasg%Na
      if ((clasg%cla(a)%switch==switch).or.(clasg%cla(a)%switch_ab==switch)) then
        defined = .true.
        if (present(pos)) pos = a
        exit
      endif
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction defined_clasg

  subroutine parse_clasg(clasg,pref,args)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse CLAs group arguments.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(INOUT):: clasg   !< CLAs group data.
  character(*), optional,                   intent(IN)::    pref    !< Prefixing string.
  character(*),                             intent(IN)::    args(:) !< Command line arguments.
  character(500)::                                          envvar  !< Environment variables buffer.
  integer(I4P)::                                            arg     !< Argument counter.
  integer(I4P)::                                            a       !< Counter.
  integer(I4P)::                                            aa      !< Counter.
  integer(I4P)::                                            nargs   !< Number of arguments consumed by a CLA.
  character(len=:), allocatable::                           prefd   !< Prefixing string.
  logical::                                                 found   !< Flag for checking if switch is a defined CLA.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (clasg%called) then
    prefd = '' ; if (present(pref)) prefd = pref
    arg = 0
    do while (arg < size(args,dim=1)) ! loop over CLAs group arguments passed
      arg = arg + 1
      found = .false.
      do a=1,clasg%Na ! loop ver CLAs group clas named options
        if (.not.clasg%cla(a)%positional) then
          if (trim(adjustl(clasg%cla(a)%switch   ))==trim(adjustl(args(arg))).or.&
              trim(adjustl(clasg%cla(a)%switch_ab))==trim(adjustl(args(arg)))) then
            if (clasg%cla(a)%act==action_store) then
              if (allocated(clasg%cla(a)%envvar)) then
                if (arg + 1 <= size(args,dim=1)) then ! verify if the value has been passed directly to cli
                  ! there are still other arguments to check
                  if (.not.clasg%defined(switch=trim(adjustl(args(arg+1))))) then
                    ! argument seem good...
                    arg = arg + 1
                    clasg%cla(a)%val = trim(adjustl(args(arg)))
                    found = .true.
                  endif
                endif
                ! check if found into arguments passed
                if (.not.found) then
                  ! not found, try to take val from environment
                  call get_environment_variable(name=clasg%cla(a)%envvar,value=envvar,status=aa)
                  if (aa==0) then
                    clasg%cla(a)%val = trim(adjustl(envvar))
                  else
                    ! flush default to val if environment is not set and default is set
                    if (allocated(clasg%cla(a)%def)) clasg%cla(a)%val = clasg%cla(a)%def
                  endif
                endif
              elseif (allocated(clasg%cla(a)%nargs)) then
                clasg%cla(a)%val = ''
                select case(clasg%cla(a)%nargs)
                case('+') ! not yet implemented
                case('*') ! not yet implemented
                case default
                  nargs = cton(str=trim(adjustl(clasg%cla(a)%nargs)),knd=1_I4P)
                  if (arg + nargs > size(args,dim=1)) then
                    call clasg%cla(a)%errored(pref=prefd,error = error_cla_nargs_insufficient)
                    clasg%error = clasg%cla(a)%error
                    return
                  endif
                  do aa=arg+nargs,arg+1,-1 ! decreasing loop due to gfortran bug
                    clasg%cla(a)%val = trim(adjustl(args(aa)))//args_sep//trim(clasg%cla(a)%val)
                  enddo
                  arg = arg + nargs
                endselect
              else
                arg = arg + 1
                clasg%cla(a)%val = trim(adjustl(args(arg)))
              endif
            elseif (clasg%cla(a)%act==action_print_help) then
              clasg%error = status_clasg_print_h
            elseif (clasg%cla(a)%act==action_print_vers) then
              clasg%error = status_clasg_print_v
            endif
            clasg%cla(a)%passed = .true.
            found = .true.
            exit
          endif
        endif
      enddo
      if (.not.found) then ! current argument (arg-th) does not correspond to a named option
        if (.not.clasg%cla(arg)%positional) then ! current argument (arg-th) is not positional... there is a problem!
          call clasg%cla(arg)%errored(pref=prefd,error=error_cla_unknown,switch=trim(adjustl(args(arg))))
          clasg%error = clasg%cla(arg)%error
          return
        else
          ! positional CLA always stores a value
          clasg%cla(arg)%val = trim(adjustl(args(arg)))
          clasg%cla(arg)%passed = .true.
        endif
      endif
    enddo
    call clasg%check_m_exclusive(pref=prefd)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse_clasg

  function usage_clasg(clasg,pref,no_header) result(usage)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get correct CLAs group usage.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(IN):: clasg     !< CLAs group data.
  character(*), optional,                   intent(IN):: pref      !< Prefixing string.
  logical,      optional,                   intent(IN):: no_header !< Avoid insert header to usage.
  character(len=:), allocatable::                        usage     !< Usage string.
  integer(I4P)::                                         a         !< Counters.
  character(len=:), allocatable::                        prefd     !< Prefixing string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  usage = clasg%progname ; if (clasg%group/='') usage = clasg%progname//' '//clasg%group
  usage = prefd//clasg%help//' '//usage//clasg%signature()
  if (clasg%description/='') usage = usage//new_line('a')//new_line('a')//prefd//clasg%description
  if (present(no_header)) then
    if (no_header) usage = ''
  endif
  if (clasg%Na_required>0) then
    usage = usage//new_line('a')//new_line('a')//prefd//'Required switches:'
    do a=1,clasg%Na
      if (clasg%cla(a)%required) usage = usage//new_line('a')//clasg%cla(a)%usage(pref=prefd)
    enddo
  endif
  if (clasg%Na_optional>0) then
    usage = usage//new_line('a')//new_line('a')//prefd//'Optional switches:'
    do a=1,clasg%Na
      if (.not.clasg%cla(a)%required) usage = usage//new_line('a')//clasg%cla(a)%usage(pref=prefd)
    enddo
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction usage_clasg

  function signature_clasg(clasg) result(signd)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get CLAs group signature for adding to the CLI one.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(IN):: clasg         !< CLAs group data.
  character(len=:), allocatable::                        signd         !< Temporary CLI signature.
  integer(I4P)::                                         a             !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  signd = ''
  do a=1,clasg%Na
    signd = signd//clasg%cla(a)%signature()
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction signature_clasg

  elemental subroutine assign_clasg(lhs,rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Arguments_Group), intent(INOUT):: lhs !< Left hand side.
  type(Type_Command_Line_Arguments_Group),  intent(IN)::    rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Type_Object members
  call lhs%assign_object(rhs)
  ! Type_Command_Line_Arguments_Group members
  if (allocated(rhs%group)) lhs%group = rhs%group
  if (allocated(rhs%cla  )) then
    if (allocated(lhs%cla)) deallocate(lhs%cla) ; allocate(lhs%cla(1:size(rhs%cla,dim=1)),source=rhs%cla)
  endif
  lhs%Na          = rhs%Na
  lhs%Na_required = rhs%Na_required
  lhs%Na_optional = rhs%Na_optional
  lhs%called      = rhs%called
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_clasg

  ! Type_Command_Line_Interface procedures
  elemental subroutine free(cli)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for freeing dynamic memory.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli !< CLI data.
  integer(I4P)::                                      g   !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Type_Object members
  call cli%free_object
  ! Type_Command_Line_Interface members
  if (allocated(cli%clasg)) then
    do g=1,size(cli%clasg,dim=1)
      call cli%clasg(g)%free
    enddo
    deallocate(cli%clasg)
  endif
  if (allocated(cli%examples))  deallocate(cli%examples)
  cli%disable_hv = .false.
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine free

  elemental subroutine finalize(cli)
  !< Procedure for freeing dynamic memory when finalizing.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  type(Type_Command_Line_Interface), intent(INOUT):: cli !< CLI data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call cli%free
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine finalize

  pure subroutine init(cli,progname,version,help,description,license,authors,examples,epilog,disable_hv)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for initializing CLI.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli          !< CLI data.
  character(*), optional,             intent(IN)::    progname     !< Program name.
  character(*), optional,             intent(IN)::    version      !< Program version.
  character(*), optional,             intent(IN)::    help         !< Help message introducing the CLI usage.
  character(*), optional,             intent(IN)::    description  !< Detailed description message introducing the program.
  character(*), optional,             intent(IN)::    license      !< License description.
  character(*), optional,             intent(IN)::    authors      !< Authors list.
  character(*), optional,             intent(IN)::    examples(1:) !< Examples of correct usage.
  character(*), optional,             intent(IN)::    epilog       !< Epilog message.
  logical,      optional,             intent(IN)::    disable_hv   !< Disable automatic inserting of 'help' and 'version' CLAs.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call cli%free
  cli%progname    = 'program' ; if (present(progname   )) cli%progname    = progname
  cli%version     = 'unknown' ; if (present(version    )) cli%version     = version
  cli%help        = 'usage: ' ; if (present(help       )) cli%help        = help
  cli%description = ''        ; if (present(description)) cli%description = description
  cli%license     = ''        ; if (present(license    )) cli%license     = license
  cli%authors     = ''        ; if (present(authors    )) cli%authors     = authors
  cli%epilog      = ''        ; if (present(epilog     )) cli%epilog      = epilog
  if (present(disable_hv)) cli%disable_hv = .true.
  if (present(examples)) then
#ifdef GNU
    allocate(cli%examples(1:size(examples)))
#else
    allocate(character(len=len(examples(1))):: cli%examples(1:size(examples))) ! does not work with gfortran 4.9.2
#endif
    cli%examples = examples
  endif
  ! initialize only the first default group
  allocate(cli%clasg(0:0))
  call cli%clasg(0)%assign_object(cli)
  cli%clasg(0)%group = ''
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine init

  subroutine add_group(cli,help,description,exclude,group)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add CLAs group to CLI.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT)::    cli               !< CLI data.
  character(*), optional,             intent(IN)::       help              !< Help message.
  character(*), optional,             intent(IN)::       description       !< Detailed description.
  character(*), optional,             intent(IN)::       exclude           !< Group name of the mutually exclusive group.
  character(*),                       intent(IN)::       group             !< Name of the grouped CLAs.
  type(Type_Command_Line_Arguments_Group), allocatable:: clasg_list_new(:) !< New (extended) CLAs group list.
  character(len=:), allocatable::                        helpd             !< Help message.
  character(len=:), allocatable::                        descriptiond      !< Detailed description.
  character(len=:), allocatable::                        excluded          !< Group name of the mutually exclusive group.
  integer(I4P)::                                         Ng                !< Number of groups.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (.not.cli%defined_group(group=group)) then
    helpd        = 'usage: ' ; if (present(help       )) helpd        = help
    descriptiond = ''        ; if (present(description)) descriptiond = description
    excluded     = ''        ; if (present(exclude    )) excluded     = exclude
    Ng = size(cli%clasg,dim=1)
    allocate(clasg_list_new(0:Ng))
    clasg_list_new(0:Ng-1) = cli%clasg(0:Ng-1)
    call clasg_list_new(Ng)%assign_object(cli)
    clasg_list_new(Ng)%help        = helpd
    clasg_list_new(Ng)%description = descriptiond
    clasg_list_new(Ng)%group       = group
    clasg_list_new(Ng)%m_exclude   = excluded
    deallocate(cli%clasg)
    allocate(cli%clasg(0:Ng))
    cli%clasg = clasg_list_new
    deallocate(clasg_list_new)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add_group

  subroutine set_mutually_exclusive_groups(cli,group1,group2)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set two CLAs group ad mutually exclusive.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli    !< CLI data.
  character(*),                       intent(IN)::    group1 !< Name of the first grouped CLAs.
  character(*),                       intent(IN)::    group2 !< Name of the second grouped CLAs.
  integer(I4P)::                                      g1,g2  !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (cli%defined_group(group=group1,g=g1).and.cli%defined_group(group=group2,g=g2)) then
    cli%clasg(g1)%m_exclude = group2
    cli%clasg(g2)%m_exclude = group1
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_mutually_exclusive_groups

  subroutine add(cli,pref,group,group_index,switch,switch_ab,help,required,positional,position,act,def,nargs,choices,exclude,&
                 envvar,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Add CLA to CLI.
  !<
  !< @note If not otherwise declared the action on CLA value is set to "store" a value that must be passed after the switch name
  !< or directly passed in case of positional CLA.
  !<
  !< @note If not otherwise speficied the CLA belongs to the default group "zero" that is the group of non-grouped CLAs.
  !<
  !< @note If CLA belongs to a not yet present group it is created on the fly.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli         !< CLI data.
  character(*), optional,             intent(IN)::    pref        !< Prefixing string.
  character(*), optional,             intent(IN)::    group       !< Name of the grouped CLAs.
  integer(I4P), optional,             intent(IN)::    group_index !< Index of the grouped CLAs.
  character(*), optional,             intent(IN)::    switch      !< Switch name.
  character(*), optional,             intent(IN)::    switch_ab   !< Abbreviated switch name.
  character(*), optional,             intent(IN)::    help        !< Help message describing the CLA.
  logical,      optional,             intent(IN)::    required    !< Flag for set required argument.
  logical,      optional,             intent(IN)::    positional  !< Flag for checking if CLA is a positional or a named CLA.
  integer(I4P), optional,             intent(IN)::    position    !< Position of positional CLA.
  character(*), optional,             intent(IN)::    act         !< CLA value action.
  character(*), optional,             intent(IN)::    def         !< Default value.
  character(*), optional,             intent(IN)::    nargs       !< Number of arguments consumed by CLA.
  character(*), optional,             intent(IN)::    choices     !< List of allowable values for the argument.
  character(*), optional,             intent(IN)::    exclude     !< Switch name of the mutually exclusive CLA.
  character(*), optional,             intent(IN)::    envvar      !< Environment variable from which take value.
  integer(I4P), optional,             intent(OUT)::   error       !< Error trapping flag.
  type(Type_Command_Line_Argument)::                  cla         !< CLA data.
  character(len=:), allocatable::                     prefd       !< Prefixing string.
  integer(I4P)::                                      g           !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! initializing CLA
  call cla%assign_object(cli)
  if (present(switch)) then
    cla%switch    = switch
    cla%switch_ab = switch
  endif
                                             if (present(switch_ab )) cla%switch_ab  = switch_ab
  cla%help       = 'Undocumented argument' ; if (present(help      )) cla%help       = help
  cla%required   = .false.                 ; if (present(required  )) cla%required   = required
  cla%positional = .false.                 ; if (present(positional)) cla%positional = positional
  cla%position   = 0_I4P                   ; if (present(position  )) cla%position   = position
  cla%act        = action_store            ; if (present(act       )) cla%act        = trim(adjustl(Upper_Case(act)))
                                             if (present(def       )) cla%def        = def
                                             if (present(nargs     )) cla%nargs      = nargs
                                             if (present(choices   )) cla%choices    = choices
  cla%m_exclude  = ''                      ; if (present(exclude   )) cla%m_exclude  = exclude
                                             if (present(envvar    )) cla%envvar     = envvar
  prefd = '' ; if (present(pref)) prefd = pref
  call cla%check(pref=prefd) ; cli%error = cla%error
  ! adding CLA to CLI
  if ((.not.present(group)).and.(.not.present(group_index))) then
    call cli%clasg(0)%add(pref=prefd,cla=cla) ; cli%error = cli%clasg(0)%error
  elseif (present(group)) then
    if (cli%defined_group(group=group,g=g)) then
      call cli%clasg(g)%add(pref=prefd,cla=cla) ; cli%error = cli%clasg(g)%error
    else
      call cli%add_group(group=group)
      call cli%clasg(size(cli%clasg,dim=1)-1)%add(pref=prefd,cla=cla) ; cli%error = cli%clasg(size(cli%clasg,dim=1)-1)%error
    endif
  elseif (present(group_index)) then
    if (group_index<=size(cli%clasg,dim=1)-1) then
      call cli%clasg(group_index)%add(pref=prefd,cla=cla) ; cli%error = cli%clasg(group_index)%error
    endif
  endif
  if (present(error)) error = cli%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine add

  subroutine check(cli,pref,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check CLAs data consistency.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli   !< CLI data.
  character(*), optional,             intent(IN)::    pref  !< Prefixing string.
  integer(I4P), optional,             intent(OUT)::   error !< Error trapping flag.
  character(len=:), allocatable::                     prefd !< Prefixing string.
  integer(I4P)::                                      g,gg  !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  do g=0,size(cli%clasg,dim=1)-1
    ! check group consistency
    call cli%clasg(g)%check(pref=prefd)
    cli%error = cli%clasg(g)%error
    if (present(error)) error = cli%error
    if (cli%error/=0) exit
    ! check mutually exclusive interaction
    if (g>0) then
      if (cli%clasg(g)%m_exclude/='') then
        if (cli%defined_group(group=cli%clasg(g)%m_exclude,g=gg)) cli%clasg(gg)%m_exclude = cli%clasg(g)%group
      endif
    endif
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine check

  subroutine check_m_exclusive(cli,pref)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if two mutually exclusive CLAs group have been called.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli   !< CLI data.
  character(*), optional,             intent(IN)::    pref  !< Prefixing string.
  character(len=:), allocatable::                     prefd !< Prefixing string.
  integer(I4P)::                                      g,gg  !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  do g=1,size(cli%clasg,dim=1)-1
    if (cli%clasg(g)%called.and.(cli%clasg(g)%m_exclude/='')) then
      if (cli%defined_group(group=cli%clasg(g)%m_exclude,g=gg)) then
        if (cli%clasg(gg)%called) then
          prefd = '' ; if (present(pref)) prefd = pref
          call cli%clasg(g)%errored(pref=prefd,error=error_clasg_m_exclude)
          cli%error = cli%clasg(g)%error
          exit
        endif
      endif
    endif
  enddo
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine check_m_exclusive

  function passed(cli,group,switch,position)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if a CLA has been passed.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(IN):: cli      !< CLI data.
  character(*), optional,             intent(IN):: group    !< Name of group (command) of CLA.
  character(*), optional,             intent(IN):: switch   !< Switch name.
  integer(I4P), optional,             intent(IN):: position !< Position of positional CLA.
  logical::                                        passed   !< Check if a CLA has been passed.
  integer(I4P)::                                   g        !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  passed = .false.
  if (.not.present(group)) then
    if (present(switch)) then
      passed = cli%clasg(0)%passed(switch=switch)
    elseif (present(position)) then
      passed = cli%clasg(0)%passed(position=position)
    endif
  else
    if (cli%defined_group(group=group,g=g)) then
      if (present(switch)) then
        passed = cli%clasg(g)%passed(switch=switch)
      elseif (present(position)) then
        passed = cli%clasg(g)%passed(position=position)
      endif
    endif
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction passed

  function defined_group(cli,g,group) result(defined)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if a CLAs group has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(IN)::  cli     !< CLI data.
  integer(I4P), optional,             intent(OUT):: g       !< Index of group.
  character(*),                       intent(IN)::  group   !< Name of group (command) of CLAs.
  logical::                                         defined !< Check if a CLAs group has been defined.
  integer(I4P)::                                    gg,ggg  !< Counters.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  defined = .false.
  do gg=0,size(cli%clasg,dim=1)-1
    ggg = gg
    if (allocated(cli%clasg(gg)%group)) defined = (cli%clasg(gg)%group==group)
    if (defined) exit
  enddo
  if (present(g)) g = ggg
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction defined_group

  function called_group(cli,group) result(called)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if a CLAs group has been runned.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(IN)::  cli    !< CLI data.
  character(*),                       intent(IN)::  group  !< Name of group (command) of CLAs.
  logical::                                         called !< Check if a CLAs group has been runned.
  integer(I4P)::                                    g      !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  called = .false.
  if (cli%defined_group(group=group,g=g)) called = cli%clasg(g)%called
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction called_group

  function defined(cli,group,switch)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Check if a CLA has been defined.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(IN):: cli     !< CLI data.
  character(*), optional,             intent(IN):: group   !< Name of group (command) of CLAs.
  character(*),                       intent(IN):: switch  !< Switch name.
  logical::                                        defined !< Check if a CLA has been defined.
  integer(I4P)::                                   g       !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  defined = .false.
  if (.not.present(group)) then
    defined = cli%clasg(0)%defined(switch=switch)
  else
    if (cli%defined_group(group=group,g=g)) defined = cli%clasg(g)%defined(switch=switch)
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction defined

  subroutine parse(cli,pref,args,error)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Parse Command Line Interfaces by means of a previously initialized CLAs groups list.
  !<
  !< @note The leading and trailing white spaces are removed from CLA values.
  !<
  !< @note If the *args* argument is passed the command line arguments are taken from it and not from the actual program CLI
  !< invocations.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli    !< CLI data.
  character(*), optional,             intent(IN)::    pref   !< Prefixing string.
  character(*), optional,             intent(IN)::    args   !< String containing command line arguments.
  integer(I4P), optional,             intent(OUT)::   error  !< Error trapping flag.
  character(len=:), allocatable::                     prefd  !< Prefixing string.
  integer(I4P)::                                      g      !< Counter for CLAs group.
  integer(I4P), allocatable::                         ai(:,:)!< Counter for CLAs grouped.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref

  ! adding help and version switches if not done by user
  if (.not.cli%disable_hv) then
    do g=0,size(cli%clasg,dim=1)-1
      if (.not.(cli%defined(group=cli%clasg(g)%group,switch='--help').and.cli%defined(group=cli%clasg(g)%group,switch='-h'))) &
        call cli%add(pref        = prefd,                     &
                     group_index = g,                         &
                     switch      = '--help',                  &
                     switch_ab   = '-h',                      &
                     help        = 'Print this help message', &
                     required    = .false.,                   &
                     def         = '',                        &
                     act         = 'print_help')
      if (.not.(cli%defined(group=cli%clasg(g)%group,switch='--version').and.cli%defined(group=cli%clasg(g)%group,switch='-v'))) &
        call cli%add(pref        = prefd,           &
                     group_index = g,               &
                     switch      = '--version',     &
                     switch_ab   = '-v',            &
                     help        = 'Print version', &
                     required    = .false.,         &
                     def         = '',              &
                     act         = 'print_version')
    enddo
  endif

  ! parsing passed CLAs grouping in indexes
  if (present(args)) then
    call cli%get_args(args=args,ai=ai)
  else
    call cli%get_args(ai=ai)
  endif

  ! checking CLI consistency
  call cli%check(pref=prefd)
  if (cli%error>0) then
    if (present(error)) error = cli%error
    return
  endif

  ! parsing cli
  do g=0,size(ai,dim=1)-1
    if (ai(g,1)>0) call cli%clasg(g)%parse(pref=prefd, args=cli%args(ai(g,1):ai(g,2)))
    cli%error = cli%clasg(g)%error
    if (cli%error /= 0) exit
  enddo
  if (cli%error>0) then
    if (present(error)) error = cli%error
    return
  endif

  ! trapping the special cases of version/help printing
  if (cli%error == status_clasg_print_v) then
    call cli%print_version(pref=prefd)
    stop
  elseif (cli%error == status_clasg_print_h) then
    write(stdout,'(A)') cli%usage(pref=prefd,g=g)
    stop
  endif

  ! checking if all required CLAs have been passed
  do g=0,size(ai,dim=1)-1
    call cli%clasg(g)%check_required(pref=prefd)
    cli%error = cli%clasg(g)%error
    if (cli%error>0) exit
  enddo
  if (cli%error>0) then
    if (present(error)) error = cli%error
    return
  endif

  ! check mutually exclusive interaction
  call cli%check_m_exclusive(pref=prefd)

  if (present(error)) error = cli%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine parse

  subroutine get_clasg_indexes(cli,ai)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get the argument indexes of CLAs groups defined parsing the actual passed CLAs.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli    !< CLI data.
  integer(I4P), allocatable,          intent(OUT)::   ai(:,:)!< CLAs grouped indexes.
  integer(I4P)::                                      Na     !< Number of command line arguments passed.
  integer(I4P)::                                      a      !< Counter for CLAs.
  integer(I4P)::                                      aa     !< Counter for CLAs.
  integer(I4P)::                                      g      !< Counter for CLAs group.
  logical::                                           found  !< Flag for inquiring if a named group is found.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  allocate(ai(0:size(cli%clasg,dim=1)-1,1:2))
  ai = 0
  if (allocated(cli%args)) then
    Na = size(cli%args,dim=1)
    a = 0
    found = .false.
    search_named: do while(a<Na)
      a = a + 1
      if (cli%defined_group(group=trim(cli%args(a)),g=g)) then
        found = .true.
        cli%clasg(g)%called = .true.
        ai(g,1) = a + 1
        aa = a
        do while(aa<Na)
          aa = aa + 1
          if (cli%defined_group(group=trim(cli%args(aa)))) then
            a = aa - 1
            ai(g,2) = a
            exit
          else
            ai(g,2) = aa
          endif
        enddo
      elseif (.not.found) then
        ai(0,2) = a
      endif
    enddo search_named
    if (ai(0,2)>0) then
      ai(0,1) = 1
      cli%clasg(0)%called = .true.
    elseif (all(ai==0)) then
      cli%clasg(0)%called = .true.
    endif
  else
    cli%clasg(0)%called = .true.
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_clasg_indexes

  subroutine get_args_from_string(cli,args,ai)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get CLAs from string.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli    !< CLI data.
  character(*),                       intent(IN)::    args   !< String containing command line arguments.
  integer(I4P), allocatable,          intent(OUT)::   ai(:,:)!< CLAs grouped indexes.
  character(len=len_trim(args))::                     argsd  !< Dummy string containing command line arguments.
  character(len=len_trim(args)), allocatable::        toks(:)!< CLAs tokenized.
  integer(I4P)::                                      Nt     !< Number of tokens.
  integer(I4P)::                                      Na     !< Number of command line arguments passed.
  integer(I4P)::                                      a      !< Counter for CLAs.
  integer(I4P)::                                      t      !< Counter for tokens.
  integer(I4P)::                                      c      !< Counter for characters inside tokens.
#ifndef GNU
  integer(I4P)::                                      length !< Maxium lenght of arguments string.
#endif
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! prepare cli arguments list
  if (allocated(cli%args)) deallocate(cli%args)

  ! sanitize arguments string
  argsd = trim(args)
  if (index(args,"'")>0) then
    argsd = sanitize_args(argsin=argsd,delimiter="'")
  elseif (index(args,'"')>0) then
    argsd = sanitize_args(argsin=argsd,delimiter='"')
  endif

  ! tokenize arguments string; the previously sanitized white spaces inside tokens are restored
  call tokenize(strin=argsd,delimiter=' ',Nt=Nt,toks=toks)
  Na = 0
  find_number_of_valid_arguments: do t=1,Nt
    if (trim(adjustl(toks(t)))/='') then
      Na = Na + 1
      do c=1,len(toks(t))
        if (toks(t)(c:c)=="'") toks(t)(c:c)=" "
      enddo
    endif
  enddo find_number_of_valid_arguments

  if (Na > 0) then
    ! allocate cli arguments list
#ifdef GNU
    allocate(cli%args(1:Na))
#else
    length = 0
    find_longest_arg: do t=1,Nt
      if (trim(adjustl(toks(t)))/='') length = max(length,len_trim(adjustl(toks(t))))
    enddo find_longest_arg
    allocate(character(length):: cli%args(1:Na))
#endif

    ! construct arguments list
    a = 0
    get_args: do t=1,Nt
      if (trim(adjustl(toks(t)))/='') then
        a = a + 1
        cli%args(a) = trim(adjustl(toks(t)))
      endif
    enddo get_args
  endif

  call cli%get_clasg_indexes(ai=ai)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  contains
    function sanitize_args(argsin,delimiter) result(sanitized)
    !-------------------------------------------------------------------------------------------------------------------------------
    !< Sanitize arguments string.
    !<
    !< Substitute white spaces enclosed into string-arguments, i.e. 'string argument with spaces...' or
    !< "string argument with spaces..." with a safe equivalent for tokenization against white spaces, i.e. the finally tokenized
    !< string is string'argument'with'spaces...
    !<
    !< @note The white spaces are reintroduce later.
    !-------------------------------------------------------------------------------------------------------------------------------
    implicit none
    character(*),               intent(IN)::       argsin    !< Arguments string.
    character(*),               intent(IN)::       delimiter !< Delimiter enclosing string argument.
    character(len=len_trim(argsin))::              sanitized !< Arguments string sanitized.
    character(len=len_trim(argsin)), allocatable:: tok(:)    !< Arguments string tokens.
    integer(I4P)::                                 Nt        !< Number of command line arguments passed.
    integer(I4P)::                                 t,tt      !< Counters.
    !-------------------------------------------------------------------------------------------------------------------------------

    !-------------------------------------------------------------------------------------------------------------------------------
    call tokenize(strin=trim(argsin),delimiter=delimiter,Nt=Nt,toks=tok)
    do t=2,Nt,2
      do tt=1,len_trim(adjustl(tok(t)))
        if (tok(t)(tt:tt)==' ') tok(t)(tt:tt) = "'"
      enddo
    enddo
    sanitized = ''
    do t=1,Nt
      sanitized = trim(sanitized)//" "//trim(adjustl(tok(t)))
    enddo
    sanitized = trim(adjustl(sanitized))
    return
    !-------------------------------------------------------------------------------------------------------------------------------
    endfunction sanitize_args
  endsubroutine get_args_from_string

  subroutine get_args_from_invocation(cli,ai)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get CLAs from CLI invocation.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli    !< CLI data.
  integer(I4P), allocatable,          intent(OUT)::   ai(:,:)!< CLAs grouped indexes.
  integer(I4P)::                                      Na     !< Number of command line arguments passed.
  character(max_val_len)::                            switch !< Switch name.
  integer(I4P)::                                      a      !< Counter for CLAs.
  integer(I4P)::                                      aa     !< Counter for CLAs.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (allocated(cli%args)) deallocate(cli%args)
  Na = command_argument_count()
  if (Na > 0) then
#ifdef GNU
    allocate(cli%args(1:Na))
#else
    aa = 0
    find_longest_arg: do a=1,Na
      call get_command_argument(a,switch)
      aa = max(aa,len_trim(switch))
    enddo find_longest_arg
    allocate(character(aa):: cli%args(1:Na))
#endif
    get_args: do a=1,Na
      call get_command_argument(a,switch)
      cli%args(a) = trim(adjustl(switch))
    enddo get_args
  endif

  call cli%get_clasg_indexes(ai=ai)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_args_from_invocation

  subroutine get_cla_cli(cli,pref,group,switch,position,error,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get CLA (single) value from CLAs list parsed.
  !<
  !< @note For logical type CLA the value is directly read without any robust error trapping.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli      !< CLI data.
  character(*), optional,             intent(IN)::    pref     !< Prefixing string.
  character(*), optional,             intent(IN)::    group    !< Name of group (command) of CLA.
  character(*), optional,             intent(IN)::    switch   !< Switch name.
  integer(I4P), optional,             intent(IN)::    position !< Position of positional CLA.
  integer(I4P), optional,             intent(OUT)::   error    !< Error trapping flag.
  class(*),                           intent(INOUT):: val      !< CLA value.
  character(len=:), allocatable::                     prefd    !< Prefixing string.
  logical::                                           found    !< Flag for checking if CLA containing switch has been found.
  integer(I4P)::                                      g        !< Group counter.
  integer(I4P)::                                      a        !< Argument counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  if (present(group)) then
    if (.not.cli%defined_group(group=group,g=g)) then
      call cli%errored(pref=prefd,error=error_cli_missing_group,group=group)
    endif
  else
    g = 0
  endif
  if (cli%error == 0 .and. cli%clasg(g)%called) then
    if (present(switch)) then
      ! searching for the CLA corresponding to switch
      found = .false.
      do a=1,cli%clasg(g)%Na
        if (.not.cli%clasg(g)%cla(a)%positional) then
          if ((cli%clasg(g)%cla(a)%switch==switch).or.(cli%clasg(g)%cla(a)%switch_ab==switch)) then
            found = .true.
            exit
          endif
        endif
      enddo
      if (.not.found) then
        call cli%errored(pref=prefd,error=error_cli_missing_cla,switch=switch)
      else
        call cli%clasg(g)%cla(a)%get(pref=prefd,val=val) ; cli%error = cli%clasg(g)%cla(a)%error
      endif
    elseif (present(position)) then
      call cli%clasg(g)%cla(position)%get(pref=prefd,val=val) ; cli%error = cli%clasg(g)%cla(position)%error
    else
      call cli%errored(pref=prefd,error=error_cli_missing_selection_cla)
    endif
  endif
  if (present(error)) error = cli%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_cla_cli

  subroutine get_cla_list_cli(cli,pref,group,switch,position,error,val)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Procedure for getting CLA multiple values from CLAs list parsed.
  !<
  !< @note For logical type CLA the value is directly read without any robust error trapping.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: cli      !< CLI data.
  character(*), optional,             intent(IN)::    pref     !< Prefixing string.
  character(*), optional,             intent(IN)::    group    !< Name of group (command) of CLA.
  character(*), optional,             intent(IN)::    switch   !< Switch name.
  integer(I4P), optional,             intent(IN)::    position !< Position of positional CLA.
  integer(I4P), optional,             intent(OUT)::   error    !< Error trapping flag.
  class(*),                           intent(INOUT):: val(1:)  !< CLA values.
  character(len=:), allocatable::                     prefd    !< Prefixing string.
  logical::                                           found    !< Flag for checking if CLA containing switch has been found.
  integer(I4P)::                                      g        !< Group counter.
  integer(I4P)::                                      a        !< Argument counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  if (present(group)) then
    if (.not.cli%defined_group(group=group,g=g)) then
      call cli%errored(pref=prefd,error=error_cli_missing_group,group=group)
    endif
  else
    g = 0
  endif
  if (present(switch)) then
    ! searching for the CLA corresponding to switch
    found = .false.
    do a=1,cli%clasg(g)%Na
      if (.not.cli%clasg(g)%cla(a)%positional) then
        if ((cli%clasg(g)%cla(a)%switch==switch).or.(cli%clasg(g)%cla(a)%switch_ab==switch)) then
          found = .true.
          exit
        endif
      endif
    enddo
    if (.not.found) then
      call cli%errored(pref=prefd,error=error_cli_missing_cla,switch=switch)
    else
      call cli%clasg(g)%cla(a)%get(pref=prefd,val=val) ; cli%error = cli%clasg(g)%cla(a)%error
    endif
  elseif (present(position)) then
    call cli%clasg(g)%cla(position)%get(pref=prefd,val=val) ; cli%error = error
  else
    call cli%errored(pref=prefd,error=error_cli_missing_selection_cla)
  endif
  if (present(error)) error = cli%error
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine get_cla_list_cli

  function usage(cli,pref,no_header,no_examples,no_epilog,g) result(usaged)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print correct usage of CLI.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(IN):: cli          !< CLI data.
  character(*), optional,             intent(IN):: pref         !< Prefixing string.
  logical,      optional,             intent(IN):: no_header    !< Avoid insert header to usage.
  logical,      optional,             intent(IN):: no_examples  !< Avoid insert examples to usage.
  logical,      optional,             intent(IN):: no_epilog    !< Avoid insert epilogue to usage.
  integer(I4P),                       intent(IN):: g            !< Group index.
  character(len=:), allocatable::                  prefd        !< Prefixing string.
  character(len=:), allocatable::                  usaged       !< Usage string.
  logical::                                        no_headerd   !< Avoid insert header to usage.
  logical::                                        no_examplesd !< Avoid insert examples to usage.
  logical::                                        no_epilogd   !< Avoid insert epilogue to usage.
  integer(I4P)::                                   gi           !< Counter.
  integer(I4P)::                                   e            !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  no_headerd = .false. ; if (present(no_header)) no_headerd = no_header
  no_examplesd = .false. ; if (present(no_examples)) no_examplesd = no_examples
  no_epilogd = .false. ; if (present(no_epilog)) no_epilogd = no_epilog
  prefd = '' ; if (present(pref)) prefd = pref
  if (g>0) then ! usage of a specific command
    usaged = cli%clasg(g)%usage(pref=prefd,no_header=no_headerd)
  else ! usage of whole CLI
    if (no_headerd) then
      usaged = ''
    else
      usaged = prefd//cli%help//cli%progname//' '//cli%signature()
      if (cli%description/='') usaged = usaged//new_line('a')//new_line('a')//prefd//cli%description
    endif
    if (cli%clasg(0)%Na>0) usaged = usaged//new_line('a')//cli%clasg(0)%usage(pref=prefd,no_header=.true.)
    if (size(cli%clasg,dim=1)>1) then
      usaged = usaged//new_line('a')//new_line('a')//prefd//'Commands:'
      do gi=1,size(cli%clasg,dim=1)-1
        usaged = usaged//new_line('a')//prefd//'  '//cli%clasg(gi)%group
        usaged = usaged//new_line('a')//prefd//repeat(' ',10)//cli%clasg(gi)%description
      enddo
      usaged = usaged//new_line('a')//new_line('a')//prefd//'For more detailed commands help try:'
      do gi=1,size(cli%clasg,dim=1)-1
        usaged = usaged//new_line('a')//prefd//'  '//cli%progname//' '//cli%clasg(gi)%group//' -h,--help'
      enddo
    endif
  endif
  if (allocated(cli%examples).and.(.not.no_examplesd)) then
    usaged = usaged//new_line('a')//new_line('a')//prefd//'Examples:'
    do e=1,size(cli%examples,dim=1)
      usaged = usaged//new_line('a')//prefd//'   '//trim(cli%examples(e))
    enddo
  endif
  if (cli%epilog/=''.and.(.not.no_epilogd)) usaged = usaged//new_line('a')//prefd//cli%epilog
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction usage

  function signature(cli) result(signd)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Get CLI signature.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(IN)::  cli   !< CLI data.
  character(len=:), allocatable::                   signd !< Temporary CLI signature.
  integer(I4P)::                                    g     !< Counter.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  signd = cli%clasg(0)%signature()
  if (size(cli%clasg,dim=1)>1) then
    signd = signd//' {'//cli%clasg(1)%group
    do g=2,size(cli%clasg,dim=1)-1
      signd = signd//','//cli%clasg(g)%group
    enddo
    signd = signd//'} ...'
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endfunction signature

  subroutine print_usage(cli,pref)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Print correct usage of CLI.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(IN):: cli   !< CLI data.
  character(*), optional,             intent(IN):: pref  !< Prefixing string.
  character(len=:), allocatable::                  prefd !< Prefixing string.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  prefd = '' ; if (present(pref)) prefd = pref
  write(stdout,'(A)')cli%usage(pref=prefd,g=0)
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine print_usage

  subroutine save_man_page(cli,error,man_file)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Save man page build on the CLI.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(IN)::  cli        !< CLI data.
  integer(I4P), optional,             intent(OUT):: error      !< Error trapping flag.
  character(*),                       intent(IN)::  man_file   !< Output file name for saving man page.
  character(len=:), allocatable::                   man        !< Man page.
  integer(I4P)::                                    idate(1:8) !< Integer array for handling the date.
  integer(I4P)::                                    e          !< Counter.
  integer(I4P)::                                    u          !< Unit file handler.
  character(*), parameter:: month(12) = ["Jan",&
                                         "Feb",&
                                         "Mar",&
                                         "Apr",&
                                         "May",&
                                         "Jun",&
                                         "Jul",&
                                         "Aug",&
                                         "Sep",&
                                         "Oct",&
                                         "Nov",&
                                         "Dec"]
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  call date_and_time(values=idate)
  man = '.TH '//cli%progname//' "1" "'//month(idate(2))//' '//trim(adjustl(strz(4,idate(1))))//'" "version '//cli%version//&
    '" "'//cli%progname//' Manual"'
  man = man//new_line('a')//'.SH NAME'
  man = man//new_line('a')//cli%progname//' - manual page for '//cli%progname//' version '//cli%version
  man = man//new_line('a')//'.SH SYNOPSIS'
  man = man//new_line('a')//'.B '//cli%progname//new_line('a')//trim(adjustl(cli%signature()))
  if (cli%description /= '') man = man//new_line('a')//'.SH DESCRIPTION'//new_line('a')//cli%description
  if (cli%clasg(0)%Na>0) then
    man = man//new_line('a')//'.SH OPTIONS'
    man = man//new_line('a')//cli%usage(no_header=.true.,no_examples=.true.,no_epilog=.true.,g=0)
  endif
  if (allocated(cli%examples)) then
    man = man//new_line('a')//'.SH EXAMPLES'
    man = man//new_line('a')//'.PP'
    man = man//new_line('a')//'.nf'
    man = man//new_line('a')//'.RS'
    do e=1,size(cli%examples,dim=1)
      man = man//new_line('a')//trim(cli%examples(e))
    enddo
    man = man//new_line('a')//'.RE'
    man = man//new_line('a')//'.fi'
    man = man//new_line('a')//'.PP'
  endif
  if (cli%authors /= '') man = man//new_line('a')//'.SH AUTHOR'//new_line('a')//cli%authors
  if (cli%license /= '') man = man//new_line('a')//'.SH COPYRIGHT'//new_line('a')//cli%license
  open(newunit=u,file=trim(adjustl(man_file)))
  if (present(error)) then
    write(u,"(A)",iostat=error)man
  else
    write(u,"(A)")man
  endif
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine save_man_page

  elemental subroutine assign_cli(lhs,rhs)
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Command_Line_Interface), intent(INOUT):: lhs !< Left hand side.
  type(Type_Command_Line_Interface),  intent(IN)::    rhs !< Right hand side.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  ! Type_Object members
  call lhs%assign_object(rhs)
  ! Type_Command_Line_Interface members
  if (allocated(rhs%clasg   )) lhs%clasg      = rhs%clasg
  if (allocated(rhs%examples)) lhs%examples   = rhs%examples
                               lhs%disable_hv = rhs%disable_hv
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine assign_cli
endmodule Data_Type_Command_Line_Interface
