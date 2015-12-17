module Data_Type_Proc
!-----------------------------------------------------------------------------------------------------------------------------------
USE IR_Precision
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
private
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
type, public:: Type_Proc
  !< Definition of processor data structure.
  ! processor data
  real(R8P)::    Wpr=0._R8P       !< Work load ratio of the processor.
  integer(I8P):: Wp=0             !< Work load of the processor.
  logical::      balanced=.false. !< Flag for checking if the processor is balanced.
  integer(I4P):: Nb=0             !< Number of blocks of the processor.
  contains
    procedure:: init => init_proc ! Procedure for initializing processor data.
    procedure:: set => set_proc   ! Procedure for setting processor data.
endtype Type_Proc
!-----------------------------------------------------------------------------------------------------------------------------------
contains
  subroutine init_proc(proc)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Initialize processor data.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Proc), intent(INOUT):: proc !< Processor (self) data.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  proc%Wpr = 0._R8P
  proc%Wp = 0
  proc%balanced = .false.
  proc%Nb = 0
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine init_proc

  subroutine set_proc(proc,Wpr,Wp,balanced,Nb)
  !---------------------------------------------------------------------------------------------------------------------------------
  !< Set processor data.
  !---------------------------------------------------------------------------------------------------------------------------------
  implicit none
  class(Type_Proc),       intent(INOUT):: proc     !< Processor (self) data.
  real(R8P),    optional, intent(IN)::    Wpr      !< Work load ratio of the processor.
  integer(I8P), optional, intent(IN)::    Wp       !< Work load of the processor.
  logical,      optional, intent(IN)::    balanced !< Flag for checking if the processor is balanced.
  integer(I4P), optional, intent(IN)::    Nb       !< Number of blocks of the processor.
  !---------------------------------------------------------------------------------------------------------------------------------

  !---------------------------------------------------------------------------------------------------------------------------------
  if (present(Wpr)) proc%Wpr = Wpr
  if (present(Wp)) proc%Wp = Wp
  if (present(balanced)) proc%balanced = balanced
  if (present(Nb)) proc%Nb = Nb
  return
  !---------------------------------------------------------------------------------------------------------------------------------
  endsubroutine set_proc
endmodule Data_Type_Proc
