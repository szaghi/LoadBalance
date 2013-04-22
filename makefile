#!/usr/bin/make
#----------------------------------------------------------------------------------------------------------------------------------
# make init

# shell
SHELL = /bin/bash
# no verbose
$(VERBOSE).SILENT:
#----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
# options
COMPILER  = intel
DEBUG     = yes
F03STD    = yes
PROFILING = no
OPTIMIZE  = no
OPENMP    = no

.PHONY : DEFAULTRULE
DEFAULTRULE: $(DEXE)loadbalance
#----------------------------------------------------------------------------------------------------------------------------------

#----------------------------------------------------------------------------------------------------------------------------------
# directory & file
DSRC  = ./src/
DOBJ  = ./obj/
DMOD  = ./mod/
DLIB  = ./lib/
DEXE  = ./
MKDIRS = $(DOBJ) $(DMOD) $(DEXE)
VPATH = $(DSRC) $(DOBJ) $(DMOD) $(DLIB)
WHICHFC = $(shell which $(FC))
PRINTCHK = "\\033[1;31m Compiler used   \\033[0m\\033[1m $(FC) => $(WHICHFC)\\033[0m \n\
            \\033[1;31mSources dir     \\033[0m\\033[1m $(DSRC)\\033[0m \n\
            \\033[1;31mObjects dir     \\033[0m\\033[1m $(DOBJ)\\033[0m \n\
            \\033[1;31mModules dir     \\033[0m\\033[1m $(DMOD)\\033[0m \n\
            \\033[1;31mExecutables dir \\033[0m\\033[1m $(DEXE)\\033[0m \n\
            \\033[1;31mLibraries       \\033[0m\\033[1m $(LIBS)\\033[0m"
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
ifeq "$(COMPILER)" "gnu"
  FC = gfortran-4.7
  OPTSC   = -cpp -c -J$(DMOD)
  OPTSL   =
  # debug
  ifeq "$(DEBUG)" "yes"
    OPTSC := $(OPTSC) -O0 -Wall -Wno-array-temporaries -Warray-bounds -fcheck=all -fbacktrace -ffpe-trap=invalid,overflow,underflow
    OPTSL := $(OPTSL) -O0 -Wall -Wno-array-temporaries -Warray-bounds -fcheck=all -fbacktrace -ffpe-trap=invalid,overflow,underflow
  endif
  # standard
  ifeq "$(F03STD)" "yes"
    OPTSC := $(OPTSC) -std=f2008 -fall-intrinsics
    OPTSL := $(OPTSL) -std=f2008 -fall-intrinsics
  endif
  # optimization
  ifeq "$(OPTIMIZE)" "yes"
    OPTSC := $(OPTSC) -O3
    OPTSL := $(OPTSL) -O3
  endif
  # openmp
  ifeq "$(OPENMP)" "yes"
    OPTSC := $(OPTSC) -fopenmp
    OPTSL := $(OPTSL) -fopenmp
  endif
endif
ifeq "$(COMPILER)" "intel"
  FC = ifort
  OPTSC   = -cpp -c -module $(DMOD)
  OPTSL   =
  # debug
  ifeq "$(DEBUG)" "yes"
    CHK = -check all
    DEB = -debug all
    WRN = -warn all
    OPTSC := $(OPTSC) -O0 -fpe-all=0 -fp-stack-check -traceback $(WRN) $(CHK) $(DEB)
    OPTSL := $(OPTSL) -O0 -fpe-all=0 -fp-stack-check -traceback $(WRN) $(CHK) $(DEB)
  endif
  # standard
  ifeq "$(F03STD)" "yes"
    OPTSC := $(OPTSC) -std03
    OPTSL := $(OPTSL) -std03
  endif
  # optimization
  ifeq "$(OPTIMIZE)" "yes"
    OPTSC := $(OPTSC) -O3 -ipo -inline all
    OPTSL := $(OPTSL) -O3 -ipo -inline all
  endif
  # openmp
  ifeq "$(OPENMP)" "yes"
    OPTSC := $(OPTSC) -openmp
    OPTSL := $(OPTSL) -openmp
    PREPROC := $(PREPROC) -DOPENMP
  endif
endif
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
# auxiliary rules
.PHONY : PRINTINFO
.NOTPARALLEL : PRINTINFO
PRINTINFO:
	@echo | tee make.log
	@echo -e $(PRINTCHK) | tee -a make.log
	@echo | tee -a make.log
	@echo -e "\033[1;31m Compiling options\033[0m" | tee -a make.log
	@echo -e "\033[1m [$(OPTSC)]\033[0m" | tee -a make.log
	@echo | tee -a make.log
	@echo -e "\033[1;31m Linking options \033[0m" | tee -a make.log
	@echo -e "\033[1m [$(OPTSL)]\033[0m" | tee -a make.log
	@echo | tee -a make.log

.PHONY : $(MKDIRS)
$(MKDIRS):
	@mkdir -p $@

.PHONY : cleanobj
cleanobj:
	@echo -e "\033[1;31m deleting objects \033[0m" | tee make.log
	@rm -fr $(DOBJ)

.PHONY : cleanmod
cleanmod:
	@echo -e "\033[1;31m deleting mods \033[0m" | tee -a make.log
	@rm -fr $(DMOD)

.PHONY : cleanexe
cleanexe:
	@echo -e "\033[1;31m deleting exes \033[0m" | tee -a make.log
	@rm -f $(addprefix $(DEXE),$(EXES))

.PHONY : cleanmsg
cleanmsg:
	@rm -f diagnostic_messages
	@rm -f error_messages

.PHONY : clean
clean: cleanobj cleanmod cleanmsg

.PHONY : cleanall
cleanall: clean cleanexe

.PHONY : tar
tar: cleanall
	@echo -e "\033[1;31m Creating tar archive of the code \033[0m" | tee make.log
	@rm -rf LoadBalance
	@mkdir -p LoadBalance
	@cp -rL src makefile LoadBalance/
	@tar czf LoadBalance.tgz LoadBalance
	@rm -rf LoadBalance
#-----------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------
# rules of linking and compiling
COTEXT  = -e "\033[1;31m Compiling\033[0m\033[1m $(<F)\033[0m"
LITEXT  = -e "\033[1;31m Assembling\033[0m\033[1m $@\033[0m"
LCEXES  = $(shell echo $(EXES) | tr '[:upper:]' '[:lower:]')
EXESPO  = $(addsuffix .o,$(LCEXES))
EXESOBJ = $(addprefix $(DOBJ),$(EXESPO))

$(DEXE)loadbalance : PRINTINFO $(MKDIRS) $(DOBJ)loadbalance.o
	@rm -f $(filter-out $(DOBJ)loadbalance.o,$(EXESOBJ))
	@echo | tee -a make.log
	@echo $(LITEXT) | tee -a make.log
	@$(FC) $(OPTSL) $(DOBJ)*.o $(LIBS) -o $@ 1>> diagnostic_messages 2>> error_messages
EXES := $(EXES) loadbalance

$(DOBJ)data_type_os.o : Data_Type_OS.f90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)data_type_sl_list.o : Data_Type_SL_List.f90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)data_type_time.o : Data_Type_Time.f90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)data_type_vector.o : Data_Type_Vector.f90 \
	$(DOBJ)ir_precision.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)ir_precision.o : IR_Precision.f90
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_io_misc.o : Lib_IO_Misc.f90 \
	$(DOBJ)ir_precision.o \
	$(DOBJ)data_type_os.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)lib_math.o : Lib_Math.f90 \
	$(DOBJ)ir_precision.o \
	$(DOBJ)data_type_sl_list.o \
	$(DOBJ)data_type_vector.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)loadbalance.o : loadbalance.f90 \
	$(DOBJ)ir_precision.o \
	$(DOBJ)data_type_sl_list.o \
	$(DOBJ)data_type_time.o \
	$(DOBJ)lib_io_misc.o \
	$(DOBJ)lib_math.o \
	$(DOBJ)ir_precision.o \
	$(DOBJ)data_type_sl_list.o \
	$(DOBJ)lib_io_misc.o
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

$(DOBJ)%.o : %.f90
	@echo $(COTEXT) | tee -a make.log
	@$(FC) $(OPTSC) $< -o $@ 1>> diagnostic_messages 2>> error_messages

#-----------------------------------------------------------------------------------------------------------------------------------
