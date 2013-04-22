# Load Balance

Load Balance is program for performing load balancing of Xnavis code. It analyzes the input mesh and produces bsplit and cutter inputs for a well-balanced parallel simulation.

The program is written in pure Fortran (standard 2003, with the only exception of __convert__ open specifier that is not standard).

## Compiling

Load Balance is develop with Intel Fortran Compiler (12.x or higher). The makefile has a help message:

> make help

     Make options of Load Balance code

     Compiler choice: COMPILER=intel => default
      COMPILER=gnu   => GNU gfortran
      COMPILER=intel => Intel Fortran

     Compiling options
      DEBUG=yes(no)    => on(off) debug                  (default yes)
      F03STD=yes(no)   => on(off) check standard fortran (default yes)
      OPTIMIZE=yes(no) => on(off) optimization           (default no)
      OPENMP=yes(no)   => on(off) OpenMP directives      (default no)

     Provided Rules: default=loadbalance => compile the code
      help         => printing this help message
      loadbalance  => compile the library
      cleanobj     => cleaning compiled object
      cleanmod     => cleaning .mod files
      cleanmsg     => cleaning make-log massage files
      clean        => running cleanobj, cleanmod and cleanmsg
      cleanall     => running clean and cleanexe
      tar          => creating a tar archive of the project
      doc          => building the documentation


To compile in debug mode:

> make DEBUG=yes

To compile in production mode:

> make DEBUG=no OPTIMIZE=yes

### Usage

Load Balance has a help message that is echoed if it is called without command line arguments:

> loadbalance

     Load balancing for Xnavis
       Usage:
         loadbalance [-swt [arg]]
           -i input_file_name
           -np #procs (default 8)
           -be => big endian bits ordering
           -le => little endian bits ordering (default)
           -pf prime_factor_list (default=[3,2]); "-" separated value => e.g. -pf 9-7-5-3-2
           -b balancing_tolerance (default 3.0%)
           -imax maximum_balancing_iteration (default 1000)
           -mgl number_of_multi-grid_levels (default 4)
           -save_blk_his => save splitting history of each original block (default no)
           -save_lvl_lis => save level block list for each level (default no)
       Examples:
         loadbalance -i cc.01                      ! little endian input file and 8 procs
         loadbalance -i cc.01 -np 64 -pf 7-5-3-2   ! particular prime factors list
         loadbalance -i cc.01 -np 16 -be           ! big endian input file and 16 procs
         loadbalance -i cc.01 -np 32 -b 2.         ! maximum admissible unbalance factor of 2%
         loadbalance -i cc.01 -np 16 -imax 100     ! maximum number of balancing iterations set to 100
         loadbalance -i cc.01 -np 32 -save_blk_his ! save block splitting history for each original block

## Todo

- Complete documentation.

## Copyrights

The Load Balance program is an open source project, it is distributed under the [GPL v3](http://www.gnu.org/licenses/gpl-3.0.html). Anyone is interest to use, to develop or to contribute to Load Balance is welcome.
