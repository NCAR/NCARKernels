!KGEN-generated Fortran source file 
  
!Generated at : 2019-02-07 15:28:28 
!KGEN version : 0.8.1 
  
! DART software - Copyright UCAR. This open source software is provided
! by UCAR, "as is", without charge, subject to all terms of use at
! http://www.image.ucar.edu/DAReS/DART/DART_download
! $Id: utilities_mod.f90 12417 2018-03-01 23:34:13Z nancy@ucar.edu $


!

module utilities_mod
!-----------------------------------------------------------------------
!   A collection of simple useful routines:
!      file_exist       Function that returns if a given
!                       file name exists
!      get_unit         Function that returns an available 
!                       Fortran unit number
!      error_handler    Print warning and error messages, 
!                       terminates program for error messages.
!      open_file        Opens a given file name for i/o and returns
!                       a unit number.  If the file is already open
!                       the unit number is returned.
!      close_file       Closes the given unit_number. If the file is 
!                       already closed, nothing happens.
!      initialize_utilities    Call this routine once before using any
!                              of the other routines in this file.  If you
!                              are using the MPI library, do not call this
!                              routine -- call mpi_initialize_utilities instead.
!                              It will call this routine as part of initializing
!                              the MPI code.
!      finalize_utilities      Call this routine at the end of a program to close
!                              the log file and flush pending output.  For an MPI
!                              program, call mpi_finalize_utilities instead.
!      dump_unit_attributes    A debug routine that dumps out a long list of
!                              attributes that can be queried from an open file unit.
!      set_output       Set the status of printing.  Can be set on a per-task
!                       basis if you are running with multiple tasks.
!                       By default all warnings and errors print no matter
!                       which task executes the code; messages only print
!                       from task 0 to avoid N copies of identical messages.
!      do_output        Logical function which returns whether informational
!                       messages should be output.  Controlled by the setting
!                       made from set_output.  Useful for messages which cannot
!                       go through the normal error handler (e.g. namelists).
!      find_namelist_in_file   Opens the namelist file and verifies the given
!                              namelist name exists.  If found, rewind the file
!                              and return true.  Then a F90 namelist read command
!                              can be used to read in the namelist values.
!                              Otherwise, print an error message and terminate.
!      check_namelist_read    Confirms that a namelist read was successful. 
!                             If it failed print an error message and terminate.
!      set_nml_output   Set the status of printing namelist data.  By default,
!                       only print to the nml log file.  Can be set to print
!                       to stdout, both, or none.  Argument is a string; valid
!                       values are 'none', 'file', 'terminal', or 'both'.
!      do_nml_file      Logical function which returns whether informational
!                       messages should be output to the file.  Controlled
!                       by a call to set_nml_output().
!      do_nml_term      Logical function which returns whether informational
!                       messages should be output to * (unit 6?).  Controlled
!                       by a call to set_nml_output().
!      set_tasknum      Only called for an MPI job with multiple tasks.
!                       Sets the 'multi-task' flag and records the local task
!                       number for later error and info messages.
!      nc_check         Check netcdf return codes, and if not ok, extract
!                       the netcdf error string and pass that to the error
!                       handler routine.  Two optional strings allow the caller
!                       to provide the subroutine name and some context.
!      write_time       Writes a timestamp in a standard format.
!      logfileunit      Global integer unit numbers for the log file and
!      nmlfileunit      for the namelist file (which defaults to same as log)
!      to_upper         converts a character string to uppercase.
!      find_textfile_dims    finds number of lines and max line length in a 
!                            text file. Used so we can record the namelist 
!                            file content in the netcdf output files.
!      file_to_text     converts the contents of a (hopefully small) file to
!                       a single text variable ... to record in the
!                       netcdf output files.
!      get_next_filename     returns the next filename, given the name of
!                            an ascii file which contains a filename per line.
!                            it returns an empty string at end of list.
!      is_longitude_between  checks whether a given longitude is between
!                            the two given limits, starting at the first and
!                            going EAST until reaching the second.  the end
!                            points are included. if min=max, all points are
!                            considered inside.  there is no rejection of the
!                            input values based on range; they are all converted
!                            to [0-360) by calling modulo() before starting.
!                            default is degrees, but there is an optional
!                            argument to select radians instead.
!      ascii_file_format  function that returns true if the string argument
!                         is indicating the requested format is ascii/text.
!                         false means unformatted/binary.  
! nsc start 31jan07
!   idea - add some unit number routine here?
!   you can extract the filename associated with a fortran unit number
!   with the inquire function on the unit.  this seems useful for
!   automatically generating filenames in messages.  here is an example
!   of how that code works:
!character(len=128) :: filename
!logical :: is_named
!integer :: rc
!inquire(unitnum, named=is_named, name=filename, iostat=rc)
!print *, 'is_named =', is_named, 'name = ', trim(filename)
!if ((rc /= 0) .or. (.not. is_named)) filename = 'unknown file'
! nsc end 31jan07
!-----------------------------------------------------------------------

!
!
!
!
!
!
!
!
!
!
!
!
!
!
!
!                     
!
!
!
!
!
!
!
!
!
!                           
!
!
!
!
!

    USE types_mod, ONLY: r4, r8, digits12, i4, i8 
!>@todo FIXME  netcdf is only needed for the nc_check() routine.
!>when it moves to the netcdf_utilities_mod module remove the 'use'
!>from here to simplify things.

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, kgen_verboselevel, kgen_tolerance, kgen_minvalue, CHECK_IDENTICAL, &
    &CHECK_IN_TOL, CHECK_OUT_TOL 

    IMPLICIT NONE 
    PRIVATE 
! module local data


integer, parameter :: E_DBG = -2,   E_MSG = -1,  E_ALLMSG = 0, E_WARN = 1, E_ERR = 2
integer, parameter :: DEBUG = -1, MESSAGE = 0, WARNING = 1, FATAL = 2
integer, parameter :: NML_NONE = 0, NML_FILE = 1, NML_TERMINAL = 2, NML_BOTH = 3


logical :: do_output_flag = .true.
integer :: nml_flag       = NML_FILE
logical :: single_task    = .true.
integer :: task_number    = 0
logical :: module_initialized = .false.
integer :: logfileunit = -1
integer :: nmlfileunit = -1

PUBLIC file_exist, get_unit, open_file, close_file, register_module, error_handler, to_upper, nc_check, logfileunit, nmlfileunit, &
&initialize_utilities, dump_unit_attributes, find_namelist_in_file, check_namelist_read, do_nml_term, do_output, set_nml_output, &
&do_nml_file, e_dbg, e_msg, e_allmsg, e_warn, e_err, fatal 
! this routine is either in the null_mpi_utilities_mod.f90, or in
! the mpi_utilities_mod.f90 file, but it is not a module subroutine.
! the mpi files use this module, and you cannot have circular module
! references.  the point of this call is that in the mpi multi-task
! case, you want to call MPI_Abort() to kill the other tasks associated
! with this job when you exit.  in the non-mpi case, it just calls exit.

interface
 subroutine exit_all(exitval)
  integer, intent(in) :: exitval
 end subroutine exit_all
end interface
! make a converter from a 1 element 1D array to a scalar.
! (will this have problems compiling if "integer" is coerced to I8 by
! compiler flags?  making to_scalar_int and to_scalar_int8 the same?
! if so, make to_scalar_int explicitly I4, i guess.)


! version controlled file description for error handling, do not edit

character(len=256), parameter :: source   = &
   "$URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/utilities/utilities_mod.f90 $"
character(len=32 ), parameter :: revision = "$Revision: 12417 $"
character(len=128), parameter :: revdate  = "$Date: 2018-03-01 16:34:13 -0700 (Thu, 01 Mar 2018) $"

character(len=512) :: msgstring1, msgstring2, msgstring3
!----------------------------------------------------------------
! Namelist input with default values
! E_ERR All warnings/errors are assumed fatal.


integer            :: TERMLEVEL      = E_ERR   
! default log and namelist output filenames

character(len=256) :: logfilename    = 'dart_log.out'
character(len=256) :: nmlfilename    = 'dart_log.nml'
! output each module subversion details

logical            :: module_details = .true.  
! print messages labeled DBG

logical            :: print_debug    = .false. 
! where to write namelist values.
! valid strings:  'none', 'file', 'terminal', 'both'

character(len=32)  :: write_nml      = 'file'  

PUBLIC kr_externs_in_utilities_mod 
PUBLIC kr_externs_out_utilities_mod 
PUBLIC kv_externs_utilities_mod 
LOGICAL :: kgenref_do_output_flag = .true. 
INTEGER :: kgenref_nml_flag             = nml_file 
LOGICAL :: kgenref_module_initialized = .false. 
INTEGER :: kgenref_logfileunit = -1 
INTEGER :: kgenref_nmlfileunit = -1 
INTEGER :: kgenref_termlevel           = e_err 

contains
!#######################################################################


   subroutine initialize_utilities(progname, alternatename, output_flag)
   character(len=*), intent(in), optional :: progname
   character(len=*), intent(in), optional :: alternatename
   logical, intent(in), optional          :: output_flag
   ! integer :: logfileunit -- public module variable
   integer :: iunit, io

   character(len=256) :: lname
   character(len=512) :: string1,string2,string3

      if ( module_initialized ) then ! nothing to do
         ! write(*,*)'Module initialized ... carry on.'


         return

      else ! initialize the module
         
         module_initialized = .true.

         if (present(output_flag)) do_output_flag = output_flag
         ! Since the logfile is not open yet, the error terminations
         ! must be handled differently than all other cases.
         ! The routines that normally write to the logfile cannot
         ! be used just yet. If we cannot open a logfile, we
         ! always abort execution at this step.


         if ( present(progname) ) then
            if (do_output_flag) write(*,*)'Starting program ',trim(adjustl(progname))
         endif

         if (do_output_flag) write(*,*)'Initializing the utilities module.'
         ! Read the namelist entry

         call find_namelist_in_file("input.nml", "utilities_nml", iunit, .false.)
         call check_namelist_read(iunit, io, "utilities_nml", .false.)
         ! Open the log file with the name from the namelist 

         logfileunit = nextunit()
         if ( logfileunit < 0 ) then
            write(*,*)'   unable to get a unit to use for the logfile.'
            write(*,*)'   stopping.'
            call exit_all(77)
         endif

         if (present(alternatename)) then
            lname = alternatename
         else
            lname = logfilename
         endif

         if (do_output_flag) write(*,*)'Trying to log to unit ', logfileunit
         if (do_output_flag) write(*,*)'Trying to open file ', trim(adjustl(lname))

         open(logfileunit, file=trim(adjustl(lname)), form='formatted', &
                           action='write', position='append', iostat = io )
         if ( io /= 0 ) then
            write(*,*)'FATAL ERROR in initialize_utilities'
            write(*,*)'  ',trim(source)
            write(*,*)'  ',trim(revision)
            write(*,*)'  ',trim(revdate)
            write(*,*)'   unable to open the logfile for writing.'
            write(*,*)'   the logfile name is "',trim(lname),'"'
            write(*,*)'   stopping.'
            call exit_all(77)
         endif
         ! Log the run-time 


         if (do_output_flag) then
            if ( present(progname) ) then
               call write_time (logfileunit, label='Starting ', &
                                string1='Program '//trim(progname))
               call write_time (             label='Starting ', &
                                string1='Program '//trim(progname))
            else
               call write_time (logfileunit, label='Starting ')
               call write_time (             label='Starting ')
            endif 
         endif
         ! Check to make sure termlevel is set to a reasonable value

         call checkTermLevel
         ! Echo the module information using normal mechanism

         call register_module(source, revision, revdate)
         ! Set the defaults for logging the namelist values

         call set_nml_output(write_nml)
         ! If nmlfilename != logfilename, open it.  otherwise set nmlfileunit
         ! to be same as logunit.

         if (do_nml_file()) then
            if (trim(adjustl(nmlfilename)) /= trim(adjustl(lname))) then
               if (do_output_flag) &
                write(*,*)'Trying to open namelist log ', trim(adjustl(nmlfilename))
       
               nmlfileunit = nextunit()
               if (nmlfileunit < 0) &
                  call error_handler(E_ERR,'initialize_utilities', &
                    'Cannot get unit for nm log file', source, revision, revdate)
   
               open(nmlfileunit, file=trim(adjustl(nmlfilename)), form='formatted', &
                    position='append', iostat = io )
               if ( io /= 0 ) then
                  call error_handler(E_ERR,'initialize_utilities', &
                      'Cannot open nm log file', source, revision, revdate)
               endif
       
            else
              nmlfileunit = logfileunit
            endif
         endif
         ! Echo the namelist values for this module using normal mechanism
         ! including a separator line for this run.

         if (do_output_flag) then
            if (do_nml_file() .and. (nmlfileunit /= logfileunit)) then
               if ( present(progname) ) then
                  write(nmlfileunit, *) '!Starting Program '//trim(progname)
               else
                  write(nmlfileunit, *) '!Starting Program '
               endif 
            endif


         endif
         ! Record the values used for variable types:

         if (do_output_flag .and. print_debug) then
        
            write(     *     ,*)  ! a little whitespace is nice
            write(logfileunit,*)  ! a little whitespace is nice

            write(string1,*)'..  digits12 is ',digits12
            write(string2,*)'r8       is ',r8
            write(string3,*)'r4       is ',r4
            call error_handler(E_DBG, 'initialize_utilities', string1, &
                               source, revision, revdate, text2=string2, text3=string3)

            write(string1,*)'..  integer  is ',kind(iunit) ! any integer variable will do
            write(string2,*)'i8       is ',i8
            write(string3,*)'i4       is ',i4
            call error_handler(E_DBG, 'initialize_utilities', string1, &
                               source, revision, revdate, text2=string2, text3=string3)
         endif

      endif

   contains

      function nextunit() result(iunit)
         integer :: iunit

         logical :: open
         integer :: i

         iunit = -1
         UnitLoop : do i = 10, 80
            inquire (i, opened=open)
            if (.not. open) then
               iunit = i
               exit UnitLoop
            endif
         enddo UnitLoop
         if ( iunit < 0 ) then 
            write(*,*)'FATAL ERROR in initialize_utilities'
            write(*,*)'  ',trim(source)
            write(*,*)'  ',trim(revision)
            write(*,*)'  ',trim(revdate)
         endif
      end function nextunit

      subroutine checktermlevel
         select case (TERMLEVEL)
             case (E_MSG)
                ! do nothing
             case (E_ALLMSG)
                ! do nothing
             case (E_WARN)
                ! do nothing
             case (E_ERR)
                ! do nothing
             case default
                print *, ' MESSAGE from initialize_utilities'
                print *, ' namelist input of TERMLEVEL is ',TERMLEVEL
                print *, ' possible values are ',E_MSG, E_ALLMSG, E_WARN, E_ERR
                if (TERMLEVEL < E_WARN ) TERMLEVEL = E_WARN
                if (TERMLEVEL > E_ERR  ) TERMLEVEL = E_ERR
                print *, ' using ',TERMLEVEL
         end select
      end subroutine checktermlevel

   end subroutine initialize_utilities
!#######################################################################


!#######################################################################


   subroutine register_module(src, rev, rdate)
   character(len=*), intent(in) :: src, rev, rdate

      if ( .not. module_initialized ) call initialize_utilities
      if ( .not. do_output_flag) return
      if ( .not. module_details) return


      write(logfileunit,*)
      write(logfileunit,*)'Registering module :'
      write(logfileunit,*)trim(src)
      write(logfileunit,*)trim(rev)
      write(logfileunit,*)trim(rdate)
      write(logfileunit,*)'Registration complete.'
      write(logfileunit,*)

      write(     *     ,*)
      write(     *     ,*)'Registering module :'
      write(     *     ,*)trim(src)
      write(     *     ,*)trim(rev)
      write(     *     ,*)trim(rdate)
      write(     *     ,*)'Registration complete.'
      write(     *     ,*)

   end subroutine register_module
!#######################################################################


!#######################################################################


   function file_exist (file_name)

      character(len=*), intent(in) :: file_name
      logical :: file_exist
      integer :: trimlen

      if ( .not. module_initialized ) call initialize_utilities

      trimlen = len_trim(file_name)

      inquire (file=file_name(1:trimlen), exist=file_exist)

   end function file_exist
!#######################################################################


   function get_unit () result (iunit)

      integer :: i, iunit
      logical :: available

      if ( .not. module_initialized ) call initialize_utilities
! ---- get available unit ----


      iunit = -1
      do i = 10, 80
         inquire (i, opened=available)
         if (.not. available) then
            iunit = i
            exit
         endif
      enddo

      if (iunit == -1) call error_handler(E_ERR,'get_unit', &
             'No available units.', source, revision, revdate)

   end function get_unit
!#######################################################################


   subroutine dump_unit_attributes(iunit) 
!--------------------------------------------------------------------------------
!  subroutine dump_unit_attributes(iunit) 
!  Useful for dumping all the attributes for a file 'unit'
!  A debugging routine, really. TJH Oct 2004
!

      integer, intent(in) :: iunit

      logical :: exists, connected, named_file
      character(len=256) :: file_name
      character(len=512) :: str1
      character(len=32)  :: srname
      character(len=32)  :: ynu     ! YES, NO, UNDEFINED ... among others
      integer :: ios, reclen, nextrecnum

      if ( .not. module_initialized ) call initialize_utilities

      srname = "dump_unit_attributes"
! --- start querying 


      write(str1,*)'for unit ',iunit 
      call error_handler(E_MSG, srname, str1, source, revision, revdate)

      inquire(iunit, opened = connected, iostat=ios)
      if ( connected .and. (ios == 0) ) &
         call error_handler(E_MSG, srname, ' connected', source, revision, revdate)

      inquire(iunit, named = named_file, iostat=ios)
      if ( named_file .and. (ios == 0) ) &
         call error_handler(E_MSG, srname, ' file is named.', source, revision, revdate)

      inquire(iunit, name = file_name, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'file name is ' // trim(adjustl(file_name))
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, exist = exists, iostat=ios)
      if ( exists .and. (ios == 0) ) &
         call error_handler(E_MSG, srname, ' file exists', source, revision, revdate)

      inquire(iunit, recl = reclen, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'record length is ', reclen
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, nextrec = nextrecnum, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'next record is ', nextrecnum
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, access = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'access_type is ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, sequential = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'is file sequential ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, direct = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'is file direct ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, form = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'file format ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, action = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'action ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, read = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'read ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, write = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'write ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, readwrite = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'readwrite ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, blank = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'blank ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, position = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'position ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, delim = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'delim ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

      inquire(iunit, pad = ynu, iostat=ios)
      if ( ios == 0 ) then
         write(str1,*)'pad ', ynu
         call error_handler(E_MSG, srname, str1, source, revision, revdate)
      endif

   end subroutine dump_unit_attributes
!#######################################################################


   subroutine error_mesg (routine, message, level)
!             ------------------------------------
!             |                                  |
!             |    a very simple error handler   |
!             |                                  |
!             ------------------------------------
!  input:
!      routine   name of the calling routine (character string)
!      message   message written to standard output (character string)
!      level     if not equal to zero then the program terminates

!
!
          character(len=*), intent(in) :: routine, message
          integer,          intent(in) :: level

          select case (iabs(level))
             case (0)
                if ( .not. do_output_flag) return
                print *, ' MESSAGE from ',routine(1:len_trim(routine))
                print *, ' ',message(1:len_trim(message))
             case (1)
                print *, ' WARNING in ',routine(1:len_trim(routine))
                print *, ' ',message(1:len_trim(message))
             case default
                print *, ' ERROR in ',routine(1:len_trim(routine))
                print *, ' ',message(1:len_trim(message))
                call exit_all(99)
          end select
!         --------------------------------------------


   end subroutine error_mesg
!#######################################################################


  subroutine error_handler(level, routine, text, src, rev, rdate, aut, text2, text3 )
!----------------------------------------------------------------------
! subroutine error_handler(level, routine, text, src, rev, rdate, aut , text2, text3)
! logs warning/error 
!
implicit none

integer, intent(in) :: level
character(len=*), intent(in) :: routine, text
character(len=*), intent(in), optional :: src, rev, rdate, aut, text2, text3

character(len=8) :: taskstr

if ( .not. module_initialized ) call initialize_utilities
! current choice is to log all errors and warnings regardless
! of setting of output flag.  messages only print from those
! tasks which are allowed to print.


select case(level)
   case (E_MSG)

      if ( .not. do_output_flag) return
      if ( single_task ) then
        write(     *     , *) trim(routine),' ', trim(text)
        write(logfileunit, *) trim(routine),' ', trim(text)
        if ( present(text2)) then
           write(     *     , *) trim(routine),' ... ', trim(text2)
           write(logfileunit, *) trim(routine),' ... ', trim(text2)
        endif
        if ( present(text3)) then
           write(     *     , *) trim(routine),' ... ', trim(text3)
           write(logfileunit, *) trim(routine),' ... ', trim(text3)
        endif
      else
        ! FIXME: should they just all use i5? but most common case is only
        ! messages from PE0, so it's tempting not to waste all those columns.
        if (task_number == 0) then
            write(taskstr, '(a)' ) "PE 0"
        else
            write(taskstr, '(a,i5)' ) "PE ", task_number
        endif
        write(     *     , *) trim(taskstr),': ',trim(routine),' ', trim(text)
        write(logfileunit, *) trim(taskstr),': ',trim(routine),' ', trim(text)
        if ( present(text2)) then
           write(     *     , *) trim(taskstr),': ',trim(routine),' ... ', trim(text2)
           write(logfileunit, *) trim(taskstr),': ',trim(routine),' ... ', trim(text2)
        endif
        if ( present(text3)) then
           write(     *     , *) trim(taskstr),': ',trim(routine),' ... ', trim(text3)
           write(logfileunit, *) trim(taskstr),': ',trim(routine),' ... ', trim(text3)
        endif
      endif

   case (E_ALLMSG)

      if ( single_task ) then
        write(     *     , *) trim(routine),' ', trim(text)
        write(logfileunit, *) trim(routine),' ', trim(text)
        if ( present(text2)) then
           write(     *     , *) trim(routine),' ... ', trim(text2)
           write(logfileunit, *) trim(routine),' ... ', trim(text2)
        endif
        if ( present(text3)) then
           write(     *     , *) trim(routine),' ... ', trim(text3)
           write(logfileunit, *) trim(routine),' ... ', trim(text3)
        endif
      else
        write(taskstr, '(a,i5)' ) "PE ", task_number
        write(     *     , *) trim(taskstr),': ',trim(routine),' ', trim(text)
        if ( present(text2)) then
           write(     *     , *) trim(taskstr),': ',trim(routine),' ... ', trim(text2)
        endif
        if ( present(text3)) then
           write(     *     , *) trim(taskstr),': ',trim(routine),' ... ', trim(text3)
        endif
      endif

   case (E_DBG)
      if (print_debug) then
         ! what about do_output_flag?  want messages from all procs or just PE0?


         if ( single_task ) then
           write(     *     , *) 'DEBUG FROM: ', trim(routine),' ', trim(text)
           write(logfileunit, *) 'DEBUG FROM: ', trim(routine),' ', trim(text)
           if ( present(text2)) then
              write(     *     , *) 'DEBUG FROM: ', trim(routine),' ... ', trim(text2)
              write(logfileunit, *) 'DEBUG FROM: ', trim(routine),' ... ', trim(text2)
           endif
           if ( present(text3)) then
              write(     *     , *) 'DEBUG FROM: ', trim(routine),' ... ', trim(text3)
              write(logfileunit, *) 'DEBUG FROM: ', trim(routine),' ... ', trim(text3)
           endif
         else
           if (task_number == 0) then
               write(taskstr, '(a)' ) "PE 0"
           else
               write(taskstr, '(a,i5)' ) "PE ", task_number
           endif
           write(     *     , *) trim(taskstr),': DEBUG FROM: ',trim(routine),' ', trim(text)
           write(logfileunit, *) trim(taskstr),': DEBUG FROM: ',trim(routine),' ', trim(text)
           if ( present(text2)) then
              write(     *     , *) trim(taskstr),': DEBUG FROM: ',trim(routine),' ... ', trim(text2)
              write(logfileunit, *) trim(taskstr),': DEBUG FROM: ',trim(routine),' ... ', trim(text2)
           endif
           if ( present(text3)) then
              write(     *     , *) trim(taskstr),': DEBUG FROM: ',trim(routine),' ... ', trim(text3)
              write(logfileunit, *) trim(taskstr),': DEBUG FROM: ',trim(routine),' ... ', trim(text3)
           endif
         endif
      endif

   case (E_WARN)

      write(     *     , *) 'WARNING FROM:'
      if ( .not. single_task ) &
      write(     *     , *) ' task id: ', task_number
      write(     *     , *) ' routine: ', trim(routine)
      write(     *     , *) ' message: ', trim(text)
      if ( present(text2)) &
      write(     *     , *) ' message: ... ', trim(text2)
      if ( present(text3)) &
      write(     *     , *) ' message: ... ', trim(text3)
      write(     *     , *) ' '
      write(     *     , *) ' source file: ', trim(src)
      write(     *     , *) ' file revision: ', trim(rev)
      write(     *     , *) ' revision date: ', trim(rdate)
      if(present(aut)) &
      write(     *     , *) ' last editor: ', trim(aut)

      write(logfileunit, *) 'WARNING FROM:'
      if ( .not. single_task ) &
      write(logfileunit, *) ' task id: ', task_number
      write(logfileunit, *) ' routine: ', trim(routine)
      write(logfileunit, *) ' message: ', trim(text)
      if ( present(text2)) &
      write(logfileunit, *) ' message: ... ', trim(text2)
      if ( present(text3)) &
      write(logfileunit, *) ' message: ... ', trim(text3)
      write(logfileunit, *) ' '
      write(logfileunit, *) ' source file: ', trim(src)
      write(logfileunit, *) ' file revision: ', trim(rev)
      write(logfileunit, *) ' revision date: ', trim(rdate)
      if(present(aut)) &
      write(logfileunit, *) ' last editor: ', trim(aut)

   case(E_ERR)

      write(     *     , *) 'ERROR FROM:'
      if ( .not. single_task ) &
      write(     *     , *) ' task id: ', task_number
      write(     *     , *) ' routine: ', trim(routine)
      write(     *     , *) ' message: ', trim(text)
      if ( present(text2)) &
      write(     *     , *) ' message: ... ', trim(text2)
      if ( present(text3)) &
      write(     *     , *) ' message: ... ', trim(text3)
      write(     *     , *) ' '
      write(     *     , *) ' source file: ', trim(src)
      write(     *     , *) ' file revision: ', trim(rev)
      write(     *     , *) ' revision date: ', trim(rdate)
      if(present(aut)) &
      write(     *     , *) ' last editor: ', trim(aut)

      write(logfileunit, *) 'ERROR FROM:'
      if ( .not. single_task ) &
      write(logfileunit, *) ' task id: ', task_number
      write(logfileunit, *) ' routine: ', trim(routine)
      write(logfileunit, *) ' message: ', trim(text)
      if ( present(text2)) &
      write(logfileunit, *) ' message: ... ', trim(text2)
      if ( present(text3)) &
      write(logfileunit, *) ' message: ... ', trim(text3)
      write(logfileunit, *) ' '
      write(logfileunit, *) ' source file: ', trim(src)
      write(logfileunit, *) ' file revision: ', trim(rev)
      write(logfileunit, *) ' revision date: ', trim(rdate)
      if(present(aut)) &
      write(logfileunit, *) ' last editor: ', trim(aut)

end select
! TERMLEVEL gets set in the namelist

if( level >= TERMLEVEL ) call exit_all( 99 ) 

end subroutine error_handler
!#######################################################################


   function open_file (fname, form, action, access, convert, delim, reclen, return_rc) result (iunit)

   character(len=*), intent(in)            :: fname
   character(len=*), intent(in),  optional :: form, action, access, convert, delim
   integer,          intent(in),  optional :: reclen
   integer,          intent(out), optional :: return_rc
   integer  :: iunit

   integer           :: rc, rlen
   logical           :: open, use_recl
   character(len=32) :: format, pos, act, stat, acc, conversion, del

   if ( .not. module_initialized ) call initialize_utilities
   ! if file already open, set iunit and return

   inquire (file=trim(fname), opened=open, number=iunit, iostat=rc)
   if (open) then
      if (present(return_rc)) return_rc = rc
      return
   endif
   ! not already open, so open it.
   ! set defaults, and then modify depending on what user requests
   ! via the arguments.  this combination of settings either creates
   ! a new file or overwrites an existing file from the beginning.

         

   format     = 'formatted'
   act        = 'readwrite'
   pos        = 'rewind'
   stat       = 'unknown'
   acc        = 'sequential'
   rlen       = 1
   del        = 'apostrophe'
   conversion = 'native'

   if (present(form)) format = form
   call to_upper(format)  
   ! change defaults based on intended action.

   if (present(action)) then
       select case(action)

          case ('read', 'READ')
             ! open existing file.  fail if not found.  read from start.
             act  = 'read'
             stat = 'old'

          case ('write', 'WRITE')
             ! create new file/replace existing file.  write at start.
             act  = 'write'
             stat = 'replace'

          case ('append', 'APPEND')
             ! create new/open existing file.  write at end if existing.
             act  = 'readwrite'
             pos  = 'append'

          case default
             ! if the user specifies an action, make sure it is a valid one.
             write(msgstring1,*) 'opening file "'//trim(fname)//'"'
             write(msgstring2,*) 'unrecognized action, "'//trim(action)//'"; valid values: "read", "write", "append"'
             call error_handler(E_ERR, 'open_file', msgstring1, source, revision, revdate, text2=msgstring2)
       end select
   endif
   ! from the ibm help pages:
   !   valid values for access: SEQUENTIAL, DIRECT or STREAM. 
   !   If ACCESS= is DIRECT, RECL= must be specified. 
   !   If ACCESS= is STREAM, RECL= must not be specified.
   !   SEQUENTIAL is the default, for which RECL= is optional
   ! i can't see how to specify all the options in any kind of reasonable way.
   ! but i need to be able to specify 'stream'... so here's a stab at it.


   if (present(access)) then
      acc = access
      call to_upper(acc)
   endif
   ! recl can't apply to stream files, is required for direct,
   ! and is optional for sequential.  ugh.

   if (present(reclen)) then
      rlen = reclen
      use_recl = .true.
   else if (acc == 'DIRECT') then
      use_recl = .true.
   else
      use_recl = .false.
   endif
   ! endian-conversion only applies to binary files
   ! valid values seem to be:  'native', 'big-endian', 'little-endian', and possibly 'cray'
   ! depending on the compiler.

   if (present(convert)) then 
      if (format == 'FORMATTED') then
         write(msgstring1,*) 'opening file "'//trim(fname)//'"'
         write(msgstring2,*) 'cannot specify binary conversion on a formatted file'
         call error_handler(E_ERR, 'open_file ', msgstring1, source, revision, revdate, text2=msgstring2)
      endif
      conversion = convert
   endif
   ! string delimiters only apply to ascii files

   if (present(delim)) then
      if (format /= 'FORMATTED') then
         write(msgstring1,*) 'opening file "'//trim(fname)//'"'
         write(msgstring2,*) 'cannot specify a delimiter on an unformatted file'
         call error_handler(E_ERR, 'open_file ', msgstring1, source, revision, revdate, text2=msgstring2)
      endif
      del = delim
   endif
   ! ok, now actually open the file


   iunit = get_unit()

   if (format == 'FORMATTED') then
      ! formatted file: only pass in recl if required
      if (use_recl) then
         open (iunit, file=trim(fname), form=format, access=acc, recl=rlen, &
               delim=del, position=pos, action=act, status=stat, iostat=rc)
      else
         open (iunit, file=trim(fname), form=format, access=acc,            &
               delim=del, position=pos, action=act, status=stat, iostat=rc)
      endif
   else  
      ! unformatted file - again, only pass in recl if required 
      if (use_recl) then
         open (iunit, file=trim(fname), form=format, access=acc, recl=rlen, &
               convert=conversion, position=pos, action=act, status=stat, iostat=rc)
      else
         open (iunit, file=trim(fname), form=format, access=acc,            &
               convert=conversion, position=pos, action=act, status=stat, iostat=rc)
      endif
   endif
   if (rc /= 0 .and. print_debug) call dump_unit_attributes(iunit) 

   if (present(return_rc)) then
      return_rc = rc
      return
   endif

   if (rc /= 0) then
      write(msgstring1, *)'Cannot open file "'//trim(fname)//'" for '//trim(act)
      write(msgstring2,*)'File may not exist or permissions may prevent the requested operation'
      write(msgstring3,*)'Error code was ', rc
      call error_handler(E_ERR, 'open_file: ', msgstring1, source, revision, revdate, &
                         text2=msgstring2, text3=msgstring3)
   endif

   end function open_file
!#######################################################################


   subroutine write_time (unit, label, string1, string2, string3, tz, brief)
! ***  Write the current time to a log file or standard output ***
!    in: unit number (default is * if not specified)
!    in: label (default is  "Time is" if not specified)
!    in: string1,2,3 (no defaults)
!  default output is a block of 3-4 lines, with dashed line separators
!  and up to 3 descriptive text strings.
!  if brief specified as true, only string1 printed if given,
!  and time printed on same line in YYYY/MM/DD HH:MM:SS format
!  with the tag 'TIME:' before it.  should be easier to postprocess.

!
!

   integer,          optional, intent(in) :: unit
   character(len=*), optional, intent(in) :: label
   character(len=*), optional, intent(in) :: string1
   character(len=*), optional, intent(in) :: string2
   character(len=*), optional, intent(in) :: string3
   logical,          optional, intent(in) :: tz
   logical,          optional, intent(in) :: brief


   integer :: lunit
   character(len= 8) :: cdate
   character(len=10) :: ctime
   character(len= 5) :: zone
   integer, dimension(8) :: values
   logical :: oneline

   if (present(unit)) then
      lunit = unit
   else
      lunit = 6   ! this should be *
   endif

   call DATE_AND_TIME(cdate, ctime, zone, values)
   ! give up if no good values were returned

   if (.not. any(values /= -HUGE(0)) ) return 

   oneline = .false.
   if (present(brief)) oneline = brief

   if (oneline) then
      if (present(string1)) then
         write(lunit,'(A,1X,I4,5(A1,I2.2))') string1//' TIME:', &
                        values(1), '/', values(2), '/', values(3), &
                        ' ', values(5), ':', values(6), ':', values(7)
      else
         write(lunit,'(A,1X,I4,5(A1,I2.2))') 'TIME: ', &
                        values(1), '/', values(2), '/', values(3), &
                        ' ', values(5), ':', values(6), ':', values(7)
      endif
   else
      write(lunit,*)
      write(lunit,*)'--------------------------------------'
      if ( present(label) ) then
         write(lunit,*) label // '... at YYYY MM DD HH MM SS = '
      else
         write(lunit,*) 'Time is  ... at YYYY MM DD HH MM SS = '
      endif 
      write(lunit,'(17x,i4,5(1x,i2))') values(1), values(2), &
                        values(3),  values(5), values(6), values(7)
   
      if(present(string1)) write(lunit,*)trim(string1)
      if(present(string2)) write(lunit,*)trim(string2)
      if(present(string3)) write(lunit,*)trim(string3)
   
      if (present(tz)) then
         if ( values(4) /= -HUGE(0) .and. tz) &
            write(lunit,*)'time zone offset is ',values(4),' minutes.'
      endif
   
      write(lunit,*)'--------------------------------------'
      write(lunit,*)
   endif

   end subroutine write_time
!#######################################################################


!#######################################################################


   function do_output ()
! *** return whether output should be written from this task ***

!

   logical :: do_output

   if ( .not. module_initialized ) call initialize_utilities

   do_output = do_output_flag

   end function do_output
!#######################################################################


   subroutine set_nml_output (nmlstring)
! *** set whether nml output is written to stdout file or only nml file
!    in:  doflag  = whether to output nml information to stdout 

!

   character(len=*), intent(in) :: nmlstring

   if ( .not. module_initialized ) call initialize_utilities

   select case (nmlstring)
      case ('NONE', 'none')
         nml_flag = NML_NONE
         call error_handler(E_MSG, 'set_nml_output', &
                            'No echo of NML values')

      case ('FILE', 'file')
         nml_flag = NML_FILE
         call error_handler(E_MSG, 'set_nml_output', &
                            'Echo NML values to log file only')
  
      case ('TERMINAL', 'terminal')
         nml_flag = NML_TERMINAL
         call error_handler(E_MSG, 'set_nml_output', &
                            'Echo NML values to terminal output only')
   
      case ('BOTH', 'both')
         nml_flag = NML_BOTH
         call error_handler(E_MSG, 'set_nml_output', &
                            'Echo NML values to both log file and terminal')

      case default
         call error_handler(E_ERR, 'set_nml_output', &
           'unrecognized input string: '//trim(nmlstring), &
           source, revision, revdate)
 
   end select

   end subroutine set_nml_output
!#######################################################################


   function do_nml_file ()
! *** return whether nml should be written to nml file

!

   logical :: do_nml_file

   if ( .not. module_initialized ) call initialize_utilities

   if ( .not. do_output()) then
      do_nml_file = .false.
   else
      do_nml_file = (nml_flag == NML_FILE .or. nml_flag == NML_BOTH)
   endif

   end function do_nml_file
!#######################################################################


   function do_nml_term ()
! *** return whether nml should be written to terminal

!

   logical :: do_nml_term

   if ( .not. module_initialized ) call initialize_utilities

   if ( .not. do_output()) then
      do_nml_term = .false.
   else
      do_nml_term = (nml_flag == NML_TERMINAL .or. nml_flag == NML_BOTH)
   endif

   end function do_nml_term
!#######################################################################


!#######################################################################


subroutine close_file(iunit)
!-----------------------------------------------------------------------
! Closes the given unit_number if that unit is open.
! Not an error to call on an already closed unit.
! Will print a message if the status of the unit cannot be determined.
!
!

integer, intent(in) :: iunit

integer :: ios
logical :: open

if ( .not. module_initialized ) call initialize_utilities

inquire (unit=iunit, opened=open, iostat=ios)
if ( ios /= 0 ) then
   write(msgstring1,*)'Unable to determine status of file unit ', iunit
   call error_handler(E_MSG, 'close_file: ', msgstring1, source, revision, revdate)
endif

if (open) close(iunit)

end subroutine close_file
!#######################################################################


subroutine find_namelist_in_file(namelist_file_name, nml_name, iunit, &
   write_to_logfile_in)
!-----------------------------------------------------------------------
! Opens namelist_file_name if it exists on unit iunit, error if it
! doesn't exist.
! Searches file for a line containing ONLY the string
! &nml_name, for instance &filter_nml. If found, backs up one record and
! returns true. Otherwise, error message and terminates
! 
!

character(len=*),  intent(in)  :: namelist_file_name
character(len=*),  intent(in)  :: nml_name
integer,           intent(out) :: iunit
logical, optional, intent(in)  :: write_to_logfile_in

character(len=256) :: nml_string, test_string, string1
integer            :: io
logical            :: write_to_logfile
! Decide if there is a logfile or not


write_to_logfile = .true.
if(present(write_to_logfile_in)) write_to_logfile = write_to_logfile_in
! Check for file existence; no file is an error

if(file_exist(trim(namelist_file_name))) then

   iunit = open_file(trim(namelist_file_name), action = 'read')
   ! Read each line until end of file is found
   ! Look for the start of a namelist with &nml_name
   ! Convert test string to all uppercase ... since that is
   ! what happens if Fortran writes a namelist.


   string1 = adjustl(nml_name)
   call to_upper(string1)             ! works in-place
   test_string = '&' // trim(string1)

   do
      read(iunit, '(A)', iostat = io) nml_string
      if(io /= 0) then
         ! No values for this namelist; error
         write(msgstring1, *) 'Namelist entry &', nml_name, ' must exist in ', namelist_file_name
         ! Can't write to logfile if it hasn't yet been opened
         if(write_to_logfile) then
            call error_handler(E_ERR, 'find_namelist_in_file', msgstring1, &
               source, revision, revdate)
         else
            write(*, *) 'FATAL ERROR before logfile initialization in utilities_mod'
            write(*, *) 'Error is in subroutine find_namelist_in_file'
            write(*, *) msgstring1
            write(*,*)'  ',trim(source)
            write(*,*)'  ',trim(revision)
            write(*,*)'  ',trim(revdate)
            call exit_all(88) 
         endif
      else

         string1 = adjustl(nml_string)
         call to_upper(string1)

         if(trim(string1) == trim(test_string)) then
            backspace(iunit)
            return
         endif

      endif
   end do
else
   ! No namelist_file_name file is an error
   write(msgstring1, *) 'Namelist input file: ', namelist_file_name, ' must exist.'
   if(write_to_logfile) then
      call error_handler(E_ERR, 'find_namelist_in_file', msgstring1, &
         source, revision, revdate)
   else
      write(*, *) 'FATAL ERROR before logfile initialization in utilities_mod'
      write(*, *) 'Error is in subroutine find_namelist_in_file'
      write(*, *) msgstring1
      write(*,*)'  ',trim(source)
      write(*,*)'  ',trim(revision)
      write(*,*)'  ',trim(revdate)
      call exit_all(88) 
   endif
endif

end subroutine find_namelist_in_file
!#######################################################################


subroutine check_namelist_read(iunit, iostat_in, nml_name, &
   write_to_logfile_in)
!-----------------------------------------------------------------------
! Confirms that a namelist read was successful. If it failed
! produces an error message and stops execution.
! 
!

integer,            intent(in) :: iunit, iostat_in
character(len=*), intent(in) :: nml_name
logical, intent(in), optional :: write_to_logfile_in

character(len=256) :: nml_string
integer            :: io
logical            :: write_to_logfile
! Decide if there is a logfile or not

write_to_logfile = .true.
if(present(write_to_logfile_in)) write_to_logfile = write_to_logfile_in

if(iostat_in == 0) then
   ! If the namelist read was successful, just close the file
   call close_file(iunit)
else
   ! If it wasn't successful, print the line on which it failed  
   backspace(iunit)
   read(iunit, '(A)', iostat = io) nml_string
   ! A failure in this read means that the namelist started but never terminated
   ! Result was falling off the end, so backspace followed by read fails
   if(io /= 0) then
      write(msgstring1, *) 'Namelist ', trim(nml_name), ' started but never terminated'
      if(write_to_logfile) then
         call error_handler(E_ERR, 'check_namelist_read', msgstring1, &
            source, revision, revdate)
      else
         write(*, *) 'FATAL ERROR before logfile initialization in utilities_mod'
         write(*, *) 'Error is in subroutine check_namelist_read'
         write(*, *) msgstring1
         write(*,*)'  ',trim(source)
         write(*,*)'  ',trim(revision)
         write(*,*)'  ',trim(revdate)
         call exit_all(66) 
      endif
   else
      ! Didn't fall off end so bad entry in the middle of namelist
      write(msgstring1, *) 'INVALID NAMELIST ENTRY: ', trim(nml_string), ' in namelist ', trim(nml_name)
      if(write_to_logfile) then
         call error_handler(E_ERR, 'check_namelist_read', msgstring1, &
            source, revision, revdate)
      else
         write(*, *) 'FATAL ERROR before logfile initialization in utilities_mod'
         write(*, *) 'Error is in subroutine check_namelist_read'
         write(*, *) msgstring1
         write(*,*)'  ',trim(source)
         write(*,*)'  ',trim(revision)
         write(*,*)'  ',trim(revdate)
         call exit_all(66) 
      endif
   endif
endif

end subroutine check_namelist_read
!#######################################################################


   subroutine nc_check(istatus, subr_name, context)
      integer, intent (in)                   :: istatus
      character(len=*), intent(in)           :: subr_name
      character(len=*), intent(in), optional :: context
  
      character(len=512) :: error_msg
      ! if no error, nothing to do here.  we are done.
  

      ! something wrong.  construct an error string and call the handler.
      ! context is optional, but is very useful if specified.
      ! if context + error code > 129, the assignment will truncate.


      if (present(context) ) then
      else
      endif
      ! this does not return 

      call error_mesg(subr_name, error_msg, FATAL)
  

   end subroutine nc_check
!#######################################################################


subroutine to_upper( string )
! Converts 'string' to uppercase
character(len=*), intent(INOUT) :: string
integer :: ismalla, ibiga, i

ismalla = ichar('a')
ibiga   = ichar('A')

do i = 1,len(string)
   if ((string(i:i) >= 'a') .and. (string(i:i) <= 'z')) then
        string(i:i)  = char( ichar(string(i:i)) + ibiga - ismalla)
   endif
enddo

end subroutine to_upper
!#######################################################################
!> copy instring to outstring, omitting all internal blanks
!> outstring must be at least as long as instring


!#######################################################################


!#######################################################################


!#######################################################################


!#######################################################################
!> this function is intended to be used when there are 2 ways to specify 
!> an unknown number of input files, most likely in a namelist.
!>
!> e.g. to specify 3 input files named 'file1', 'file2', 'file3' you can
!> either give:
!>   input_files = 'file1', 'file2', 'file3' 
!>     OR
!>   input_file_list = 'flist'  
!>
!>   and the contents of text file 'flist' are (e.g. 'cat flist' gives)
!>          file1
!>          file2
!>          file3
!>   each filename is on a separate line with no quotes
!>
!> you pass both those variables into this function and when it returns
!> the 'name_array' will have all the filenames as an array of strings.
!> it will have opened the text files, if given, and read in the contents.
!> it returns the number of filenames it found.
!>
!> contrast this with the following function where you tell it how many
!> names and/or indirect files you expect, and how many filenames you
!> expect to have at the end.


!#######################################################################
!> this function is intended to be used when there are 2 ways to specify 
!> a KNOWN number of input files, most likely in a namelist.
!>
!> e.g. to specify 6 input files named 'file1a', 'file2a', 'file3a', 
!>                                     'file1b', 'file2b', 'file3b', 
!> either give:
!>   input_files = 'file1a', 'file2a', 'file3a', 'file1b', 'file2b', 'file3b', 
!>     OR
!>   input_file_list = 'flistA', 'flistB'
!>
!>   and the contents of text file 'flistA' are (e.g. 'cat flistA' gives)
!>          file1a
!>          file2a
!>          file3a
!>   and the contents of text file 'flistB' are (e.g. 'cat flistB' gives)
!>          file1b
!>          file2b
!>          file3b
!>
!>   each filename is on a separate line with no quotes, and the result
!>   is all of the contents of list1 followed by list2, or the files in
!>   order as they're given explicitly in the first string array.
!>
!> you pass both those variables into this function and when it returns
!> the 'name_array' will have all the filenames as an array of strings.
!> it will have opened the text files, if given, and read in the contents.
!> it dies with a fatal error if it can't construct a list of the right length.
!> (the 'right length' is nlists * nentries)
!>
!> contrast this with the previous function where you don't know (or care)
!> how many filenames are specified, and there's only a single listlist option.


!#######################################################################


!#######################################################################


!#######################################################################


!#######################################################################


!#######################################################################


!#######################################################################


!-----------------------------------------------------------------------
!>


!-----------------------------------------------------------------------
!>


!-----------------------------------------------------------------------
!>  if matching true or false, uppercase the string so any
!>  combination of upper and lower case matches.  match with
!>  and without the enclosing periods (e.g. .true. and true
!>  both match, as would .TrUe.).  also allow plain T and F.
!>  if 'string to match' is provided, no upper casing is done
!>  and the string must match exactly.


!=======================================================================
! End of utilities_mod
!=======================================================================


!read state subroutine for kr_externs_in_utilities_mod 
SUBROUTINE kr_externs_in_utilities_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
      
    READ (UNIT = kgen_unit) do_output_flag 
    READ (UNIT = kgen_unit) nml_flag 
    READ (UNIT = kgen_unit) single_task 
    READ (UNIT = kgen_unit) task_number 
    READ (UNIT = kgen_unit) module_initialized 
    READ (UNIT = kgen_unit) logfileunit 
    READ (UNIT = kgen_unit) nmlfileunit 
    READ (UNIT = kgen_unit) msgstring2 
    READ (UNIT = kgen_unit) msgstring3 
    READ (UNIT = kgen_unit) msgstring1 
    READ (UNIT = kgen_unit) termlevel 
    READ (UNIT = kgen_unit) logfilename 
    READ (UNIT = kgen_unit) nmlfilename 
    READ (UNIT = kgen_unit) module_details 
    READ (UNIT = kgen_unit) print_debug 
    READ (UNIT = kgen_unit) write_nml 
END SUBROUTINE kr_externs_in_utilities_mod 
  
!read state subroutine for kr_externs_out_utilities_mod 
SUBROUTINE kr_externs_out_utilities_mod(kgen_unit) 
    INTEGER, INTENT(IN) :: kgen_unit 
      
    LOGICAL :: kgen_istrue 
    REAL(KIND=8) :: kgen_array_sum 
    READ (UNIT = kgen_unit) kgenref_do_output_flag 
    READ (UNIT = kgen_unit) kgenref_nml_flag 
    READ (UNIT = kgen_unit) kgenref_module_initialized 
    READ (UNIT = kgen_unit) kgenref_logfileunit 
    READ (UNIT = kgen_unit) kgenref_nmlfileunit 
    READ (UNIT = kgen_unit) kgenref_termlevel 
END SUBROUTINE kr_externs_out_utilities_mod 
  
!verify state subroutine for kv_externs_utilities_mod 
SUBROUTINE kv_externs_utilities_mod(check_status) 
    TYPE(check_t), INTENT(INOUT) :: check_status 
      
    CALL kv_utilities_mod_logical__("do_output_flag", check_status, do_output_flag, kgenref_do_output_flag) 
    CALL kv_utilities_mod_integer__("nml_flag", check_status, nml_flag, kgenref_nml_flag) 
    CALL kv_utilities_mod_logical__("module_initialized", check_status, module_initialized, kgenref_module_initialized) 
    CALL kv_utilities_mod_integer__("logfileunit", check_status, logfileunit, kgenref_logfileunit) 
    CALL kv_utilities_mod_integer__("nmlfileunit", check_status, nmlfileunit, kgenref_nmlfileunit) 
    CALL kv_utilities_mod_integer__("termlevel", check_status, termlevel, kgenref_termlevel) 
END SUBROUTINE kv_externs_utilities_mod 
  
!verify state subroutine for kv_utilities_mod_logical__ 
RECURSIVE SUBROUTINE kv_utilities_mod_logical__(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    LOGICAL, INTENT(IN) :: var, kgenref_var 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    logical :: diff 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    IF (var .EQV. kgenref_var) THEN 
        check_status%numIdentical = check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        check_status%numOutTol = check_status%numOutTol + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_OUT_TOL 
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "NOT IMPLEMENTED" 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 0) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "NOT IMPLEMENTED" 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "NOT IMPLEMENTED" 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
END SUBROUTINE kv_utilities_mod_logical__ 
  
!verify state subroutine for kv_utilities_mod_integer__ 
RECURSIVE SUBROUTINE kv_utilities_mod_integer__(varname, check_status, var, kgenref_var) 
    CHARACTER(LEN=*), INTENT(IN) :: varname 
    TYPE(check_t), INTENT(INOUT) :: check_status 
    INTEGER, INTENT(IN) :: var, kgenref_var 
    INTEGER :: check_result 
    LOGICAL :: is_print = .FALSE. 
      
    integer :: diff 
      
    check_status%numTotal = check_status%numTotal + 1 
      
    IF (var == kgenref_var) THEN 
        check_status%numIdentical = check_status%numIdentical + 1 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
            END IF   
        END IF   
        check_result = CHECK_IDENTICAL 
    ELSE 
        diff = ABS(var - kgenref_var) 
        IF (diff <= kgen_tolerance) THEN 
            check_status%numInTol = check_status%numInTol + 1 
            IF (kgen_verboseLevel > 1) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_IN_TOL 
        ELSE 
            check_status%numOutTol = check_status%numOutTol + 1 
            IF (kgen_verboseLevel > 0) THEN 
                IF (check_status%rank == 0) THEN 
                    WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                END IF   
            END IF   
            check_result = CHECK_OUT_TOL 
        END IF   
    END IF   
    IF (check_result == CHECK_IDENTICAL) THEN 
        IF (kgen_verboseLevel > 2) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "Difference is ", 0 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_OUT_TOL) THEN 
        IF (kgen_verboseLevel > 0) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "Difference is ", diff 
                WRITE (*, *) "" 
            END IF   
        END IF   
    ELSE IF (check_result == CHECK_IN_TOL) THEN 
        IF (kgen_verboseLevel > 1) THEN 
            IF (check_status%rank == 0) THEN 
                WRITE (*, *) "Difference is ", diff 
                WRITE (*, *) "" 
            END IF   
        END IF   
    END IF   
      
END SUBROUTINE kv_utilities_mod_integer__ 
  
end module utilities_mod
! <next few lines under version control, do not edit>
! $URL: https://svn-dares-dart.cgd.ucar.edu/DART/releases/Manhattan/assimilation_code/modules/utilities/utilities_mod.f90 $
! $Id: utilities_mod.f90 12417 2018-03-01 23:34:13Z nancy@ucar.edu $
! $Revision: 12417 $
! $Date: 2018-03-01 16:34:13 -0700 (Thu, 01 Mar 2018) $
