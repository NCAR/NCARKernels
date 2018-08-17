NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:51:28 2018
FILE NAME: kernel_driver.F90

PROCEDURE NAME: MAIN_
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

    37: inl(1212): Source for routine not found.: KGEN_UTILS_MOD::KGEN_GET_NEWUNIT
    51: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_CAM::KR_EXTERNS_IN_MICRO_MG_CAM
    51: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG2_0::KR_EXTERNS_IN_MICRO_MG2_0
    51: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::KR_EXTERNS_IN_MICRO_MG_UTILS
    51: vec( 110): Vectorization obstructive procedure reference.: WV_SAT_METHODS::KR_EXTERNS_IN_WV_SAT_METHODS
    51: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_CAM::MICRO_MG_CAM_TEND_PACK
    51: vec( 110): Vectorization obstructive procedure reference.: KGEN_UTILS_MOD::KGEN_RANKTHREADINVOKE
    51: vec( 182): Deallocation obstructs vectorization.
    51: vec( 181): Allocation obstructs vectorization.
    51: vec( 110): Vectorization obstructive procedure reference.: KGEN_UTILS_MOD::KGEN_GET_NEWUNIT
    51: vec( 180): I/O statement obstructs vectorization.
    55: inl(1212): Source for routine not found.: KGEN_UTILS_MOD::KGEN_RANKTHREADINVOKE
    55: vec( 118): Unvectorizable data type.
    55: vec( 103): Unvectorized loop.
    55: vec( 108): Unvectorizable loop structure.
    56: vec( 118): Unvectorizable data type.
    56: vec( 103): Unvectorized loop.
    56: vec( 108): Unvectorizable loop structure.
    60: vec( 118): Unvectorizable data type.
    60: vec( 103): Unvectorized loop.
    60: vec( 108): Unvectorizable loop structure.
    73: inl(1212): Source for routine not found.: MICRO_MG_CAM::KR_EXTERNS_IN_MICRO_MG_CAM
    74: inl(1212): Source for routine not found.: MICRO_MG2_0::KR_EXTERNS_IN_MICRO_MG2_0
    75: inl(1212): Source for routine not found.: MICRO_MG_UTILS::KR_EXTERNS_IN_MICRO_MG_UTILS
    76: inl(1212): Source for routine not found.: WV_SAT_METHODS::KR_EXTERNS_IN_WV_SAT_METHODS
    79: inl(1212): Source for routine not found.: MICRO_MG_CAM::MICRO_MG_CAM_TEND_PACK


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:51:28 2018
FILE NAME: kernel_driver.F90

PROCEDURE NAME: MAIN_
FORMAT LIST

 LINE   LOOP      STATEMENT

     6:               PROGRAM kernel_driver
     7:                   USE kgen_utils_mod, ONLY: kgen_get_newunit, kgen_error_stop, kgen_dp, kgen_array_sumcheck, kgen_rankthreadinvoke
     8:                   USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt
     9:                   USE micro_mg_cam, ONLY: micro_mg_cam_tend_pack
    10:            
    11:                   USE micro_mg_cam, ONLY: kr_externs_in_micro_mg_cam
    12:                   USE shr_kind_mod, ONLY: r8 => shr_kind_r8
    13:                   USE micro_mg2_0, ONLY: kr_externs_in_micro_mg2_0
    14:                   USE micro_mg_utils, ONLY: kr_externs_in_micro_mg_utils
    15:                   USE wv_sat_methods, ONLY: kr_externs_in_wv_sat_methods
    16:                   IMPLICIT NONE
    17:            
    18:                   LOGICAL :: kgen_isverified
    19:                   INTEGER :: kgen_ierr_list, kgen_unit_list
    20:                   INTEGER :: kgen_ierr, kgen_unit, kgen_case_count, kgen_count_verified
    21:                   CHARACTER(LEN=1024) :: kgen_filepath
    22:                   REAL(KIND=kgen_dp) :: kgen_measure, kgen_total_time, kgen_min_time, kgen_max_time
    23:                   REAL(KIND=8) :: kgen_array_sum
    24:                   INTEGER :: kgen_mpirank, kgen_openmptid, kgen_kernelinvoke
    25:                   LOGICAL :: kgen_evalstage, kgen_warmupstage, kgen_mainstage
    26:                   COMMON / state / kgen_mpirank, kgen_openmptid, kgen_kernelinvoke, kgen_evalstage, kgen_warmupstage, kgen_mainstage
    27:            
    28:                   REAL(KIND=r8) :: dtime
    29:                   INTEGER :: nlev
    30:                   INTEGER :: mgncol
    31:                   kgen_total_time = 0.0_kgen_dp
    32:                   kgen_min_time = HUGE(0.0_kgen_dp)
    33:                   kgen_max_time = 0.0_kgen_dp
    34:                   kgen_case_count = 0
    35:                   kgen_count_verified = 0
    36:            
    37:                   kgen_unit_list = kgen_get_newunit()
    38:                   !OPEN (UNIT=kgen_unit_list, FILE="../data/pcols" // PCOLSSIZE // "/kgen_statefile.lst", STATUS="OLD", IOSTAT=kgen_ierr_list)
    39:                   OPEN (UNIT=kgen_unit_list, FILE=STATEFILE, STATUS="OLD", IOSTAT=kgen_ierr_list)
    40:                   IF (kgen_ierr_list .NE. 0) THEN
    41:           !            CALL SYSTEM("ls -1 micro_mg_tend2_0.*.*.* > kgen_statefile.lst")
    42:           !            CALL SLEEP(1)
    43:                       kgen_unit_list = kgen_get_newunit()
    44:                       OPEN (UNIT=kgen_unit_list, FILE="kgen_statefile.lst", STATUS="OLD", IOSTAT=kgen_ierr_list)
    45:                   END IF
    46:                   IF (kgen_ierr_list .NE. 0) THEN
    47:                       WRITE (*, *) ""
    48:                       WRITE (*, *) "ERROR: ""kgen_statefile.lst"" is not found in current directory."
    49:                       STOP
    50:                   END IF
    51: +------>          DO WHILE ( kgen_ierr_list .EQ. 0 )
    52: |                     READ (UNIT = kgen_unit_list, FMT="(A)", IOSTAT=kgen_ierr_list) kgen_filepath
    53: |                     IF (kgen_ierr_list .EQ. 0) THEN
    54: |                         kgen_unit = kgen_get_newunit()
    55: |                         CALL kgen_rankthreadinvoke(TRIM(ADJUSTL(kgen_filepath)), kgen_mpirank, kgen_openmptid, kgen_kernelinvoke)
    56: |                         OPEN (UNIT=kgen_unit, FILE=TRIM(ADJUSTL(kgen_filepath)), STATUS="OLD", ACCESS="STREAM", FORM="UNFORMATTED", &
    57: |                         &ACTION="READ", CONVERT="BIG_ENDIAN", IOSTAT=kgen_ierr)
    58: |                         IF (kgen_ierr == 0) THEN
    59: |                             WRITE (*, *) ""
    60: |                             WRITE (*, *) "***************** Verification against '" // trim(adjustl(kgen_filepath)) // "' &
    61: |                             &*****************"
    62: |                             kgen_evalstage = .TRUE.
    63: |                             kgen_warmupstage = .FALSE.
    64: |                             kgen_mainstage = .FALSE.
    65: |          
    66: |          
    67: |                             !driver read in arguments
    68: |                             READ (UNIT = kgen_unit) dtime
    69: |                             READ (UNIT = kgen_unit) nlev
    70: |                             READ (UNIT = kgen_unit) mgncol
    71: |          
    72: |                             !extern input variables
    73: |                             CALL kr_externs_in_micro_mg_cam(kgen_unit)
    74: |                             CALL kr_externs_in_micro_mg2_0(kgen_unit)
    75: |                             CALL kr_externs_in_micro_mg_utils(kgen_unit)
    76: |                             CALL kr_externs_in_wv_sat_methods(kgen_unit)
    77: |          
    78: |                             !callsite part
    79: |                             CALL micro_mg_cam_tend_pack(kgen_unit, kgen_measure, kgen_isverified, dtime, nlev, mgncol)
    80: |                             REWIND (UNIT=kgen_unit)
    81: |                             kgen_evalstage = .FALSE.
    82: |                             kgen_warmupstage = .TRUE.
    83: |                             kgen_mainstage = .FALSE.
    84: |          
    85: |          
    86: |                             !driver read in arguments
    87: |                             READ (UNIT = kgen_unit) dtime
    88: |                             READ (UNIT = kgen_unit) nlev
    89: |                             READ (UNIT = kgen_unit) mgncol
    90: |          
    91: |                             !extern input variables
    92: |                             CALL kr_externs_in_micro_mg_cam(kgen_unit)
    93: |                             CALL kr_externs_in_micro_mg2_0(kgen_unit)
    94: |                             CALL kr_externs_in_micro_mg_utils(kgen_unit)
    95: |                             CALL kr_externs_in_wv_sat_methods(kgen_unit)
    96: |          
    97: |                             !callsite part
    98: |                             CALL micro_mg_cam_tend_pack(kgen_unit, kgen_measure, kgen_isverified, dtime, nlev, mgncol)
    99: |                             REWIND (UNIT=kgen_unit)
   100: |                             kgen_evalstage = .FALSE.
   101: |                             kgen_warmupstage = .FALSE.
   102: |                             kgen_mainstage = .TRUE.
   103: |                             kgen_case_count = kgen_case_count + 1
   104: |                             kgen_isverified = .FALSE.
   105: |          
   106: |          
   107: |                             !driver read in arguments
   108: |                             READ (UNIT = kgen_unit) dtime
   109: |                             READ (UNIT = kgen_unit) nlev
   110: |                             READ (UNIT = kgen_unit) mgncol
   111: |          
   112: |                             !extern input variables
   113: |                             CALL kr_externs_in_micro_mg_cam(kgen_unit)
   114: |                             CALL kr_externs_in_micro_mg2_0(kgen_unit)
   115: |                             CALL kr_externs_in_micro_mg_utils(kgen_unit)
   116: |                             CALL kr_externs_in_wv_sat_methods(kgen_unit)
   117: |          
   118: |                             !callsite part
   119: |                             CALL micro_mg_cam_tend_pack(kgen_unit, kgen_measure, kgen_isverified, dtime, nlev, mgncol)
   120: |                             kgen_total_time = kgen_total_time + kgen_measure
   121: |                             kgen_min_time = MIN( kgen_min_time, kgen_measure )
   122: |                             kgen_max_time = MAX( kgen_max_time, kgen_measure )
   123: |                             IF (kgen_isverified) THEN
   124: |                                 kgen_count_verified = kgen_count_verified + 1
   125: |                             END IF
   126: |                         END IF
   127: |                         CLOSE (UNIT=kgen_unit)
   128: |                     END IF
   129: +------           END DO
   130:            
   131:                   CLOSE (UNIT=kgen_unit_list)
   132:            
   133:                   WRITE (*, *) ""
   134:                   WRITE (*, "(A)") "****************************************************"
   135:                   WRITE (*, "(4X,A)") "kernel execution summary: micro_mg_tend2_0"
   136:                   WRITE (*, "(A)") "****************************************************"
   137:                   IF (kgen_case_count == 0) THEN
   138:                       WRITE (*, *) "No data file is verified."
   139:                   ELSE
   140:                       WRITE (*, "(4X, A36, A1, I6)") "Total number of verification cases   ", ":", kgen_case_count
   141:                       WRITE (*, "(4X, A36, A1, I6)") "Number of verification-passed cases ", ":", kgen_count_verified
   142:                       WRITE (*, *) ""
   143:                       WRITE (*, "(4X, A, E12.4)") "Average call time (usec): ", kgen_total_time / DBLE(kgen_case_count)
   144:                       WRITE (*, "(4X, A, E12.4)") "Minimum call time (usec): ", kgen_min_time
   145:                       WRITE (*, "(4X, A, E12.4)") "Maximum call time (usec): ", kgen_max_time
   146:                   END IF
   147:                   WRITE (*, "(A)") "****************************************************"
   148:               END PROGRAM


