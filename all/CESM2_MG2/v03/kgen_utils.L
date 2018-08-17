NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_ARRAY_SUMCHECK
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_ARRAY_SUMCHECK
FORMAT LIST

 LINE   LOOP      STATEMENT

    43:           subroutine kgen_array_sumcheck(varname, sum1, sum2, finish)
    44:               character(*), intent(in) :: varname
    45:               real(kind=8), intent(in) :: sum1, sum2
    46:               real(kind=8), parameter  :: max_rel_diff = 1.E-10
    47:               real(kind=8)  :: diff, rel_diff
    48:               logical, intent(in), optional :: finish
    49:               logical checkresult
    50:           
    51:               if ( sum1 == sum2 ) then
    52:                   checkresult = .TRUE.
    53:               else
    54:                   checkresult = .FALSE.
    55:           
    56:                   diff = ABS(sum2 - sum1)
    57:           
    58:                   if ( .NOT. (sum1 == 0._8) ) then
    59:           
    60:                       rel_diff = ABS(diff / sum1)
    61:                       if ( rel_diff > max_rel_diff ) then
    62:           
    63:                           print *, ''
    64:                           print *, 'SUM of array, "', varname, '", is different.'
    65:                           print *, 'From file : ', sum1
    66:                           print *, 'From array: ', sum2
    67:                           print *, 'Difference: ', diff
    68:                           print *, 'Normalized difference: ', rel_diff
    69:           
    70:                           if ( present(finish) .AND. finish ) then
    71:                               stop
    72:                           end if
    73:                       end if
    74:                   else
    75:                       print *, ''
    76:                       print *, 'SUM of array, "', varname, '", is different.'
    77:                       print *, 'From file : ', sum1
    78:                       print *, 'From array: ', sum2
    79:                       print *, 'Difference: ', diff
    80:           
    81:                       if ( present(finish) .AND. finish ) then
    82:                           stop
    83:                       end if
    84:                   end if
    85:               end if
    86:           end subroutine


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS1
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

    94: vec( 118): Unvectorizable data type.
    94: vec( 103): Unvectorized loop.
    94: vec( 108): Unvectorizable loop structure.
    94: vec( 103): Unvectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS1
FORMAT LIST

 LINE   LOOP      STATEMENT

    88:           function kgen_tostr_args1(idx1) result(tostr)
    89:               integer, intent(in) :: idx1
    90:               character(len=64) :: str_idx1
    91:               character(len=64) :: tostr
    92:           
    93:               write(str_idx1, *) idx1
    94: +======>      tostr = trim(adjustl(str_idx1))
    95:           end function


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS2
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   104: vec( 118): Unvectorizable data type.
   104: vec( 103): Unvectorized loop.
   104: vec( 108): Unvectorizable loop structure.
   104: vec( 103): Unvectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS2
FORMAT LIST

 LINE   LOOP      STATEMENT

    97:           function kgen_tostr_args2(idx1, idx2) result(tostr)
    98:               integer, intent(in) :: idx1, idx2
    99:               character(len=64) :: str_idx1, str_idx2
   100:               character(len=128) :: tostr
   101:           
   102:               write(str_idx1, *) idx1
   103:               write(str_idx2, *) idx2
   104: +======>      tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2))
   105:           end function


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS3
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   115: vec( 118): Unvectorizable data type.
   115: vec( 103): Unvectorized loop.
   115: vec( 108): Unvectorizable loop structure.
   115: vec( 103): Unvectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS3
FORMAT LIST

 LINE   LOOP      STATEMENT

   107:           function kgen_tostr_args3(idx1, idx2, idx3) result(tostr)
   108:               integer, intent(in) :: idx1, idx2, idx3
   109:               character(len=64) :: str_idx1, str_idx2, str_idx3
   110:               character(len=192) :: tostr
   111:           
   112:               write(str_idx1, *) idx1
   113:               write(str_idx2, *) idx2
   114:               write(str_idx3, *) idx3
   115: +======>      tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
   116:                   // ", " // trim(adjustl(str_idx3))
   117:           end function


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS4
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   128: vec( 118): Unvectorizable data type.
   128: vec( 103): Unvectorized loop.
   128: vec( 108): Unvectorizable loop structure.
   128: vec( 103): Unvectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:02 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS4
FORMAT LIST

 LINE   LOOP      STATEMENT

   119:           function kgen_tostr_args4(idx1, idx2, idx3, idx4) result(tostr)
   120:               integer, intent(in) :: idx1, idx2, idx3, idx4
   121:               character(len=64) :: str_idx1, str_idx2, str_idx3, str_idx4
   122:               character(len=256) :: tostr
   123:           
   124:               write(str_idx1, *) idx1
   125:               write(str_idx2, *) idx2
   126:               write(str_idx3, *) idx3
   127:               write(str_idx4, *) idx4
   128: +======>      tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
   129:                   // ", " // trim(adjustl(str_idx3)) // ", " // trim(adjustl(str_idx4))
   130:           end function


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS5
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   142: vec( 118): Unvectorizable data type.
   142: vec( 103): Unvectorized loop.
   142: vec( 108): Unvectorizable loop structure.
   142: vec( 103): Unvectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS5
FORMAT LIST

 LINE   LOOP      STATEMENT

   132:           function kgen_tostr_args5(idx1, idx2, idx3, idx4, idx5) result(tostr)
   133:               integer, intent(in) :: idx1, idx2, idx3, idx4, idx5
   134:               character(len=64) :: str_idx1, str_idx2, str_idx3, str_idx4, str_idx5
   135:               character(len=320) :: tostr
   136:           
   137:               write(str_idx1, *) idx1
   138:               write(str_idx2, *) idx2
   139:               write(str_idx3, *) idx3
   140:               write(str_idx4, *) idx4
   141:               write(str_idx5, *) idx5
   142: +======>      tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
   143:                   // ", " // trim(adjustl(str_idx3)) // ", " // trim(adjustl(str_idx4)) &
   144:                   // ", " // trim(adjustl(str_idx5))
   145:           end function


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS6
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   158: vec( 118): Unvectorizable data type.
   158: vec( 103): Unvectorized loop.
   158: vec( 108): Unvectorizable loop structure.
   158: vec( 103): Unvectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_TOSTR_ARGS6
FORMAT LIST

 LINE   LOOP      STATEMENT

   147:           function kgen_tostr_args6(idx1, idx2, idx3, idx4, idx5, idx6) result(tostr)
   148:               integer, intent(in) :: idx1, idx2, idx3, idx4, idx5, idx6
   149:               character(len=64) :: str_idx1, str_idx2, str_idx3, str_idx4, str_idx5, str_idx6
   150:               character(len=384) :: tostr
   151:           
   152:               write(str_idx1, *) idx1
   153:               write(str_idx2, *) idx2
   154:               write(str_idx3, *) idx3
   155:               write(str_idx4, *) idx4
   156:               write(str_idx5, *) idx5
   157:               write(str_idx6, *) idx6
   158: +======>      tostr = trim(adjustl(str_idx1)) // ", " // trim(adjustl(str_idx2)) &
   159:                   // ", " // trim(adjustl(str_idx3)) // ", " // trim(adjustl(str_idx4)) &
   160:                   // ", " // trim(adjustl(str_idx5)) // ", " // trim(adjustl(str_idx6))
   161:           end function


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL4_DIM1
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   173: vec( 101): Vectorized loop.
   175: vec( 110): Vectorization obstructive procedure reference.: RANDOM_NUMBER
   175: vec( 103): Unvectorized loop.
   176: opt(1025): Reference to this procedure inhibits optimization.: RANDOM_NUMBER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL4_DIM1
FORMAT LIST

 LINE   LOOP      STATEMENT

   163:           subroutine kgen_perturb_real4_dim1(var, pertlim)
   164:               real*4, intent(inout), dimension(:) :: var
   165:               real*4, intent(in) :: pertlim
   166:               integer, allocatable :: rndm_seed(:)
   167:               integer :: rndm_seed_sz
   168:               real*4 :: pertval
   169:               integer :: idx1
   170:           
   171:               call random_seed(size=rndm_seed_sz)
   172:               allocate(rndm_seed(rndm_seed_sz))
   173: V======>      rndm_seed = 121869
   174:               call random_seed(put=rndm_seed)
   175: +------>      do idx1=1,size(var, dim=1)
   176: |                 call random_number(pertval)
   177: |                 pertval = 2.0_4*pertlim*(0.5_4 - pertval)
   178: |                 var(idx1) = var(idx1)*(1.0_4 + pertval)
   179: +------       end do
   180:               deallocate(rndm_seed)
   181:           end subroutine


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL4_DIM2
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   193: vec( 101): Vectorized loop.
   196: vec( 110): Vectorization obstructive procedure reference.: RANDOM_NUMBER
   196: vec( 103): Unvectorized loop.
   197: opt(1025): Reference to this procedure inhibits optimization.: RANDOM_NUMBER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL4_DIM2
FORMAT LIST

 LINE   LOOP      STATEMENT

   183:           subroutine kgen_perturb_real4_dim2(var, pertlim)
   184:               real*4, intent(inout), dimension(:,:) :: var
   185:               real*4, intent(in) :: pertlim
   186:               integer, allocatable :: rndm_seed(:)
   187:               integer :: rndm_seed_sz
   188:               real*4 :: pertval
   189:               integer :: idx1,idx2
   190:           
   191:               call random_seed(size=rndm_seed_sz)
   192:               allocate(rndm_seed(rndm_seed_sz))
   193: V======>      rndm_seed = 121869
   194:               call random_seed(put=rndm_seed)
   195: +------>      do idx1=1,size(var, dim=1)
   196: |+----->          do idx2=1,size(var, dim=2)
   197: ||                    call random_number(pertval)
   198: ||                    pertval = 2.0_4*pertlim*(0.5_4 - pertval)
   199: ||                    var(idx1,idx2) = var(idx1,idx2)*(1.0_4 + pertval)
   200: |+-----           end do
   201: +------       end do
   202:               deallocate(rndm_seed)
   203:           end subroutine


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL4_DIM3
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   215: vec( 101): Vectorized loop.
   219: vec( 110): Vectorization obstructive procedure reference.: RANDOM_NUMBER
   219: vec( 103): Unvectorized loop.
   220: opt(1025): Reference to this procedure inhibits optimization.: RANDOM_NUMBER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL4_DIM3
FORMAT LIST

 LINE   LOOP      STATEMENT

   205:           subroutine kgen_perturb_real4_dim3(var, pertlim)
   206:               real*4, intent(inout), dimension(:,:,:) :: var
   207:               real*4, intent(in) :: pertlim
   208:               integer, allocatable :: rndm_seed(:)
   209:               integer :: rndm_seed_sz
   210:               real*4 :: pertval
   211:               integer :: idx1,idx2,idx3
   212:           
   213:               call random_seed(size=rndm_seed_sz)
   214:               allocate(rndm_seed(rndm_seed_sz))
   215: V======>      rndm_seed = 121869
   216:               call random_seed(put=rndm_seed)
   217: +------>      do idx1=1,size(var, dim=1)
   218: |+----->          do idx2=1,size(var, dim=2)
   219: ||+---->              do idx3=1,size(var, dim=3)
   220: |||                       call random_number(pertval)
   221: |||                       pertval = 2.0_4*pertlim*(0.5_4 - pertval)
   222: |||                       var(idx1,idx2,idx3) = var(idx1,idx2,idx3)*(1.0_4 + pertval)
   223: ||+----               end do
   224: |+-----           end do
   225: +------       end do
   226:               deallocate(rndm_seed)
   227:           end subroutine


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL8_DIM1
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   239: vec( 101): Vectorized loop.
   241: vec( 110): Vectorization obstructive procedure reference.: RANDOM_NUMBER
   241: vec( 103): Unvectorized loop.
   242: opt(1025): Reference to this procedure inhibits optimization.: RANDOM_NUMBER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL8_DIM1
FORMAT LIST

 LINE   LOOP      STATEMENT

   229:           subroutine kgen_perturb_real8_dim1(var, pertlim)
   230:               real*8, intent(inout), dimension(:) :: var
   231:               real*8, intent(in) :: pertlim
   232:               integer, allocatable :: rndm_seed(:)
   233:               integer :: rndm_seed_sz
   234:               real*8 :: pertval
   235:               integer :: idx1
   236:           
   237:               call random_seed(size=rndm_seed_sz)
   238:               allocate(rndm_seed(rndm_seed_sz))
   239: V======>      rndm_seed = 121869
   240:               call random_seed(put=rndm_seed)
   241: +------>      do idx1=1,size(var, dim=1)
   242: |                 call random_number(pertval)
   243: |                 pertval = 2.0_8*pertlim*(0.5_8 - pertval)
   244: |                 var(idx1) = var(idx1)*(1.0_8 + pertval)
   245: +------       end do
   246:               deallocate(rndm_seed)
   247:           end subroutine


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL8_DIM2
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   259: vec( 101): Vectorized loop.
   262: vec( 110): Vectorization obstructive procedure reference.: RANDOM_NUMBER
   262: vec( 103): Unvectorized loop.
   263: opt(1025): Reference to this procedure inhibits optimization.: RANDOM_NUMBER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL8_DIM2
FORMAT LIST

 LINE   LOOP      STATEMENT

   249:           subroutine kgen_perturb_real8_dim2(var, pertlim)
   250:               real*8, intent(inout), dimension(:,:) :: var
   251:               real*8, intent(in) :: pertlim
   252:               integer, allocatable :: rndm_seed(:)
   253:               integer :: rndm_seed_sz
   254:               real*8 :: pertval
   255:               integer :: idx1,idx2
   256:           
   257:               call random_seed(size=rndm_seed_sz)
   258:               allocate(rndm_seed(rndm_seed_sz))
   259: V======>      rndm_seed = 121869
   260:               call random_seed(put=rndm_seed)
   261: +------>      do idx1=1,size(var, dim=1)
   262: |+----->          do idx2=1,size(var, dim=2)
   263: ||                    call random_number(pertval)
   264: ||                    pertval = 2.0_8*pertlim*(0.5_8 - pertval)
   265: ||                    var(idx1,idx2) = var(idx1,idx2)*(1.0_8 + pertval)
   266: |+-----           end do
   267: +------       end do
   268:               deallocate(rndm_seed)
   269:           end subroutine


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL8_DIM3
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   281: vec( 101): Vectorized loop.
   285: vec( 110): Vectorization obstructive procedure reference.: RANDOM_NUMBER
   285: vec( 103): Unvectorized loop.
   286: opt(1025): Reference to this procedure inhibits optimization.: RANDOM_NUMBER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PERTURB_REAL8_DIM3
FORMAT LIST

 LINE   LOOP      STATEMENT

   271:           subroutine kgen_perturb_real8_dim3(var, pertlim)
   272:               real*8, intent(inout), dimension(:,:,:) :: var
   273:               real*8, intent(in) :: pertlim
   274:               integer, allocatable :: rndm_seed(:)
   275:               integer :: rndm_seed_sz
   276:               real*8 :: pertval
   277:               integer :: idx1,idx2,idx3
   278:           
   279:               call random_seed(size=rndm_seed_sz)
   280:               allocate(rndm_seed(rndm_seed_sz))
   281: V======>      rndm_seed = 121869
   282:               call random_seed(put=rndm_seed)
   283: +------>      do idx1=1,size(var, dim=1)
   284: |+----->          do idx2=1,size(var, dim=2)
   285: ||+---->              do idx3=1,size(var, dim=3)
   286: |||                       call random_number(pertval)
   287: |||                       pertval = 2.0_8*pertlim*(0.5_8 - pertval)
   288: |||                       var(idx1,idx2,idx3) = var(idx1,idx2,idx3)*(1.0_8 + pertval)
   289: ||+----               end do
   290: |+-----           end do
   291: +------       end do
   292:               deallocate(rndm_seed)
   293:           end subroutine


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_INIT_CHECK
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_INIT_CHECK
FORMAT LIST

 LINE   LOOP      STATEMENT

   295:           subroutine kgen_init_check(check, verboseLevel, tolerance, minValue)
   296:             type(check_t), intent(inout) :: check
   297:             integer, intent(in), optional :: verboseLevel
   298:             real(kind=kgen_dp), intent(in), optional :: tolerance
   299:             real(kind=kgen_dp), intent(in), optional :: minValue
   300:           
   301:             check%Passed   = .TRUE.
   302:             check%numOutTol = 0
   303:             check%numInTol = 0
   304:             check%numTotal = 0
   305:             check%numIdentical = 0
   306:           
   307:             if(present(verboseLevel)) then
   308:                check%verboseLevel = verboseLevel
   309:             else
   310:                 check%verboseLevel = 1
   311:             end if
   312:             if(present(tolerance)) then
   313:                 kgen_tolerance = tolerance
   314:             else
   315:                 kgen_tolerance = 1.0D-15
   316:             end if
   317:             if(present(minvalue)) then
   318:                 kgen_minvalue = minvalue
   319:             else
   320:                 kgen_minvalue = 1.0D-15
   321:             end if
   322:           end subroutine kgen_init_check


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PRINT_CHECK
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   328: vec( 103): Unvectorized loop.
   328: vec( 108): Unvectorizable loop structure.
   330: vec( 103): Unvectorized loop.
   330: vec( 108): Unvectorizable loop structure.
   331: vec( 103): Unvectorized loop.
   331: vec( 108): Unvectorizable loop structure.
   332: vec( 103): Unvectorized loop.
   332: vec( 108): Unvectorizable loop structure.
   333: vec( 103): Unvectorized loop.
   333: vec( 108): Unvectorizable loop structure.
   336: vec( 103): Unvectorized loop.
   336: vec( 108): Unvectorizable loop structure.
   338: vec( 103): Unvectorized loop.
   338: vec( 108): Unvectorizable loop structure.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_PRINT_CHECK
FORMAT LIST

 LINE   LOOP      STATEMENT

   324:           subroutine kgen_print_check(kname, check)
   325:              character(len=*) :: kname
   326:              type(check_t), intent(in) ::  check
   327:           
   328:              write (*,*) TRIM(kname),': Tolerance for normalized RMS: ',kgen_tolerance
   329:              !write (*,*) TRIM(kname),':',check%numFatal,'fatal errors,',check%numWarning,'warnings detected, and',check%numIdentical,'identical out of',check%numTotal,'variables checked'
   330:              write (*,*) TRIM(kname),': Number of variables checked: ',check%numTotal
   331:              write (*,*) TRIM(kname),': Number of Identical results: ',check%numIdentical
   332:              write (*,*) TRIM(kname),': Number of variables within tolerance(not identical): ',check%numInTol
   333:              write (*,*) TRIM(kname),': Number of variables out of tolerance: ', check%numOutTol
   334:           
   335:              if (check%numOutTol> 0) then
   336:                   write(*,*) TRIM(kname),': Verification FAILED'
   337:              else
   338:                   write(*,*) TRIM(kname),': Verification PASSED'
   339:              endif
   340:           end subroutine kgen_print_check


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_GET_NEWUNIT
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   350: vec( 103): Unvectorized loop.
   350: vec( 180): I/O statement obstructs vectorization.
   350: vec( 110): Vectorization obstructive procedure reference.: RANDOM_NUMBER
   351: opt(1025): Reference to this procedure inhibits optimization.: RANDOM_NUMBER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_GET_NEWUNIT
FORMAT LIST

 LINE   LOOP      STATEMENT

   342:           FUNCTION kgen_get_newunit() RESULT ( new_unit )
   343:               INTEGER, PARAMETER :: UNIT_MIN=100, UNIT_MAX=1000000
   344:               LOGICAL :: is_opened
   345:               INTEGER :: nunit, new_unit, counter
   346:               REAL :: r
   347:           
   348:               CALL RANDOM_SEED
   349:               new_unit = -1
   350: +------>      DO counter=1, UNIT_MAX
   351: |                 CALL RANDOM_NUMBER(r)
   352: |                 nunit = INT(r*UNIT_MAX+UNIT_MIN)
   353: |                 INQUIRE (UNIT=nunit, OPENED=is_opened)
   354: |                 IF (.NOT. is_opened) THEN
   355: |                     new_unit = nunit
   356: |                     EXIT
   357: |                 END IF
   358: +------       END DO
   359:           END FUNCTION kgen_get_newunit


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_ERROR_STOP
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_ERROR_STOP
FORMAT LIST

 LINE   LOOP      STATEMENT

   361:           SUBROUTINE kgen_error_stop( msg )
   362:               IMPLICIT NONE
   363:               CHARACTER(LEN=*), INTENT(IN) :: msg
   364:           
   365:               WRITE (*,*) msg
   366:               STOP 1
   367:           END SUBROUTINE


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_RANKTHREADINVOKE
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   380: vec( 103): Unvectorized loop.
   380: vec( 108): Unvectorizable loop structure.
   380: vec( 180): I/O statement obstructs vectorization.
   380: vec( 110): Vectorization obstructive procedure reference.: INDEX


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:03 2018
FILE NAME: kgen_utils.f90

PROCEDURE NAME: KGEN_UTILS_MOD::KGEN_RANKTHREADINVOKE
FORMAT LIST

 LINE   LOOP      STATEMENT

   369:           SUBROUTINE kgen_rankthreadinvoke( str, rank, thread, invoke )
   370:               CHARACTER(*), INTENT(IN) :: str
   371:               INTEGER, INTENT(OUT) :: rank, thread, invoke
   372:               INTEGER :: pos1, pos2, i, e
   373:           
   374:               pos1 = 1
   375:           
   376:               rank = -1
   377:               thread = -1
   378:               invoke = -1
   379:           
   380: +------>      DO
   381: |                 pos2 = INDEX(str(pos1:), ".")
   382: |                 IF (pos2 == 0) THEN
   383: |                     READ(str(pos1:),*,IOSTAT=e) i
   384: |                     IF ( e == 0 ) THEN
   385: |                         rank = thread
   386: |                         thread = invoke
   387: |                         READ(str(pos1:), *) invoke
   388: |                     END IF
   389: |                     EXIT
   390: |                 END IF
   391: |         
   392: |                 READ(str(pos1:pos1+pos2-2),*,IOSTAT=e) i
   393: |                 IF ( e == 0 ) THEN
   394: |                     rank = thread
   395: |                     thread = invoke
   396: |                     READ(str(pos1:pos1+pos2-2), *) invoke
   397: |                 END IF
   398: |         
   399: |                 pos1 = pos2+pos1
   400: +------       END DO
   401:           END SUBROUTINE


