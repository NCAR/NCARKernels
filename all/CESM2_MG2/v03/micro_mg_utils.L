NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::RISING_FACTORIAL_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::RISING_FACTORIAL_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   247:           subroutine  rising_factorial_r8(x, n, res)
   248:             real(r8), intent(in)  :: x, n
   249:             real(r8), intent(out) :: res
   250:           
   251:             res = gamma(x+n)/gamma(x)
   252:           
   253:           end subroutine rising_factorial_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::RISING_FACTORIAL_V8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   268: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::RISING_FACTORIAL_V8
FORMAT LIST

 LINE   LOOP      STATEMENT

   255:           subroutine  rising_factorial_v8(x, n, res,vlen)
   256:             integer, intent(in)   :: vlen
   257:             real(r8), intent(in)  :: x(vlen), n
   258:             real(r8), intent(out) :: res(vlen)
   259:             integer :: i
   260:             real(r8) :: tmp(vlen)
   261:           
   262:           #if 0
   263:             do i=1,vlen
   264:               tmp(i) = x(i)+n
   265:               res(i) = gamma(tmp(i))/gamma(x(i))
   266:             enddo
   267:           #else
   268: V======>    tmp = x+n
   269: +======>    res = gamma(tmp)
   270: +======>    tmp = gamma(x)
   271: +======>    res = res/tmp
   272:           #endif
   273:           
   274:           end subroutine rising_factorial_v8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::RISING_FACTORIAL_INT
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   288: opt(1418): Constant-length loop is expanded.
   288: vec( 101): Vectorized loop.
   289: vec( 126): Idiom detected.: PRODUCT


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::RISING_FACTORIAL_INT
FORMAT LIST

 LINE   LOOP      STATEMENT

   277:           subroutine rising_factorial_int(x, n, res)
   278:             real(r8), intent(in) :: x
   279:             integer, intent(in) :: n
   280:             real(r8), intent(out) :: res
   281:           
   282:             integer :: i
   283:             real(r8) :: factor
   284:           
   285:             res = 1._r8
   286:             factor = x
   287:           
   288: *------>    do i = 1, n
   289: |              res = res * factor
   290: |              factor = factor + 1._r8
   291: *------     end do
   292:           
   293:           end subroutine rising_factorial_int


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::RISING_FACTORIAL_VINT
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   304: opt(1592): Outer loop unrolled inside inner loop.: J
   304: vec( 103): Unvectorized loop.
   304: vec( 113): Overhead of loop division is too large.
   308: vec( 101): Vectorized loop.
   309: vec( 126): Idiom detected.: PRODUCT


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::RISING_FACTORIAL_VINT
FORMAT LIST

 LINE   LOOP      STATEMENT

   295:           subroutine rising_factorial_vint(x, n, res,vlen)
   296:             integer, intent(in) :: vlen
   297:             real(r8), intent(in) :: x(vlen)
   298:             integer, intent(in) :: n
   299:             real(r8), intent(out) :: res(vlen)
   300:           
   301:             integer :: i,j
   302:             real(r8) :: factor
   303:           
   304: U------>    do j=1,vlen
   305: |             res(j) = 1._r8
   306: |             factor = x(j)
   307: |         
   308: |V----->      do i = 1, n
   309: ||               res(j) = res(j) * factor
   310: ||               factor = factor + 1._r8
   311: |V-----       end do
   312: U------     enddo
   313:           
   314:           end subroutine rising_factorial_vint


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::CALC_AB
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::CALC_AB
FORMAT LIST

 LINE   LOOP      STATEMENT

   317:           subroutine calc_ab(t, qv, xxl,ab)
   318:             real(r8), intent(in) :: t     ! Temperature
   319:             real(r8), intent(in) :: qv    ! Saturation vapor pressure
   320:             real(r8), intent(in) :: xxl   ! Latent heat
   321:           
   322:             real(r8), intent(out) :: ab
   323:           
   324:             real(r8) :: dqsdt
   325:           
   326:             dqsdt = xxl*qv / (rv * t**2)
   327:             ab = 1._r8 + dqsdt*xxl/cpp
   328:           
   329:           end subroutine  calc_ab


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_LINE
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   359: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_INT
   362: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_R8
   367: opt(1418): Constant-length loop is expanded.
   369: inl(1222): Inlined: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_LINE
FORMAT LIST

 LINE   LOOP      STATEMENT

   332:           subroutine size_dist_param_liq_line(props, qcic, ncic, rho, pgam, lamc)
   333:             type(MGHydrometeorProps), intent(in) :: props
   334:             real(r8), intent(in) :: qcic
   335:             real(r8), intent(inout) :: ncic
   336:             real(r8), intent(in) :: rho
   337:           
   338:             real(r8), intent(out) :: pgam
   339:             real(r8), intent(out) :: lamc
   340:           
   341:             type(MGHydrometeorProps) :: props_loc
   342:             real(r8) :: tmp
   343:           
   344:             if (qcic > qsmall) then
   345:                ! Local copy of properties that can be modified.
   346:                ! (Elemental routines that operate on arrays can't modify scalar
   347:                ! arguments.)
   348:           
   349:                props_loc = props
   350:                ! Get pgam from fit to observations of martin et al. 1994
   351:           
   352:                pgam = 1.0_r8 - 0.7_r8 * exp(-0.008_r8*1.e-6_r8*ncic*rho)
   353:                pgam = 1._r8/(pgam**2) - 1._r8
   354:                pgam = max(pgam, 2._r8)
   355:                ! Set coefficient for use in size_dist_param_basic.
   356:                ! The 3D case is so common and optimizable that we specialize it:
   357:           
   358:                if (props_loc%eff_dim == 3._r8) then
   359:         I         call rising_factorial(pgam+1._r8, 3,tmp)
   360:                   props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
   361:                else
   362:         I         call rising_factorial(pgam+1._r8, props_loc%eff_dim,tmp)
   363:                   props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
   364:                end if
   365:                ! Limit to between 2 and 50 microns mean size.
   366:           
   367: *======>       props_loc%lambda_bounds = (pgam+1._r8)*1._r8/[50.e-6_r8, 2.e-6_r8]
   368:           
   369:         I      call size_dist_param_basic(props_loc, qcic, ncic, lamc)
   370:           
   371:             else
   372:                ! pgam not calculated in this case, so set it to a value likely to
   373:                ! cause an error if it is accidentally used
   374:                ! (gamma function undefined for negative integers)
   375:                pgam = -100._r8
   376:                lamc = 0._r8
   377:             end if
   378:           
   379:           end subroutine size_dist_param_liq_line


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_VECT
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   396: vec( 101): Vectorized loop.
   401: opt(1097): This statement prevents loop optimization.
   408: vec( 103): Unvectorized loop.
   414: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_INT
   415: vec( 120): Unvectorizable dependency.: PROPS_LOC%SHAPE_COEF
   417: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_R8
   418: vec( 120): Unvectorizable dependency.: PROPS_LOC%SHAPE_COEF
   421: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: PROPS_LOC%LAMBDA_BOUNDS
   422: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: PROPS_LOC%LAMBDA_BOUNDS
   423: inl(1222): Inlined: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
   426: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_VECT
FORMAT LIST

 LINE   LOOP      STATEMENT

   383:           subroutine size_dist_param_liq_vect(props, qcic, ncic, rho, pgam, lamc, mgncol)
   384:           
   385:             type(mghydrometeorprops), intent(in) :: props
   386:             integer,                          intent(in) :: mgncol
   387:             real(r8), dimension(mgncol), intent(in) :: qcic
   388:             real(r8), dimension(mgncol), intent(inout) :: ncic
   389:             real(r8), dimension(mgncol), intent(in) :: rho
   390:             real(r8), dimension(mgncol), intent(out) :: pgam
   391:             real(r8), dimension(mgncol), intent(out) :: lamc
   392:             type(mghydrometeorprops) :: props_loc
   393:             integer :: i
   394:             real(r8) :: tmp
   395:           
   396: V------>    do i=1,mgncol
   397: |              if (qcic(i) > qsmall) then
   398: |                 ! Local copy of properties that can be modified.
   399: |                 ! (Elemental routines that operate on arrays can't modify scalar
   400: |                 ! arguments.)
   401: |                 props_loc = props
   402: |                 ! Get pgam from fit to observations of martin et al. 1994
   403: |                 pgam(i) = 1.0_r8 - 0.7_r8 * exp(-0.008_r8*1.e-6_r8*ncic(i)*rho(i))
   404: |                 pgam(i) = 1._r8/(pgam(i)**2) - 1._r8
   405: |                 pgam(i) = max(pgam(i), 2._r8)
   406: |              endif
   407: V------     enddo
   408: +------>    do i=1,mgncol
   409: |              if (qcic(i) > qsmall) then
   410: |                 ! Set coefficient for use in size_dist_param_basic.
   411: |                 ! The 3D case is so common and optimizable that we specialize
   412: |                 ! it:
   413: |                 if (props_loc%eff_dim == 3._r8) then
   414: |       I            call rising_factorial(pgam(i)+1._r8, 3,tmp)
   415: |                    props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
   416: |                 else
   417: |       I            call rising_factorial(pgam(i)+1._r8, props_loc%eff_dim,tmp)
   418: |                    props_loc%shape_coef = pi / 6._r8 * props_loc%rho * tmp
   419: |                 end if
   420: |                 ! Limit to between 2 and 50 microns mean size.
   421: |                 props_loc%lambda_bounds(1) = (pgam(i)+1._r8)*1._r8/50.e-6_r8
   422: |                 props_loc%lambda_bounds(2) = (pgam(i)+1._r8)*1._r8/2.e-6_r8
   423: |       I         call size_dist_param_basic(props_loc, qcic(i), ncic(i), lamc(i))
   424: |              endif
   425: +------     enddo
   426: V------>    do i=1,mgncol
   427: |              if (qcic(i) <= qsmall) then
   428: |                 ! pgam not calculated in this case, so set it to a value likely to
   429: |                 ! cause an error if it is accidentally used
   430: |                 ! (gamma function undefined for negative integers)
   431: |                 pgam(i) = -100._r8
   432: |                 lamc(i) = 0._r8
   433: |              end if
   434: V------     enddo
   435:           
   436:           end subroutine size_dist_param_liq_vect


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   439: opt(1317): Logical clause simplified.
   451: inl(1222): Inlined: MICRO_MG_UTILS::LIMITER_IS_ON
   452: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: NCIC
   456: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: LAMC
   461: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: LAMC
   462: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: NCIC
   464: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: LAMC
   465: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: NCIC
   469: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: LAMC
   472: opt(1317): Logical clause simplified.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
FORMAT LIST

 LINE   LOOP      STATEMENT

   439:           elemental subroutine size_dist_param_basic_line(props, qic, nic, lam, n0)
   440:             type(MGHydrometeorProps), intent(in) :: props
   441:             real(r8), intent(in) :: qic
   442:             real(r8), intent(inout) :: nic
   443:           
   444:             real(r8), intent(out) :: lam
   445:             real(r8), intent(out), optional :: n0
   446:           
   447:             if (qic > qsmall) then
   448:                ! add upper limit to in-cloud number concentration to prevent
   449:                ! numerical error
   450:           
   451:         I      if (limiter_is_on(props%min_mean_mass)) then
   452:                   nic = min(nic, qic / props%min_mean_mass)
   453:                end if
   454:                ! lambda = (c n/q)^(1/d)
   455:           
   456:                lam = (props%shape_coef * nic/qic)**(1._r8/props%eff_dim)
   457:                ! check for slope
   458:                ! adjust vars
   459:           
   460:                if (lam < props%lambda_bounds(1)) then
   461:                   lam = props%lambda_bounds(1)
   462:                   nic = lam**(props%eff_dim) * qic/props%shape_coef
   463:                else if (lam > props%lambda_bounds(2)) then
   464:                   lam = props%lambda_bounds(2)
   465:                   nic = lam**(props%eff_dim) * qic/props%shape_coef
   466:                end if
   467:           
   468:             else
   469:                lam = 0._r8
   470:             end if
   471:           
   472:             if (present(n0)) n0 = nic * lam
   473:           
   474:           end subroutine size_dist_param_basic_line


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_VECT
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   488: vec( 103): Unvectorized loop.
   494: inl(1222): Inlined: MICRO_MG_UTILS::LIMITER_IS_ON
   495: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: NIC
   499: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: LAM
   504: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: LAM
   505: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: NIC
   507: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: LAM
   508: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: NIC
   512: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: LAM
   517: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_VECT
FORMAT LIST

 LINE   LOOP      STATEMENT

   476:           subroutine size_dist_param_basic_vect(props, qic, nic, lam, mgncol, n0)
   477:           
   478:             type (mghydrometeorprops), intent(in) :: props
   479:             integer,                          intent(in) :: mgncol
   480:             real(r8), dimension(mgncol), intent(in) :: qic
   481:             real(r8), dimension(mgncol), intent(inout) :: nic
   482:             real(r8), dimension(mgncol), intent(out) :: lam
   483:             real(r8), dimension(mgncol), intent(out), optional :: n0
   484:             integer :: i
   485:             integer :: cnt
   486:           !  cnt = count(qic > qsmall)
   487:           !  print *,'param_basic: ',cnt,mgncol
   488: +------>    do i=1,mgncol
   489: |         
   490: |              if (qic(i) > qsmall) then
   491: |                 ! add upper limit to in-cloud number concentration to prevent
   492: |                 ! numerical error
   493: |         
   494: |       I         if (limiter_is_on(props%min_mean_mass)) then
   495: |                    nic(i) = min(nic(i), qic(i) / props%min_mean_mass)
   496: |                 end if
   497: |                 ! lambda = (c n/q)^(1/d)
   498: |         
   499: |                 lam(i) = (props%shape_coef * nic(i)/qic(i))**(1._r8/props%eff_dim)
   500: |                 ! check for slope
   501: |                 ! adjust vars
   502: |         
   503: |                 if (lam(i) < props%lambda_bounds(1)) then
   504: |                    lam(i) = props%lambda_bounds(1)
   505: |                    nic(i) = lam(i)**(props%eff_dim) * qic(i)/props%shape_coef
   506: |                 else if (lam(i) > props%lambda_bounds(2)) then
   507: |                    lam(i) = props%lambda_bounds(2)
   508: |                    nic(i) = lam(i)**(props%eff_dim) * qic(i)/props%shape_coef
   509: |                 end if
   510: |         
   511: |              else
   512: |                 lam(i) = 0._r8
   513: |              end if
   514: |         
   515: +------     enddo
   516:           
   517: V======>    if (present(n0)) n0 = nic * lam
   518:           
   519:           end subroutine size_dist_param_basic_vect


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::AVG_DIAMETER
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::AVG_DIAMETER
FORMAT LIST

 LINE   LOOP      STATEMENT

   522:           real(r8) elemental function avg_diameter(q, n, rho_air, rho_sub)
   523:             ! Finds the average diameter of particles given their density, and
   524:             ! mass/number concentrations in the air.
   525:             ! Assumes that diameter follows an exponential distribution.
   526:             real(r8), intent(in) :: q         ! mass mixing ratio
   527:             real(r8), intent(in) :: n         ! number concentration (per volume)
   528:             real(r8), intent(in) :: rho_air   ! local density of the air
   529:             real(r8), intent(in) :: rho_sub   ! density of the particle substance
   530:           
   531:             avg_diameter = (pi * rho_sub * n/(q*rho_air))**(-1._r8/3._r8)
   532:           
   533:           end function avg_diameter


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::VAR_COEF_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   543: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_R8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::VAR_COEF_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   535:           subroutine var_coef_r8(relvar, a, res)
   536:             ! Finds a coefficient for process rates based on the relative variance
   537:             ! of cloud water.
   538:             real(r8), intent(in) :: relvar
   539:             real(r8), intent(in) :: a
   540:             real(r8), intent(out) :: res
   541:             real(r8) :: tmp
   542:           
   543:         I   call rising_factorial(relvar, a,tmp)
   544:             res = tmp / relvar**a
   545:           
   546:           end subroutine var_coef_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::VAR_COEF_V8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   558: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_V8
   559: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::VAR_COEF_V8
FORMAT LIST

 LINE   LOOP      STATEMENT

   548:           subroutine var_coef_v8(relvar, a, res, vlen)
   549:             ! Finds a coefficient for process rates based on the relative variance
   550:             ! of cloud water.
   551:             integer :: vlen
   552:             real(r8), intent(in) :: relvar(vlen)
   553:             real(r8), intent(in) :: a
   554:             real(r8), intent(out) :: res(vlen)
   555:             integer :: i
   556:             real(r8) :: tmpA(vlen)
   557:           
   558:         I    call rising_factorial(relvar,a,tmpA,vlen)
   559: V------>     do i=1,vlen
   560: |               res(i) = tmpA(i)/relvar(i)**a
   561: V------      enddo
   562:           
   563:           end subroutine var_coef_v8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::VAR_COEF_INT
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   573: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_INT


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::VAR_COEF_INT
FORMAT LIST

 LINE   LOOP      STATEMENT

   565:           subroutine var_coef_int(relvar, a, res)
   566:             ! Finds a coefficient for process rates based on the relative variance
   567:             ! of cloud water.
   568:             real(r8), intent(in) :: relvar
   569:             integer, intent(in) :: a
   570:             real(r8), intent(out) :: res
   571:             real(r8) :: tmp
   572:           
   573:         I   call rising_factorial(relvar, a,tmp)
   574:             res = tmp / relvar**a
   575:           
   576:           end subroutine var_coef_int


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::VAR_COEF_VINT
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   586: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_VINT
   587: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::VAR_COEF_VINT
FORMAT LIST

 LINE   LOOP      STATEMENT

   577:           subroutine var_coef_vint(relvar, a, res, vlen)
   578:             ! Finds a coefficient for process rates based on the relative variance
   579:             ! of cloud water.
   580:             integer, intent(in) :: vlen
   581:             real(r8), intent(in) :: relvar(vlen)
   582:             integer, intent(in) :: a
   583:             real(r8), intent(out) :: res(vlen)
   584:             integer :: i
   585:             real(r8) :: tmp(vlen)
   586:         I   call rising_factorial(relvar, a,tmp,vlen)
   587: V------>    do i=1,vlen
   588: |              res(i) = tmp(i) / relvar(i)**a
   589: V------     enddo
   590:           
   591:           end subroutine var_coef_vint


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ICE_DEPOSITION_SUBLIMATION
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   635: vec( 103): Unvectorized loop.
   645: inl(1222): Inlined: MICRO_MG_UTILS::CALC_AB
   647: inl(1222): Inlined: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
   655: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: VAP_DEP
   659: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: ICE_SUBLIM
   662: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: ICE_SUBLIM
   663: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: VAP_DEP
   670: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: BERG
   672: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: BERG
   676: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: BERG
   677: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: VAP_DEP
   678: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: ICE_SUBLIM


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ICE_DEPOSITION_SUBLIMATION
FORMAT LIST

 LINE   LOOP      STATEMENT

   602:           subroutine ice_deposition_sublimation(t, qv, qi, ni, &
   603:                                                 icldm, rho, dv,qvl, qvi, &
   604:                                                 berg, vap_dep, ice_sublim, mgncol)
   605:             !INPUT VARS:
   606:             !===============================================
   607:           
   608:             integer,  intent(in) :: mgncol
   609:             real(r8), dimension(mgncol), intent(in) :: t
   610:             real(r8), dimension(mgncol), intent(in) :: qv
   611:             real(r8), dimension(mgncol), intent(in) :: qi
   612:             real(r8), dimension(mgncol), intent(in) :: ni
   613:             real(r8), dimension(mgncol), intent(in) :: icldm
   614:             real(r8), dimension(mgncol), intent(in) :: rho
   615:             real(r8), dimension(mgncol), intent(in) :: dv
   616:             real(r8), dimension(mgncol), intent(in) :: qvl
   617:             real(r8), dimension(mgncol), intent(in) :: qvi
   618:             !OUTPUT VARS:
   619:             !===============================================
   620:           
   621:             real(r8), dimension(mgncol), intent(out) :: vap_dep !ice deposition (cell-ave value)
   622:             real(r8), dimension(mgncol), intent(out) :: ice_sublim !ice sublimation (cell-ave value)
   623:             real(r8), dimension(mgncol), intent(out) :: berg !bergeron enhancement (cell-ave value)
   624:             !INTERNAL VARS:
   625:             !===============================================
   626:           
   627:             real(r8) :: ab
   628:             real(r8) :: epsi
   629:             real(r8) :: qiic
   630:             real(r8) :: niic
   631:             real(r8) :: lami
   632:             real(r8) :: n0i
   633:             integer :: i
   634:           
   635: +------>    do i=1,mgncol
   636: |              if (qi(i)>=qsmall) then
   637: |                 !GET IN-CLOUD qi, ni
   638: |                 !===============================================
   639: |         
   640: |                 qiic = qi(i)/icldm(i)
   641: |                 niic = ni(i)/icldm(i)
   642: |                 !Compute linearized condensational heating correction
   643: |         
   644: |                 !ab=calc_ab(t(i), qvi(i), xxls)
   645: |       I         call calc_ab(t(i), qvi(i), xxls, ab)
   646: |                 !Get slope and intercept of gamma distn for ice.
   647: |       I         call size_dist_param_basic(mg_ice_props, qiic, niic, lami, n0i)
   648: |                 !Get depletion timescale=1/eps
   649: |                 epsi = 2._r8*pi*n0i*rho(i)*Dv(i)/(lami*lami)
   650: |                 !Compute deposition/sublimation
   651: |         
   652: |                 vap_dep(i) = epsi/ab*(qv(i) - qvi(i))
   653: |                 !Make this a grid-averaged quantity
   654: |         
   655: |                 vap_dep(i)=vap_dep(i)*icldm(i)
   656: |                 !Split into deposition or sublimation.
   657: |         
   658: |                 if (t(i) < tmelt .and. vap_dep(i)>0._r8) then
   659: |                    ice_sublim(i)=0._r8
   660: |                 else
   661: |                 ! make ice_sublim negative for consistency with other evap/sub processes
   662: |                    ice_sublim(i)=min(vap_dep(i),0._r8)
   663: |                    vap_dep(i)=0._r8
   664: |                 end if
   665: |                 !sublimation occurs @ any T. Not so for berg.
   666: |         
   667: |                 if (t(i) < tmelt) then
   668: |                    !Compute bergeron rate assuming cloud for whole step.
   669: |         
   670: |                    berg(i) = max(epsi/ab*(qvl(i) - qvi(i)), 0._r8)
   671: |                 else !T>frz
   672: |                    berg(i)=0._r8
   673: |                 end if !T<frz
   674: |         
   675: |              else !where qi<qsmall
   676: |                 berg(i)=0._r8
   677: |                 vap_dep(i)=0._r8
   678: |                 ice_sublim(i)=0._r8
   679: |              end if !qi>qsmall
   680: +------     enddo
   681:           end subroutine ice_deposition_sublimation


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KK2000_LIQ_AUTOCONVERSION
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   709: vec( 101): Vectorized loop.
   710: inl(1222): Inlined: MICRO_MG_UTILS::VAR_COEF_R8
   713: vec( 101): Vectorized loop.
   715: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KK2000_LIQ_AUTOCONVERSION
FORMAT LIST

 LINE   LOOP      STATEMENT

   688:           subroutine kk2000_liq_autoconversion(microp_uniform, qcic, &
   689:                ncic, rho, relvar, prc, nprc, nprc1, mgncol)
   690:           
   691:             integer, intent(in) :: mgncol
   692:             logical, intent(in) :: microp_uniform
   693:           
   694:             real(r8), dimension(mgncol), intent(in) :: qcic
   695:             real(r8), dimension(mgncol), intent(in) :: ncic
   696:             real(r8), dimension(mgncol), intent(in) :: rho
   697:           
   698:             real(r8), dimension(mgncol), intent(in) :: relvar
   699:           
   700:             real(r8), dimension(mgncol), intent(out) :: prc
   701:             real(r8), dimension(mgncol), intent(out) :: nprc
   702:             real(r8), dimension(mgncol), intent(out) :: nprc1
   703:           
   704:             real(r8), dimension(mgncol) :: prc_coef
   705:             integer :: i
   706:             ! Take variance into account, or use uniform value.
   707:           
   708:             if (.not. microp_uniform) then
   709: V------>       do i=1,mgncol
   710: |       I         call var_coef(relvar(i), 2.47_r8, prc_coef(i))
   711: V------        enddo
   712:             else
   713: V======>       prc_coef = 1._r8
   714:             end if
   715: V------>    do i=1,mgncol
   716: |              if (qcic(i) >= icsmall) then
   717: |                 ! nprc is increase in rain number conc due to autoconversion
   718: |                 ! nprc1 is decrease in cloud droplet conc due to autoconversion
   719: |                 ! assume exponential sub-grid distribution of qc, resulting in additional
   720: |                 ! factor related to qcvar below
   721: |                 ! switch for sub-columns, don't include sub-grid qc
   722: |         
   723: |         
   724: |                 prc(i) = prc_coef(i) * &
   725: |                      0.01_r8 * 1350._r8 * qcic(i)**2.47_r8 * (ncic(i)*1.e-6_r8*rho(i))**(-1.1_r8)
   726: |                 nprc(i) = prc(i) * (1._r8/droplet_mass_25um)
   727: |                 nprc1(i) = prc(i)*ncic(i)/qcic(i)
   728: |         
   729: |              else
   730: |                 prc(i)=0._r8
   731: |                 nprc(i)=0._r8
   732: |                 nprc1(i)=0._r8
   733: |              end if
   734: V------     enddo
   735:           end subroutine kk2000_liq_autoconversion


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SB2001V2_LIQ_AUTOCONVERSION
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   777: vec( 101): Vectorized loop.
   780: inl(1222): Inlined: MICRO_MG_UTILS::VAR_COEF_R8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SB2001V2_LIQ_AUTOCONVERSION
FORMAT LIST

 LINE   LOOP      STATEMENT

   738:           subroutine sb2001v2_liq_autoconversion(pgam,qc,nc,qr,rho,relvar,au,nprc,nprc1,mgncol)
   739:             ! ---------------------------------------------------------------------
   740:             ! AUTO_SB:  calculates the evolution of mass- and number mxg-ratio for
   741:             ! drizzle drops due to autoconversion. The autoconversion rate assumes
   742:             ! f(x)=A*x**(nu_c)*exp(-Bx) in drop MASS x.
   743:             ! Code from Hugh Morrison, Sept 2014
   744:             ! autoconversion
   745:             ! use simple lookup table of dnu values to get mass spectral shape parameter
   746:             ! equivalent to the size spectral shape parameter pgam
   747:             !
   748:           
   749:           
   750:            
   751:             integer, intent(in) :: mgncol
   752:            
   753:             real(r8), dimension(mgncol), intent (in)    :: pgam
   754:             real(r8), dimension(mgncol), intent (in)    :: qc  ! = qc (cld water mixing ratio)
   755:             real(r8), dimension(mgncol), intent (in)    :: nc  ! = nc (cld water number conc /kg)
   756:             real(r8), dimension(mgncol), intent (in)    :: qr  ! = qr (rain water mixing ratio)
   757:             real(r8), dimension(mgncol), intent (in)    :: rho ! = rho : density profile
   758:             real(r8), dimension(mgncol), intent (in)    :: relvar
   759:            
   760:             real(r8), dimension(mgncol), intent (out)   :: au ! = prc autoconversion rate
   761:             real(r8), dimension(mgncol), intent (out)   :: nprc1 ! = number tendency
   762:             real(r8), dimension(mgncol), intent (out)   :: nprc ! = number tendency fixed size for rain
   763:             ! parameters for droplet mass spectral shape,
   764:             !used by Seifert and Beheng (2001)
   765:             ! warm rain scheme only (iparam = 1)
   766:            
   767:             real(r8), parameter :: dnu(16) = [0._r8,-0.557_r8,-0.430_r8,-0.307_r8, &
   768:                -0.186_r8,-0.067_r8,0.050_r8,0.167_r8,0.282_r8,0.397_r8,0.512_r8, &
   769:                0.626_r8,0.739_r8,0.853_r8,0.966_r8,0.966_r8]
   770:             ! parameters for Seifert and Beheng (2001) autoconversion/accretion
   771:           
   772:             real(r8), parameter :: kc = 9.44e9_r8
   773:             real(r8), parameter :: kr = 5.78e3_r8
   774:             real(r8) :: dum, dum1, nu, pra_coef
   775:             integer :: dumi, i
   776:           
   777: V------>    do i=1,mgncol
   778: |         
   779: |             !pra_coef = var_coef(relvar(i), 2.47_r8)
   780: |       I     call  var_coef(relvar(i), 2.47_r8,pra_coef)
   781: |         
   782: |              if (qc(i) > qsmall) then
   783: |                dumi=int(pgam(i))
   784: |       G        nu=dnu(dumi)+(dnu(dumi+1)-dnu(dumi))* &
   785: |                        (pgam(i)-dumi)
   786: |         
   787: |                dum = 1._r8-qc(i)/(qc(i)+qr(i))
   788: |                dum1 = 600._r8*dum**0.68_r8*(1._r8-dum**0.68_r8)**3
   789: |         
   790: |                au(i) = kc/(20._r8*2.6e-7_r8)* &
   791: |                  (nu+2._r8)*(nu+4._r8)/(nu+1._r8)**2._r8* &
   792: |                  (rho(i)*qc(i)/1000._r8)**4._r8/(rho(i)*nc(i)/1.e6_r8)**2._r8* &
   793: |                  (1._r8+dum1/(1._r8-dum)**2)*1000._r8 / rho(i)
   794: |         
   795: |                nprc1(i) = au(i)*2._r8/2.6e-7_r8*1000._r8
   796: |                nprc(i) = au(i)/droplet_mass_40um
   797: |              else
   798: |                au(i) = 0._r8
   799: |                nprc1(i) = 0._r8
   800: |                nprc(i)=0._r8
   801: |              end if
   802: |          
   803: V------     enddo
   804:           
   805:             end subroutine sb2001v2_liq_autoconversion


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SB2001V2_ACCRE_CLD_WATER_RAIN
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   838: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SB2001V2_ACCRE_CLD_WATER_RAIN
FORMAT LIST

 LINE   LOOP      STATEMENT

   810:           subroutine sb2001v2_accre_cld_water_rain(qc,nc,qr,rho,relvar,pra,npra,mgncol)
   811:             ! ---------------------------------------------------------------------
   812:             ! ACCR_SB calculates the evolution of mass mxng-ratio due to accretion
   813:             ! and self collection following Seifert & Beheng (2001).
   814:             !
   815:             !
   816:            
   817:             integer, intent(in) :: mgncol
   818:            
   819:             real(r8), dimension(mgncol), intent (in)    :: qc  ! = qc (cld water mixing ratio)
   820:             real(r8), dimension(mgncol), intent (in)    :: nc  ! = nc (cld water number conc /kg)
   821:             real(r8), dimension(mgncol), intent (in)    :: qr  ! = qr (rain water mixing ratio)
   822:             real(r8), dimension(mgncol), intent (in)    :: rho ! = rho : density profile
   823:             real(r8), dimension(mgncol), intent (in)    :: relvar
   824:             ! Output tendencies
   825:           
   826:             real(r8), dimension(mgncol), intent(out) :: pra  ! MMR
   827:             real(r8), dimension(mgncol), intent(out) :: npra ! Number
   828:             ! parameters for Seifert and Beheng (2001) autoconversion/accretion
   829:           
   830:             real(r8), parameter :: kc = 9.44e9_r8
   831:             real(r8), parameter :: kr = 5.78e3_r8
   832:           
   833:             real(r8) :: dum, dum1
   834:             integer :: i
   835:             ! accretion
   836:           
   837:           
   838: V------>    do i =1,mgncol
   839: |         
   840: |             if (qc(i) > qsmall) then
   841: |               dum = 1._r8-qc(i)/(qc(i)+qr(i))
   842: |               dum1 = (dum/(dum+5.e-4_r8))**4._r8
   843: |               pra(i) = kr*rho(i)*0.001_r8*qc(i)*qr(i)*dum1
   844: |               npra(i) = pra(i)*rho(i)*0.001_r8*(nc(i)*rho(i)*1.e-6_r8)/ &
   845: |                    (qc(i)*rho(i)*0.001_r8)*1.e6_r8 / rho(i)
   846: |             else
   847: |               pra(i) = 0._r8
   848: |               npra(i) = 0._r8
   849: |             end if
   850: |          
   851: V------     enddo
   852:            
   853:             end subroutine sb2001v2_accre_cld_water_rain


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ICE_AUTOCONVERSION
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   880: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ICE_AUTOCONVERSION
FORMAT LIST

 LINE   LOOP      STATEMENT

   859:           subroutine ice_autoconversion(t, qiic, lami, n0i, dcs, prci, nprci, mgncol)
   860:           
   861:             integer, intent(in) :: mgncol
   862:             real(r8), dimension(mgncol), intent(in) :: t
   863:             real(r8), dimension(mgncol), intent(in) :: qiic
   864:             real(r8), dimension(mgncol), intent(in) :: lami
   865:             real(r8), dimension(mgncol), intent(in) :: n0i
   866:             real(r8),                    intent(in) :: dcs
   867:           
   868:             real(r8), dimension(mgncol), intent(out) :: prci
   869:             real(r8), dimension(mgncol), intent(out) :: nprci
   870:             ! Assume autoconversion timescale of 180 seconds.
   871:           
   872:             real(r8), parameter :: ac_time = 180._r8
   873:             ! Average mass of an ice particle.
   874:           
   875:             real(r8) :: m_ip
   876:             ! Ratio of autoconversion diameter to average diameter.
   877:             real(r8) :: d_rat
   878:             integer :: i
   879:           
   880: V------>    do i=1,mgncol
   881: |              if (t(i) <= tmelt .and. qiic(i) >= qsmall) then
   882: |         
   883: |                 d_rat = lami(i)*dcs
   884: |                 ! Rate of ice particle conversion (number).
   885: |         
   886: |                 nprci(i) = n0i(i)/(lami(i)*ac_time)*exp(-d_rat)
   887: |         
   888: |                 m_ip = (rhoi*pi/6._r8) / lami(i)**3
   889: |                 ! Rate of mass conversion.
   890: |                 ! Note that this is:
   891: |                 ! m n (d^3 + 3 d^2 + 6 d + 6)
   892: |         
   893: |                 prci(i) = m_ip * nprci(i) * &
   894: |                      (((d_rat + 3._r8)*d_rat + 6._r8)*d_rat + 6._r8)
   895: |         
   896: |              else
   897: |                 prci(i) = 0._r8
   898: |                 nprci(i) = 0._r8
   899: |              end if
   900: V------     enddo
   901:           end subroutine ice_autoconversion


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::IMMERSION_FREEZING
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   936: vec( 101): Vectorized loop.
   937: inl(1222): Inlined: MICRO_MG_UTILS::VAR_COEF_INT
   940: vec( 101): Vectorized loop.
   942: vec( 101): Vectorized loop.
   945: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_INT
   950: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_INT


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:06 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::IMMERSION_FREEZING
FORMAT LIST

 LINE   LOOP      STATEMENT

   906:           subroutine immersion_freezing(microp_uniform, t, pgam, lamc, &
   907:                qcic, ncic, relvar, mnuccc, nnuccc, mgncol)
   908:           
   909:             integer, intent(in) :: mgncol
   910:             logical, intent(in) :: microp_uniform
   911:             ! Temperature
   912:           
   913:             real(r8), dimension(mgncol), intent(in) :: t
   914:             ! Cloud droplet size distribution parameters
   915:           
   916:             real(r8), dimension(mgncol), intent(in) :: pgam
   917:             real(r8), dimension(mgncol), intent(in) :: lamc
   918:             ! MMR and number concentration of in-cloud liquid water
   919:           
   920:             real(r8), dimension(mgncol), intent(in) :: qcic
   921:             real(r8), dimension(mgncol), intent(in) :: ncic
   922:             ! Relative variance of cloud water
   923:           
   924:             real(r8), dimension(mgncol), intent(in) :: relvar
   925:             ! Output tendencies
   926:           
   927:             real(r8), dimension(mgncol), intent(out) :: mnuccc ! MMR
   928:             real(r8), dimension(mgncol), intent(out) :: nnuccc ! Number
   929:             ! Coefficients that will be omitted for sub-columns
   930:           
   931:             real(r8), dimension(mgncol) :: dum
   932:             integer :: i
   933:             real(r8) :: tmp
   934:           
   935:             if (.not. microp_uniform) then
   936: V------>       do i=1,mgncol
   937: |       I         call  var_coef(relvar(i), 2,dum(i))
   938: V------        enddo
   939:             else
   940: V======>       dum = 1._r8
   941:             end if
   942: V------>    do i=1,mgncol
   943: |              if (qcic(i) >= qsmall .and. t(i) < 269.15_r8) then
   944: |         
   945: |       I         call rising_factorial(pgam(i)+1._r8, 3,tmp)
   946: |                 nnuccc(i) = &
   947: |                      pi/6._r8*ncic(i)*tmp* &
   948: |                      bimm*(exp(aimm*(tmelt - t(i)))-1._r8)/lamc(i)**3
   949: |         
   950: |       I         call rising_factorial(pgam(i)+4._r8, 3,tmp)
   951: |                 mnuccc(i) = dum(i) * nnuccc(i) * &
   952: |                      pi/6._r8*rhow* &
   953: |                      tmp/lamc(i)**3
   954: |         
   955: |              else
   956: |                 mnuccc(i) = 0._r8
   957: |                 nnuccc(i) = 0._r8
   958: |              end if ! qcic > qsmall and t < 4 deg C
   959: V------     enddo
   960:           
   961:           end subroutine immersion_freezing


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::CONTACT_FREEZING
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1011: vec( 103): Unvectorized loop.
  1011: vec( 113): Overhead of loop division is too large.
  1017: inl(1222): Inlined: MICRO_MG_UTILS::VAR_COEF_R8
  1019: inl(1222): Inlined: MICRO_MG_UTILS::VAR_COEF_R8
  1031: vec( 101): Vectorized loop.
  1033: vec( 101): Vectorized loop.
  1035: vec( 126): Idiom detected.: SUM
  1035: vec( 101): Vectorized loop.
  1038: inl(1222): Inlined: MICRO_MG_UTILS::RISING_FACTORIAL_INT


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::CONTACT_FREEZING
FORMAT LIST

 LINE   LOOP      STATEMENT

   967:           subroutine contact_freezing (microp_uniform, t, p, rndst, nacon, &
   968:                pgam, lamc, qcic, ncic, relvar, mnucct, nnucct, mgncol, mdust)
   969:           
   970:             logical, intent(in) :: microp_uniform
   971:           
   972:             integer, intent(in) :: mgncol
   973:             integer, intent(in) :: mdust
   974:           
   975:             real(r8), dimension(mgncol), intent(in) :: t            ! Temperature
   976:             real(r8), dimension(mgncol), intent(in) :: p            ! Pressure
   977:             real(r8), dimension(mgncol, mdust), intent(in) :: rndst ! Radius (for multiple dust bins)
   978:             real(r8), dimension(mgncol, mdust), intent(in) :: nacon ! Number (for multiple dust bins)
   979:             ! Size distribution parameters for cloud droplets
   980:           
   981:             real(r8), dimension(mgncol), intent(in) :: pgam
   982:             real(r8), dimension(mgncol), intent(in) :: lamc
   983:             ! MMR and number concentration of in-cloud liquid water
   984:           
   985:             real(r8), dimension(mgncol), intent(in) :: qcic
   986:             real(r8), dimension(mgncol), intent(in) :: ncic
   987:             ! Relative cloud water variance
   988:           
   989:             real(r8), dimension(mgncol), intent(in) :: relvar
   990:             ! Output tendencies
   991:           
   992:             real(r8), dimension(mgncol), intent(out) :: mnucct ! MMR
   993:             real(r8), dimension(mgncol), intent(out) :: nnucct ! Number
   994:           
   995:             real(r8) :: tcnt                  ! scaled relative temperature
   996:             real(r8) :: viscosity             ! temperature-specific viscosity (kg/m/s)
   997:             real(r8) :: mfp                   ! temperature-specific mean free path (m)
   998:             ! Dimension these according to number of dust bins, inferred from rndst size
   999:           
  1000:             real(r8) :: nslip(size(rndst,2))  ! slip correction factors
  1001:             real(r8) :: ndfaer(size(rndst,2)) ! aerosol diffusivities (m^2/sec)
  1002:             ! Coefficients not used for subcolumns
  1003:           
  1004:             real(r8) :: dum, dum1,tmp
  1005:             ! Common factor between mass and number.
  1006:           
  1007:             real(r8) :: contact_factor
  1008:           
  1009:             integer  :: i
  1010:           
  1011: +------>    do i = 1,mgncol
  1012: |         
  1013: |              if (qcic(i) >= qsmall .and. t(i) < 269.15_r8) then
  1014: |         
  1015: |                 if (.not. microp_uniform) then
  1016: |                    !dum = var_coef(relvar(i), 4._r8/3._r8)
  1017: |       I            call var_coef(relvar(i), 4._r8/3._r8,dum)
  1018: |                    !dum1 = var_coef(relvar(i), 1._r8/3._r8)
  1019: |       I            call var_coef(relvar(i), 1._r8/3._r8, dum1)
  1020: |                 else
  1021: |                    dum = 1._r8
  1022: |                    dum1 = 1._r8
  1023: |                 endif
  1024: |         
  1025: |                 tcnt=(270.16_r8-t(i))**1.3_r8
  1026: |                 viscosity = 1.8e-5_r8*(t(i)/298.0_r8)**0.85_r8    ! Viscosity (kg/m/s)
  1027: |                 mfp = 2.0_r8*viscosity/ &                         ! Mean free path (m)
  1028: |                              (p(i)*sqrt( 8.0_r8*28.96e-3_r8/(pi*8.314409_r8*t(i)) ))
  1029: |                 ! Note that these two are vectors.
  1030: |         
  1031: |V=====>          nslip = 1.0_r8+(mfp/rndst(i,:))*(1.257_r8+(0.4_r8*exp(-(1.1_r8*rndst(i,:)/mfp))))! Slip correction factor
  1032: |         
  1033: |V=====>          ndfaer = 1.381e-23_r8*t(i)*nslip/(6._r8*pi*viscosity*rndst(i,:))  ! aerosol diffusivity (m2/s)
  1034: |         
  1035: |V=====>          contact_factor = dot_product(ndfaer,nacon(i,:)*tcnt) * pi * &
  1036: |                      ncic(i) * (pgam(i) + 1._r8) / lamc(i)
  1037: |         
  1038: |       I         call rising_factorial(pgam(i)+2._r8, 3,tmp)
  1039: |                 mnucct(i) = dum * contact_factor * &
  1040: |                      pi/3._r8*rhow*tmp/lamc(i)**3
  1041: |         
  1042: |                 nnucct(i) =  dum1 * 2._r8 * contact_factor
  1043: |         
  1044: |              else
  1045: |         
  1046: |                 mnucct(i)=0._r8
  1047: |                 nnucct(i)=0._r8
  1048: |         
  1049: |              end if ! qcic > qsmall and t < 4 deg C
  1050: +------     end do
  1051:           
  1052:           end subroutine contact_freezing


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SNOW_SELF_AGGREGATION
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1077: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SNOW_SELF_AGGREGATION
FORMAT LIST

 LINE   LOOP      STATEMENT

  1059:           subroutine snow_self_aggregation(t, rho, asn, rhosn, qsic, nsic, nsagg, mgncol)
  1060:           
  1061:             integer,                          intent(in) :: mgncol
  1062:           
  1063:             real(r8), dimension(mgncol), intent(in) :: t     ! Temperature
  1064:             real(r8), dimension(mgncol), intent(in) :: rho   ! Density
  1065:             real(r8), dimension(mgncol), intent(in) :: asn   ! fall speed parameter for snow
  1066:             real(r8),                    intent(in) :: rhosn ! density of snow
  1067:             ! In-cloud snow
  1068:           
  1069:             real(r8), dimension(mgncol), intent(in) :: qsic ! MMR
  1070:             real(r8), dimension(mgncol), intent(in) :: nsic ! Number
  1071:             ! Output number tendency
  1072:           
  1073:             real(r8), dimension(mgncol), intent(out) :: nsagg
  1074:           
  1075:             integer :: i
  1076:           
  1077: V------>    do i=1,mgncol
  1078: |              if (qsic(i) >= qsmall .and. t(i) <= tmelt) then
  1079: |                 nsagg(i) = -1108._r8*eii/(4._r8*720._r8*rhosn)*asn(i)*qsic(i)*nsic(i)*rho(i)*&
  1080: |                      ((qsic(i)/nsic(i))*(1._r8/(rhosn*pi)))**((bs-1._r8)/3._r8)
  1081: |              else
  1082: |                 nsagg(i)=0._r8
  1083: |              end if
  1084: V------     enddo
  1085:           end subroutine snow_self_aggregation


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ACCRETE_CLOUD_WATER_SNOW
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1133: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ACCRETE_CLOUD_WATER_SNOW
FORMAT LIST

 LINE   LOOP      STATEMENT

  1094:           subroutine accrete_cloud_water_snow(t, rho, asn, uns, mu, qcic, ncic, qsic, &
  1095:                pgam, lamc, lams, n0s, psacws, npsacws, mgncol)
  1096:           
  1097:             integer, intent(in) :: mgncol
  1098:             real(r8), dimension(mgncol), intent(in) :: t   ! Temperature
  1099:             real(r8), dimension(mgncol), intent(in) :: rho ! Density
  1100:             real(r8), dimension(mgncol), intent(in) :: asn ! Fallspeed parameter (snow)
  1101:             real(r8), dimension(mgncol), intent(in) :: uns ! Current fallspeed   (snow)
  1102:             real(r8), dimension(mgncol), intent(in) :: mu  ! Viscosity
  1103:             ! In-cloud liquid water
  1104:           
  1105:             real(r8), dimension(mgncol), intent(in) :: qcic ! MMR
  1106:             real(r8), dimension(mgncol), intent(in) :: ncic ! Number
  1107:             ! In-cloud snow
  1108:           
  1109:             real(r8), dimension(mgncol), intent(in) :: qsic ! MMR
  1110:             ! Cloud droplet size parameters
  1111:           
  1112:             real(r8), dimension(mgncol), intent(in) :: pgam
  1113:             real(r8), dimension(mgncol), intent(in) :: lamc
  1114:             ! Snow size parameters
  1115:           
  1116:             real(r8), dimension(mgncol), intent(in) :: lams
  1117:             real(r8), dimension(mgncol), intent(in) :: n0s
  1118:             ! Output tendencies
  1119:           
  1120:             real(r8), dimension(mgncol), intent(out) :: psacws  ! Mass mixing ratio
  1121:             real(r8), dimension(mgncol), intent(out) :: npsacws ! Number concentration
  1122:           
  1123:             real(r8) :: dc0 ! Provisional mean droplet size
  1124:             real(r8) :: dum
  1125:             real(r8) :: eci ! collection efficiency for riming of snow by droplets
  1126:             ! Fraction of cloud droplets accreted per second
  1127:           
  1128:             real(r8) :: accrete_rate
  1129:             integer :: i
  1130:             ! ignore collision of snow with droplets above freezing
  1131:           
  1132:           
  1133: V------>    do i=1,mgncol
  1134: |              if (qsic(i) >= qsmall .and. t(i) <= tmelt .and. qcic(i) >= qsmall) then
  1135: |                 ! put in size dependent collection efficiency
  1136: |                 ! mean diameter of snow is area-weighted, since
  1137: |                 ! accretion is function of crystal geometric area
  1138: |                 ! collection efficiency is approximation based on stoke's law (Thompson et al. 2004)
  1139: |         
  1140: |         
  1141: |                 dc0 = (pgam(i)+1._r8)/lamc(i)
  1142: |                 dum = dc0*dc0*uns(i)*rhow*lams(i)/(9._r8*mu(i))
  1143: |                 eci = dum*dum/((dum+0.4_r8)*(dum+0.4_r8))
  1144: |         
  1145: |                 eci = max(eci,0._r8)
  1146: |                 eci = min(eci,1._r8)
  1147: |                 ! no impact of sub-grid distribution of qc since psacws
  1148: |                 ! is linear in qc
  1149: |         
  1150: |                 accrete_rate = pi/4._r8*asn(i)*rho(i)*n0s(i)*eci*gamma_bs_plus3 / lams(i)**(bs+3._r8)
  1151: |                 psacws(i) = accrete_rate*qcic(i)
  1152: |                 npsacws(i) = accrete_rate*ncic(i)
  1153: |              else
  1154: |                 psacws(i) = 0._r8
  1155: |                 npsacws(i) = 0._r8
  1156: |              end if
  1157: V------     enddo
  1158:           end subroutine accrete_cloud_water_snow


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SECONDARY_ICE_PRODUCTION
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1177: vec( 101): Vectorized loop.
  1187: opt(1112): Loop fused with previous loop.: I


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SECONDARY_ICE_PRODUCTION
FORMAT LIST

 LINE   LOOP      STATEMENT

  1164:           subroutine secondary_ice_production(t, psacws, msacwi, nsacwi, mgncol)
  1165:           
  1166:             integer, intent(in) :: mgncol
  1167:             real(r8), dimension(mgncol), intent(in) :: t ! Temperature
  1168:             ! Accretion of cloud water to snow tendencies
  1169:           
  1170:             real(r8), dimension(mgncol), intent(inout) :: psacws ! MMR
  1171:             ! Output (ice) tendencies
  1172:           
  1173:             real(r8), dimension(mgncol), intent(out) :: msacwi ! MMR
  1174:             real(r8), dimension(mgncol), intent(out) :: nsacwi ! Number
  1175:             integer :: i
  1176:           
  1177: V------>    do i=1,mgncol
  1178: |              if((t(i) < 270.16_r8) .and. (t(i) >= 268.16_r8)) then
  1179: |                 nsacwi(i) = 3.5e8_r8*(270.16_r8-t(i))/2.0_r8*psacws(i)
  1180: |              else if((t(i) < 268.16_r8) .and. (t(i) >= 265.16_r8)) then
  1181: |                 nsacwi(i) = 3.5e8_r8*(t(i)-265.16_r8)/3.0_r8*psacws(i)
  1182: |              else
  1183: |                 nsacwi(i) = 0.0_r8
  1184: |              endif
  1185: |           enddo
  1186: |         
  1187: |           do i=1,mgncol
  1188: |              msacwi(i) = min(nsacwi(i)*mi0, psacws(i))
  1189: |              psacws(i) = psacws(i) - msacwi(i)
  1190: V------     enddo
  1191:           end subroutine secondary_ice_production


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ACCRETE_RAIN_SNOW
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1238: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ACCRETE_RAIN_SNOW
FORMAT LIST

 LINE   LOOP      STATEMENT

  1197:           subroutine accrete_rain_snow(t, rho, umr, ums, unr, uns, qric, qsic, &
  1198:                lamr, n0r, lams, n0s, pracs, npracs, mgncol)
  1199:           
  1200:             integer,                          intent(in) :: mgncol
  1201:           
  1202:             real(r8), dimension(mgncol), intent(in) :: t   ! Temperature
  1203:             real(r8), dimension(mgncol), intent(in) :: rho ! Density
  1204:             ! Fallspeeds
  1205:             ! mass-weighted
  1206:           
  1207:             real(r8), dimension(mgncol), intent(in) :: umr ! rain
  1208:             real(r8), dimension(mgncol), intent(in) :: ums ! snow
  1209:             ! number-weighted
  1210:             real(r8), dimension(mgncol), intent(in) :: unr ! rain
  1211:             real(r8), dimension(mgncol), intent(in) :: uns ! snow
  1212:             ! In cloud MMRs
  1213:           
  1214:             real(r8), dimension(mgncol), intent(in) :: qric ! rain
  1215:             real(r8), dimension(mgncol), intent(in) :: qsic ! snow
  1216:             ! Size distribution parameters
  1217:             ! rain
  1218:           
  1219:             real(r8), dimension(mgncol), intent(in) :: lamr
  1220:             real(r8), dimension(mgncol), intent(in) :: n0r
  1221:             ! snow
  1222:             real(r8), dimension(mgncol), intent(in) :: lams
  1223:             real(r8), dimension(mgncol), intent(in) :: n0s
  1224:             ! Output tendencies
  1225:           
  1226:             real(r8), dimension(mgncol), intent(out) :: pracs  ! MMR
  1227:             real(r8), dimension(mgncol), intent(out) :: npracs ! Number
  1228:             ! Collection efficiency for accretion of rain by snow
  1229:           
  1230:             real(r8), parameter :: ecr = 1.0_r8
  1231:             ! Ratio of average snow diameter to average rain diameter.
  1232:           
  1233:             real(r8) :: d_rat
  1234:             ! Common factor between mass and number expressions
  1235:             real(r8) :: common_factor
  1236:             integer :: i
  1237:           
  1238: V------>    do i=1,mgncol
  1239: |              if (qric(i) >= icsmall .and. qsic(i) >= icsmall .and. t(i) <= tmelt) then
  1240: |         
  1241: |                 common_factor = pi*ecr*rho(i)*n0r(i)*n0s(i)/(lamr(i)**3 * lams(i))
  1242: |         
  1243: |                 d_rat = lamr(i)/lams(i)
  1244: |         
  1245: |                 pracs(i) = common_factor*pi*rhow* &
  1246: |                      sqrt((1.2_r8*umr(i)-0.95_r8*ums(i))**2 + 0.08_r8*ums(i)*umr(i)) / lamr(i)**3 * &
  1247: |                      ((0.5_r8*d_rat + 2._r8)*d_rat + 5._r8)
  1248: |         
  1249: |                 npracs(i) = common_factor*0.5_r8* &
  1250: |                      sqrt(1.7_r8*(unr(i)-uns(i))**2 + 0.3_r8*unr(i)*uns(i)) * &
  1251: |                      ((d_rat + 1._r8)*d_rat + 1._r8)
  1252: |         
  1253: |              else
  1254: |                 pracs(i) = 0._r8
  1255: |                 npracs(i) = 0._r8
  1256: |              end if
  1257: V------     enddo
  1258:           end subroutine accrete_rain_snow


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::HETEROGENEOUS_RAIN_FREEZING
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1279: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::HETEROGENEOUS_RAIN_FREEZING
FORMAT LIST

 LINE   LOOP      STATEMENT

  1264:           subroutine heterogeneous_rain_freezing(t, qric, nric, lamr, mnuccr, nnuccr, mgncol)
  1265:           
  1266:             integer,                          intent(in) :: mgncol
  1267:             real(r8), dimension(mgncol), intent(in) :: t    ! Temperature
  1268:             ! In-cloud rain
  1269:           
  1270:             real(r8), dimension(mgncol), intent(in) :: qric ! MMR
  1271:             real(r8), dimension(mgncol), intent(in) :: nric ! Number
  1272:             real(r8), dimension(mgncol), intent(in) :: lamr ! size parameter
  1273:             ! Output tendencies
  1274:           
  1275:             real(r8), dimension(mgncol), intent(out) :: mnuccr ! MMR
  1276:             real(r8), dimension(mgncol), intent(out) :: nnuccr ! Number
  1277:             integer :: i
  1278:           
  1279: V------>    do i=1,mgncol
  1280: |         
  1281: |              if (t(i) < 269.15_r8 .and. qric(i) >= qsmall) then
  1282: |                 nnuccr(i) = pi*nric(i)*bimm* &
  1283: |                      (exp(aimm*(tmelt - t(i)))-1._r8)/lamr(i)**3
  1284: |         
  1285: |                 mnuccr(i) = nnuccr(i) * 20._r8*pi*rhow/lamr(i)**3
  1286: |         
  1287: |              else
  1288: |                 mnuccr(i) = 0._r8
  1289: |                 nnuccr(i) = 0._r8
  1290: |              end if
  1291: V------     enddo
  1292:           end subroutine heterogeneous_rain_freezing


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ACCRETE_CLOUD_WATER_RAIN
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1325: vec( 101): Vectorized loop.
  1326: inl(1222): Inlined: MICRO_MG_UTILS::VAR_COEF_R8
  1330: vec( 101): Vectorized loop.
  1332: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ACCRETE_CLOUD_WATER_RAIN
FORMAT LIST

 LINE   LOOP      STATEMENT

  1299:           subroutine accrete_cloud_water_rain(microp_uniform, qric, qcic, &
  1300:                ncic, relvar, accre_enhan, pra, npra, mgncol)
  1301:           
  1302:             logical, intent(in) :: microp_uniform
  1303:             integer, intent(in) :: mgncol
  1304:             ! In-cloud rain
  1305:             real(r8), dimension(mgncol), intent(in) :: qric ! MMR
  1306:             ! Cloud droplets
  1307:           
  1308:             real(r8), dimension(mgncol), intent(in) :: qcic ! MMR
  1309:             real(r8), dimension(mgncol), intent(in) :: ncic ! Number
  1310:             ! SGS variability
  1311:           
  1312:             real(r8), dimension(mgncol), intent(in) :: relvar
  1313:             real(r8), dimension(mgncol), intent(in) :: accre_enhan
  1314:             ! Output tendencies
  1315:           
  1316:             real(r8), dimension(mgncol), intent(out) :: pra  ! MMR
  1317:             real(r8), dimension(mgncol), intent(out) :: npra ! Number
  1318:             ! Coefficient that varies for subcolumns
  1319:           
  1320:             real(r8), dimension(mgncol) :: pra_coef
  1321:           
  1322:             integer :: i
  1323:           
  1324:             if (.not. microp_uniform) then
  1325: V------>      do i=1,mgncol
  1326: |       I        call  var_coef(relvar(i), 1.15_r8, pra_coef(i))
  1327: |                pra_coef(i) = pra_coef(i)*accre_enhan(i)
  1328: V------       enddo
  1329:             else
  1330: V======>      pra_coef = 1._r8
  1331:             end if
  1332: V------>    do i=1,mgncol
  1333: |             if (qric(i) >= qsmall .and. qcic(i) >= qsmall) then
  1334: |               ! include sub-grid distribution of cloud water
  1335: |         
  1336: |               pra(i) = pra_coef(i) * 67._r8*(qcic(i)*qric(i))**1.15_r8
  1337: |         
  1338: |               npra(i) = pra(i)*ncic(i)/qcic(i)
  1339: |         
  1340: |             else
  1341: |               pra(i) = 0._r8
  1342: |               npra(i) = 0._r8
  1343: |             end if
  1344: V------     end do
  1345:           end subroutine accrete_cloud_water_rain


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SELF_COLLECTION_RAIN
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1365: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::SELF_COLLECTION_RAIN
FORMAT LIST

 LINE   LOOP      STATEMENT

  1351:           subroutine self_collection_rain(rho, qric, nric, nragg, mgncol)
  1352:           
  1353:             integer,                          intent(in) :: mgncol
  1354:             real(r8), dimension(mgncol), intent(in) :: rho  ! Air density
  1355:             ! Rain
  1356:           
  1357:             real(r8), dimension(mgncol), intent(in) :: qric ! MMR
  1358:             real(r8), dimension(mgncol), intent(in) :: nric ! Number
  1359:             ! Output number tendency
  1360:           
  1361:             real(r8), dimension(mgncol), intent(out) :: nragg
  1362:           
  1363:             integer :: i
  1364:           
  1365: V------>    do i=1,mgncol
  1366: |              if (qric(i) >= qsmall) then
  1367: |                 nragg(i) = -8._r8*nric(i)*qric(i)*rho(i)
  1368: |              else
  1369: |                 nragg(i) = 0._r8
  1370: |              end if
  1371: V------     enddo
  1372:           end subroutine self_collection_rain


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ACCRETE_CLOUD_ICE_SNOW
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1407: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::ACCRETE_CLOUD_ICE_SNOW
FORMAT LIST

 LINE   LOOP      STATEMENT

  1379:           subroutine accrete_cloud_ice_snow(t, rho, asn, qiic, niic, qsic, &
  1380:                lams, n0s, prai, nprai, mgncol)
  1381:           
  1382:             integer,                          intent(in) :: mgncol
  1383:             real(r8), dimension(mgncol), intent(in) :: t    ! Temperature
  1384:             real(r8), dimension(mgncol), intent(in) :: rho   ! Density
  1385:           
  1386:             real(r8), dimension(mgncol), intent(in) :: asn  ! Snow fallspeed parameter
  1387:             ! Cloud ice
  1388:           
  1389:             real(r8), dimension(mgncol), intent(in) :: qiic ! MMR
  1390:             real(r8), dimension(mgncol), intent(in) :: niic ! Number
  1391:           
  1392:             real(r8), dimension(mgncol), intent(in) :: qsic ! Snow MMR
  1393:             ! Snow size parameters
  1394:           
  1395:             real(r8), dimension(mgncol), intent(in) :: lams
  1396:             real(r8), dimension(mgncol), intent(in) :: n0s
  1397:             ! Output tendencies
  1398:           
  1399:             real(r8), dimension(mgncol), intent(out) :: prai ! MMR
  1400:             real(r8), dimension(mgncol), intent(out) :: nprai ! Number
  1401:             ! Fraction of cloud ice particles accreted per second
  1402:           
  1403:             real(r8) :: accrete_rate
  1404:           
  1405:             integer :: i
  1406:           
  1407: V------>    do i=1,mgncol
  1408: |              if (qsic(i) >= qsmall .and. qiic(i) >= qsmall .and. t(i) <= tmelt) then
  1409: |         
  1410: |                 accrete_rate = pi/4._r8 * eii * asn(i) * rho(i) * n0s(i) * gamma_bs_plus3/ &
  1411: |                      lams(i)**(bs+3._r8)
  1412: |         
  1413: |                 prai(i) = accrete_rate * qiic(i)
  1414: |                 nprai(i) = accrete_rate * niic(i)
  1415: |         
  1416: |              else
  1417: |                 prai(i) = 0._r8
  1418: |                 nprai(i) = 0._r8
  1419: |              end if
  1420: V------     enddo
  1421:           end subroutine accrete_cloud_ice_snow


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::EVAPORATE_SUBLIMATE_PRECIP
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1477: vec( 101): Vectorized loop.
  1481: vec( 101): Vectorized loop.
  1488: vec( 103): Unvectorized loop.
  1488: vec( 113): Overhead of loop division is too large.
  1497: opt(1019): Feedback of scalar value from one loop pass to another.: QCLR
  1497: vec( 121): Unvectorizable dependency.
  1504: inl(1222): Inlined: MICRO_MG_UTILS::CALC_AB
  1524: inl(1222): Inlined: MICRO_MG_UTILS::CALC_AB


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::EVAPORATE_SUBLIMATE_PRECIP
FORMAT LIST

 LINE   LOOP      STATEMENT

  1429:           subroutine evaporate_sublimate_precip(t, rho, dv, mu, sc, q, qvl, qvi, &
  1430:                lcldm, precip_frac, arn, asn, qcic, qiic, qric, qsic, lamr, n0r, lams, n0s, &
  1431:                pre, prds, am_evp_st, mgncol)
  1432:           
  1433:             integer,  intent(in) :: mgncol
  1434:           
  1435:             real(r8), dimension(mgncol), intent(in) :: t    ! temperature
  1436:             real(r8), dimension(mgncol), intent(in) :: rho  ! air density
  1437:             real(r8), dimension(mgncol), intent(in) :: dv   ! water vapor diffusivity
  1438:             real(r8), dimension(mgncol), intent(in) :: mu   ! viscosity
  1439:             real(r8), dimension(mgncol), intent(in) :: sc   ! schmidt number
  1440:             real(r8), dimension(mgncol), intent(in) :: q    ! humidity
  1441:             real(r8), dimension(mgncol), intent(in) :: qvl  ! saturation humidity (water)
  1442:             real(r8), dimension(mgncol), intent(in) :: qvi  ! saturation humidity (ice)
  1443:             real(r8), dimension(mgncol), intent(in) :: lcldm  ! liquid cloud fraction
  1444:             real(r8), dimension(mgncol), intent(in) :: precip_frac ! precipitation fraction (maximum overlap)
  1445:             ! fallspeed parameters
  1446:           
  1447:             real(r8), dimension(mgncol), intent(in) :: arn  ! rain
  1448:             real(r8), dimension(mgncol), intent(in) :: asn  ! snow
  1449:             ! In-cloud MMRs
  1450:           
  1451:             real(r8), dimension(mgncol), intent(in) :: qcic ! cloud liquid
  1452:             real(r8), dimension(mgncol), intent(in) :: qiic ! cloud ice
  1453:             real(r8), dimension(mgncol), intent(in) :: qric ! rain
  1454:             real(r8), dimension(mgncol), intent(in) :: qsic ! snow
  1455:             ! Size parameters
  1456:             ! rain
  1457:           
  1458:             real(r8), dimension(mgncol), intent(in) :: lamr
  1459:             real(r8), dimension(mgncol), intent(in) :: n0r
  1460:             ! snow
  1461:             real(r8), dimension(mgncol), intent(in) :: lams
  1462:             real(r8), dimension(mgncol), intent(in) :: n0s
  1463:             ! Output tendencies
  1464:           
  1465:             real(r8), dimension(mgncol), intent(out) :: pre
  1466:             real(r8), dimension(mgncol), intent(out) :: prds
  1467:             real(r8), dimension(mgncol), intent(out) :: am_evp_st ! Fractional area where rain evaporates.
  1468:           
  1469:             real(r8) :: qclr   ! water vapor mixing ratio in clear air
  1470:             real(r8) :: ab     ! correction to account for latent heat
  1471:             real(r8) :: eps    ! 1/ sat relaxation timescale
  1472:           
  1473:             real(r8), dimension(mgncol) :: dum
  1474:           
  1475:             integer :: i
  1476:           
  1477: V======>    am_evp_st = 0._r8
  1478:             ! set temporary cloud fraction to zero if cloud water + ice is very small
  1479:             ! this will ensure that evaporation/sublimation of precip occurs over
  1480:             ! entire grid cell, since min cloud fraction is specified otherwise
  1481: V------>    do i=1,mgncol
  1482: |              if (qcic(i)+qiic(i) < 1.e-6_r8) then
  1483: |                 dum(i) = 0._r8
  1484: |              else
  1485: |                 dum(i) = lcldm(i)
  1486: |              end if
  1487: V------     enddo
  1488: +------>    do i=1,mgncol
  1489: |           ! only calculate if there is some precip fraction > cloud fraction
  1490: |         
  1491: |              if (precip_frac(i) > dum(i)) then
  1492: |         
  1493: |                 if (qric(i) >= qsmall .or. qsic(i) >= qsmall) then
  1494: |                    am_evp_st(i) = precip_frac(i) - dum(i)
  1495: |                    ! calculate q for out-of-cloud region
  1496: |         
  1497: |                    qclr=(q(i)-dum(i)*qvl(i))/(1._r8-dum(i))
  1498: |                 end if
  1499: |                 ! evaporation of rain
  1500: |         
  1501: |                 if (qric(i) >= qsmall) then
  1502: |         
  1503: |                    !ab = calc_ab(t(i), qvl(i), xxlv)
  1504: |       I            call calc_ab(t(i), qvl(i), xxlv, ab)
  1505: |                    eps = 2._r8*pi*n0r(i)*rho(i)*Dv(i)* &
  1506: |                         (f1r/(lamr(i)*lamr(i))+ &
  1507: |                         f2r*(arn(i)*rho(i)/mu(i))**0.5_r8* &
  1508: |                         sc(i)**(1._r8/3._r8)*gamma_half_br_plus5/ &
  1509: |                         (lamr(i)**(5._r8/2._r8+br/2._r8)))
  1510: |         
  1511: |                    pre(i) = eps*(qclr-qvl(i))/ab
  1512: |                    ! only evaporate in out-of-cloud region
  1513: |                    ! and distribute across precip_frac
  1514: |         
  1515: |                    pre(i)=min(pre(i)*am_evp_st(i),0._r8)
  1516: |                    pre(i)=pre(i)/precip_frac(i)
  1517: |                 else
  1518: |                    pre(i) = 0._r8
  1519: |                 end if
  1520: |                 ! sublimation of snow
  1521: |         
  1522: |                 if (qsic(i) >= qsmall) then
  1523: |                    ! ab = calc_ab(t(i), qvi(i), xxls)
  1524: |       I            call calc_ab(t(i), qvi(i), xxls, ab)
  1525: |                    eps = 2._r8*pi*n0s(i)*rho(i)*Dv(i)* &
  1526: |                         (f1s/(lams(i)*lams(i))+ &
  1527: |                         f2s*(asn(i)*rho(i)/mu(i))**0.5_r8* &
  1528: |                         sc(i)**(1._r8/3._r8)*gamma_half_bs_plus5/ &
  1529: |                         (lams(i)**(5._r8/2._r8+bs/2._r8)))
  1530: |                    prds(i) = eps*(qclr-qvi(i))/ab
  1531: |                    ! only sublimate in out-of-cloud region and distribute over precip_frac
  1532: |         
  1533: |                    prds(i)=min(prds(i)*am_evp_st(i),0._r8)
  1534: |                    prds(i)=prds(i)/precip_frac(i)
  1535: |                 else
  1536: |                    prds(i) = 0._r8
  1537: |                 end if
  1538: |         
  1539: |              else
  1540: |                 prds(i) = 0._r8
  1541: |                 pre(i) = 0._r8
  1542: |              end if
  1543: +------     enddo
  1544:           
  1545:           end subroutine evaporate_sublimate_precip


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::BERGERON_PROCESS_SNOW
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1582: vec( 101): Vectorized loop.
  1585: inl(1222): Inlined: MICRO_MG_UTILS::CALC_AB


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::BERGERON_PROCESS_SNOW
FORMAT LIST

 LINE   LOOP      STATEMENT

  1550:           subroutine bergeron_process_snow(t, rho, dv, mu, sc, qvl, qvi, asn, &
  1551:                qcic, qsic, lams, n0s, bergs, mgncol)
  1552:           
  1553:             integer, intent(in) :: mgncol
  1554:           
  1555:             real(r8), dimension(mgncol), intent(in) :: t    ! temperature
  1556:             real(r8), dimension(mgncol), intent(in) :: rho  ! air density
  1557:             real(r8), dimension(mgncol), intent(in) :: dv   ! water vapor diffusivity
  1558:             real(r8), dimension(mgncol), intent(in) :: mu   ! viscosity
  1559:             real(r8), dimension(mgncol), intent(in) :: sc   ! schmidt number
  1560:             real(r8), dimension(mgncol), intent(in) :: qvl  ! saturation humidity (water)
  1561:             real(r8), dimension(mgncol), intent(in) :: qvi  ! saturation humidity (ice)
  1562:             ! fallspeed parameter for snow
  1563:           
  1564:             real(r8), dimension(mgncol), intent(in) :: asn
  1565:             ! In-cloud MMRs
  1566:           
  1567:             real(r8), dimension(mgncol), intent(in) :: qcic ! cloud liquid
  1568:             real(r8), dimension(mgncol), intent(in) :: qsic ! snow
  1569:             ! Size parameters for snow
  1570:           
  1571:             real(r8), dimension(mgncol), intent(in) :: lams
  1572:             real(r8), dimension(mgncol), intent(in) :: n0s
  1573:             ! Output tendencies
  1574:           
  1575:             real(r8), dimension(mgncol), intent(out) :: bergs
  1576:           
  1577:             real(r8) :: ab     ! correction to account for latent heat
  1578:             real(r8) :: eps    ! 1/ sat relaxation timescale
  1579:           
  1580:             integer :: i
  1581:           
  1582: V------>    do i=1,mgncol
  1583: |              if (qsic(i) >= qsmall.and. qcic(i) >= qsmall .and. t(i) < tmelt) then
  1584: |                 ! ab = calc_ab(t(i), qvi(i), xxls)
  1585: |       I         call calc_ab(t(i), qvi(i), xxls, ab)
  1586: |                 eps = 2._r8*pi*n0s(i)*rho(i)*Dv(i)* &
  1587: |                      (f1s/(lams(i)*lams(i))+ &
  1588: |                      f2s*(asn(i)*rho(i)/mu(i))**0.5_r8* &
  1589: |                      sc(i)**(1._r8/3._r8)*gamma_half_bs_plus5/ &
  1590: |                      (lams(i)**(5._r8/2._r8+bs/2._r8)))
  1591: |                 bergs(i) = eps*(qvl(i)-qvi(i))/ab
  1592: |              else
  1593: |                 bergs(i) = 0._r8
  1594: |              end if
  1595: V------     enddo
  1596:           end subroutine bergeron_process_snow


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::LIMITER_IS_ON
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1606: vec( 122): Dependency unknown. Unvectorizable dependency is assumed.: CONFLICT.I64


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::LIMITER_IS_ON
FORMAT LIST

 LINE   LOOP      STATEMENT

  1602:           pure function limiter_is_on(lim)
  1603:             real(r8), intent(in) :: lim
  1604:             logical :: limiter_is_on
  1605:           
  1606:             limiter_is_on = transfer(lim, limiter_off) /= limiter_off
  1607:           
  1608:           end function limiter_is_on


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KR_EXTERNS_IN_MICRO_MG_UTILS
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1616: inl(1214): Expansion routine is too big for automatic expansion.: MICRO_MG_UTILS::KR_MICRO_MG_UTILS_MGHYDROMETEORPROPS


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KR_EXTERNS_IN_MICRO_MG_UTILS
FORMAT LIST

 LINE   LOOP      STATEMENT

  1611:           SUBROUTINE kr_externs_in_micro_mg_utils(kgen_unit)
  1612:               INTEGER, INTENT(IN) :: kgen_unit
  1613:               LOGICAL :: kgen_istrue
  1614:               REAL(KIND=8) :: kgen_array_sum
  1615:            
  1616:               CALL kr_micro_mg_utils_mghydrometeorprops(mg_liq_props, kgen_unit, "mg_liq_props", .FALSE.)
  1617:               CALL kr_micro_mg_utils_mghydrometeorprops(mg_ice_props, kgen_unit, "mg_ice_props", .FALSE.)
  1618:               CALL kr_micro_mg_utils_mghydrometeorprops(mg_rain_props, kgen_unit, "mg_rain_props", .FALSE.)
  1619:               CALL kr_micro_mg_utils_mghydrometeorprops(mg_snow_props, kgen_unit, "mg_snow_props", .FALSE.)
  1620:               READ (UNIT = kgen_unit) rv
  1621:               READ (UNIT = kgen_unit) cpp
  1622:               READ (UNIT = kgen_unit) tmelt
  1623:               READ (UNIT = kgen_unit) xxlv
  1624:               READ (UNIT = kgen_unit) xxls
  1625:               READ (UNIT = kgen_unit) gamma_bs_plus3
  1626:               READ (UNIT = kgen_unit) gamma_half_br_plus5
  1627:               READ (UNIT = kgen_unit) gamma_half_bs_plus5
  1628:           END SUBROUTINE kr_externs_in_micro_mg_utils


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KR_EXTERNS_OUT_MICRO_MG_UTILS
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KR_EXTERNS_OUT_MICRO_MG_UTILS
FORMAT LIST

 LINE   LOOP      STATEMENT

  1631:           SUBROUTINE kr_externs_out_micro_mg_utils(kgen_unit)
  1632:               INTEGER, INTENT(IN) :: kgen_unit
  1633:            
  1634:               LOGICAL :: kgen_istrue
  1635:               REAL(KIND=8) :: kgen_array_sum
  1636:           END SUBROUTINE kr_externs_out_micro_mg_utils


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KR_MICRO_MG_UTILS_MGHYDROMETEORPROPS
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1666: inl(1212): Source for routine not found.: KGEN_UTILS_MOD::KGEN_ARRAY_SUMCHECK
  1666: opt(1418): Constant-length loop is expanded.
  1666: vec( 103): Unvectorized loop.
  1666: vec( 107): Iteration count is too small.
  1669: opt(1418): Constant-length loop is expanded.
  1669: vec( 103): Unvectorized loop.
  1669: vec( 107): Iteration count is too small.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KR_MICRO_MG_UTILS_MGHYDROMETEORPROPS
FORMAT LIST

 LINE   LOOP      STATEMENT

  1639:           RECURSIVE SUBROUTINE kr_micro_mg_utils_mghydrometeorprops(var, kgen_unit, printname, printvar)
  1640:               TYPE(mghydrometeorprops), INTENT(INOUT) :: var
  1641:               INTEGER, INTENT(IN) :: kgen_unit
  1642:               CHARACTER(LEN=*), INTENT(IN) :: printname
  1643:               LOGICAL, INTENT(IN), OPTIONAL :: printvar
  1644:               LOGICAL :: kgen_istrue
  1645:               REAL(KIND=8) :: kgen_array_sum
  1646:            
  1647:               READ (UNIT = kgen_unit) var%rho
  1648:               IF (PRESENT( printvar ) .AND. printvar) THEN
  1649:                   WRITE (*, *) "KGEN DEBUG: " // printname // "%rho = ", var%rho
  1650:               END IF
  1651:            
  1652:               READ (UNIT = kgen_unit) var%eff_dim
  1653:               IF (PRESENT( printvar ) .AND. printvar) THEN
  1654:                   WRITE (*, *) "KGEN DEBUG: " // printname // "%eff_dim = ", var%eff_dim
  1655:               END IF
  1656:            
  1657:               READ (UNIT = kgen_unit) var%shape_coef
  1658:               IF (PRESENT( printvar ) .AND. printvar) THEN
  1659:                   WRITE (*, *) "KGEN DEBUG: " // printname // "%shape_coef = ", var%shape_coef
  1660:               END IF
  1661:            
  1662:               READ (UNIT = kgen_unit) kgen_istrue
  1663:               IF (kgen_istrue) THEN
  1664:                   READ (UNIT = kgen_unit) kgen_array_sum
  1665:                   READ (UNIT = kgen_unit) var%lambda_bounds
  1666: *======>          CALL kgen_array_sumcheck(printname // "%lambda_bounds", kgen_array_sum, DBLE(SUM(var%lambda_bounds, &
  1667:                   &mask=(var%lambda_bounds .eq. var%lambda_bounds))), .TRUE.)
  1668:                   IF (PRESENT( printvar ) .AND. printvar) THEN
  1669: *======>              WRITE (*, *) "KGEN DEBUG: DBLE(SUM(" // printname // "%lambda_bounds)) = ", DBLE(SUM(var%lambda_bounds, &
  1670:                       &mask=(var%lambda_bounds .eq. var%lambda_bounds)))
  1671:                   END IF
  1672:               END IF
  1673:            
  1674:               READ (UNIT = kgen_unit) var%min_mean_mass
  1675:               IF (PRESENT( printvar ) .AND. printvar) THEN
  1676:                   WRITE (*, *) "KGEN DEBUG: " // printname // "%min_mean_mass = ", var%min_mean_mass
  1677:               END IF
  1678:            
  1679:           END SUBROUTINE kr_micro_mg_utils_mghydrometeorprops


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KV_MICRO_MG_UTILS_MGHYDROMETEORPROPS
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  1700: inl(1212): Source for routine not found.: KGEN_UTILS_MOD::KGEN_INIT_CHECK
  1705: vec( 118): Unvectorizable data type.
  1705: vec( 103): Unvectorized loop.
  1705: vec( 108): Unvectorizable loop structure.
  1713: vec( 118): Unvectorizable data type.
  1713: vec( 103): Unvectorized loop.
  1713: vec( 108): Unvectorizable loop structure.
  1719: vec( 118): Unvectorizable data type.
  1719: vec( 103): Unvectorized loop.
  1719: vec( 108): Unvectorizable loop structure.
  1742: vec( 118): Unvectorizable data type.
  1742: vec( 103): Unvectorized loop.
  1742: vec( 108): Unvectorizable loop structure.
  1750: vec( 118): Unvectorizable data type.
  1750: vec( 103): Unvectorized loop.
  1750: vec( 108): Unvectorizable loop structure.
  1756: vec( 118): Unvectorizable data type.
  1756: vec( 103): Unvectorized loop.
  1756: vec( 108): Unvectorizable loop structure.
  1779: vec( 118): Unvectorizable data type.
  1779: vec( 103): Unvectorized loop.
  1779: vec( 108): Unvectorizable loop structure.
  1787: vec( 118): Unvectorizable data type.
  1787: vec( 103): Unvectorized loop.
  1787: vec( 108): Unvectorizable loop structure.
  1793: vec( 118): Unvectorizable data type.
  1793: vec( 103): Unvectorized loop.
  1793: vec( 108): Unvectorizable loop structure.
  1813: opt(1418): Constant-length loop is expanded.
  1816: vec( 118): Unvectorizable data type.
  1816: vec( 103): Unvectorized loop.
  1816: vec( 108): Unvectorizable loop structure.
  1822: vec( 103): Unvectorized loop.
  1822: vec( 107): Iteration count is too small.
  1823: vec( 103): Unvectorized loop.
  1823: vec( 107): Iteration count is too small.
  1830: opt(1418): Constant-length loop is expanded.
  1831: opt(1418): Constant-length loop is expanded.
  1835: vec( 118): Unvectorizable data type.
  1835: vec( 103): Unvectorized loop.
  1835: vec( 108): Unvectorizable loop structure.
  1841: vec( 118): Unvectorizable data type.
  1841: vec( 103): Unvectorized loop.
  1841: vec( 108): Unvectorizable loop structure.
  1850: vec( 103): Unvectorized loop.
  1850: vec( 107): Iteration count is too small.
  1852: opt(1418): Constant-length loop is expanded.
  1853: opt(1418): Constant-length loop is expanded.
  1860: vec( 103): Unvectorized loop.
  1860: vec( 107): Iteration count is too small.
  1862: opt(1418): Constant-length loop is expanded.
  1863: opt(1418): Constant-length loop is expanded.
  1874: vec( 118): Unvectorizable data type.
  1874: vec( 103): Unvectorized loop.
  1874: vec( 108): Unvectorizable loop structure.
  1882: vec( 118): Unvectorizable data type.
  1882: vec( 103): Unvectorized loop.
  1882: vec( 108): Unvectorizable loop structure.
  1888: vec( 118): Unvectorizable data type.
  1888: vec( 103): Unvectorized loop.
  1888: vec( 108): Unvectorizable loop structure.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:07 2018
FILE NAME: micro_mg_utils.F90

PROCEDURE NAME: MICRO_MG_UTILS::KV_MICRO_MG_UTILS_MGHYDROMETEORPROPS
FORMAT LIST

 LINE   LOOP      STATEMENT

  1682:           RECURSIVE SUBROUTINE kv_micro_mg_utils_mghydrometeorprops(varname, check_status, var, kgenref_var)
  1683:               CHARACTER(LEN=*), INTENT(IN) :: varname
  1684:               TYPE(check_t), INTENT(INOUT) :: check_status
  1685:               TYPE(mghydrometeorprops), INTENT(IN) :: var, kgenref_var
  1686:               TYPE(check_t) :: dtype_check_status, comp_check_status
  1687:               INTEGER :: check_result
  1688:               LOGICAL :: is_print = .FALSE.
  1689:            
  1690:               real(KIND=r8) :: diff_rho
  1691:               real(KIND=r8) :: diff_eff_dim
  1692:               real(KIND=r8) :: diff_shape_coef
  1693:               INTEGER :: n_lambda_bounds
  1694:               real(KIND=r8) :: nrmsdiff_lambda_bounds, rmsdiff_lambda_bounds
  1695:               real(KIND=r8), ALLOCATABLE :: buf1_lambda_bounds(:), buf2_lambda_bounds(:)
  1696:               real(KIND=r8) :: diff_min_mean_mass
  1697:            
  1698:               check_status%numTotal = check_status%numTotal + 1
  1699:            
  1700:               CALL kgen_init_check(dtype_check_status, verboseLevel=check_status%verboseLevel)
  1701:               dtype_check_status%numTotal = dtype_check_status%numTotal + 1
  1702:               IF (var%rho == kgenref_var%rho) THEN
  1703:                   dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
  1704:                   IF (check_status%verboseLevel > 2) THEN
  1705:                       WRITE (*, *) trim(adjustl(varname)), "%rho is IDENTICAL."
  1706:                   END IF
  1707:                   check_result = CHECK_IDENTICAL
  1708:               ELSE
  1709:                   diff_rho = ABS(var%rho - kgenref_var%rho)
  1710:                   IF (diff_rho <= kgen_tolerance) THEN
  1711:                       dtype_check_status%numInTol = dtype_check_status%numInTol + 1
  1712:                       IF (check_status%verboseLevel > 1) THEN
  1713:                           WRITE (*, *) trim(adjustl(varname)), "%rho is NOT IDENTICAL(within tolerance)."
  1714:                       END IF
  1715:                       check_result = CHECK_IN_TOL
  1716:                   ELSE
  1717:                       dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
  1718:                       IF (check_status%verboseLevel > 1) THEN
  1719:                           WRITE (*, *) trim(adjustl(varname)), "%rho is NOT IDENTICAL(out of tolerance)."
  1720:                       END IF
  1721:                       check_result = CHECK_OUT_TOL
  1722:                   END IF
  1723:               END IF
  1724:               IF (check_result == CHECK_IDENTICAL) THEN
  1725:                   CONTINUE
  1726:               ELSE IF (check_result == CHECK_OUT_TOL) THEN
  1727:                   IF (check_status%verboseLevel > 2) THEN
  1728:                       WRITE (*, *) "Difference is ", diff_rho
  1729:                       WRITE (*, *) ""
  1730:                   END IF
  1731:               ELSE IF (check_result == CHECK_IN_TOL) THEN
  1732:                   IF (check_status%verboseLevel > 2) THEN
  1733:                       WRITE (*, *) "Difference is ", diff_rho
  1734:                       WRITE (*, *) ""
  1735:                   END IF
  1736:               END IF
  1737:            
  1738:               dtype_check_status%numTotal = dtype_check_status%numTotal + 1
  1739:               IF (var%eff_dim == kgenref_var%eff_dim) THEN
  1740:                   dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
  1741:                   IF (check_status%verboseLevel > 2) THEN
  1742:                       WRITE (*, *) trim(adjustl(varname)), "%eff_dim is IDENTICAL."
  1743:                   END IF
  1744:                   check_result = CHECK_IDENTICAL
  1745:               ELSE
  1746:                   diff_eff_dim = ABS(var%eff_dim - kgenref_var%eff_dim)
  1747:                   IF (diff_eff_dim <= kgen_tolerance) THEN
  1748:                       dtype_check_status%numInTol = dtype_check_status%numInTol + 1
  1749:                       IF (check_status%verboseLevel > 1) THEN
  1750:                           WRITE (*, *) trim(adjustl(varname)), "%eff_dim is NOT IDENTICAL(within tolerance)."
  1751:                       END IF
  1752:                       check_result = CHECK_IN_TOL
  1753:                   ELSE
  1754:                       dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
  1755:                       IF (check_status%verboseLevel > 1) THEN
  1756:                           WRITE (*, *) trim(adjustl(varname)), "%eff_dim is NOT IDENTICAL(out of tolerance)."
  1757:                       END IF
  1758:                       check_result = CHECK_OUT_TOL
  1759:                   END IF
  1760:               END IF
  1761:               IF (check_result == CHECK_IDENTICAL) THEN
  1762:                   CONTINUE
  1763:               ELSE IF (check_result == CHECK_OUT_TOL) THEN
  1764:                   IF (check_status%verboseLevel > 2) THEN
  1765:                       WRITE (*, *) "Difference is ", diff_eff_dim
  1766:                       WRITE (*, *) ""
  1767:                   END IF
  1768:               ELSE IF (check_result == CHECK_IN_TOL) THEN
  1769:                   IF (check_status%verboseLevel > 2) THEN
  1770:                       WRITE (*, *) "Difference is ", diff_eff_dim
  1771:                       WRITE (*, *) ""
  1772:                   END IF
  1773:               END IF
  1774:            
  1775:               dtype_check_status%numTotal = dtype_check_status%numTotal + 1
  1776:               IF (var%shape_coef == kgenref_var%shape_coef) THEN
  1777:                   dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
  1778:                   IF (check_status%verboseLevel > 2) THEN
  1779:                       WRITE (*, *) trim(adjustl(varname)), "%shape_coef is IDENTICAL."
  1780:                   END IF
  1781:                   check_result = CHECK_IDENTICAL
  1782:               ELSE
  1783:                   diff_shape_coef = ABS(var%shape_coef - kgenref_var%shape_coef)
  1784:                   IF (diff_shape_coef <= kgen_tolerance) THEN
  1785:                       dtype_check_status%numInTol = dtype_check_status%numInTol + 1
  1786:                       IF (check_status%verboseLevel > 1) THEN
  1787:                           WRITE (*, *) trim(adjustl(varname)), "%shape_coef is NOT IDENTICAL(within tolerance)."
  1788:                       END IF
  1789:                       check_result = CHECK_IN_TOL
  1790:                   ELSE
  1791:                       dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
  1792:                       IF (check_status%verboseLevel > 1) THEN
  1793:                           WRITE (*, *) trim(adjustl(varname)), "%shape_coef is NOT IDENTICAL(out of tolerance)."
  1794:                       END IF
  1795:                       check_result = CHECK_OUT_TOL
  1796:                   END IF
  1797:               END IF
  1798:               IF (check_result == CHECK_IDENTICAL) THEN
  1799:                   CONTINUE
  1800:               ELSE IF (check_result == CHECK_OUT_TOL) THEN
  1801:                   IF (check_status%verboseLevel > 2) THEN
  1802:                       WRITE (*, *) "Difference is ", diff_shape_coef
  1803:                       WRITE (*, *) ""
  1804:                   END IF
  1805:               ELSE IF (check_result == CHECK_IN_TOL) THEN
  1806:                   IF (check_status%verboseLevel > 2) THEN
  1807:                       WRITE (*, *) "Difference is ", diff_shape_coef
  1808:                       WRITE (*, *) ""
  1809:                   END IF
  1810:               END IF
  1811:            
  1812:               dtype_check_status%numTotal = dtype_check_status%numTotal + 1
  1813: *======>      IF (ALL(var%lambda_bounds == kgenref_var%lambda_bounds)) THEN
  1814:                   dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
  1815:                   IF (check_status%verboseLevel > 2) THEN
  1816:                       WRITE (*, *) trim(adjustl(varname)), "%lambda_bounds is IDENTICAL."
  1817:                   END IF
  1818:                   check_result = CHECK_IDENTICAL
  1819:               ELSE
  1820:                   ALLOCATE (buf1_lambda_bounds(SIZE(var%lambda_bounds,dim=1)))
  1821:                   ALLOCATE (buf2_lambda_bounds(SIZE(var%lambda_bounds,dim=1)))
  1822: +======>          n_lambda_bounds = COUNT(var%lambda_bounds /= kgenref_var%lambda_bounds)
  1823: +======>          WHERE ( ABS(kgenref_var%lambda_bounds) > kgen_minvalue )
  1824: |                     buf1_lambda_bounds = ((var%lambda_bounds-kgenref_var%lambda_bounds)/kgenref_var%lambda_bounds)**2
  1825: |                     buf2_lambda_bounds = (var%lambda_bounds-kgenref_var%lambda_bounds)**2
  1826: |                 ELSEWHERE
  1827: |                     buf1_lambda_bounds = (var%lambda_bounds-kgenref_var%lambda_bounds)**2
  1828: |                     buf2_lambda_bounds = buf1_lambda_bounds
  1829: +======           END WHERE
  1830:                   nrmsdiff_lambda_bounds = SQRT(SUM(buf1_lambda_bounds)/REAL(n_lambda_bounds))
  1831: *======>          rmsdiff_lambda_bounds = SQRT(SUM(buf2_lambda_bounds)/REAL(n_lambda_bounds))
  1832: *======>          IF (nrmsdiff_lambda_bounds > kgen_tolerance) THEN
  1833:                       dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
  1834:                       IF (check_status%verboseLevel > 1) THEN
  1835:                           WRITE (*, *) trim(adjustl(varname)), "%lambda_bounds is NOT IDENTICAL(out of tolerance)."
  1836:                       END IF
  1837:                       check_result = CHECK_OUT_TOL
  1838:                   ELSE
  1839:                       dtype_check_status%numInTol = dtype_check_status%numInTol + 1
  1840:                       IF (check_status%verboseLevel > 1) THEN
  1841:                           WRITE (*, *) trim(adjustl(varname)), "%lambda_bounds is NOT IDENTICAL(within tolerance)."
  1842:                       END IF
  1843:                       check_result = CHECK_IN_TOL
  1844:                   END IF
  1845:               END IF
  1846:               IF (check_result == CHECK_IDENTICAL) THEN
  1847:                   CONTINUE
  1848:               ELSE IF (check_result == CHECK_OUT_TOL) THEN
  1849:                   IF (check_status%verboseLevel > 2) THEN
  1850:                       WRITE (*, *) count( var%lambda_bounds /= kgenref_var%lambda_bounds), " of ", size( var%lambda_bounds ), " elements &
  1851: +======>              &are different."
  1852: *======>              WRITE (*, *) "Average - kernel ", sum(var%lambda_bounds)/real(size(var%lambda_bounds))
  1853: *======>              WRITE (*, *) "Average - reference ", sum(kgenref_var%lambda_bounds)/real(size(kgenref_var%lambda_bounds))
  1854:                       WRITE (*, *) "RMS of difference is ", rmsdiff_lambda_bounds
  1855:                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lambda_bounds
  1856:                       WRITE (*, *) ""
  1857:                   END IF
  1858:               ELSE IF (check_result == CHECK_IN_TOL) THEN
  1859:                   IF (check_status%verboseLevel > 2) THEN
  1860: +======>              WRITE (*, *) count( var%lambda_bounds /= kgenref_var%lambda_bounds), " of ", size( var%lambda_bounds ), " elements &
  1861:                       &are different."
  1862: *======>              WRITE (*, *) "Average - kernel ", sum(var%lambda_bounds)/real(size(var%lambda_bounds))
  1863: *======>              WRITE (*, *) "Average - reference ", sum(kgenref_var%lambda_bounds)/real(size(kgenref_var%lambda_bounds))
  1864:                       WRITE (*, *) "RMS of difference is ", rmsdiff_lambda_bounds
  1865:                       WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff_lambda_bounds
  1866:                       WRITE (*, *) ""
  1867:                   END IF
  1868:               END IF
  1869:            
  1870:               dtype_check_status%numTotal = dtype_check_status%numTotal + 1
  1871:               IF (var%min_mean_mass == kgenref_var%min_mean_mass) THEN
  1872:                   dtype_check_status%numIdentical = dtype_check_status%numIdentical + 1
  1873:                   IF (check_status%verboseLevel > 2) THEN
  1874:                       WRITE (*, *) trim(adjustl(varname)), "%min_mean_mass is IDENTICAL."
  1875:                   END IF
  1876:                   check_result = CHECK_IDENTICAL
  1877:               ELSE
  1878:                   diff_min_mean_mass = ABS(var%min_mean_mass - kgenref_var%min_mean_mass)
  1879:                   IF (diff_min_mean_mass <= kgen_tolerance) THEN
  1880:                       dtype_check_status%numInTol = dtype_check_status%numInTol + 1
  1881:                       IF (check_status%verboseLevel > 1) THEN
  1882:                           WRITE (*, *) trim(adjustl(varname)), "%min_mean_mass is NOT IDENTICAL(within tolerance)."
  1883:                       END IF
  1884:                       check_result = CHECK_IN_TOL
  1885:                   ELSE
  1886:                       dtype_check_status%numOutTol = dtype_check_status%numOutTol + 1
  1887:                       IF (check_status%verboseLevel > 1) THEN
  1888:                           WRITE (*, *) trim(adjustl(varname)), "%min_mean_mass is NOT IDENTICAL(out of tolerance)."
  1889:                       END IF
  1890:                       check_result = CHECK_OUT_TOL
  1891:                   END IF
  1892:               END IF
  1893:               IF (check_result == CHECK_IDENTICAL) THEN
  1894:                   CONTINUE
  1895:               ELSE IF (check_result == CHECK_OUT_TOL) THEN
  1896:                   IF (check_status%verboseLevel > 2) THEN
  1897:                       WRITE (*, *) "Difference is ", diff_min_mean_mass
  1898:                       WRITE (*, *) ""
  1899:                   END IF
  1900:               ELSE IF (check_result == CHECK_IN_TOL) THEN
  1901:                   IF (check_status%verboseLevel > 2) THEN
  1902:                       WRITE (*, *) "Difference is ", diff_min_mean_mass
  1903:                       WRITE (*, *) ""
  1904:                   END IF
  1905:               END IF
  1906:            
  1907:               IF (dtype_check_status%numTotal == dtype_check_status%numIdentical) THEN
  1908:                   check_status%numIdentical = check_status%numIdentical + 1
  1909:               ELSE IF (dtype_check_status%numOutTol > 0) THEN
  1910:                   check_status%numOutTol = check_status%numOutTol + 1
  1911:               ELSE IF (dtype_check_status%numInTol > 0) THEN
  1912:                   check_status%numInTol = check_status%numInTol + 1
  1913:               END IF
  1914:           END SUBROUTINE kv_micro_mg_utils_mghydrometeorprops


