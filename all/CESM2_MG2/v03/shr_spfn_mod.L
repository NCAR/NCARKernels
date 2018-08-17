NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:05 2018
FILE NAME: shr_spfn_mod.F90

PROCEDURE NAME: SHR_SPFN_MOD::SHR_SPFN_GAMMA_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   103: inl(1214): Expansion routine is too big for automatic expansion.: SHR_SPFN_MOD::SHR_SPFN_GAMMA_NONINTRINSIC_R8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:05 2018
FILE NAME: shr_spfn_mod.F90

PROCEDURE NAME: SHR_SPFN_MOD::SHR_SPFN_GAMMA_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

    98:             elemental function shr_spfn_gamma_r8(x) result(res)
    99:               real(r8), intent(in) :: x
   100:               real(r8) :: res
   101:               ! No intrinsic
   102:           
   103:               res = shr_spfn_gamma_nonintrinsic_r8(x)
   104:           
   105:             end function shr_spfn_gamma_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:05 2018
FILE NAME: shr_spfn_mod.F90

PROCEDURE NAME: SHR_SPFN_MOD::SHR_SPFN_GAMMA_NONINTRINSIC_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   428: vec( 101): Vectorized loop.
   429: opt(1019): Feedback of scalar value from one loop pass to another.: XNUM
   429: vec( 126): Idiom detected.: ITERATION
   430: opt(1019): Feedback of scalar value from one loop pass to another.: XDEN
   430: vec( 126): Idiom detected.: ITERATION
   442: vec( 101): Vectorized loop.
   443: vec( 126): Idiom detected.: PRODUCT
   455: opt(1512): Loop unrolled.: I
   455: opt(1019): Feedback of scalar value from one loop pass to another.: SUM


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:05 2018
FILE NAME: shr_spfn_mod.F90

PROCEDURE NAME: SHR_SPFN_MOD::SHR_SPFN_GAMMA_NONINTRINSIC_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   243:             pure function shr_spfn_gamma_nonintrinsic_r8(X) result(gamma)
   244:               !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   245:               ! 7 Feb 2013 -- S. Santos
   246:               ! The following comments are from the original version. Changes have
   247:               ! been made to update syntax and allow inclusion into this module.
   248:               !----------------------------------------------------------------------
   249:               ! THIS ROUTINE CALCULATES THE GAMMA FUNCTION FOR A REAL ARGUMENT X.
   250:               !   COMPUTATION IS BASED ON AN ALGORITHM OUTLINED IN REFERENCE 1.
   251:               !   THE PROGRAM USES RATIONAL FUNCTIONS THAT APPROXIMATE THE GAMMA
   252:               !   FUNCTION TO AT LEAST 20 SIGNIFICANT DECIMAL DIGITS.  COEFFICIENTS
   253:               !   FOR THE APPROXIMATION OVER THE INTERVAL (1,2) ARE UNPUBLISHED.
   254:               !   THOSE FOR THE APPROXIMATION FOR X .GE. 12 ARE FROM REFERENCE 2.
   255:               !   THE ACCURACY ACHIEVED DEPENDS ON THE ARITHMETIC SYSTEM, THE
   256:               !   COMPILER, THE INTRINSIC FUNCTIONS, AND PROPER SELECTION OF THE
   257:               !   MACHINE-DEPENDENT CONSTANTS.
   258:               !*******************************************************************
   259:               !*******************************************************************
   260:               ! EXPLANATION OF MACHINE-DEPENDENT CONSTANTS
   261:               ! BETA   - RADIX FOR THE FLOATING-POINT REPRESENTATION
   262:               ! MAXEXP - THE SMALLEST POSITIVE POWER OF BETA THAT OVERFLOWS
   263:               ! XBIG   - THE LARGEST ARGUMENT FOR WHICH GAMMA(X) IS REPRESENTABLE
   264:               !          IN THE MACHINE, I.E., THE SOLUTION TO THE EQUATION
   265:               !                  GAMMA(XBIG) = BETA**MAXEXP
   266:               ! XINF   - THE LARGEST MACHINE REPRESENTABLE FLOATING-POINT NUMBER;
   267:               !          APPROXIMATELY BETA**MAXEXP
   268:               ! EPS    - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
   269:               !          1.0+EPS .GT. 1.0
   270:               ! XMININ - THE SMALLEST POSITIVE FLOATING-POINT NUMBER SUCH THAT
   271:               !          1/XMININ IS MACHINE REPRESENTABLE
   272:               !     APPROXIMATE VALUES FOR SOME IMPORTANT MACHINES ARE:
   273:               !                            BETA       MAXEXP        XBIG
   274:               ! CRAY-1         (S.P.)        2         8191        966.961
   275:               ! CYBER 180/855
   276:               !   UNDER NOS    (S.P.)        2         1070        177.803
   277:               ! IEEE (IBM/XT,
   278:               !   SUN, ETC.)   (S.P.)        2          128        35.040
   279:               ! IEEE (IBM/XT,
   280:               !   SUN, ETC.)   (D.P.)        2         1024        171.624
   281:               ! IBM 3033       (D.P.)       16           63        57.574
   282:               ! VAX D-FORMAT   (D.P.)        2          127        34.844
   283:               ! VAX G-FORMAT   (D.P.)        2         1023        171.489
   284:               !                            XINF         EPS        XMININ
   285:               ! CRAY-1         (S.P.)   5.45E+2465   7.11E-15    1.84E-2466
   286:               ! CYBER 180/855
   287:               !   UNDER NOS    (S.P.)   1.26E+322    3.55E-15    3.14E-294
   288:               ! IEEE (IBM/XT,
   289:               !   SUN, ETC.)   (S.P.)   3.40E+38     1.19E-7     1.18E-38
   290:               ! IEEE (IBM/XT,
   291:               !   SUN, ETC.)   (D.P.)   1.79D+308    2.22D-16    2.23D-308
   292:               ! IBM 3033       (D.P.)   7.23D+75     2.22D-16    1.39D-76
   293:               ! VAX D-FORMAT   (D.P.)   1.70D+38     1.39D-17    5.88D-39
   294:               ! VAX G-FORMAT   (D.P.)   8.98D+307    1.11D-16    1.12D-308
   295:               !*******************************************************************
   296:               !*******************************************************************
   297:               ! ERROR RETURNS
   298:               !  THE PROGRAM RETURNS THE VALUE XINF FOR SINGULARITIES OR
   299:               !     WHEN OVERFLOW WOULD OCCUR.  THE COMPUTATION IS BELIEVED
   300:               !     TO BE FREE OF UNDERFLOW AND OVERFLOW.
   301:               !  INTRINSIC FUNCTIONS REQUIRED ARE:
   302:               !     INT, DBLE, EXP, LOG, REAL, SIN
   303:               ! REFERENCES:  AN OVERVIEW OF SOFTWARE DEVELOPMENT FOR SPECIAL
   304:               !              FUNCTIONS   W. J. CODY, LECTURE NOTES IN MATHEMATICS,
   305:               !              506, NUMERICAL ANALYSIS DUNDEE, 1975, G. A. WATSON
   306:               !              (ED.), SPRINGER VERLAG, BERLIN, 1976.
   307:               !              COMPUTER APPROXIMATIONS, HART, ET. AL., WILEY AND
   308:               !              SONS, NEW YORK, 1968.
   309:               !  LATEST MODIFICATION: OCTOBER 12, 1989
   310:               !  AUTHORS: W. J. CODY AND L. STOLTZ
   311:               !           APPLIED MATHEMATICS DIVISION
   312:               !           ARGONNE NATIONAL LABORATORY
   313:               !           ARGONNE, IL 60439
   314:               !----------------------------------------------------------------------
   315:           
   316:               !
   317:               !
   318:               !
   319:               !
   320:               !
   321:               !
   322:               !
   323:               !
   324:               !
   325:               !
   326:               !
   327:               !
   328:               !
   329:               !
   330:               !
   331:               !
   332:               !
   333:               !
   334:               !
   335:               !
   336:               !
   337:               !
   338:               !
   339:               !
   340:           
   341:               real(r8), intent(in) :: x
   342:               real(r8) :: gamma
   343:               real(r8) :: fact, res, sum, xden, xnum, y, y1, ysq, z
   344:           
   345:               integer :: i, n
   346:               logical :: negative_odd
   347:               ! log(2*pi)/2
   348:           
   349:               real(r8), parameter :: logsqrt2pi = 0.9189385332046727417803297E0_r8
   350:               !----------------------------------------------------------------------
   351:               !  NUMERATOR AND DENOMINATOR COEFFICIENTS FOR RATIONAL MINIMAX
   352:               !     APPROXIMATION OVER (1,2).
   353:               !----------------------------------------------------------------------
   354:           
   355:               real(r8), parameter :: P(8) = &
   356:                    (/-1.71618513886549492533811E+0_r8, 2.47656508055759199108314E+1_r8, &
   357:                    -3.79804256470945635097577E+2_r8, 6.29331155312818442661052E+2_r8, &
   358:                    8.66966202790413211295064E+2_r8,-3.14512729688483675254357E+4_r8, &
   359:                    -3.61444134186911729807069E+4_r8, 6.64561438202405440627855E+4_r8 /)
   360:               real(r8), parameter :: Q(8) = &
   361:                    (/-3.08402300119738975254353E+1_r8, 3.15350626979604161529144E+2_r8, &
   362:                    -1.01515636749021914166146E+3_r8,-3.10777167157231109440444E+3_r8, &
   363:                    2.25381184209801510330112E+4_r8, 4.75584627752788110767815E+3_r8, &
   364:                    -1.34659959864969306392456E+5_r8,-1.15132259675553483497211E+5_r8 /)
   365:               !----------------------------------------------------------------------
   366:               !  COEFFICIENTS FOR MINIMAX APPROXIMATION OVER (12, INF).
   367:               !----------------------------------------------------------------------
   368:               real(r8), parameter :: C(7) = &
   369:                    (/-1.910444077728E-03_r8,          8.4171387781295E-04_r8, &
   370:                    -5.952379913043012E-04_r8,       7.93650793500350248E-04_r8, &
   371:                    -2.777777777777681622553E-03_r8, 8.333333333333333331554247E-02_r8, &
   372:                    5.7083835261E-03_r8 /)
   373:           
   374:               negative_odd = .false.
   375:               fact = 1._r8
   376:               n = 0
   377:               y = x
   378:               if (y <= 0._r8) then
   379:                  !----------------------------------------------------------------------
   380:                  !  ARGUMENT IS NEGATIVE
   381:                  !----------------------------------------------------------------------
   382:                  y = -x
   383:                  y1 = aint(y)
   384:                  res = y - y1
   385:                  if (res /= 0._r8) then
   386:                     negative_odd = (y1 /= aint(y1*0.5_r8)*2._r8)
   387:                     fact = -pi/sin(pi*res)
   388:                     y = y + 1._r8
   389:                  else
   390:                     gamma = xinfr8
   391:                     return
   392:                  end if
   393:               end if
   394:               !----------------------------------------------------------------------
   395:               !  ARGUMENT IS POSITIVE
   396:               !----------------------------------------------------------------------
   397:               if (y < epsr8) then
   398:                  !----------------------------------------------------------------------
   399:                  !  ARGUMENT .LT. EPS
   400:                  !----------------------------------------------------------------------
   401:                  if (y >= xminr8) then
   402:                     res = 1._r8/y
   403:                  else
   404:                     gamma = xinfr8
   405:                     return
   406:                  end if
   407:               elseif (y < 12._r8) then
   408:                  y1 = y
   409:                  if (y < 1._r8) then
   410:                     !----------------------------------------------------------------------
   411:                     !  0.0 .LT. ARGUMENT .LT. 1.0
   412:                     !----------------------------------------------------------------------
   413:                     z = y
   414:                     y = y + 1._r8
   415:                  else
   416:                     !----------------------------------------------------------------------
   417:                     !  1.0 .LT. ARGUMENT .LT. 12.0, REDUCE ARGUMENT IF NECESSARY
   418:                     !----------------------------------------------------------------------
   419:                     n = int(y) - 1
   420:                     y = y - real(n, r8)
   421:                     z = y - 1._r8
   422:                  end if
   423:                  !----------------------------------------------------------------------
   424:                  !  EVALUATE APPROXIMATION FOR 1.0 .LT. ARGUMENT .LT. 2.0
   425:                  !----------------------------------------------------------------------
   426:                  xnum = 0._r8
   427:                  xden = 1._r8
   428: V------>         do i=1,8
   429: |                   xnum = (xnum+P(i))*z
   430: |                   xden = xden*z + Q(i)
   431: V------          end do
   432:                  res = xnum/xden + 1._r8
   433:                  if (y1 < y) then
   434:                     !----------------------------------------------------------------------
   435:                     !  ADJUST RESULT FOR CASE  0.0 .LT. ARGUMENT .LT. 1.0
   436:                     !----------------------------------------------------------------------
   437:                     res = res/y1
   438:                  elseif (y1 > y) then
   439:                     !----------------------------------------------------------------------
   440:                     !  ADJUST RESULT FOR CASE  2.0 .LT. ARGUMENT .LT. 12.0
   441:                     !----------------------------------------------------------------------
   442: V------>            do i = 1,n
   443: |                      res = res*y
   444: |                      y = y + 1._r8
   445: V------             end do
   446:                  end if
   447:               else
   448:                  !----------------------------------------------------------------------
   449:                  !  EVALUATE FOR ARGUMENT .GE. 12.0,
   450:                  !----------------------------------------------------------------------
   451:                  if (y <= xbig_gamma) then
   452:                     ysq = y*y
   453:                     sum = C(7)
   454: +------>            do i=1,6
   455: |                      sum = sum/ysq + C(i)
   456: +------             end do
   457:                     sum = sum/y - y + logsqrt2pi
   458:                     sum = sum + (y-0.5_r8)*log(y)
   459:                     res = exp(sum)
   460:                  else
   461:                     gamma = xinfr8
   462:                     return
   463:                  end if
   464:               end if
   465:               !----------------------------------------------------------------------
   466:               !  FINAL ADJUSTMENTS AND RETURN
   467:               !----------------------------------------------------------------------
   468:               if (negative_odd)  res = -res
   469:               if (fact /= 1._r8) res = fact/res
   470:               gamma = res
   471:               ! ---------- LAST LINE OF GAMMA ----------
   472:             end function shr_spfn_gamma_nonintrinsic_r8


