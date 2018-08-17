NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_TO_QSAT_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_TO_QSAT_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   140:           subroutine  wv_sat_svp_to_qsat_r8(es, p, qs)
   141:           
   142:             real(r8), intent(in)  :: es  ! SVP
   143:             real(r8), intent(in)  :: p   ! Current pressure.
   144:             real(r8), intent(out) :: qs
   145:             ! If pressure is less than SVP, set qs to maximum of 1.
   146:           
   147:             if ( (p - es) <= 0._r8 ) then
   148:                qs = 1.0_r8
   149:             else
   150:                qs = epsilo*es / (p - omeps*es)
   151:             end if
   152:           
   153:           end subroutine wv_sat_svp_to_qsat_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_TO_QSAT_V8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   163: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_TO_QSAT_V8
FORMAT LIST

 LINE   LOOP      STATEMENT

   155:           subroutine  wv_sat_svp_to_qsat_v8(es, p, qs, vlen)
   156:           
   157:             integer,  intent(in) :: vlen
   158:             real(r8), intent(in)  :: es(vlen)  ! SVP
   159:             real(r8), intent(in)  :: p(vlen)   ! Current pressure.
   160:             real(r8), intent(out) :: qs(vlen)
   161:             integer :: i
   162:             ! If pressure is less than SVP, set qs to maximum of 1.
   163: V------>    do i=1,vlen
   164: |              if ( (p(i) - es(i)) <= 0._r8 ) then
   165: |                 qs(i) = 1.0_r8
   166: |              else
   167: |                 qs(i) = epsilo*es(i) / (p(i) - omeps*es(i))
   168: |              end if
   169: V------     enddo
   170:           
   171:           end subroutine wv_sat_svp_to_qsat_v8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_QSAT_WATER_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   189: inl(1222): Inlined: WV_SAT_METHODS::WV_SAT_SVP_WATER_R8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_QSAT_WATER_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   173:           subroutine wv_sat_qsat_water_r8(t, p, es, qs, idx)
   174:             !------------------------------------------------------------------!
   175:             ! Purpose:                                                         !
   176:             !   Calculate SVP over water at a given temperature, and then      !
   177:             !   calculate and return saturation specific humidity.             !
   178:             !------------------------------------------------------------------!
   179:             ! Inputs
   180:           
   181:             real(r8), intent(in) :: t    ! Temperature
   182:             real(r8), intent(in) :: p    ! Pressure
   183:             ! Outputs
   184:             real(r8), intent(out) :: es  ! Saturation vapor pressure
   185:             real(r8), intent(out) :: qs  ! Saturation specific humidity
   186:           
   187:             integer,  intent(in), optional :: idx ! Scheme index
   188:           
   189:         I   call wv_sat_svp_water(t, es, idx)
   190:           
   191:             call wv_sat_svp_to_qsat(es, p, qs)
   192:             ! Ensures returned es is consistent with limiters on qs.
   193:           
   194:             es = min(es, p)
   195:           
   196:           end subroutine wv_sat_qsat_water_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_QSAT_WATER_V8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   216: inl(1222): Inlined: WV_SAT_METHODS::WV_SAT_SVP_WATER_V8
   217: inl(1222): Inlined: WV_SAT_METHODS::WV_SAT_SVP_TO_QSAT_V8
   218: opt(1112): Loop fused with previous loop.: I


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_QSAT_WATER_V8
FORMAT LIST

 LINE   LOOP      STATEMENT

   198:           subroutine wv_sat_qsat_water_v8(t, p, es, qs, vlen, idx)
   199:             !------------------------------------------------------------------!
   200:             ! Purpose:                                                         !
   201:             !   Calculate SVP over water at a given temperature, and then      !
   202:             !   calculate and return saturation specific humidity.             !
   203:             !------------------------------------------------------------------!
   204:             ! Inputs
   205:           
   206:             integer,  intent(in) :: vlen
   207:             real(r8), intent(in) :: t(vlen)    ! Temperature
   208:             real(r8), intent(in) :: p(vlen)    ! Pressure
   209:             ! Outputs
   210:             real(r8), intent(out) :: es(vlen)  ! Saturation vapor pressure
   211:             real(r8), intent(out) :: qs(vlen)  ! Saturation specific humidity
   212:           
   213:             integer,  intent(in), optional :: idx ! Scheme index
   214:             integer :: i
   215:           
   216:         I   call wv_sat_svp_water(t, es, vlen, idx)
   217:         I   call wv_sat_svp_to_qsat(es, p, qs, vlen)
   218: f------>    do i=1,vlen
   219: |              ! Ensures returned es is consistent with limiters on qs.
   220: |              es(i) = min(es(i), p(i))
   221: f------     enddo
   222:           
   223:           end subroutine wv_sat_qsat_water_v8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_QSAT_ICE_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   241: inl(1222): Inlined: WV_SAT_METHODS::WV_SAT_SVP_ICE_R8
   243: inl(1235): Inline halted: code size exceeds limit.: WV_SAT_METHODS::WV_SAT_QSAT_ICE_R8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_QSAT_ICE_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   225:           subroutine wv_sat_qsat_ice_r8(t, p, es, qs, idx)
   226:             !------------------------------------------------------------------!
   227:             ! Purpose:                                                         !
   228:             !   Calculate SVP over ice at a given temperature, and then        !
   229:             !   calculate and return saturation specific humidity.             !
   230:             !------------------------------------------------------------------!
   231:             ! Inputs
   232:           
   233:             real(r8), intent(in) :: t    ! Temperature
   234:             real(r8), intent(in) :: p    ! Pressure
   235:             ! Outputs
   236:             real(r8), intent(out) :: es  ! Saturation vapor pressure
   237:             real(r8), intent(out) :: qs  ! Saturation specific humidity
   238:           
   239:             integer,  intent(in), optional :: idx ! Scheme index
   240:           
   241:         I   call wv_sat_svp_ice(t, es, idx)
   242:           
   243:             call wv_sat_svp_to_qsat(es, p, qs)
   244:             ! Ensures returned es is consistent with limiters on qs.
   245:           
   246:             es = min(es, p)
   247:           
   248:           end subroutine wv_sat_qsat_ice_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_QSAT_ICE_V8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   268: inl(1222): Inlined: WV_SAT_METHODS::WV_SAT_SVP_ICE_V8
   269: inl(1222): Inlined: WV_SAT_METHODS::WV_SAT_SVP_TO_QSAT_V8
   270: opt(1112): Loop fused with previous loop.: I


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_QSAT_ICE_V8
FORMAT LIST

 LINE   LOOP      STATEMENT

   250:           subroutine wv_sat_qsat_ice_v8(t, p, es, qs, vlen, idx)
   251:             !------------------------------------------------------------------!
   252:             ! Purpose:                                                         !
   253:             !   Calculate SVP over ice at a given temperature, and then        !
   254:             !   calculate and return saturation specific humidity.             !
   255:             !------------------------------------------------------------------!
   256:             ! Inputs
   257:           
   258:             integer,  intent(in) :: vlen
   259:             real(r8), intent(in) :: t(vlen)    ! Temperature
   260:             real(r8), intent(in) :: p(vlen)    ! Pressure
   261:             ! Outputs
   262:             real(r8), intent(out) :: es(vlen)  ! Saturation vapor pressure
   263:             real(r8), intent(out) :: qs(vlen)  ! Saturation specific humidity
   264:           
   265:             integer,  intent(in), optional :: idx ! Scheme index
   266:             integer :: i
   267:           
   268:         I   call wv_sat_svp_ice(t, es, vlen, idx)
   269:         I   call wv_sat_svp_to_qsat(es, p, qs, vlen)
   270: f------>    do i=1,vlen
   271: |              ! Ensures returned es is consistent with limiters on qs.
   272: |              es(i) = min(es(i), p(i))
   273: f------     enddo
   274:           
   275:           end subroutine wv_sat_qsat_ice_v8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_WATER_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   298: inl(1222): Inlined: WV_SAT_METHODS::GOFFGRATCH_SVP_WATER_R8
   300: inl(1222): Inlined: WV_SAT_METHODS::MURPHYKOOP_SVP_WATER
   302: inl(1222): Inlined: WV_SAT_METHODS::OLDGOFFGRATCH_SVP_WATER
   304: inl(1222): Inlined: WV_SAT_METHODS::BOLTON_SVP_WATER
   304: inl(1235): Inline halted: code size exceeds limit.: WV_SAT_METHODS::WV_SAT_QSAT_WATER_R8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_WATER_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   283:           subroutine  wv_sat_svp_water_r8(t, es, idx)
   284:             real(r8), intent(in) :: t
   285:             integer,  intent(in), optional :: idx
   286:             real(r8), intent(out) :: es
   287:           
   288:             integer :: use_idx
   289:           
   290:             if (present(idx)) then
   291:                use_idx = idx
   292:             else
   293:                use_idx = default_idx
   294:             end if
   295:           
   296:             select case (use_idx)
   297:             case(GoffGratch_idx)
   298:         I      call GoffGratch_svp_water(t,es)
   299:             case(MurphyKoop_idx)
   300:         I      call MurphyKoop_svp_water(t,es)
   301:             case(OldGoffGratch_idx)
   302:         I      call OldGoffGratch_svp_water(t,es)
   303:             case(Bolton_idx)
   304:         I      call Bolton_svp_water(t,es)
   305:             end select
   306:           
   307:           end subroutine wv_sat_svp_water_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_WATER_V8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   325: inl(1222): Inlined: WV_SAT_METHODS::GOFFGRATCH_SVP_WATER_V8
   327: vec( 101): Vectorized loop.
   328: inl(1222): Inlined: WV_SAT_METHODS::MURPHYKOOP_SVP_WATER
   331: vec( 101): Vectorized loop.
   332: inl(1222): Inlined: WV_SAT_METHODS::OLDGOFFGRATCH_SVP_WATER
   335: vec( 101): Vectorized loop.
   336: inl(1222): Inlined: WV_SAT_METHODS::BOLTON_SVP_WATER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_WATER_V8
FORMAT LIST

 LINE   LOOP      STATEMENT

   309:           subroutine  wv_sat_svp_water_v8(t, es, vlen, idx)
   310:             integer,  intent(in) :: vlen
   311:             real(r8), intent(in) :: t(vlen)
   312:             integer,  intent(in), optional :: idx
   313:             real(r8), intent(out) :: es(vlen)
   314:             integer :: i
   315:             integer :: use_idx
   316:           
   317:             if (present(idx)) then
   318:                use_idx = idx
   319:             else
   320:                use_idx = default_idx
   321:             end if
   322:           
   323:             select case (use_idx)
   324:             case(GoffGratch_idx)
   325:         I      call GoffGratch_svp_water(t,es,vlen)
   326:             case(MurphyKoop_idx)
   327: V------>       do i=1,vlen
   328: |       I         call MurphyKoop_svp_water(t(i),es(i))
   329: V------        enddo
   330:             case(OldGoffGratch_idx)
   331: V------>       do i=1,vlen
   332: |       I         call OldGoffGratch_svp_water(t(i),es(i))
   333: V------        enddo
   334:             case(Bolton_idx)
   335: V------>       do i=1,vlen
   336: |       I         call Bolton_svp_water(t(i),es(i))
   337: V------        enddo
   338:             end select
   339:           
   340:           end subroutine wv_sat_svp_water_v8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_ICE_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   357: inl(1222): Inlined: WV_SAT_METHODS::GOFFGRATCH_SVP_ICE_R8
   359: inl(1222): Inlined: WV_SAT_METHODS::MURPHYKOOP_SVP_ICE
   361: inl(1222): Inlined: WV_SAT_METHODS::OLDGOFFGRATCH_SVP_ICE
   363: inl(1222): Inlined: WV_SAT_METHODS::BOLTON_SVP_WATER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_ICE_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   342:           subroutine wv_sat_svp_ice_r8(t, es, idx)
   343:             real(r8), intent(in) :: t
   344:             integer,  intent(in), optional :: idx
   345:             real(r8), intent(out) :: es
   346:           
   347:             integer :: use_idx
   348:           
   349:             if (present(idx)) then
   350:                use_idx = idx
   351:             else
   352:                use_idx = default_idx
   353:             end if
   354:           
   355:             select case (use_idx)
   356:             case(GoffGratch_idx)
   357:         I      call GoffGratch_svp_ice(t,es)
   358:             case(MurphyKoop_idx)
   359:         I      call MurphyKoop_svp_ice(t,es)
   360:             case(OldGoffGratch_idx)
   361:         I      call OldGoffGratch_svp_ice(t,es)
   362:             case(Bolton_idx)
   363:         I      call Bolton_svp_water(t,es)
   364:             end select
   365:           
   366:           end subroutine wv_sat_svp_ice_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_ICE_V8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   385: inl(1222): Inlined: WV_SAT_METHODS::GOFFGRATCH_SVP_ICE_V8
   387: vec( 101): Vectorized loop.
   388: inl(1222): Inlined: WV_SAT_METHODS::MURPHYKOOP_SVP_ICE
   391: vec( 101): Vectorized loop.
   392: inl(1222): Inlined: WV_SAT_METHODS::OLDGOFFGRATCH_SVP_ICE
   395: vec( 101): Vectorized loop.
   396: inl(1222): Inlined: WV_SAT_METHODS::BOLTON_SVP_WATER


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::WV_SAT_SVP_ICE_V8
FORMAT LIST

 LINE   LOOP      STATEMENT

   368:           subroutine wv_sat_svp_ice_v8(t, es, vlen, idx)
   369:             integer,  intent(in) :: vlen
   370:             real(r8), intent(in) :: t(vlen)
   371:             integer,  intent(in), optional :: idx
   372:             real(r8), intent(out) :: es(vlen)
   373:             integer :: i
   374:           
   375:             integer :: use_idx
   376:           
   377:             if (present(idx)) then
   378:                use_idx = idx
   379:             else
   380:                use_idx = default_idx
   381:             end if
   382:           
   383:             select case (use_idx)
   384:             case(GoffGratch_idx)
   385:         I      call GoffGratch_svp_ice(t,es,vlen)
   386:             case(MurphyKoop_idx)
   387: V------>       do i=1,vlen
   388: |       I         call MurphyKoop_svp_ice(t(i),es(i))
   389: V------        enddo
   390:             case(OldGoffGratch_idx)
   391: V------>       do i=1,vlen
   392: |       I         call OldGoffGratch_svp_ice(t(i),es(i))
   393: V------        enddo
   394:             case(Bolton_idx)
   395: V------>       do i=1,vlen
   396: |       I         call Bolton_svp_water(t(i),es(i))
   397: V------        enddo
   398:             end select
   399:           
   400:           end subroutine wv_sat_svp_ice_v8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::GOFFGRATCH_SVP_WATER_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::GOFFGRATCH_SVP_WATER_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   408:           subroutine GoffGratch_svp_water_r8(t, es)
   409:             real(r8), intent(in)  :: t  ! Temperature in Kelvin
   410:             real(r8), intent(out) :: es             ! SVP in Pa
   411:             ! uncertain below -70 C
   412:           
   413:             es = 10._r8**(-7.90298_r8*(tboil/t-1._r8)+ &
   414:                  5.02808_r8*log10(tboil/t)- &
   415:                  1.3816e-7_r8*(10._r8**(11.344_r8*(1._r8-t/tboil))-1._r8)+ &
   416:                  8.1328e-3_r8*(10._r8**(-3.49149_r8*(tboil/t-1._r8))-1._r8)+ &
   417:                  log10(1013.246_r8))*100._r8
   418:           
   419:           end subroutine GoffGratch_svp_water_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::GOFFGRATCH_SVP_WATER_V8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   428: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::GOFFGRATCH_SVP_WATER_V8
FORMAT LIST

 LINE   LOOP      STATEMENT

   421:           subroutine GoffGratch_svp_water_v8(t, es, vlen)
   422:             integer :: vlen
   423:             real(r8), intent(in)  :: t(vlen)  ! Temperature in Kelvin
   424:             real(r8), intent(out) :: es(vlen) ! SVP in Pa
   425:             integer :: i
   426:             ! uncertain below -70 C
   427:           
   428: V------>    do i=1,vlen
   429: |              es(i) = 10._r8**(-7.90298_r8*(tboil/t(i)-1._r8)+ &
   430: |                5.02808_r8*log10(tboil/t(i))- &
   431: |                1.3816e-7_r8*(10._r8**(11.344_r8*(1._r8-t(i)/tboil))-1._r8)+ &
   432: |                8.1328e-3_r8*(10._r8**(-3.49149_r8*(tboil/t(i)-1._r8))-1._r8)+ &
   433: |                log10(1013.246_r8))*100._r8
   434: V------     enddo
   435:           
   436:           end subroutine GoffGratch_svp_water_v8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::GOFFGRATCH_SVP_ICE_R8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::GOFFGRATCH_SVP_ICE_R8
FORMAT LIST

 LINE   LOOP      STATEMENT

   438:           subroutine GoffGratch_svp_ice_r8(t, es)
   439:             real(r8), intent(in)  :: t  ! Temperature in Kelvin
   440:             real(r8), intent(out) :: es             ! SVP in Pa
   441:             ! good down to -100 C
   442:           
   443:             es = 10._r8**(-9.09718_r8*(h2otrip/t-1._r8)-3.56654_r8* &
   444:                  log10(h2otrip/t)+0.876793_r8*(1._r8-t/h2otrip)+ &
   445:                  log10(6.1071_r8))*100._r8
   446:           
   447:           end subroutine GoffGratch_svp_ice_r8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::GOFFGRATCH_SVP_ICE_V8
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   457: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::GOFFGRATCH_SVP_ICE_V8
FORMAT LIST

 LINE   LOOP      STATEMENT

   449:           subroutine GoffGratch_svp_ice_v8(t, es, vlen)
   450:             integer :: vlen
   451:             real(r8), intent(in)  :: t(vlen)  ! Temperature in Kelvin
   452:             real(r8), intent(out) :: es(vlen)             ! SVP in Pa
   453:             integer :: i
   454:             ! good down to -100 C
   455:           
   456:            
   457: V------>    do i=1,vlen
   458: |           es(i) = 10._r8**(-9.09718_r8*(h2otrip/t(i)-1._r8)-3.56654_r8* &
   459: |                log10(h2otrip/t(i))+0.876793_r8*(1._r8-t(i)/h2otrip)+ &
   460: |                log10(6.1071_r8))*100._r8
   461: V------     enddo
   462:           
   463:           end subroutine GoffGratch_svp_ice_v8


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::MURPHYKOOP_SVP_WATER
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::MURPHYKOOP_SVP_WATER
FORMAT LIST

 LINE   LOOP      STATEMENT

   468:           subroutine MurphyKoop_svp_water(t, es)
   469:             real(r8), intent(in)  :: t  ! Temperature in Kelvin
   470:             real(r8), intent(out) :: es             ! SVP in Pa
   471:             ! (good for 123 < T < 332 K)
   472:           
   473:             es = exp(54.842763_r8 - (6763.22_r8 / t) - (4.210_r8 * log(t)) + &
   474:                  (0.000367_r8 * t) + (tanh(0.0415_r8 * (t - 218.8_r8)) * &
   475:                  (53.878_r8 - (1331.22_r8 / t) - (9.44523_r8 * log(t)) + &
   476:                  0.014025_r8 * t)))
   477:           
   478:           end subroutine MurphyKoop_svp_water


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::MURPHYKOOP_SVP_ICE
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::MURPHYKOOP_SVP_ICE
FORMAT LIST

 LINE   LOOP      STATEMENT

   480:           subroutine MurphyKoop_svp_ice(t, es)
   481:             real(r8), intent(in) :: t  ! Temperature in Kelvin
   482:             real(r8) :: es             ! SVP in Pa
   483:             ! (good down to 110 K)
   484:           
   485:             es = exp(9.550426_r8 - (5723.265_r8 / t) + (3.53068_r8 * log(t)) &
   486:                  - (0.00728332_r8 * t))
   487:           
   488:           end subroutine MurphyKoop_svp_ice


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::OLDGOFFGRATCH_SVP_WATER
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::OLDGOFFGRATCH_SVP_WATER
FORMAT LIST

 LINE   LOOP      STATEMENT

   501:           subroutine OldGoffGratch_svp_water(t,es)
   502:             real(r8), intent(in)  :: t
   503:             real(r8), intent(out) :: es
   504:             real(r8) :: ps, e1, e2, f1, f2, f3, f4, f5, f
   505:           
   506:             ps = 1013.246_r8
   507:             e1 = 11.344_r8*(1.0_r8 - t/tboil)
   508:             e2 = -3.49149_r8*(tboil/t - 1.0_r8)
   509:             f1 = -7.90298_r8*(tboil/t - 1.0_r8)
   510:             f2 = 5.02808_r8*log10(tboil/t)
   511:             f3 = -1.3816_r8*(10.0_r8**e1 - 1.0_r8)/10000000.0_r8
   512:             f4 = 8.1328_r8*(10.0_r8**e2 - 1.0_r8)/1000.0_r8
   513:             f5 = log10(ps)
   514:             f  = f1 + f2 + f3 + f4 + f5
   515:           
   516:             es = (10.0_r8**f)*100.0_r8
   517:            
   518:           end subroutine OldGoffGratch_svp_water


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::OLDGOFFGRATCH_SVP_ICE
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::OLDGOFFGRATCH_SVP_ICE
FORMAT LIST

 LINE   LOOP      STATEMENT

   520:           subroutine OldGoffGratch_svp_ice(t,es)
   521:             real(r8), intent(in) :: t
   522:             real(r8), intent(out) :: es
   523:             real(r8) :: term1, term2, term3
   524:           
   525:             term1 = 2.01889049_r8/(tmelt/t)
   526:             term2 = 3.56654_r8*log(tmelt/t)
   527:             term3 = 20.947031_r8*(tmelt/t)
   528:           
   529:             es = 575.185606e10_r8*exp(-(term1 + term2 + term3))
   530:            
   531:           end subroutine OldGoffGratch_svp_ice


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::BOLTON_SVP_WATER
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::BOLTON_SVP_WATER
FORMAT LIST

 LINE   LOOP      STATEMENT

   541:           subroutine Bolton_svp_water(t, es)
   542:             real(r8),parameter :: c1 = 611.2_r8
   543:             real(r8),parameter :: c2 = 17.67_r8
   544:             real(r8),parameter :: c3 = 243.5_r8
   545:           
   546:             real(r8), intent(in)  :: t  ! Temperature in Kelvin
   547:             real(r8), intent(out) :: es             ! SVP in Pa
   548:           
   549:             es = c1*exp( (c2*(t - tmelt))/((t - tmelt)+c3) )
   550:           
   551:           end subroutine Bolton_svp_water


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::KR_EXTERNS_IN_WV_SAT_METHODS
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::KR_EXTERNS_IN_WV_SAT_METHODS
FORMAT LIST

 LINE   LOOP      STATEMENT

   554:           SUBROUTINE kr_externs_in_wv_sat_methods(kgen_unit)
   555:               INTEGER, INTENT(IN) :: kgen_unit
   556:               LOGICAL :: kgen_istrue
   557:               REAL(KIND=8) :: kgen_array_sum
   558:            
   559:               READ (UNIT = kgen_unit) tmelt
   560:               READ (UNIT = kgen_unit) h2otrip
   561:               READ (UNIT = kgen_unit) tboil
   562:               READ (UNIT = kgen_unit) epsilo
   563:               READ (UNIT = kgen_unit) omeps
   564:               READ (UNIT = kgen_unit) default_idx
   565:           END SUBROUTINE kr_externs_in_wv_sat_methods


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::KR_EXTERNS_OUT_WV_SAT_METHODS
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:45:08 2018
FILE NAME: wv_sat_methods.F90

PROCEDURE NAME: WV_SAT_METHODS::KR_EXTERNS_OUT_WV_SAT_METHODS
FORMAT LIST

 LINE   LOOP      STATEMENT

   568:           SUBROUTINE kr_externs_out_wv_sat_methods(kgen_unit)
   569:               INTEGER, INTENT(IN) :: kgen_unit
   570:            
   571:               LOGICAL :: kgen_istrue
   572:               REAL(KIND=8) :: kgen_array_sum
   573:           END SUBROUTINE kr_externs_out_wv_sat_methods


