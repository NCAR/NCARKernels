NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:46:30 2018
FILE NAME: micro_mg2_0.F90

PROCEDURE NAME: MICRO_MG2_0::MICRO_MG_TEND
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

   700: opt(1772): Loop nest fused with following nest(s).
   700: vec( 101): Vectorized loop.
   719: opt(1772): Loop nest fused with following nest(s).
   719: vec( 101): Vectorized loop.
   736: opt(1772): Loop nest fused with following nest(s).
   736: vec( 101): Vectorized loop.
   745: opt(1772): Loop nest fused with following nest(s).
   745: vec( 101): Vectorized loop.
   765: vec( 110): Vectorization obstructive procedure reference.: WV_SAT_METHODS::WV_SAT_QSAT_ICE_V8
   765: vec( 110): Vectorization obstructive procedure reference.: WV_SAT_METHODS::WV_SAT_QSAT_WATER_V8
   767: inl(1212): Source for routine not found.: WV_SAT_METHODS::WV_SAT_QSAT_WATER_V8
   767: opt(1025): Reference to this procedure inhibits optimization.: WV_SAT_METHODS::WV_SAT_QSAT_WATER_V8
   768: inl(1212): Source for routine not found.: WV_SAT_METHODS::WV_SAT_QSAT_ICE_V8
   769: vec( 101): Vectorized loop.
   792: opt(1592): Outer loop unrolled inside inner loop.
   792: vec( 101): Vectorized loop.
   800: opt(1772): Loop nest fused with following nest(s).
   800: vec( 101): Vectorized loop.
   811: opt(1772): Loop nest fused with following nest(s).
   811: vec( 101): Vectorized loop.
   821: opt(1772): Loop nest fused with following nest(s).
   821: vec( 101): Vectorized loop.
   831: opt(1772): Loop nest fused with following nest(s).
   831: vec( 101): Vectorized loop.
   838: opt(1772): Loop nest fused with following nest(s).
   838: vec( 101): Vectorized loop.
   855: opt(1772): Loop nest fused with following nest(s).
   855: vec( 101): Vectorized loop.
   870: opt(1772): Loop nest fused with following nest(s).
   870: vec( 101): Vectorized loop.
   883: opt(1772): Loop nest fused with following nest(s).
   883: vec( 101): Vectorized loop.
   890: vec( 101): Vectorized loop.
   894: opt(1772): Loop nest fused with following nest(s).
   894: vec( 101): Vectorized loop.
   909: opt(1772): Loop nest fused with following nest(s).
   909: vec( 101): Vectorized loop.
   921: opt(1772): Loop nest fused with following nest(s).
   921: vec( 101): Vectorized loop.
   933: opt(1772): Loop nest fused with following nest(s).
   933: vec( 101): Vectorized loop.
   950: opt(1592): Outer loop unrolled inside inner loop.
   950: vec( 101): Vectorized loop.
   951: opt(1592): Outer loop unrolled inside inner loop.
   951: vec( 101): Vectorized loop.
   957: opt(1592): Outer loop unrolled inside inner loop.
   957: vec( 101): Vectorized loop.
   975: opt(1592): Outer loop unrolled inside inner loop.
   975: vec( 101): Vectorized loop.
   980: opt(1592): Outer loop unrolled inside inner loop.
   980: vec( 101): Vectorized loop.
   981: vec( 101): Vectorized loop.
   999: opt(1593): Loop nest collapsed into one loop.: K
   999: vec( 101): Vectorized loop.
  1037: opt(1593): Loop nest collapsed into one loop.: K
  1037: vec( 101): Vectorized loop.
  1074: opt(1593): Loop nest collapsed into one loop.: K
  1074: vec( 101): Vectorized loop.
  1075: opt(1401): Moved invariant if outside of an outer loop.
  1087: opt(1394): Moved invariant if outside of an inner loop.
  1101: opt(1394): Moved invariant if outside of an inner loop.
  1117: opt(1592): Outer loop unrolled inside inner loop.
  1117: vec( 101): Vectorized loop.
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::BERGERON_PROCESS_SNOW
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::HETEROGENEOUS_RAIN_FREEZING
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::ACCRETE_CLOUD_WATER_SNOW
  1119: vec( 118): Unvectorizable data type.
  1119: vec( 110): Vectorization obstructive procedure reference.: WV_SAT_METHODS::WV_SAT_QSAT_ICE_V8
  1119: vec( 110): Vectorization obstructive procedure reference.: WV_SAT_METHODS::WV_SAT_QSAT_WATER_V8
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::ICE_DEPOSITION_SUBLIMATION
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::ACCRETE_CLOUD_ICE_SNOW
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::EVAPORATE_SUBLIMATE_PRECIP
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::ACCRETE_CLOUD_WATER_RAIN
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SB2001V2_ACCRE_CLD_WATER_RAIN
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SELF_COLLECTION_RAIN
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SECONDARY_ICE_PRODUCTION
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::ACCRETE_RAIN_SNOW
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::CONTACT_FREEZING
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::IMMERSION_FREEZING
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SNOW_SELF_AGGREGATION
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::ICE_AUTOCONVERSION
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SB2001V2_LIQ_AUTOCONVERSION
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_VECT
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::KK2000_LIQ_AUTOCONVERSION
  1119: vec( 182): Deallocation obstructs vectorization.
  1119: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_VECT
  1119: vec( 181): Allocation obstructs vectorization.
  1121: opt(1082): Backward transfers inhibit loop optimization.
  1121: vec( 103): Unvectorized loop.
  1121: vec( 108): Unvectorizable loop structure.
  1124: vec( 101): Vectorized loop.
  1129: opt(1082): Backward transfers inhibit loop optimization.
  1129: vec( 103): Unvectorized loop.
  1129: vec( 108): Unvectorizable loop structure.
  1136: vec( 101): Vectorized loop.
  1150: inl(1212): Source for routine not found.: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_VECT
  1159: inl(1212): Source for routine not found.: MICRO_MG_UTILS::KK2000_LIQ_AUTOCONVERSION
  1165: vec( 101): Vectorized loop.
  1166: vec( 101): Vectorized loop.
  1169: vec( 101): Vectorized loop.
  1174: vec( 101): Vectorized loop.
  1182: vec( 101): Vectorized loop.
  1186: inl(1212): Source for routine not found.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_VECT
  1191: inl(1212): Source for routine not found.: MICRO_MG_UTILS::SB2001V2_LIQ_AUTOCONVERSION
  1200: inl(1212): Source for routine not found.: MICRO_MG_UTILS::ICE_AUTOCONVERSION
  1205: vec( 101): Vectorized loop.
  1206: vec( 101): Vectorized loop.
  1212: vec( 101): Vectorized loop.
  1213: vec( 101): Vectorized loop.
  1216: vec( 101): Vectorized loop.
  1220: vec( 101): Vectorized loop.
  1228: vec( 101): Vectorized loop.
  1238: vec( 101): Vectorized loop.
  1242: vec( 101): Vectorized loop.
  1243: vec( 101): Vectorized loop.
  1256: vec( 101): Vectorized loop.
  1260: vec( 101): Vectorized loop.
  1261: vec( 101): Vectorized loop.
  1274: inl(1212): Source for routine not found.: MICRO_MG_UTILS::IMMERSION_FREEZING
  1280: vec( 101): Vectorized loop.
  1289: inl(1212): Source for routine not found.: MICRO_MG_UTILS::CONTACT_FREEZING
  1289: opt(1592): Outer loop unrolled inside inner loop.
  1289: vec( 101): Vectorized loop.
  1293: vec( 101): Vectorized loop.
  1294: vec( 101): Vectorized loop.
  1301: vec( 101): Vectorized loop.
  1304: vec( 101): Vectorized loop.
  1327: vec( 101): Vectorized loop.
  1328: vec( 101): Vectorized loop.
  1329: vec( 101): Vectorized loop.
  1330: vec( 101): Vectorized loop.
  1331: vec( 101): Vectorized loop.
  1332: vec( 101): Vectorized loop.
  1335: inl(1212): Source for routine not found.: MICRO_MG_UTILS::SNOW_SELF_AGGREGATION
  1338: inl(1212): Source for routine not found.: MICRO_MG_UTILS::ACCRETE_CLOUD_WATER_SNOW
  1343: inl(1212): Source for routine not found.: MICRO_MG_UTILS::SECONDARY_ICE_PRODUCTION
  1345: vec( 101): Vectorized loop.
  1346: vec( 101): Vectorized loop.
  1349: inl(1212): Source for routine not found.: MICRO_MG_UTILS::ACCRETE_RAIN_SNOW
  1353: inl(1212): Source for routine not found.: MICRO_MG_UTILS::HETEROGENEOUS_RAIN_FREEZING
  1357: inl(1212): Source for routine not found.: MICRO_MG_UTILS::SB2001V2_ACCRE_CLD_WATER_RAIN
  1360: inl(1212): Source for routine not found.: MICRO_MG_UTILS::ACCRETE_CLOUD_WATER_RAIN
  1364: inl(1212): Source for routine not found.: MICRO_MG_UTILS::SELF_COLLECTION_RAIN
  1367: inl(1212): Source for routine not found.: MICRO_MG_UTILS::ACCRETE_CLOUD_ICE_SNOW
  1370: vec( 101): Vectorized loop.
  1371: vec( 101): Vectorized loop.
  1374: inl(1212): Source for routine not found.: MICRO_MG_UTILS::EVAPORATE_SUBLIMATE_PRECIP
  1380: inl(1212): Source for routine not found.: MICRO_MG_UTILS::BERGERON_PROCESS_SNOW
  1384: vec( 101): Vectorized loop.
  1389: inl(1212): Source for routine not found.: MICRO_MG_UTILS::ICE_DEPOSITION_SUBLIMATION
  1393: vec( 101): Vectorized loop.
  1395: vec( 101): Vectorized loop.
  1405: vec( 101): Vectorized loop.
  1410: vec( 101): Vectorized loop.
  1448: vec( 101): Vectorized loop.
  1476: vec( 101): Vectorized loop.
  1499: opt(1394): Moved invariant if outside of an inner loop.
  1512: vec( 101): Vectorized loop.
  1532: vec( 101): Vectorized loop.
  1548: opt(1112): Loop fused with previous loop.: I
  1568: vec( 101): Vectorized loop.
  1593: vec( 101): Vectorized loop.
  1597: opt(1394): Moved invariant if outside of an inner loop.
  1620: vec( 101): Vectorized loop.
  1636: vec( 101): Vectorized loop.
  1656: opt(1112): Loop fused with previous loop.: I
  1673: vec( 101): Vectorized loop.
  1688: vec( 101): Vectorized loop.
  1705: vec( 101): Vectorized loop.
  1730: vec( 101): Vectorized loop.
  1753: opt(1394): Moved invariant if outside of an inner loop.
  1823: opt(1394): Moved invariant if outside of an inner loop.
  1859: opt(1592): Outer loop unrolled inside inner loop.
  1859: vec( 101): Vectorized loop.
  1860: opt(1592): Outer loop unrolled inside inner loop.
  1860: vec( 101): Vectorized loop.
  1861: opt(1592): Outer loop unrolled inside inner loop.
  1861: vec( 101): Vectorized loop.
  1862: opt(1592): Outer loop unrolled inside inner loop.
  1862: vec( 101): Vectorized loop.
  1875: inl(1222): Inlined: MICRO_MG2_0::CALC_RERCLD
  1891: opt(1592): Outer loop unrolled inside inner loop.
  1891: vec( 101): Vectorized loop.
  1892: opt(1592): Outer loop unrolled inside inner loop.
  1892: vec( 101): Vectorized loop.
  1895: opt(1592): Outer loop unrolled inside inner loop.
  1895: vec( 101): Vectorized loop.
  1896: opt(1592): Outer loop unrolled inside inner loop.
  1896: vec( 101): Vectorized loop.
  1897: opt(1592): Outer loop unrolled inside inner loop.
  1897: vec( 101): Vectorized loop.
  1899: opt(1592): Outer loop unrolled inside inner loop.
  1899: vec( 101): Vectorized loop.
  1900: opt(1592): Outer loop unrolled inside inner loop.
  1900: vec( 101): Vectorized loop.
  1901: opt(1592): Outer loop unrolled inside inner loop.
  1901: vec( 101): Vectorized loop.
  1903: opt(1592): Outer loop unrolled inside inner loop.
  1903: vec( 101): Vectorized loop.
  1904: opt(1592): Outer loop unrolled inside inner loop.
  1904: vec( 101): Vectorized loop.
  1905: opt(1592): Outer loop unrolled inside inner loop.
  1905: vec( 101): Vectorized loop.
  1907: opt(1592): Outer loop unrolled inside inner loop.
  1907: vec( 101): Vectorized loop.
  1908: opt(1592): Outer loop unrolled inside inner loop.
  1908: vec( 101): Vectorized loop.
  1909: opt(1592): Outer loop unrolled inside inner loop.
  1909: vec( 101): Vectorized loop.
  1915: opt(1592): Outer loop unrolled inside inner loop.
  1915: vec( 101): Vectorized loop.
  1916: opt(1592): Outer loop unrolled inside inner loop.
  1916: vec( 101): Vectorized loop.
  1919: opt(1593): Loop nest collapsed into one loop.: K
  1919: vec( 101): Vectorized loop.
  1921: opt(1401): Moved invariant if outside of an outer loop.
  1941: opt(1394): Moved invariant if outside of an inner loop.
  1946: opt(1394): Moved invariant if outside of an inner loop.
  1952: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_VECT
  1952: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_VECT
  1952: vec( 103): Unvectorized loop.
  1956: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_VECT
  1964: opt(1593): Loop nest collapsed into one loop.: K
  1964: vec( 101): Vectorized loop.
  2022: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_VECT
  2022: vec( 103): Unvectorized loop.
  2026: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_VECT
  2032: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2032: vec( 103): Unvectorized loop.
  2050: inl(1212): Source for routine not found.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2050: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2087: opt(1593): Loop nest collapsed into one loop.: K
  2087: vec( 101): Vectorized loop.
  2098: vec( 182): Deallocation obstructs vectorization.
  2098: vec( 181): Allocation obstructs vectorization.
  2099: vec( 126): Idiom detected.: MAX/MIN
  2099: vec( 101): Vectorized loop.
  2107: vec( 103): Unvectorized loop.
  2107: vec( 113): Overhead of loop division is too large.
  2110: vec( 101): Vectorized loop.
  2111: vec( 101): Vectorized loop.
  2113: vec( 101): Vectorized loop.
  2114: vec( 101): Vectorized loop.
  2133: vec( 101): Vectorized loop.
  2170: vec( 101): Vectorized loop.
  2186: vec( 126): Idiom detected.: MAX/MIN
  2186: vec( 101): Vectorized loop.
  2195: vec( 101): Vectorized loop.
  2196: vec( 101): Vectorized loop.
  2213: vec( 101): Vectorized loop.
  2241: vec( 101): Vectorized loop.
  2252: vec( 126): Idiom detected.: MAX/MIN
  2252: vec( 101): Vectorized loop.
  2261: vec( 101): Vectorized loop.
  2262: vec( 101): Vectorized loop.
  2279: vec( 101): Vectorized loop.
  2297: vec( 101): Vectorized loop.
  2308: vec( 126): Idiom detected.: MAX/MIN
  2308: vec( 101): Vectorized loop.
  2315: vec( 103): Unvectorized loop.
  2315: vec( 113): Overhead of loop division is too large.
  2317: vec( 101): Vectorized loop.
  2318: vec( 101): Vectorized loop.
  2335: vec( 101): Vectorized loop.
  2353: vec( 101): Vectorized loop.
  2369: opt(1593): Loop nest collapsed into one loop.: K
  2369: vec( 101): Vectorized loop.
  2370: opt(1401): Moved invariant if outside of an outer loop.
  2382: opt(1394): Moved invariant if outside of an inner loop.
  2387: opt(1394): Moved invariant if outside of an inner loop.
  2404: opt(1593): Loop nest collapsed into one loop.: K
  2404: vec( 101): Vectorized loop.
  2435: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2435: vec( 103): Unvectorized loop.
  2460: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2482: opt(1593): Loop nest collapsed into one loop.: K
  2482: vec( 101): Vectorized loop.
  2521: opt(1593): Loop nest collapsed into one loop.: K
  2521: vec( 101): Vectorized loop.
  2556: vec( 110): Vectorization obstructive procedure reference.: WV_SAT_METHODS::WV_SAT_QSAT_WATER_V8
  2557: vec( 101): Vectorized loop.
  2562: opt(1025): Reference to this procedure inhibits optimization.: WV_SAT_METHODS::WV_SAT_QSAT_WATER_V8
  2564: vec( 101): Vectorized loop.
  2605: opt(1593): Loop nest collapsed into one loop.: K
  2605: vec( 101): Vectorized loop.
  2606: opt(1401): Moved invariant if outside of an outer loop.
  2618: opt(1394): Moved invariant if outside of an inner loop.
  2623: opt(1394): Moved invariant if outside of an inner loop.
  2640: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2640: vec( 103): Unvectorized loop.
  2644: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2665: opt(1592): Outer loop unrolled inside inner loop.: K
  2666: vec( 101): Vectorized loop.
  2679: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_LINE
  2679: vec( 103): Unvectorized loop.
  2696: inl(1212): Source for routine not found.: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_LINE
  2696: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::SIZE_DIST_PARAM_LIQ_LINE
  2734: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2734: vec( 103): Unvectorized loop.
  2740: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2754: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2754: vec( 103): Unvectorized loop.
  2759: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::SIZE_DIST_PARAM_BASIC_LINE
  2774: opt(1593): Loop nest collapsed into one loop.: K
  2774: vec( 101): Vectorized loop.
  2792: opt(1592): Outer loop unrolled inside inner loop.
  2792: vec( 101): Vectorized loop.
  2793: opt(1592): Outer loop unrolled inside inner loop.
  2793: vec( 101): Vectorized loop.
  2804: opt(1592): Outer loop unrolled inside inner loop.
  2804: vec( 101): Vectorized loop.
  2807: inl(1212): Source for routine not found.: MICRO_MG_UTILS::AVG_DIAMETER
  2807: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::AVG_DIAMETER
  2807: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::AVG_DIAMETER
  2807: vec( 103): Unvectorized loop.
  2810: vec( 101): Vectorized loop.
  2822: opt(1592): Outer loop unrolled inside inner loop.
  2822: vec( 101): Vectorized loop.
  2825: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::AVG_DIAMETER
  2825: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::AVG_DIAMETER
  2825: vec( 103): Unvectorized loop.
  2828: vec( 101): Vectorized loop.
  2849: opt(1593): Loop nest collapsed into one loop.: I
  2849: vec( 101): Vectorized loop.
  2927: opt(1592): Outer loop unrolled inside inner loop.
  2927: vec( 101): Vectorized loop.
  2928: opt(1592): Outer loop unrolled inside inner loop.
  2928: vec( 101): Vectorized loop.
  2929: opt(1592): Outer loop unrolled inside inner loop.
  2929: vec( 101): Vectorized loop.
  2930: opt(1592): Outer loop unrolled inside inner loop.
  2930: vec( 101): Vectorized loop.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:46:30 2018
FILE NAME: micro_mg2_0.F90

PROCEDURE NAME: MICRO_MG2_0::MICRO_MG_TEND
FORMAT LIST

 LINE   LOOP      STATEMENT

   230:           subroutine micro_mg_tend ( &
   231:                mgncol,             nlev,               deltatin,           &
   232:                t,                            q,                            &
   233:                qcn,                          qin,                          &
   234:                ncn,                          nin,                          &
   235:                qrn,                          qsn,                          &
   236:                nrn,                          nsn,                          &
   237:                relvar,                       accre_enhan,                  &
   238:                p,                            pdel,                         &
   239:                cldn,    liqcldf,        icecldf,       qsatfac,            &
   240:                qcsinksum_rate1ord,                                         &
   241:                naai,                         npccn,                        &
   242:                rndst,                        nacon,                        &
   243:                tlat,                         qvlat,                        &
   244:                qctend,                       qitend,                       &
   245:                nctend,                       nitend,                       &
   246:                qrtend,                       qstend,                       &
   247:                nrtend,                       nstend,                       &
   248:                effc,               effc_fn,            effi,               &
   249:                sadice,                       sadsnow,                      &
   250:                prect,                        preci,                        &
   251:                nevapr,                       evapsnow,                     &
   252:                am_evp_st,                                                  &
   253:                prain,                        prodsnow,                     &
   254:                cmeout,                       deffi,                        &
   255:                pgamrad,                      lamcrad,                      &
   256:                qsout,                        dsout,                        &
   257:                lflx,               iflx,                                   &
   258:                rflx,               sflx,               qrout,              &
   259:                reff_rain,                    reff_snow,                    &
   260:                qcsevap,            qisevap,            qvres,              &
   261:                cmeitot,            vtrmc,              vtrmi,              &
   262:                umr,                          ums,                          &
   263:                qcsedten,                     qisedten,                     &
   264:                qrsedten,                     qssedten,                     &
   265:                pratot,                       prctot,                       &
   266:                mnuccctot,          mnuccttot,          msacwitot,          &
   267:                psacwstot,          bergstot,           bergtot,            &
   268:                melttot,                      homotot,                      &
   269:                qcrestot,           prcitot,            praitot,            &
   270:                qirestot,           mnuccrtot,          pracstot,           &
   271:                meltsdttot,         frzrdttot,          mnuccdtot,          &
   272:                nrout,                        nsout,                        &
   273:                refl,               arefl,              areflz,             &
   274:                frefl,              csrfl,              acsrfl,             &
   275:                fcsrfl,                       rercld,                       &
   276:                ncai,                         ncal,                         &
   277:                qrout2,                       qsout2,                       &
   278:                nrout2,                       nsout2,                       &
   279:                drout2,                       dsout2,                       &
   280:                freqs,                        freqr,                        &
   281:                nfice,                        qcrat,                        &
   282:                errstring, & ! Below arguments are "optional" (pass null pointers to omit).
   283:                tnd_qsnow,          tnd_nsnow,          re_ice,             &
   284:                prer_evap,                                                      &
   285:                frzimm,             frzcnt,             frzdep)
   286:             ! Constituent properties.
   287:           
   288:               USE micro_mg_utils, ONLY: mg_liq_props, mg_ice_props, mg_rain_props, mg_snow_props
   289:             ! Size calculation functions.
   290:           
   291:               USE micro_mg_utils, ONLY: size_dist_param_liq, size_dist_param_basic, avg_diameter
   292:             ! Microphysical processes.
   293:           
   294:               USE micro_mg_utils, ONLY: ice_deposition_sublimation, sb2001v2_liq_autoconversion, sb2001v2_accre_cld_water_rain, &
   295:               &kk2000_liq_autoconversion, ice_autoconversion, immersion_freezing, contact_freezing, snow_self_aggregation, &
   296:               &accrete_cloud_water_snow, secondary_ice_production, accrete_rain_snow, heterogeneous_rain_freezing, &
   297:               &accrete_cloud_water_rain, self_collection_rain, accrete_cloud_ice_snow, evaporate_sublimate_precip, bergeron_process_snow
   298:             !Authors: Hugh Morrison, Andrew Gettelman, NCAR, Peter Caldwell, LLNL
   299:             ! e-mail: morrison@ucar.edu, andrew@ucar.edu
   300:             ! input arguments
   301:           
   302:           
   303:             integer,  intent(in) :: mgncol         ! number of microphysics columns
   304:             integer,  intent(in) :: nlev           ! number of layers
   305:             real(r8), intent(in) :: deltatin       ! time step (s)
   306:             real(r8), intent(in) :: t(mgncol,nlev) ! input temperature (K)
   307:             real(r8), intent(in) :: q(mgncol,nlev) ! input h20 vapor mixing ratio (kg/kg)
   308:             ! note: all input cloud variables are grid-averaged
   309:           
   310:             real(r8), intent(in) :: qcn(mgncol,nlev)       ! cloud water mixing ratio (kg/kg)
   311:             real(r8), intent(in) :: qin(mgncol,nlev)       ! cloud ice mixing ratio (kg/kg)
   312:             real(r8), intent(in) :: ncn(mgncol,nlev)       ! cloud water number conc (1/kg)
   313:             real(r8), intent(in) :: nin(mgncol,nlev)       ! cloud ice number conc (1/kg)
   314:           
   315:             real(r8), intent(in) :: qrn(mgncol,nlev)       ! rain mixing ratio (kg/kg)
   316:             real(r8), intent(in) :: qsn(mgncol,nlev)       ! snow mixing ratio (kg/kg)
   317:             real(r8), intent(in) :: nrn(mgncol,nlev)       ! rain number conc (1/kg)
   318:             real(r8), intent(in) :: nsn(mgncol,nlev)       ! snow number conc (1/kg)
   319:           
   320:             real(r8), intent(in) :: relvar(mgncol,nlev)      ! cloud water relative variance (-)
   321:             real(r8), intent(in) :: accre_enhan(mgncol,nlev) ! optional accretion
   322:                                                        ! enhancement factor (-)
   323:           
   324:             real(r8), intent(in) :: p(mgncol,nlev)        ! air pressure (pa)
   325:             real(r8), intent(in) :: pdel(mgncol,nlev)     ! pressure difference across level (pa)
   326:           
   327:             real(r8), intent(in) :: cldn(mgncol,nlev)      ! cloud fraction (no units)
   328:             real(r8), intent(in) :: liqcldf(mgncol,nlev)   ! liquid cloud fraction (no units)
   329:             real(r8), intent(in) :: icecldf(mgncol,nlev)   ! ice cloud fraction (no units)
   330:             real(r8), intent(in) :: qsatfac(mgncol,nlev)   ! subgrid cloud water saturation scaling factor (no units)
   331:             ! used for scavenging
   332:             ! Inputs for aerosol activation
   333:           
   334:             real(r8), intent(in) :: naai(mgncol,nlev)     ! ice nucleation number (from microp_aero_ts) (1/kg)
   335:             real(r8), intent(in) :: npccn(mgncol,nlev)   ! ccn activated number tendency (from microp_aero_ts) (1/kg*s)
   336:             ! Note that for these variables, the dust bin is assumed to be the last index.
   337:             ! (For example, in CAM, the last dimension is always size 4.)
   338:           
   339:             real(r8), intent(in) :: rndst(:,:,:)  ! radius of each dust bin, for contact freezing (from microp_aero_ts) (m)
   340:             real(r8), intent(in) :: nacon(:,:,:) ! number in each dust bin, for contact freezing  (from microp_aero_ts) (1/m^3)
   341:             ! output arguments
   342:            
   343:           
   344:             real(r8), intent(out) :: qcsinksum_rate1ord(mgncol,nlev) ! 1st order rate for
   345:             ! direct cw to precip conversion
   346:             real(r8), intent(out) :: tlat(mgncol,nlev)         ! latent heating rate       (W/kg)
   347:             real(r8), intent(out) :: qvlat(mgncol,nlev)        ! microphysical tendency qv (1/s)
   348:             real(r8), intent(out) :: qctend(mgncol,nlev)       ! microphysical tendency qc (1/s)
   349:             real(r8), intent(out) :: qitend(mgncol,nlev)       ! microphysical tendency qi (1/s)
   350:             real(r8), intent(out) :: nctend(mgncol,nlev)       ! microphysical tendency nc (1/(kg*s))
   351:             real(r8), intent(out) :: nitend(mgncol,nlev)       ! microphysical tendency ni (1/(kg*s))
   352:           
   353:             real(r8), intent(out) :: qrtend(mgncol,nlev)       ! microphysical tendency qr (1/s)
   354:             real(r8), intent(out) :: qstend(mgncol,nlev)       ! microphysical tendency qs (1/s)
   355:             real(r8), intent(out) :: nrtend(mgncol,nlev)       ! microphysical tendency nr (1/(kg*s))
   356:             real(r8), intent(out) :: nstend(mgncol,nlev)       ! microphysical tendency ns (1/(kg*s))
   357:             real(r8), intent(out) :: effc(mgncol,nlev)         ! droplet effective radius (micron)
   358:             real(r8), intent(out) :: effc_fn(mgncol,nlev)      ! droplet effective radius, assuming nc = 1.e8 kg-1
   359:             real(r8), intent(out) :: effi(mgncol,nlev)         ! cloud ice effective radius (micron)
   360:             real(r8), intent(out) :: sadice(mgncol,nlev)       ! cloud ice surface area density (cm2/cm3)
   361:             real(r8), intent(out) :: sadsnow(mgncol,nlev)      ! cloud snow surface area density (cm2/cm3)
   362:             real(r8), intent(out) :: prect(mgncol)             ! surface precip rate (m/s)
   363:             real(r8), intent(out) :: preci(mgncol)             ! cloud ice/snow precip rate (m/s)
   364:             real(r8), intent(out) :: nevapr(mgncol,nlev)       ! evaporation rate of rain + snow (1/s)
   365:             real(r8), intent(out) :: evapsnow(mgncol,nlev)     ! sublimation rate of snow (1/s)
   366:             real(r8), intent(out) :: am_evp_st(mgncol,nlev)    ! stratiform evaporation area (frac)
   367:             real(r8), intent(out) :: prain(mgncol,nlev)        ! production of rain + snow (1/s)
   368:             real(r8), intent(out) :: prodsnow(mgncol,nlev)     ! production of snow (1/s)
   369:             real(r8), intent(out) :: cmeout(mgncol,nlev)       ! evap/sub of cloud (1/s)
   370:             real(r8), intent(out) :: deffi(mgncol,nlev)        ! ice effective diameter for optics (radiation) (micron)
   371:             real(r8), intent(out) :: pgamrad(mgncol,nlev)      ! ice gamma parameter for optics (radiation) (no units)
   372:             real(r8), intent(out) :: lamcrad(mgncol,nlev)      ! slope of droplet distribution for optics (radiation) (1/m)
   373:             real(r8), intent(out) :: qsout(mgncol,nlev)        ! snow mixing ratio (kg/kg)
   374:             real(r8), intent(out) :: dsout(mgncol,nlev)        ! snow diameter (m)
   375:             real(r8), intent(out) :: lflx(mgncol,nlev+1)       ! grid-box average liquid condensate flux (kg m^-2 s^-1)
   376:             real(r8), intent(out) :: iflx(mgncol,nlev+1)       ! grid-box average ice condensate flux (kg m^-2 s^-1)
   377:             real(r8), intent(out) :: rflx(mgncol,nlev+1)       ! grid-box average rain flux (kg m^-2 s^-1)
   378:             real(r8), intent(out) :: sflx(mgncol,nlev+1)       ! grid-box average snow flux (kg m^-2 s^-1)
   379:             real(r8), intent(out) :: qrout(mgncol,nlev)        ! grid-box average rain mixing ratio (kg/kg)
   380:             real(r8), intent(out) :: reff_rain(mgncol,nlev)    ! rain effective radius (micron)
   381:             real(r8), intent(out) :: reff_snow(mgncol,nlev)    ! snow effective radius (micron)
   382:             real(r8), intent(out) :: qcsevap(mgncol,nlev)      ! cloud water evaporation due to sedimentation (1/s)
   383:             real(r8), intent(out) :: qisevap(mgncol,nlev)      ! cloud ice sublimation due to sublimation (1/s)
   384:             real(r8), intent(out) :: qvres(mgncol,nlev)        ! residual condensation term to ensure RH < 100% (1/s)
   385:             real(r8), intent(out) :: cmeitot(mgncol,nlev)      ! grid-mean cloud ice sub/dep (1/s)
   386:             real(r8), intent(out) :: vtrmc(mgncol,nlev)        ! mass-weighted cloud water fallspeed (m/s)
   387:             real(r8), intent(out) :: vtrmi(mgncol,nlev)        ! mass-weighted cloud ice fallspeed (m/s)
   388:             real(r8), intent(out) :: umr(mgncol,nlev)          ! mass weighted rain fallspeed (m/s)
   389:             real(r8), intent(out) :: ums(mgncol,nlev)          ! mass weighted snow fallspeed (m/s)
   390:             real(r8), intent(out) :: qcsedten(mgncol,nlev)     ! qc sedimentation tendency (1/s)
   391:             real(r8), intent(out) :: qisedten(mgncol,nlev)     ! qi sedimentation tendency (1/s)
   392:             real(r8), intent(out) :: qrsedten(mgncol,nlev)     ! qr sedimentation tendency (1/s)
   393:             real(r8), intent(out) :: qssedten(mgncol,nlev)     ! qs sedimentation tendency (1/s)
   394:             ! microphysical process rates for output (mixing ratio tendencies) (all have units of 1/s)
   395:           
   396:             real(r8), intent(out) :: pratot(mgncol,nlev)          ! accretion of cloud by rain
   397:             real(r8), intent(out) :: prctot(mgncol,nlev)          ! autoconversion of cloud to rain
   398:             real(r8), intent(out) :: mnuccctot(mgncol,nlev)       ! mixing ratio tend due to immersion freezing
   399:             real(r8), intent(out) :: mnuccttot(mgncol,nlev)       ! mixing ratio tend due to contact freezing
   400:             real(r8), intent(out) :: msacwitot(mgncol,nlev)       ! mixing ratio tend due to H-M splintering
   401:             real(r8), intent(out) :: psacwstot(mgncol,nlev)       ! collection of cloud water by snow
   402:             real(r8), intent(out) :: bergstot(mgncol,nlev)        ! bergeron process on snow
   403:             real(r8), intent(out) :: bergtot(mgncol,nlev)         ! bergeron process on cloud ice
   404:             real(r8), intent(out) :: melttot(mgncol,nlev)         ! melting of cloud ice
   405:             real(r8), intent(out) :: homotot(mgncol,nlev)         ! homogeneous freezing cloud water
   406:             real(r8), intent(out) :: qcrestot(mgncol,nlev)        ! residual cloud condensation due to removal of excess supersat
   407:             real(r8), intent(out) :: prcitot(mgncol,nlev)         ! autoconversion of cloud ice to snow
   408:             real(r8), intent(out) :: praitot(mgncol,nlev)         ! accretion of cloud ice by snow
   409:             real(r8), intent(out) :: qirestot(mgncol,nlev)        ! residual ice deposition due to removal of excess supersat
   410:             real(r8), intent(out) :: mnuccrtot(mgncol,nlev)       ! mixing ratio tendency due to heterogeneous freezing of rain to snow (1/s)
   411:             real(r8), intent(out) :: pracstot(mgncol,nlev)        ! mixing ratio tendency due to accretion of rain by snow (1/s)
   412:             real(r8), intent(out) :: meltsdttot(mgncol,nlev)      ! latent heating rate due to melting of snow  (W/kg)
   413:             real(r8), intent(out) :: frzrdttot(mgncol,nlev)       ! latent heating rate due to homogeneous freezing of rain (W/kg)
   414:             real(r8), intent(out) :: mnuccdtot(mgncol,nlev)       ! mass tendency from ice nucleation
   415:             real(r8), intent(out) :: nrout(mgncol,nlev)        ! rain number concentration (1/m3)
   416:             real(r8), intent(out) :: nsout(mgncol,nlev)        ! snow number concentration (1/m3)
   417:             real(r8), intent(out) :: refl(mgncol,nlev)         ! analytic radar reflectivity
   418:             real(r8), intent(out) :: arefl(mgncol,nlev)        ! average reflectivity will zero points outside valid range
   419:             real(r8), intent(out) :: areflz(mgncol,nlev)       ! average reflectivity in z.
   420:             real(r8), intent(out) :: frefl(mgncol,nlev)        ! fractional occurrence of radar reflectivity
   421:             real(r8), intent(out) :: csrfl(mgncol,nlev)        ! cloudsat reflectivity
   422:             real(r8), intent(out) :: acsrfl(mgncol,nlev)       ! cloudsat average
   423:             real(r8), intent(out) :: fcsrfl(mgncol,nlev)       ! cloudsat fractional occurrence of radar reflectivity
   424:             real(r8), intent(out) :: rercld(mgncol,nlev)       ! effective radius calculation for rain + cloud
   425:             real(r8), intent(out) :: ncai(mgncol,nlev)         ! output number conc of ice nuclei available (1/m3)
   426:             real(r8), intent(out) :: ncal(mgncol,nlev)         ! output number conc of CCN (1/m3)
   427:             real(r8), intent(out) :: qrout2(mgncol,nlev)       ! copy of qrout as used to compute drout2
   428:             real(r8), intent(out) :: qsout2(mgncol,nlev)       ! copy of qsout as used to compute dsout2
   429:             real(r8), intent(out) :: nrout2(mgncol,nlev)       ! copy of nrout as used to compute drout2
   430:             real(r8), intent(out) :: nsout2(mgncol,nlev)       ! copy of nsout as used to compute dsout2
   431:             real(r8), intent(out) :: drout2(mgncol,nlev)       ! mean rain particle diameter (m)
   432:             real(r8), intent(out) :: dsout2(mgncol,nlev)       ! mean snow particle diameter (m)
   433:             real(r8), intent(out) :: freqs(mgncol,nlev)        ! fractional occurrence of snow
   434:             real(r8), intent(out) :: freqr(mgncol,nlev)        ! fractional occurrence of rain
   435:             real(r8), intent(out) :: nfice(mgncol,nlev)        ! fractional occurrence of ice
   436:             real(r8), intent(out) :: qcrat(mgncol,nlev)        ! limiter for qc process rates (1=no limit --> 0. no qc)
   437:           
   438:             real(r8), intent(out) :: prer_evap(mgncol,nlev)
   439:           
   440:             character(128),   intent(out) :: errstring  ! output status (non-blank for error return)
   441:             ! Tendencies calculated by external schemes that can replace MG's native
   442:             ! process tendencies.
   443:             ! Used with CARMA cirrus microphysics
   444:             ! (or similar external microphysics model)
   445:           
   446:           
   447:             real(r8), intent(in) :: tnd_qsnow(:,:) ! snow mass tendency (kg/kg/s)
   448:             real(r8), intent(in) :: tnd_nsnow(:,:) ! snow number tendency (#/kg/s)
   449:             real(r8), intent(in) :: re_ice(:,:)    ! ice effective radius (m)
   450:             ! From external ice nucleation.
   451:           
   452:             real(r8), intent(in) :: frzimm(:,:) ! Number tendency due to immersion freezing (1/cm3)
   453:             real(r8), intent(in) :: frzcnt(:,:) ! Number tendency due to contact freezing (1/cm3)
   454:             real(r8), intent(in) :: frzdep(:,:) ! Number tendency due to deposition nucleation (1/cm3)
   455:             ! local workspace
   456:             ! all units mks unless otherwise stated
   457:             ! local copies of input variables
   458:           
   459:           
   460:             real(r8) :: qc(mgncol,nlev)      ! cloud liquid mixing ratio (kg/kg)
   461:             real(r8) :: qi(mgncol,nlev)      ! cloud ice mixing ratio (kg/kg)
   462:             real(r8) :: nc(mgncol,nlev)      ! cloud liquid number concentration (1/kg)
   463:             real(r8) :: ni(mgncol,nlev)      ! cloud liquid number concentration (1/kg)
   464:             real(r8) :: qr(mgncol,nlev)      ! rain mixing ratio (kg/kg)
   465:             real(r8) :: qs(mgncol,nlev)      ! snow mixing ratio (kg/kg)
   466:             real(r8) :: nr(mgncol,nlev)      ! rain number concentration (1/kg)
   467:             real(r8) :: ns(mgncol,nlev)      ! snow number concentration (1/kg)
   468:             ! general purpose variables
   469:           
   470:             real(r8) :: deltat            ! sub-time step (s)
   471:             real(r8) :: mtime             ! the assumed ice nucleation timescale
   472:             ! physical properties of the air at a given point
   473:           
   474:             real(r8) :: rho(mgncol,nlev)    ! density (kg m-3)
   475:             real(r8) :: dv(mgncol,nlev)     ! diffusivity of water vapor
   476:             real(r8) :: mu(mgncol,nlev)     ! viscosity
   477:             real(r8) :: sc(mgncol,nlev)     ! schmidt number
   478:             real(r8) :: rhof(mgncol,nlev)   ! density correction factor for fallspeed
   479:             ! cloud fractions
   480:           
   481:             real(r8) :: precip_frac(mgncol,nlev) ! precip fraction assuming maximum overlap
   482:             real(r8) :: cldm(mgncol,nlev)   ! cloud fraction
   483:             real(r8) :: icldm(mgncol,nlev)  ! ice cloud fraction
   484:             real(r8) :: lcldm(mgncol,nlev)  ! liq cloud fraction
   485:             real(r8) :: qsfm(mgncol,nlev)   ! subgrid cloud water saturation scaling factor
   486:             ! mass mixing ratios
   487:           
   488:             real(r8) :: qcic(mgncol,nlev)   ! in-cloud cloud liquid
   489:             real(r8) :: qiic(mgncol,nlev)   ! in-cloud cloud ice
   490:             real(r8) :: qsic(mgncol,nlev)   ! in-precip snow
   491:             real(r8) :: qric(mgncol,nlev)   ! in-precip rain
   492:             ! number concentrations
   493:           
   494:             real(r8) :: ncic(mgncol,nlev)   ! in-cloud droplet
   495:             real(r8) :: niic(mgncol,nlev)   ! in-cloud cloud ice
   496:             real(r8) :: nsic(mgncol,nlev)   ! in-precip snow
   497:             real(r8) :: nric(mgncol,nlev)   ! in-precip rain
   498:             ! maximum allowed ni value
   499:             real(r8) :: nimax(mgncol,nlev)
   500:             ! Size distribution parameters for:
   501:             ! cloud ice
   502:           
   503:             real(r8) :: lami(mgncol,nlev)   ! slope
   504:             real(r8) :: n0i(mgncol,nlev)    ! intercept
   505:             ! cloud liquid
   506:             real(r8) :: lamc(mgncol,nlev)   ! slope
   507:             real(r8) :: pgam(mgncol,nlev)   ! spectral width parameter
   508:             ! snow
   509:             real(r8) :: lams(mgncol,nlev)   ! slope
   510:             real(r8) :: n0s(mgncol,nlev)    ! intercept
   511:             ! rain
   512:             real(r8) :: lamr(mgncol,nlev)   ! slope
   513:             real(r8) :: n0r(mgncol,nlev)    ! intercept
   514:             ! Rates/tendencies due to:
   515:             ! Instantaneous snow melting
   516:           
   517:           
   518:             real(r8) :: minstsm(mgncol,nlev)    ! mass mixing ratio
   519:             real(r8) :: ninstsm(mgncol,nlev)    ! number concentration
   520:             ! Instantaneous rain freezing
   521:             real(r8) :: minstrf(mgncol,nlev)    ! mass mixing ratio
   522:             real(r8) :: ninstrf(mgncol,nlev)    ! number concentration
   523:             ! deposition of cloud ice
   524:           
   525:             real(r8) :: vap_dep(mgncol,nlev)    ! deposition from vapor to ice PMC 12/3/12
   526:             ! sublimation of cloud ice
   527:             real(r8) :: ice_sublim(mgncol,nlev) ! sublimation from ice to vapor PMC 12/3/12
   528:             ! ice nucleation
   529:             real(r8) :: nnuccd(mgncol,nlev) ! number rate from deposition/cond.-freezing
   530:             real(r8) :: mnuccd(mgncol,nlev) ! mass mixing ratio
   531:             ! freezing of cloud water
   532:             real(r8) :: mnuccc(mgncol,nlev) ! mass mixing ratio
   533:             real(r8) :: nnuccc(mgncol,nlev) ! number concentration
   534:             ! contact freezing of cloud water
   535:             real(r8) :: mnucct(mgncol,nlev) ! mass mixing ratio
   536:             real(r8) :: nnucct(mgncol,nlev) ! number concentration
   537:             ! deposition nucleation in mixed-phase clouds (from external scheme)
   538:             real(r8) :: mnudep(mgncol,nlev) ! mass mixing ratio
   539:             real(r8) :: nnudep(mgncol,nlev) ! number concentration
   540:             ! ice multiplication
   541:             real(r8) :: msacwi(mgncol,nlev) ! mass mixing ratio
   542:             real(r8) :: nsacwi(mgncol,nlev) ! number concentration
   543:             ! autoconversion of cloud droplets
   544:             real(r8) :: prc(mgncol,nlev)    ! mass mixing ratio
   545:             real(r8) :: nprc(mgncol,nlev)   ! number concentration (rain)
   546:             real(r8) :: nprc1(mgncol,nlev)  ! number concentration (cloud droplets)
   547:             ! self-aggregation of snow
   548:             real(r8) :: nsagg(mgncol,nlev)  ! number concentration
   549:             ! self-collection of rain
   550:             real(r8) :: nragg(mgncol,nlev)  ! number concentration
   551:             ! collection of droplets by snow
   552:             real(r8) :: psacws(mgncol,nlev)     ! mass mixing ratio
   553:             real(r8) :: npsacws(mgncol,nlev)    ! number concentration
   554:             ! collection of rain by snow
   555:             real(r8) :: pracs(mgncol,nlev)  ! mass mixing ratio
   556:             real(r8) :: npracs(mgncol,nlev) ! number concentration
   557:             ! freezing of rain
   558:             real(r8) :: mnuccr(mgncol,nlev) ! mass mixing ratio
   559:             real(r8) :: nnuccr(mgncol,nlev) ! number concentration
   560:             ! freezing of rain to form ice (mg add 4/26/13)
   561:             real(r8) :: mnuccri(mgncol,nlev)    ! mass mixing ratio
   562:             real(r8) :: nnuccri(mgncol,nlev)    ! number concentration
   563:             ! accretion of droplets by rain
   564:             real(r8) :: pra(mgncol,nlev)    ! mass mixing ratio
   565:             real(r8) :: npra(mgncol,nlev)   ! number concentration
   566:             ! autoconversion of cloud ice to snow
   567:             real(r8) :: prci(mgncol,nlev)   ! mass mixing ratio
   568:             real(r8) :: nprci(mgncol,nlev)  ! number concentration
   569:             ! accretion of cloud ice by snow
   570:             real(r8) :: prai(mgncol,nlev)   ! mass mixing ratio
   571:             real(r8) :: nprai(mgncol,nlev)  ! number concentration
   572:             ! evaporation of rain
   573:             real(r8) :: pre(mgncol,nlev)    ! mass mixing ratio
   574:             ! sublimation of snow
   575:             real(r8) :: prds(mgncol,nlev)   ! mass mixing ratio
   576:             ! number evaporation
   577:             real(r8) :: nsubi(mgncol,nlev)  ! cloud ice
   578:             real(r8) :: nsubc(mgncol,nlev)  ! droplet
   579:             real(r8) :: nsubs(mgncol,nlev)  ! snow
   580:             real(r8) :: nsubr(mgncol,nlev)  ! rain
   581:             ! bergeron process
   582:             real(r8) :: berg(mgncol,nlev)   ! mass mixing ratio (cloud ice)
   583:             real(r8) :: bergs(mgncol,nlev)  ! mass mixing ratio (snow)
   584:             ! fallspeeds
   585:             ! number-weighted
   586:            
   587:             real(r8) :: uns(mgncol,nlev)    ! snow
   588:             real(r8) :: unr(mgncol,nlev)    ! rain
   589:             ! air density corrected fallspeed parameters
   590:             real(r8) :: arn(mgncol,nlev)    ! rain
   591:             real(r8) :: asn(mgncol,nlev)    ! snow
   592:             real(r8) :: acn(mgncol,nlev)    ! cloud droplet
   593:             real(r8) :: ain(mgncol,nlev)    ! cloud ice
   594:             real(r8) :: ajn(mgncol,nlev)    ! cloud small ice
   595:             ! Mass of liquid droplets used with external heterogeneous freezing.
   596:           
   597:             real(r8) :: mi0l(mgncol)
   598:             ! saturation vapor pressures
   599:           
   600:             real(r8) :: esl(mgncol,nlev)    ! liquid
   601:             real(r8) :: esi(mgncol,nlev)    ! ice
   602:             real(r8) :: esn, esnA(mgncol)   ! checking for RH after rain evap
   603:            
   604:             ! saturation vapor mixing ratios
   605:           
   606:             real(r8) :: qvl(mgncol,nlev)    ! liquid
   607:             real(r8) :: qvi(mgncol,nlev)    ! ice
   608:             real(r8) :: qvn, qvnA(mgncol),qvnAI(mgncol)   ! checking for RH after rain evap
   609:             ! relative humidity
   610:           
   611:             real(r8) :: relhum(mgncol,nlev)
   612:             ! parameters for cloud water and cloud ice sedimentation calculations
   613:           
   614:             real(r8) :: fc(mgncol,nlev)
   615:             real(r8) :: fnc(mgncol,nlev)
   616:             real(r8) :: fi(mgncol,nlev)
   617:             real(r8) :: fni(mgncol,nlev)
   618:           
   619:             real(r8) :: fr(mgncol,nlev)
   620:             real(r8) :: fnr(mgncol,nlev)
   621:             real(r8) :: fs(mgncol,nlev)
   622:             real(r8) :: fns(mgncol,nlev)
   623:           
   624:             real(r8) :: faloutc(nlev)
   625:             real(r8) :: faloutnc(nlev)
   626:             real(r8) :: falouti(nlev)
   627:             real(r8) :: faloutni(nlev)
   628:           
   629:             real(r8) :: faloutr(nlev)
   630:             real(r8) :: faloutnr(nlev)
   631:             real(r8) :: falouts(nlev)
   632:             real(r8) :: faloutns(nlev)
   633:           
   634:             real(r8) :: faltndc
   635:             real(r8) :: faltndnc
   636:             real(r8) :: faltndi
   637:             real(r8) :: faltndni
   638:             real(r8) :: faltndqie
   639:             real(r8) :: faltndqce
   640:           
   641:             real(r8) :: faltndr
   642:             real(r8) :: faltndnr
   643:             real(r8) :: faltnds
   644:             real(r8) :: faltndns
   645:           
   646:             real(r8) :: rainrt(mgncol,nlev)     ! rain rate for reflectivity calculation
   647:             ! dummy variables
   648:           
   649:             real(r8) :: dum
   650:             real(r8) :: dum1
   651:             real(r8) :: dum2
   652:             real(r8) :: dum1A(mgncol), dum2A(mgncol)
   653:            
   654:             real(r8) :: dumni0
   655:             real(r8) :: dumns0
   656:             ! dummies for checking RH
   657:             real(r8) :: qtmp
   658:             real(r8) :: ttmp
   659:             real(r8) :: qtmpA(mgncol), ttmpA(mgncol)
   660:             ! dummies for conservation check
   661:             real(r8) :: ratio
   662:             real(r8) :: tmpfrz
   663:             ! dummies for in-cloud variables
   664:             real(r8) :: dumc(mgncol,nlev)   ! qc
   665:             real(r8) :: dumnc(mgncol,nlev)  ! nc
   666:             real(r8) :: dumi(mgncol,nlev)   ! qi
   667:             real(r8) :: dumni(mgncol,nlev)  ! ni
   668:             real(r8) :: dumr(mgncol,nlev)   ! rain mixing ratio
   669:             real(r8) :: dumnr(mgncol,nlev)  ! rain number concentration
   670:             real(r8) :: dums(mgncol,nlev)   ! snow mixing ratio
   671:             real(r8) :: dumns(mgncol,nlev)  ! snow number concentration
   672:             ! Array dummy variable
   673:             real(r8) :: dum_2D(mgncol,nlev)
   674:             real(r8) :: pdel_inv(mgncol,nlev)
   675:             ! loop array variables
   676:             ! "i" and "k" are column/level iterators for internal (MG) variables
   677:             ! "n" is used for other looping (currently just sedimentation)
   678:           
   679:             integer i, k, n
   680:             ! number of sub-steps for loops over "n" (for sedimentation)
   681:           
   682:             integer nstep
   683:             integer mdust
   684:             ! Varaibles to scale fall velocity between small and regular ice regimes.
   685:           
   686:             real(r8) :: irad
   687:             real(r8) :: ifrac
   688:             !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   689:             ! Return error message
   690:           
   691:           
   692: +======>    errstring = ' '
   693:             ! Process inputs
   694:             ! assign variable deltat to deltatin
   695:           
   696:           
   697:             deltat = deltatin
   698:             ! Copies of input concentrations that may be changed internally.
   699:           
   700: V======>    qc = qcn
   701: V======     nc = ncn
   702: +======>    qi = qin
   703: +======>    ni = nin
   704: +======>    qr = qrn
   705: +======>    nr = nrn
   706: +======>    qs = qsn
   707: +======>    ns = nsn
   708:             ! cldn: used to set cldm, unused for subcolumns
   709:             ! liqcldf: used to set lcldm, unused for subcolumns
   710:             ! icecldf: used to set icldm, unused for subcolumns
   711:           
   712:           
   713:             if (microp_uniform) then
   714:                ! subcolumns, set cloud fraction variables to one
   715:                ! if cloud water or ice is present, if not present
   716:                ! set to mincld (mincld used instead of zero, to prevent
   717:                ! possible division by zero errors).
   718:           
   719: V======>       where (qc >= qsmall)
   720: |                 lcldm = 1._r8
   721: |              elsewhere
   722: |                 lcldm = mincld
   723: |              end where
   724: |         
   725: |              where (qi >= qsmall)
   726: |                 icldm = 1._r8
   727: |              elsewhere
   728: |                 icldm = mincld
   729: V======        end where
   730:           
   731: +======>       cldm = max(icldm, lcldm)
   732: +======>       qsfm = 1._r8
   733:           
   734:             else
   735:                ! get cloud fraction, check for minimum
   736: V======>       cldm = max(cldn,mincld)
   737: V======        lcldm = max(liqcldf,mincld)
   738: +======>       icldm = max(icecldf,mincld)
   739: +======>       qsfm = qsatfac
   740:             end if
   741:             ! Initialize local variables
   742:             ! local physical properties
   743:           
   744:           
   745: V======>    rho = p/(r*t)
   746: V======     dv = 8.794E-5_r8 * t**1.81_r8 / p
   747: +======>    mu = 1.496E-6_r8 * t**1.5_r8 / (t + 120._r8)
   748: +======>    sc = mu/(rho*dv)
   749:             ! air density adjustment for fallspeed parameters
   750:             ! includes air density correction factor to the
   751:             ! power of 0.54 following Heymsfield and Bansemer 2007
   752:           
   753:           
   754: +======>    rhof=(rhosu/rho)**0.54_r8
   755:           
   756: +======>    arn=ar*rhof
   757: +======>    asn=as*rhof
   758: +======>    acn=g*rhow/(18._r8*mu)
   759: +======>    ain=ai*(rhosu/rho)**0.35_r8
   760: +======>    ajn=aj*(rhosu/rho)**0.35_r8
   761:             !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   762:             ! Get humidity and saturation vapor pressures
   763:           
   764:           
   765: +------>    do k=1,nlev
   766: |         
   767: |              call qsat_water(t(:,k), p(:,k), esl(:,k), qvl(:,k),mgncol)
   768: |              call qsat_ice(t(:,k), p(:,k), esi(:,k), qvi(:,k),mgncol)
   769: |V----->       do i=1,mgncol
   770: ||                ! make sure when above freezing that esi=esl, not active yet
   771: ||        
   772: ||                if (t(i,k) >= tmelt) then
   773: ||                   esi(i,k)=esl(i,k)
   774: ||                   qvi(i,k)=qvl(i,k)
   775: ||                else
   776: ||                   ! Scale the water saturation values to reflect subgrid scale
   777: ||                   ! ice cloud fraction, where ice clouds begin forming at a
   778: ||                   ! gridbox average relative humidity of rhmini (not 1).
   779: ||                   ! NOTE: For subcolumns and other non-subgrid clouds, qsfm willi
   780: ||                   ! be 1.
   781: ||        
   782: ||                   !
   783: ||                   qvi(i,k) = qsfm(i,k) * qvi(i,k)
   784: ||                   esi(i,k) = qsfm(i,k) * esi(i,k)
   785: ||                   qvl(i,k) = qsfm(i,k) * qvl(i,k)
   786: ||                   esl(i,k) = qsfm(i,k) * esl(i,k)
   787: ||                end if
   788: ||        
   789: |V-----        end do
   790: +------     end do
   791:           
   792: V======>    relhum = q / max(qvl, qsmall)
   793:             !===============================================
   794:             ! set mtime here to avoid answer-changing
   795:           
   796:           
   797:             mtime=deltat
   798:             ! initialize microphysics output
   799:           
   800: V======>    qcsevap=0._r8
   801: V======     qisevap=0._r8
   802: +======>    qvres  =0._r8
   803: +======>    cmeitot =0._r8
   804: +======>    vtrmc =0._r8
   805: +======>    vtrmi =0._r8
   806: +======>    qcsedten =0._r8
   807: +======>    qisedten =0._r8
   808: +======>    qrsedten =0._r8
   809: +======>    qssedten =0._r8
   810:           
   811: V======>    pratot=0._r8
   812: V======     prctot=0._r8
   813: +======>    mnuccctot=0._r8
   814: +======>    mnuccttot=0._r8
   815: +======>    msacwitot=0._r8
   816: +======>    psacwstot=0._r8
   817: +======>    bergstot=0._r8
   818: +======>    bergtot=0._r8
   819: +======>    melttot=0._r8
   820: +======>    homotot=0._r8
   821: V======>    qcrestot=0._r8
   822: V======     prcitot=0._r8
   823: +======>    praitot=0._r8
   824: +======>    qirestot=0._r8
   825: +======>    mnuccrtot=0._r8
   826: +======>    pracstot=0._r8
   827: +======>    meltsdttot=0._r8
   828: +======>    frzrdttot=0._r8
   829: +======>    mnuccdtot=0._r8
   830:           
   831: V======>    rflx=0._r8
   832: V======     sflx=0._r8
   833: +======>    lflx=0._r8
   834: +======>    iflx=0._r8
   835:             ! initialize precip output
   836:           
   837:           
   838: V======>    qrout=0._r8
   839: V======     qsout=0._r8
   840: +======>    nrout=0._r8
   841: +======>    nsout=0._r8
   842:             ! for refl calc
   843:           
   844: +======>    rainrt = 0._r8
   845:             ! initialize rain size
   846:           
   847: +======>    rercld=0._r8
   848:           
   849: +======>    qcsinksum_rate1ord = 0._r8
   850:             ! initialize variables for trop_mozart
   851:           
   852: +======>    nevapr = 0._r8
   853: +======>    prer_evap = 0._r8
   854: +======>    evapsnow = 0._r8
   855: V======>    am_evp_st = 0._r8
   856: V======     prain = 0._r8
   857: +======>    prodsnow = 0._r8
   858: +======>    cmeout = 0._r8
   859:           
   860: +======>    precip_frac = mincld
   861:           
   862: +======>    lamc=0._r8
   863:             ! initialize microphysical tendencies
   864:           
   865:           
   866: +======>    tlat=0._r8
   867: +======>    qvlat=0._r8
   868: +======>    qctend=0._r8
   869: +======>    qitend=0._r8
   870: V======>    qstend = 0._r8
   871: V======     qrtend = 0._r8
   872: +======>    nctend=0._r8
   873: +======>    nitend=0._r8
   874: +======>    nrtend = 0._r8
   875: +======>    nstend = 0._r8
   876:             ! initialize in-cloud and in-precip quantities to zero
   877:           
   878: +======>    qcic  = 0._r8
   879: +======>    qiic  = 0._r8
   880: +======>    qsic  = 0._r8
   881: +======>    qric  = 0._r8
   882:           
   883: V======>    ncic  = 0._r8
   884: V======     niic  = 0._r8
   885: +======>    nsic  = 0._r8
   886: +======>    nric  = 0._r8
   887:             ! initialize precip at surface
   888:           
   889:           
   890: V======>    prect = 0._r8
   891: +======>    preci = 0._r8
   892:             ! initialize precip fallspeeds to zero
   893:           
   894: V======>    ums = 0._r8
   895: V======     uns = 0._r8
   896: +======>    umr = 0._r8
   897: +======>    unr = 0._r8
   898:             ! initialize limiter for output
   899:           
   900: +======>    qcrat = 1._r8
   901:             ! Many outputs have to be initialized here at the top to work around
   902:             ! ifort problems, even if they are always overwritten later.
   903:           
   904: +======>    effc = 10._r8
   905: +======>    lamcrad = 0._r8
   906: +======>    pgamrad = 0._r8
   907: +======>    effc_fn = 10._r8
   908: +======>    effi = 25._r8
   909: V======>    sadice = 0._r8
   910: V======     sadsnow = 0._r8
   911: +======>    deffi = 50._r8
   912:           
   913: +======>    qrout2 = 0._r8
   914: +======>    nrout2 = 0._r8
   915: +======>    drout2 = 0._r8
   916: +======>    qsout2 = 0._r8
   917: +======>    nsout2 = 0._r8
   918: +======>    dsout = 0._r8
   919: +======>    dsout2 = 0._r8
   920:           
   921: V======>    freqr = 0._r8
   922: V======     freqs = 0._r8
   923:           
   924: +======>    reff_rain = 0._r8
   925: +======>    reff_snow = 0._r8
   926:           
   927: +======>    refl = -9999._r8
   928: +======>    arefl = 0._r8
   929: +======>    areflz = 0._r8
   930: +======>    frefl = 0._r8
   931: +======>    csrfl = 0._r8
   932: +======>    acsrfl = 0._r8
   933: V======>    fcsrfl = 0._r8
   934: |         
   935: V======     ncal = 0._r8
   936: +======>    ncai = 0._r8
   937:           
   938: +======>    nfice = 0._r8
   939:             !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
   940:             ! droplet activation
   941:             ! get provisional droplet number after activation. This is used for
   942:             ! all microphysical process calculations, for consistency with update of
   943:             ! droplet mass before microphysics
   944:             ! calculate potential for droplet activation if cloud water is present
   945:             ! tendency from activation (npccn) is read in from companion routine
   946:             ! output activated liquid and ice (convert from #/kg -> #/m3)
   947:             !--------------------------------------------------
   948:           
   949:           
   950: V======>    where (qc >= qsmall)
   951: +======        nc = max(nc + npccn*deltat, 0._r8)
   952:                ncal = nc*rho/lcldm ! sghan minimum in #/cm3
   953: V======>    elsewhere
   954: |              ncal = 0._r8
   955: |           end where
   956: V======   
   957:             where (t < icenuct)
   958: V======>       ncai = naai*rho
   959: |           elsewhere
   960: |              ncai = 0._r8
   961: V======     end where
   962:             !===============================================
   963:             ! ice nucleation if activated nuclei exist at t<-5C AND rhmini + 5%
   964:             ! NOTE: If using gridbox average values, condensation will not occur until rh=1,
   965:             ! so the threshold seems like it should be 1.05 and not rhmini + 0.05. For subgrid
   966:             ! clouds (using rhmini and qsfacm), the relhum has already been adjusted, and thus
   967:             ! the nucleation threshold should also be 1.05 and not rhmini + 0.05.
   968:             !-------------------------------------------------------
   969:           
   970:           
   971:             !
   972:           
   973:           
   974:             if (do_cldice) then
   975: V======>       where (naai > 0._r8 .and. t < icenuct .and. &
   976: |                   relhum*esl/esi > 1.05_r8)
   977: |                 !if NAAI > 0. then set numice = naai (as before)
   978: |                 !note: this is gridbox averaged
   979: |         
   980: +======           nnuccd = (naai-ni/icldm)/mtime*icldm
   981:                   nnuccd = max(nnuccd,0._r8)
   982: V======>          nimax = naai*icldm
   983: V======           !Calc mass of new particles using new crystal mass...
   984:                   !also this will be multiplied by mtime as nnuccd is...
   985: V======>  
   986: |         
   987: |                 mnuccd = nnuccd * mi0
   988: |         
   989: |              elsewhere
   990: |                 nnuccd = 0._r8
   991: |                 nimax = 0._r8
   992: |                 mnuccd = 0._r8
   993: V======        end where
   994:           
   995:             end if
   996:             !=============================================================================
   997:           
   998:           
   999: W------>    do k=1,nlev
  1000: |         
  1001: |*----->       do i=1,mgncol
  1002: ||                ! calculate instantaneous precip processes (melting and homogeneous freezing)
  1003: ||                ! melting of snow at +2 C
  1004: ||        
  1005: ||        
  1006: ||                if (t(i,k) > snowmelt) then
  1007: ||                   if (qs(i,k) > 0._r8) then
  1008: ||                      ! make sure melting snow doesn't reduce temperature below threshold
  1009: ||        
  1010: ||                      dum = -xlf/cpp*qs(i,k)
  1011: ||                      if (t(i,k)+dum < snowmelt) then
  1012: ||                         dum = (t(i,k)-snowmelt)*cpp/xlf
  1013: ||                         dum = dum/qs(i,k)
  1014: ||                         dum = max(0._r8,dum)
  1015: ||                         dum = min(1._r8,dum)
  1016: ||                      else
  1017: ||                         dum = 1._r8
  1018: ||                      end if
  1019: ||        
  1020: ||                      minstsm(i,k) = dum*qs(i,k)
  1021: ||                      ninstsm(i,k) = dum*ns(i,k)
  1022: ||        
  1023: ||                      dum1=-xlf*minstsm(i,k)/deltat
  1024: ||                      tlat(i,k)=tlat(i,k)+dum1
  1025: ||                      meltsdttot(i,k)=meltsdttot(i,k) + dum1
  1026: ||        
  1027: ||                      qs(i,k) = max(qs(i,k) - minstsm(i,k), 0._r8)
  1028: ||                      ns(i,k) = max(ns(i,k) - ninstsm(i,k), 0._r8)
  1029: ||                      qr(i,k) = max(qr(i,k) + minstsm(i,k), 0._r8)
  1030: ||                      nr(i,k) = max(nr(i,k) + ninstsm(i,k), 0._r8)
  1031: ||                   end if
  1032: ||                end if
  1033: ||        
  1034: |*-----        end do
  1035: W------     end do
  1036:           
  1037: W------>    do k=1,nlev
  1038: |*----->      do i=1,mgncol
  1039: ||                ! freezing of rain at -5 C
  1040: ||        
  1041: ||                if (t(i,k) < rainfrze) then
  1042: ||        
  1043: ||                   if (qr(i,k) > 0._r8) then
  1044: ||                      ! make sure freezing rain doesn't increase temperature above threshold
  1045: ||        
  1046: ||                      dum = xlf/cpp*qr(i,k)
  1047: ||                      if (t(i,k)+dum > rainfrze) then
  1048: ||                         dum = -(t(i,k)-rainfrze)*cpp/xlf
  1049: ||                         dum = dum/qr(i,k)
  1050: ||                         dum = max(0._r8,dum)
  1051: ||                         dum = min(1._r8,dum)
  1052: ||                      else
  1053: ||                         dum = 1._r8
  1054: ||                      end if
  1055: ||        
  1056: ||                      minstrf(i,k) = dum*qr(i,k)
  1057: ||                      ninstrf(i,k) = dum*nr(i,k)
  1058: ||                      ! heating tendency
  1059: ||        
  1060: ||                      dum1 = xlf*minstrf(i,k)/deltat
  1061: ||                      tlat(i,k)=tlat(i,k)+dum1
  1062: ||                      frzrdttot(i,k)=frzrdttot(i,k) + dum1
  1063: ||        
  1064: ||                      qr(i,k) = max(qr(i,k) - minstrf(i,k), 0._r8)
  1065: ||                      nr(i,k) = max(nr(i,k) - ninstrf(i,k), 0._r8)
  1066: ||                      qs(i,k) = max(qs(i,k) + minstrf(i,k), 0._r8)
  1067: ||                      ns(i,k) = max(ns(i,k) + ninstrf(i,k), 0._r8)
  1068: ||        
  1069: ||                   end if
  1070: ||                end if
  1071: |*-----        end do
  1072: W------     end do
  1073:           
  1074: W------>    do k=1,nlev
  1075: |*----->      do i=1,mgncol
  1076: ||                ! obtain in-cloud values of cloud water/ice mixing ratios and number concentrations
  1077: ||                !-------------------------------------------------------
  1078: ||                ! for microphysical process calculations
  1079: ||                ! units are kg/kg for mixing ratio, 1/kg for number conc
  1080: ||        
  1081: ||                if (qc(i,k).ge.qsmall) then
  1082: ||                   ! limit in-cloud values to 0.005 kg/kg
  1083: ||                   qcic(i,k)=min(qc(i,k)/lcldm(i,k),5.e-3_r8)
  1084: ||                   ncic(i,k)=max(nc(i,k)/lcldm(i,k),0._r8)
  1085: ||                   ! specify droplet concentration
  1086: ||        
  1087: ||                   if (nccons) then
  1088: ||                      ncic(i,k)=ncnst/rho(i,k)
  1089: ||                   end if
  1090: ||                else
  1091: ||                   qcic(i,k)=0._r8
  1092: ||                   ncic(i,k)=0._r8
  1093: ||                end if
  1094: ||        
  1095: ||                if (qi(i,k).ge.qsmall) then
  1096: ||                   ! limit in-cloud values to 0.005 kg/kg
  1097: ||                   qiic(i,k)=min(qi(i,k)/icldm(i,k),5.e-3_r8)
  1098: ||                   niic(i,k)=max(ni(i,k)/icldm(i,k),0._r8)
  1099: ||                   ! switch for specification of cloud ice number
  1100: ||        
  1101: ||                   if (nicons) then
  1102: ||                      niic(i,k)=ninst/rho(i,k)
  1103: ||                   end if
  1104: ||                else
  1105: ||                   qiic(i,k)=0._r8
  1106: ||                   niic(i,k)=0._r8
  1107: ||                end if
  1108: ||        
  1109: |*-----        end do
  1110: W------     end do
  1111:             !========================================================================
  1112:             ! for sub-columns cldm has already been set to 1 if cloud
  1113:             ! water or ice is present, so precip_frac will be correctly set below
  1114:             ! and nothing extra needs to be done here
  1115:           
  1116:           
  1117: V======>    precip_frac = cldm
  1118:           
  1119: +------>    micro_vert_loop: do k=1,nlev
  1120: |         
  1121: |              if (trim(micro_mg_precip_frac_method) == 'in_cloud') then
  1122: |         
  1123: |                 if (k /= 1) then
  1124: |V=====>             where (qc(:,k) < qsmall .and. qi(:,k) < qsmall)
  1125: ||                      precip_frac(:,k) = precip_frac(:,k-1)
  1126: |V=====              end where
  1127: |                 endif
  1128: |         
  1129: |              else if (trim(micro_mg_precip_frac_method) == 'max_overlap') then
  1130: |                 ! calculate precip fraction based on maximum overlap assumption
  1131: |                 ! if rain or snow mix ratios are smaller than threshold,
  1132: |                 ! then leave precip_frac as cloud fraction at current level
  1133: |         
  1134: |         
  1135: |                 if (k /= 1) then
  1136: |V=====>             where (qr(:,k-1) >= qsmall .or. qs(:,k-1) >= qsmall)
  1137: |*=====                 precip_frac(:,k)=max(precip_frac(:,k-1),precip_frac(:,k))
  1138: |                    end where
  1139: |*=====>          end if
  1140: |*=====   
  1141: |              endif
  1142: |              !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  1143: |              ! get size distribution parameters based on in-cloud cloud water
  1144: |              ! these calculations also ensure consistency between number and mixing ratio
  1145: |              !cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  1146: |              ! cloud liquid
  1147: |              !-------------------------------------------
  1148: |         
  1149: |         
  1150: |              call size_dist_param_liq(mg_liq_props, qcic(1:mgncol,k), ncic(1:mgncol,k),&
  1151: |                   rho(1:mgncol,k), pgam(1:mgncol,k), lamc(1:mgncol,k), mgncol)
  1152: |              !========================================================================
  1153: |              ! autoconversion of cloud liquid water to rain
  1154: |              ! formula from Khrouditnov and Kogan (2000), modified for sub-grid distribution of qc
  1155: |              ! minimum qc of 1 x 10^-8 prevents floating point error
  1156: |         
  1157: |         
  1158: |              if (.not. do_sb_physics) then
  1159: |                call kk2000_liq_autoconversion(microp_uniform, qcic(1:mgncol,k), &
  1160: |                   ncic(:,k), rho(:,k), relvar(:,k), prc(:,k), nprc(:,k), nprc1(:,k), mgncol)
  1161: |              endif
  1162: |              ! assign qric based on prognostic qr, using assumed precip fraction
  1163: |              ! note: this could be moved above for consistency with qcic and qiic calculations
  1164: |         
  1165: |V=====>       qric(:,k) = qr(:,k)/precip_frac(:,k)
  1166: |V=====>       nric(:,k) = nr(:,k)/precip_frac(:,k)
  1167: |              ! limit in-precip mixing ratios to 10 g/kg
  1168: |         
  1169: |V=====>       qric(:,k)=min(qric(:,k),0.01_r8)
  1170: |              ! add autoconversion to precip from above to get provisional rain mixing ratio
  1171: |              ! and number concentration (qric and nric)
  1172: |         
  1173: |         
  1174: |V=====>       where (qric(:,k).lt.qsmall)
  1175: ||                qric(:,k)=0._r8
  1176: ||                nric(:,k)=0._r8
  1177: |V=====        end where
  1178: |              ! make sure number concentration is a positive number to avoid
  1179: |              ! taking root of negative later
  1180: |         
  1181: |         
  1182: |V=====>       nric(:,k)=max(nric(:,k),0._r8)
  1183: |              ! Get size distribution parameters for cloud ice
  1184: |         
  1185: |         
  1186: |              call size_dist_param_basic(mg_ice_props, qiic(:,k), niic(:,k), &
  1187: |                   lami(:,k), mgncol, n0=n0i(:,k))
  1188: |              ! Alternative autoconversion
  1189: |         	
  1190: |              if (do_sb_physics) then
  1191: |                call sb2001v2_liq_autoconversion(pgam(:,k),qcic(1:mgncol,k),ncic(:,k), &
  1192: |                     qric(:,k),rho(:,k),relvar(:,k),prc(:,k),nprc(:,k),nprc1(:,k), mgncol)
  1193: |              endif	
  1194: |              !.......................................................................
  1195: |              ! Autoconversion of cloud ice to snow
  1196: |              ! similar to Ferrier (1994)
  1197: |         
  1198: |         
  1199: |              if (do_cldice) then
  1200: |                 call ice_autoconversion(t(:,k), qiic(:,k), lami(:,k), n0i(:,k), &
  1201: |                      dcs, prci(:,k), nprci(:,k), mgncol)
  1202: |              else
  1203: |                 ! Add in the particles that we have already converted to snow, and
  1204: |                 ! don't do any further autoconversion of ice.
  1205: |V=====>          prci(:,k)  = tnd_qsnow(:,k) / cldm(:,k)
  1206: |V=====>          nprci(:,k) = tnd_nsnow(:,k) / cldm(:,k)
  1207: |              end if
  1208: |              ! note, currently we don't have this
  1209: |              ! inside the do_cldice block, should be changed later
  1210: |              ! assign qsic based on prognostic qs, using assumed precip fraction
  1211: |         
  1212: |V=====>       qsic(:,k) = qs(:,k)/precip_frac(:,k)
  1213: |V=====>       nsic(:,k) = ns(:,k)/precip_frac(:,k)
  1214: |              ! limit in-precip mixing ratios to 10 g/kg
  1215: |         
  1216: |V=====>       qsic(:,k)=min(qsic(:,k),0.01_r8)
  1217: |              ! if precip mix ratio is zero so should number concentration
  1218: |         
  1219: |         
  1220: |V=====>       where (qsic(:,k) < qsmall)
  1221: ||                qsic(:,k)=0._r8
  1222: ||                nsic(:,k)=0._r8
  1223: |V=====        end where
  1224: |              ! make sure number concentration is a positive number to avoid
  1225: |              ! taking root of negative later
  1226: |         
  1227: |         
  1228: |V=====>       nsic(:,k)=max(nsic(:,k),0._r8)
  1229: |              !.......................................................................
  1230: |              ! get size distribution parameters for precip
  1231: |              !......................................................................
  1232: |              ! rain
  1233: |         
  1234: |         
  1235: |              call size_dist_param_basic(mg_rain_props, qric(:,k), nric(:,k), &
  1236: |                   lamr(:,k), mgncol, n0=n0r(:,k))
  1237: |         
  1238: |V=====>       where (lamr(:,k) >= qsmall)
  1239: ||                ! provisional rain number and mass weighted mean fallspeed (m/s)
  1240: ||        
  1241: ||        
  1242: |*=====           unr(:,k) = min(arn(:,k)*gamma_br_plus1/lamr(:,k)**br,9.1_r8*rhof(:,k))
  1243: |                 umr(:,k) = min(arn(:,k)*gamma_br_plus4/(6._r8*lamr(:,k)**br),9.1_r8*rhof(:,k))
  1244: |V=====>  
  1245: |V=====        elsewhere
  1246: |                 umr(:,k) = 0._r8
  1247: |V=====>          unr(:,k) = 0._r8
  1248: ||             end where
  1249: ||             !......................................................................
  1250: |V=====        ! snow
  1251: |         
  1252: |         
  1253: |              call size_dist_param_basic(mg_snow_props, qsic(:,k), nsic(:,k), &
  1254: |                   lams(:,k), mgncol, n0=n0s(:,k))
  1255: |         
  1256: |V=====>       where (lams(:,k) > 0._r8)
  1257: ||                ! provisional snow number and mass weighted mean fallspeed (m/s)
  1258: ||        
  1259: ||        
  1260: |*=====           ums(:,k) = min(asn(:,k)*gamma_bs_plus4/(6._r8*lams(:,k)**bs),1.2_r8*rhof(:,k))
  1261: |                 uns(:,k) = min(asn(:,k)*gamma_bs_plus1/lams(:,k)**bs,1.2_r8*rhof(:,k))
  1262: |V=====>  
  1263: |V=====        elsewhere
  1264: |                 ums(:,k) = 0._r8
  1265: |V=====>          uns(:,k) = 0._r8
  1266: ||             end where
  1267: ||        
  1268: |V=====        if (do_cldice) then
  1269: |                 if (.not. use_hetfrz_classnuc) then
  1270: |                    ! heterogeneous freezing of cloud water
  1271: |                    !----------------------------------------------
  1272: |         
  1273: |         
  1274: |                    call immersion_freezing(microp_uniform, t(:,k), pgam(:,k), lamc(:,k), &
  1275: |                         qcic(1:mgncol,k), ncic(:,k), relvar(:,k), mnuccc(:,k), nnuccc(:,k), mgncol)
  1276: |                    ! make sure number of droplets frozen does not exceed available ice nuclei concentration
  1277: |                    ! this prevents 'runaway' droplet freezing
  1278: |         
  1279: |         
  1280: |V=====>             where (qcic(1:mgncol,k).ge.qsmall .and. t(:,k).lt.269.15_r8)
  1281: ||                      where (nnuccc(:,k)*lcldm(:,k).gt.nnuccd(:,k))
  1282: ||                         ! scale mixing ratio of droplet freezing with limit
  1283: ||                         mnuccc(:,k)=mnuccc(:,k)*(nnuccd(:,k)/(nnuccc(:,k)*lcldm(:,k)))
  1284: ||                         nnuccc(:,k)=nnuccd(:,k)/lcldm(:,k)
  1285: ||                      end where
  1286: |V=====              end where
  1287: |         
  1288: |                    mdust = size(rndst,3)
  1289: |V=====>             call contact_freezing(microp_uniform, t(:,k), p(:,k), rndst(:,k,:), &
  1290: |                         nacon(:,k,:), pgam(:,k), lamc(:,k), qcic(1:mgncol,k), ncic(:,k), &
  1291: |                         relvar(:,k), mnucct(:,k), nnucct(:,k), mgncol, mdust)
  1292: |         
  1293: |V=====>             mnudep(:,k)=0._r8
  1294: |V=====>             nnudep(:,k)=0._r8
  1295: |         
  1296: |                 else
  1297: |                    ! Mass of droplets frozen is the average droplet mass, except
  1298: |                    ! with two limiters: concentration must be at least 1/cm^3, and
  1299: |                    ! mass must be at least the minimum defined above.
  1300: |         
  1301: |V=====>             mi0l = qcic(1:mgncol,k)/max(ncic(:,k), 1.0e6_r8/rho(:,k))
  1302: |*=====>             mi0l = max(mi0l_min, mi0l)
  1303: |         
  1304: |V=====>             where (qcic(1:mgncol,k) >= qsmall)
  1305: ||                      nnuccc(:,k) = frzimm(:,k)*1.0e6_r8/rho(:,k)
  1306: ||                      mnuccc(:,k) = nnuccc(:,k)*mi0l
  1307: ||        
  1308: ||                      nnucct(:,k) = frzcnt(:,k)*1.0e6_r8/rho(:,k)
  1309: ||                      mnucct(:,k) = nnucct(:,k)*mi0l
  1310: ||        
  1311: ||                      nnudep(:,k) = frzdep(:,k)*1.0e6_r8/rho(:,k)
  1312: ||                      mnudep(:,k) = nnudep(:,k)*mi0
  1313: ||                   elsewhere
  1314: ||                      nnuccc(:,k) = 0._r8
  1315: ||                      mnuccc(:,k) = 0._r8
  1316: ||        
  1317: ||                      nnucct(:,k) = 0._r8
  1318: ||                      mnucct(:,k) = 0._r8
  1319: ||        
  1320: ||                      nnudep(:,k) = 0._r8
  1321: ||                      mnudep(:,k) = 0._r8
  1322: |V=====              end where
  1323: |         
  1324: |                 end if
  1325: |         
  1326: |              else
  1327: |V=====>          mnuccc(:,k)=0._r8
  1328: |V=====>          nnuccc(:,k)=0._r8
  1329: |V=====>          mnucct(:,k)=0._r8
  1330: |V=====>          nnucct(:,k)=0._r8
  1331: |V=====>          mnudep(:,k)=0._r8
  1332: |V=====>          nnudep(:,k)=0._r8
  1333: |              end if
  1334: |         
  1335: |              call snow_self_aggregation(t(:,k), rho(:,k), asn(:,k), rhosn, qsic(:,k), nsic(:,k), &
  1336: |                   nsagg(:,k), mgncol)
  1337: |         
  1338: |              call accrete_cloud_water_snow(t(:,k), rho(:,k), asn(:,k), uns(:,k), mu(:,k), &
  1339: |                   qcic(1:mgncol,k), ncic(:,k), qsic(:,k), pgam(:,k), lamc(:,k), lams(:,k), n0s(:,k), &
  1340: |                   psacws(:,k), npsacws(:,k), mgncol)
  1341: |         
  1342: |              if (do_cldice) then
  1343: |                 call secondary_ice_production(t(:,k), psacws(:,k), msacwi(:,k), nsacwi(:,k), mgncol)
  1344: |              else
  1345: |V=====>          nsacwi(:,k) = 0.0_r8
  1346: |V=====>          msacwi(:,k) = 0.0_r8
  1347: |              end if
  1348: |         
  1349: |              call accrete_rain_snow(t(:,k), rho(:,k), umr(:,k), ums(:,k), unr(:,k), uns(:,k), &
  1350: |                   qric(:,k), qsic(:,k), lamr(:,k), n0r(:,k), lams(:,k), n0s(:,k), &
  1351: |                   pracs(:,k), npracs(:,k), mgncol)
  1352: |         
  1353: |              call heterogeneous_rain_freezing(t(:,k), qric(:,k), nric(:,k), lamr(:,k), &
  1354: |                   mnuccr(:,k), nnuccr(:,k), mgncol)
  1355: |         
  1356: |              if (do_sb_physics) then
  1357: |                call sb2001v2_accre_cld_water_rain(qcic(1:mgncol,k), ncic(:,k), qric(:,k), &
  1358: |                     rho(:,k), relvar(:,k), pra(:,k), npra(:,k), mgncol)
  1359: |              else
  1360: |                call accrete_cloud_water_rain(microp_uniform, qric(:,k), qcic(1:mgncol,k), &
  1361: |                     ncic(:,k), relvar(:,k), accre_enhan(:,k), pra(:,k), npra(:,k), mgncol)
  1362: |              endif
  1363: |         
  1364: |              call self_collection_rain(rho(:,k), qric(:,k), nric(:,k), nragg(:,k), mgncol)
  1365: |         
  1366: |              if (do_cldice) then
  1367: |                 call accrete_cloud_ice_snow(t(:,k), rho(:,k), asn(:,k), qiic(:,k), niic(:,k), &
  1368: |                      qsic(:,k), lams(:,k), n0s(:,k), prai(:,k), nprai(:,k), mgncol)
  1369: |              else
  1370: |V=====>          prai(:,k) = 0._r8
  1371: |V=====>          nprai(:,k) = 0._r8
  1372: |              end if
  1373: |         
  1374: |              call evaporate_sublimate_precip(t(:,k), rho(:,k), &
  1375: |                   dv(:,k), mu(:,k), sc(:,k), q(:,k), qvl(:,k), qvi(:,k), &
  1376: |                   lcldm(:,k), precip_frac(:,k), arn(:,k), asn(:,k), qcic(1:mgncol,k), qiic(:,k), &
  1377: |                   qric(:,k), qsic(:,k), lamr(:,k), n0r(:,k), lams(:,k), n0s(:,k), &
  1378: |                   pre(:,k), prds(:,k), am_evp_st(:,k), mgncol)
  1379: |         
  1380: |              call bergeron_process_snow(t(:,k), rho(:,k), dv(:,k), mu(:,k), sc(:,k), &
  1381: |                   qvl(:,k), qvi(:,k), asn(:,k), qcic(1:mgncol,k), qsic(:,k), lams(:,k), n0s(:,k), &
  1382: |                   bergs(:,k), mgncol)
  1383: |         
  1384: |V=====>       bergs(:,k)=bergs(:,k)*micro_mg_berg_eff_factor
  1385: |              !+++PMC 12/3/12 - NEW VAPOR DEP/SUBLIMATION GOES HERE!!!
  1386: |         
  1387: |              if (do_cldice) then
  1388: |         
  1389: |                 call ice_deposition_sublimation(t(:,k), q(:,k), qi(:,k), ni(:,k), &
  1390: |                      icldm(:,k), rho(:,k), dv(:,k), qvl(:,k), qvi(:,k), &
  1391: |                      berg(:,k), vap_dep(:,k), ice_sublim(:,k), mgncol)
  1392: |         
  1393: |V=====>          berg(:,k)=berg(:,k)*micro_mg_berg_eff_factor
  1394: |         
  1395: |V=====>          where (ice_sublim(:,k) < 0._r8 .and. qi(:,k) > qsmall .and. icldm(:,k) > mincld)
  1396: ||                   nsubi(:,k) = sublim_factor*ice_sublim(:,k) / qi(:,k) * ni(:,k) / icldm(:,k)
  1397: ||        
  1398: ||                elsewhere
  1399: ||                   nsubi(:,k) = 0._r8
  1400: |V=====           end where
  1401: |                 ! bergeron process should not reduce nc unless
  1402: |                 ! all ql is removed (which is handled elsewhere)
  1403: |                 !in fact, nothing in this entire file makes nsubc nonzero.
  1404: |         
  1405: |V=====>          nsubc(:,k) = 0._r8
  1406: |         
  1407: |              end if !do_cldice
  1408: |              !---PMC 12/3/12
  1409: |         
  1410: |V----->       do i=1,mgncol
  1411: ||                ! conservation to ensure no negative values of cloud water/precipitation
  1412: ||                ! in case microphysical process rates are large
  1413: ||                !===================================================================
  1414: ||                ! note: for check on conservation, processes are multiplied by omsm
  1415: ||                ! to prevent problems due to round off error
  1416: ||                ! conservation of qc
  1417: ||                !-------------------------------------------------------------------
  1418: ||        
  1419: ||        
  1420: ||                dum = ((prc(i,k)+pra(i,k)+mnuccc(i,k)+mnucct(i,k)+msacwi(i,k)+ &
  1421: ||                     psacws(i,k)+bergs(i,k))*lcldm(i,k)+berg(i,k))*deltat
  1422: ||        
  1423: ||                if (dum.gt.qc(i,k)) then
  1424: ||                   ratio = qc(i,k)/deltat/((prc(i,k)+pra(i,k)+mnuccc(i,k)+mnucct(i,k)+ &
  1425: ||                        msacwi(i,k)+psacws(i,k)+bergs(i,k))*lcldm(i,k)+berg(i,k))*omsm
  1426: ||                   prc(i,k) = prc(i,k)*ratio
  1427: ||                   pra(i,k) = pra(i,k)*ratio
  1428: ||                   mnuccc(i,k) = mnuccc(i,k)*ratio
  1429: ||                   mnucct(i,k) = mnucct(i,k)*ratio
  1430: ||                   msacwi(i,k) = msacwi(i,k)*ratio
  1431: ||                   psacws(i,k) = psacws(i,k)*ratio
  1432: ||                   bergs(i,k) = bergs(i,k)*ratio
  1433: ||                   berg(i,k) = berg(i,k)*ratio
  1434: ||                   qcrat(i,k) = ratio
  1435: ||                else
  1436: ||                   qcrat(i,k) = 1._r8
  1437: ||                end if
  1438: ||                !PMC 12/3/12: ratio is also frac of step w/ liquid.
  1439: ||                !thus we apply berg for "ratio" of timestep and vapor
  1440: ||                !deposition for the remaining frac of the timestep.
  1441: ||        
  1442: ||                if (qc(i,k) >= qsmall) then
  1443: ||                   vap_dep(i,k) = vap_dep(i,k)*(1._r8-qcrat(i,k))
  1444: ||                end if
  1445: ||        
  1446: |V-----        end do
  1447: |         
  1448: |V----->       do i=1,mgncol
  1449: ||                !=================================================================
  1450: ||                ! apply limiter to ensure that ice/snow sublimation and rain evap
  1451: ||                ! don't push conditions into supersaturation, and ice deposition/nucleation don't
  1452: ||                ! push conditions into sub-saturation
  1453: ||                ! note this is done after qc conservation since we don't know how large
  1454: ||                ! vap_dep is before then
  1455: ||                ! estimates are only approximate since other process terms haven't been limited
  1456: ||                ! for conservation yet
  1457: ||                ! first limit ice deposition/nucleation vap_dep + mnuccd
  1458: ||        
  1459: ||        
  1460: ||                dum1 = vap_dep(i,k) + mnuccd(i,k)
  1461: ||                if (dum1 > 1.e-20_r8) then
  1462: ||                   dum = (q(i,k)-qvi(i,k))/(1._r8 + xxls_squared*qvi(i,k)/(cpp*rv*t(i,k)**2))/deltat
  1463: ||                   dum = max(dum,0._r8)
  1464: ||                   if (dum1 > dum) then
  1465: ||                      ! Allocate the limited "dum" tendency to mnuccd and vap_dep
  1466: ||                      ! processes. Don't divide by cloud fraction; these are grid-
  1467: ||                      ! mean rates.
  1468: ||                      dum1 = mnuccd(i,k) / (vap_dep(i,k)+mnuccd(i,k))
  1469: ||                      mnuccd(i,k) = dum*dum1
  1470: ||                      vap_dep(i,k) = dum - mnuccd(i,k)
  1471: ||                   end if
  1472: ||                end if
  1473: ||        
  1474: |V-----        end do
  1475: |         
  1476: |V----->       do i=1,mgncol
  1477: ||                !===================================================================
  1478: ||                ! conservation of nc
  1479: ||                !-------------------------------------------------------------------
  1480: ||        
  1481: ||                dum = (nprc1(i,k)+npra(i,k)+nnuccc(i,k)+nnucct(i,k)+ &
  1482: ||                     npsacws(i,k)-nsubc(i,k))*lcldm(i,k)*deltat
  1483: ||        
  1484: ||                if (dum.gt.nc(i,k)) then
  1485: ||                   ratio = nc(i,k)/deltat/((nprc1(i,k)+npra(i,k)+nnuccc(i,k)+nnucct(i,k)+&
  1486: ||                        npsacws(i,k)-nsubc(i,k))*lcldm(i,k))*omsm
  1487: ||        
  1488: ||                   nprc1(i,k) = nprc1(i,k)*ratio
  1489: ||                   npra(i,k) = npra(i,k)*ratio
  1490: ||                   nnuccc(i,k) = nnuccc(i,k)*ratio
  1491: ||                   nnucct(i,k) = nnucct(i,k)*ratio
  1492: ||                   npsacws(i,k) = npsacws(i,k)*ratio
  1493: ||                   nsubc(i,k)=nsubc(i,k)*ratio
  1494: ||                end if
  1495: ||        
  1496: ||                mnuccri(i,k)=0._r8
  1497: ||                nnuccri(i,k)=0._r8
  1498: ||        
  1499: ||                if (do_cldice) then
  1500: ||                   ! freezing of rain to produce ice if mean rain size is smaller than Dcs
  1501: ||        
  1502: ||                   if (lamr(i,k) > qsmall .and. 1._r8/lamr(i,k) < Dcs) then
  1503: ||                      mnuccri(i,k)=mnuccr(i,k)
  1504: ||                      nnuccri(i,k)=nnuccr(i,k)
  1505: ||                      mnuccr(i,k)=0._r8
  1506: ||                      nnuccr(i,k)=0._r8
  1507: ||                   end if
  1508: ||                end if
  1509: ||        
  1510: |V-----        end do
  1511: |         
  1512: |V----->       do i=1,mgncol
  1513: ||                ! conservation of rain mixing ratio
  1514: ||                !-------------------------------------------------------------------
  1515: ||        
  1516: ||                dum = ((-pre(i,k)+pracs(i,k)+mnuccr(i,k)+mnuccri(i,k))*precip_frac(i,k)- &
  1517: ||                     (pra(i,k)+prc(i,k))*lcldm(i,k))*deltat
  1518: ||                ! note that qrtend is included below because of instantaneous freezing/melt
  1519: ||        
  1520: ||                if (dum.gt.qr(i,k).and. &
  1521: ||                     (-pre(i,k)+pracs(i,k)+mnuccr(i,k)+mnuccri(i,k)).ge.qsmall) then
  1522: ||                   ratio = (qr(i,k)/deltat+(pra(i,k)+prc(i,k))*lcldm(i,k))/   &
  1523: ||                        precip_frac(i,k)/(-pre(i,k)+pracs(i,k)+mnuccr(i,k)+mnuccri(i,k))*omsm
  1524: ||                   pre(i,k)=pre(i,k)*ratio
  1525: ||                   pracs(i,k)=pracs(i,k)*ratio
  1526: ||                   mnuccr(i,k)=mnuccr(i,k)*ratio
  1527: ||                   mnuccri(i,k)=mnuccri(i,k)*ratio
  1528: ||                end if
  1529: ||        
  1530: |V-----        end do
  1531: |         
  1532: |V----->       do i=1,mgncol
  1533: ||                ! conservation of rain number
  1534: ||                !-------------------------------------------------------------------
  1535: ||                ! Add evaporation of rain number.
  1536: ||        
  1537: ||        
  1538: ||                if (pre(i,k) < 0._r8) then
  1539: ||                   dum = pre(i,k)*deltat/qr(i,k)
  1540: ||                   dum = max(-1._r8,dum)
  1541: ||                   nsubr(i,k) = dum*nr(i,k)/deltat
  1542: ||                else
  1543: ||                   nsubr(i,k) = 0._r8
  1544: ||                end if
  1545: ||        
  1546: ||             end do
  1547: ||        
  1548: ||             do i=1,mgncol
  1549: ||        
  1550: ||                dum = ((-nsubr(i,k)+npracs(i,k)+nnuccr(i,k)+nnuccri(i,k)-nragg(i,k))*precip_frac(i,k)- &
  1551: ||                     nprc(i,k)*lcldm(i,k))*deltat
  1552: ||        
  1553: ||                if (dum.gt.nr(i,k)) then
  1554: ||                   ratio = (nr(i,k)/deltat+nprc(i,k)*lcldm(i,k))/precip_frac(i,k)/ &
  1555: ||                        (-nsubr(i,k)+npracs(i,k)+nnuccr(i,k)+nnuccri(i,k)-nragg(i,k))*omsm
  1556: ||        
  1557: ||                   nragg(i,k)=nragg(i,k)*ratio
  1558: ||                   npracs(i,k)=npracs(i,k)*ratio
  1559: ||                   nnuccr(i,k)=nnuccr(i,k)*ratio
  1560: ||                   nsubr(i,k)=nsubr(i,k)*ratio
  1561: ||                   nnuccri(i,k)=nnuccri(i,k)*ratio
  1562: ||                end if
  1563: ||        
  1564: |V-----        end do
  1565: |         
  1566: |              if (do_cldice) then
  1567: |         
  1568: |V----->          do i=1,mgncol
  1569: ||                   ! conservation of qi
  1570: ||                   !-------------------------------------------------------------------
  1571: ||        
  1572: ||        
  1573: ||                   dum = ((-mnuccc(i,k)-mnucct(i,k)-mnudep(i,k)-msacwi(i,k))*lcldm(i,k)+(prci(i,k)+ &
  1574: ||                        prai(i,k))*icldm(i,k)-mnuccri(i,k)*precip_frac(i,k) &
  1575: ||                        -ice_sublim(i,k)-vap_dep(i,k)-berg(i,k)-mnuccd(i,k))*deltat
  1576: ||        
  1577: ||                   if (dum.gt.qi(i,k)) then
  1578: ||                      ratio = (qi(i,k)/deltat+vap_dep(i,k)+berg(i,k)+mnuccd(i,k)+ &
  1579: ||                           (mnuccc(i,k)+mnucct(i,k)+mnudep(i,k)+msacwi(i,k))*lcldm(i,k)+ &
  1580: ||                           mnuccri(i,k)*precip_frac(i,k))/ &
  1581: ||                           ((prci(i,k)+prai(i,k))*icldm(i,k)-ice_sublim(i,k))*omsm
  1582: ||                      prci(i,k) = prci(i,k)*ratio
  1583: ||                      prai(i,k) = prai(i,k)*ratio
  1584: ||                      ice_sublim(i,k) = ice_sublim(i,k)*ratio
  1585: ||                   end if
  1586: ||        
  1587: |V-----           end do
  1588: |         
  1589: |              end if
  1590: |         
  1591: |              if (do_cldice) then
  1592: |         
  1593: |V----->          do i=1,mgncol
  1594: ||                   ! conservation of ni
  1595: ||                   !-------------------------------------------------------------------
  1596: ||        
  1597: ||                   if (use_hetfrz_classnuc) then
  1598: ||                      tmpfrz = nnuccc(i,k)
  1599: ||                   else
  1600: ||                      tmpfrz = 0._r8
  1601: ||                   end if
  1602: ||                   dum = ((-nnucct(i,k)-tmpfrz-nnudep(i,k)-nsacwi(i,k))*lcldm(i,k)+(nprci(i,k)+ &
  1603: ||                        nprai(i,k)-nsubi(i,k))*icldm(i,k)-nnuccri(i,k)*precip_frac(i,k)- &
  1604: ||                        nnuccd(i,k))*deltat
  1605: ||        
  1606: ||                   if (dum.gt.ni(i,k)) then
  1607: ||                      ratio = (ni(i,k)/deltat+nnuccd(i,k)+ &
  1608: ||                           (nnucct(i,k)+tmpfrz+nnudep(i,k)+nsacwi(i,k))*lcldm(i,k)+ &
  1609: ||                           nnuccri(i,k)*precip_frac(i,k))/ &
  1610: ||                           ((nprci(i,k)+nprai(i,k)-nsubi(i,k))*icldm(i,k))*omsm
  1611: ||                      nprci(i,k) = nprci(i,k)*ratio
  1612: ||                      nprai(i,k) = nprai(i,k)*ratio
  1613: ||                      nsubi(i,k) = nsubi(i,k)*ratio
  1614: ||                   end if
  1615: ||        
  1616: |V-----           end do
  1617: |         
  1618: |              end if
  1619: |         
  1620: |V----->       do i=1,mgncol
  1621: ||                ! conservation of snow mixing ratio
  1622: ||                !-------------------------------------------------------------------
  1623: ||        
  1624: ||                dum = (-(prds(i,k)+pracs(i,k)+mnuccr(i,k))*precip_frac(i,k)-(prai(i,k)+prci(i,k))*icldm(i,k) &
  1625: ||                     -(bergs(i,k)+psacws(i,k))*lcldm(i,k))*deltat
  1626: ||        
  1627: ||                if (dum.gt.qs(i,k).and.-prds(i,k).ge.qsmall) then
  1628: ||                   ratio = (qs(i,k)/deltat+(prai(i,k)+prci(i,k))*icldm(i,k)+ &
  1629: ||                        (bergs(i,k)+psacws(i,k))*lcldm(i,k)+(pracs(i,k)+mnuccr(i,k))*precip_frac(i,k))/ &
  1630: ||                        precip_frac(i,k)/(-prds(i,k))*omsm
  1631: ||                   prds(i,k)=prds(i,k)*ratio
  1632: ||                end if
  1633: ||        
  1634: |V-----        end do
  1635: |         
  1636: |V----->       do i=1,mgncol
  1637: ||                ! conservation of snow number
  1638: ||                !-------------------------------------------------------------------
  1639: ||                ! calculate loss of number due to sublimation
  1640: ||                ! for now neglect sublimation of ns
  1641: ||        
  1642: ||                nsubs(i,k)=0._r8
  1643: ||        
  1644: ||                dum = ((-nsagg(i,k)-nsubs(i,k)-nnuccr(i,k))*precip_frac(i,k)-nprci(i,k)*icldm(i,k))*deltat
  1645: ||        
  1646: ||                if (dum.gt.ns(i,k)) then
  1647: ||                   ratio = (ns(i,k)/deltat+nnuccr(i,k)* &
  1648: ||                        precip_frac(i,k)+nprci(i,k)*icldm(i,k))/precip_frac(i,k)/ &
  1649: ||                        (-nsubs(i,k)-nsagg(i,k))*omsm
  1650: ||                   nsubs(i,k)=nsubs(i,k)*ratio
  1651: ||                   nsagg(i,k)=nsagg(i,k)*ratio
  1652: ||                end if
  1653: ||        
  1654: ||             end do
  1655: ||        
  1656: ||             do i=1,mgncol
  1657: ||                ! next limit ice and snow sublimation and rain evaporation
  1658: ||                ! get estimate of q and t at end of time step
  1659: ||                ! don't include other microphysical processes since they haven't
  1660: ||                ! been limited via conservation checks yet
  1661: ||        
  1662: ||        
  1663: ||        
  1664: ||                   qtmpA(i)=q(i,k)-(ice_sublim(i,k)+vap_dep(i,k)+mnuccd(i,k)+ &
  1665: ||                        (pre(i,k)+prds(i,k))*precip_frac(i,k))*deltat
  1666: ||                   ttmpA(i)=t(i,k)+((pre(i,k)*precip_frac(i,k))*xxlv+ &
  1667: ||                        (prds(i,k)*precip_frac(i,k)+vap_dep(i,k)+ice_sublim(i,k)+mnuccd(i,k))*xxls)*deltat/cpp
  1668: ||                   ! use rhw to allow ice supersaturation
  1669: |V-----        enddo
  1670: |              ! modify ice/precip evaporation rate if q > qsat
  1671: |              call qsat_water(ttmpA, p(:,k), esnA, qvnAI,mgncol)
  1672: |         
  1673: |V----->       do i=1,mgncol
  1674: ||                if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_r8) then
  1675: ||                   if (qtmpA(i) > qvnAI(i)) then
  1676: ||        
  1677: ||                      dum1A(i)=pre(i,k)*precip_frac(i,k)/((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k))
  1678: ||                      dum2A(i)=prds(i,k)*precip_frac(i,k)/((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k))
  1679: ||                      ! recalculate q and t after vap_dep and mnuccd but without evap or sublim
  1680: ||                      qtmpA(i)=q(i,k)-(vap_dep(i,k)+mnuccd(i,k))*deltat
  1681: ||                      ttmpA(i)=t(i,k)+((vap_dep(i,k)+mnuccd(i,k))*xxls)*deltat/cpp
  1682: ||                   endif
  1683: ||                 endif
  1684: |V-----         enddo
  1685: |               ! use rhw to allow ice supersaturation
  1686: |               call qsat_water(ttmpA, p(:,k), esnA, qvnA,mgncol)
  1687: |         
  1688: |V----->       do i=1,mgncol
  1689: ||                if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_r8) then
  1690: ||                   if (qtmpA(i) > qvnAI(i)) then
  1691: ||        
  1692: ||                      dum=(qtmpA(i)-qvnA(i))/(1._r8 + xxlv_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))
  1693: ||                      dum=min(dum,0._r8)
  1694: ||                      ! modify rates if needed, divide by precip_frac to get local (in-precip) value
  1695: ||        
  1696: ||                      pre(i,k)=dum*dum1A(i)/deltat/precip_frac(i,k)
  1697: ||                      ! do separately using RHI for prds and ice_sublim
  1698: ||        #if 1
  1699: ||                   endif
  1700: ||                endif
  1701: |V-----        enddo
  1702: |         
  1703: |                   call qsat_ice(ttmpA, p(:,k), esnA, qvnA,mgncol)
  1704: |         
  1705: |V----->       do i=1,mgncol
  1706: ||                if ((pre(i,k)+prds(i,k))*precip_frac(i,k)+ice_sublim(i,k) < -1.e-20_r8) then
  1707: ||                   if (qtmpA(i) > qvnAI(i)) then
  1708: ||        #else
  1709: ||        
  1710: ||                  call qsat_ice(ttmpA(i), p(i,k), esnA(i), qvnA(i))
  1711: ||        #endif
  1712: ||        
  1713: ||                      dum=(qtmpA(i)-qvnA(i))/(1._r8 + xxls_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))
  1714: ||                      dum=min(dum,0._r8)
  1715: ||                      ! modify rates if needed, divide by precip_frac to get local (in-precip) value
  1716: ||        
  1717: ||                      prds(i,k) = dum*dum2A(i)/deltat/precip_frac(i,k)
  1718: ||                      ! don't divide ice_sublim by cloud fraction since it is grid-averaged
  1719: ||        
  1720: ||                      dum1A(i) = (1._r8-dum1A(i)-dum2A(i))
  1721: ||                      ice_sublim(i,k) = dum*dum1A(i)/deltat
  1722: ||                   end if
  1723: ||                end if
  1724: ||        
  1725: |V-----        end do
  1726: |              ! Big "administration" loop enforces conservation, updates variables
  1727: |              ! that accumulate over substeps, and sets output variables.
  1728: |         
  1729: |         
  1730: |V----->       do i=1,mgncol
  1731: ||                ! get tendencies due to microphysical conversion processes
  1732: ||                !==========================================================
  1733: ||                ! note: tendencies are multiplied by appropriate cloud/precip
  1734: ||                ! fraction to get grid-scale values
  1735: ||                ! note: vap_dep is already grid-average values
  1736: ||                ! The net tendencies need to be added to rather than overwritten,
  1737: ||                ! because they may have a value already set for instantaneous
  1738: ||                ! melting/freezing.
  1739: ||        
  1740: ||        
  1741: ||                qvlat(i,k) = qvlat(i,k)-(pre(i,k)+prds(i,k))*precip_frac(i,k)-&
  1742: ||                     vap_dep(i,k)-ice_sublim(i,k)-mnuccd(i,k)-mnudep(i,k)*lcldm(i,k)
  1743: ||        
  1744: ||                tlat(i,k) = tlat(i,k)+((pre(i,k)*precip_frac(i,k)) &
  1745: ||                     *xxlv+(prds(i,k)*precip_frac(i,k)+vap_dep(i,k)+ice_sublim(i,k)+mnuccd(i,k)+mnudep(i,k)*lcldm(i,k))*xxls+ &
  1746: ||                     ((bergs(i,k)+psacws(i,k)+mnuccc(i,k)+mnucct(i,k)+msacwi(i,k))*lcldm(i,k)+(mnuccr(i,k)+ &
  1747: ||                     pracs(i,k)+mnuccri(i,k))*precip_frac(i,k)+berg(i,k))*xlf)
  1748: ||        
  1749: ||                qctend(i,k) = qctend(i,k)+ &
  1750: ||                     (-pra(i,k)-prc(i,k)-mnuccc(i,k)-mnucct(i,k)-msacwi(i,k)- &
  1751: ||                     psacws(i,k)-bergs(i,k))*lcldm(i,k)-berg(i,k)
  1752: ||        
  1753: ||                if (do_cldice) then
  1754: ||                   qitend(i,k) = qitend(i,k)+ &
  1755: ||                        (mnuccc(i,k)+mnucct(i,k)+mnudep(i,k)+msacwi(i,k))*lcldm(i,k)+(-prci(i,k)- &
  1756: ||                        prai(i,k))*icldm(i,k)+vap_dep(i,k)+berg(i,k)+ice_sublim(i,k)+ &
  1757: ||                        mnuccd(i,k)+mnuccri(i,k)*precip_frac(i,k)
  1758: ||                end if
  1759: ||        
  1760: ||                qrtend(i,k) = qrtend(i,k)+ &
  1761: ||                     (pra(i,k)+prc(i,k))*lcldm(i,k)+(pre(i,k)-pracs(i,k)- &
  1762: ||                     mnuccr(i,k)-mnuccri(i,k))*precip_frac(i,k)
  1763: ||        
  1764: ||                qstend(i,k) = qstend(i,k)+ &
  1765: ||                     (prai(i,k)+prci(i,k))*icldm(i,k)+(psacws(i,k)+bergs(i,k))*lcldm(i,k)+(prds(i,k)+ &
  1766: ||                     pracs(i,k)+mnuccr(i,k))*precip_frac(i,k)
  1767: ||        
  1768: ||        
  1769: ||                cmeout(i,k) = vap_dep(i,k) + ice_sublim(i,k) + mnuccd(i,k)
  1770: ||                ! add output for cmei (accumulate)
  1771: ||        
  1772: ||                cmeitot(i,k) = vap_dep(i,k) + ice_sublim(i,k) + mnuccd(i,k)
  1773: ||                ! assign variables for trop_mozart, these are grid-average
  1774: ||                !-------------------------------------------------------------------
  1775: ||                ! evaporation/sublimation is stored here as positive term
  1776: ||        
  1777: ||        
  1778: ||                evapsnow(i,k) = -prds(i,k)*precip_frac(i,k)
  1779: ||                nevapr(i,k) = -pre(i,k)*precip_frac(i,k)
  1780: ||                prer_evap(i,k) = -pre(i,k)*precip_frac(i,k)
  1781: ||                ! change to make sure prain is positive: do not remove snow from
  1782: ||                ! prain used for wet deposition
  1783: ||        
  1784: ||                prain(i,k) = (pra(i,k)+prc(i,k))*lcldm(i,k)+(-pracs(i,k)- &
  1785: ||                     mnuccr(i,k)-mnuccri(i,k))*precip_frac(i,k)
  1786: ||                prodsnow(i,k) = (prai(i,k)+prci(i,k))*icldm(i,k)+(psacws(i,k)+bergs(i,k))*lcldm(i,k)+(&
  1787: ||                     pracs(i,k)+mnuccr(i,k))*precip_frac(i,k)
  1788: ||                ! following are used to calculate 1st order conversion rate of cloud water
  1789: ||                !    to rain and snow (1/s), for later use in aerosol wet removal routine
  1790: ||                ! previously, wetdepa used (prain/qc) for this, and the qc in wetdepa may be smaller than the qc
  1791: ||                !    used to calculate pra, prc, ... in this routine
  1792: ||                ! qcsinksum_rate1ord = { rate of direct transfer of cloud water to rain & snow }
  1793: ||                !                      (no cloud ice or bergeron terms)
  1794: ||        
  1795: ||                qcsinksum_rate1ord(i,k) = (pra(i,k)+prc(i,k)+psacws(i,k))*lcldm(i,k)
  1796: ||                ! Avoid zero/near-zero division.
  1797: ||                qcsinksum_rate1ord(i,k) = qcsinksum_rate1ord(i,k) / &
  1798: ||                     max(qc(i,k),1.0e-30_r8)
  1799: ||                ! microphysics output, note this is grid-averaged
  1800: ||        
  1801: ||        
  1802: ||                pratot(i,k) = pra(i,k)*lcldm(i,k)
  1803: ||                prctot(i,k) = prc(i,k)*lcldm(i,k)
  1804: ||                mnuccctot(i,k) = mnuccc(i,k)*lcldm(i,k)
  1805: ||                mnuccttot(i,k) = mnucct(i,k)*lcldm(i,k)
  1806: ||                msacwitot(i,k) = msacwi(i,k)*lcldm(i,k)
  1807: ||                psacwstot(i,k) = psacws(i,k)*lcldm(i,k)
  1808: ||                bergstot(i,k) = bergs(i,k)*lcldm(i,k)
  1809: ||                bergtot(i,k) = berg(i,k)
  1810: ||                prcitot(i,k) = prci(i,k)*icldm(i,k)
  1811: ||                praitot(i,k) = prai(i,k)*icldm(i,k)
  1812: ||                mnuccdtot(i,k) = mnuccd(i,k)*icldm(i,k)
  1813: ||        
  1814: ||                pracstot(i,k) = pracs(i,k)*precip_frac(i,k)
  1815: ||                mnuccrtot(i,k) = mnuccr(i,k)*precip_frac(i,k)
  1816: ||        
  1817: ||        
  1818: ||                nctend(i,k) = nctend(i,k)+&
  1819: ||                     (-nnuccc(i,k)-nnucct(i,k)-npsacws(i,k)+nsubc(i,k) &
  1820: ||                     -npra(i,k)-nprc1(i,k))*lcldm(i,k)
  1821: ||        
  1822: ||                if (do_cldice) then
  1823: ||                   if (use_hetfrz_classnuc) then
  1824: ||                      tmpfrz = nnuccc(i,k)
  1825: ||                   else
  1826: ||                      tmpfrz = 0._r8
  1827: ||                   end if
  1828: ||                   nitend(i,k) = nitend(i,k)+ nnuccd(i,k)+ &
  1829: ||                        (nnucct(i,k)+tmpfrz+nnudep(i,k)+nsacwi(i,k))*lcldm(i,k)+(nsubi(i,k)-nprci(i,k)- &
  1830: ||                        nprai(i,k))*icldm(i,k)+nnuccri(i,k)*precip_frac(i,k)
  1831: ||                end if
  1832: ||        
  1833: ||                nstend(i,k) = nstend(i,k)+(nsubs(i,k)+ &
  1834: ||                     nsagg(i,k)+nnuccr(i,k))*precip_frac(i,k)+nprci(i,k)*icldm(i,k)
  1835: ||        
  1836: ||                nrtend(i,k) = nrtend(i,k)+ &
  1837: ||                     nprc(i,k)*lcldm(i,k)+(nsubr(i,k)-npracs(i,k)-nnuccr(i,k) &
  1838: ||                     -nnuccri(i,k)+nragg(i,k))*precip_frac(i,k)
  1839: ||                ! make sure that ni at advanced time step does not exceed
  1840: ||                ! maximum (existing N + source terms*dt), which is possible if mtime < deltat
  1841: ||                ! note that currently mtime = deltat
  1842: ||                !================================================================
  1843: ||        
  1844: ||        
  1845: ||                if (do_cldice .and. nitend(i,k).gt.0._r8.and.ni(i,k)+nitend(i,k)*deltat.gt.nimax(i,k)) then
  1846: ||                   nitend(i,k)=max(0._r8,(nimax(i,k)-ni(i,k))/deltat)
  1847: ||                end if
  1848: ||        
  1849: |V-----        end do
  1850: |              ! End of "administration" loop
  1851: |         
  1852: |         
  1853: +------     end do micro_vert_loop ! end k loop
  1854:             !-----------------------------------------------------
  1855:             ! convert rain/snow q and N for output to history, note,
  1856:             ! output is for gridbox average
  1857:           
  1858:           
  1859: V======>    qrout = qr
  1860: V======>    nrout = nr * rho
  1861: V======>    qsout = qs
  1862: V======>    nsout = ns * rho
  1863:             ! calculate n0r and lamr from rain mass and number
  1864:             ! divide by precip fraction to get in-precip (local) values of
  1865:             ! rain mass and number, divide by rhow to get rain number in kg^-1
  1866:           
  1867:           
  1868: +------>    do k=1,nlev
  1869: |         
  1870: |              call size_dist_param_basic(mg_rain_props, qric(:,k), nric(:,k), lamr(:,k), mgncol, n0=n0r(:,k))
  1871: |              ! Calculate rercld
  1872: |              ! calculate mean size of combined rain and cloud water
  1873: |         
  1874: |         
  1875: |       I      call calc_rercld(lamr(:,k), n0r(:,k), lamc(:,k), pgam(:,k), qric(:,k), qcic(1:mgncol,k), ncic(:,k), &
  1876: |                   rercld(:,k), mgncol)
  1877: |         
  1878: +------     enddo
  1879:             ! Assign variables back to start-of-timestep values
  1880:             ! Some state variables are changed before the main microphysics loop
  1881:             ! to make "instantaneous" adjustments. Afterward, we must move those changes
  1882:             ! back into the tendencies.
  1883:             ! These processes:
  1884:             !  - Droplet activation (npccn, impacts nc)
  1885:             !  - Instantaneous snow melting  (minstsm/ninstsm, impacts qr/qs/nr/ns)
  1886:             !  - Instantaneous rain freezing (minstfr/ninstrf, impacts qr/qs/nr/ns)
  1887:             !================================================================================
  1888:             ! Re-apply droplet activation tendency
  1889:           
  1890:           
  1891: V======>    nc = ncn
  1892: V======>    nctend = nctend + npccn
  1893:             ! Re-apply rain freezing and snow melting.
  1894:           
  1895: V======>    dum_2D = qs
  1896: V======>    qs = qsn
  1897: V======>    qstend = qstend + (dum_2D-qs)/deltat
  1898:           
  1899: V======>    dum_2D = ns
  1900: V======>    ns = nsn
  1901: V======>    nstend = nstend + (dum_2D-ns)/deltat
  1902:           
  1903: V======>    dum_2D = qr
  1904: V======>    qr = qrn
  1905: V======>    qrtend = qrtend + (dum_2D-qr)/deltat
  1906:           
  1907: V======>    dum_2D = nr
  1908: V======>    nr = nrn
  1909: V======>    nrtend = nrtend + (dum_2D-nr)/deltat
  1910:             !.............................................................................
  1911:             !================================================================================
  1912:             ! modify to include snow. in prain & evap (diagnostic here: for wet dep)
  1913:           
  1914:           
  1915: V======>    nevapr = nevapr + evapsnow
  1916: V======>    prain = prain + prodsnow
  1917:           
  1918:           
  1919: W------>    do k=1,nlev
  1920: |         
  1921: |*----->       do i=1,mgncol
  1922: ||                ! calculate sedimentation for cloud water and ice
  1923: ||                !================================================================================
  1924: ||                ! update in-cloud cloud mixing ratio and number concentration
  1925: ||                ! with microphysical tendencies to calculate sedimentation, assign to dummy vars
  1926: ||                ! note: these are in-cloud values***, hence we divide by cloud fraction
  1927: ||        
  1928: ||        
  1929: ||                dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)/lcldm(i,k)
  1930: ||                dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)/icldm(i,k)
  1931: ||                dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k),0._r8)
  1932: ||                dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat)/icldm(i,k),0._r8)
  1933: ||        
  1934: ||                dumr(i,k) = (qr(i,k)+qrtend(i,k)*deltat)/precip_frac(i,k)
  1935: ||                dumnr(i,k) = max((nr(i,k)+nrtend(i,k)*deltat)/precip_frac(i,k),0._r8)
  1936: ||                dums(i,k) = (qs(i,k)+qstend(i,k)*deltat)/precip_frac(i,k)
  1937: ||                dumns(i,k) = max((ns(i,k)+nstend(i,k)*deltat)/precip_frac(i,k),0._r8)
  1938: ||                ! switch for specification of droplet and crystal number
  1939: ||        
  1940: ||        
  1941: ||                if (nccons) then
  1942: ||                   dumnc(i,k)=ncnst/rho(i,k)
  1943: ||                end if
  1944: ||                ! switch for specification of cloud ice number
  1945: ||        
  1946: ||                if (nicons) then
  1947: ||                   dumni(i,k)=ninst/rho(i,k)
  1948: ||                end if
  1949: |*-----        enddo
  1950: W------     enddo
  1951:           
  1952: +------>    do k=1,nlev
  1953: |              ! obtain new slope parameter to avoid possible singularity
  1954: |         
  1955: |         
  1956: |              call size_dist_param_basic(mg_ice_props, dumi(:,k), dumni(:,k), &
  1957: |                   lami(:,k), mgncol)
  1958: |         
  1959: |              call size_dist_param_liq(mg_liq_props, dumc(:,k), dumnc(:,k), rho(:,k), &
  1960: |                   pgam(:,k), lamc(:,k), mgncol)
  1961: |         
  1962: +------     enddo
  1963:           
  1964: W------>    do k=1,nlev
  1965: |*----->       do i=1,mgncol
  1966: ||                ! calculate number and mass weighted fall velocity for droplets and cloud ice
  1967: ||                !-------------------------------------------------------------------
  1968: ||        
  1969: ||        
  1970: ||                if (dumc(i,k).ge.qsmall) then
  1971: ||        
  1972: ||                   vtrmc(i,k)=acn(i,k)*gamma(4._r8+bc+pgam(i,k))/ &
  1973: ||                        (lamc(i,k)**bc*gamma(pgam(i,k)+4._r8))
  1974: ||        
  1975: ||                   fc(i,k) = g*rho(i,k)*vtrmc(i,k)
  1976: ||        
  1977: ||                   fnc(i,k) = g*rho(i,k)* &
  1978: ||                        acn(i,k)*gamma(1._r8+bc+pgam(i,k))/ &
  1979: ||                        (lamc(i,k)**bc*gamma(pgam(i,k)+1._r8))
  1980: ||                else
  1981: ||                   fc(i,k) = 0._r8
  1982: ||                   fnc(i,k)= 0._r8
  1983: ||                end if
  1984: ||                ! calculate number and mass weighted fall velocity for cloud ice
  1985: ||        
  1986: ||        
  1987: ||                if (dumi(i,k).ge.qsmall) then
  1988: ||        
  1989: ||                   vtrmi(i,k)=min(ain(i,k)*gamma_bi_plus4/(6._r8*lami(i,k)**bi), &
  1990: ||                        1.2_r8*rhof(i,k))
  1991: ||        
  1992: ||                   fi(i,k) = g*rho(i,k)*vtrmi(i,k)
  1993: ||                   fni(i,k) = g*rho(i,k)* &
  1994: ||                        min(ain(i,k)*gamma_bi_plus1/lami(i,k)**bi,1.2_r8*rhof(i,k))
  1995: ||                   ! adjust the ice fall velocity for smaller (r < 20 um) ice
  1996: ||                   ! particles (blend over 18-20 um)
  1997: ||        
  1998: ||                   irad = 1.5_r8 / lami(i,k) * 1e6_r8
  1999: ||                   ifrac = min(1._r8, max(0._r8, (irad - 18._r8) / 2._r8))
  2000: ||         
  2001: ||                   if (ifrac .lt. 1._r8) then
  2002: ||                      vtrmi(i,k) = ifrac * vtrmi(i,k) + &
  2003: ||                         (1._r8 - ifrac) * &
  2004: ||                         min(ajn(i,k)*gamma_bj_plus4/(6._r8*lami(i,k)**bj), &
  2005: ||                         1.2_r8*rhof(i,k))
  2006: ||        
  2007: ||                      fi(i,k) = g*rho(i,k)*vtrmi(i,k)
  2008: ||                      fni(i,k) = ifrac * fni(i,k) + &
  2009: ||                         (1._r8 - ifrac) * &
  2010: ||                         g*rho(i,k)* &
  2011: ||                         min(ajn(i,k)*gamma_bj_plus1/lami(i,k)**bj,1.2_r8*rhof(i,k))
  2012: ||                   end if
  2013: ||                else
  2014: ||                   fi(i,k) = 0._r8
  2015: ||                   fni(i,k)= 0._r8
  2016: ||                end if
  2017: ||        
  2018: |*-----        enddo
  2019: |         
  2020: W------     enddo
  2021:           
  2022: +------>    do k=1,nlev
  2023: |                 ! fallspeed for rain
  2024: |         
  2025: |         
  2026: |                 call size_dist_param_basic(mg_rain_props, dumr(:,k), dumnr(:,k), &
  2027: |                      lamr(:,k), mgncol)
  2028: +------     enddo
  2029:           
  2030: +------>    do k=1,nlev
  2031: |         
  2032: |*----->       do i=1,mgncol
  2033: ||                if (lamr(i,k).ge.qsmall) then
  2034: ||                   ! 'final' values of number and mass weighted mean fallspeed for rain (m/s)
  2035: ||        
  2036: ||        
  2037: ||                   unr(i,k) = min(arn(i,k)*gamma_br_plus1/lamr(i,k)**br,9.1_r8*rhof(i,k))
  2038: ||                   umr(i,k) = min(arn(i,k)*gamma_br_plus4/(6._r8*lamr(i,k)**br),9.1_r8*rhof(i,k))
  2039: ||        
  2040: ||                   fr(i,k) = g*rho(i,k)*umr(i,k)
  2041: ||                   fnr(i,k) = g*rho(i,k)*unr(i,k)
  2042: ||        
  2043: ||                else
  2044: ||                   fr(i,k)=0._r8
  2045: ||                   fnr(i,k)=0._r8
  2046: ||                end if
  2047: ||                ! fallspeed for snow
  2048: ||        
  2049: ||        
  2050: ||                call size_dist_param_basic(mg_snow_props, dums(i,k), dumns(i,k), &
  2051: ||                     lams(i,k))
  2052: ||        
  2053: ||                if (lams(i,k).ge.qsmall) then
  2054: ||                   ! 'final' values of number and mass weighted mean fallspeed for snow (m/s)
  2055: ||        
  2056: ||                   ums(i,k) = min(asn(i,k)*gamma_bs_plus4/(6._r8*lams(i,k)**bs),1.2_r8*rhof(i,k))
  2057: ||                   uns(i,k) = min(asn(i,k)*gamma_bs_plus1/lams(i,k)**bs,1.2_r8*rhof(i,k))
  2058: ||        
  2059: ||                   fs(i,k) = g*rho(i,k)*ums(i,k)
  2060: ||                   fns(i,k) = g*rho(i,k)*uns(i,k)
  2061: ||        
  2062: ||                else
  2063: ||                   fs(i,k)=0._r8
  2064: ||                   fns(i,k)=0._r8
  2065: ||                end if
  2066: ||                ! redefine dummy variables - sedimentation is calculated over grid-scale
  2067: ||                ! quantities to ensure conservation
  2068: ||        
  2069: ||        
  2070: ||                dumc(i,k) = (qc(i,k)+qctend(i,k)*deltat)
  2071: ||                dumnc(i,k) = max((nc(i,k)+nctend(i,k)*deltat),0._r8)
  2072: ||                dumi(i,k) = (qi(i,k)+qitend(i,k)*deltat)
  2073: ||                dumni(i,k) = max((ni(i,k)+nitend(i,k)*deltat),0._r8)
  2074: ||                dumr(i,k) = (qr(i,k)+qrtend(i,k)*deltat)
  2075: ||                dumnr(i,k) = max((nr(i,k)+nrtend(i,k)*deltat),0._r8)
  2076: ||                dums(i,k) = (qs(i,k)+qstend(i,k)*deltat)
  2077: ||                dumns(i,k) = max((ns(i,k)+nstend(i,k)*deltat),0._r8)
  2078: ||        
  2079: ||                if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._r8
  2080: ||                if (dumi(i,k).lt.qsmall) dumni(i,k)=0._r8
  2081: ||                if (dumr(i,k).lt.qsmall) dumnr(i,k)=0._r8
  2082: ||                if (dums(i,k).lt.qsmall) dumns(i,k)=0._r8
  2083: ||        
  2084: |*-----        enddo
  2085: +------     end do       !!! vertical loop
  2086:           
  2087: W------>    do k=1,nlev
  2088: |*----->       do i=1,mgncol
  2089: ||               pdel_inv(i,k) = 1._r8/pdel(i,k)
  2090: |*-----        enddo
  2091: W------     enddo
  2092:             ! initialize nstep for sedimentation sub-steps
  2093:             ! calculate number of split time steps to ensure courant stability criteria
  2094:             ! for sedimentation calculations
  2095:             !-------------------------------------------------------------------
  2096:           
  2097:           
  2098: +------>    do i=1,mgncol
  2099: |V=====>       nstep = 1 + int(max( &
  2100: |                   maxval( fi(i,:)*pdel_inv(i,:)), &
  2101: |                   maxval(fni(i,:)*pdel_inv(i,:))) &
  2102: |                   * deltat)
  2103: |              ! loop over sedimentation sub-time step to ensure stability
  2104: |              !==============================================================
  2105: |         
  2106: |         
  2107: |*----->       do n = 1,nstep
  2108: ||        
  2109: ||                if (do_cldice) then
  2110: ||V====>             falouti  = fi(i,:)  * dumi(i,:)
  2111: ||V====>             faloutni = fni(i,:) * dumni(i,:)
  2112: ||                else
  2113: ||V====>             falouti  = 0._r8
  2114: ||V====>             faloutni = 0._r8
  2115: ||                end if
  2116: ||                ! top of model
  2117: ||        
  2118: ||        
  2119: ||                k = 1
  2120: ||                ! add fallout terms to microphysical tendencies
  2121: ||        
  2122: ||                faltndi = falouti(k)/pdel(i,k)
  2123: ||                faltndni = faloutni(k)/pdel(i,k)
  2124: ||                qitend(i,k) = qitend(i,k)-faltndi/nstep
  2125: ||                nitend(i,k) = nitend(i,k)-faltndni/nstep
  2126: ||                ! sedimentation tendency for output
  2127: ||        
  2128: ||                qisedten(i,k)=qisedten(i,k)-faltndi/nstep
  2129: ||        
  2130: ||                dumi(i,k) = dumi(i,k)-faltndi*deltat/nstep
  2131: ||                dumni(i,k) = dumni(i,k)-faltndni*deltat/nstep
  2132: ||        
  2133: ||V---->          do k = 2,nlev
  2134: |||                  ! for cloud liquid and ice, if cloud fraction increases with height
  2135: |||                  ! then add flux from above to both vapor and cloud water of current level
  2136: |||                  ! this means that flux entering clear portion of cell from above evaporates
  2137: |||                  ! instantly
  2138: |||                  ! note: this is not an issue with precip, since we assume max overlap
  2139: |||       
  2140: |||       
  2141: |||                  dum1=icldm(i,k)/icldm(i,k-1)
  2142: |||                  dum1=min(dum1,1._r8)
  2143: |||       
  2144: |||                  faltndqie=(falouti(k)-falouti(k-1))/pdel(i,k)
  2145: |||                  faltndi=(falouti(k)-dum1*falouti(k-1))/pdel(i,k)
  2146: |||                  faltndni=(faloutni(k)-dum1*faloutni(k-1))/pdel(i,k)
  2147: |||                  ! add fallout terms to eulerian tendencies
  2148: |||       
  2149: |||       
  2150: |||                  qitend(i,k) = qitend(i,k)-faltndi/nstep
  2151: |||                  nitend(i,k) = nitend(i,k)-faltndni/nstep
  2152: |||                  ! sedimentation tendency for output
  2153: |||       
  2154: |||                  qisedten(i,k)=qisedten(i,k)-faltndi/nstep
  2155: |||                  ! add terms to to evap/sub of cloud water
  2156: |||       
  2157: |||       
  2158: |||                  qvlat(i,k)=qvlat(i,k)-(faltndqie-faltndi)/nstep
  2159: |||                  ! for output
  2160: |||                  qisevap(i,k)=qisevap(i,k)-(faltndqie-faltndi)/nstep
  2161: |||       
  2162: |||                  tlat(i,k)=tlat(i,k)+(faltndqie-faltndi)*xxls/nstep
  2163: |||       
  2164: |||                  dumi(i,k) = dumi(i,k)-faltndi*deltat/nstep
  2165: |||                  dumni(i,k) = dumni(i,k)-faltndni*deltat/nstep
  2166: |||       
  2167: ||V----           end do
  2168: ||                ! Ice flux
  2169: ||        
  2170: ||V---->          do k = 1,nlev
  2171: |||                 iflx(i,k+1) = iflx(i,k+1) + falouti(k) / g / real(nstep)
  2172: ||V----           end do
  2173: ||                ! units below are m/s
  2174: ||                ! sedimentation flux at surface is added to precip flux at surface
  2175: ||                ! to get total precip (cloud + precip water) rate
  2176: ||        
  2177: ||        
  2178: ||                prect(i) = prect(i)+falouti(nlev)/g/real(nstep)/1000._r8
  2179: ||                preci(i) = preci(i)+falouti(nlev)/g/real(nstep)/1000._r8
  2180: ||        
  2181: |*-----        end do
  2182: |              ! calculate number of split time steps to ensure courant stability criteria
  2183: |              ! for sedimentation calculations
  2184: |              !-------------------------------------------------------------------
  2185: |         
  2186: |V=====>       nstep = 1 + int(max( &
  2187: |                   maxval( fc(i,:)*pdel_inv(i,:)), &
  2188: |                   maxval(fnc(i,:)*pdel_inv(i,:))) &
  2189: |                   * deltat)
  2190: |              ! loop over sedimentation sub-time step to ensure stability
  2191: |              !==============================================================
  2192: |         
  2193: |*----->       do n = 1,nstep
  2194: ||        
  2195: ||V====>          faloutc  = fc(i,:)  * dumc(i,:)
  2196: ||V====>          faloutnc = fnc(i,:) * dumnc(i,:)
  2197: ||                ! top of model
  2198: ||        
  2199: ||                k = 1
  2200: ||                ! add fallout terms to microphysical tendencies
  2201: ||        
  2202: ||                faltndc = faloutc(k)/pdel(i,k)
  2203: ||                faltndnc = faloutnc(k)/pdel(i,k)
  2204: ||                qctend(i,k) = qctend(i,k)-faltndc/nstep
  2205: ||                nctend(i,k) = nctend(i,k)-faltndnc/nstep
  2206: ||                ! sedimentation tendency for output
  2207: ||        
  2208: ||                qcsedten(i,k)=qcsedten(i,k)-faltndc/nstep
  2209: ||        
  2210: ||                dumc(i,k) = dumc(i,k)-faltndc*deltat/nstep
  2211: ||                dumnc(i,k) = dumnc(i,k)-faltndnc*deltat/nstep
  2212: ||        
  2213: ||V---->          do k = 2,nlev
  2214: |||       
  2215: |||                  dum=lcldm(i,k)/lcldm(i,k-1)
  2216: |||                  dum=min(dum,1._r8)
  2217: |||                  faltndqce=(faloutc(k)-faloutc(k-1))/pdel(i,k)
  2218: |||                  faltndc=(faloutc(k)-dum*faloutc(k-1))/pdel(i,k)
  2219: |||                  faltndnc=(faloutnc(k)-dum*faloutnc(k-1))/pdel(i,k)
  2220: |||                  ! add fallout terms to eulerian tendencies
  2221: |||       
  2222: |||                  qctend(i,k) = qctend(i,k)-faltndc/nstep
  2223: |||                  nctend(i,k) = nctend(i,k)-faltndnc/nstep
  2224: |||                  ! sedimentation tendency for output
  2225: |||       
  2226: |||                  qcsedten(i,k)=qcsedten(i,k)-faltndc/nstep
  2227: |||                  ! add terms to to evap/sub of cloud water
  2228: |||       
  2229: |||                  qvlat(i,k)=qvlat(i,k)-(faltndqce-faltndc)/nstep
  2230: |||                  ! for output
  2231: |||                  qcsevap(i,k)=qcsevap(i,k)-(faltndqce-faltndc)/nstep
  2232: |||       
  2233: |||                  tlat(i,k)=tlat(i,k)+(faltndqce-faltndc)*xxlv/nstep
  2234: |||       
  2235: |||                  dumc(i,k) = dumc(i,k)-faltndc*deltat/nstep
  2236: |||                  dumnc(i,k) = dumnc(i,k)-faltndnc*deltat/nstep
  2237: |||       
  2238: ||V----           end do
  2239: ||                !Liquid condensate flux here
  2240: ||        
  2241: ||V---->          do k = 1,nlev
  2242: |||                  lflx(i,k+1) = lflx(i,k+1) + faloutc(k) / g / real(nstep)
  2243: ||V----           end do
  2244: ||        
  2245: ||                prect(i) = prect(i)+faloutc(nlev)/g/real(nstep)/1000._r8
  2246: ||        
  2247: |*-----        end do
  2248: |              ! calculate number of split time steps to ensure courant stability criteria
  2249: |              ! for sedimentation calculations
  2250: |              !-------------------------------------------------------------------
  2251: |         
  2252: |V=====>       nstep = 1 + int(max( &
  2253: |                   maxval( fr(i,:)*pdel_inv(i,:)), &
  2254: |                   maxval(fnr(i,:)*pdel_inv(i,:))) &
  2255: |                   * deltat)
  2256: |              ! loop over sedimentation sub-time step to ensure stability
  2257: |              !==============================================================
  2258: |         
  2259: |*----->       do n = 1,nstep
  2260: ||        
  2261: ||V====>          faloutr  = fr(i,:)  * dumr(i,:)
  2262: ||V====>          faloutnr = fnr(i,:) * dumnr(i,:)
  2263: ||                ! top of model
  2264: ||        
  2265: ||                k = 1
  2266: ||                ! add fallout terms to microphysical tendencies
  2267: ||        
  2268: ||                faltndr = faloutr(k)/pdel(i,k)
  2269: ||                faltndnr = faloutnr(k)/pdel(i,k)
  2270: ||                qrtend(i,k) = qrtend(i,k)-faltndr/nstep
  2271: ||                nrtend(i,k) = nrtend(i,k)-faltndnr/nstep
  2272: ||                ! sedimentation tendency for output
  2273: ||        
  2274: ||                qrsedten(i,k)=qrsedten(i,k)-faltndr/nstep
  2275: ||        
  2276: ||                dumr(i,k) = dumr(i,k)-faltndr*deltat/real(nstep)
  2277: ||                dumnr(i,k) = dumnr(i,k)-faltndnr*deltat/real(nstep)
  2278: ||        
  2279: ||V---->          do k = 2,nlev
  2280: |||       
  2281: |||                  faltndr=(faloutr(k)-faloutr(k-1))/pdel(i,k)
  2282: |||                  faltndnr=(faloutnr(k)-faloutnr(k-1))/pdel(i,k)
  2283: |||                  ! add fallout terms to eulerian tendencies
  2284: |||       
  2285: |||                  qrtend(i,k) = qrtend(i,k)-faltndr/nstep
  2286: |||                  nrtend(i,k) = nrtend(i,k)-faltndnr/nstep
  2287: |||                  ! sedimentation tendency for output
  2288: |||       
  2289: |||                  qrsedten(i,k)=qrsedten(i,k)-faltndr/nstep
  2290: |||       
  2291: |||                  dumr(i,k) = dumr(i,k)-faltndr*deltat/real(nstep)
  2292: |||                  dumnr(i,k) = dumnr(i,k)-faltndnr*deltat/real(nstep)
  2293: |||       
  2294: ||V----           end do
  2295: ||                ! Rain Flux
  2296: ||        
  2297: ||V---->          do k = 1,nlev
  2298: |||                  rflx(i,k+1) = rflx(i,k+1) + faloutr(k) / g / real(nstep)
  2299: ||V----           end do
  2300: ||        
  2301: ||                prect(i) = prect(i)+faloutr(nlev)/g/real(nstep)/1000._r8
  2302: ||        
  2303: |*-----        end do
  2304: |              ! calculate number of split time steps to ensure courant stability criteria
  2305: |              ! for sedimentation calculations
  2306: |              !-------------------------------------------------------------------
  2307: |         
  2308: |V=====>       nstep = 1 + int(max( &
  2309: |                   maxval( fs(i,:)*pdel_inv(i,:)), &
  2310: |                   maxval(fns(i,:)*pdel_inv(i,:))) &
  2311: |                   * deltat)
  2312: |              ! loop over sedimentation sub-time step to ensure stability
  2313: |              !==============================================================
  2314: |         
  2315: |*----->       do n = 1,nstep
  2316: ||        
  2317: ||V====>          falouts  = fs(i,:)  * dums(i,:)
  2318: ||V====>          faloutns = fns(i,:) * dumns(i,:)
  2319: ||                ! top of model
  2320: ||        
  2321: ||                k = 1
  2322: ||                ! add fallout terms to microphysical tendencies
  2323: ||        
  2324: ||                faltnds = falouts(k)/pdel(i,k)
  2325: ||                faltndns = faloutns(k)/pdel(i,k)
  2326: ||                qstend(i,k) = qstend(i,k)-faltnds/nstep
  2327: ||                nstend(i,k) = nstend(i,k)-faltndns/nstep
  2328: ||                ! sedimentation tendency for output
  2329: ||        
  2330: ||                qssedten(i,k)=qssedten(i,k)-faltnds/nstep
  2331: ||        
  2332: ||                   dums(i,k) = dums(i,k)-faltnds*deltat/real(nstep)
  2333: ||                   dumns(i,k) = dumns(i,k)-faltndns*deltat/real(nstep)
  2334: ||        
  2335: ||V---->          do k = 2,nlev
  2336: |||       
  2337: |||                  faltnds=(falouts(k)-falouts(k-1))/pdel(i,k)
  2338: |||                  faltndns=(faloutns(k)-faloutns(k-1))/pdel(i,k)
  2339: |||                  ! add fallout terms to eulerian tendencies
  2340: |||       
  2341: |||                  qstend(i,k) = qstend(i,k)-faltnds/nstep
  2342: |||                  nstend(i,k) = nstend(i,k)-faltndns/nstep
  2343: |||                  ! sedimentation tendency for output
  2344: |||       
  2345: |||                  qssedten(i,k)=qssedten(i,k)-faltnds/nstep
  2346: |||       
  2347: |||                  dums(i,k) = dums(i,k)-faltnds*deltat/real(nstep)
  2348: |||                  dumns(i,k) = dumns(i,k)-faltndns*deltat/real(nstep)
  2349: |||       
  2350: ||V----           end do   !! k loop
  2351: ||                ! Snow Flux
  2352: ||        
  2353: ||V---->          do k = 1,nlev
  2354: |||                  sflx(i,k+1) = sflx(i,k+1) + falouts(k) / g / real(nstep)
  2355: ||V----           end do
  2356: ||        
  2357: ||                prect(i) = prect(i)+falouts(nlev)/g/real(nstep)/1000._r8
  2358: ||                preci(i) = preci(i)+falouts(nlev)/g/real(nstep)/1000._r8
  2359: ||        
  2360: |*-----        end do   !! nstep loop
  2361: |         
  2362: +------     enddo
  2363:             ! end sedimentation
  2364:             !ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
  2365:             ! get new update for variables that includes sedimentation tendency
  2366:             ! note : here dum variables are grid-average, NOT in-cloud
  2367:           
  2368:           
  2369: W------>    do k=1,nlev
  2370: |*----->       do i=1,mgncol
  2371: ||                dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._r8)
  2372: ||                dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._r8)
  2373: ||                dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._r8)
  2374: ||                dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._r8)
  2375: ||        
  2376: ||                dumr(i,k) = max(qr(i,k)+qrtend(i,k)*deltat,0._r8)
  2377: ||                dumnr(i,k) = max(nr(i,k)+nrtend(i,k)*deltat,0._r8)
  2378: ||                dums(i,k) = max(qs(i,k)+qstend(i,k)*deltat,0._r8)
  2379: ||                dumns(i,k) = max(ns(i,k)+nstend(i,k)*deltat,0._r8)
  2380: ||                ! switch for specification of droplet and crystal number
  2381: ||        
  2382: ||                if (nccons) then
  2383: ||                   dumnc(i,k)=ncnst/rho(i,k)*lcldm(i,k)
  2384: ||                end if
  2385: ||                ! switch for specification of cloud ice number
  2386: ||        
  2387: ||                if (nicons) then
  2388: ||                   dumni(i,k)=ninst/rho(i,k)*icldm(i,k)
  2389: ||                end if
  2390: ||        
  2391: ||                if (dumc(i,k).lt.qsmall) dumnc(i,k)=0._r8
  2392: ||                if (dumi(i,k).lt.qsmall) dumni(i,k)=0._r8
  2393: ||                if (dumr(i,k).lt.qsmall) dumnr(i,k)=0._r8
  2394: ||                if (dums(i,k).lt.qsmall) dumns(i,k)=0._r8
  2395: ||        
  2396: |*-----        enddo
  2397: |         
  2398: W------     enddo
  2399:             ! calculate instantaneous processes (melting, homogeneous freezing)
  2400:             !====================================================================
  2401:             ! melting of snow at +2 C
  2402:           
  2403:           
  2404: W------>    do k=1,nlev
  2405: |         
  2406: |*----->       do i=1,mgncol
  2407: ||        
  2408: ||                if (t(i,k)+tlat(i,k)/cpp*deltat > snowmelt) then
  2409: ||                   if (dums(i,k) > 0._r8) then
  2410: ||                      ! make sure melting snow doesn't reduce temperature below threshold
  2411: ||        
  2412: ||                      dum = -xlf/cpp*dums(i,k)
  2413: ||                      if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt. snowmelt) then
  2414: ||                         dum = (t(i,k)+tlat(i,k)/cpp*deltat-snowmelt)*cpp/xlf
  2415: ||                         dum = dum/dums(i,k)
  2416: ||                         dum = max(0._r8,dum)
  2417: ||                         dum = min(1._r8,dum)
  2418: ||                      else
  2419: ||                         dum = 1._r8
  2420: ||                      end if
  2421: ||        
  2422: ||                      qstend(i,k)=qstend(i,k)-dum*dums(i,k)/deltat
  2423: ||                      nstend(i,k)=nstend(i,k)-dum*dumns(i,k)/deltat
  2424: ||                      qrtend(i,k)=qrtend(i,k)+dum*dums(i,k)/deltat
  2425: ||                      nrtend(i,k)=nrtend(i,k)+dum*dumns(i,k)/deltat
  2426: ||        
  2427: ||                      dum1=-xlf*dum*dums(i,k)/deltat
  2428: ||                      tlat(i,k)=tlat(i,k)+dum1
  2429: ||                      meltsdttot(i,k)=meltsdttot(i,k) + dum1
  2430: ||                   end if
  2431: ||                end if
  2432: |*-----        enddo
  2433: W------     enddo
  2434: +------>     do k=1,nlev
  2435: |*----->        do i=1,mgncol
  2436: ||                ! freezing of rain at -5 C
  2437: ||        
  2438: ||        
  2439: ||                if (t(i,k)+tlat(i,k)/cpp*deltat < rainfrze) then
  2440: ||        
  2441: ||                   if (dumr(i,k) > 0._r8) then
  2442: ||                      ! make sure freezing rain doesn't increase temperature above threshold
  2443: ||        
  2444: ||                      dum = xlf/cpp*dumr(i,k)
  2445: ||                      if (t(i,k)+tlat(i,k)/cpp*deltat+dum.gt.rainfrze) then
  2446: ||                         dum = -(t(i,k)+tlat(i,k)/cpp*deltat-rainfrze)*cpp/xlf
  2447: ||                         dum = dum/dumr(i,k)
  2448: ||                         dum = max(0._r8,dum)
  2449: ||                         dum = min(1._r8,dum)
  2450: ||                      else
  2451: ||                         dum = 1._r8
  2452: ||                      end if
  2453: ||        
  2454: ||                      qrtend(i,k)=qrtend(i,k)-dum*dumr(i,k)/deltat
  2455: ||                      nrtend(i,k)=nrtend(i,k)-dum*dumnr(i,k)/deltat
  2456: ||                      ! get mean size of rain = 1/lamr, add frozen rain to either snow or cloud ice
  2457: ||                      ! depending on mean rain size
  2458: ||        
  2459: ||        
  2460: ||                      call size_dist_param_basic(mg_rain_props, dumr(i,k), dumnr(i,k), &
  2461: ||                           lamr(i,k))
  2462: ||        
  2463: ||                      if (lamr(i,k) < 1._r8/Dcs) then
  2464: ||                         qstend(i,k)=qstend(i,k)+dum*dumr(i,k)/deltat
  2465: ||                         nstend(i,k)=nstend(i,k)+dum*dumnr(i,k)/deltat
  2466: ||                      else
  2467: ||                         qitend(i,k)=qitend(i,k)+dum*dumr(i,k)/deltat
  2468: ||                         nitend(i,k)=nitend(i,k)+dum*dumnr(i,k)/deltat
  2469: ||                      end if
  2470: ||                      ! heating tendency
  2471: ||        
  2472: ||                      dum1 = xlf*dum*dumr(i,k)/deltat
  2473: ||                      frzrdttot(i,k)=frzrdttot(i,k) + dum1
  2474: ||                      tlat(i,k)=tlat(i,k)+dum1
  2475: ||        
  2476: ||                   end if
  2477: ||                end if
  2478: ||        
  2479: |*-----         enddo
  2480: +------      enddo
  2481:              if (do_cldice) then
  2482: W------>        do k=1,nlev
  2483: |*----->          do i=1,mgncol
  2484: ||                   if (t(i,k)+tlat(i,k)/cpp*deltat > tmelt) then
  2485: ||                      if (dumi(i,k) > 0._r8) then
  2486: ||                         ! limit so that melting does not push temperature below freezing
  2487: ||                         !-----------------------------------------------------------------
  2488: ||        
  2489: ||                         dum = -dumi(i,k)*xlf/cpp
  2490: ||                         if (t(i,k)+tlat(i,k)/cpp*deltat+dum.lt.tmelt) then
  2491: ||                            dum = (t(i,k)+tlat(i,k)/cpp*deltat-tmelt)*cpp/xlf
  2492: ||                            dum = dum/dumi(i,k)
  2493: ||                            dum = max(0._r8,dum)
  2494: ||                            dum = min(1._r8,dum)
  2495: ||                         else
  2496: ||                            dum = 1._r8
  2497: ||                         end if
  2498: ||        
  2499: ||                         qctend(i,k)=qctend(i,k)+dum*dumi(i,k)/deltat
  2500: ||                         ! for output
  2501: ||        
  2502: ||                         melttot(i,k)=dum*dumi(i,k)/deltat
  2503: ||                         ! assume melting ice produces droplet
  2504: ||                         ! mean volume radius of 8 micron
  2505: ||        
  2506: ||        
  2507: ||                         nctend(i,k)=nctend(i,k)+3._r8*dum*dumi(i,k)/deltat/ &
  2508: ||                              (4._r8*pi*5.12e-16_r8*rhow)
  2509: ||        
  2510: ||                         qitend(i,k)=((1._r8-dum)*dumi(i,k)-qi(i,k))/deltat
  2511: ||                         nitend(i,k)=((1._r8-dum)*dumni(i,k)-ni(i,k))/deltat
  2512: ||                         tlat(i,k)=tlat(i,k)-xlf*dum*dumi(i,k)/deltat
  2513: ||                      end if
  2514: ||                   end if
  2515: |*-----           enddo
  2516: W------        enddo
  2517:                ! homogeneously freeze droplets at -40 C
  2518:                !-----------------------------------------------------------------
  2519:           
  2520:           
  2521: W------>       do k=1,nlev
  2522: |*----->          do i=1,mgncol
  2523: ||                   if (t(i,k)+tlat(i,k)/cpp*deltat < 233.15_r8) then
  2524: ||                      if (dumc(i,k) > 0._r8) then
  2525: ||                         ! limit so that freezing does not push temperature above threshold
  2526: ||        
  2527: ||                         dum = dumc(i,k)*xlf/cpp
  2528: ||                         if (t(i,k)+tlat(i,k)/cpp*deltat+dum.gt.233.15_r8) then
  2529: ||                            dum = -(t(i,k)+tlat(i,k)/cpp*deltat-233.15_r8)*cpp/xlf
  2530: ||                            dum = dum/dumc(i,k)
  2531: ||                            dum = max(0._r8,dum)
  2532: ||                            dum = min(1._r8,dum)
  2533: ||                         else
  2534: ||                            dum = 1._r8
  2535: ||                         end if
  2536: ||        
  2537: ||                         qitend(i,k)=qitend(i,k)+dum*dumc(i,k)/deltat
  2538: ||                         ! for output
  2539: ||                         homotot(i,k)=dum*dumc(i,k)/deltat
  2540: ||                         ! assume 25 micron mean volume radius of homogeneously frozen droplets
  2541: ||                         ! consistent with size of detrained ice in stratiform.F90
  2542: ||        
  2543: ||                         nitend(i,k)=nitend(i,k)+dum*3._r8*dumc(i,k)/(4._r8*3.14_r8*1.563e-14_r8* &
  2544: ||                              500._r8)/deltat
  2545: ||                         qctend(i,k)=((1._r8-dum)*dumc(i,k)-qc(i,k))/deltat
  2546: ||                         nctend(i,k)=((1._r8-dum)*dumnc(i,k)-nc(i,k))/deltat
  2547: ||                         tlat(i,k)=tlat(i,k)+xlf*dum*dumc(i,k)/deltat
  2548: ||                      end if
  2549: ||                   end if
  2550: |*-----           enddo
  2551: W------        enddo
  2552:                ! remove any excess over-saturation, which is possible due to non-linearity when adding
  2553:                ! together all microphysical processes
  2554:                !-----------------------------------------------------------------
  2555:                ! follow code similar to old CAM scheme
  2556: +------>       do k=1,nlev
  2557: |V----->          do i=1,mgncol
  2558: ||                   qtmpA(i)=q(i,k)+qvlat(i,k)*deltat
  2559: ||                   ttmpA(i)=t(i,k)+tlat(i,k)/cpp*deltat
  2560: ||                   ! use rhw to allow ice supersaturation
  2561: |V-----           enddo
  2562: |                 call qsat_water(ttmpA, p(:,k), esnA, qvnA,mgncol)
  2563: |         
  2564: |V----->          do i=1,mgncol
  2565: ||                   if (qtmpA(i) > qvnA(i) .and. qvnA(i) > 0 .and. allow_sed_supersat) then
  2566: ||                      ! expression below is approximate since there may be ice deposition
  2567: ||                      dum = (qtmpA(i)-qvnA(i))/(1._r8+xxlv_squared*qvnA(i)/(cpp*rv*ttmpA(i)**2))/deltat
  2568: ||                      ! add to output cme
  2569: ||                      cmeout(i,k) = cmeout(i,k)+dum
  2570: ||                      ! now add to tendencies, partition between liquid and ice based on temperature
  2571: ||                      if (ttmpA(i) > 268.15_r8) then
  2572: ||                         dum1=0.0_r8
  2573: ||                         ! now add to tendencies, partition between liquid and ice based on te
  2574: ||                         !-------------------------------------------------------
  2575: ||                      else if (ttmpA(i) < 238.15_r8) then
  2576: ||                         dum1=1.0_r8
  2577: ||                      else
  2578: ||                         dum1=(268.15_r8-ttmpA(i))/30._r8
  2579: ||                      end if
  2580: ||        
  2581: ||                      dum = (qtmpA(i)-qvnA(i))/(1._r8+(xxls*dum1+xxlv*(1._r8-dum1))**2 &
  2582: ||                           *qvnA(i)/(cpp*rv*ttmpA(i)**2))/deltat
  2583: ||                      qctend(i,k)=qctend(i,k)+dum*(1._r8-dum1)
  2584: ||                      ! for output
  2585: ||                      qcrestot(i,k)=dum*(1._r8-dum1)
  2586: ||                      qitend(i,k)=qitend(i,k)+dum*dum1
  2587: ||                      qirestot(i,k)=dum*dum1
  2588: ||                      qvlat(i,k)=qvlat(i,k)-dum
  2589: ||                      ! for output
  2590: ||                      qvres(i,k)=-dum
  2591: ||                      tlat(i,k)=tlat(i,k)+dum*(1._r8-dum1)*xxlv+dum*dum1*xxls
  2592: ||                   end if
  2593: |V-----           enddo
  2594: +------        enddo
  2595:           
  2596:             end if
  2597:             ! calculate effective radius for pass to radiation code
  2598:             !=========================================================
  2599:             ! if no cloud water, default value is 10 micron for droplets,
  2600:             ! 25 micron for cloud ice
  2601:             ! update cloud variables after instantaneous processes to get effective radius
  2602:             ! variables are in-cloud to calculate size dist parameters
  2603:           
  2604:           
  2605: W------>    do k=1,nlev
  2606: |*----->       do i=1,mgncol
  2607: ||                dumc(i,k) = max(qc(i,k)+qctend(i,k)*deltat,0._r8)/lcldm(i,k)
  2608: ||                dumi(i,k) = max(qi(i,k)+qitend(i,k)*deltat,0._r8)/icldm(i,k)
  2609: ||                dumnc(i,k) = max(nc(i,k)+nctend(i,k)*deltat,0._r8)/lcldm(i,k)
  2610: ||                dumni(i,k) = max(ni(i,k)+nitend(i,k)*deltat,0._r8)/icldm(i,k)
  2611: ||        
  2612: ||                dumr(i,k) = max(qr(i,k)+qrtend(i,k)*deltat,0._r8)/precip_frac(i,k)
  2613: ||                dumnr(i,k) = max(nr(i,k)+nrtend(i,k)*deltat,0._r8)/precip_frac(i,k)
  2614: ||                dums(i,k) = max(qs(i,k)+qstend(i,k)*deltat,0._r8)/precip_frac(i,k)
  2615: ||                dumns(i,k) = max(ns(i,k)+nstend(i,k)*deltat,0._r8)/precip_frac(i,k)
  2616: ||                ! switch for specification of droplet and crystal number
  2617: ||        
  2618: ||                if (nccons) then
  2619: ||                   dumnc(i,k)=ncnst/rho(i,k)
  2620: ||                end if
  2621: ||                ! switch for specification of cloud ice number
  2622: ||        
  2623: ||                if (nicons) then
  2624: ||                   dumni(i,k)=ninst/rho(i,k)
  2625: ||                end if
  2626: ||                ! limit in-cloud mixing ratio to reasonable value of 5 g kg-1
  2627: ||        
  2628: ||                dumc(i,k)=min(dumc(i,k),5.e-3_r8)
  2629: ||                dumi(i,k)=min(dumi(i,k),5.e-3_r8)
  2630: ||                ! limit in-precip mixing ratios
  2631: ||                dumr(i,k)=min(dumr(i,k),10.e-3_r8)
  2632: ||                dums(i,k)=min(dums(i,k),10.e-3_r8)
  2633: |*-----        enddo
  2634: W------     enddo
  2635:             ! cloud ice effective radius
  2636:             !-----------------------------------------------------------------
  2637:           
  2638:             if (do_cldice) then
  2639: +------>       do k=1,nlev
  2640: |*----->          do i=1,mgncol
  2641: ||                   if (dumi(i,k).ge.qsmall) then
  2642: ||        
  2643: ||                      dum_2D(i,k) = dumni(i,k)
  2644: ||                      call size_dist_param_basic(mg_ice_props, dumi(i,k), dumni(i,k), &
  2645: ||                           lami(i,k), dumni0)
  2646: ||        
  2647: ||                      if (dumni(i,k) /=dum_2D(i,k)) then
  2648: ||                         ! adjust number conc if needed to keep mean size in reasonable range
  2649: ||                         nitend(i,k)=(dumni(i,k)*icldm(i,k)-ni(i,k))/deltat
  2650: ||                      end if
  2651: ||        
  2652: ||                      effi(i,k) = 1.5_r8/lami(i,k)*1.e6_r8
  2653: ||                      sadice(i,k) = 2._r8*pi*(lami(i,k)**(-3))*dumni0*rho(i,k)*1.e-2_r8  ! m2/m3 -> cm2/cm3
  2654: ||        
  2655: ||                   else
  2656: ||                      effi(i,k) = 25._r8
  2657: ||                      sadice(i,k) = 0._r8
  2658: ||                   end if
  2659: ||                   ! ice effective diameter for david mitchell's optics
  2660: ||        
  2661: ||                   deffi(i,k)=effi(i,k)*rhoi/rhows*2._r8
  2662: |*-----           enddo
  2663: +------        enddo
  2664:             else
  2665: U------>       do k=1,nlev
  2666: |V----->          do i=1,mgncol
  2667: ||                   ! NOTE: If CARMA is doing the ice microphysics, then the ice effective
  2668: ||                   ! radius has already been determined from the size distribution.
  2669: ||                   effi(i,k) = re_ice(i,k) * 1.e6_r8      ! m -> um
  2670: ||                   deffi(i,k)=effi(i,k) * 2._r8
  2671: ||                   sadice(i,k) = 4._r8*pi*(effi(i,k)**2)*ni(i,k)*rho(i,k)*1e-2_r8
  2672: |V-----           enddo
  2673: U------        enddo
  2674:             end if
  2675:             ! cloud droplet effective radius
  2676:             !-----------------------------------------------------------------
  2677:           
  2678: +------>    do k=1,nlev
  2679: |*----->       do i=1,mgncol
  2680: ||                if (dumc(i,k).ge.qsmall) then
  2681: ||                   ! switch for specification of droplet and crystal number
  2682: ||        
  2683: ||        
  2684: ||                   if (nccons) then
  2685: ||                      ! make sure nc is consistence with the constant N by adjusting tendency, need
  2686: ||                      ! to multiply by cloud fraction
  2687: ||                      ! note that nctend may be further adjusted below if mean droplet size is
  2688: ||                      ! out of bounds
  2689: ||        
  2690: ||                      nctend(i,k)=(ncnst/rho(i,k)*lcldm(i,k)-nc(i,k))/deltat
  2691: ||        
  2692: ||                   end if
  2693: ||        
  2694: ||                   dum = dumnc(i,k)
  2695: ||        
  2696: ||                   call size_dist_param_liq(mg_liq_props, dumc(i,k), dumnc(i,k), rho(i,k), &
  2697: ||                        pgam(i,k), lamc(i,k))
  2698: ||        
  2699: ||                   if (dum /= dumnc(i,k)) then
  2700: ||                      ! adjust number conc if needed to keep mean size in reasonable range
  2701: ||                      nctend(i,k)=(dumnc(i,k)*lcldm(i,k)-nc(i,k))/deltat
  2702: ||                   end if
  2703: ||        
  2704: ||                   effc(i,k) = (pgam(i,k)+3._r8)/lamc(i,k)/2._r8*1.e6_r8
  2705: ||                   !assign output fields for shape here
  2706: ||                   lamcrad(i,k)=lamc(i,k)
  2707: ||                   pgamrad(i,k)=pgam(i,k)
  2708: ||                   ! recalculate effective radius for constant number, in order to separate
  2709: ||                   ! first and second indirect effects
  2710: ||                   !======================================
  2711: ||                   ! assume constant number of 10^8 kg-1
  2712: ||        
  2713: ||        
  2714: ||                   dumnc(i,k)=1.e8_r8
  2715: ||                   ! Pass in "false" adjust flag to prevent number from being changed within
  2716: ||                   ! size distribution subroutine.
  2717: ||        
  2718: ||                   call size_dist_param_liq(mg_liq_props, dumc(i,k), dumnc(i,k), rho(i,k), &
  2719: ||                        pgam(i,k), lamc(i,k))
  2720: ||        
  2721: ||                   effc_fn(i,k) = (pgam(i,k)+3._r8)/lamc(i,k)/2._r8*1.e6_r8
  2722: ||        
  2723: ||                else
  2724: ||                   effc(i,k) = 10._r8
  2725: ||                   lamcrad(i,k)=0._r8
  2726: ||                   pgamrad(i,k)=0._r8
  2727: ||                   effc_fn(i,k) = 10._r8
  2728: ||                end if
  2729: |*-----        enddo
  2730: +------     enddo
  2731:             ! recalculate 'final' rain size distribution parameters
  2732:             ! to ensure that rain size is in bounds, adjust rain number if needed
  2733: +------>    do k=1,nlev
  2734: |*----->       do i=1,mgncol
  2735: ||        
  2736: ||                if (dumr(i,k).ge.qsmall) then
  2737: ||        
  2738: ||                   dum = dumnr(i,k)
  2739: ||        
  2740: ||                   call size_dist_param_basic(mg_rain_props, dumr(i,k), dumnr(i,k), &
  2741: ||                        lamr(i,k))
  2742: ||        
  2743: ||                   if (dum /= dumnr(i,k)) then
  2744: ||                      ! adjust number conc if needed to keep mean size in reasonable range
  2745: ||                      nrtend(i,k)=(dumnr(i,k)*precip_frac(i,k)-nr(i,k))/deltat
  2746: ||                   end if
  2747: ||        
  2748: ||                end if
  2749: |*-----        enddo
  2750: +------     enddo
  2751:             ! recalculate 'final' snow size distribution parameters
  2752:             ! to ensure that snow size is in bounds, adjust snow number if needed
  2753: +------>    do k=1,nlev
  2754: |*----->       do i=1,mgncol
  2755: ||                if (dums(i,k).ge.qsmall) then
  2756: ||        
  2757: ||                   dum = dumns(i,k)
  2758: ||        
  2759: ||                   call size_dist_param_basic(mg_snow_props, dums(i,k), dumns(i,k), &
  2760: ||                        lams(i,k), n0=dumns0)
  2761: ||        
  2762: ||                   if (dum /= dumns(i,k)) then
  2763: ||                      ! adjust number conc if needed to keep mean size in reasonable range
  2764: ||                      nstend(i,k)=(dumns(i,k)*precip_frac(i,k)-ns(i,k))/deltat
  2765: ||                   end if
  2766: ||        
  2767: ||                   sadsnow(i,k) = 2._r8*pi*(lams(i,k)**(-3))*dumns0*rho(i,k)*1.e-2_r8  ! m2/m3 -> cm2/cm3
  2768: ||        
  2769: ||                end if
  2770: ||        
  2771: ||        
  2772: |*-----        end do ! vertical k loop
  2773: +------     enddo
  2774: W------>    do k=1,nlev
  2775: |*----->       do i=1,mgncol
  2776: ||                ! if updated q (after microphysics) is zero, then ensure updated n is also zero
  2777: ||                !=================================================================================
  2778: ||                if (qc(i,k)+qctend(i,k)*deltat.lt.qsmall) nctend(i,k)=-nc(i,k)/deltat
  2779: ||                if (do_cldice .and. qi(i,k)+qitend(i,k)*deltat.lt.qsmall) nitend(i,k)=-ni(i,k)/deltat
  2780: ||                if (qr(i,k)+qrtend(i,k)*deltat.lt.qsmall) nrtend(i,k)=-nr(i,k)/deltat
  2781: ||                if (qs(i,k)+qstend(i,k)*deltat.lt.qsmall) nstend(i,k)=-ns(i,k)/deltat
  2782: ||        
  2783: |*-----        end do
  2784: |         
  2785: W------     end do
  2786:             ! DO STUFF FOR OUTPUT:
  2787:             !==================================================
  2788:             ! qc and qi are only used for output calculations past here,
  2789:             ! so add qctend and qitend back in one more time
  2790:           
  2791:           
  2792: V======>    qc = qc + qctend*deltat
  2793: V======>    qi = qi + qitend*deltat
  2794:             ! averaging for snow and rain number and diameter
  2795:             !--------------------------------------------------
  2796:             ! drout2/dsout2:
  2797:             ! diameter of rain and snow
  2798:             ! dsout:
  2799:             ! scaled diameter of snow (passed to radiation in CAM)
  2800:             ! reff_rain/reff_snow:
  2801:             ! calculate effective radius of rain and snow in microns for COSP using Eq. 9 of COSP v1.3 manual
  2802:           
  2803:           
  2804: V======>    where (qrout .gt. 1.e-7_r8 &
  2805: |                .and. nrout.gt.0._r8)
  2806: |              qrout2 = qrout * precip_frac
  2807: +======        nrout2 = nrout * precip_frac
  2808:                ! The avg_diameter call does the actual calculation; other diameter
  2809: +======>       ! outputs are just drout2 times constants.
  2810: +======        drout2 = avg_diameter(qrout, nrout, rho, rhow)
  2811:                freqr = precip_frac
  2812: V======>  
  2813: |              reff_rain=1.5_r8*drout2*1.e6_r8
  2814: |           elsewhere
  2815: |              qrout2 = 0._r8
  2816: |              nrout2 = 0._r8
  2817: |              drout2 = 0._r8
  2818: |              freqr = 0._r8
  2819: |              reff_rain = 0._r8
  2820: V======     end where
  2821:           
  2822: V======>    where (qsout .gt. 1.e-7_r8 &
  2823: |                .and. nsout.gt.0._r8)
  2824: |              qsout2 = qsout * precip_frac
  2825: +======        nsout2 = nsout * precip_frac
  2826:                ! The avg_diameter call does the actual calculation; other diameter
  2827: +======>       ! outputs are just dsout2 times constants.
  2828: +======        dsout2 = avg_diameter(qsout, nsout, rho, rhosn)
  2829:                freqs = precip_frac
  2830: V======>  
  2831: |              dsout=3._r8*rhosn/rhows*dsout2
  2832: |         
  2833: |              reff_snow=1.5_r8*dsout2*1.e6_r8
  2834: |           elsewhere
  2835: |              dsout  = 0._r8
  2836: |              qsout2 = 0._r8
  2837: |              nsout2 = 0._r8
  2838: |              dsout2 = 0._r8
  2839: |              freqs  = 0._r8
  2840: |              reff_snow=0._r8
  2841: V======     end where
  2842:             ! analytic radar reflectivity
  2843:             !--------------------------------------------------
  2844:             ! formulas from Matthew Shupe, NOAA/CERES
  2845:             ! *****note: radar reflectivity is local (in-precip average)
  2846:             ! units of mm^6/m^3
  2847:           
  2848:           
  2849: W------>    do i = 1,mgncol
  2850: |*----->       do k=1,nlev
  2851: ||                if (qc(i,k).ge.qsmall .and. (nc(i,k)+nctend(i,k)*deltat).gt.10._r8) then
  2852: ||                   dum=(qc(i,k)/lcldm(i,k)*rho(i,k)*1000._r8)**2 &
  2853: ||                        /(0.109_r8*(nc(i,k)+nctend(i,k)*deltat)/lcldm(i,k)*rho(i,k)/1.e6_r8)*lcldm(i,k)/precip_frac(i,k)
  2854: ||                else
  2855: ||                   dum=0._r8
  2856: ||                end if
  2857: ||                if (qi(i,k).ge.qsmall) then
  2858: ||                   dum1=(qi(i,k)*rho(i,k)/icldm(i,k)*1000._r8/0.1_r8)**(1._r8/0.63_r8)*icldm(i,k)/precip_frac(i,k)
  2859: ||                else
  2860: ||                   dum1=0._r8
  2861: ||                end if
  2862: ||        
  2863: ||                if (qsout(i,k).ge.qsmall) then
  2864: ||                   dum1=dum1+(qsout(i,k)*rho(i,k)*1000._r8/0.1_r8)**(1._r8/0.63_r8)
  2865: ||                end if
  2866: ||        
  2867: ||                refl(i,k)=dum+dum1
  2868: ||                ! add rain rate, but for 37 GHz formulation instead of 94 GHz
  2869: ||                ! formula approximated from data of Matrasov (2007)
  2870: ||                ! rainrt is the rain rate in mm/hr
  2871: ||                ! reflectivity (dum) is in DBz
  2872: ||        
  2873: ||        
  2874: ||                if (rainrt(i,k).ge.0.001_r8) then
  2875: ||                   dum=log10(rainrt(i,k)**6._r8)+16._r8
  2876: ||                   ! convert from DBz to mm^6/m^3
  2877: ||        
  2878: ||        
  2879: ||                   dum = 10._r8**(dum/10._r8)
  2880: ||                else
  2881: ||                   ! don't include rain rate in R calculation for values less than 0.001 mm/hr
  2882: ||                   dum=0._r8
  2883: ||                end if
  2884: ||                ! add to refl
  2885: ||        
  2886: ||        
  2887: ||                refl(i,k)=refl(i,k)+dum
  2888: ||                !output reflectivity in Z.
  2889: ||        
  2890: ||                areflz(i,k)=refl(i,k) * precip_frac(i,k)
  2891: ||                ! convert back to DBz
  2892: ||        
  2893: ||        
  2894: ||                if (refl(i,k).gt.minrefl) then
  2895: ||                   refl(i,k)=10._r8*log10(refl(i,k))
  2896: ||                else
  2897: ||                   refl(i,k)=-9999._r8
  2898: ||                end if
  2899: ||                !set averaging flag
  2900: ||        
  2901: ||                if (refl(i,k).gt.mindbz) then
  2902: ||                   arefl(i,k)=refl(i,k) * precip_frac(i,k)
  2903: ||                   frefl(i,k)=precip_frac(i,k)
  2904: ||                else
  2905: ||                   arefl(i,k)=0._r8
  2906: ||                   areflz(i,k)=0._r8
  2907: ||                   frefl(i,k)=0._r8
  2908: ||                end if
  2909: ||                ! bound cloudsat reflectivity
  2910: ||        
  2911: ||        
  2912: ||                csrfl(i,k)=min(csmax,refl(i,k))
  2913: ||                !set averaging flag
  2914: ||        
  2915: ||                if (csrfl(i,k).gt.csmin) then
  2916: ||                   acsrfl(i,k)=refl(i,k) * precip_frac(i,k)
  2917: ||                   fcsrfl(i,k)=precip_frac(i,k)
  2918: ||                else
  2919: ||                   acsrfl(i,k)=0._r8
  2920: ||                   fcsrfl(i,k)=0._r8
  2921: ||                end if
  2922: ||        
  2923: |*-----        end do
  2924: W------     end do
  2925:             !redefine fice here....
  2926:           
  2927: V======>    dum_2D = qsout + qrout + qc + qi
  2928: V======>    dumi = qsout + qi
  2929: V======>    where (dumi .gt. qsmall .and. dum_2D .gt. qsmall)
  2930: +======        nfice=min(dumi/dum_2D,1._r8)
  2931:             elsewhere
  2932: V======>       nfice=0._r8
  2933: |           end where
  2934: |         
  2935: V======   end subroutine micro_mg_tend
  2936:           !========================================================================
  2937:           !OUTPUT CALCULATIONS


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:46:30 2018
FILE NAME: micro_mg2_0.F90

PROCEDURE NAME: MICRO_MG2_0::CALC_RERCLD
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  2941: opt(1097): This statement prevents loop optimization.
  2958: vec( 110): Vectorization obstructive procedure reference.: MICRO_MG_UTILS::RISING_FACTORIAL_INT
  2958: vec( 103): Unvectorized loop.
  2968: inl(1212): Source for routine not found.: MICRO_MG_UTILS::RISING_FACTORIAL_INT
  2968: opt(1025): Reference to this procedure inhibits optimization.: MICRO_MG_UTILS::RISING_FACTORIAL_INT


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:46:30 2018
FILE NAME: micro_mg2_0.F90

PROCEDURE NAME: MICRO_MG2_0::CALC_RERCLD
FORMAT LIST

 LINE   LOOP      STATEMENT

  2941:           subroutine calc_rercld(lamr, n0r, lamc, pgam, qric, qcic, ncic, rercld, mgncol)
  2942:             integer, intent(in) :: mgncol
  2943:             real(r8), dimension(mgncol), intent(in) :: lamr          ! rain size parameter (slope)
  2944:             real(r8), dimension(mgncol), intent(in) :: n0r           ! rain size parameter (intercept)
  2945:             real(r8), dimension(mgncol), intent(in) :: lamc          ! size distribution parameter (slope)
  2946:             real(r8), dimension(mgncol), intent(in) :: pgam          ! droplet size parameter
  2947:             real(r8), dimension(mgncol), intent(in) :: qric          ! in-cloud rain mass mixing ratio
  2948:             real(r8), dimension(mgncol), intent(in) :: qcic          ! in-cloud cloud liquid
  2949:             real(r8), dimension(mgncol), intent(in) :: ncic          ! in-cloud droplet number concentration
  2950:           
  2951:             real(r8), dimension(mgncol), intent(inout) :: rercld     ! effective radius calculation for rain + cloud
  2952:             ! combined size of precip & cloud drops
  2953:           
  2954:             real(r8) :: Atmp,tmp
  2955:           
  2956:             integer :: i
  2957:           
  2958: +------>    do i=1,mgncol
  2959: |              ! Rain drops
  2960: |              if (lamr(i) > 0._r8) then
  2961: |                 Atmp = n0r(i) * pi / (2._r8 * lamr(i)**3._r8)
  2962: |              else
  2963: |                 Atmp = 0._r8
  2964: |              end if
  2965: |              ! Add cloud drops
  2966: |         
  2967: |              if (lamc(i) > 0._r8) then
  2968: |                 call rising_factorial(pgam(i)+1._r8, 2,tmp)
  2969: |                 Atmp = Atmp + &
  2970: |                      ncic(i) * pi * tmp/(4._r8 * lamc(i)**2._r8)
  2971: |              end if
  2972: |         
  2973: |              if (Atmp > 0._r8) then
  2974: |                 rercld(i) = rercld(i) + 3._r8 *(qric(i) + qcic(i)) / (4._r8 * rhow * Atmp)
  2975: |              end if
  2976: +------     enddo
  2977:           end subroutine calc_rercld


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:46:30 2018
FILE NAME: micro_mg2_0.F90

PROCEDURE NAME: MICRO_MG2_0::KR_EXTERNS_IN_MICRO_MG2_0
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:46:30 2018
FILE NAME: micro_mg2_0.F90

PROCEDURE NAME: MICRO_MG2_0::KR_EXTERNS_IN_MICRO_MG2_0
FORMAT LIST

 LINE   LOOP      STATEMENT

  2984:           SUBROUTINE kr_externs_in_micro_mg2_0(kgen_unit)
  2985:               INTEGER, INTENT(IN) :: kgen_unit
  2986:               LOGICAL :: kgen_istrue
  2987:               REAL(KIND=8) :: kgen_array_sum
  2988:            
  2989:               READ (UNIT = kgen_unit) nccons
  2990:               READ (UNIT = kgen_unit) nicons
  2991:               READ (UNIT = kgen_unit) ncnst
  2992:               READ (UNIT = kgen_unit) ninst
  2993:               READ (UNIT = kgen_unit) dcs
  2994:               READ (UNIT = kgen_unit) g
  2995:               READ (UNIT = kgen_unit) r
  2996:               READ (UNIT = kgen_unit) rv
  2997:               READ (UNIT = kgen_unit) cpp
  2998:               READ (UNIT = kgen_unit) tmelt
  2999:               READ (UNIT = kgen_unit) xxlv
  3000:               READ (UNIT = kgen_unit) xlf
  3001:               READ (UNIT = kgen_unit) xxls
  3002:               READ (UNIT = kgen_unit) microp_uniform
  3003:               READ (UNIT = kgen_unit) do_cldice
  3004:               READ (UNIT = kgen_unit) use_hetfrz_classnuc
  3005:               READ (UNIT = kgen_unit) rhosu
  3006:               READ (UNIT = kgen_unit) icenuct
  3007:               READ (UNIT = kgen_unit) snowmelt
  3008:               READ (UNIT = kgen_unit) rainfrze
  3009:               READ (UNIT = kgen_unit) gamma_br_plus1
  3010:               READ (UNIT = kgen_unit) gamma_br_plus4
  3011:               READ (UNIT = kgen_unit) gamma_bs_plus1
  3012:               READ (UNIT = kgen_unit) gamma_bs_plus4
  3013:               READ (UNIT = kgen_unit) gamma_bi_plus1
  3014:               READ (UNIT = kgen_unit) gamma_bi_plus4
  3015:               READ (UNIT = kgen_unit) gamma_bj_plus1
  3016:               READ (UNIT = kgen_unit) gamma_bj_plus4
  3017:               READ (UNIT = kgen_unit) xxlv_squared
  3018:               READ (UNIT = kgen_unit) xxls_squared
  3019:               READ (UNIT = kgen_unit) micro_mg_precip_frac_method
  3020:               READ (UNIT = kgen_unit) micro_mg_berg_eff_factor
  3021:               READ (UNIT = kgen_unit) allow_sed_supersat
  3022:               READ (UNIT = kgen_unit) do_sb_physics
  3023:           END SUBROUTINE kr_externs_in_micro_mg2_0


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:46:30 2018
FILE NAME: micro_mg2_0.F90

PROCEDURE NAME: MICRO_MG2_0::KR_EXTERNS_OUT_MICRO_MG2_0
DIAGNOSTIC LIST

 LINE              DIAGNOSTIC MESSAGE

  No diagnostic message.


NEC Fortran Compiler (1.3.0) for Vector Engine   Fri Aug 17 18:46:30 2018
FILE NAME: micro_mg2_0.F90

PROCEDURE NAME: MICRO_MG2_0::KR_EXTERNS_OUT_MICRO_MG2_0
FORMAT LIST

 LINE   LOOP      STATEMENT

  3026:           SUBROUTINE kr_externs_out_micro_mg2_0(kgen_unit)
  3027:               INTEGER, INTENT(IN) :: kgen_unit
  3028:            
  3029:               LOGICAL :: kgen_istrue
  3030:               REAL(KIND=8) :: kgen_array_sum
  3031:           END SUBROUTINE kr_externs_out_micro_mg2_0


