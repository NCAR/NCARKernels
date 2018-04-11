#! /bin/sh -f

USAGE='usage: bld ARCH'

if test "$#" -lt 1
then
echo $USAGE 1>&2
exit 2
fi

if [ $1 = "SUN" ]
then

MAKENAME=make

elif [ $1 = "SGI" ]
then

MAKENAME=gmake

elif [ $1 = "CRAY_PVP" ]
then

MAKENAME=gnumake

elif [ $1 = "CRAY_T3E" ]
then

MAKENAME=gmake

elif [ $1 = "HP" ]
then

MAKENAME=gmake

elif [ $1 = "HP_IA64" ]
then

MAKENAME=make

elif [ $1 = "DEC" ]
then

MAKENAME=gmake

elif [ $1 = "IBM" ]
then

MAKENAME=gmake

elif [ $1 = "DARWIN_INTEL" ]
then

MAKENAME=make

elif [ $1 = "DARWIN_GNU" ]
then

MAKENAME=make

elif [ $1 = "LINUX_INTEL" ]
then

MAKENAME=make
EXT=_host
elif [ $1 = "LINUX_BDW" ]
then

MAKENAME=make

elif [ $1 = "LINUX_INTELO2Precise" ]
then

MAKENAME=make
EXT=_hostO2precise


elif [ $1 = "LINUX_INTELMIC" ]
then

MAKENAME=make
EXT=_mic


elif [ $1 = "LINUX_INTELMICO2Precise" ]
then

MAKENAME=make
EXT=_micO2precise

elif [ $1 = "LINUX_PC" ]
then

MAKENAME=gmake

elif [ $1 = "LINUX_IA64" ]
then

MAKENAME=gmake

else

echo "Unrecognized Architecture" $1
exit 1

fi



echo '   '
#echo 'begin compilation...'
#date
#$MAKENAME testx${EXT}   ARCH=$1 
#$MAKENAME alogx${EXT}   ARCH=$1
#$MAKENAME expx${EXT}    ARCH=$1
#$MAKENAME powerx${EXT}  ARCH=$1
#$MAKENAME sinx${EXT}    ARCH=$1
#$MAKENAME sqrtx${EXT}   ARCH=$1
#$MAKENAME dlogx${EXT}   ARCH=$1
#$MAKENAME dex${EXT}px${EXT}   ARCH=$1
#$MAKENAME dpowerx${EXT} ARCH=$1
#$MAKENAME dsinx${EXT}   ARCH=$1
#$MAKENAME dsqrtx${EXT}  ARCH=$1
date
echo "start run"


echo '================================='
echo 'run ALOG test...'
echo '================================='
date
./alogx${EXT}
date

echo '================================='
echo 'run EXP test...'
echo '================================='
date
./expx${EXT}
date

echo '================================='
echo 'run PWR test...'
echo '================================='
date
./powerx${EXT}
date

echo '================================='
echo 'run SIN test...'
echo '================================='
date
./sinx${EXT}
date

echo '================================='
echo 'run SQRT test...'
echo '================================='
date
./sqrtx${EXT}
date

echo '================================='
echo 'run DLOG test...'
echo '================================='
date
./dlogx${EXT}
date

echo '================================='
echo 'run DEXP test...'
echo '================================='
date
./dexpx${EXT}
date

echo '================================='
echo 'run DPWR test...'
echo '================================='
date
./dpowerx${EXT}
date

echo '================================='
echo 'run DSIN test...'
echo '================================='
date
./dsinx${EXT}
date

echo '================================='
echo 'run DSQRT test...'
echo '================================='
date
./dsqrtx${EXT}
date

echo '================================='
echo 'run performance test...'
echo '================================='
date
./testx${EXT}
date

cp FUN.DAT ../results/$1

exit 0
