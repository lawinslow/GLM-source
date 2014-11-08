#!/bin/sh

PREVDAY=""

while read LINE ; do
    DATETIME=`echo $LINE | cut -f1 -d,`
    THEREST=`echo $LINE | cut -f2- -d,`

    if [ "$PREVDAY" = "" ] ; then
       PREVDAY=$DATETIME
       HOUR=0
    else
       if [ "$PREVDAY" != "$DATETIME" ] ; then
          PREVDAY=$DATETIME
          HOUR=0
       else
          HOUR=`expr $HOUR + 1`
       fi

       DD=`echo $DATETIME | cut -f1 -d-`
       if [ $DD -lt 100 ] ; then # wrong date format
          YY=`echo $DATETIME | cut -f3 -d-`
          MM=`echo $DATETIME | cut -f2 -d-`
          if [ $YY -lt 20 ] ; then
              YY=`expr $YY + 2000`
          else
              YY=`expr $YY + 1900`
          fi
          DATETIME="$YY-$MM-$DD"
       fi

       if [ "$HOUR" -lt "10" ] ; then HOUR=0$HOUR; fi
       DATETIME="$DATETIME $HOUR:00:00"
    fi

    echo $DATETIME,$THEREST
done

exit 0
