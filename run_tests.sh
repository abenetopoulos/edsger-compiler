#/bin/bash

WD=tests

cd $WD
for t in `ls *.eds | sed 's/\(.*\)\..*/\1/'`; do
    ../edsgerc ${t}.eds
    rc=$?
    if [[ ${rc} != 0 ]]; then
       printf "Failed while reading %s.eds\n" "${t}"
    fi
done

cd ..
