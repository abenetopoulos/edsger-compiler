#/bin/bash

WD=tests
rm ${WD}/outputs/*
mkdir -p ${WD}/outputs

cd $WD
for t in `ls *.eds | sed 's/\(.*\)\..*/\1/'`; do
    ../lexer ${t}.eds > outputs/out_${t}
done

cd ..
