#!/bin/bash
echo THE FOLLOWING DEPENDENCIES ARE REQUIRED: VMD, GFORTRAN
gfortran main.f90 parameters.f90 md.f90 -o main
if [ $? -eq 0 ]; then
    echo SUCCESSFULLY COMPILED
else
    echo COMPILATION FAILED.
    exit 1
fi
rm -rf dump/*
./main
if test -f trajectory.xyz; then
    rm trajectory.xyz
fi
for file in $(awk 'FNR==2 {print $NF, FILENAME}' dump/*.xyz | sort -n | awk '{print $2}')
    do
        cat "$file" >> trajectory.xyz
done
for file in $(awk 'FNR==2 {print $NF, FILENAME}' dump/vel*.txt | sort -n | awk '{print $2}')
do
    cat "$file" >> vels.txt
done
for file in $(awk 'FNR==2 {print $NF, FILENAME}' dump/acc*.txt | sort -n | awk '{print $2}')
do
    cat "$file" >> accs.txt
done
echo TRAJECTORY FILE SUCCESSFULLY CREATED, LOADING TRAJECTORY FILE IN VMD
vmd -xyz trajectory.xyz
