#!/bin/bash
read -p "ENTER NUMBER OF MOLECULES: " N 
read -p "ENTER UPPER BOUND FOR PERIODIC BOX (ANGSTROMS): " L
read -p "ENTER TARGET TEMPERATURE (KELVIN): " T
if [ $T -lt 0 ] || [ $N -lt 0 ] || [ $L -lt 0 ]; then
    echo INVALID
    exit 1
fi
sed -i "3s/.*/    INTEGER, PARAMETER :: N = ${N}/" parameters.f90
sed -i "8s/.*/    REAL(KIND=8), PARAMETER :: L = ${L}/" parameters.f90
sed -i "11s/.*/    REAL(KIND=8), PARAMETER :: T = ${T}/" parameters.f90
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
# for file in $(awk 'FNR==2 {print $NF, FILENAME}' dump/vel*.txt | sort -n | awk '{print $2}')
# do
#     cat "$file" >> vels.txt
# done
# for file in $(awk 'FNR==2 {print $NF, FILENAME}' dump/acc*.txt | sort -n | awk '{print $2}')
# do
#     cat "$file" >> accs.txt
# done
if test -f trajectory.xyz; then
    echo TRAJECTORY FILE SUCCESSFULLY CREATED
else
    echo ERROR CREATING TRAJECTORY FILE
    exit 1
fi
#vmd -e mol.tcl
vmd -xyz trajectory.xyz