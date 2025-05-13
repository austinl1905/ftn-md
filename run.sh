#!/bin/bash
echo THE FOLLOWING DEPENDENCIES ARE REQUIRED: VMD, GFORTRAN, BC
read -p "ENTER NUMBER OF MOLECULES: " N 
read -p "ENTER UPPER BOUND FOR PERIODIC BOX AS DECIMAL VALUE (ANGSTROMS): " L
read -p "ENTER TARGET TEMPERATURE AS DECIMAL VALUE (KELVIN): " T
if (( $(echo "$T < 0.0" |bc -l) )) || [ $N -lt 0 ] || (( $(echo "$L < 0.0" |bc -l) )); then
    echo INVALID
    exit 1
fi
sed -i "3s/.*/    INTEGER, PARAMETER :: N = ${N}/" parameters.f90
sed -i "8s/.*/    REAL(KIND=8), PARAMETER :: L = ${L}/" parameters.f90
sed -i "11s/.*/    REAL(KIND=8), PARAMETER :: T = ${T}/" parameters.f90
sed -i "3s/.*/pbc set {${L} ${L} ${L} 90.0 90.0 90.0} -all/" mol.tcl
gfortran main.f90 parameters.f90 md.f90 -o main 
gfortran main.f90 parameters.f90 md.f90 -o main # I DONT KNOW WHY I HAVE TO DO THIS. N DOESNT UPDATE OTHERWISE I HATE IT
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
vmd -e mol.tcl