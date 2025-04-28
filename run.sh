#!/bin/bash
gfortran main.f90 parameters.f90 md.f90 -o main
echo SUCCESSFULLY COMPILED
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
for file in $(awk 'FNR==2 {print $NF, FILENAME}' dump/pe*.txt | sort -n | awk '{print $2}')
do 
    cat "$file" >> pes.txt
done
for file in $(awk 'FNR==2 {print $NF, FILENAME}' dump/temp*.txt | sort -n | awk '{print $2}')
do 
    cat "$file" >> temps.txt
done

echo TRAJECTORY FILE SUCCESSFULLY CREATED