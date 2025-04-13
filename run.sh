#!/bin/bash
gfortran main.f90 parameters.f90 md.f90 -o main
echo SUCCESSFULLY COMPILED
rm -rf dump/*
./main
if test -f trajectory.xyz; then
    rm trajectory.xyz
fi
for i in dump/*.xyz
    do cat $i >> trajectory.xyz
done
echo TRAJECTORY FILE SUCCESSFULLY CREATED