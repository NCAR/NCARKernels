#!/bin/bash


index=(0 1 2 3 4 5 6 7 8 9)  
for i in "${index[@]}"  
do  
    mpirun -n 2 ./kernel.exe | grep "Average call time"
done  
