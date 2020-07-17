#!/bin/bash

rm -f poseidon2.sif

sudo singularity build poseidon2.sif poseidon2.def

scp poseidon2.sif schmid@mpi-sdag1.sdag.ppj.shh.mpg.de:/projects1/singularity_scratch/cache/poseidon2.sif
