#!/bin/bash

singularity exec --bind=/projects1 /projects1/singularity_scratch/cache/poseidon2.sif /poseidon2.R "$@"

