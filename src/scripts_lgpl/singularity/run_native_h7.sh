#! /bin/bash
  
# Usage:
#   - Copy this script into your working folder, next to the dimr config file.
#   - Modify this script where needed (e.g. number of nodes, number of tasks per node).
#   - Execute this script from the command line of H7 using: ./run_native_h7.sh
#
# This is an h7 specific script for single or multi-node simulations.

# Set bash options. Exit on failures (and propagate errors in pipes).
set -eo pipefail

# These variables should be modified.
NODES=1
TASKS_PER_NODE=1
JOB_NAME="test_model"
PARTITION="test"
TIME_LIMIT="00:15:00"


DIMR_FOLDER="/p/d-hydro/development/alma8/dimrsets/2.26.04.78838"
DIMR_FILE="${PWD}/dimr_config.xml"
  
NTASKS=(( $NODES * $TASKS_PER_NODE ))

# Modify the value of the process tag in dimr_config.xml.
PROCESS_STR="$(seq -s " " 0 $((SLURM_NTASKS-1)))"
sed -i "s/\(<process.*>\)[^<>]*\(<\/process.*\)/\1$PROCESS_STR\2/" $DIMR_FILE

# The name of the MDU file is read from the DIMR configuration file.
MDU_FILENAME=$(sed -n 's/\r//; s/<inputFile>\(.*[.]mdu\)<\/inputFile>/\1/p' $DIMR_FILE | sed -n 's/^\s*\(.*\)\s*$/\1/p')

# Partition data using dflowfm.
if [[ $NTASKS -gt 1 ]]; then
    pushd dflowfm
    echo "Partitioning parallel model..."
    echo "Run dflowfm on $MDU_FILENAME with $NTASKS partitions."
    srun -n 1 -N 1 ${DIMR_FOLDER}/lnx64/bin/run_dflowfm.sh --partition:ndomains=${NTASKS}:icgsolver=6 $MDU_FILENAME
    popd
else
    echo "Sequential model..."
fi

# Run simulation using dimr.
echo "Run simulation..."
sbatch \ 
    --job-name=$JOB_NAME \
    --partition=$PARTITION \
    --time=$TIME_LIMIT \
    --nodes=$NODES \
    --ntasks-per-node=$TASKS_PER_NODE \
    ${DIMR_FOLDER}/lnx64/bin/submit_dimr_h7.sh -m $CONFIG_PATH