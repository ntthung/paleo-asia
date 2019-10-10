#!/bin/bash
### Set the job name
#PBS -N paleo-asia

### Send email when job is started and is completed
#PBS -m abe
#PBS -M email@address.com

### Uncomment to set the queue name
### PBS -q default

### Job-array
#PBS -J 1-8

### Resources request
#PBS -l select=1:ncpus=24:mem=16GB

### Walltime
#PBS -l walltime=00:15:00

echo Working directory is $PBS_O_WORKDIR
cd $PBS_O_WORKDIR

### Run for information
echo Running on host `hostname`
echo Time is `date`
echo Directory is `pwd`
echo This jobs run on the following processors:
echo `cat $PBS_NODEFILE`

### Run R
module load R
Rscript KWF_ensemble_cluster.R ${PBS_ARRAY_INDEX}
