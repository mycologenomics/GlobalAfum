#!/bin/sh

#PBS -lwalltime=72:00:00
#PBS -lselect=1:ncpus=32:mem=124gb:avx2=true

module load anaconda3/personal
source activate raxml-ng_env

raxml-ng --seed 2 --msa /rds/general/project/fisher-aspergillus-results/live/SNPs/finalSNPs/all_withR
ef.fa --threads 32 --model GTR+G --prefix /rds/general/project/fisher-aspergillus-results/live/SNPs/f
inalSNPs/raxml-ng_global