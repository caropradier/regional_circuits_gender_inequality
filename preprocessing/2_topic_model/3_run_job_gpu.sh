#!/bin/bash
#SBATCH --time=3:00:00
#SBATCH --account=def-vlarivie
#SBATCH --nodes=1
#SBATCH --cpus-per-task=16
#SBATCH --job-name=full_dataset
#SBATCH --mem=92G
#SBATCH --gpus-per-node=2
#SBATCH --mail-user=carolina.pradier@umontreal.ca
#SBATCH --mail-type=ALL
#SBATCH  --output="../../job_outputs/job-%u-%x-%j.out"

# ---------------------------------------------------------------------
echo "Current working directory: `pwd`"
echo "Starting run at: `date`"
# ---------------------------------------------------------------------
echo ""
echo "Job ID:  $SLURM_JOB_ID"
echo ""
# ---------------------------------------------------------------------
module load python
module load scipy-stack
source ~/ENV/bin/activate

python topic_modeling_pipeline.py latam_text_topics_3title.txt ../job_outputs/bertopic_output_20240302_195213/embeddings.p
# ---------------------------------------------------------------------
echo "Job finished with exit code $? at: `date`"
# ---------------------------------------------------------------------
