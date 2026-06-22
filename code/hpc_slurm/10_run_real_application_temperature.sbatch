#!/bin/bash -l
#SBATCH --job-name=lsta_realapp
#SBATCH --account=f202500010hpcvlabuminhox
#SBATCH --partition=dev-x86
#SBATCH --time=00:20:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --output=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.out
#SBATCH --error=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.err

set -euo pipefail
umask 007

source /projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/project.env
source "${HPC_DIR}/deucalion_config.sh"

export WORK_REPO="${PROJECT_DIR}/repository_work"

module purge
module load "${R_MODULE}"

export R_PROFILE_USER=/dev/null
export R_ENVIRON_USER=/dev/null
export R_DEFAULT_PACKAGES="datasets,utils,grDevices,graphics,stats,methods"

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1

cd "${WORK_REPO}"

Rscript --vanilla \
  "${WORK_REPO}/hpc/unified/run_real_application_temperature.R"

APP_DIR="${RESULTS_DIR}/real_application/final_temperature"

grep -q '^REAL_APPLICATION_TEMPERATURE_STATUS=PASS$' \
  "${APP_DIR}/real_application_temperature_status.txt"

printf '%s\n' \
  "REAL_APPLICATION_TEMPERATURE_JOB_STATUS=PASS" \
  "JOB_ID=${SLURM_JOB_ID}" \
  "UTC=$(date -u --iso-8601=seconds)" \
  > "${APP_DIR}/real_application_temperature_job_status.txt"

echo "REAL_APPLICATION_TEMPERATURE_JOB_STATUS=PASS"
