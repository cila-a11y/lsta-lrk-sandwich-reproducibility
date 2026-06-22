#!/bin/bash -l
#SBATCH --job-name=lsta_pilotagg
#SBATCH --account=f202500010hpcvlabuminhox
#SBATCH --partition=dev-x86
#SBATCH --time=00:10:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --output=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.out
#SBATCH --error=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.err

set -euo pipefail
umask 007

source /projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/deucalion_config.sh

export WORK_REPO="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/repository_work"
export PILOT_ROOT="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/results/unified_pilot"
export DESIGN_DIR="${PILOT_ROOT}/design"
export TASK_ROOT="${PILOT_ROOT}/tasks"
export AGG_DIR="${PILOT_ROOT}/aggregated"
export LOG_ROOT="${PILOT_ROOT}/worker_logs"

module purge
module load "${R_MODULE}"

export R_PROFILE_USER=/dev/null
export R_ENVIRON_USER=/dev/null
export R_DEFAULT_PACKAGES="datasets,utils,grDevices,graphics,stats,methods"

export OMP_NUM_THREADS=1
export OPENBLAS_NUM_THREADS=1
export MKL_NUM_THREADS=1
export VECLIB_MAXIMUM_THREADS=1
export NUMEXPR_NUM_THREADS=1

export TMPDIR="${PROJECT_DIR}/tmp/${SLURM_JOB_ID}"
mkdir -p "${TMPDIR}"

cd "${WORK_REPO}"

Rscript --vanilla \
  "${WORK_REPO}/hpc/unified/aggregate_unified_pilot.R" \
  "${DESIGN_DIR}" \
  "${TASK_ROOT}" \
  "${AGG_DIR}" \
  "${LOG_ROOT}"

grep -q '^PILOT_AGGREGATION_STATUS=PASS$' \
  "${AGG_DIR}/pilot_aggregation_status.txt"

sha256sum \
  "${WORK_REPO}/hpc/unified/aggregate_unified_pilot.R" \
  "${AGG_DIR}/pilot_task_status.csv" \
  "${AGG_DIR}/pilot_metrics_selected_bandwidth.csv" \
  "${AGG_DIR}/pilot_final_job_sizing.csv" \
  > "${AGG_DIR}/pilot_aggregation_rerun_sha256.txt"

printf '%s\n' \
  "PILOT_AGGREGATION_RERUN_STATUS=PASS" \
  "JOB_ID=${SLURM_JOB_ID}" \
  "UTC=$(date -u --iso-8601=seconds)" \
  > "${AGG_DIR}/pilot_aggregation_rerun_status.txt"

rm -rf "${TMPDIR}"

echo "PILOT_AGGREGATION_RERUN_STATUS=PASS"
