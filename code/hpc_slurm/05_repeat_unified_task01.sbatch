#!/bin/bash -l
#SBATCH --job-name=lsta_task01rep
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

source /projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/deucalion_config.sh

export WORK_REPO="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/repository_work"
export PILOT_ROOT="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/results/unified_pilot"
export DESIGN_DIR="${PILOT_ROOT}/design"
export REPEAT_ROOT="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/results/unified_pilot_single_task_repeat"

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

rm -rf "${REPEAT_ROOT}"
mkdir -p "${REPEAT_ROOT}"

cd "${WORK_REPO}"

Rscript --vanilla \
  "${WORK_REPO}/hpc/unified/run_unified_task.R" \
  "${WORK_REPO}" \
  "${DESIGN_DIR}" \
  "${REPEAT_ROOT}" \
  1

grep -q '^TASK_STATUS=PASS$' \
  "${REPEAT_ROOT}/task_0001/task_status.txt"

printf '%s\n' \
  "SINGLE_TASK_REPEAT_STATUS=PASS" \
  "JOB_ID=${SLURM_JOB_ID}" \
  "TASK_ID=1" \
  "UTC=$(date -u --iso-8601=seconds)" \
  > "${REPEAT_ROOT}/single_task_repeat_status.txt"

rm -rf "${TMPDIR}"

echo "SINGLE_TASK_REPEAT_STATUS=PASS"
