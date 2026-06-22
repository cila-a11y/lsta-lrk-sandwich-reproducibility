#!/bin/bash -l
#SBATCH --job-name=lsta_finaldes
#SBATCH --account=f202500010hpcvlabuminhox
#SBATCH --partition=dev-x86
#SBATCH --time=00:15:00
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
export FINAL_DESIGN_DIR="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/results/unified_final/design"

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
mkdir -p "${TMPDIR}" "$(dirname "${FINAL_DESIGN_DIR}")"

cd "${WORK_REPO}"

Rscript --vanilla \
  "${WORK_REPO}/hpc/unified/materialize_final_design.R" \
  "${WORK_REPO}" \
  "${FINAL_DESIGN_DIR}"

grep -q '^FINAL_DESIGN_STATUS=PASS$' \
  "${FINAL_DESIGN_DIR}/final_design_status.txt"

grep -q '^FINAL_TASKS=4500$' \
  "${FINAL_DESIGN_DIR}/final_design_status.txt"

grep -q '^RNG_STREAMS=4500$' \
  "${FINAL_DESIGN_DIR}/final_design_status.txt"

sha256sum \
  "${WORK_REPO}/hpc/unified/materialize_final_design.R" \
  "${FINAL_DESIGN_DIR}/task_manifest.csv" \
  "${FINAL_DESIGN_DIR}/task_manifest.rds" \
  "${FINAL_DESIGN_DIR}/rng_streams.rds" \
  > "${FINAL_DESIGN_DIR}/materialization_sha256.txt"

printf '%s\n' \
  "FINAL_DESIGN_FREEZE_STATUS=PASS" \
  "JOB_ID=${SLURM_JOB_ID}" \
  "UTC=$(date -u --iso-8601=seconds)" \
  "TASKS=4500" \
  "STREAMS=4500" \
  "METHODS=BL,BS,HT,FT" \
  > "${FINAL_DESIGN_DIR}/freeze_status.txt"

rm -rf "${TMPDIR}"

echo "FINAL_DESIGN_FREEZE_STATUS=PASS"
