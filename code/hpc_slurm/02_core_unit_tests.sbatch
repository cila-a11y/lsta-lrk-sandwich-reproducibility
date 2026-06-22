#!/bin/bash -l
#SBATCH --job-name=lsta_core_test
#SBATCH --account=f202500010hpcvlabuminhox
#SBATCH --partition=dev-x86
#SBATCH --time=00:30:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=4
#SBATCH --mem=32G
#SBATCH --output=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.out
#SBATCH --error=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.err

set -euo pipefail
umask 007

source /projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/deucalion_config.sh

export WORK_REPO="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/repository_work"
export CORE_TEST_RESULTS="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/results/validation/core_tests"

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
mkdir -p "${TMPDIR}" "${CORE_TEST_RESULTS}"

rm -f \
  "${CORE_TEST_RESULTS}/core_test_status.txt" \
  "${CORE_TEST_RESULTS}/core_test_details.txt"

echo "============================================================"
echo "Core estimator tests"
echo "UTC: $(date -u --iso-8601=seconds)"
echo "JOB_ID: ${SLURM_JOB_ID}"
echo "NODE: $(hostname -f)"
echo "WORK_REPO: ${WORK_REPO}"
echo "R_LIBS_USER: ${R_LIBS_USER}"
echo "============================================================"

cd "${WORK_REPO}"

Rscript --vanilla \
  "${WORK_REPO}/hpc/tests/test_core_estimators.R"

grep -q '^CORE_TEST_STATUS=PASS$' \
  "${CORE_TEST_RESULTS}/core_test_status.txt"

sha256sum \
  "${WORK_REPO}/src/simulation/gap_sandwich_lag_window.R" \
  "${WORK_REPO}/src/simulation/mc_lagwindow_article_corrected.R" \
  "${WORK_REPO}/src/application/article_application_corrected.R" \
  "${WORK_REPO}/src/application/article_application_expanded.R" \
  "${WORK_REPO}/hpc/tests/test_core_estimators.R" \
  > "${CORE_TEST_RESULTS}/tested_sources_sha256.txt"

utils_file="${CORE_TEST_RESULTS}/sessionInfo.txt"

Rscript --vanilla -e \
  "utils::capture.output(sessionInfo(), file='${utils_file}')"

printf '%s\n' \
  "CORE_TEST_FREEZE_STATUS=PASS" \
  "JOB_ID=${SLURM_JOB_ID}" \
  "UTC=$(date -u --iso-8601=seconds)" \
  > "${CORE_TEST_RESULTS}/freeze_status.txt"

rm -rf "${TMPDIR}"

echo
echo "CORE_TEST_FREEZE_STATUS=PASS"
