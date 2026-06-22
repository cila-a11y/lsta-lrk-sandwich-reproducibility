#!/bin/bash -l
#SBATCH --job-name=lsta_final4500
#SBATCH --account=f202500010hpcvlabuminhox
#SBATCH --partition=normal-x86
#SBATCH --time=03:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=128
#SBATCH --mem=220G
#SBATCH --output=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.out
#SBATCH --error=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.err

set -euo pipefail
umask 007

source /projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/deucalion_config.sh

export WORK_REPO="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/repository_work"
export FINAL_ROOT="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/results/unified_final"
export FINAL_DESIGN_DIR="${FINAL_ROOT}/design"
export FINAL_TASK_ROOT="${FINAL_ROOT}/tasks"
export FINAL_AGG_DIR="${FINAL_ROOT}/aggregated"
export FINAL_LOG_ROOT="${FINAL_ROOT}/worker_logs"

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

if [[ -e "${FINAL_AGG_DIR}" ]]; then
  echo "ERRO: agregação final já existe." >&2
  echo "${FINAL_AGG_DIR}" >&2
  exit 1
fi

mkdir -p "${FINAL_TASK_ROOT}" "${FINAL_LOG_ROOT}"

cd "${WORK_REPO}"

grep -q '^FINAL_DESIGN_STATUS=PASS$' \
  "${FINAL_DESIGN_DIR}/final_design_status.txt"

grep -q '^FINAL_DESIGN_FREEZE_STATUS=PASS$' \
  "${FINAL_DESIGN_DIR}/freeze_status.txt"

echo "============================================================"
echo "Unified final run: 4500 tasks, 128 persistent workers"
echo "UTC: $(date -u --iso-8601=seconds)"
echo "JOB_ID: ${SLURM_JOB_ID}"
echo "NODE: $(hostname -f)"
echo "SLURM_NTASKS: ${SLURM_NTASKS}"
echo "============================================================"

PIDS=()
FAILED=0

for WORKER_ID in $(seq 1 128); do
  WORKER_LABEL="$(printf 'worker_%03d' "${WORKER_ID}")"

  bash "${WORK_REPO}/hpc/unified/run_final_worker_batch.sh" \
    "${WORK_REPO}" \
    "${FINAL_DESIGN_DIR}" \
    "${FINAL_TASK_ROOT}" \
    "${FINAL_LOG_ROOT}" \
    "${WORKER_ID}" \
    128 \
    > "${FINAL_LOG_ROOT}/${WORKER_LABEL}.out" \
    2> "${FINAL_LOG_ROOT}/${WORKER_LABEL}.err" &

  PIDS+=("$!")
done

for PID in "${PIDS[@]}"; do
  if ! wait "${PID}"; then
    FAILED=1
  fi
done

if [[ "${FAILED}" -ne 0 ]]; then
  echo "ERRO: pelo menos um worker final falhou." >&2
  exit 1
fi

PASS_COUNT="$(
  grep -R '^TASK_STATUS=PASS$' \
    "${FINAL_TASK_ROOT}"/task_*/task_status.txt |
  wc -l
)"

if [[ "${PASS_COUNT}" != "4500" ]]; then
  echo "ERRO: apenas ${PASS_COUNT}/4500 tarefas têm TASK_STATUS=PASS." >&2
  exit 1
fi

Rscript --vanilla \
  "${WORK_REPO}/hpc/unified/aggregate_unified_final.R" \
  "${FINAL_DESIGN_DIR}" \
  "${FINAL_TASK_ROOT}" \
  "${FINAL_AGG_DIR}" \
  "${FINAL_LOG_ROOT}"

grep -q '^FINAL_AGGREGATION_STATUS=PASS$' \
  "${FINAL_AGG_DIR}/final_aggregation_status.txt"

sha256sum \
  "${WORK_REPO}/hpc/unified/run_unified_task.R" \
  "${WORK_REPO}/hpc/unified/run_final_worker_batch.sh" \
  "${WORK_REPO}/hpc/unified/aggregate_unified_final.R" \
  "${WORK_REPO}/hpc/unified/study_spec.R" \
  "${FINAL_AGG_DIR}/final_task_status.csv" \
  "${FINAL_AGG_DIR}/final_metrics_selected_bandwidth.csv" \
  "${FINAL_AGG_DIR}/final_negative_eigen_summary.csv" \
  > "${FINAL_AGG_DIR}/final_run_sha256.txt"

printf '%s\n' \
  "FINAL_4500_RUN_STATUS=PASS" \
  "JOB_ID=${SLURM_JOB_ID}" \
  "UTC=$(date -u --iso-8601=seconds)" \
  "TASKS=4500" \
  "WORKERS=128" \
  "METHODS=BL,BS,HT,FT" \
  > "${FINAL_AGG_DIR}/final_run_status.txt"

rm -rf "${TMPDIR}"

echo "FINAL_4500_RUN_STATUS=PASS"
