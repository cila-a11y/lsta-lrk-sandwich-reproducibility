#!/bin/bash -l
#SBATCH --job-name=lsta_pilot90
#SBATCH --account=f202500010hpcvlabuminhox
#SBATCH --partition=dev-x86
#SBATCH --time=00:45:00
#SBATCH --nodes=1
#SBATCH --ntasks=90
#SBATCH --cpus-per-task=1
#SBATCH --mem=180G
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

if [[ -e "${TASK_ROOT}" || -e "${AGG_DIR}" || -e "${LOG_ROOT}" ]]; then
  echo "ERRO: outputs piloto já existem." >&2
  echo "${TASK_ROOT}" >&2
  echo "${AGG_DIR}" >&2
  echo "${LOG_ROOT}" >&2
  exit 1
fi

mkdir -p "${TASK_ROOT}" "${LOG_ROOT}"

cd "${WORK_REPO}"

echo "============================================================"
echo "Unified pilot: 90 atomic tasks"
echo "UTC: $(date -u --iso-8601=seconds)"
echo "JOB_ID: ${SLURM_JOB_ID}"
echo "NODE: $(hostname -f)"
echo "SLURM_NTASKS: ${SLURM_NTASKS}"
echo "TASK_ROOT: ${TASK_ROOT}"
echo "============================================================"

grep -q '^PILOT_DESIGN_STATUS=PASS$' \
  "${DESIGN_DIR}/design_status.txt"

grep -q '^PILOT_DESIGN_FREEZE_STATUS=PASS$' \
  "${DESIGN_DIR}/freeze_status.txt"

PIDS=()
FAILED=0

for TASK_ID in $(seq 1 90); do
  TASK_LABEL="$(printf 'task_%04d' "${TASK_ID}")"

  srun \
    --exclusive \
    --nodes=1 \
    --ntasks=1 \
    --cpus-per-task=1 \
    --cpu-bind=cores \
    Rscript --vanilla \
      "${WORK_REPO}/hpc/unified/run_unified_task.R" \
      "${WORK_REPO}" \
      "${DESIGN_DIR}" \
      "${TASK_ROOT}" \
      "${TASK_ID}" \
      > "${LOG_ROOT}/${TASK_LABEL}.out" \
      2> "${LOG_ROOT}/${TASK_LABEL}.err" &

  PIDS+=("$!")
done

for PID in "${PIDS[@]}"; do
  if ! wait "${PID}"; then
    FAILED=1
  fi
done

if [[ "${FAILED}" -ne 0 ]]; then
  echo "ERRO: pelo menos uma tarefa piloto falhou." >&2
  exit 1
fi

PASS_COUNT="$(
  grep -R '^TASK_STATUS=PASS$' \
    "${TASK_ROOT}"/task_*/task_status.txt |
  wc -l
)"

if [[ "${PASS_COUNT}" != "90" ]]; then
  echo "ERRO: apenas ${PASS_COUNT}/90 tarefas têm TASK_STATUS=PASS." >&2
  exit 1
fi

Rscript --vanilla \
  "${WORK_REPO}/hpc/unified/aggregate_unified_pilot.R" \
  "${DESIGN_DIR}" \
  "${TASK_ROOT}" \
  "${AGG_DIR}" \
  "${LOG_ROOT}"

grep -q '^PILOT_AGGREGATION_STATUS=PASS$' \
  "${AGG_DIR}/pilot_aggregation_status.txt"

sha256sum \
  "${WORK_REPO}/hpc/unified/run_unified_task.R" \
  "${WORK_REPO}/hpc/unified/aggregate_unified_pilot.R" \
  "${WORK_REPO}/hpc/unified/study_spec.R" \
  "${AGG_DIR}/pilot_task_status.csv" \
  "${AGG_DIR}/pilot_metrics_selected_bandwidth.csv" \
  "${AGG_DIR}/pilot_final_job_sizing.csv" \
  > "${AGG_DIR}/pilot_run_sha256.txt"

printf '%s\n' \
  "PILOT_90_RUN_STATUS=PASS" \
  "JOB_ID=${SLURM_JOB_ID}" \
  "UTC=$(date -u --iso-8601=seconds)" \
  "TASKS=90" \
  "METHODS=BL,BS,HT,FT" \
  > "${AGG_DIR}/pilot_run_status.txt"

rm -rf "${TMPDIR}"

echo
echo "PILOT_90_RUN_STATUS=PASS"
