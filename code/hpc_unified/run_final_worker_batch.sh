#!/bin/bash
set -euo pipefail
umask 007

if [[ "$#" -ne 6 ]]; then
  echo "Usage: run_final_worker_batch.sh <work_repo> <design_dir> <task_root> <log_root> <worker_id> <worker_count>" >&2
  exit 2
fi

WORK_REPO="$1"
DESIGN_DIR="$2"
TASK_ROOT="$3"
LOG_ROOT="$4"
WORKER_ID="$5"
WORKER_COUNT="$6"

export DESIGN_DIR

if [[ "${WORKER_ID}" -lt 1 || "${WORKER_COUNT}" -lt 1 ]]; then
  echo "Invalid worker_id or worker_count." >&2
  exit 2
fi

TOTAL_TASKS="$(
  Rscript --vanilla -e '
    manifest <- readRDS(file.path(Sys.getenv("DESIGN_DIR"), "task_manifest.rds"))
    cat(nrow(manifest))
  '
)"

if [[ "${TOTAL_TASKS}" != "4500" ]]; then
  echo "Unexpected number of final tasks: ${TOTAL_TASKS}" >&2
  exit 1
fi

mkdir -p "${TASK_ROOT}" "${LOG_ROOT}"

WORKER_LABEL="$(printf 'worker_%03d' "${WORKER_ID}")"
STATUS_FILE="${LOG_ROOT}/${WORKER_LABEL}.status"

DONE=0
SKIPPED=0
FAILED=0
START_UTC="$(date -u --iso-8601=seconds)"

echo "WORKER_STATUS=RUNNING" > "${STATUS_FILE}"
echo "WORKER_ID=${WORKER_ID}" >> "${STATUS_FILE}"
echo "WORKER_COUNT=${WORKER_COUNT}" >> "${STATUS_FILE}"
echo "TOTAL_TASKS=${TOTAL_TASKS}" >> "${STATUS_FILE}"
echo "START_UTC=${START_UTC}" >> "${STATUS_FILE}"

for TASK_ID in $(seq "${WORKER_ID}" "${WORKER_COUNT}" "${TOTAL_TASKS}"); do
  TASK_LABEL="$(printf 'task_%04d' "${TASK_ID}")"
  TASK_DIR="${TASK_ROOT}/${TASK_LABEL}"

  if [[ -s "${TASK_DIR}/task_status.txt" ]] &&
     grep -q '^TASK_STATUS=PASS$' "${TASK_DIR}/task_status.txt"
  then
    SKIPPED=$((SKIPPED + 1))
    continue
  fi

  if [[ -e "${TASK_DIR}" ]]; then
    echo "Incomplete existing task directory: ${TASK_DIR}" >&2
    FAILED=1
    continue
  fi

  if Rscript --vanilla \
      "${WORK_REPO}/hpc/unified/run_unified_task.R" \
      "${WORK_REPO}" \
      "${DESIGN_DIR}" \
      "${TASK_ROOT}" \
      "${TASK_ID}" \
      > "${LOG_ROOT}/${TASK_LABEL}.out" \
      2> "${LOG_ROOT}/${TASK_LABEL}.err"
  then
    if grep -q '^TASK_STATUS=PASS$' "${TASK_DIR}/task_status.txt"; then
      DONE=$((DONE + 1))
    else
      echo "Task completed without PASS status: ${TASK_ID}" >&2
      FAILED=1
    fi
  else
    echo "Task failed: ${TASK_ID}" >&2
    FAILED=1
    break
  fi
done

END_UTC="$(date -u --iso-8601=seconds)"

{
  echo "WORKER_STATUS=$([[ "${FAILED}" -eq 0 ]] && echo PASS || echo FAIL)"
  echo "WORKER_ID=${WORKER_ID}"
  echo "WORKER_COUNT=${WORKER_COUNT}"
  echo "TOTAL_TASKS=${TOTAL_TASKS}"
  echo "TASKS_DONE=${DONE}"
  echo "TASKS_SKIPPED=${SKIPPED}"
  echo "START_UTC=${START_UTC}"
  echo "END_UTC=${END_UTC}"
} > "${STATUS_FILE}"

exit "${FAILED}"
