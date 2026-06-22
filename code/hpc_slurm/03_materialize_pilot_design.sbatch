#!/bin/bash -l
#SBATCH --job-name=lsta_design
#SBATCH --account=f202500010hpcvlabuminhox
#SBATCH --partition=dev-x86
#SBATCH --time=00:15:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=8G
#SBATCH --output=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.out
#SBATCH --error=/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/logs/%x_%j.err

set -euo pipefail
umask 007

source /projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/hpc/deucalion_config.sh

export WORK_REPO="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/repository_work"
export PILOT_ROOT="/projects/F202500010HPCVLABUMINHO/cecilia/LSTA-2026-0415_lrk_estimation/results/unified_pilot"
export DESIGN_DIR="${PILOT_ROOT}/design"

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
mkdir -p \
    "${TMPDIR}" \
    "${PILOT_ROOT}"

cd "${WORK_REPO}"

echo "============================================================"
echo "Pilot-design materialization"
echo "UTC: $(date -u --iso-8601=seconds)"
echo "JOB_ID: ${SLURM_JOB_ID}"
echo "NODE: $(hostname -f)"
echo "WORK_REPO: ${WORK_REPO}"
echo "DESIGN_DIR: ${DESIGN_DIR}"
echo "============================================================"

Rscript --vanilla -e '
files <- c(
  "hpc/unified/study_spec.R",
  "hpc/unified/materialize_pilot_design.R"
)

for (file in files) {
  parse(file = file)
  cat("PARSE_STATUS=PASS:", file, "\n")
}
'

Rscript --vanilla \
    "${WORK_REPO}/hpc/unified/materialize_pilot_design.R" \
    "${WORK_REPO}" \
    "${DESIGN_DIR}"

grep -q '^PILOT_DESIGN_STATUS=PASS$' \
    "${DESIGN_DIR}/design_status.txt"

required_files=(
    pilot_manifest.csv
    pilot_manifest.rds
    rng_streams.rds
    method_table.csv
    scenario_table.csv
    study_spec.rds
)

for file in "${required_files[@]}"; do
    if [[ ! -s "${DESIGN_DIR}/${file}" ]]; then
        echo "ERRO: ficheiro ausente ou vazio: ${file}" >&2
        exit 1
    fi
done

CSV_TASK_COUNT="$(
    awk -F',' 'END {print NR - 1}' \
        "${DESIGN_DIR}/pilot_manifest.csv"
)"

if [[ "${CSV_TASK_COUNT}" != "90" ]]; then
    echo "ERRO: o CSV contém ${CSV_TASK_COUNT} tarefas." >&2
    exit 1
fi

Rscript --vanilla - <<'RS'
design_dir <- Sys.getenv("DESIGN_DIR")

manifest <- readRDS(
  file.path(
    design_dir,
    "pilot_manifest.rds"
  )
)

streams <- readRDS(
  file.path(
    design_dir,
    "rng_streams.rds"
  )
)

spec <- readRDS(
  file.path(
    design_dir,
    "study_spec.rds"
  )
)

stopifnot(
  nrow(manifest) == 90L,
  length(streams) == 90L,
  spec$pilot_replications == 10L,
  spec$final_replications == 500L,
  identical(
    spec$methods$method_code,
    c("BL", "BS", "HT", "FT")
  ),
  !anyDuplicated(manifest$task_id),
  !anyDuplicated(manifest$task_key),
  !anyDuplicated(manifest$rng_stream_sha256)
)

cat("SBATCH_RDS_VALIDATION=PASS\n")
RS

sha256sum \
    "${WORK_REPO}/hpc/unified/study_spec.R" \
    "${WORK_REPO}/hpc/unified/materialize_pilot_design.R" \
    "${DESIGN_DIR}/pilot_manifest.csv" \
    "${DESIGN_DIR}/pilot_manifest.rds" \
    "${DESIGN_DIR}/rng_streams.rds" \
    "${DESIGN_DIR}/method_table.csv" \
    "${DESIGN_DIR}/scenario_table.csv" \
    "${DESIGN_DIR}/study_spec.rds" \
    > "${DESIGN_DIR}/materialization_sha256.txt"

printf '%s\n' \
    "PILOT_DESIGN_FREEZE_STATUS=PASS" \
    "JOB_ID=${SLURM_JOB_ID}" \
    "UTC=$(date -u --iso-8601=seconds)" \
    "TASKS=${CSV_TASK_COUNT}" \
    "STREAMS=90" \
    "METHODS=BL,BS,HT,FT" \
    > "${DESIGN_DIR}/freeze_status.txt"

rm -rf "${TMPDIR}"

echo
echo "PILOT_DESIGN_FREEZE_STATUS=PASS"
