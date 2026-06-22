# ============================================================
# Unified Monte Carlo specification
# ============================================================

unified_study_spec <- function() {
  list(
    design_version = "LSTA-unified-v1.0",
    base_seed = 20260620L,

    pilot_replications = 10L,
    final_replications = 500L,

    sample_sizes = c(
      300L,
      600L,
      1200L
    ),

    probability_grid = seq(
      0.05,
      0.95,
      by = 0.05
    ),

    max_bandwidth_cap = 160L,

    polynomial_beta = 2.2,
    polynomial_filter_length = 600L,

    negative_eigen_tolerance = -1e-10,
    near_singular_tolerance = 1e-4,

    truth_rho_tolerance = 1e-12,

    methods = data.frame(
      method_index = 1:4,

      method_code = c(
        "BL",
        "BS",
        "HT",
        "FT"
      ),

      method_label = c(
        "Bartlett lag-window",
        "Bartlett sample-centered",
        "Hard truncation",
        "Exact flat-top lag-window"
      ),

      window = c(
        "Bartlett",
        "Bartlett",
        "Rectangular",
        "Trapezoidal exact flat-top"
      ),

      normalization = c(
        "pair-normalized",
        "sample-centered",
        "pair-normalized",
        "pair-normalized"
      ),

      psd_guaranteed = c(
        FALSE,
        TRUE,
        FALSE,
        FALSE
      ),

      stringsAsFactors = FALSE
    )
  )
}


unified_scenario_table <- function(
  spec = unified_study_spec()
) {
  data.frame(
    scenario_index = 1:3,

    scenario_id = c(
      "geom_04",
      "geom_08",
      "tpl_22"
    ),

    scenario_label = c(
      "AR(1), rho = 0.4",
      "AR(1), rho = 0.8",
      paste0(
        "truncated positive linear, beta = ",
        spec$polynomial_beta
      )
    ),

    scenario_family = c(
      "geometric",
      "geometric",
      "truncated_linear_proxy"
    ),

    generator = c(
      "ar1",
      "ar1",
      "positive_linear"
    ),

    parameter_1 = c(
      0.4,
      0.8,
      spec$polynomial_beta
    ),

    parameter_2 = c(
      NA_real_,
      NA_real_,
      spec$polynomial_filter_length
    ),

    stringsAsFactors = FALSE
  )
}


unified_exact_flat_top_weights <- function(
  bandwidth,
  max_lag
) {
  bandwidth <- as.integer(bandwidth)
  max_lag <- as.integer(max_lag)

  if (
    length(bandwidth) != 1L ||
    is.na(bandwidth) ||
    bandwidth < 1L
  ) {
    stop(
      "`bandwidth` must be one positive integer.",
      call. = FALSE
    )
  }

  if (
    length(max_lag) != 1L ||
    is.na(max_lag) ||
    max_lag < 0L
  ) {
    stop(
      "`max_lag` must be one nonnegative integer.",
      call. = FALSE
    )
  }

  if (max_lag == 0L) {
    return(numeric(0L))
  }

  u <- seq_len(max_lag) / bandwidth

  weights <- ifelse(
    u <= 0.5,
    1,
    ifelse(
      u <= 1,
      2 * (1 - u),
      0
    )
  )

  weights[
    abs(weights) < 1e-15
  ] <- 0

  as.numeric(weights)
}


unified_make_rng_streams <- function(
  n_streams,
  base_seed = unified_study_spec()$base_seed
) {
  n_streams <- as.integer(n_streams)
  base_seed <- as.integer(base_seed)

  if (
    length(n_streams) != 1L ||
    is.na(n_streams) ||
    n_streams < 1L
  ) {
    stop(
      "`n_streams` must be one positive integer.",
      call. = FALSE
    )
  }

  if (
    length(base_seed) != 1L ||
    is.na(base_seed) ||
    base_seed < 1L
  ) {
    stop(
      "`base_seed` must be one positive integer.",
      call. = FALSE
    )
  }

  old_kind <- RNGkind()

  seed_existed <- exists(
    ".Random.seed",
    envir = .GlobalEnv,
    inherits = FALSE
  )

  if (seed_existed) {
    old_seed <- get(
      ".Random.seed",
      envir = .GlobalEnv,
      inherits = FALSE
    )
  }

  on.exit({
    do.call(
      RNGkind,
      as.list(old_kind)
    )

    if (seed_existed) {
      assign(
        ".Random.seed",
        old_seed,
        envir = .GlobalEnv
      )
    } else if (
      exists(
        ".Random.seed",
        envir = .GlobalEnv,
        inherits = FALSE
      )
    ) {
      rm(
        ".Random.seed",
        envir = .GlobalEnv
      )
    }
  }, add = TRUE)

  RNGkind(
    kind = "L'Ecuyer-CMRG",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  set.seed(base_seed)

  streams <- vector(
    mode = "list",
    length = n_streams
  )

  streams[[1L]] <- .Random.seed

  if (n_streams >= 2L) {
    for (index in 2:n_streams) {
      streams[[index]] <-
        parallel::nextRNGStream(
          streams[[index - 1L]]
        )
    }
  }

  valid <- vapply(
    streams,
    function(stream) {
      is.integer(stream) &&
        length(stream) == 7L &&
        all(is.finite(stream))
    },
    logical(1L)
  )

  if (!all(valid)) {
    stop(
      "Invalid L'Ecuyer-CMRG stream generated.",
      call. = FALSE
    )
  }

  streams
}


unified_set_rng_stream <- function(stream) {
  if (
    !is.integer(stream) ||
    length(stream) != 7L ||
    any(!is.finite(stream))
  ) {
    stop(
      "Invalid L'Ecuyer-CMRG stream.",
      call. = FALSE
    )
  }

  RNGkind(
    kind = "L'Ecuyer-CMRG",
    normal.kind = "Inversion",
    sample.kind = "Rejection"
  )

  assign(
    ".Random.seed",
    stream,
    envir = .GlobalEnv
  )

  invisible(stream)
}


unified_build_task_table <- function(
  replications,
  spec = unified_study_spec()
) {
  replications <- as.integer(replications)

  if (
    length(replications) != 1L ||
    is.na(replications) ||
    replications < 1L
  ) {
    stop(
      "`replications` must be one positive integer.",
      call. = FALSE
    )
  }

  scenarios <- unified_scenario_table(spec)
  sample_sizes <- as.integer(spec$sample_sizes)

  n_tasks <-
    nrow(scenarios) *
    length(sample_sizes) *
    replications

  rows <- vector(
    mode = "list",
    length = n_tasks
  )

  task_id <- 0L

  for (
    scenario_row in seq_len(nrow(scenarios))
  ) {
    for (
      sample_size_index in seq_along(sample_sizes)
    ) {
      for (
        replication in seq_len(replications)
      ) {
        task_id <- task_id + 1L

        scenario <-
          scenarios[scenario_row, ]

        sample_size <-
          sample_sizes[[sample_size_index]]

        task_key <- sprintf(
          "%s__n%04d__r%04d",
          scenario$scenario_id,
          sample_size,
          replication
        )

        rows[[task_id]] <- data.frame(
          task_id = task_id,
          task_key = task_key,

          scenario_index =
            scenario$scenario_index,

          scenario_id =
            scenario$scenario_id,

          scenario_label =
            scenario$scenario_label,

          scenario_family =
            scenario$scenario_family,

          generator =
            scenario$generator,

          parameter_1 =
            scenario$parameter_1,

          parameter_2 =
            scenario$parameter_2,

          sample_size_index =
            sample_size_index,

          sample_size =
            sample_size,

          replication =
            replication,

          rng_stream_index =
            task_id,

          output_relative_path =
            sprintf(
              "task_%04d",
              task_id
            ),

          design_version =
            spec$design_version,

          stringsAsFactors = FALSE
        )
      }
    }
  }

  task_table <- do.call(
    rbind,
    rows
  )

  rownames(task_table) <- NULL

  if (nrow(task_table) != n_tasks) {
    stop(
      "Incorrect number of tasks generated.",
      call. = FALSE
    )
  }

  if (
    anyDuplicated(task_table$task_id) ||
    anyDuplicated(task_table$task_key) ||
    anyDuplicated(
      task_table$output_relative_path
    )
  ) {
    stop(
      "Task identifiers are not unique.",
      call. = FALSE
    )
  }

  task_table
}


unified_expected_pilot_tasks <- function(
  spec = unified_study_spec()
) {
  nrow(unified_scenario_table(spec)) *
    length(spec$sample_sizes) *
    spec$pilot_replications
}


unified_expected_final_tasks <- function(
  spec = unified_study_spec()
) {
  nrow(unified_scenario_table(spec)) *
    length(spec$sample_sizes) *
    spec$final_replications
}
