#' Create dataset for optimisation
#'
#' @param processed_output Processed model output
#' @param max_year Maximum year to quantify impact to
#'
#' @return Data for use in optimisation
#' @export
create_optimisation_data <- function(processed_output, max_year = 2030){
  epi_out <- processed_output %>%
    dplyr::filter(.data$year %in% 2024:max_year) %>%
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur, .data$pre, .data$replenishment, .data$post) %>%
    dplyr::summarise(cases = sum(.data$cases),
                     deaths = sum(.data$deaths),
                     y = sum(.data$y))
  
  cost_out <- processed_output %>%
    dplyr::filter(.data$year %in% 2024:2026) %>%
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur, .data$pre, .data$replenishment, .data$post) %>%
    dplyr::summarise(cost = sum(.data$total_cost)) %>%
    dplyr::ungroup()
  
  out <- dplyr::left_join(epi_out, cost_out, by = c("NAME_0", "NAME_1", "NAME_2", "ur", "pre", "replenishment", "post"))
  return(out)
}

#' Filter non-dominant solutions
#'
#' @param x Data to optimise
#'
#' @export
dominant <- function(x){
  xgp <- x %>%
    dplyr::filter(.data$replenishment == "gp")
  
  x %>%
    dplyr::filter(.data$replenishment != "gp") %>%
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur, .data$pre) %>%
    # Select only dominant solutions
    dplyr::arrange(.data$cost, .by_group = TRUE) %>%
    dplyr::filter(.data$y == cummin(.data$y)) %>%
    dplyr::ungroup() %>%
    # Add GP solutions back in
    dplyr::bind_rows(xgp) %>%
    dplyr::arrange(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur, .data$pre, .data$cost)
}

#' Filter non-dominant solutions
#'
#' @param x Data to optimise
#'
#' @export
dominant2 <- function(x){
  xgp <- x %>% dplyr::filter(.data$replenishment == "gp")
  
  x0 <- x %>%
    dplyr::filter(.data$replenishment != "gp", .data$y == 0) %>%
    group_by(.data$NAME_0, 
             .data$NAME_1, .data$NAME_2, .data$ur, .data$pre) %>%
    slice_min(cost, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  x %>% dplyr::filter(.data$replenishment != "gp", .data$y > 0) %>% 
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, 
                    .data$ur, .data$pre) %>% 
    dplyr::arrange(.data$cost, .by_group = TRUE) %>%
    dplyr::filter(.data$y == cummin(.data$y)) %>% 
    dplyr::ungroup() %>% 
    dplyr::bind_rows(xgp) %>%
    dplyr::bind_rows(x0) %>%
    dplyr::arrange(.data$NAME_0, 
                   .data$NAME_1, .data$NAME_2, .data$ur, .data$pre, .data$cost)
}

#' Create optimisation matrices
#' 
#' Creates two i (solution) x j (site) matrices one containing costs one y.
#' Where the number of solutions between sites differ, "empty" solutions are
#' assigned maximum cost and maximum y (an will therefore never be selected)
#'
#' @param x Optimisation data filtered for dominant solutions
#' @param nopt Maximum number of options per site
#' @param nsites Number of sites
#' @param maxy Maximum y over all sites
#' @param maxcost Maximum cost over all sites
#' 
#' @export
create_optim_matrices <- function(x, nopt, nsites, maxy, maxcost){
  outcome <- matrix(maxy, nrow = nopt, ncol = nsites)
  cost <- matrix(maxcost, nrow = nopt, ncol = nsites)
  for(s in seq_along(x)){
    n <- nrow(x[[s]])
    outcome[1:n, s] <- x[[s]]$y
    cost[1:n, s] <- x[[s]]$cost
  }
  list(outcome = outcome, cost = cost)
}

#' Perform optimisation for a single budget line
#'
#' @param x Optimisation data
#' @param budget Budget
#' 
#' @import ompr
#' @import ROI.plugin.glpk
#'
#' @return Optimisaed output
#' @export
single_optimisation <- function(x, budget){
  
  d2 <- x$NAME_0[1] %in% c("Guatemala", "Myanmar", "Honduras", "Thailand")
  
  # Filter for dominant solutions
  if(!d2){
    optim_data <- x %>%
      dominant()
  } else {
    optim_data <- x %>%
      dominant2()
  }
  
  
  
  maxy <- max(optim_data$y)
  maxcost <- max(optim_data$cost)
  
  site_id <- paste0(optim_data$NAME_0, optim_data$NAME_1, optim_data$NAME_2, optim_data$ur)
  optim_data_split <- split(optim_data, site_id)
  
  nsites <- length(optim_data_split)
  nopt <- max(sapply(optim_data_split, nrow))
  
  om <- create_optim_matrices(optim_data_split, nopt, nsites, maxy, maxcost)
  outcome <- om$outcome
  cost <- om$cost
  
  optimout <- ompr::MIPModel() %>%
    # A binary matrix defining a given solution
    ompr::add_variable(idmat[i, j], i = 1:nopt, j = 1:nsites, type = "binary") %>%
    # Objective - to minimise y
    ompr::set_objective(sum_expr(outcome[i, j] * idmat[i, j], i = 1:nopt, j = 1:nsites), "min") %>%
    # Each column must have only 1 selection
    ompr::add_constraint(sum_expr(idmat[i,j], i = 1:nopt) == 1, j = 1:nsites) %>%
    # Total cost must be <= budget
    ompr::add_constraint(sum_expr(cost[i, j] * idmat[i, j], i = 1:nopt, j = 1:nsites) <= budget) %>%
    # Solve
    ompr::solve_model(ompr.roi::with_ROI(solver = "glpk")) %>% 
    # Extract solution
    ompr::get_solution(idmat[i,j]) %>% 
    dplyr::filter(.data$value == 1)
  
  optim_solution <- list()
  for(site in seq_along(optim_data_split)){
    optim_solution[[site]] <- optim_data_split[[site]][optimout[site, "i"],]
  }
  optim_solution <- dplyr::bind_rows(optim_solution)
  return(optim_solution)
}


#' Run multiple optimisations
#'
#' @param x Optimisation data
#' @param gp_replenishment_budget Budget in global plan for period 2024:2026 (inclusive)
#' @param budget_prop Vector of proportions: these are the budgets as a proportion of the GP budget
#' @param force_gp Option to fix Global PLan strategy at budegt_prop = 1
#'
#' @return Optimisated output
#' @export
multi_optimisation <- function(x, gp_replenishment_budget, budget_prop, force_gp = FALSE){
  budgets <- gp_replenishment_budget * budget_prop
  
  # In the first phase (when affordable) all solutions must include (max of at least 25 tx) treatment
  first_phase_data <- x %>%
    dplyr::filter(grepl("tx100", .data$replenishment) | grepl("tx75", .data$replenishment) | 
                    grepl("tx50", .data$replenishment) | grepl("tx25", .data$replenishment) | 
                    grepl("gp", .data$replenishment))
  # Estimate the minimum budget required to obtain a first phase solution
  min_budget_first_phase <- first_phase_data %>%
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur) %>%
    dplyr::filter(.data$cost == min(.data$cost)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data$cost) %>%
    sum()
  # In the second phase (when first phase not affordable) all interventions except treatment 
  # (and RTS,S which is "free") have been removed
  second_phase_data <- x %>% dplyr::filter(.data$replenishment %in% 
                                             c("tx25", "none", "rtss", "tx25_rtss"))
  # Estimate the minimum budget required to obtain a second phase solution
  min_budget_second_phase <- second_phase_data %>%
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur) %>%
    dplyr::filter(.data$cost == min(.data$cost)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data$cost) %>%
    sum()
  # Run the optimisation for all budgets
  out <- list()
  for(i in seq_along(budgets)){
    if(budget_prop[i] == 1 & force_gp){
      out[[i]] <- dplyr::filter(x, .data$replenishment == "gp") %>%
        dplyr::mutate(budget = budgets[i],
                      budget_prop = budget_prop[i])
    } else {
      if(budgets[i] >= min_budget_first_phase){
        out[[i]] <- single_optimisation(first_phase_data, budgets[i]) %>%
          dplyr::mutate(budget = budgets[i],
                        budget_prop = budget_prop[i]) 
      } else {
        if(budgets[i] < min_budget_second_phase){
          message("Incompatible budget found")
        } else {
          out[[i]] <- single_optimisation(second_phase_data, budgets[i]) %>%
            dplyr::mutate(budget = budgets[i],
                          budget_prop = budget_prop[i])
        }
      }
    }
  }
  return(out)
}


#' Get case weighting
#'
#' @param iso ISO code
#' @export
get_weighting <- function(iso){
  case_weighting %>%
    dplyr::filter(.data$ISO == iso) %>%
    dplyr::pull(.data$multiplier)
}