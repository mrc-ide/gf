#' Create dataset for optimisation
#'
#' @param processed_output Processed model output
#'
#' @return Data for use in optimisation
#' @export
create_optimisation_data <- function(processed_output){
  processed_output %>%
    dplyr::filter(.data$year %in% 2024:2026) %>%
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur, .data$pre, .data$replenishment) %>%
    dplyr::summarise(cases = sum(.data$cases),
                     deaths = sum(.data$deaths),
                     cost = sum(.data$total_cost)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(y = (.data$cases * (sum(.data$deaths) / sum(.data$cases))) + .data$deaths)
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
  # Filter for dominant solutions
  optim_data <- x %>%
    dominant()
  
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
#' @param budget_prop Vector of proportions: these are the budgets as a proportion of the GP budget
#'
#' @return Optimisated output
#' @export
multi_optimisation <- function(x, budget_prop){
  budgets <- sum(dplyr::filter(x, .data$replenishment == "gp")$cost) * budget_prop
  
  # In the first phase (when affordable) all solutions must include treatment
  first_phase_data <- x %>%
    dplyr::filter(grepl("tx", .data$replenishment) | grepl("gp", .data$replenishment))
  # Estimate the minimum budget required to obtain a first phase solution
  min_budget_first_phase <- first_phase_data %>%
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur) %>%
    dplyr::filter(.data$cost == min(.data$cost)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data$cost) %>%
    sum()
  # In the second phase (when first phase not affordable) all interventions except treatment have been removed
  second_phase_data <- x %>%
    dplyr::filter(.data$replenishment %in% c("tx", "none"))
  # Estimate the minimum budget required to obtain a second phase solution
  min_budget_second_phase <- second_phase_data %>%
    dplyr::group_by(.data$NAME_0, .data$NAME_1, .data$NAME_2, .data$ur) %>%
    dplyr::filter(.data$cost == min(.data$cost)) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::pull(.data$cost) %>%
    sum()
  # Run the optimisation for all budgets (if budget prop == 1 we "force" the GP solution currently)
  out <- list()
  for(i in seq_along(budgets)){
    if(budget_prop[i] == 1){
      out[[i]] <- dplyr::filter(x, .data$replenishment == "gp")
    } else {
      if(budgets[i] >= min_budget_first_phase){
        out[[i]] <- single_optimisation(first_phase_data, budgets[i])
      } else {
        if(budgets[i] < min_budget_second_phase){
          out[[i]] <- NULL
        }
        out[[i]] <- single_optimisation(second_phase_data, budgets[i])
      }
    }
    out[[i]] <- out[[i]] %>%
      dplyr::mutate(budget = budgets[i],
                    budget_prop = budget_prop[i])
  }
  return(out)
}