library(reticulate)

pyblp <- import('pyblp')

product_data <- read.csv(pyblp$data$BLP_PRODUCTS_LOCATION)
agent_data <- read.csv(pyblp$data$BLP_AGENTS_LOCATION)

problem <- pyblp$Problem(product_formulations = tuple(
  pyblp$Formulation('1 + hpwt + air + mpd + space'), #  Linear demand
  pyblp$Formulation('1 + prices + hpwt + air + mpd + space'), #Non linear demand
  pyblp$Formulation('1 + log(hpwt) + air + log(mpg) + log(space) + trend') # Supply
),
agent_formulation = pyblp$Formulation('0 + I(1/income)'), # Price interaction
costs_type = 'log', # Log linear costs
product_data = product_data,
agent_data = agent_data
)

results <- problem$solve(
  sigma = diag(c(3.612, 0, 4.628, 1.818, 1.050, 2.056)),
  pi = rbind(0, -43.501, 0, 0, 0, 0),
  initial_update = T,
  costs_bounds = tuple(0.001, NULL),
  W_type = 'clustered',
  se_type = 'clustered'
)

elasticities = results$compute_elasticities()
markups = results$compute_markups()
instrument_results = results$compute_optimal_instruments(method = 'approximate')
updated_problem = instrument_results$to_problem()


results_with_instruments = updated_problem$solve(sigma = diag(c(3.612, 0, 4.628, 1.818, 1.050, 2.056)),
                                                 pi = rbind(0, -43.501, 0, 0, 0, 0),
                                                 initial_update = T,
                                                 costs_bounds = tuple(0.001, NULL),
                                                 W_type = 'clustered',
                                                 se_type = 'clustered')
