# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #

### Clear memory
rm(list = ls())

### Load Apollo library
library(apollo)

### Load tidyverse library
library(tidyverse)

### Initialise code
apollo_initialise()

# ################################################################# #
#### LOAD DATA AND APPLY ANY TRANSFORMATIONS                     ####
# ################################################################# #


database <- read_csv("input/processed/trips_rp.csv") %>%
  
  #Note throughout that R5 values are being used for bus and rail
  
  filter(
    
    #Remove 2 instances where a walk, bike, car or taxi attribute is NA
    !if_any(c(walk_distance_DO:car_time_DO,taxi_cost_DO,taxi_time_DO), is.na),
    
    #Remove further 51 instances where bus was chosen despite being unavailable
    !(choice == 5 & av_bus_R5 == 0),

    #Remove further 12 instances where rail was chosen despite being unavailable
    !(choice == 6 & av_rail_R5 == 0)
    
    ) %>%
  
  mutate(
    
    #Where another method returns NA, set it as equal to the corresponding DO attribute
    walk_time_R5 = if_else(is.na(walk_time_R5),walk_time_DO,walk_time_R5),
    walk_time_GR = if_else(is.na(walk_time_GR),walk_time_DO,walk_time_GR),
    bike_time_R5 = if_else(is.na(bike_time_R5),bike_time_DO,bike_time_R5),
    bike_time_GR = if_else(is.na(bike_time_GR),bike_time_DO,bike_time_GR),
    car_cost_R5 = if_else(is.na(car_cost_R5),car_cost_DO,car_cost_R5),
    car_cost_GR = if_else(is.na(car_cost_GR),car_cost_DO,car_cost_GR),
    car_time_R5 = if_else(is.na(car_time_R5),car_time_DO,car_time_R5),
    car_time_GR = if_else(is.na(car_time_GR),car_time_DO,car_time_GR),
    taxi_cost_R5 = if_else(is.na(taxi_cost_R5),taxi_cost_DO,taxi_cost_R5),
    taxi_cost_GR = if_else(is.na(taxi_cost_GR),taxi_cost_DO,taxi_cost_GR),
    taxi_time_R5 = if_else(is.na(taxi_time_R5),taxi_time_DO,taxi_time_R5),
    taxi_time_GR = if_else(is.na(taxi_time_GR),taxi_time_DO,taxi_time_GR),
    bus_cost_GR = if_else(is.na(bus_cost_GR),bus_cost_R5,bus_cost_GR),
    bus_time_GR = if_else(is.na(bus_time_GR),bus_time_R5,bus_time_GR),
    rail_cost_GR = if_else(is.na(rail_cost_GR),rail_cost_R5,rail_cost_GR),
    rail_time_GR = if_else(is.na(rail_time_GR),rail_time_R5,rail_time_GR),
    
    #Remove NAs as Apollo does not allow them - missing values handled by availability constraints
    across(everything(), ~ replace_na(., 0))
    
  )

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities = function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["walk"]] = asc_walk + beta * (                                                                                                   + theta * (walk_time_DO + delta_R5 * (walk_time_R5 - walk_time_DO) + delta_GR * (walk_time_GR - walk_time_DO))) 
  V[["bike"]] = asc_bike + beta * (                                                                                                   + theta * (bike_time_DO + delta_R5 * (bike_time_R5 - bike_time_DO) + delta_GR * (bike_time_GR - bike_time_DO))) 
  V[["car"]]  = asc_car  + beta * (car_cost_DO  + gamma_R5 * (car_cost_R5  - car_cost_DO ) + gamma_GR * (car_cost_GR  - car_cost_DO ) + theta * (car_time_DO  + delta_R5 * (car_time_R5  - car_time_DO ) + delta_GR * (car_time_GR - car_time_DO  )))
  V[["taxi"]] = asc_taxi + beta * (taxi_cost_DO + gamma_R5 * (taxi_cost_R5 - taxi_cost_DO) + gamma_GR * (taxi_cost_GR - taxi_cost_DO) + theta * (taxi_time_DO + delta_R5 * (taxi_time_R5 - car_time_DO ) + delta_GR * (taxi_time_GR - taxi_time_DO)))
 
  V[["bus"]]  = asc_bus  + beta * (bus_cost_R5                                             + gamma_GR * (bus_cost_GR  - bus_cost_R5 ) + theta * (bus_time_R5                                             + delta_GR * (bus_time_GR - bus_time_R5  )))
  V[["rail"]] = asc_rail + beta * (rail_cost_R5                                            + gamma_GR * (rail_cost_GR - rail_cost_R5) + theta * (rail_time_R5                                            + delta_GR * (rail_time_GR - rail_time_R5)))
  

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(walk=1, bike=2, car=3, taxi=4, bus=5, rail=6),
    avail         = list(walk=1, bike=1, car=1, taxi = 1, bus=av_bus_R5, rail=av_rail_R5),
    choiceVar     = choice,
    utilities     = V
  )
  
  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  asc_walk = 0,
  asc_bike = 0,
  asc_car  = 0,
  asc_taxi = 0,
  asc_bus  = 0,
  asc_rail = 0,
  beta     = 0,
  gamma_R5 = 0,
  gamma_GR = 0,
  theta    = 0,
  delta_R5 = 0,
  delta_GR = 0
)

# ################################################################# #
#### ESTIMATE MODELS                                             ####
# ################################################################# #


#DO base
apollo_control = list(
  modelName       = "DO_base",
  modelDescr      = "MNL model on DECISIONS using only DO attributes",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_R5","gamma_GR","delta_R5","delta_GR")
apollo_inputs = apollo_validateInputs()
DO_base <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
DO_base_ouput <- tibble(
  name = "DO_base",
  loglikelihood =  round(DO_base$finalLL,0),
  LRT = NA,
  adj_rho_2 = round(DO_base$adjRho2_C,3),
  asc_bike = round(DO_base$estimate[["asc_bike"]],3),
  asc_bike_rse = round(DO_base$robse[["asc_bike"]],3),
  asc_car = round(DO_base$estimate[["asc_car"]],3),
  asc_car_rse = round(DO_base$robse[["asc_car"]],3),
  asc_taxi = round(DO_base$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(DO_base$robse[["asc_taxi"]],3),
  asc_bus = round(DO_base$estimate[["asc_bus"]],3),
  asc_bus_rse = round(DO_base$robse[["asc_bus"]],3),
  asc_rail = round(DO_base$estimate[["asc_rail"]],3),
  asc_rail_rse = round(DO_base$robse[["asc_rail"]],3),
  beta = round(DO_base$estimate[["beta"]],3),
  beta_rse = round(DO_base$robse[["beta"]],3),
  gamma_R5 = round(DO_base$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(DO_base$robse[["gamma_R5"]],3),
  gamma_GR = round(DO_base$estimate[["gamma_GR"]],3),
  gamma_GR_rse = round(DO_base$robse[["gamma_GR"]],3),
  theta = round(DO_base$estimate[["theta"]],3),
  theta_rse = round(DO_base$robse[["theta"]],3),
  delta_R5 = round(DO_base$estimate[["delta_R5"]],3),
  delta_R5_rse = round(DO_base$robse[["delta_R5"]],3),
  delta_GR = round(DO_base$estimate[["delta_GR"]],3),
  delta_GR_rse = round(DO_base$robse[["delta_GR"]],3)
)

#DO/R5 cost
apollo_control = list(
  modelName       = "DO_R5_cost",
  modelDescr      = "MNL model on DECISIONS using DO attributes with R5 cost perturbation",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_GR","delta_R5","delta_GR")
apollo_inputs = apollo_validateInputs()
DO_R5_cost <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
DO_R5_cost_ouput <- tibble(
  name = "DO_R5_cost",
  loglikelihood =  round(DO_R5_cost$finalLL,0),
  LRT = round(apollo_lrTest(DO_base,DO_R5_cost),3),
  adj_rho_2 = round(DO_R5_cost$adjRho2_C,3),
  asc_bike = round(DO_R5_cost$estimate[["asc_bike"]],3),
  asc_bike_rse = round(DO_R5_cost$robse[["asc_bike"]],3),
  asc_car = round(DO_R5_cost$estimate[["asc_car"]],3),
  asc_car_rse = round(DO_R5_cost$robse[["asc_car"]],3),
  asc_taxi = round(DO_R5_cost$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(DO_R5_cost$robse[["asc_taxi"]],3),
  asc_bus = round(DO_R5_cost$estimate[["asc_bus"]],3),
  asc_bus_rse = round(DO_R5_cost$robse[["asc_bus"]],3),
  asc_rail = round(DO_R5_cost$estimate[["asc_rail"]],3),
  asc_rail_rse = round(DO_R5_cost$robse[["asc_rail"]],3),
  beta = round(DO_R5_cost$estimate[["beta"]],3),
  beta_rse = round(DO_R5_cost$robse[["beta"]],3),
  gamma_R5 = round(DO_R5_cost$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(DO_R5_cost$robse[["gamma_R5"]],3),
  gamma_GR = round(DO_R5_cost$estimate[["gamma_GR"]],3),
  gamma_GR_rse = round(DO_R5_cost$robse[["gamma_GR"]],3),
  theta = round(DO_R5_cost$estimate[["theta"]],3),
  theta_rse = round(DO_R5_cost$robse[["theta"]],3),
  delta_R5 = round(DO_R5_cost$estimate[["delta_R5"]],3),
  delta_R5_rse = round(DO_R5_cost$robse[["delta_R5"]],3),
  delta_GR = round(DO_R5_cost$estimate[["delta_GR"]],3),
  delta_GR_rse = round(DO_R5_cost$robse[["delta_GR"]],3)
)

#DO/GR cost
apollo_control = list(
  modelName       = "DO_GR_cost",
  modelDescr      = "MNL model on DECISIONS using DO attributes with GR cost perturbation",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_R5","delta_R5","delta_GR")
apollo_inputs = apollo_validateInputs()
DO_GR_cost <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
DO_GR_cost_ouput <- tibble(
  name = "DO_GR_cost",
  loglikelihood =  round(DO_GR_cost$finalLL,0),
  LRT = round(apollo_lrTest(DO_base,DO_GR_cost),3),
  adj_rho_2 = round(DO_GR_cost$adjRho2_C,3),
  asc_bike = round(DO_GR_cost$estimate[["asc_bike"]],3),
  asc_bike_rse = round(DO_GR_cost$robse[["asc_bike"]],3),
  asc_car = round(DO_GR_cost$estimate[["asc_car"]],3),
  asc_car_rse = round(DO_GR_cost$robse[["asc_car"]],3),
  asc_taxi = round(DO_GR_cost$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(DO_GR_cost$robse[["asc_taxi"]],3),
  asc_bus = round(DO_GR_cost$estimate[["asc_bus"]],3),
  asc_bus_rse = round(DO_GR_cost$robse[["asc_bus"]],3),
  asc_rail = round(DO_GR_cost$estimate[["asc_rail"]],3),
  asc_rail_rse = round(DO_GR_cost$robse[["asc_rail"]],3),
  beta = round(DO_GR_cost$estimate[["beta"]],3),
  beta_rse = round(DO_GR_cost$robse[["beta"]],3),
  gamma_R5 = round(DO_GR_cost$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(DO_GR_cost$robse[["gamma_R5"]],3),
  gamma_GR = round(DO_GR_cost$estimate[["gamma_GR"]],3),
  gamma_GR_rse = round(DO_GR_cost$robse[["gamma_GR"]],3),
  theta = round(DO_GR_cost$estimate[["theta"]],3),
  theta_rse = round(DO_GR_cost$robse[["theta"]],3),
  delta_R5 = round(DO_GR_cost$estimate[["delta_R5"]],3),
  delta_R5_rse = round(DO_GR_cost$robse[["delta_R5"]],3),
  delta_GR = round(DO_GR_cost$estimate[["delta_GR"]],3),
  delta_GR_rse = round(DO_GR_cost$robse[["delta_GR"]],3)
)

#DO/R5 time
apollo_control = list(
  modelName       = "DO_R5_time",
  modelDescr      = "MNL model on DECISIONS using DO attributes with R5 time perturbation",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_R5","gamma_GR","delta_GR")
apollo_inputs = apollo_validateInputs()
DO_R5_time <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
DO_R5_time_ouput <- tibble(
  name = "DO_R5_time",
  loglikelihood =  round(DO_R5_time$finalLL,0),
  LRT = round(apollo_lrTest(DO_base,DO_R5_time),3),
  adj_rho_2 = round(DO_R5_time$adjRho2_C,3),
  asc_bike = round(DO_R5_time$estimate[["asc_bike"]],3),
  asc_bike_rse = round(DO_R5_time$robse[["asc_bike"]],3),
  asc_car = round(DO_R5_time$estimate[["asc_car"]],3),
  asc_car_rse = round(DO_R5_time$robse[["asc_car"]],3),
  asc_taxi = round(DO_R5_time$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(DO_R5_time$robse[["asc_taxi"]],3),
  asc_bus = round(DO_R5_time$estimate[["asc_bus"]],3),
  asc_bus_rse = round(DO_R5_time$robse[["asc_bus"]],3),
  asc_rail = round(DO_R5_time$estimate[["asc_rail"]],3),
  asc_rail_rse = round(DO_R5_time$robse[["asc_rail"]],3),
  beta = round(DO_R5_time$estimate[["beta"]],3),
  beta_rse = round(DO_R5_time$robse[["beta"]],3),
  gamma_R5 = round(DO_R5_time$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(DO_R5_time$robse[["gamma_R5"]],3),
  gamma_GR = round(DO_R5_time$estimate[["gamma_GR"]],3),
  gamma_GR_rse = round(DO_R5_time$robse[["gamma_GR"]],3),
  theta = round(DO_R5_time$estimate[["theta"]],3),
  theta_rse = round(DO_R5_time$robse[["theta"]],3),
  delta_R5 = round(DO_R5_time$estimate[["delta_R5"]],3),
  delta_R5_rse = round(DO_R5_time$robse[["delta_R5"]],3),
  delta_GR = round(DO_R5_time$estimate[["delta_GR"]],3),
  delta_GR_rse = round(DO_R5_time$robse[["delta_GR"]],3)
)

#DO/GR time
apollo_control = list(
  modelName       = "DO_GR_time",
  modelDescr      = "MNL model on DECISIONS using DO attributes with GR time perturbation",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_R5","gamma_GR","delta_R5")
apollo_inputs = apollo_validateInputs()
DO_GR_time <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
DO_GR_time_ouput <- tibble(
  name = "DO_GR_time",
  loglikelihood =  round(DO_GR_time$finalLL,0),
  LRT = round(apollo_lrTest(DO_base,DO_GR_time),3),
  adj_rho_2 = round(DO_GR_time$adjRho2_C,3),
  asc_bike = round(DO_GR_time$estimate[["asc_bike"]],3),
  asc_bike_rse = round(DO_GR_time$robse[["asc_bike"]],3),
  asc_car = round(DO_GR_time$estimate[["asc_car"]],3),
  asc_car_rse = round(DO_GR_time$robse[["asc_car"]],3),
  asc_taxi = round(DO_GR_time$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(DO_GR_time$robse[["asc_taxi"]],3),
  asc_bus = round(DO_GR_time$estimate[["asc_bus"]],3),
  asc_bus_rse = round(DO_GR_time$robse[["asc_bus"]],3),
  asc_rail = round(DO_GR_time$estimate[["asc_rail"]],3),
  asc_rail_rse = round(DO_GR_time$robse[["asc_rail"]],3),
  beta = round(DO_GR_time$estimate[["beta"]],3),
  beta_rse = round(DO_GR_time$robse[["beta"]],3),
  gamma_R5 = round(DO_GR_time$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(DO_GR_time$robse[["gamma_R5"]],3),
  gamma_GR = round(DO_GR_time$estimate[["gamma_GR"]],3),
  gamma_GR_rse = round(DO_GR_time$robse[["gamma_GR"]],3),
  theta = round(DO_GR_time$estimate[["theta"]],3),
  theta_rse = round(DO_GR_time$robse[["theta"]],3),
  delta_R5 = round(DO_GR_time$estimate[["delta_R5"]],3),
  delta_R5_rse = round(DO_GR_time$robse[["delta_R5"]],3),
  delta_GR = round(DO_GR_time$estimate[["delta_GR"]],3),
  delta_GR_rse = round(DO_GR_time$robse[["delta_GR"]],3)
)

DO_output <- bind_rows(
  DO_base_ouput,
  DO_R5_cost_ouput,
  DO_GR_cost_ouput,
  DO_R5_time_ouput,
  DO_GR_time_ouput
)

apollo_saveOutput(DO_base)
apollo_saveOutput(DO_R5_cost)
apollo_saveOutput(DO_GR_cost)
apollo_saveOutput(DO_R5_time)
apollo_saveOutput(DO_GR_time)

write_csv(DO_output,"output/choicemodels/DO_output.csv")

