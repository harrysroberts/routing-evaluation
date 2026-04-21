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
    
    #Remove 15 instances where a walk, bike, car or taxi attribute is NA
    !if_any(c(walk_distance_GR:car_time_GR,taxi_cost_GR,taxi_time_GR), is.na),
    
    #Remove further 62 instances where bus was chosen despite being unavailable
    !(choice == 5 & av_bus_GR == 0),

    #Remove further 36 instances where rail was chosen despite being unavailable
    !(choice == 6 & av_rail_GR == 0)
    
    ) %>%
  
  mutate(
    
    #Where another method returns NA, set it as equal to the corresponding R5 attribute
    walk_time_R5 = if_else(is.na(walk_time_R5),walk_time_GR,walk_time_R5),
    walk_time_DO = if_else(is.na(walk_time_DO),walk_time_GR,walk_time_DO),
    bike_time_R5 = if_else(is.na(bike_time_R5),bike_time_GR,bike_time_R5),
    bike_time_DO = if_else(is.na(bike_time_DO),bike_time_GR,bike_time_DO),
    car_cost_R5 = if_else(is.na(car_cost_R5),car_cost_GR,car_cost_R5),
    car_cost_DO = if_else(is.na(car_cost_DO),car_cost_GR,car_cost_DO),
    car_time_R5 = if_else(is.na(car_time_R5),car_time_GR,car_time_R5),
    car_time_DO = if_else(is.na(car_time_DO),car_time_GR,car_time_DO),
    taxi_cost_R5 = if_else(is.na(taxi_cost_R5),taxi_cost_GR,taxi_cost_R5),
    taxi_cost_DO = if_else(is.na(taxi_cost_DO),taxi_cost_GR,taxi_cost_DO),
    taxi_time_R5 = if_else(is.na(taxi_time_R5),taxi_time_GR,taxi_time_R5),
    taxi_time_DO = if_else(is.na(taxi_time_DO),taxi_time_GR,taxi_time_DO),
    bus_cost_R5 = if_else(is.na(bus_cost_R5),bus_cost_GR,bus_cost_R5),
    bus_time_R5 = if_else(is.na(bus_time_R5),bus_time_GR,bus_time_R5),
    rail_cost_R5 = if_else(is.na(rail_cost_R5),rail_cost_GR,rail_cost_R5),
    rail_time_R5 = if_else(is.na(rail_time_R5),rail_time_GR,rail_time_R5),
    
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
  V[["walk"]] = asc_walk + beta * (                                                                                                   + theta * (walk_time_GR + delta_R5 * (walk_time_R5 - walk_time_GR) + delta_DO * (walk_time_DO - walk_time_GR))) 
  V[["bike"]] = asc_bike + beta * (                                                                                                   + theta * (bike_time_GR + delta_R5 * (bike_time_R5 - bike_time_GR) + delta_DO * (bike_time_DO - bike_time_GR))) 
  V[["car"]]  = asc_car  + beta * (car_cost_GR  + gamma_R5 * (car_cost_R5  - car_cost_GR ) + gamma_DO * (car_cost_DO  - car_cost_GR ) + theta * (car_time_GR  + delta_R5 * (car_time_R5  - car_time_GR ) + delta_DO * (car_time_DO - car_time_GR  )))
  V[["taxi"]] = asc_taxi + beta * (taxi_cost_GR + gamma_R5 * (taxi_cost_R5 - taxi_cost_GR) + gamma_DO * (taxi_cost_DO - taxi_cost_GR) + theta * (taxi_time_GR + delta_R5 * (taxi_time_R5 - car_time_GR ) + delta_DO * (taxi_time_DO - taxi_time_GR)))
 
  V[["bus"]]  = asc_bus  + beta * (bus_cost_GR  + gamma_R5 * (bus_cost_R5  - bus_cost_GR ) + gamma_DO * (bus_cost_R5  - bus_cost_GR ) + theta * (bus_time_GR  + delta_R5 * (bus_time_R5  - bus_time_GR ) + delta_DO * (bus_time_R5  - bus_time_GR )                                           ))
  V[["rail"]] = asc_rail + beta * (rail_cost_GR + gamma_R5 * (rail_cost_R5 - rail_cost_GR) + gamma_DO * (rail_cost_R5 - rail_cost_GR) + theta * (rail_time_GR + delta_R5 * (rail_time_R5 - rail_time_GR) + delta_DO * (rail_time_R5 - rail_time_GR)                                           ))
  

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(walk=1, bike=2, car=3, taxi=4, bus=5, rail=6),
    avail         = list(walk=1, bike=1, car=1, taxi = 1, bus=av_bus_GR, rail=av_rail_GR),
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
  gamma_DO = 0,
  theta    = 0,
  delta_R5 = 0,
  delta_DO = 0
)

# ################################################################# #
#### ESTIMATE MODELS                                             ####
# ################################################################# #


#GR base
apollo_control = list(
  modelName       = "GR_base",
  modelDescr      = "MNL model on DECISIONS using only GR attributes",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_R5","gamma_DO","delta_R5","delta_DO")
apollo_inputs = apollo_validateInputs()
GR_base <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
GR_base_ouput <- tibble(
  name = "GR_base",
  loglikelihood =  round(GR_base$finalLL,0),
  LRT = NA,
  adj_rho_2 = round(GR_base$adjRho2_C,3),
  asc_bike = round(GR_base$estimate[["asc_bike"]],3),
  asc_bike_rse = round(GR_base$robse[["asc_bike"]],3),
  asc_car = round(GR_base$estimate[["asc_car"]],3),
  asc_car_rse = round(GR_base$robse[["asc_car"]],3),
  asc_taxi = round(GR_base$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(GR_base$robse[["asc_taxi"]],3),
  asc_bus = round(GR_base$estimate[["asc_bus"]],3),
  asc_bus_rse = round(GR_base$robse[["asc_bus"]],3),
  asc_rail = round(GR_base$estimate[["asc_rail"]],3),
  asc_rail_rse = round(GR_base$robse[["asc_rail"]],3),
  beta = round(GR_base$estimate[["beta"]],3),
  beta_rse = round(GR_base$robse[["beta"]],3),
  gamma_R5 = round(GR_base$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(GR_base$robse[["gamma_R5"]],3),
  gamma_DO = round(GR_base$estimate[["gamma_DO"]],3),
  gamma_DO_rse = round(GR_base$robse[["gamma_DO"]],3),
  theta = round(GR_base$estimate[["theta"]],3),
  theta_rse = round(GR_base$robse[["theta"]],3),
  delta_R5 = round(GR_base$estimate[["delta_R5"]],3),
  delta_R5_rse = round(GR_base$robse[["delta_R5"]],3),
  delta_DO = round(GR_base$estimate[["delta_DO"]],3),
  delta_DO_rse = round(GR_base$robse[["delta_DO"]],3)
)

#GR/R5 cost
apollo_control = list(
  modelName       = "GR_R5_cost",
  modelDescr      = "MNL model on DECISIONS using GR attributes with R5 cost perturbation",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_DO","delta_R5","delta_DO")
apollo_inputs = apollo_validateInputs()
GR_R5_cost <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
GR_R5_cost_ouput <- tibble(
  name = "GR_R5_cost",
  loglikelihood =  round(GR_R5_cost$finalLL,0),
  LRT = round(apollo_lrTest(GR_base,GR_R5_cost),3),
  adj_rho_2 = round(GR_R5_cost$adjRho2_C,3),
  asc_bike = round(GR_R5_cost$estimate[["asc_bike"]],3),
  asc_bike_rse = round(GR_R5_cost$robse[["asc_bike"]],3),
  asc_car = round(GR_R5_cost$estimate[["asc_car"]],3),
  asc_car_rse = round(GR_R5_cost$robse[["asc_car"]],3),
  asc_taxi = round(GR_R5_cost$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(GR_R5_cost$robse[["asc_taxi"]],3),
  asc_bus = round(GR_R5_cost$estimate[["asc_bus"]],3),
  asc_bus_rse = round(GR_R5_cost$robse[["asc_bus"]],3),
  asc_rail = round(GR_R5_cost$estimate[["asc_rail"]],3),
  asc_rail_rse = round(GR_R5_cost$robse[["asc_rail"]],3),
  beta = round(GR_R5_cost$estimate[["beta"]],3),
  beta_rse = round(GR_R5_cost$robse[["beta"]],3),
  gamma_R5 = round(GR_R5_cost$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(GR_R5_cost$robse[["gamma_R5"]],3),
  gamma_DO = round(GR_R5_cost$estimate[["gamma_DO"]],3),
  gamma_DO_rse = round(GR_R5_cost$robse[["gamma_DO"]],3),
  theta = round(GR_R5_cost$estimate[["theta"]],3),
  theta_rse = round(GR_R5_cost$robse[["theta"]],3),
  delta_R5 = round(GR_R5_cost$estimate[["delta_R5"]],3),
  delta_R5_rse = round(GR_R5_cost$robse[["delta_R5"]],3),
  delta_DO = round(GR_R5_cost$estimate[["delta_DO"]],3),
  delta_DO_rse = round(GR_R5_cost$robse[["delta_DO"]],3)
)

#GR/DO cost
apollo_control = list(
  modelName       = "GR_DO_cost",
  modelDescr      = "MNL model on DECISIONS using GR attributes with DO cost perturbation",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_R5","delta_R5","delta_DO")
apollo_inputs = apollo_validateInputs()
GR_DO_cost <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
GR_DO_cost_ouput <- tibble(
  name = "GR_DO_cost",
  loglikelihood =  round(GR_DO_cost$finalLL,0),
  LRT = round(apollo_lrTest(GR_base,GR_DO_cost),3),
  adj_rho_2 = round(GR_DO_cost$adjRho2_C,3),
  asc_bike = round(GR_DO_cost$estimate[["asc_bike"]],3),
  asc_bike_rse = round(GR_DO_cost$robse[["asc_bike"]],3),
  asc_car = round(GR_DO_cost$estimate[["asc_car"]],3),
  asc_car_rse = round(GR_DO_cost$robse[["asc_car"]],3),
  asc_taxi = round(GR_DO_cost$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(GR_DO_cost$robse[["asc_taxi"]],3),
  asc_bus = round(GR_DO_cost$estimate[["asc_bus"]],3),
  asc_bus_rse = round(GR_DO_cost$robse[["asc_bus"]],3),
  asc_rail = round(GR_DO_cost$estimate[["asc_rail"]],3),
  asc_rail_rse = round(GR_DO_cost$robse[["asc_rail"]],3),
  beta = round(GR_DO_cost$estimate[["beta"]],3),
  beta_rse = round(GR_DO_cost$robse[["beta"]],3),
  gamma_R5 = round(GR_DO_cost$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(GR_DO_cost$robse[["gamma_R5"]],3),
  gamma_DO = round(GR_DO_cost$estimate[["gamma_DO"]],3),
  gamma_DO_rse = round(GR_DO_cost$robse[["gamma_DO"]],3),
  theta = round(GR_DO_cost$estimate[["theta"]],3),
  theta_rse = round(GR_DO_cost$robse[["theta"]],3),
  delta_R5 = round(GR_DO_cost$estimate[["delta_R5"]],3),
  delta_R5_rse = round(GR_DO_cost$robse[["delta_R5"]],3),
  delta_DO = round(GR_DO_cost$estimate[["delta_DO"]],3),
  delta_DO_rse = round(GR_DO_cost$robse[["delta_DO"]],3)
)

#GR/R5 time
apollo_control = list(
  modelName       = "GR_R5_time",
  modelDescr      = "MNL model on DECISIONS using GR attributes with R5 time perturbation",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_R5","gamma_DO","delta_DO")
apollo_inputs = apollo_validateInputs()
GR_R5_time <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
GR_R5_time_ouput <- tibble(
  name = "GR_R5_time",
  loglikelihood =  round(GR_R5_time$finalLL,0),
  LRT = round(apollo_lrTest(GR_base,GR_R5_time),3),
  adj_rho_2 = round(GR_R5_time$adjRho2_C,3),
  asc_bike = round(GR_R5_time$estimate[["asc_bike"]],3),
  asc_bike_rse = round(GR_R5_time$robse[["asc_bike"]],3),
  asc_car = round(GR_R5_time$estimate[["asc_car"]],3),
  asc_car_rse = round(GR_R5_time$robse[["asc_car"]],3),
  asc_taxi = round(GR_R5_time$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(GR_R5_time$robse[["asc_taxi"]],3),
  asc_bus = round(GR_R5_time$estimate[["asc_bus"]],3),
  asc_bus_rse = round(GR_R5_time$robse[["asc_bus"]],3),
  asc_rail = round(GR_R5_time$estimate[["asc_rail"]],3),
  asc_rail_rse = round(GR_R5_time$robse[["asc_rail"]],3),
  beta = round(GR_R5_time$estimate[["beta"]],3),
  beta_rse = round(GR_R5_time$robse[["beta"]],3),
  gamma_R5 = round(GR_R5_time$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(GR_R5_time$robse[["gamma_R5"]],3),
  gamma_DO = round(GR_R5_time$estimate[["gamma_DO"]],3),
  gamma_DO_rse = round(GR_R5_time$robse[["gamma_DO"]],3),
  theta = round(GR_R5_time$estimate[["theta"]],3),
  theta_rse = round(GR_R5_time$robse[["theta"]],3),
  delta_R5 = round(GR_R5_time$estimate[["delta_R5"]],3),
  delta_R5_rse = round(GR_R5_time$robse[["delta_R5"]],3),
  delta_DO = round(GR_R5_time$estimate[["delta_DO"]],3),
  delta_DO_rse = round(GR_R5_time$robse[["delta_DO"]],3)
)

#GR/DO time
apollo_control = list(
  modelName       = "GR_DO_time",
  modelDescr      = "MNL model on DECISIONS using GR attributes with DO time perturbation",
  indivID         = "ID", 
  outputDirectory = "output/choicemodels"
)
apollo_fixed = c("asc_walk","gamma_R5","gamma_DO","delta_R5")
apollo_inputs = apollo_validateInputs()
GR_DO_time <- apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)
GR_DO_time_ouput <- tibble(
  name = "GR_DO_time",
  loglikelihood =  round(GR_DO_time$finalLL,0),
  LRT = round(apollo_lrTest(GR_base,GR_DO_time),3),
  adj_rho_2 = round(GR_DO_time$adjRho2_C,3),
  asc_bike = round(GR_DO_time$estimate[["asc_bike"]],3),
  asc_bike_rse = round(GR_DO_time$robse[["asc_bike"]],3),
  asc_car = round(GR_DO_time$estimate[["asc_car"]],3),
  asc_car_rse = round(GR_DO_time$robse[["asc_car"]],3),
  asc_taxi = round(GR_DO_time$estimate[["asc_taxi"]],3),
  asc_taxi_rse = round(GR_DO_time$robse[["asc_taxi"]],3),
  asc_bus = round(GR_DO_time$estimate[["asc_bus"]],3),
  asc_bus_rse = round(GR_DO_time$robse[["asc_bus"]],3),
  asc_rail = round(GR_DO_time$estimate[["asc_rail"]],3),
  asc_rail_rse = round(GR_DO_time$robse[["asc_rail"]],3),
  beta = round(GR_DO_time$estimate[["beta"]],3),
  beta_rse = round(GR_DO_time$robse[["beta"]],3),
  gamma_R5 = round(GR_DO_time$estimate[["gamma_R5"]],3),
  gamma_R5_rse = round(GR_DO_time$robse[["gamma_R5"]],3),
  gamma_DO = round(GR_DO_time$estimate[["gamma_DO"]],3),
  gamma_DO_rse = round(GR_DO_time$robse[["gamma_DO"]],3),
  theta = round(GR_DO_time$estimate[["theta"]],3),
  theta_rse = round(GR_DO_time$robse[["theta"]],3),
  delta_R5 = round(GR_DO_time$estimate[["delta_R5"]],3),
  delta_R5_rse = round(GR_DO_time$robse[["delta_R5"]],3),
  delta_DO = round(GR_DO_time$estimate[["delta_DO"]],3),
  delta_DO_rse = round(GR_DO_time$robse[["delta_DO"]],3)
)

GR_output <- bind_rows(
  GR_base_ouput,
  GR_R5_cost_ouput,
  GR_DO_cost_ouput,
  GR_R5_time_ouput,
  GR_DO_time_ouput
)

apollo_saveOutput(GR_base)
apollo_saveOutput(GR_R5_cost)
apollo_saveOutput(GR_DO_cost)
apollo_saveOutput(GR_R5_time)
apollo_saveOutput(GR_DO_time)

write_csv(GR_output,"output/choicemodels/GR_output.csv")

