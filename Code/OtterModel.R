# Model Otter from Excel into R

#Date Created: March 14, 2023

#Author: Julia Adelsheim
#Collaborators: Izzy Morgante (Nov, Dec 2022, Jan 2023),
#               Andreas Novotny (12-2-2022),
#               Patrick Pata (1-19-23- 3-14-23)

#Platform: Mac
#Purpose of Script: Change ROR by set amounts and see how energy needs change

# Functions ------------------------------------------------------------


#' Bioenergetic Model of Energy Requirements for Sea Otters
#'
#' Estimates the daily energy requirements of sea otters based on body mass, metabolic rates, and time allocation across behaviors. 
#' The model accounts for growth and, optionally, the additional cost of raising a pup.
#'
#' @param para_data A data frame containing input data for the model. Must include the following columns:
#'   \itemize{
#'     \item \code{Av_mass}: Average body mass (kg).
#'     \item \code{Metabolic_rate}: Metabolic rate (kJ/kg/min) for each behavior.
#'     \item \code{Percentage_of_day}: Proportion of the day spent on each behavior (numeric).
#'     \item \code{Behaviour}: Behavioral states (e.g., "rest", "forage", "activity").
#'     \item \code{Growth}: Daily growth energy requirement (kJ/day).
#'     \item \code{With_pup}: Indicates if the sea otter is raising a pup ("yes" or "no").
#'     \item \code{Sex}: Biological sex ("M" for male, "F" for female).
#'     \item \code{Age_class}: Age class of the otter (e.g., "adult").
#'     \item \code{Year_class}: Specific year class (e.g., 2, 3).
#'   }
#' @param Cost.pup Numeric. Additional energy cost of raising a pup (kJ/day). Default is \code{3931}.
#' @param return.all Logical. If \code{TRUE}, returns the full dataset with intermediate calculations.
#'   If \code{FALSE}, returns a summarized dataset. Default is \code{FALSE}.
#'
#' @return A data frame summarizing energy expenditure or the full dataset if \code{return.all = TRUE}.
#' 
#' @details 
#' This function models sea otter energy expenditure by:
#' \enumerate{
#'   \item Calculating energy expenditure rates (kJ/min) for each behavior based on body mass and metabolic rate.
#'   \item Summing daily energy costs of behaviors.
#'   \item Adding costs for growth and pup care (if applicable).
#'   \item Computing gross and net rates of return from foraging activities.
#'   \item Modeling time allocation and energy costs for resting and foraging.
#' }
#' 
#' @examples
#' \dontrun{
#' # Example input data
#' data <- tibble(
#'   Av_mass = c(30, 25),
#'   Metabolic_rate = c(0.05, 0.04),
#'   Percentage_of_day = c(0.5, 0.4),
#'   Behaviour = c("rest", "forage"),
#'   Growth = c(500, 400),
#'   With_pup = c("yes", "no"),
#'   Sex = c("F", "M"),
#'   Age_class = c("adult", "adult"),
#'   Year_class = c(3, 2)
#' )
#' 
#' # Run the model with summarized results
#' otter_summary <- otteR(data)
#' 
#' # Run the model with all intermediate calculations
#' otter_full <- otteR(data, return.all = TRUE)
#' }
#' 
#' @export

otteR <- function(para_data, Cost.pup = 3931, return.all = FALSE) {
  
  # Total number of minutes in a day
  total_min_per_day <- 1440
  
  # Main calculation pipeline
  output <- para_data %>% 
    # 1. Calculate energy expenditure rate (kJ/min) for each behavior
    #    Based on body mass (kg) and metabolic rate (kJ/kg/min)
    mutate(Behaviour_rate = Av_mass * Metabolic_rate) %>%
    
    # 2. Calculate daily time allocation and energy costs for each behavior
    #    Multiply proportion of day spent on behavior by total minutes per day
    mutate(Minutes_per_day = Percentage_of_day * total_min_per_day) %>% 
    mutate(Actual_costs = Behaviour_rate * Minutes_per_day) %>%
    
    # 3. Aggregate energy costs for all behaviors per day
    #    Reshape data to simplify calculations for different behavior categories
    pivot_wider(names_from = Behaviour, values_from = c(
      Percentage_of_day, Minutes_per_day, Metabolic_rate, Behaviour_rate, Actual_costs)) %>%
    mutate(Sum_actual_costs = Actual_costs_rest + Actual_costs_activity + Actual_costs_forage) %>%
    
    # 4. Add pup care energy cost if applicable
    mutate(Pup_cost = ifelse(With_pup == 'yes', Cost.pup, 0)) %>%
    
    # 5. Calculate gross rate of return (ROR_gross, kJ/min) from foraging
    mutate(ROR_gross = (Sum_actual_costs + Pup_cost + Growth) / Minutes_per_day_forage) %>%
    #    Adjust ROR for adults using predefined year classes
    mutate(ROR_gross =
             ifelse(Age_class != "adult",
                    ROR_gross,
                    ifelse(Sex == "M",
                           mean(pull(filter(.data = ., Sex == "M", Year_class == 3),
                                ROR_gross)),
                           ifelse(With_pup == "no",
                                  mean(pull(filter(.data = ., Sex == "F", With_pup == "no", Year_class == 2),
                                       ROR_gross)),
                                  mean(pull(filter(.data = ., Sex == "F", With_pup == "yes", Year_class == 2),
                                       ROR_gross)))))) %>%
    
    # 6. Calculate net rate of return (ROR_net, kJ/min)
    #    Subtract resting metabolism from gross ROR
    mutate(ROR_net = ROR_gross - (Behaviour_rate_forage - Behaviour_rate_rest)) %>%
    
    # 7. Calculate resting costs during non-foraging periods
    mutate(Resting_cost_wout_foraging = Behaviour_rate_rest * (Minutes_per_day_rest + Minutes_per_day_forage)) %>%
    
    # 8. Model daily time allocation and energy costs for foraging
    mutate(Minutes_per_day_forage_modeled =
             (Actual_costs_activity + Resting_cost_wout_foraging + Pup_cost + Growth) / ROR_net) %>%
    mutate(Percentage_of_day_forage_modeled =
             Minutes_per_day_forage_modeled / total_min_per_day) %>%
    mutate(Actual_costs_forage_modeled =
             Minutes_per_day_forage_modeled * Behaviour_rate_forage) %>%
    
    # 9. Model daily time allocation and energy costs for resting
    mutate(Percentage_of_day_rest_modeled =
             1 - (Percentage_of_day_forage_modeled + Percentage_of_day_activity)) %>%
    mutate(Minutes_per_day_rest_modeled =
             total_min_per_day * Percentage_of_day_rest_modeled) %>%
    mutate(Actual_costs_rest_modeled =
             Minutes_per_day_rest_modeled * Behaviour_rate_rest) %>%
    
    # 10. Calculate total energy expenditure (modeled vs. original)
    mutate(Total_energy_expenditure_modeled =
             Actual_costs_rest_modeled + Actual_costs_forage_modeled +
             Actual_costs_activity + Growth + Pup_cost) %>%
    mutate(Total_energy_expenditure_original =
             Actual_costs_rest + Actual_costs_forage + Actual_costs_activity + Growth + Pup_cost) %>%
    arrange(Sex, With_pup, Year_class)
  
  # Prepare a cleaned summary output
  clean_output <- output %>% 
    select(Sex, With_pup, Age_class, Year_class, Pup_cost, Growth,
           Percentage_of_day_activity, Minutes_per_day_activity, Actual_costs_activity,
           Percentage_of_day_forage_modeled, Minutes_per_day_forage_modeled, Actual_costs_forage_modeled,
           Percentage_of_day_rest_modeled, Minutes_per_day_rest_modeled, Actual_costs_rest_modeled,
           Total_energy_expenditure_modeled)
  
  # Return either full output or summary, based on user preference
  if (return.all == TRUE) {
    return(output)
  } else {
    return(clean_output)
  }
}





