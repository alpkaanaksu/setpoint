# ====== Packages ======

library(dplyr)
library(data.table)
library(ggplot2)

# ====== Helpers ======

#' A generic logistic function
#' 
#' @param x 
#' @param d
#' @param a
#' @param min
#' @param max
logistic <- function (x, d = 0, a = 1, min = 0, max = 1) {
  min + (max - min)/(1 + exp(a * (d - x)))
}

#' Logistic function that delivers values between 0 - 1 for the interval 0 - 1
#'
#' @param x
logistic_0_1 <- function(x) {
  logistic(x, d = 0.5, a = 10)
}

#' Function representing a linear relationship.
#' Overflows are prevented.
#' 
#' @param x
#' @param min_y Minimum y value
#' @param max_y Maximum y value
#' @param min_x Minimum y value
#' @param max_x Maximum y value
#' 
limited_linear <- function(
    x,
    min_y = 0, max_y = 1,
    min_x = 0, max_x = 1
  ) {
  slope = (max_y - min_y) / (max_x - min_x)
  
  result <- case_when(
    x > max_x ~ max_y,
    x < min_x ~ min_y,
    TRUE ~ slope * x
  )
}

# ====== Relationships ======

#' Calculate initial emotional reaction of the person
#' 
#' @param life_event Objective valence of the life event
#' @param  emotional_reactivity Tendency of the person to be influenced emotionally by external events
initial_reaction <- function (life_event, emotional_reactivity) {
    life_event * emotional_reactivity
}

#' Calculate the difference between the SWB and set point
#' 
#' @param SWB Current SWB
#' @param SWB_set_point Baseline level of SWB for a person
delta_SWB <- function (SWB, SWB_set_point) {
    SWB - SWB_set_point
}

#' Calculate the adaptation rate for a specific person at a specific delta SWB
#' 
#' @param delta_SWB_arg Current delta SWB
#' @param adaptation_rate_person Personal adaptation rate
adaptation_rate <- function(delta_SWB_arg, adaptation_rate_person) {
  (-delta_SWB_arg) * adaptation_rate_person
}

#' Calculate the SWB given current SWB and adaptation rate
#' 
#' @param SWB Current SWB
#' @param adaptation_rate Adaptation rate at the moment
SWB <- function (SWB, adaptation_rate) {
  limited_linear(SWB + adaptation_rate)
}


#' Calculate the SWB for a given person for the next iteration
#' 
#' @param person A list representing a person
SWB_chain <- function(person) {
  delta_SWB_var <- delta_SWB(person$SWB, person$SWB_set_point)
  adaptation_rate_var <- adaptation_rate(delta_SWB_var, person$adaptation_rate)
  SWB_var <- SWB(person$SWB, adaptation_rate_var)
  
  person$SWB <- SWB_var
  person
}

#' Calculate the SWB for multiple persons after a life event
#' 
#' @param persons A data.table containing the persons
#' @param life_event Objective value of a life event
event <- function(persons, life_event) {
  persons[, SWB := initial_reaction(life_event, emotional_reactivity) + SWB]
}

#' Simulate an adaptation
#' 
#' @param persons A data.table containing the persons
#' @param years Years to wait
adaptation <- function(persons, years) {
  for(i in seq_len(years)) {
    # Take current row (.SD) as list and use it for the SWB_chain function
    persons[, SWB := SWB_chain(as.list(.SD))$SWB]
  }
}


# ====== Simulation ======

# A person is represented by a list:
# list(emotional_reactivity, adaptation_rate, SWB_set_point)

#' Run a t-test using a simulated group
#' 
#' @param n Sample size
#' @param years Years to wait after the life event
#' @param life_event Objective value of the life event
test_adaptation <- function (n, years, life_event) {
  # Simulate data
  data <- data.table(
    emotional_reactivity = runif(n, 0.5, 1),
    adaptation_rate = runif(n, 0.4, 0.6),
    SWB_set_point = runif(n, 0, 1)
  )
  
  data[, SWB := SWB_set_point]
  
  # Pre measurement
  SWB_pre <- data$SWB
  
  # Simulate waiting
  data |>
    event(0.2) |>
    adaptation(years)
  
  # Post measurement
  SWB_post <- data$SWB
  
  # Test
  t.test(SWB_pre, SWB_post)
}

#' Plot a single person
#' 
#' @param person A list representing a person
#' @param years Years to wait after the life event
#' @param life_event Objective value of the life event
plot_single_person <- function (person, years, life_event) {
  person$SWB <- person$SWB_set_point + initial_reaction(life_event, person$emotional_reactivity)
  
  person_data <- data.table() |> rbind(person)
  
  for(i in seq_len(years)) {
    person <- SWB_chain(person)
    person_data <- rbind(person_data, person)
  }
  
  person_data[, year := 0:years]
  
  person_data |>
    ggplot(aes(year, SWB)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = person$SWB_set_point, color = "blue") +
    geom_vline(xintercept = 0, color = "gray") +
    annotate("text", x = 0, y = person$SWB_set_point, label = "Personal Set Point", hjust = 0, vjust = 1.5, color = "blue")
}

