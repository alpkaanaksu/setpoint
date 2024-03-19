
# A person is represented by a list:
# list(emotional_reactivity, adaptation_rate, SWB_set_point)

# ====== Helpers ======

library(dplyr)

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

adaptation_rate <- function(delta_SWB_arg, adaptation_rate_person) {
  (-delta_SWB_arg) * adaptation_rate_person
}

SWB <- function (SWB, adaptation_rate) {
  limited_linear(SWB + adaptation_rate)
}

event <- function(persons, life_event) {
  persons[, SWB := initial_reaction(life_event, emotional_reactivity) + SWB]
}

SWB_chain <- function(person) {
  delta_SWB_var <- delta_SWB(person$SWB, person$SWB_set_point)
  adaptation_rate_var <- adaptation_rate(delta_SWB_var, person$adaptation_rate)
  SWB_var <- SWB(person$SWB, adaptation_rate_var)
  
  person$SWB <- SWB_var
  person
}

adaptation <- function(persons, years) {
  for(i in seq_len(years)) {
    # Take current row (.SD) as list and use it for the SWB_chain function
    persons[, SWB := SWB_chain(as.list(.SD))$SWB]
  }
}


# ====== Simulation ======

library("data.table")

test_adaptation <- function (n, years, life_event) {
  data <- data.table(
    emotional_reactivity = runif(n, 0.5, 1),
    adaptation_rate = runif(n, 0.4, 0.6),
    SWB_set_point = runif(n, 0, 1)
  )
  
  data[, SWB := SWB_set_point]
  
  SWB_pre <- data$SWB
  
  data |>
    event(0.2) |>
    adaptation(years)
  
  SWB_post <- data$SWB
  
  # Test
  
  t.test(SWB_pre, SWB_post)
}

# Plot a single person

plot_single_person <- function (years, life_event) {
  person <- list(
    emotional_reactivity = runif(1, 0.5, 1),
    adaptation_rate = runif(1, 0.4, 0.6),
    SWB_set_point = runif(1, 0, 1)
  )
  
  person$SWB <- person$SWB_set_point + initial_reaction(life_event, person$emotional_reactivity)
  
  person_data <- data.table() |> rbind(person)
  
  for(i in seq_len(years)) {
    person <- SWB_chain(person)
    person_data <- rbind(person_data, person)
  }
  
  person_data[, year := 0:years]
  
  library(ggplot2)
  
  person_data |>
    ggplot(aes(year, SWB)) +
    geom_point() +
    geom_line() +
    geom_hline(yintercept = person$SWB_set_point, color = "blue") +
    geom_vline(xintercept = 0, color = "gray") +
    annotate("text", x = 0, y = person$SWB_set_point, label = "Personal Set Point", hjust = 0, vjust = 1.5, color = "blue")
}

test_adaptation(1000, 5, -0.2)
plot_single_person(5, -1)

