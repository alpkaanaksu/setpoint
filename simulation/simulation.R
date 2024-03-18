
# A person is represented by a list:
# list(sensitivity, adaptation_rate, SWB_set_point)

# ====== Relationships ======

initial_reaction <- function (life_event, sensitivity) {
    life_event * sensitivity
}

delta_SWB <- function (SWB, SWB_set_point) {
    SWB - SWB_set_point
}

adaptation_rate <- function(delta_SWB_arg, adaptation_rate_person) {
  (-delta_SWB_arg) * adaptation_rate_person
}

SWB <- function (SWB, adaptation_rate) {
  SWB + adaptation_rate
}

event <- function(persons, life_event) {
  persons[, SWB := initial_reaction(life_event, sensitivity) + SWB]
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
    sensitivity = runif(n, 0.5, 1),
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
    sensitivity = runif(1, 0.5, 1),
    adaptation_rate = runif(1, 0.4, 0.6),
    SWB_set_point = runif(1, 0, 1)
  )
  
  person$SWB <- person$SWB_set_point + initial_reaction(life_event, person$sensitivity)
  
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
    annotate("text", x = 0, y = person$SWB_set_point, label = "Personal Set Point", hjust = 0, vjust = 1.5, color = "blue")
}

test_adaptation(1000, 5, -0.2)
plot_single_person(5, -0.2)
