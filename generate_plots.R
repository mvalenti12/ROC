source("fun.R")
generate_plots(n_graphs = 25,
               predicted_0 = rexp(1000,10),
               predicted_1 = 1-rexp(1000,10),
               x_coord = 0.5,
               identifier = "exponentials")

generate_plots(n_graphs = 25,
               predicted_0 = rbeta(10000,2,10),
               predicted_1 = 1-rbeta(10000,3,10),
               x_coord = 0.5,
               identifier = "beta")

generate_plots(n_graphs = 25,
               predicted_0 = rnorm(10000,0.3,0.1),
               predicted_1 = rnorm(10000,0.6,0.1),
               x_coord = 0.5,
               identifier = "normal")

generate_plots(n_graphs = 25,
               predicted_0 = rnorm(10000,0.3,0.1),
               predicted_1 = 1-rbeta(10000,3,10),
               x_coord = 0.5,
               identifier = "normal_beta")

