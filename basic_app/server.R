server <- function(input, output) {
    observeEvent(input$run_model, {
        # Load contact and population data
        polymod <- socialmixr::polymod
        contact_data <- socialmixr::contact_matrix(
            polymod,
            countries = "United Kingdom",
            age.limits = c(0, 20, 40),
            symmetric = TRUE
        )
        
        contact_matrix <- t(contact_data[["matrix"]])
        demography_vector <- contact_data[["demography"]][["population"]]
        names(demography_vector) <- rownames(contact_matrix)
        
        initial_i <- 1e-6
        initial_conditions <- rbind(
            c(S = 1 - initial_i, E = 0, I = initial_i, R = 0, V = 0),
            c(S = 1 - initial_i, E = 0, I = initial_i, R = 0, V = 0),
            c(S = 1 - initial_i, E = 0, I = initial_i, R = 0, V = 0)
        )
        rownames(initial_conditions) <- rownames(contact_matrix)
        
        uk_population <- population(
            name = "UK",
            contact_matrix = contact_matrix,
            demography_vector = demography_vector,
            initial_conditions = initial_conditions
        )
        
        close_schools <- intervention(
            type = "contacts",
            time_begin = 200,
            time_end = 260,
            reduction = matrix(c(0.5, 0.01, 0.01))
        )
        
        intervention_duration <- 60
        close_workplaces <- intervention(
            name = "Workplace closure",
            type = "contacts",
            time_begin = 80,
            time_end = 80 + intervention_duration,
            reduction = matrix(c(0.01, 0.3, 0.01))
        )
        
        output_data <- model_default(
            population = uk_population,
            transmissibility = 1.5 / 7.0,
            infectiousness_rate = 1.0 / 3.0,
            recovery_rate = 1.0 / 7.0,
            intervention = list(contacts = c(close_schools,close_workplaces)),
            time_end = 600, increment = 1.0
        )
        
        data_infections <- new_infections(output_data, by_group = TRUE)
        
        output$epidemic_curve <- renderPlot({
            plot_intervention_cases <-
                ggplot() +
                geom_vline(
                    xintercept = c(
                        close_schools$time_begin,
                        close_schools$time_end
                    ),
                    linetype = "dotted"
                ) +
                geom_vline(
                    xintercept = c(
                        close_workplaces$time_begin,
                        close_workplaces$time_end
                    ),
                    colour = "red",
                    linetype = "dotted"
                ) +
                annotate(
                    geom = "text",
                    x = mean(c(close_schools$time_begin, close_schools$time_end)),
                    y = 50000,
                    label = "Schools closed"
                ) +
                annotate(
                    geom = "text",
                    x = mean(c(
                        close_workplaces$time_begin,
                        close_workplaces$time_end
                    )),
                    y = 30000,
                    colour = "red",
                    label = "Workplaces\nclosed"
                ) +
                geom_line(
                    data = data_infections,
                    aes(time, new_infections, colour = demography_group),
                    linetype = "solid"
                ) +
                scale_y_sqrt(
                    labels = scales::comma,
                    breaks = c(10^seq(3, 5), 5e4)
                ) +
                scale_colour_brewer(
                    palette = "Dark2",
                    name = "Age group"
                ) +
                coord_cartesian(
                    expand = FALSE
                ) +
                theme_bw() +
                theme(
                    legend.position = "top"
                ) +
                labs(
                    x = "Simulation time (days)",
                    linetype = "Compartment",
                    y = "Individuals"
                )
            
            plot_intervention_cases
        })
    })
}
