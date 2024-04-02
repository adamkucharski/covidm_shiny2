# BASIC SHINY APP SERVER

server <- function(input, output,session) {
    
    # ---------- SET UP ----------

    # Resizeable interventions
    jqui_resizable(jqui_draggable(ui = "#vv_1", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 430, minHeight = 430, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#vv_2", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 430, minHeight = 430, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#vv_3", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 430, minHeight = 430, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    
    # Set intervention rectangles as horizontally draggable and resizable
    jqui_resizable(jqui_draggable(ui = "#iv_1", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 410, minHeight = 410, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_2", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 390, minHeight = 390, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_3", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 370, minHeight = 370, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_4", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 350, minHeight = 350, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_5", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 330, minHeight = 330, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_6", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 310, minHeight = 310, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_7", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 290, minHeight = 290, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    jqui_resizable(jqui_draggable(ui = "#iv_8", options = list(axis = "x", containment = "#iv_bounds")), 
                   options = list(maxHeight = 270, minHeight = 270, handles = "w, e", shiny = shsize, containment = "#iv_bounds"))
    
    # New interventions
    jqui_draggable(ui = "#add_school_closure",         options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_social_distancing",      options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_elderly_shielding_part", options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_elderly_shielding_full", options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_self_isolation",         options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_lockdown",               options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_custom",                 options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_draggable(ui = "#add_vaccine",                options = list(revert = TRUE, helper = "clone", opacity = 0.75, revertDuration = 0));
    jqui_droppable(ui = "#display", options = list(accept = "[id^=add]", shiny = shdrop));
    
    # ---------- INTERVENTIONS ----------
    
    # Set up interventions
    iv = reactiveValues(
        active   = rep(F, 8),
        x        = rep(0, 8), # left pixel position of intervention rectangle
        w        = rep(0, 8), # pixel width of intervention rectangle
        t0       = rep(0, 8), # absolute start time of intervention
        t1       = rep(1, 8)  # absolute end time of intervention
    );
    vv = reactiveValues(
        active   = rep(F, 3),
        x        = rep(0, 3), # left pixel position of intervention rectangle
        w        = rep(0, 3), # pixel width of intervention rectangle
        t0       = rep(0, 3), # absolute start time of intervention
        t1       = rep(1, 3)  # absolute end time of intervention
    );
    
    # Set t0 and t1 of interventions from x and w, updating label for intervention i
    update_iv_t = function(i) { 
        # Calculate range of times shown on X axis
        params = parameters();
        xaxis_breaks = breaks_pretty(5)(c(ymd(params$date0), ymd(params$date0) + params$time1));
        ax_t0 = as.numeric(xaxis_breaks[1]);
        ax_t1 = as.numeric(tail(xaxis_breaks, 1));
        
        # Set t0 and t1
        iv$t0 = (iv$x / 810) * (ax_t1 - ax_t0) + ax_t0;
        iv$t1 = ((iv$x + iv$w) / 810) * (ax_t1 - ax_t0) + ax_t0;
        vv$t0 = (vv$x / 810) * (ax_t1 - ax_t0) + ax_t0;
        vv$t1 = ((vv$x + vv$w) / 810) * (ax_t1 - ax_t0) + ax_t0;
        
        # Set timespan text
        d = function(t) format(as.Date(round(t), origin = "1970-01-01"), "%b %d");
        if (i > 0) {
            html(paste0("int_timespan_", i), paste0(d(iv$t0[i]), " &ndash; ", d(iv$t1[i])));
        } else {
            html(paste0("vax_timespan_", -i), paste0(d(vv$t0[-i]), " &ndash; ", d(vv$t1[-i])));
        }
    }
    
    # Set x and w of interventions from t0 and t1
    update_iv_x = function() {
        # Calculate range of times shown on X axis
        params = parameters();
        xaxis_breaks = breaks_pretty(5)(c(ymd(params$date0), ymd(params$date0) + params$time1));
        ax_t0 = as.numeric(xaxis_breaks[1]);
        ax_t1 = as.numeric(tail(xaxis_breaks, 1));
        
        # Ensure times are in proper range
        iv$t0 = pmax(iv$t0, min(as.numeric(ymd(params$date0)), ax_t0));
        iv$t1 = pmin(iv$t1, max(as.numeric(ymd(params$date0) + params$time1), ax_t1));
        iv$t1 = pmax(iv$t1, iv$t0 + 7);
        
        vv$t0 = pmax(vv$t0, min(as.numeric(ymd(params$date0)), ax_t0));
        vv$t1 = pmin(vv$t1, max(as.numeric(ymd(params$date0) + params$time1), ax_t1));
        vv$t1 = pmax(vv$t1, vv$t0 + 7);
        
        # Set x and w, as well as updating actual positions
        iv$x = (iv$t0 - ax_t0) * 810 / (ax_t1 - ax_t0);
        iv$w = (iv$t1 - iv$t0) * 810 / (ax_t1 - ax_t0);
        for (i in 1:8) {
            setcss(paste0("iv_", i), left = iv$x[i], width = iv$w[i]);
        }
        
        vv$x = (vv$t0 - ax_t0) * 810 / (ax_t1 - ax_t0);
        vv$w = (vv$t1 - vv$t0) * 810 / (ax_t1 - ax_t0);
        for (i in 1:3) {
            setcss(paste0("vv_", i), left = vv$x[i], width = vv$w[i]);
        }
    }
    
    # Observe changing type of interventions
    observeEvent(input$int_type_1, { i = as.numeric(input$int_type_1); updateSliderInput(session, inputId = "int_strength_1", label = iv_def[[i]]$strength_name); html("iv_title_1", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_2, { i = as.numeric(input$int_type_2); updateSliderInput(session, inputId = "int_strength_2", label = iv_def[[i]]$strength_name); html("iv_title_2", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_3, { i = as.numeric(input$int_type_3); updateSliderInput(session, inputId = "int_strength_3", label = iv_def[[i]]$strength_name); html("iv_title_3", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_4, { i = as.numeric(input$int_type_4); updateSliderInput(session, inputId = "int_strength_4", label = iv_def[[i]]$strength_name); html("iv_title_4", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_5, { i = as.numeric(input$int_type_5); updateSliderInput(session, inputId = "int_strength_5", label = iv_def[[i]]$strength_name); html("iv_title_5", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_6, { i = as.numeric(input$int_type_6); updateSliderInput(session, inputId = "int_strength_6", label = iv_def[[i]]$strength_name); html("iv_title_6", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_7, { i = as.numeric(input$int_type_7); updateSliderInput(session, inputId = "int_strength_7", label = iv_def[[i]]$strength_name); html("iv_title_7", nbsp(iv_def[[i]]$name)); });
    observeEvent(input$int_type_8, { i = as.numeric(input$int_type_8); updateSliderInput(session, inputId = "int_strength_8", label = iv_def[[i]]$strength_name); html("iv_title_8", nbsp(iv_def[[i]]$name)); });
    
    observeEvent(input$vax_rate_1, { n = if (input$vax_rate_1 == "total") input$vax_n_1 * (vv$t1[1] - vv$t0[1]) else input$vax_n_1 / (vv$t1[1] - vv$t0[1]); updateNumericInput(session, inputId = "vax_n_1", value = n); });
    observeEvent(input$vax_rate_2, { n = if (input$vax_rate_2 == "total") input$vax_n_2 * (vv$t1[2] - vv$t0[2]) else input$vax_n_2 / (vv$t1[2] - vv$t0[2]); updateNumericInput(session, inputId = "vax_n_2", value = n); });
    observeEvent(input$vax_rate_3, { n = if (input$vax_rate_3 == "total") input$vax_n_3 * (vv$t1[3] - vv$t0[3]) else input$vax_n_3 / (vv$t1[3] - vv$t0[3]); updateNumericInput(session, inputId = "vax_n_3", value = n); });
    
    # Observe dragging and resizing of interventions
    observeEvent(input$iv_1_mv, { iv$x[1] = input$iv_1_mv; update_iv_t(1); });
    observeEvent(input$iv_2_mv, { iv$x[2] = input$iv_2_mv; update_iv_t(2); });
    observeEvent(input$iv_3_mv, { iv$x[3] = input$iv_3_mv; update_iv_t(3); });
    observeEvent(input$iv_4_mv, { iv$x[4] = input$iv_4_mv; update_iv_t(4); });
    observeEvent(input$iv_5_mv, { iv$x[5] = input$iv_5_mv; update_iv_t(5); });
    observeEvent(input$iv_6_mv, { iv$x[6] = input$iv_6_mv; update_iv_t(6); });
    observeEvent(input$iv_7_mv, { iv$x[7] = input$iv_7_mv; update_iv_t(7); });
    observeEvent(input$iv_8_mv, { iv$x[8] = input$iv_8_mv; update_iv_t(8); });
    
    observeEvent(input$iv_1_sz, { iv$x[1] = input$iv_1_sz$x; iv$w[1] = input$iv_1_sz$w; update_iv_t(1); });
    observeEvent(input$iv_2_sz, { iv$x[2] = input$iv_2_sz$x; iv$w[2] = input$iv_2_sz$w; update_iv_t(2); });
    observeEvent(input$iv_3_sz, { iv$x[3] = input$iv_3_sz$x; iv$w[3] = input$iv_3_sz$w; update_iv_t(3); });
    observeEvent(input$iv_4_sz, { iv$x[4] = input$iv_4_sz$x; iv$w[4] = input$iv_4_sz$w; update_iv_t(4); });
    observeEvent(input$iv_5_sz, { iv$x[5] = input$iv_5_sz$x; iv$w[5] = input$iv_5_sz$w; update_iv_t(5); });
    observeEvent(input$iv_6_sz, { iv$x[6] = input$iv_6_sz$x; iv$w[6] = input$iv_6_sz$w; update_iv_t(6); });
    observeEvent(input$iv_7_sz, { iv$x[7] = input$iv_7_sz$x; iv$w[7] = input$iv_7_sz$w; update_iv_t(7); });
    observeEvent(input$iv_8_sz, { iv$x[8] = input$iv_8_sz$x; iv$w[8] = input$iv_8_sz$w; update_iv_t(8); });
    
    observeEvent(input$vv_1_mv, { vv$x[1] = input$vv_1_mv; update_iv_t(-1); });
    observeEvent(input$vv_2_mv, { vv$x[2] = input$vv_2_mv; update_iv_t(-2); });
    observeEvent(input$vv_3_mv, { vv$x[3] = input$vv_3_mv; update_iv_t(-3); });
    
    observeEvent(input$vv_1_sz, { vv$x[1] = input$vv_1_sz$x; vv$w[1] = input$vv_1_sz$w; update_iv_t(-1); });
    observeEvent(input$vv_2_sz, { vv$x[2] = input$vv_2_sz$x; vv$w[2] = input$vv_2_sz$w; update_iv_t(-2); });
    observeEvent(input$vv_3_sz, { vv$x[3] = input$vv_3_sz$x; vv$w[3] = input$vv_3_sz$w; update_iv_t(-3); });
    
    
    #observeEvent(input$run_model, {
    observe({
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
        
        # Define initial conditions
        initial_i <- (input$epi_seed_size/3)/demography_vector
        initial_r <- input$epi_immune
        
        initial_conditions <- rbind(
            c(S = 1 - initial_r - initial_i[1], E = 0, I = initial_i[1], R = initial_r, V = 0),
            c(S = 1 - initial_r - initial_i[2], E = 0, I = initial_i[2], R = initial_r, V = 0),
            c(S = 1 - initial_r - initial_i[3], E = 0, I = initial_i[3], R = initial_r, V = 0)
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
        
        # Calculate transmission rate
        # Recovery rate
        recovery_rate_in <- 1/7
        R0_in <- input$epi_R0
        transmission_rate_in <- R0_in*recovery_rate_in
        
        output_data <- model_default(
            population = uk_population,
            transmission_rate = transmission_rate_in,
            infectiousness_rate = 1.0 / 3.0,
            recovery_rate = recovery_rate_in,
            intervention = list(contacts = c(close_schools,close_workplaces)),
            time_end = as.numeric(input$epi_sim_time), increment = 1.0
        )
        
        data_infections <- new_infections(output_data, by_group = TRUE)

        output$cases_plot <- renderPlot({
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
    
    
    # ---------- DISPLAY PANEL ----------
    

}
