library(shiny)
library(shinyBS)
library(shinyjs)
library(shinyjqui)
library(shinythemes)
library(shinyWidgets)
library(rhandsontable)
library(stringr)
library(leaflet)
library(yaml)
library(qs)

library(epidemics)
library(ggplot2)
library(dplyr)
library(socialmixr) # Ensure this package is installed for `socialmixr::polymod`


# Susceptibility and clinical fraction
covuy = qread("./data/covuy.qs")
covu = rep(unname(unlist(covuy[, colMeans(.SD), .SDcols = u_00:u_70])), each = 2)
covy = rep(unname(unlist(covuy[, colMeans(.SD), .SDcols = y_00:y_70])), each = 2)

iv_def = list(
    list(name = "School closures",
         strength_default = 100,
         strength_name = "Schools closed (%)",
         contact = function(x) c(1, 1, 1-x, 1,  1, 1, 1-x, 1),
         fIs = function(x) rep(1, 16)
    ),
    list(name = "Social distancing",
         strength_default = 50,
         strength_name = "Intensity of social distancing (%)",
         contact = function(x) c(1, 1-x, 1, 1-x,  1, 1-x, 1, 1-x),
         fIs = function(x) rep(1, 16)
    ),
    list(name = "Elderly shielding (stay-at-home)",
         strength_default = 75,
         strength_name = "Proportion of elderly individuals shielded, non-home contacts (%)",
         contact = function(x) c(1, 1, 1, 1,  1, 1-x, 1-x, 1-x),
         fIs = function(x) rep(1, 16)
    ),
    list(name = "Elderly shielding (full shielding)",
         strength_default = 75,
         strength_name = "Proportion of elderly individuals shielded, all contacts (%)",
         contact = function(x) c(1, 1, 1, 1,  1-x, 1-x, 1-x, 1-x),
         fIs = function(x) rep(1, 16)
    ),
    list(name = "Self-isolation",
         strength_default = 50,
         strength_name = "Adherence to self-isolation among symptomatics (%)",
         contact = function(x) c(1, 1, 1, 1,  1, 1, 1, 1),
         fIs = function(x) rep(1 - 0.7*x, 16)
    ),
    list(name = "Lockdown",
         strength_default = 90,
         strength_name = "Intensity of lockdown (%)",
         contact = function(x) c(1, 1-x, 1-x, 1-x,  1, 1-x, 1-x, 1-x),
         fIs = function(x) rep(1, 16)
    )#,
    #list(name = "Custom",
    #    strength_default = 50,
    #    strength_name = "To do",
    #    contact = function(x) c(1, 1, 1, 1,  1, 1, 1, 1),
    #    fIs = function(x) 1
    #)
)

intervention_types = 1:length(iv_def);
names(intervention_types) = sapply(iv_def, function(x) x$name);

intervention_adders = c("add_school_closure", "add_social_distancing", "add_elderly_shielding_part", "add_elderly_shielding_full", "add_self_isolation", "add_lockdown", "add_custom")
intervention_strength_names = c("Schools closed (%)", "Intensity of social distancing (%)", "Effectiveness of elderly shielding (%)", 
                                "Adherence to self-isolation among symptomatics (%)", "Intensity of lockdown (%)", "NULL");
intervention_strength_defaults = c(100, 50, 75, 50, 90, 0);

# Helper to change CSS
setcss = function(id, ...)
{
    arguments = list(...);
    n = names(arguments);
    v = sapply(arguments, function(x) { if (is.numeric(x)) paste0(x, "px") else x });
    nv = paste0('"', n, '" : "', v, '"', collapse = ", ");
    command = paste0('$("#', id, '").css({ ', nv, ' });');
    runjs(command);
}


# UI COMPONENTS

nbsp = function(text)
{
    str_replace_all(text, " ", "&nbsp;")
}

customSlider = function(inputId, label, min, max, value, step = 1, width = NULL)
{
    HTML(paste0(
        "<div class=\"form-group shiny-input-container\"", if (is.null(width)) NULL else paste0(" style=\"", width, "\""), ">",
        "<label class=\"control-label\" for=\"", inputId, "\">", label, "</label>",
        "<input class=\"js-range-slider\" id=\"", inputId, "\" data-min=\"", min, "\" data-max=\"", max, "\" data-from=\"", value, "\" data-step=\"", step, "\"",
        " data-grid=\"false\" data-prettify-separator=\",\" data-prettify-enabled=\"true\" data-keyboard=\"true\" data-data-type=\"number\"/></div>"))
}

customAgeSlider = function(inputId, label, value1, value2, step = 1)
{
    HTML(paste0(
        "<div class=\"form-group shiny-input-container\">",
        "<label class=\"control-label\" for=\"", inputId, "\">", label, "</label>",
        "<input class=\"js-range-slider\" id=\"", inputId, "\" data-type=\"double\" data-min=\"0\" data-max=\"80\" data-from=\"", value1, "\" data-to=\"", value2, "\" data-step=\"5\"",
        " data-grid=\"false\" data-prettify-separator=\",\" data-prettify-enabled=\"true\" data-keyboard=\"true\" data-data-type=\"number\" data-max-postfix=\"+\" data-min-interval=\"5\"/></div>"))
}

iconTab = function(id, title, icon)
{
    HTML(paste0(
        "<span id=\"", id, "\"><i class=\"fa fa-", icon, " fa-3x fa-fw\"></i>",
        "<br />", title, "</span>"))
}

interventionRect = function(n, display = "block")
{
    colours   = c("#162d6040", "#3771c840", "#37c8ab40", "#ffd42a40", "#ff7f2a40", "#ff006640", "#bc5fd340", "#9d93ac40");
    tags$div(
        dropdownButton(inputId = paste0("int_menu_", n),
            tags$label(id = paste0("int_timespan_", n), "Date range"),
            selectInput(inputId = paste0("int_type_", n), label = NULL, choices = intervention_types, selected = 2),
            customSlider(inputId = paste0("int_strength_", n), label = 'Strength', value = 50, min = 1, max = 100),
            bsButton(inputId = paste0("int_remove_", n), label = "Remove", icon = icon("trash"), style = "warning", size = "small"),
            circle = TRUE, icon = icon("gear"), width = "200px", size = "xs", status = "int"
        ),
        div(id = paste0("iv_title_", n), style = "position: absolute; left: 22px; top: 4px", class = "noselect"),
        id = paste0("iv_", n), 
        style = paste0("font-size: 8pt; background-color: ", colours[n], "; height: ", 430 - n * 20, "px; width: 100px; position: absolute; left: 0px; bottom: 0px; margin: 0px; display: ", display),
        class = "noselect interv_rect"
    )
}

vaccineRect = function(n, display = "block")
{
    colours   = c("#dddddd40", "#dddddd40", "#dddddd40");
    outlines  = c("#44aa7780", "#4477aa80", "#4444aa80");
    tags$div(
        dropdownButton(inputId = paste0("vax_menu_", n),
            tags$label(id = paste0("vax_timespan_", n), "Date range"),
            customAgeSlider(paste0("vax_ages_", n), "Ages to vaccinate", value1 = 0, value2 = 80, step = 5),
            numericInput(paste0("vax_n_", n), "Vaccines administered", value = 1000000, min = 0, step = 5000),
            radioButtons(paste0("vax_rate_", n), NULL, c("total", "per day"), selected = "total", inline = TRUE),
            bsButton(inputId = paste0("vax_remove_", n), label = "Remove", icon = icon("trash"), style = "warning", size = "small"),
            circle = TRUE, icon = icon("gear"), width = "200px", size = "xs", status = "int"
        ),
        div(id = paste0("vv_title_", n), style = "position: absolute; left: 22px; top: 4px", class = "noselect"),
        id = paste0("vv_", n), 
        style = paste0("font-size: 8pt; background-color: ", colours[n], "; border-color: ", outlines[n], "; border-style: dashed; border-width: 1px 1px 0px 1px; height: ", 430, "px; width: 100px; position: absolute; left: 0px; bottom: 0px; margin: 0px; display: ", display),
        class = "noselect interv_rect"
    )
}

# for communicating position of intervention rectangles to server code
shsize = list(
    mv = list(
        `dragstop` = JS('function(event, ui) { return ui.position.left; }')
    ),
    sz = list(
        `resizestop` = JS('function(event, ui) { return { x : ui.position.left, w : ui.size.width }; }')
    )
)

shdrop = list(
    notify = list(
        `drop` = JS("function(event, ui) { return { x : ui.position.left, y : ui.position.top, id : $(ui.draggable).attr('id') }; }")
    )
)

# IP geolocation
ip_file = system.file("extdata", "ip2_sample.bin", package = "rgeolocate");

# Help
help_bubbles = list(
    # Display panels
    tab_cases         = c("Cases", "bottom", "The incidence of individuals developing clinical symptoms each day."),
    tab_hospital      = c("Hospital", "bottom", "The number of ICU and non-ICU (i.e. general ward) beds required for treating COVID-19 patients over time."),
    tab_deaths        = c("Deaths", "bottom", "The incidence of deaths from COVID-19 each day."),
    tab_transmission  = c("Transmission", "bottom", "The basic reproductive number R\u2080 and effective reproductive number R\u2091 over time."),
    tab_dynamics      = c("Dynamics", "bottom", "The number of individuals in each model compartment over time."),
    compare           = c("Compare", "bottom", "Show the dynamics of an unmitigated epidemic (i.e., with no interventions) for comparison."),
    
    # Control panels
    reset             = c("Reset tab", "bottom", "Reset the current tab to its default settings."),
    
    #tab_location      = c("Location", "bottom", "The basic reproduction number is xxx yyy zzz..."),
    loc_reg_column    = c("Country", "top", "Use demographic data from WorldPop.org and GADM.org to set the population size and age distribution for the modelled population."),
    loc_pyramid       = c("Population pyramid", "top", "Dynamics of SARS-CoV-2 transmission vary strongly by age, and accordingly the age distribution of the population can have a substantial impact upon model outputs."),

    #tab_contact       = c("Contact", "bottom", "The basic reproduction number is xxx yyy zzz..."),
    con_matrix        = c("Contact matrix", "top", "Use either the default contact matrix"),
    con_plots         = c("Contact plots", "top", "These plots show the number of contacts each individual makes per day, on average, with other individuals of specific age groups, broken down by where the contacts occur. Brighter colours correspond to more contacts made."),

    #tab_epidemic      = c("Epidemic", "bottom", "The basic reproduction number is xxx yyy zzz..."),
    epi_seed_date     = c("Epidemic start date", "top", "The day upon which the first individuals are infected with SARS-CoV-2."),
    epi_sim_time      = c("Simulate for", "top", "Number of years to simulate the epidemic for."),
    epi_seed_size     = c("Starting number of infections", "top", "Number of individuals infected with SARS-CoV-2 on the first day of the epidemic."),
    epi_R0            = c("Basic reproduction number, R\u2080", "top", "The basic reproduction number is the average number of people an infectious person transmits infection to in a fully susceptible population."),
    epi_immune        = c("Proportion immune at start", "top", "The proportion of the population that is immune to infection. People with immunity don't get sick, but they also don't transmit the virus to others. If the proportion immune exceeds the herd immunity threshold, the epidemic will not spread at all."),
    epi_rho           = c("Case ascertainment rate", "top", "The proportion of clinical cases that are captured by surveillance. The number of cases reported typically underestimates the true number of cases, because individuals may not seek care, not be counted among the sick, or not be diagnosed correctly."),
    epi_season_column = c("Seasonality", "top", "Many infectious diseases exhibit patterns of seasonal transmission. Respiratory viruses tend to transmit more efficiently in the winter and less efficiently in the summer. These controls allow you to specify the week of the year in which transmission peaks, and the amplitude (strength) of seasonality. An amplitude of 0 means no seasonal variation in transmission."),

    #tab_virus         = c("Virus", "bottom", "The basic reproduction number is xxx yyy zzz..."),
    vir_uy_column     = c("Susceptibility and clinical fraction", "top", "Susceptibility to infection is the relative propensity an individual of a given age has for getting infected given contact with an infectious person. The clinical fraction is the proportion of infections resulting in clinical symptoms, depending on an individual's age."),
    vir_d_column      = c("Durations", "top", "The latent period is the average time between infection and the onset of infectiousness; the preclinical period is the average amount of time between the onset of infectiousness and the onset of symptoms; the clinical period is the average amount of time that a symptomatic individual remains infectious; and the subclinical period is the average amount of time that a subclinically-infected person is infectious."),
    imm_wn            = c("Natural immunity waning rate", "top", "The average rate, per year, at which natural immunity to SARS-CoV-2 wanes."),
    model_diagram     = c("Model diagram", "top", "Diagram showing transitions between states of infection in the model."),

    #tab_interventions = c("Interventions", "bottom", "The basic reproduction number is xxx yyy zzz..."),
    add_school_closure         = c("School closure", "top", "Reduce school contacts by a specified amount."),
    add_social_distancing      = c("Social distancing", "top", "Reduce work and other contacts by a specified amount."),
    add_elderly_shielding_part = c("Elderly shielding (stay-at-home)", "top", "Reduce non-home contacts by a specified amount, only for the elderly."),
    add_elderly_shielding_full = c("Elderly shielding (full shielding)", "top", "Reduce all contacts by a specified amount, only for the elderly."),
    add_self_isolation         = c("Self-isolation", "top", "Reduce infectiousness of individuals with clinical symptoms by a specified amount."),
    add_lockdown               = c("Lockdown", "top", "Reduce non-home contacts by a specified amount, for all individuals."),
    add_vaccine                = c("Vaccine", "top", "Add a vaccination campaign."),
    int_elderly       = c("Age threshold for elderly shielding", "top", "Individuals this age and above are considered elderly for the purposes of elderly shielding."),
    imm_ev            = c("Vaccine effectiveness", "top", "The proportion of individuals receiving a vaccine who develop effective protection against infection."),
    imm_wv            = c("Vaccine protection waning rate", "top", "The average rate, per year, at which vaccine protection against SARS-CoV-2 wanes."),

    #tab_health        = c("Health", "bottom", "The basic reproduction number is xxx yyy zzz..."),
    hea_burdens       = c("Health burdens", "top", "The hospitalisation probability is the probability that an individual with clinical symptoms requires hospitalisation. The ICU probability is the probability that an individual who requires hospitalisation requires ICU treatment. The case fatality ratio (CFR) is the probability that an individual with clinical symptoms dies from COVID-19."),
    hea_durations     = c("Durations", "top", "The basic reproduction number is xxx yyy zzz..."),
    hea_capacity      = c("Bed capacity", "top", "Set these capacity figures to compare general ward and ICU bed requirements to available spaces."),

    #tab_data          = c("Data", "bottom", "The basic reproduction number is xxx yyy zzz..."),
    dat_points        = c("Data", "top", "Enter daily cases and deaths here to compare model output to observed data."),
    dat_show_column   = c("Show data on main display", "top", "Show cases and deaths data on the main plots above. Note that reported cases are often a substantial underestimate of the true number of cases, and it may be necessary to adjust the case ascertainment rate (Epidemic tab) to align model output with reported case data."),
    dat_import_column = c("Import", "top", "Import data from the European Centre for Disease Prevention and Control (ECDC) on the daily number of new COVID-19 cases and deaths for a given country."),

    #tab_saveload      = c("Save / Load", "bottom", "The basic reproduction number is xxx yyy zzz..."),
    sav_download      = c("Download epidemic data", "top", "Download a CSV file giving the dynamics of the epidemic over time.")

    #tab_about         = c("About", "bottom", "The basic reproduction number is xxx yyy zzz...")
)
