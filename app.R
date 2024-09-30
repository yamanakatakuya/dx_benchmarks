# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB diagnostic benchmarks for selected countries.
# Takuya Yamanaka, September 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 1.0_alpha"

## load packages
library("shiny")
library("glue")
library("rlang")
library(shinydashboard)
library(formattable)
library(htmlwidgets)
library(shinythemes)
library(tidyverse)

# selected countries
select <- c("AO",	"AR",	"AZ",	"BD",	"BY",	"BZ",	"BO",	"BR",	"KH",	"CF",	"CL",	"CN",	"CO",	"CG",	"CR",	"CU",	"KP",	"CD",	"DO",	"EC",	"SV",	"ET",	"GA",	"GT",	"GY",	"HT",	"HN",	"ID",	"JM",	"KE",	"KG",	"LA",	"LS",	"LR",	"MX",	"MN",	"MZ",	"MM",	"NA",	"NP",	"NI",	"NG",	"PK",	"PA",	"PG",	"PY",	"PE",	"PH",	"MD",	"ZA",	"SR",	"TJ",	"TH",	"UG",	"UA",	"TZ",	"UY",	"UZ",	"VE",	"VN",	"ZM",	"ZW")

## load TPT and strategy datasets
str <- read.csv("latest_strategy_2024-08-26.csv") %>%
  select(country:g_whoregion,plhiv_all_screen_data_available:m_wrd_tat_lt_48h,m_wrd)

tpt <- read.csv("latest_contacts_tpt_2024-08-26.csv") %>%
  select(iso2,newinc_con:newinc_con_screen)

notif <- read.csv("latest_notifications_2024-09-30.csv") %>%
  select(iso2,new_labconf:tbdeaths_vr)


df <- str %>%
  left_join(tpt, by = "iso2") %>%
  left_join(notif, by = "iso2") %>%
  filter(!is.na(plhiv_all_screen_data_available)) %>%
  mutate(iso2 = ifelse(country == "Namibia", "NA", iso2))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- 
  navbarPage(
    "TB Diagnostic Benchmarks",
    tabPanel(
      "Country benckmarks",
      fluidPage(theme = shinytheme("sandstone"),
      # --------------------- each country ---------------------#
      fluidPage(
        
        fluidRow(tags$div(id = "page_header",
                          HTML("Select a country"),
                          uiOutput(outputId = "country"))
        ),
        
        
        fluidRow(
          column(width = 12,
                 tags$div(
                   class   = "cw-container",
                   checked = NA,
                   formattableOutput("table"))
          )
        ),
        
        fluidRow(tags$div(id = "metadata",
                          style = "padding: 20px; font-style: italic; font-size: 80%;",
                          
                          # Add app version number and links to GTB and Github
                          HTML(paste0(app_version,
                                      ", Source code on <a href='https://github.com/yamanakatakuya/dx_benckmark' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
        ))
      ))
    

    
  )
  )

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Back end server code (called each time a new session is initiated)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

server <- function(input, output, session) {
  
  # output for a country selection
  output$country <- renderUI({
    
    already_selected <- input$iso2
    
    # Create a named list for selectInput
    country_list <- df %>%
      select(iso2, country) %>%
      arrange(country)
    
    country_list <- setNames(country_list[,"iso2"], country_list[,"country"])
    
    selectInput(inputId = "iso2",
                label = "",
                choices = country_list,
                # next line needed to avoid losing the selected country when the language is changed
                selected = already_selected,
                selectize = FALSE,
                width = "380px")
  })
  
  # base reactive data frame
  dfc <- reactive({
     dfc <- df %>%
       filter(iso2 == input$iso2)
     
     dfc <- dfc %>%
       mutate(district = as.numeric(district)) %>%
       mutate(score1a = newinc_con_screen/newinc_con*100,
              score1b = plhiv_all_screen/plhiv_all*100,
              score1c = plhiv_new_screen/plhiv_new*100,
              score1d = prisoners_screen/prisoners*100,
              score1e = miners/miners_screen_data_available*100,
              score2  = district_cxr/district*100,
              score3  = district_wrd/district*100,
              score4  = phcf_wrd/phcf*100,
              score5  = newinc_rdx/c_newinc*100,
              score6  = wrd_test_capacity/presumptive*100,
              score7  = m_wrd_error_rate_lte_5pct/m_wrd*100,
              score8  = presumptive_wrd/presumptive*100,
              score9a = (r_rlt_new+r_rlt_ret)/(pulm_labconf_new+pulm_labconf_ret)*100,
              score9b = rr_dst_rlt_fq/(rr_new+rr_ret)*100,
              score9c = (rr_fqr_bdqr_lzdr+rr_fqr_bdqs_lzdr+rr_fqr_bdqr_lzds+rr_fqr_bdqs_lzds+rr_fqr_bdqr_lzdu+rr_fqr_bdqs_lzdu)/rr_fqr*100,
              score9d = (rr_fqr_bdqr_lzdr+rr_fqr_bdqs_lzdr+rr_fqr_bdqu_lzdr+rr_fqr_bdqr_lzds+rr_fqr_bdqs_lzds+rr_fqr_bdqu_lzds)/rr_fqr*100,
              score10 = (newinc_pulm_labconf_rdx+newinc_pulm_clindx_rdx)/(new_labconf+new_clindx+new_ep+ret_rel_labconf)*100,
              score11 = district_monitor_pos_rate/district*100,
              score12 = m_wrd_tat_lt_48h/m_wrd*100) %>%
       mutate(numer1a = newinc_con_screen,
              numer1b = plhiv_all_screen,
              numer1c = plhiv_new_screen,
              numer1d = prisoners_screen,
              numer1e = miners,
              numer2  = district_cxr,
              numer3  = district_wrd,
              numer4  = phcf_wrd,
              numer5  = newinc_rdx,
              numer6  = wrd_test_capacity,
              numer7  = m_wrd_error_rate_lte_5pct,
              numer8  = presumptive_wrd,
              numer9a = (r_rlt_new+r_rlt_ret),
              numer9b = rr_dst_rlt_fq,
              numer9c = (rr_fqr_bdqr_lzdr+rr_fqr_bdqs_lzdr+rr_fqr_bdqr_lzds+rr_fqr_bdqs_lzds+rr_fqr_bdqr_lzdu+rr_fqr_bdqs_lzdu),
              numer9d = (rr_fqr_bdqr_lzdr+rr_fqr_bdqs_lzdr+rr_fqr_bdqu_lzdr+rr_fqr_bdqr_lzds+rr_fqr_bdqs_lzds+rr_fqr_bdqu_lzds),
              numer10 = (newinc_pulm_labconf_rdx+newinc_pulm_clindx_rdx),
              numer11 = district_monitor_pos_rate,
              numer12 = m_wrd_tat_lt_48h) %>%
       mutate(denom1a = newinc_con,
              denom1b = plhiv_all,
              denom1c = plhiv_new,
              denom1d = prisoners,
              denom1e = miners_screen_data_available,
              denom2  = district,
              denom3  = district,
              denom4  = phcf,
              denom5  = c_newinc,
              denom6  = presumptive,
              denom7  = m_wrd,
              denom8  = presumptive,
              denom9a = (pulm_labconf_new+pulm_labconf_ret),
              denom9b = (rr_new+rr_ret),
              denom9c = rr_fqr,
              denom9d = rr_fqr,
              denom10 = (new_labconf+new_clindx+new_ep+ret_rel_labconf),
              denom11 = district,
              denom12 = m_wrd) %>%
       select(country:g_whoregion,score1a:denom12)

     category_values <- c("1A","1B","1C","1D","1E",
                               "2","3","4","5","6","7","8",
                               "9A","9B","9C","9D",
                               "10","11","12")

     dfc <- dfc %>% 
       pivot_longer(!country:g_whoregion,
                    names_to = "variable",
                    values_to = "value") %>%
       mutate(Benchmark = rep(category_values, times = 3)) %>% 
       mutate(category = ifelse(grepl("^score", variable), "Score", 
                               ifelse(grepl("^numer", variable), "Numerator", "Denominator"))) %>% 
       select(!variable) %>% 
       pivot_wider(names_from = category, values_from = value) 
     
     explanation <- c("Percentage of household contacts who were evaluated for TB disease and TB infection",
                      "Percentage of people living with HIV who were screened for TB",
                      "Percentage of people newly diagnosed with HIV who were screened for TB",
                      "Percentage of people in prison are screened for TB",
                      "Percentage of miners exposed to silica dust who were screened for TB",
                      "Percentage of districts in which chest X-ray is used regularly for TB screening",
                      "Percentage of districts in which all facilities have a TB diagnostic algorithm requiring use of a WHO-recommended rapid diagnostic test as the initial diagnostic test for all patients with presumptive TB",
                      "Percentage of primary health-care facilities with access to WHO-recommended rapid diagnostic tests",
                      "Percentage of new and relapse cases tested using a WHO-recommended rapid diagnostic test as the initial diagnostic test",
                      "Percentage of tests required to test all patients with presumptive TB that can be performed with existing instruments",
                      "Percentage of sites providing molecular WHO-recommended rapid diagnostics testing for TB with annual error rates of 5% or less",
                      "Percentage of people with presumptive TB tested with a WHO-recommended rapid diagnostic test",
                      "Percentage of pleplw diagnosed with new and previously treated bacteriologically confirmed pulmonary TB with test results for rifampicin",
                      "Percentage of people with resistance to rifampicin and with test results for susceptibility to fluoroquinolones",
                      "Percentage of people with resistance to rifampicin and resistance to fluoroquinolones and with susceptibility test results for bedaquiline",
                      "Percentage of patients with resistance to rifampicin and resistance to fluoroquinolones and with susceptibility test results for linezolid",
                      "Percentage of new and relapse pulmonary bacteriologically confirmed and clinically diagnosed cases tested with a WHO-recommended rapid diagnostic test",
                      "Percentage of districts that monitored test positivity rate",
                      "Percentage of laboratories that achieved a turn-around time within 48 hours for at least 80% of samples received for WHO-recommended rapid diagnostic testing")
     
     dfc$Explanation <- explanation
     
     dfc <- dfc %>% 
       mutate(Step = c(rep(1, times = 6), rep(2, times = 4), rep(3, times = 6), rep(4, times = 3))) %>%
       select(country:g_whoregion,Step,Benchmark,Explanation,Numerator,Denominator,Score)
     

  })
  
  output$table <- renderFormattable({
    
    dfc <- dfc()

    df_table <- dfc %>%
      select(!country:g_whoregion)
    
    # Custom color_bar function with no bar for NA and 0, and complete bar for 100
    custom_color_bar <- function(color) {
      formatter("span",
                style = function(x) {
                  ifelse(is.na(x) | x == 0, 
                         "background-color: transparent;",  # No bar for NA and 0
                         paste("display: inline-block; direction: rtl; width:", 
                               percent(x / 100), "; background-color:", color, ";"))
                })
    }
    
    custom_score_format <- formatter("span",
                                     style = function(x) {
                                       ifelse(is.na(x) | x == 0, 
                                              "background-color: transparent;",  # No bar for NA and 0
                                              paste("display: inline-block; direction: rtl; width:", 
                                                    percent(x / 100), "; background-color: lightblue;"))
                                     },
                                     x ~ ifelse(x < 10 & !is.na(x), 
                                                sprintf("%.1f", x),  # Show 1 decimal for values less than 10
                                                sprintf("%.0f", x))  # Show 0 decimals for values 10 or more
    )
    
    custom_color_bar_by_step <- function(Step, score) {
      color <- ifelse(Step == 1, "#064871", 
                      ifelse(Step == 2, "#DA471F", 
                             ifelse(Step == 3, "#0096B3", "#AD176E")))
      
      formatter("span",
                style = function(x) {
                  ifelse(is.na(x) | x == 0, 
                         "background-color: transparent;",  # No bar for NA and 0
                         paste("display: inline-block; direction: rtl; width:", 
                               percent(x / 100), "; background-color:", color, "; color: black;"))
                },
                x ~ ifelse(x < 10 & !is.na(x), 
                           sprintf("%.1f", x),  # Show 1 decimal for values less than 10
                           sprintf("%.0f", x))  # Show 0 decimals for values 10 or more
      )
    }
    
    
    custom_color_bar_and_background <- function(Step, Score) {
      # Define original bar colors based on 'id' values
      bar_color <- ifelse(Step == 1, "#064871", 
                          ifelse(Step == 2, "#DA471F", 
                                 ifelse(Step == 3, "#0096B3", 
                                        ifelse(Step == 4, "#AD176E", "transparent"))))
      
      # Define lighter background color versions based on the 'Step' values
      background_color <- ifelse(Step == 1, "#98B2C7",     # Lighter version of #064871
                                 ifelse(Step == 2, "#F5A489",   # Lighter version of #DA471F
                                        ifelse(Step == 3, "#B3DDE6", # Lighter version of #0096B3
                                               ifelse(Step == 4, "#DB97B5", "transparent"))))
      
      # Return the formatter with custom color bar and different background color
      formatter("span",
                style = function(x) {
                  ifelse(is.na(x) | x == 0, 
                         paste("background-color: transparent;"),  # No bar for NA and 0
                         paste("display: inline-block; direction: rtl; width:", 
                               percent(x / 100), "; background-color:", bar_color, "; color: black;"))
                },
                x ~ ifelse(x < 10 & !is.na(x), 
                           sprintf("%.1f", x),  # Show 1 decimal for values less than 10
                           sprintf("%.0f", x)),  # Show 0 decimals for values 10 or more
                background = function(x) {
                  paste("background-color:", background_color, ";")  # Set the lighter background color
                }
      )
    }
       
    formattable(
      df_table,
      list(
        Score = custom_color_bar_and_background(df_table$Step)
      ),
      align = c("r","r","r","r","r","l")
    )
  })
  
  
  
}  


# Run the application
shinyApp(ui = ui, server = server)

