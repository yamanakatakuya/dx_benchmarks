# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB diagnostic benchmarks for selected countries.
# Takuya Yamanaka, October 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 1.0"

## load packages
library("shiny")
library("glue")
library("rlang")
library(shinydashboard)
library(formattable)
library(htmlwidgets)
library(shinythemes)
library(tidyverse)
library(kableExtra)
library(htmltools)

# selected countries
select <- c("AO",	"AR",	"AZ",	"BD",	"BY",	"BZ",	"BO",	"BR",	"KH",	"CF",	"CL",	"CN",	"CO",	"CG",	"CR",	"CU",	"KP",	"CD",	"DO",	"EC",	"SV",	"ET",	"GA",	"GT",	"GY",	"HT",	"HN",	"ID",	"JM",	"KE",	"KG",	"LA",	"LS",	"LR",	"MX",	"MN",	"MZ",	"MM",	"NA",	"NP",	"NI",	"NG",	"PK",	"PA",	"PG",	"PY",	"PE",	"PH",	"MD",	"ZA",	"SR",	"TJ",	"TH",	"UG",	"UA",	"TZ",	"UY",	"UZ",	"VE",	"VN",	"ZM",	"ZW")

## load nofitications, TPT and strategy dataset
### data is as of 18 October 2024
str <- read.csv("latest_strategy_2024-10-18.csv") %>%
  select(country:g_whoregion,plhiv_all_screen_data_available:m_wrd_tat_lt_48h,m_wrd)

tpt <- read.csv("latest_contacts_tpt_2024-10-18.csv") %>%
  select(iso2,newinc_con:newinc_con_screen)

notif <- read.csv("latest_notifications_2024-10-18.csv") %>%
  select(iso2,new_labconf:tbdeaths_vr)

bench <- read.csv("dx_benchmark_exp.csv")

df <- str %>%
  left_join(tpt, by = "iso2") %>%
  left_join(notif, by = "iso2") %>%
  filter(!is.na(plhiv_all_screen_data_available)) %>%
  mutate(iso2 = ifelse(country == "Namibia", "NA", iso2))

year <- 2023

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Web interface code
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ui <- 
  navbarPage(
    "TB Diagnostic Benchmarks",
    tabPanel(
      "Country benchmarks",
      fluidPage(theme = shinytheme("sandstone"),
      # --------------------- each country ---------------------#
      fluidPage(
        
        fluidRow(tags$div(id = "page_header",
                          HTML("Select a country"),
                          uiOutput(outputId = "country"))
        ),
        
        fluidRow(
          tags$div(
            style="padding: 10px 20px 10px 20px;class: ms-fontWeight-bold;",
            h4(paste0("WHO benchmarks of universal access to rapid TB diagnostics, ", year))
          )
        ),
        
        fluidRow(
          column(width = 12,
                 tags$div(
                   class   = "cw-container",
                   checked = NA,
                   htmlOutput("table"))
          )
        ),
        
        fluidRow(tags$div(style = "padding-left: 20px; padding-right: 20px;",
                          textOutput(outputId = "page_footer"))
        ),
        
        fluidRow(tags$div(id = "link",
                          style = "padding-left: 20px; padding-right: 20px;",
                          
                          # Add app version number and links to GTB and Github
                          HTML(paste0("For further details of TB diagnostic benchmarks, see
                                  <a href='https://iris.who.int/handle/10665/366854' target='_blank'>
                            WHO standard: universal access to rapid tuberculosis diagnostics</a>."))
        )),
        
        
        fluidRow(tags$div(id = "metadata",
                          style = "padding: 20px; font-style: italic; font-size: 80%;",
                          
                          # Add app version number and links to GTB and Github
                          HTML(paste0(app_version,
                                      ", Source code on <a href='https://github.com/yamanakatakuya/dx_benchmarks' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
        ))
      ))
    

    
  ),
  
  tabPanel(
    "Details of benchmarks",
  
    # --------------------- each country ---------------------#
    fluidPage(
      
  
      fluidRow(
        tags$div(
          style="padding: 10px 20px 10px 20px;class: ms-fontWeight-bold;",
          h4(paste0("Summary: Numerator and denominator used for WHO benchmarks of universal access to rapid TB diagnostics"))
        )
      ),
      
      fluidRow(
               tags$div(style = "padding-left: 20px; padding-right: 20px;",
                 class   = "cw-container",
                 checked = NA,
                 htmlOutput("table_bench"))
      ),
      
      fluidRow(tags$div(id = "link2",
                        style = "padding-left: 20px; padding-right: 20px;",
                        
                        # Add app version number and links to GTB and Github
                        HTML(paste0("For further details of TB diagnostic benchmarks, see
                                  <a href='https://iris.who.int/handle/10665/366854' target='_blank'>
                            WHO standard: universal access to rapid tuberculosis diagnostics</a>."))
      )),
      
      
      fluidRow(tags$div(id = "metadata2",
                        style = "padding: 20px; font-style: italic; font-size: 80%;",
                        
                        # Add app version number and links to GTB and Github
                        HTML(paste0(app_version,
                                    ", Source code on <a href='https://github.com/yamanakatakuya/dx_benchmarks' target='_blank'>Github</a>.
                                  Data collected and published by the
                                  <a href='https://www.who.int/teams/global-tuberculosis-programme/data' target='_blank'>
                            World Health Organization</a>.</i>"))
      ))
      
    )    
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
              score1e = miners_screen/miners*100,
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
              numer1e = miners_screen,
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
              denom1e = miners,
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
       mutate(category = ifelse(grepl("^score", variable), "Percentage", 
                               ifelse(grepl("^numer", variable), "Numerator", "Denominator"))) %>% 
       select(!variable) %>% 
       pivot_wider(names_from = category, values_from = value)
     
     explanation <- c("Percentage of household contacts who were evaluated for TB disease and TB infection",
                      "Percentage of people living with HIV who were screened for TB",
                      "Percentage of people newly diagnosed with HIV who were screened for TB",
                      "Percentage of people in prison are screened for TB",
                      "Percentage of miners exposed to silica dust who were screened for TB",
                      "Percentage of districts in which chest X-ray is used regularly for TB screening",
                      "Percentage of districts in which all facilities have a TB diagnostic algorithm requiring use of a WHO-recommended rapid diagnostic test as the initial diagnostic test for all people with presumptive TB",
                      "Percentage of primary health-care facilities with access to WHO-recommended rapid diagnostic tests",
                      "Percentage of new and relapse cases tested using a WHO-recommended rapid diagnostic test as the initial diagnostic test",
                      "Percentage of tests required to test all people with presumptive TB that can be performed with existing instruments",
                      "Percentage of sites providing molecular WHO-recommended rapid diagnostics testing for TB with annual error rates of 5% or less",
                      "Percentage of people with presumptive TB tested with a WHO-recommended rapid diagnostic test",
                      "Percentage of people diagnosed with new and previously treated bacteriologically confirmed pulmonary TB with test results for rifampicin",
                      "Percentage of people with resistance to rifampicin and with test results for susceptibility to fluoroquinolones",
                      "Percentage of people with resistance to rifampicin and resistance to fluoroquinolones and with susceptibility test results for bedaquiline",
                      "Percentage of people with resistance to rifampicin and resistance to fluoroquinolones and with susceptibility test results for linezolid",
                      "Percentage of new and relapse pulmonary bacteriologically confirmed and clinically diagnosed cases tested with a WHO-recommended rapid diagnostic test",
                      "Percentage of districts that monitored test positivity rate",
                      "Percentage of laboratories that achieved a turn-around time within 48 hours for at least 80% of samples received for WHO-recommended rapid diagnostic testing")
     
     explanation2 <- c("Increase the number of people with presumptive TB in care",
                       "","","","","",
                       "Increase access to WRDs",
                       "","","",
                       "Increase WRD and drug resistance testing",
                       "","","","","",
                       "Increase WRD-based diagnosis",
                       "","")
     
     dfc$`Explanation of steps` <- explanation2
     dfc$`Explanation of benchmarks` <- explanation
     
     dfc <- dfc %>% 
       mutate(Step = c(rep(1, times = 6), rep(2, times = 4), rep(3, times = 6), rep(4, times = 3))) %>%
       select(country:g_whoregion,Step,`Explanation of steps`,Benchmark,`Explanation of benchmarks`,Numerator,Denominator,Percentage)
     

  })
  
  output$table <- renderUI({
    
    dfc <- dfc()

    df_table <- dfc %>%
      select(!country:g_whoregion)
    
    # Color palette for each id
    color_map <- c("1" = "#064871",  # Color for id 1
                   "2" = "#DA471F",  # Color for id 2
                   "3" = "#0096B3",  # Color for id 3
                   "4" = "#AD176E")  # Color for id 4
    
    step_color <- color_map[as.character(df_table$Step)]
    
    # Function to format score based on value range
    format_score <- function(x) {
      if (is.na(x)) {
        return("—")
      } else if (x > 100) {
        return(">100*")  # showing >100
      } else if (x >= 10) {
        return(sprintf("%.0f", x))  # No decimal for scores >= 10
      } else if (x >= 1| x==0) {
        return(sprintf("%.1f", x))  # One decimal for scores >= 1 and < 10
      } else {
        return(sprintf("%.2f", x))  # Two decimals for scores < 1
      }
    }
    
    # Format Numerator and Denominator with thousand separators
    df_table <- df_table %>%
      mutate(Numerator = ifelse(is.na(Numerator), "—", format(df_table$Numerator, big.mark = " ", scientific = FALSE))) %>%
      mutate(Denominator = ifelse(is.na(Denominator), "—", format(df_table$Denominator, big.mark = " ", scientific = FALSE))) 
    
    # Add a helper column to the data frame to identify the rows for special styling
    df_table$row_index <- ifelse(row.names(df_table) %in% c(2:6, 8:10, 12:16, 18:19), TRUE, FALSE)

    
    ft <- formattable(
      df_table[, !names(df_table) %in% "row_index"],  # Exclude the RowHighlight column
      list(
        Step = formatter("span", 
                         style = function(x, row_index) {
                        
                           # Define the rows where you want white text color
                           ifelse(df_table$row_index,
                                  style(color = "white", 
                                        font_weight = "bold",  # Make the text bold
                                        display = "block", 
                                        width = "10px", 
                                        text_align = "right"),
                                  style(color = "white",   # Default color for other rows
                                        `background-color` = step_color,
                                        font_weight = "bold",  # Make the text bold
                                          display = "block", 
                                        width = "50px", 
                                        text_align = "right"))
                         }),
        `Explanation of steps` = formatter("span", style = ~ style(
          display = "block", 
          width = "200px",   # Set max width of Explanation column
          overflow = "hidden",
          white_space = "normal", # Enable word wrapping
          text_align = "left"
        )),
        Benchmark = formatter("span", style = ~ style(
          display = "block", 
          width = "10px",   # Set max width of Explanation column
          text_align = "right"
        )),
        `Explanation of benchmarks` = formatter("span", style = ~ style(
          display = "block", 
          width = "400px",   # Set max width of Explanation column
          overflow = "hidden",
          white_space = "normal", # Enable word wrapping
          text_align = "left"
        )),
        Percentage = formatter("span",
                          x ~ icontext(ifelse(is.na(x) | x == 0, "", "color_bar"), sapply(x, format_score)),
                          style = function(x, Step) {
                            # Apply color only if Score is not NA or 0
                            ifelse(is.na(x) | x == 0,
                                   style(display = "block", width = "450px", text_align = "center", background = "none", color = "black"),
                                   ifelse(x <= 4.9, 
                                          style(display = "grid", 
                                                width = paste0(x*1, "%"),   # Width proportional to the score
                                                background = color_map[as.character(df_table$Step)],  # Set color based on Step
                                                height = "30px",          # Bar height*
                                                border_radius = "4px",    # Rounded corners
                                                color = "black",          # Text color inside the bar
                                                text_align = "center",
                                                position = "relative",     # Enable absolute positioning
                                                padding_left = "125px"      # Add padding for text inside the bar
                                          ),
                                          ifelse(x > 100, 
                                                 style(display = "grid", 
                                                       width = paste0(100, "%"),   # Width proportional to the score
                                                       background = color_map[as.character(df_table$Step)],  # Set color based on Step
                                                       height = "30px",          # Bar height*
                                                       border_radius = "4px",    # Rounded corners
                                                       color = "white",          # Text color inside the bar
                                                       text_align = "center",
                                                       position = "relative",     # Enable absolute positioning
                                                       padding_left = "125px"      # Add padding for text inside the bar
                                                 ),
                                                 style(display = "block", 
                                         width = paste0(x*1, "%"),   # Width proportional to the score
                                         background = color_map[as.character(df_table$Step)],  # Set color based on Step
                                         height = "30px",          # Bar height*
                                         border_radius = "4px",    # Rounded corners
                                         color = "white",          # Text color inside the bar
                                         text_align = "center"))))
                          })
      ),
      align = c("c","l","c","l","r","r","c")
    ) %>%
    as.htmlwidget(width="60%")
    
  })
  
  
  output$page_footer <- renderText({
    paste0("* Value above 100%. Benchmark 6: testing capacity above 100% does not imply universal access but simply sufficient capacity is available. The distribution and accessibility of such capacity may still be inadequate requiring optimization. Other benchmarks: values above 100% may be due to data errors."
    )
  })
  
  
  output$table_bench <- function(){
    
    step_colors <- c("#064871", "","","","","",
                     "#DA471F", "","","",
                     "#0096B3", "","","","","",
                     "#AD176E", "","")
    
    # Create a kableExtra table
    kable(bench, "html", escape = FALSE, col.names = c("Step", "Benchmark", "Numerator", "Denominator"), table.attr = "style='width:50%;'") %>%
      kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover", "condensed")) %>%
      row_spec(0, background = "#00A14C", color = "white") %>%  # Header row styling
      column_spec(1, extra_css = "text-align: center;", background = step_colors, color = "white", width = "10px") %>%
      column_spec(2, extra_css = "text-align: center;", width = "10px") %>%
      column_spec(3, width = "500px") %>%
      column_spec(4, width = "350px") %>%
      collapse_rows(columns = 1, valign = "middle") #%>%  # Merge cells for "Step"
      # Apply background colors to the merged cells of "Step"
  #     row_spec(1:6, background = "#064871", color = "white") %>%  # Color for Step 1
  #     row_spec(7:10, background = "#DA471F", color = "white") %>%  # Color for Step 2
  #     row_spec(11:16, background = "#0096B3", color = "white") %>%  # Color for Step 3
  #     row_spec(17:19, background = "#AD176E", color = "white")  # Color for Step 4
      
  }
  
  
}  


# Run the application
shinyApp(ui = ui, server = server)

