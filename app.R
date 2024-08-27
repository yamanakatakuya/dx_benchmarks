# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Shiny app to display TB diagnostic benchmarks for selected countries.
# Takuya Yamanaka, September 2024
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

app_version <- "Version 1.0_beta"

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
  select(country:g_whoregion,plhiv_all_screen_data_available:m_wrd_tat_lt_48h)

tpt <- read.csv("latest_contacts_tpt_2024-08-26.csv") %>%
  select(iso2,newinc_con:newinc_con_screen)

df <- str %>%
  left_join(tpt, by = "iso2") %>%
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
       mutate(bm1a = newinc_con_screen/newinc_con*100,
              bm1b = plhiv_all_screen/plhiv_all*100,
              bm1c = plhiv_new_screen/plhiv_new*100,
              bm1d = prisoners_screen/prisoners*100,
              bm1e = miners/miners_screen_data_available*100,
              bm2  = district_cxr/district*100,
              bm3  = district_wrd/district*100,
              bm4  = phcf_wrd/phcf*100,
              bm5  = NA,
              bm6  = wrd_test_capacity/presumptive*100,
              bm7  = NA, # m_wrd_error_rate_lte_5pct/m_wrd"100
              bm8  = presumptive_wrd/presumptive*100,
              bm9a = NA,
              bm9b = NA,
              bm9c = NA,
              bm9d = NA,
              bm10 = NA,
              bm11 = district_monitor_pos_rate/district*100,
              bm12 = NA) #m_wrd_tat_lt_48h/m_wrd"100
     
     dfc <- dfc %>% 
       select(!contains("_available")&!contains("_description")) %>%
       pivot_longer(!country:g_whoregion,
                    names_to = "variable",
                    values_to = "value")
     
     dfc2 <- dfc %>% 
       mutate(category = ifelse(grepl("^bm", variable),"pcnt","number")) %>%
       pivot_wider(names_from = "category", values_from = "value")

  })
  
  output$table <- renderFormattable({
    
    dfc <- dfc()
    
    formattable(
      dfc,
      list(
        age = color_tile("white", "orange"),
        grade = formatter("span", style = x ~ ifelse(
          x == "A",
          style(color = "green", font.weight = "bold"), NA
        )),
        area(col = c(test1_score, test2_score)) ~ normalize_bar("pink", 0.2),
        final_score = formatter(
          "span",
          style = x ~ style(color = ifelse(rank(-x) <= 3, "green", "gray")),
          x ~ sprintf("%.2f (rank: %02d)", x, rank(-x))
        ),
        registered = formatter(
          "span",
          style = x ~ style(color = ifelse(x, "green", "red")),
          x ~ icontext(ifelse(x, "ok", "remove"), ifelse(x, "Yes", "No"))
        )
      )
    )
  })
  
  
  
}  
