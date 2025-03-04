## load packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(dplyr)
library(kableExtra)

# selected countries
select <- c("AO",	"AR",	"AZ",	"BD",	"BY",	"BZ",	"BO",	"BR",	"KH",	"CF",	"CL",	"CN",	"CO",	"CG",	"CR",	"CU",	"KP",	"CD",	"DO",	"EC",	"SV",	"ET",	"GA",	"GT",	"GY",	"HT",	"HN", "IN",	"ID",	"JM",	"KE",	"KG",	"LA",	"LS",	"LR",	"MX",	"MN",	"MZ",	"MM",	"NA",	"NP",	"NI",	"NG",	"PK",	"PA",	"PG",	"PY",	"PE",	"PH",	"MD","RU",	"SL","ZA",	"SR",	"TJ",	"TH",	"UG",	"UA",	"TZ",	"UY",	"UZ",	"VE",	"VN",	"ZM",	"ZW")

## load notifications, TPT and strategy datasets
### data is as of 18 October 2024
str <- read.csv("latest_strategy_2024-10-18.csv", na.strings = "") |>
  select(country:g_whoregion ,plhiv_all_screen_data_available:m_wrd_tat_lt_48h,m_wrd)

tpt <- read.csv("latest_contacts_tpt_2024-10-18.csv", na.strings = "") |>
  select(iso2,newinc_con:newinc_con_screen)

notif <- read.csv("latest_notifications_2024-10-18.csv", na.strings = "") |>
  select(iso2,new_labconf:tbdeaths_vr)

bench <- read.csv("dx_benchmark_exp.csv")


sum_of_row <- function(x) {
  tosum <- as.matrix(x)
  summed <- rowMeans((tosum), na.rm=TRUE) * rowSums(!is.na((tosum)))
  # Flush out any NaN's
  summed <- ifelse(is.nan(summed), NA, summed)
  return(summed)
}

df <- str |>
  left_join(tpt, by = "iso2") |>
  left_join(notif, by = "iso2") |>
  filter(!is.na(plhiv_all_screen_data_available)|iso2 %in% select) |> 
  # Restrict to the variables needed for the benchmarks
  select(iso2, country, year,g_whoregion,
         newinc_con_screen, newinc_con,
         plhiv_all_screen, plhiv_all,
         plhiv_new_screen, plhiv_new,
         prisoners_screen, prisoners,
         miners_screen, miners,
         district_cxr, district,
         district_wrd,
         phcf_wrd, phcf,
         newinc_rdx, newinc_pulm_labconf_rdx, newinc_pulm_clindx_rdx, newinc_ep_rdx, c_newinc,
         wrd_test_capacity, presumptive,
         m_wrd_error_rate_lte_5pct, m_wrd,
         presumptive_wrd, presumptive,
         r_rlt_new, r_rlt_ret,
         pulm_labconf_new, pulm_labconf_ret,
         rr_dst_rlt_fq, rr_new, rr_ret,
         rr_fqr_bdqr_lzdr, rr_fqr_bdqs_lzdr, rr_fqr_bdqu_lzdr,
         rr_fqr_bdqr_lzds, rr_fqr_bdqs_lzds, rr_fqr_bdqu_lzds,
         rr_fqr_bdqr_lzdu, rr_fqr_bdqs_lzdu,
         rr_fqr,
         newinc_pulm_labconf_rdx, newinc_pulm_clindx_rdx,
         new_labconf, new_clindx, ret_rel_labconf, ret_rel_clindx,
         district_monitor_pos_rate,
         m_wrd_tat_lt_48h) 
  
  df <- df |> 
  mutate(
    num_1a = newinc_con_screen,
    den_1a = newinc_con,
    num_1b = plhiv_all_screen,
    den_1b = plhiv_all,
    num_1c = plhiv_new_screen,
    den_1c = plhiv_new,
    num_1d = prisoners_screen,
    den_1d = prisoners,
    num_1e = miners_screen,
    den_1e = miners,
    num_2 = district_cxr,
    den_2 = district,
    num_3 = district_wrd,
    den_3 = district,
    num_4 = phcf_wrd,
    den_4 = phcf,
    num_5 = sum_of_row(df |> select(newinc_rdx, newinc_pulm_labconf_rdx, newinc_pulm_clindx_rdx, newinc_ep_rdx)),
    den_5 = c_newinc,
    num_6 = wrd_test_capacity,
    den_6 = presumptive,
    num_7 = m_wrd_error_rate_lte_5pct,
    den_7 = m_wrd,
    num_8 = presumptive_wrd,
    den_8 = presumptive,
    num_9a = sum_of_row(df |> select(r_rlt_new, r_rlt_ret)),
    den_9a = sum_of_row(df |> select(pulm_labconf_new, pulm_labconf_ret)),
    num_9b = rr_dst_rlt_fq,
    den_9b = sum_of_row(df |> select(rr_new, rr_ret)),
    num_9c = sum_of_row(df |> select(rr_fqr_bdqr_lzdr, rr_fqr_bdqs_lzdr, rr_fqr_bdqr_lzds, rr_fqr_bdqs_lzds, rr_fqr_bdqr_lzdu, rr_fqr_bdqs_lzdu)),
    den_9c = rr_fqr,
    num_9d = sum_of_row(df |> select(rr_fqr_bdqr_lzdr, rr_fqr_bdqs_lzdr, rr_fqr_bdqu_lzdr, rr_fqr_bdqr_lzds, rr_fqr_bdqs_lzds, rr_fqr_bdqu_lzds)),
    den_9d = rr_fqr,
    num_10 = sum_of_row(df |> select(newinc_pulm_labconf_rdx, newinc_pulm_clindx_rdx)),
    den_10 = sum_of_row(df |> select(new_labconf, new_clindx, ret_rel_labconf, ret_rel_clindx)),
    num_11 = district_monitor_pos_rate,
    den_11 = district,
    num_12 = m_wrd_tat_lt_48h,
    den_12 = m_wrd
    ) 

