################################################################################
##
## [ PROJ ] < Problem set #4 >
## [ FILE ] < Perales_script.R >
## [ AUTH ] < Lorraine / Lperales00 >
## [ INIT ] < 4/25/2020 >
##
################################################################################
#Grade: 20/20
#Good job Lorraine

#rkey
sc_key('AdHIjk1j7sA2LaLO3xsHkea6bLBCAQgUYpYbpODW')

# Part I (2 pts)
## ---------------------------
## libraries


library(tidyverse)
#install.packages("rscorecard")
#devtools::install_github("btskinner/rscorecard")
#install.packages("rlang")
library(rscorecard)
## ---------------------------

## ---------------------------
## directory paths

data_dir <- file.path('~','R','RClass2', 'Week_4', 'HW', 'EDL_ps4', 'data')

plots_dir <- file.path('~','R','RClass2', 'Week_4', 'HW', 'EDL_ps4', 'plots')

## ---------------------------

## Part II - College Scorecard API (5 pts)
## -----------------------------------------------------------------------------
## Part 2 - Label each question using comments


## 2.

library(rscorecard)

full_df <- sc_init() %>% 
  sc_filter(region == 2, ccbasic == c(21,22,23), locale == 41:43) %>% 
  sc_select(unitid, instnm, stabbr) %>% 
  sc_year("latest") %>% 
  sc_get()


### 5.
##- Filter
#- Include only institutions that predominantly offers bachelor's degree
#        - Do not include any service schools or institutions located outside the 50 states (ie. exclude schools whose `region` is classified as `U.S. Service Schools` or `Outlying Areas`)
#    - Select
#        - Unitid
#        - Institution name
#        - Institution control
#        - Institution state
#        - In-state tuition and fees
#        - Out-of-state tuition and fees
#       - Median earnings of students working and not enrolled 10 years after entry

sc_dict("REGION") ## 1 & 9
sc_dict("PREDDEG") ##3
sc_dict("NPCURL")
sc_dict("control")

full_df <- sc_init() %>%
  sc_filter(PREDDEG == 3, REGION == c(1,9)) %>%
  sc_select(unitid, instnm, control, st_fips, tuitionfee_in, tuitionfee_out, md_earn_wne_p10) %>%
  sc_get()



# 6. Perform some further data manipulations and save the resulting dataframe in `df`:
# 
#   - Add a column called `school_type` that is `1` if the institution is public and `2` if it is private
# - Add a column called `tuitionfee_diff` that is the difference between in-state and out-of-state tuition
# - Add a column called `tuitionfee_diff_pct` that is the [percentage increase](https://www.onlinemathlearning.com/image-files/percent-change.png) between the in-state and out-of-state tuition (don't have to multiply by 100)

full_df <- full_df %>% mutate(school_type = recode(control,
                                          "1" = 1,
                                          "2" = 2,
                                          "3" = 2),
                     tuitionfee_diff = tuitionfee_out - tuitionfee_in,
                     tuitionfee_diff_pct = tuitionfee_diff/tuitionfee_in)

 
# 7. Use `saveRDS()` to save your `df` in a file called `<last_name>_rscorecard.RDS` in the `data_dir`. Add this data file and commit with the message `add rscorecard data`.


saveRDS(full_df, file.path(data_dir, "perales_rsscorecard.RDS"))

## Part III - ggplot (6 pts)
## -----------------------------------------------------------------------------
# PART 3
# 
# 1. Still on the `dev_[initials]` branch, use `full_df` to create the following scatterplot in `<last_name>_script.R`:
#   
#   - X-axis: Out-of-state tuition
# - Y-axis: Median earnings
# - Color: School type
# - Add smoothed prediction lines for each school type
# - Label your graph
# 
# Save your plot as `out_of_state_tuition_earnings.png` in the `plot_dir`. Add this plot and commit with the message `add scatterplot (out-of-state tuition vs earnings)`.

ggplot(data = full_df) + geom_point(mapping = aes(x = tuitionfee_out, y = md_earn_wne_p10 , color = as_factor(school_type))) + geom_smooth(mapping = aes(x = tuitionfee_out, y = md_earn_wne_p10, color=as_factor(school_type))) + 
  ylab("Median Earnings") + xlab("Out of state tuition") + scale_color_discrete(name = "School Type", labels = c("Public", "Private"))

saveRDS(full_df, file.path(plots_dir, "out_of_state_tuition_earning.png"))

# 2. 

ggplot(data = full_df) + geom_point(mapping = aes(x = tuitionfee_in, y = md_earn_wne_p10 , color = as_factor(school_type))) + geom_smooth(mapping = aes(x = tuitionfee_in, y = md_earn_wne_p10, color=as_factor(school_type))) + 
  ylab("Median Earnings") + xlab("In state tuition") + scale_color_discrete(name = "School Type", labels = c("Public", "Private"))

saveRDS(full_df, file.path(plots_dir, "in_state_tuition_earnings.png"))

 
# 3. Now, filter your `df` to include only public universities for creating the following bar plot:
#   
#   - X-axis: State
# - Y-axis: Average `tuitionfee_diff_pct` for public universities in that state
# - Label your graph
# 
# Save your plot as `diff_in_tuition_by_state.png` in the `plot_dir`. Add this plot and commit with the message `add barplot (diff in tuition)`.


full_df <- na.omit(full_df)
full_df <- full_df %>% filter(school_type == 1) %>% mutate(st_fips = recode(st_fips,
                                                               "9" = "Connecticut",
                                                               "23" = "Maine",
                                                               "25" = "Massachusetts",
                                                               "33" = "New Hampshire",
                                                               "44" = "Rhode Island",
                                                               "50" = "Vermont",
                                                               "66" = "Guam",
                                                               "72" = "Puerto Rico",
                                                               "78" = "Virgin Islands"))




ggplot(full_df, aes(x = st_fips, y = (tuitionfee_diff_pct*.10), fill = forcats::fct_rev(as_factor(round(tuitionfee_diff_pct*100, digits = 0))))) +
  geom_bar(stat="identity") + labs(x = "State", y = "percent tuition diff", fill = "Percentage") + 
  theme_minimal(base_size = 10) + scale_y_continuous(labels = scales::percent)

saveRDS(full_df, file.path(plots_dir, "diff_in_tuition_by_state.png"))  

## Part IV - Pull request (4 pts)
## Part IV 

# 2. Use the `rscorecard` package to pull any data you'd like. Save the data in a file called `<last_name>_custom.RDS` in the `data_dir`.
# 
#     Add the data file and commit with the message `add custom data`.
# 

LP_df <- sc_init() %>% 
  sc_filter(region == 2, ccbasic == c(21,22,23), locale == 41:43) %>% 
  sc_select(unitid, instnm, stabbr) %>% 
  sc_year("latest") %>% 
  sc_get()


LP_df <- sc_init() %>%
  sc_filter(PREDDEG == 3, REGION == c(1,9)) %>%
  sc_select(unitid, instnm, control, ugds_black, c150_4, st_fips, pct_black, tuitionfee_in, tuitionfee_out, md_earn_wne_p10, highdeg, locale, loan_wdraw_orig_yr3_rt) %>%
  sc_get()

saveRDS(LP_df, file.path(data_dir, "LP_custom.RDS"))

# 3. Using that data, create a plot of your choice. Save your plot as `<last_name>_custom.png` in the `plot_dir`.

LP_df <- na.omit(LP_df)

 
ggplot(data= LP_df, aes(x = pct_black, y = loan_wdraw_orig_yr3_rt)) + geom_point() + geom_smooth() + ylab("Percentage of students who dropped out within 3 years with student loan") + xlab("Percentage of black students")
saveRDS(LP_custom, file.path(plots_dir, "LP_custom.png"))


## Part V - I got issues (2 pts)
#ISSUE
#https://github.com/Rucla-ed/rclass2/issues/88#issue-608087527


## Part VI - Wrapping up (1 pt)
# spent 6 hours working on this pset

## -----------------------------------------------------------------------------
## END SCRIPT
