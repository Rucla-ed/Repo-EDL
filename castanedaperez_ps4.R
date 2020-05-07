##
##[ PROJ ] < Problem set # 4>
## [ FILE ] < castanedaperez_script>
## [ AUTH ] < Estefania Castaneda Perez / ecp9 >
## [ INIT ] < May 1 2020>
## [Key] < jtgi1g4pgbfaqcvH7xet4txk5ABUOiijdfJ7FWWT
################################################################################

## ---------------------------
## libraries
## ---------------------------
library(tidyverse)
library(rscorecard)
## ---------------------------
## directory paths
## ---------------------------

data_dir<- setwd("/Users/andromeda/EDL_ps4/data/")
plot_dir <- setwd("/Users/andromeda/EDL_ps4/plots/")

## -----------------------------------------------------------------------------
## Part X - Label each question using comments
## -----------------------------------------------------------------------------

#install
devtools::install_github("btskinner/rscorecard")
#set API key
sc_key("jtgi1g4pgbfaqcvH7xet4txk5ABUOiijdfJ7FWWT")

#load data
library(rscorecard)

df <- sc_init() %>% 
  sc_filter(region == 2, ccbasic == c(21,22,23), locale == 41:43) %>% 
  sc_select(unitid, instnm, stabbr) %>% 
  sc_year("latest") %>% 
  sc_get()
df


###fetching data

full_df 
sc_dict('^__')
preddeg==3 
region ==9

df<-sc_init() %>% 
  sc_filter(preddeg == 3, region !=9) %>%
  sc_select(unitid, instnm, control, stabbr, tuitionfee_in, tuitionfee_out, md_earn_wne_p10, st_fips) %>%
  sc_get()

df <- df %>% mutate (school_type = recode(control,
                                          "1" = 1,
                                          "2" = 2,
                                          "3" = 2),  tuitionfee_diff = tuitionfee_in - tuitionfee_out,     tuitionfee_diff_pct = (tuitionfee_diff) / tuitionfee_in)


saveRDS(df, file.path(data_dir, "ecp.RDS"))


### ggplot code

df_plot<-ggplot(data= df, aes(x = tuitionfee_out, y = md_earn_wne_p10, color= as.factor(school_type))) + geom_point() + geom_smooth() + ylab("median earnings") + xlab("out state tuition")

saveRDS(df_plot, file.path(plot_dir, "ecpplot.png"))



df_plot2<-ggplot(data= df, aes(x = tuitionfee_in, y = md_earn_wne_p10, color= as.factor(school_type))) + geom_point() + geom_smooth() + ylab("median earnings") + xlab("in state tuition")
saveRDS(df_plot2, file.path(plot_dir, "ecpplot2.png"))


## df with only public universities
df<-na.omit(df)

public_school_only<-df %>% filter(school_type==1) %>% mutate(st_fips = recode(st_fips, "72" = "PR"))


df_plot3<-ggplot(public_school_only, aes(x = st_fips, y = tuitionfee_diff_pct *-1), fill = forcats::fct_rev(as_factor(round(tuitionfee_diff_pct*100, digits = 0)))) +  geom_bar(stat="identity") + ylab(" Tuition Fee Diff") + xlab("States")


saveRDS(df_plot3, file.path(plot_dir, "ecpplot3.png"))



### Part IV

df <- sc_init() %>% 
  sc_filter(region == 2, ccbasic == c(21,22,23), locale == 41:43) %>% 
  sc_select(unitid, instnm, stabbr) %>% 
  sc_year("latest") %>% 
  sc_get()



df_ecp<-sc_init() %>% 
  sc_filter(preddeg == 3, region !=9) %>%
  sc_select(unitid, instnm, control, stabbr,first_gen, pct_hispanic) %>%
  sc_get()


saveRDS(df_ecp, file.path(data_dir, "ecp_custom.RDS"))


ggplot(data= df_ecp, aes(x = pct_hispanic, y = first_gen)) + geom_point() + geom_smooth() + ylab("Share of first Generation Students") + xlab("Percentage of hispanic students")


saveRDS(df_ecp, file.path(plot_dir, "ecp_custom.png"))


##Issue22

#https://github.com/Rucla-ed/rclass2/issues/101

## -----------------------------------------------------------------------------
## END SCRIPT
## -----------------------------------------------------------------------------