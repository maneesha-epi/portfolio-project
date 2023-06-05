## hpv 2021 data tables and analysis

# color blind friendly 
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


#########################################################################################################

# exploring categorical data: hpv vaccination status, hpvstat
# unweighted proportion = frequency of the sample
prop_unw <- coded_hpv21 %>%
  group_by(hpvstat) %>%
  summarize(freq = n()) %>%
  mutate(prop = freq/sum(freq)) %>%
  arrange(desc(prop))
prop_unw 

# weighted proportion = has estimated frequency of the population
prop_w <- svytable(~hpvstat, design = design_hpv21) %>%
  as.data.frame() %>%
  # Freq is default in svytable
  mutate(prop = round(Freq / sum(Freq),4), perc = percent(Freq / sum(Freq), accuracy = 0.01)) %>%
  arrange(desc(prop))
prop_w

# graph of weighted proportions
ggplot(prop_w,
       mapping = aes(x = hpvstat, y = prop)) +
  geom_col(fill = "#E69F00") +
  coord_flip() +
  # adding below re-orders the factor level
  # scale_x_discrete(limits = prop_w$hpvstat) +
  # add this to change the scale of y from prop to percent
  scale_y_continuous(labels = scales::percent) +
  # this adds number in bar
  geom_text(aes(label = perc), 
           position = position_stack(vjust = 0.5),
           hjust = 0.5, size = 3.5, colour = "black") +
  labs(
    x = "HPV Vaccination Status", y = "Proportion", 
    title = "HPV Vaccination Status",
    caption = "HPV Module Data, Behavioral Risk Factor Surveillance System, 2021"
  ) + 
  theme(plot.title = element_text(hjust = 0.6, face = "bold", size = 12)) 


# calculating weighted proportions storing as tibble
prop_w_tibble <- svytable(~hpvstat, design = design_hpv21) %>%
  as_tibble() %>%
  mutate(Proportion = n/sum(n))
prop_w_tibble

#########################################################################################################

# exploring sex & hpv vaccination status
# weighted proportion
prop_wSex <- svytable(~sex + hpvstat, design = design_hpv21) %>%
  data.frame() %>%
  # group by to get denominator percentages correct as per tbl_summary()
  group_by(hpvstat) %>%
  # Freq is default in svytable
  mutate(n_total = sum(Freq),
         prop_vac = Freq / sum(Freq), 
         perc_vac = percent(Freq / sum(Freq), accuracy = 0.01)) %>%
  arrange(desc(prop_vac))
prop_wSex

# graph
ggplot(prop_wSex,
       mapping = aes(x = hpvstat, y = prop_vac, fill = sex)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = perc_vac), 
            position = position_stack(vjust = 0.5),
            hjust = 0.5, size = 3.5, colour = "black") +
  labs(x = "HPV Vaccination Status", y = "Proportion", 
       title = "HPV Vaccination Status by Sex", 
       caption = "HPV Module Data, Behavioral Risk Factor Surveillance System, 2021") + 
  guides(fill = guide_legend(title = "Sex")) + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values = alpha(c("#CC79A7", "#56B4E9"))) +
  theme(plot.title = element_text(hjust = 0.6, face = "bold")) 



#########################################################################################################

# cluster stacked bar graph
# table
prop_wSexState <- svytable(~sex + state + hpvstat, design = design_hpv21) %>%
  as.data.frame() %>%
  group_by(hpvstat) %>%
  mutate(prop_vac = round(Freq / sum(Freq),3), 
         perc_vac = percent(Freq / sum(Freq), accuracy = 0.01)) %>%
  arrange(desc(prop_vac))
prop_wSexState
# graph
hpv_states_plot <- ggplot(prop_wSexState, 
       mapping = aes(x = sex, y = prop_vac, fill = hpvstat)) + 
  geom_bar(stat = "identity", position = "stack") +
  facet_grid(~ state) +
  scale_y_continuous(labels = scales::percent) +
  geom_text(aes(label = perc_vac), 
            position = position_stack(vjust = 0.5),
            hjust = 0.5, size = 3.5, colour = "black") +
    labs( x = "Sex", y = "Proportion", 
        title = "HPV Vaccination Status by Sex Across Selected US States",
        caption = "HPV Module Data, Behavioral Risk Factor Surveillance System, 2021") + 
  guides(fill = guide_legend(title = "HPV Vaccination Status")) + 
  theme(legend.position = "bottom") +
  scale_fill_manual(values = alpha(c("#D55E00", "#E69F00", "#009E73"))) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16)) 

  ggsave("hpv_states_plot.png", plot = hpv_states_plot, height = 8, width = 11)

#########################################################################################################

# other tables: HPV Test at CC Screening 
svytable(~crvclhpv + sex + hpvstat, design = dsgn1_hpv21) %>%
  as_tibble() %>%
  group_by(crvclhpv) %>%
  filter(sex == "Female") %>%
  mutate(
    n_crvclhpv = sum(n), 
    Prop_hpvstat = n/sum(n)) %>%
  # any further mutations called on it would not use the grouping 
  ungroup()

# what is the probability that a person in full vac status has insurance?
svytable(~hlthpln + hpvstat, design = dsgn1_hpv21)%>%
  as_tibble() %>%
  group_by(hlthpln) %>%
  mutate(
    n_hlthpln = sum(n), 
    Prop_hpvstat = n/sum(n)) %>%
  # any further mutations called on it would not use the grouping 
  ungroup()

# does receiving hpv vaccine vary by state?
svytable(~hpvstat + state, design = dsgn1_hpv21) %>%
  as_tibble() %>%
  group_by(state) %>%
  mutate(n_state = sum(n),
         Prop_hpv = round((n/n_state), 2)) %>%
  as.data.frame() %>%
  ungroup()

#########################################################################################################
# summarize data with tbl_summary(), produceds publishable data table

# main frequency table with sampel counts and population proportion
# NOTE: 951 observations missing `hpvstat` have been removed. To include these observations,
# use `forcats::fct_na_value_to_level()` on `hpvstat` column before passing to `tbl_svysummary()` --> NOT WORKING
maintable <- design_hpv21 %>%
  tbl_svysummary(
    # stratify summary statistics by the "both" column
    by = hpvstat,
    # summarize a subset of the columns
    include = c(sex, ageg5yr, race, educag, hlthpln, state),
    # adding labels to table
    label = list(
      sex ~ "Sex", 
      ageg5yr ~ "Age Group", 
      race ~ "Race/Ethnicity", 
      educag ~ "Education Level",
      hlthpln ~ "Health Insurance",
      state ~ "State of Residence"
    ), 
    # sample count & population percentage 
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"), 
    # remove missing section
    missing = "no"
  ) %>%
  modify_spanning_header(all_stat_cols() ~ "HPV Vaccination Status") %>%
  bold_labels() %>%
 # add_overall() %>% 
  add_p() 
maintable
# export table
maintable %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "maintable.docx")

#########################################################################################################
# summarize a logistic regression model predicting hpv vaccination status
# display odds ratio estimates, confidence intervals, p-values for covariates, reference levels for categorical vars

logregm1 = glm(hpvshot ~ female + hisp + educ,
         data = logreg_hpv21,
         family = binomial(link = "logit"))
summary(logregm1)

# manual = works
logremcheck <- glm(hpvshot ~ female + hisp + educ, family = binomial(link = "logit"), 
                   na.action = na.omit, data = logreg_hpv21)
summary(logremcheck)
exp(logremcheck$coefficients[-1])
# percentage
exp((logremcheck$coefficients[-1])-1)*100

# summarize model with table regression
logregtable1 <- tbl_regression(logregm1, exponentiate = TRUE, 
                             pvalue_fun = ~style_pvalue(.x, digits = 2),
                             label = list(
                               female ~ "Sex", 
                               hisp ~ "Race/Ethnicity", 
                               educ ~ "Education Level")) %>%
  # adding labels to table
  bold_labels() %>%
  add_global_p()

# export table
logregtable1 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "logregtable1.docx")

#########################################################################################################

# summarize a logistic regression model predicting hpv vaccination status
# display odds ratio estimates, confidence intervals, p-values for covariates, reference levels for categorical vars

logregm2 = glm(hpvshot ~ ins + hlth + dochk + cervs,
               data = logreg_hpv21,
               family = binomial(link = "logit"))
summary(logregm2)

# summarize model with table regression
logregtable2 <- tbl_regression(logregm2, exponentiate = TRUE, 
                             pvalue_fun = ~style_pvalue(.x, digits = 2),
                             label = list(
                               ins ~ "Health Insurance", 
                               hlth ~ "Good/Better Health ", 
                               dochk ~ "Time Since Routine Check-up", 
                               cervs ~ "Had Cervical Cancer Screening")) %>%
  # adding labels to table
  bold_labels() %>%
  add_global_p()

# see percentages
exp((logregm2$coefficients[-1])-1)*100

# export table
logregtable2 %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "logregtable2.docx")

#########################################################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################
#####################################################################


#########################################################################################################
# INCOMPLETE
# pie/donut graph
# table
prop_wCervhpv <- svytable(~cervscrn + sex + crvclhpv + hpvstat, design = design_hpv21) %>%
  as.data.frame() %>%
  filter(cervscrn == "Yes") %>%
  filter(sex == "Female") %>%
  group_by(crvclhpv) %>%
  arrange(desc(hpvstat)) %>%
  mutate(prop = round(Freq / sum(Freq), 2)) %>%
  arrange(desc(prop))


#graph 
ggplot(prop_wCervhpv) +
  geom_col(aes(x = 1, y = prop, fill = hpvstat), position = "fill") +
  # change bar to circle
  coord_polar(theta = "y", start = 0) +
  # split to two
  facet_wrap(~ crvclhpv) +
  # label 
  geom_text(aes(x = 0, y = labypos, label = prop), color = "black") +
  # donut style
  xlim(0, 1.5) +
  # add colors 
  scale_fill_manual(values = alpha(c("#D55E00", "#E69F00", "#009E73"))) +
  # adds border, place before removing the other noise
  theme_bw() +
  # remove all the grid lines
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  guides(fill = guide_legend(title = "Vaccination Status")) 

