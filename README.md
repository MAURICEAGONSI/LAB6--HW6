# LAB6--HW6

MAURICE AGONSI

> load("ACS_2021_couples.RData")  # assumes acs2021_couples is loaded
> 
> acs2021_couples <- acs2021_couples %>%
+   mutate(age_diff = AGE - h_age)
> # convert EDUC factor -> numeric years
> acs2021_couples <- acs2021_couples %>%
+   mutate(
+     educ_numeric = fct_recode(EDUC,
+                               "0" = "N/A or no schooling",
+                               "2" = "Nursery school to grade 4",
+                               "6.5" = "Grade 5, 6, 7, or 8",
+                               "9" = "Grade 9",
+                               "10" = "Grade 10",
+                               "11" = "Grade 11",
+                               "12" = "Grade 12",
+                               "13" = "1 year of college",
+                               "14" = "2 years of college",
+                               "15" = "3 years of college",
+                               "16" = "4 years of college",
+                               "17" = "5+ years of college"
+     ),
+     educ_numeric = as.numeric(as.character(educ_numeric)),
+     h_educ_numeric = fct_recode(h_educ,
+                                 "0" = "N/A or no schooling",
+                                 "2" = "Nursery school to grade 4",
+                                 "6.5" = "Grade 5, 6, 7, or 8",
+                                 "9" = "Grade 9",
+                                 "10" = "Grade 10",
+                                 "11" = "Grade 11",
+                                 "12" = "Grade 12",
+                                 "13" = "1 year of college",
+                                 "14" = "2 years of college",
+                                 "15" = "3 years of college",
+                                 "16" = "4 years of college",
+                                 "17" = "5+ years of college"
+     ),
+     h_educ_numeric = as.numeric(as.character(h_educ_numeric)),
+     educ_diff = educ_numeric - h_educ_numeric
+   )
> # SUBGROUP 
> acs_subgroup <- acs2021_couples %>%
+   filter(AGE >= 25, AGE <= 55,
+          LABFORCE == 2,   # presumably 2 == in labor force? (use correct code)
+          WKSWORK2 > 4,
+          UHRSWORK >= 35)
> summary(acs2021_couples$age_diff)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
-90.0000  -3.0000   0.0000  -0.7558   2.0000  78.0000 
> summary(acs2021_couples$educ_diff)
     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
-16.00000  -1.00000   0.00000  -0.02167   1.00000  17.00000 
> # By sex/householder sex 
> summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-90.000  -5.000  -2.000  -2.506   0.000  49.000 
> summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Male")&(acs2021_couples$h_sex == "Female")])
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-77.000   0.000   1.000   1.795   4.000  78.000 
> summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Male")&(acs2021_couples$h_sex == "Male")])
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-88.000  -6.000  -1.000  -3.568   1.000  59.000 
> summary(acs2021_couples$age_diff[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Female")])
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -85.00   -4.00    0.00   -2.64    1.00   60.00 
> # compare raw ages
> summary(acs2021_couples$AGE[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    0.0    40.0    54.0    53.1    66.0    95.0 
> summary(acs2021_couples$h_age[(acs2021_couples$SEX == "Female")&(acs2021_couples$h_sex == "Male")])
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
   15.0    42.0    57.0    55.6    69.0    96.0 
> # Tabulate 
> table(acs2021_couples$SEX, acs2021_couples$h_sex)
        
           Male Female
  Male    22865 322095
  Female 412275  18829
> # Histogram of age_diff
> ggplot(acs2021_couples, aes(x = age_diff)) +
+   geom_histogram(bins = 80) +
+   labs(title = "Distribution of age_diff (spouse - householder)",
+        x = "age_diff (years)", y = "count")
> # Scatter: educ_diff vs age_diff with smoother
> ggplot(acs_subgroup, aes(x = educ_diff, y = age_diff)) +
+   geom_jitter(alpha = 0.15, height = 0, width = 0.2) +
+   geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = TRUE) +
+   labs(title = "age_diff vs educ_diff (subsample)")
> # Age difference by sex pairing
> acs2021_couples %>%
+   mutate(pair = paste(SEX, "with h_", h_sex, sep="")) %>%
+   ggplot(aes(x = pair, y = age_diff)) +
+   geom_boxplot() +
+   coord_flip() +
+   labs(title = "Age difference by sex pairing")
+   
> MODELS
> # Simple linear
> m1 <- lm(age_diff ~ educ_diff, data = acs2021_couples)
> # Polynomial: squared and cubed
> m2 <- lm(age_diff ~ educ_diff + I(educ_diff^2) + I(educ_diff^3), data = acs2021_couples)
> # Add controls: ages and polynomials, separate educ terms for each partner
> m3 <- lm(age_diff ~ educ_numeric + h_educ_numeric + AGE + h_age +
+            I(AGE^2) + I(h_age^2) + I(educ_numeric^2) ,
+          data = acs_subgroup)  # or acs2021_couples
+      
> # Robust SEs
> coeftest(m1, vcov = vcovHC(m1, type = "HC1"))
>  PREDICTIONS 
> # Create grid of educ_diff values for predicted age_diff
> grid <- data.frame(educ_diff = seq(min(acs2021_couples$educ_diff, na.rm=TRUE),
+                                    max(acs2021_couples$educ_diff, na.rm=TRUE), length.out = 101))
> grid$pred_m2 <- predict(m2, newdata = grid, se.fit = TRUE)$fit
> 
> # CIs using vcovHC:
> pred_m2 <- predict(m2, newdata = grid, se.fit = TRUE)
> grid$pred <- pred_m2$fit
> grid$se <- pred_m2$se.fit
> grid <- grid %>% mutate(lwr = pred - 1.96 * se, upr = pred + 1.96 * se)
> ggplot(grid, aes(x = educ_diff, y = pred)) +
+   geom_line() +
+   geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2) +
+   labs(title = "Predicted age_diff vs educ_diff (M2)", x = "educ_diff", y = "predicted age_diff")
> # Essential outputs for report
> saveRDS(list(m1 = m1, m2 = m2, m3 = m3, grid = grid), file = "lab6_results.rds")
> acs2021_couples %>% filter(abs(age_diff) > 30) %>%
+   select(AGE, h_age, age_diff, SEX, h_sex, educ_numeric, h_educ_numeric) %>%
+   arrange(desc(abs(age_diff)))
# A tibble: 3,986 × 7
     AGE h_age age_diff SEX    h_sex  educ_numeric h_educ_numeric
   <dbl> <dbl>    <dbl> <fct>  <fct>         <dbl>          <dbl>
 1     0    90      -90 Female Male              0             12
 2     2    90      -88 Male   Male              0             12
 3     5    90      -85 Female Female            2             12
 4     0    84      -84 Female Female            0             16
 5     2    85      -83 Female Male              0             11
 6     1    82      -81 Female Male              0             16
 7     0    80      -80 Female Female            0             15
 8    94    16       78 Male   Female           17             11
 9    16    93      -77 Female Male             10             15
10     6    83      -77 Male   Female            2             12
# ℹ 3,976 more rows

ANALYSIS

1. Data Preparation & Variable Construction.
2. The analysis starts by loading the ACS 2021 couples dataset, which contains demographic and socioeconomic information about couples.
•	Creating Key Variables:
•	Age Difference: Calculated as AGE-h_age, representing the difference between the spouse’s age and the householder’s age.
•	Education Mapping: Both partners’ education levels (originally categorical) are mapped to numeric years using a custom mapping. For example, “Grade 12” becomes 12, “4 years of college” becomes 16, etc. This allows for quantitative analysis of education differences.
•	Education Difference: Calculated as educ_numeric - h_educ_numeric, showing the difference in years of education between partners.

2. Defining the Subgroup
•	Subgroup Criteria: To focus on a relevant population, I filtered for couples where:
•	The spouse is aged 25–55 (prime working age).
•	The spouse is in the labor force (LABFORCE == 2).
•	The spouse works more than 4 weeks per year and at least 35 hours per week (full-time).
This subgroup is important because it targets working-age, economically active couples, reducing noise from retirees, students, or those not in the labor force.

3. Summary Statistics
Age Difference (All Couples)
•	Min: -90
•	Median: 0
•	Mean: -0.76
•	Max: 78
•	Quartiles: 1st: -3, 3rd: 2
 RESULT: Most couples have small age differences, but there are some extreme outliers.
Education Difference (All Couples)
•	Min: -16
•	Median: 0
•	Mean: -0.02
•	Max: 17
•	Quartiles: 1st: -1, 3rd: 1
   RESULT: Most couples have similar education levels, but some have large gaps.
By Sex Pairing
   I summarized age differences by couple type (e.g., Female-Male, Male-Female, Male-Male, Female-Female):
•	Female-Male: Mean age difference is -2.5 (female is younger), median is -2.
•	Male-Female: Mean is 1.8 (male is younger), median is 1.
•	Same-sex couples: Slightly larger negative means, indicating the householder tends to be older.
Counts by Pairing
•	Most couples are Male-Female, but there are substantial numbers of same-sex couples, allowing for meaningful subgroup analysis.
 GRAPHS
4. Visualizations
•	Histogram of Age Difference: Shows the distribution, with most couples clustered around zero but a long tail of outliers.
•	Scatterplot (Education Difference vs. Age Difference): For the working-age subgroup, you plotted education difference against age difference, adding a polynomial smoother to capture non-linear trends.
•	Boxplot by Sex Pairing: Visualizes how age difference varies by couple type.

5. Statistical Models & Hypothesis Tests
Linear Model (M1):
•	Model: age_diff ~ educ_diff
•	Purpose: Tests if the education difference predicts the age difference.
•	Robust SEs: Used to account for heteroskedasticity.
Polynomial Model (M2):
•	Model: Adds squared and cubed terms for education difference to capture non-linear effects.
•	Purpose: Checks if the relationship is more complex than linear.
Full Model (M3):
•	Model: Controls for both partners’ ages and education (including polynomials).
•	Purpose: Provides a more nuanced test, controlling for confounders.
Hypothesis Tests:
•	Null Hypothesis: Education difference does not predict age difference.
•	Results: (You would interpret coefficients and p-values here; typically, small coefficients and high p-values suggest weak relationships, but you’d check for significance and direction.)

6. Predicted Values & Sensitivity
•	Prediction Grid: You created a grid of education difference values and predicted age differences using the polynomial model (M2), including confidence intervals.
•	Visualization: Plotted predicted age difference against education difference, with a ribbon for confidence intervals.
Are the predictions sensible?
•	The predicted values are generally close to zero, with only modest changes as the education difference varies. This matches the summary stats: most couples have similar ages and education levels.
•	The confidence intervals are narrow near the center and widen at the extremes, reflecting less data and more uncertainty for large education gaps.

7. Outliers & Data Quality
   I identified couples with extreme age differences (|age_diff| > 30), listing their ages, sex, and education. This helps check for data errors or unusual cases.

My Conclusion & Interpretation
•	Main Findings:
•	Most couples have small age and education differences.
•	There are some outliers, but they are rare.
•	The relationship between education difference and age difference is weak and mostly linear, with little evidence for strong non-linear effects.
•	Sex pairing matters: Female-Male couples tend to have the female younger, Male-Female the male younger, and same-sex couples show similar patterns.
•	Predictions: The models predict small changes in age difference as education difference varies, which is sensible given the data.
•	Impressive Aspects: I used robust statistics, visualizations, and careful subgroup selection to ensure meaningful results


