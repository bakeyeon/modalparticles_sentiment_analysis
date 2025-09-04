library(languageR)
library(rms)
library(xtable)
library(MASS)
library(lme4)
library(visreg)
library(lattice)


# working directory
setwd("C:/Users/hyeye/Desktop/2023-24WS/Gesture")


d <- read.table("Modalpartikeln.csv", header=T, na.strings = c("NA"), sep=",", stringsAsFactors = T)

head(d)



# scores for each modal particles gained by type 1) and 2) questions.


assign_score <- function(word) {
  if (word == 1) {
    return(-2)
  } else if (word == 2) {
    return(-1)
  } else if (word == 3) {
    return(0)
  } else if (word == 4) {
    return(1)
  } else if (word == 5) {
    return(2)
  }
}

word_columns <- c("halt", "eben", "eigentlich", "wirklich", "mal", "ja", "ruhig", "nuneinmal", "doch", "schon", "halt_eben", "eigentlich_wirklich", "mal_ja", "ruhig_nuneinmal", "doch_schon")
for (word_col in word_columns) {
  d[[paste0(word_col, "_score")]] <- sapply(d[[word_col]], assign_score)
}


head(d)

#nr conf sweat conf_sweat broken grim broken_grim kisscl kissm
# 1 P01    2     2          2      2    2           4      5     4
# 2 P02    5     4          2      2    2           4      5     5
# 3 P03    3     2          2      2    2           2      3     3
# 4 P04    3     2          2      4    2           4      4     4
# 5 P05    2     2          4      2    2           5      4     4
# 6 P06    5     5          4      5    3           5      3     3
# kisscl_kissm imp fear imp_fear kiss twoh kiss_twoh sungl smile
# 1            4   2    1        5    3    4         1     5     2
# 2            2   4    2        4    4    5         2     5     5
# 3            2   2    2        3    3    2         2     3     2
# 4            4   1    2        4    4    4         4     3     3
# 5            3   2    2        4    4    5         2     4     4
# 6            3   5    5        2    4    5         5     5     4
# sungl_smile joycat disa joycat_disa fluency culturalbg name_cult gender
# 1           2      2    2           3       5         KR                F
# 2           5      2    1           5       5         KR                M
# 3           2      2    2           2       4         KR                M
# 4           3      2    2           4       1         KR                F
# 5           2      4    2           1       5         KR                M
# 6           5      5    1           5       4         KR                M
# age conf_score sweat_score conf_sweat_score broken_score grim_score
# 1 30s         -1          -1               -1           -1         -1
# 2 60a          2           1               -1           -1         -1
# 3 60a          0          -1               -1           -1         -1
# 4 60a          0          -1               -1            1         -1
# 5 40s         -1          -1                1           -1         -1
# 6 60a          2           2                1            2          0
# broken_grim_score kisscl_score kissm_score kisscl_kissm_score imp_score
# 1                 1            2           1                  1        -1
# 2                 1            2           2                 -1         1
# 3                -1            0           0                 -1        -1
# 4                 1            1           1                  1        -2
# 5                 2            1           1                  0        -1
# 6                 2            0           0                  0         2
# fear_score imp_fear_score kiss_score twoh_score kiss_twoh_score
# 1         -2              2          0          1              -2
# 2         -1              1          1          2              -1
# 3         -1              0          0         -1              -1
# 4         -1              1          1          1               1
# 5         -1              1          1          2              -1
# 6          2             -1          1          2               2
# sungl_score smile_score sungl_smile_score joycat_score disa_score
# 1           2          -1                -1           -1         -1
# 2           2           2                 2           -1         -2
# 3           0          -1                -1           -1         -1
# 4           0           0                 0           -1         -1
# 5           1           1                -1            1         -1
# 6           2           1                 2            2         -2
# joycat_disa_score
# 1                 0
# 2                 2
# 3                -1
# 4                 1
# 5                -2
# 6                 2

DE_raw <- d
DE_raw

# Calculate scores for t1 and t2.

calculate_score <- function(d) {
  score <- colSums(d[, c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
                         "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score")])
  return(score)
}

score <- calculate_score(d)
print(score)

# halt_score                   eben_score 
# -21                           -47 
# eigentlich_score              wirklich_score 
# -14                          -37 
# mal_score                  ja_score 
# 29                          11 
# ruhig_score                nuneinmal_score 
# 41                          -10 
# doch_score                 schon_score 
# -27                        -5 
# halt_eben_score             eigentlich_wirklich_score 
# -10                          -22 
# mal_ja_score               ruhig_nuneinmal_score 
# -9                          -2 
# doch_schon_score 
# 19 

#################################################################################################


#score by cultural backgrounds.

d$culturalbg

# Calculate scores for culturalbg = "germany"
calculate_score_DE <- function(d) {
  score <- colSums(d[d$culturalbg == "germany", c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
                                             "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score")])
  return(score)
}

# Calculate scores for culturalbg = "multi"
calculate_score_Multi <- function(d) {
  score <- colSums(d[d$culturalbg == "multi", c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
                                                "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score")])
  return(score)
}

# Calculate scores for culturalbg = "outofeu"
calculate_score_outofeu <- function(d) {
  score <- colSums(d[d$culturalbg == "outofeu", c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
                                               "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score")])
  return(score)
}

#number of participants in each group.
count_per_value <- table(d$culturalbg)
print(count_per_value)

# germany   multi outofeu 
# 21      10       4 

# Calculate scores for DE_raw
score_DE <- calculate_score_DE(DE_raw)
# halt_score                eben_score 
# -10                       -27 
# eigentlich_score            wirklich_score 
# -9                       -21 
# mal_score                  ja_score 
# 13                         6 
# ruhig_score           nuneinmal_score 
# 28                        -7 
# doch_score               schon_score 
# -22                        -2 
# halt_eben_score eigentlich_wirklich_score 
# -7                       -13 
# mal_ja_score     ruhig_nuneinmal_score 
# -3                         4 
# doch_schon_score 
# 15 


score_Multi <- calculate_score_Multi(DE_raw)
# halt_score                eben_score 
# -9                       -14 
# eigentlich_score            wirklich_score 
# -5                       -14 
# mal_score                  ja_score 
# 9                         2 
# ruhig_score           nuneinmal_score 
# 9                         0 
# doch_score               schon_score 
# -4                        -4 
# halt_eben_score eigentlich_wirklich_score 
# -4                        -6 
# mal_ja_score     ruhig_nuneinmal_score 
# -2                        -3 
# doch_schon_score 
# -1 


score_outofeu <- calculate_score_outofeu(DE_raw)
# halt_score                eben_score 
# -2                        -6 
# eigentlich_score            wirklich_score 
# 0                        -2 
# mal_score                  ja_score 
# 7                         3 
# ruhig_score           nuneinmal_score 
# 4                        -3 
# doch_score               schon_score 
# -1                         1 
# halt_eben_score eigentlich_wirklich_score 
# 1                        -3 
# mal_ja_score     ruhig_nuneinmal_score 
# -4                        -3 
# doch_schon_score 
# 5 


score_DE
score_Multi
score_outofeu


################################################################################################


# calculate means for each.

calculate_variable_means <- function(d) {
  variable_means <- colMeans(d[, c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
                                   "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score")])
  return(variable_means)
}

variable_means <- calculate_variable_means(d)
print(variable_means)

# halt_score                  eben_score 
# -0.60000000                 -1.34285714 
# eigentlich_score            wirklich_score 
# -0.40000000                 -1.05714286 
# mal_score                   ja_score 
# 0.82857143                  0.31428571 
# ruhig_score                 nuneinmal_score 
# 1.17142857                  -0.28571429 
# doch_score                  schon_score 
# -0.77142857                 -0.14285714 
# halt_eben_score             eigentlich_wirklich_score 
# -0.28571429                 -0.62857143 
# mal_ja_score                ruhig_nuneinmal_score 
# -0.25714286                 -0.05714286 
# doch_schon_score 
# 0.54285714 


#################################################################################################


#means by different cultural groups.


# Calculate scores for culturalbg = "germany"
calculate_variable_means_DE <- function(d) {
  score <- colMeans(d[d$culturalbg == "germany", c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
  
                                                                                                "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score")])
  return(score)
}

# Calculate scores for culturalbg = "multi"
calculate_variable_means_Multi <- function(d) {
  score <- colMeans(d[d$culturalbg == "multi", c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
                                                "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score")])
 
  return(score)
}

# Calculate scores for culturalbg = "outofeu"
calculate_variable_means_outofeu <- function(d) {
  score <- colMeans(d[d$culturalbg == "outofeu", c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
                                                  "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score")])
  return(score)
}

demeans <- calculate_variable_means_DE(DE_raw)
# halt_score                eben_score 
# -0.4761905                -1.2857143 
# eigentlich_score            wirklich_score 
# -0.4285714                -1.0000000 
# mal_score                  ja_score 
# 0.6190476                 0.2857143 
# ruhig_score           nuneinmal_score 
# 1.3333333                -0.3333333 
# doch_score               schon_score 
# -1.0476190                -0.0952381 
# halt_eben_score eigentlich_wirklich_score 
# -0.3333333                -0.6190476 
# mal_ja_score     ruhig_nuneinmal_score 
# -0.1428571                 0.1904762 
# doch_schon_score 
# 0.7142857 

multmeans <- calculate_variable_means_Multi(DE_raw)
# halt_score                eben_score 
# -0.9                      -1.4 
# eigentlich_score            wirklich_score 
# -0.5                      -1.4 
# mal_score                  ja_score 
# 0.9                       0.2 
# ruhig_score           nuneinmal_score 
# 0.9                       0.0 
# doch_score               schon_score 
# -0.4                      -0.4 
# halt_eben_score eigentlich_wirklich_score 
# -0.4                      -0.6 
# mal_ja_score     ruhig_nuneinmal_score 
# -0.2                      -0.3 
# doch_schon_score 
# -0.1 

oeumeans <- calculate_variable_means_outofeu(DE_raw)
# halt_score                eben_score 
# -0.50                     -1.50 
# eigentlich_score            wirklich_score 
# 0.00                     -0.50 
# mal_score                  ja_score 
# 1.75                      0.75 
# ruhig_score           nuneinmal_score 
# 1.00                     -0.75 
# doch_score               schon_score 
# -0.25                      0.25 
# halt_eben_score eigentlich_wirklich_score 
# 0.25                     -0.75 
# mal_ja_score     ruhig_nuneinmal_score 
# -1.00                     -0.75 
# doch_schon_score 
# 1.25 


###############################################################################################


relevant_columns <- c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", 
                      "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score", 
                      "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", 
                      "ruhig_nuneinmal_score", "doch_schon_score", "culturalbg")

relevant_data <- DE_raw[, relevant_columns]

convert_to_numeric <- function(x) {
  ifelse(x == "germany", 1,
         ifelse(x == "multi", 2,
                ifelse(x == "outofeu", 3, NA)))
}

relevant_data$culturalbg_numeric <- convert_to_numeric(relevant_data$culturalbg)
head(relevant_data)

relevant_data2 <- relevant_data[, c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", 
                            "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score", 
                            "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", 
                            "ruhig_nuneinmal_score", "doch_schon_score","culturalbg_numeric")]

correlation_matrix <- cor(relevant_data2)

correlation_with_culturalbg <- correlation_matrix["culturalbg_numeric", -length(correlation_matrix)]

p_values <- apply(relevant_data2[, -length(relevant_data)], 2, function(x) {
  cor_test <- cor.test(x, relevant_data2$culturalbg_numeric)
  return(cor_test$p.value)
})

correlation_results <- data.frame(Correlation = correlation_with_culturalbg, P_Value = p_values)
print(correlation_results)

#                           Correlation     P_Value
# halt_score                -0.09751493 0.577332434
# eben_score                -0.09552494 0.585162221
# eigentlich_score           0.16672135 0.338447184
# wirklich_score             0.04760239 0.785971304
# mal_score                  0.51070720 0.001719694 ***
# ja_score                   0.09604060 0.583128812
# ruhig_score               -0.17389389 0.317772357
# nuneinmal_score           -0.04020783 0.818615215
# doch_score                 0.45209709 0.006399838 ***
# schon_score                0.02723876 0.876566782
# halt_eben_score            0.18551040 0.286002773
# eigentlich_wirklich_score -0.03408610 0.845871491
# mal_ja_score              -0.26421178 0.125095909
# ruhig_nuneinmal_score     -0.26059794 0.130536479
# doch_schon_score          -0.04145202 0.813100023
# culturalbg_numeric         1.00000000 0.000000000 


# mal and doch has statistically significant correlations with cultural backgrounds. 
# Both variables show a positive correlation with cultural backgrounds, suggesting that cultural background may influence the values of "mal" and "doch".



##############################################################################################################################


#correlation test with fluency.

cor_test <- function(x, y) {
  result <- cor.test(x, y)
  return(c(result$estimate, result$p.value))
}

relevant_columns <- c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", 
                      "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score", 
                      "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", 
                      "ruhig_nuneinmal_score", "doch_schon_score")

correlation_matrix <- cor(DE_raw[, relevant_columns], method = "pearson")

correlation_with_fluency <- apply(DE_raw[, relevant_columns], 2, function(x) cor_test(x, DE_raw$kenntnisse))

correlation_results <- data.frame(Correlation = correlation_with_fluency[1, ],
                                  P_Value = correlation_with_fluency[2, ])

print(correlation_results)


#                             Correlation      P_Value
# halt_score                 0.143518558 0.4107912038
# eben_score                -0.139229905 0.4250577794
# eigentlich_score          -0.011684467 0.9468858736
# wirklich_score             0.187566147 0.2806018170
# mal_score                  0.037562947 0.8303675424
# ja_score                  -0.155302984 0.3730154098
# ruhig_score                0.598107594 0.0001478616 ***
# nuneinmal_score            0.082658920 0.6368795052
# doch_score                -0.001262795 0.9942557037
# schon_score                0.115494237 0.5088208484
# halt_eben_score           -0.169016564 0.3317435518
# eigentlich_wirklich_score  0.146717244 0.4003295259
# mal_ja_score               0.252441407 0.1434562675
# ruhig_nuneinmal_score      0.157696201 0.3656028902
# doch_schon_score           0.085216709 0.6264521628

# ruhig has statistically significant correlations with fluency.
# a higher “ruhig_score” suggests that the respondent’s fluency is higher.
# It can be interpreted that the higher the “fluency” (as “fluency” increases), the more positively the “ruhig_score” tends to be evaluated.





###################################################################################################


#make a subset including scores and participant backgrounds. 
#dsub <- d[, c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score","culturalbg", "kenntnisse", "gender", "age")]

dsub <- d[, c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", 
              "doch_score", "schon_score", "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score", "culturalbg", "kenntnisse", "gender", "age")]

head(dsub)

# halt_score eben_score eigentlich_score wirklich_score mal_score
# 1          0         -1               -1             -2         0
# 2          2         -1                0             -1         2
# 3         -1         -2                0             -2         0
# 4         -1         -2               -1             -1         0
# 5          0         -1                0             -1         1
# 6         -1         -1                0             -1         0
# ja_score ruhig_score nuneinmal_score doch_score schon_score
# 1        0          -2               0         -1          -1
# 2        2           2               1         -1           0
# 3        0           2               0         -1           0
# 4        0           1              -1         -1           0
# 5       -1           2              -1         -1           0
# 6        0           1               0          0          -1
# halt_eben_score eigentlich_wirklich_score mal_ja_score
# 1              -1                        -2            0
# 2              -1                        -1            0
# 3               1                         0            0
# 4              -1                         0            0
# 5               0                         0            0
# 6               0                        -1            0
# ruhig_nuneinmal_score doch_schon_score culturalbg kenntnisse gender age
# 1                    -2               -1      multi          2      m 20s
# 2                     0                0    germany          5      f 20s
# 3                     2                0    germany          5      f 20s
# 4                     1                1    germany          5      m 20s
# 5                     0                1    germany          5      m 20s
# 6                    -1                1      multi          5      f 20s


#compare if the difference is significant.

calculate_diff_score <- function(variable_means) {
  t1_diff <- c(
    diff_halt_eben_score = variable_means[1] - variable_means[2],
    diff_eigentlich_wirklich_score = variable_means[3] - variable_means[4],
    diff_mal_ja_score = variable_means[5] - variable_means[6],
    diff_ruhig_nuneinmal_score = variable_means[7] - variable_means[8],
    diff_doch_schon_score = variable_means[9] - variable_means[10]
  )
  
  return(t1_diff)
}

t1_diffscores <- calculate_diff_score(variable_means)

show(t1_diffscores)


# diff_halt_eben_score.halt_score diff_eigentlich_wirklich_score.eigentlich_score 
# 0.7428571                                       0.6571429 
# diff_mal_ja_score.mal_score          diff_ruhig_nuneinmal_score.ruhig_score 
# 0.5142857                                       1.4571429 
# diff_doch_schon_score.doch_score 
# -0.6285714 

t1_scores <-  variable_means[1:10]
t2_scores <-  variable_means[11:15]
show(t1_scores)
show(t2_scores)


# The difference between the score calculated by the t1 question and the score calculated by the t2 question.

diff_scores <- t2_scores - t1_diffscores

print(diff_scores)

# halt_eben_score           eigentlich_wirklich_score 
# -1.0285714                -1.2857143 
# mal_ja_score              ruhig_nuneinmal_score 
# -0.7714286                -1.5142857 
# doch_schon_score 
# 1.1714286 



#now the diffscores are divided into half. 

diff_scores <- diff_scores/2

show(diff_scores)

# halt_eben_score           eigentlich_wirklich_score 
# -0.5142857                -0.6428571 
# mal_ja_score               ruhig_nuneinmal_score 
# -0.3857143                -0.7571429 
# doch_schon_score 
# 0.5857143 

show(t1_scores)


# diff scores is added to the raw score of first half of a modal particle pair, subtracted from the raw score of other half.

adjusted_scores <- c(
  halt_score_adjusted = t1_scores[1] + (diff_scores[1]),
  eben_score_adjusted = t1_scores[2] - (diff_scores[1]),
  eigentlich_score_adjusted = t1_scores[3] + (diff_scores[2]),
  wirklich_score_adjusted = t1_scores[4] - (diff_scores[2]),
  mal_score_adjusted = t1_scores[5] + (diff_scores[3]),
  ja_score_adjusted = t1_scores[6] - (diff_scores[3]),
  ruhig_score_adjusted = t1_scores[7] + (diff_scores[4]),
  nuneinmal_score_adjusted = t1_scores[8] - (diff_scores[4]),
  doch_score_adjusted = t1_scores[9] + (diff_scores[5]),
  schon_score_adjusted = t1_scores[10] - (diff_scores[5])
)

print(adjusted_scores)


# halt_score_adjusted.halt_score 
# -1.1142857 
# eben_score_adjusted.eben_score 
# -0.8285714 
# eigentlich_score_adjusted.eigentlich_score 
# -1.0428571 
# wirklich_score_adjusted.wirklich_score 
# -0.4142857 
# mal_score_adjusted.mal_score 
# 0.4428571 
# ja_score_adjusted.ja_score 
# 0.7000000 
# ruhig_score_adjusted.ruhig_score 
# 0.4142857 
# nuneinmal_score_adjusted.nuneinmal_score 
# 0.4714286 
# doch_score_adjusted.doch_score 
# -0.1857143 
# schon_score_adjusted.schon_score 
# -0.7285714 


# normalization with -1 and 1. 

min_score <- -1
max_score <- 1

normalized_halt <- (adjusted_scores[1]   - min_score) / (max_score - min_score)
normalized_eben <- (adjusted_scores[2] - min_score) / (max_score - min_score)
normalized_eigentlich <- (adjusted_scores[3] - min_score) / (max_score - min_score)
normalized_wirklich <- (adjusted_scores[4] - min_score) / (max_score - min_score)
normalized_ruhig <- (adjusted_scores[7]  - min_score) / (max_score - min_score)
normalized_nuneinmal <- (adjusted_scores[8] - min_score) / (max_score - min_score)

names(normalized_halt) <- "normalized_halt"
names(normalized_eben) <- "normalized_eben"
names(normalized_eigentlich) <- "normalized_eigentlich"
names(normalized_wirklich) <- "normalized_wirklich"
names(normalized_ruhig) <- "normalized_ruhig"
names(normalized_nuneinmal) <- "normalized_nuneinmal"

normalized_scores <- c(normalized_halt, normalized_eben, normalized_eigentlich, normalized_wirklich, normalized_ruhig, normalized_nuneinmal)
print(normalized_scores)


# normalized_halt       normalized_eben       normalized_eigentlich 
# -0.05714286           0.08571429            -0.02142857 
# normalized_wirklich   normalized_ruhig      normalized_nuneinmal 
# 0.29285714            0.70714286            0.73571429 


#############################################################################################


# calculate the special case of ja and schon (positive and negative cases)

#-----------------------------------------positive-------------------------------------------

# positive case scores


dpos <- d


assign_score_positive <- function(word) {
  if (word == 1) {
    return(0)
  } else if (word == 2) {
    return(0)
  } else if (word == 3) {
    return(0)
  } else if (word == 4) {
    return(1)
  } else if (word == 5) {
    return(2)
  }
}



word_columns <- c("ja", "schon")
for (word_col in word_columns) { 
  dpos[[paste0(word_col, "_posscore")]] <- sapply(dpos[[word_col]], assign_score_positive)
}



calculate_pos_score <- function(dpos) {
  pos_score <- colSums(dpos[, c("ja_posscore", "schon_posscore")])
  return(pos_score)
}

pos_scores <- calculate_pos_score(dpos)

show(pos_scores)

# ja_posscore schon_posscore 
# 20              9 


show(dpos$ja_posscore)


#number of filtered data

filtered_data_ja_nonzero <- dpos[dpos$ja_posscore != 0, ]
unique(filtered_data_ja_nonzero$ja_posscore)
nrow(filtered_data_ja_nonzero)
# [1] 16

filtered_data_schon_nonzero <- dpos[dpos$schon_posscore != 0, ]
unique(filtered_data_schon_nonzero$schon_posscore)
nrow(filtered_data_schon_nonzero)
# [1] 9


# means for ja and schon.
ja_mean <- mean(dpos$ja_posscore)
schon_mean <- mean(dpos$schon_posscore)

show(ja_mean)
# [1] 0.5714286

show(schon_mean)
# [1] 0.2571429


# create a dataset for t1 scores for nonzero ja and schon.
mean_mal <- mean(as.numeric(filtered_data_ja_nonzero[["mal_score"]]))
mean_doch <- mean(as.numeric(filtered_data_schon_nonzero[["doch_score"]]))

t1_scores_pos <- c(mean_mal, mean_doch)
show(t1_scores_pos)
# [1]  1.0000000 -0.5555556

#  difference scores according to t1 questions.

pos_diff_ja_t1 <- mean_mal - ja_mean 
names(pos_diff_ja_t1) <- "pos_diff_ja_t1"

show(pos_diff_ja_t1)

# pos_diff_ja_t1 
# 0.4285714 

pos_diff_schon_t1 <- mean_doch - schon_mean
names(pos_diff_schon_t1) <- "pos_diff_schon_t1"

show(pos_diff_schon_t1)

# pos_diff_schon_t1 
# -0.8126984 

# percentage of difference according to t2 questions.

mean_mal_ja_score_pos <- mean(filtered_data_ja_nonzero$mal_ja_score)
print(mean_mal_ja_score_pos)
# [1] -0.3125

mean_doch_schon_score_pos <- mean(filtered_data_schon_nonzero $doch_schon_score)
print(mean_doch_schon_score_pos)
# [1] 0.6666667

# difference of t1 and t2 scores for positive case of ja and schon.
diff_scores_mj <- mean_mal_ja_score_pos  - pos_diff_ja_t1
diff_scores_ds <- mean_doch_schon_score_pos - pos_diff_schon_t1

show(diff_scores_mj)
# pos_diff_ja_t1 
# -0.7410714 

show(diff_scores_ds)
# pos_diff_schon_t1 
# 1.479365 

#divide into half for calculation.
diff_scores_mj <- diff_scores_mj/2
diff_scores_ds <- diff_scores_ds/2

show(diff_scores_mj)
# pos_diff_ja_t1 
# -0.3705357 

show(diff_scores_ds)
# pos_diff_schon_t1 
# 0.7396825 



# adjusted scores.
# mal/doch t1 + diffscores & ja/schon t1 - diffscores

adjusted_mal <- mean_mal + diff_scores_mj
adjusted_doch <- mean_doch + diff_scores_ds
adjusted_ja <- ja_mean - diff_scores_mj
adjusted_schon <- schon_mean - diff_scores_ds

pos_adjusted_scores <- c(adjusted_mal, adjusted_doch, adjusted_ja, adjusted_schon)

show(pos_adjusted_scores)

# pos_diff_ja_t1 pos_diff_schon_t1    pos_diff_ja_t1 pos_diff_schon_t1 
# 0.6294643         0.1841270         0.9419643        -0.4825397 



# normalization

min_score <- -1
max_score <- 1

normalized_mal <- (adjusted_mal - min_score) / (max_score - min_score)
normalized_ja <- (adjusted_ja - min_score) / (max_score - min_score)
names(normalized_mal) <- "pos_normalized_mal"
names(normalized_ja) <- "pos_normalized_ja"

normalized_doch <- (adjusted_doch - min_score) / (max_score - min_score)
normalized_schon <- (adjusted_schon - min_score) / (max_score - min_score)
names(normalized_doch) <- "pos_normalized_doch"
names(normalized_schon) <- "pos_normalized_schon"



normalized_pos <- c(normalized_mal, normalized_ja, normalized_doch, normalized_schon) 

print(normalized_pos)


# pos_normalized_mal    pos_normalized_ja  pos_normalized_doch 
# 0.8147321            0.9709821            0.5920635 
# pos_normalized_schon 
# 0.2587302 


############################################################################################################
#-----------------------------------------negative ja and schon-------------------------------------------


#negative case scores 



dneg <- d

show(dneg)

assign_score_negative <- function(word) {
  if (word == 1) {
    return(-2)
  } else if (word == 2) {
    return(-1)
  } else{
    return(0)
  }
}



word_columns <- c("ja", "schon")
for (word_col in word_columns) {
  dneg[[paste0(word_col, "_negscore")]] <- sapply(dneg[[word_col]], assign_score_negative)
}


show(dneg)


#calculate t1 scores.
calculate_neg_score <- function(dneg) {
  neg_score <- colSums(dneg[, c("ja_negscore", "schon_negscore")])

  return(neg_score)
}

neg_scores <- calculate_neg_score(dneg)

show(neg_scores)


# ja_negscore schon_negscore
# -9            -14



#number of filtered data

filtered_data_ja_neg_nonzero <- dneg[dneg$ja_negscore != 0, ]
show(filtered_data_ja_neg_nonzero)

unique(filtered_data_ja_neg_nonzero$ja_negscore)
nrow(filtered_data_ja_neg_nonzero)
# [1] 8

filtered_data_schon_neg_nonzero <- dneg[dneg$schon_negscore != 0, ]
show(filtered_data_schon_neg_nonzero)

unique(filtered_data_schon_neg_nonzero$schon_negscore)
nrow(filtered_data_schon_neg_nonzero)
# [1] 12



# means for the scores of ja and schon.
ja_mean_neg <- mean(dneg$ja_negscore)
schon_mean_neg <- mean(dneg$schon_negscore)

show(ja_mean_neg)
#[1] -0.2571429

show(schon_mean_neg)
# [1] -0.4


# create a subset for t1 scores for nonzero mal and doch.
mean_mal_neg <- mean(as.numeric(filtered_data_ja_neg_nonzero[["mal_score"]]))
mean_doch_neg <- mean(as.numeric(filtered_data_schon_neg_nonzero[["doch_score"]]))


t1_scores_neg <- c(mean_mal_neg, ja_mean_neg, mean_doch_neg, schon_mean_neg)
show(t1_scores_neg)

# [1]  1.0000000 -0.2571429 -1.0000000 -0.4000000

#  difference scores according to t1 questions.


neg_diff_ja_t1 <- t1_scores_neg[1] - t1_scores_neg[2]
names(neg_diff_ja_t1) <- "neg_diff_ja_t1"

show(neg_diff_ja_t1)
# neg_diff_ja_t1 
# 1.257143 

neg_diff_schon_t1 <- t1_scores_neg[3] - t1_scores_neg[4]
names(neg_diff_schon_t1) <- "neg_diff_schon_t1"

show(neg_diff_schon_t1)
# neg_diff_schon_t1 
# -0.6 



#calculate t2 scores for neg ja and schon.

mean_mal_ja_score_neg <- mean(filtered_data_ja_neg_nonzero$mal_ja_score)
print(mean_mal_ja_score_neg)

#[1] -0.75


mean_doch_schon_score_neg <- mean(filtered_data_schon_neg_nonzero$doch_schon_score)
print(mean_doch_schon_score_neg)

# [1] 0.5


# difference of t1 and t2 scores for neg case of ja and schon.
diff_scores_mj_neg <- mean_mal_ja_score_neg  - neg_diff_ja_t1
diff_scores_ds_neg <- mean_doch_schon_score_neg - neg_diff_schon_t1

show(diff_scores_mj_neg)
# neg_diff_ja_t1 
# -2.007143

show(diff_scores_ds_neg)
# neg_diff_schon_t1 
# 1.1 


# half for calculation.

diff_scores_mj_neg <- diff_scores_mj_neg/2
diff_scores_ds_neg <- diff_scores_ds_neg/2

show(diff_scores_mj_neg)
# neg_diff_ja_t1 
# -1.003571 

show(diff_scores_ds_neg)
# neg_diff_schon_t1 
# 0.55 


# adjusted scores.
# mal/doch t1 + diffscores & ja/schon t1 - diffscores

adjusted_mal_neg <- mean_mal_neg + diff_scores_mj_neg
adjusted_doch_neg <- mean_doch_neg + diff_scores_ds_neg
adjusted_ja_neg <- ja_mean_neg - diff_scores_mj_neg
adjusted_schon_neg <- schon_mean_neg - diff_scores_ds_neg

names(adjusted_mal_neg) <- "adjusted_mal_neg"
names(adjusted_doch_neg) <- "adjusted_doch_neg"
names(adjusted_ja_neg) <- "adjusted_ja_neg"
names(adjusted_schon_neg) <- "adjusted_schon_neg"

neg_adjusted_scores <- c(adjusted_mal_neg, adjusted_doch_neg, adjusted_ja_neg, adjusted_schon_neg)

show(neg_adjusted_scores)

# adjusted_mal_neg  adjusted_doch_neg    adjusted_ja_neg 
# -0.003571429       -0.450000000        0.746428571 
# adjusted_schon_neg 
# -0.950000000 

# normalization.

min_score <- -1
max_score <- 1

normalized_mal_neg <- (adjusted_mal_neg - min_score) / (max_score - min_score)
normalized_ja_neg <- (adjusted_ja_neg - min_score) / (max_score - min_score)
names(normalized_mal_neg) <- "neg_normalized_mal"
names(normalized_ja_neg) <- "neg_normalized_ja"

normalized_doch_neg <- (adjusted_doch_neg - min_score) / (max_score - min_score)
normalized_schon_neg <- (adjusted_schon_neg - min_score) / (max_score - min_score)
names(normalized_doch_neg) <- "neg_normalized_doch"
names(normalized_schon_neg) <- "neg_normalized_schon"



normalized_neg <- c(normalized_mal_neg, normalized_ja_neg, normalized_doch_neg, normalized_schon_neg) 

print(normalized_neg)


# neg_normalized_mal          neg_normalized_ja     neg_normalized_doch 
# 0.4982143                   0.8732143             0.2750000 
# neg_normalized_schon 
# 0.0250000 


# too positive values for neg cases.

#adjust mal-ja by subtracting 0.9.
normalized_mal_neg <- normalized_mal_neg - 0.9
normalized_ja_neg <- normalized_ja_neg - 0.9

#adjust doch-schon by subtracting 0.03.
normalized_doch_neg <-  normalized_doch_neg - 0.03
normalized_schon_neg <- normalized_schon_neg - 0.03

normalized_neg <- c(normalized_mal_neg, normalized_ja_neg, normalized_doch_neg, normalized_schon_neg) 

print(normalized_neg)

# neg_normalized_mal    neg_normalized_ja  neg_normalized_doch 
# -0.40178571          -0.02678571           0.24500000 
# neg_normalized_schon 
# -0.00500000 


# total scores. 
scores_total <- c(normalized_scores, normalized_pos, normalized_neg)

show(scores_total)

# normalized_halt           normalized_eben       normalized_eigentlich 
# -0.05714286               0.08571429            -0.02142857 
# normalized_wirklich       normalized_ruhig      normalized_nuneinmal 
# 0.29285714                0.70714286            0.73571429 
# pos_normalized_mal        pos_normalized_ja     pos_normalized_doch 
# 0.81473214                0.97098214            0.59206349 
# pos_normalized_schon      neg_normalized_mal    neg_normalized_ja 
# 0.25873016                -0.40178571           -0.02678571 
# neg_normalized_doch       neg_normalized_schon 
# 0.24500000                -0.00500000 



#subtract 0.1 additionally to adjust the scores as intended.
scores_total_adj_DE <- scores_total - 0.1


show(scores_total_adj_DE)

# normalized_halt           normalized_eben       normalized_eigentlich 
# -0.15714286               -0.01428571           -0.12142857 
# normalized_wirklich       normalized_ruhig      normalized_nuneinmal 
# 0.19285714                0.60714286            0.63571429 
# pos_normalized_mal        pos_normalized_ja     pos_normalized_doch 
# 0.71473214                0.87098214            0.49206349 
# pos_normalized_schon      neg_normalized_mal     neg_normalized_ja 
# 0.15873016                -0.50178571           -0.12678571 
# neg_normalized_doch       neg_normalized_schon 
# 0.14500000                -0.10500000 






# graph for modal particles except special pairs. 
variable_names_nscores <- c("halt", "eben", "eigentlich", "wirklich", "ruhig", "nuneinmal")
barplot(normalized_scores,
        names.arg = variable_names_nscores,
        main = "scores without special cases",
        xlab = "Variable",
        ylab = "Score")


# graph for positive cases. 
variable_names_pos <- c("pos_mal", "pos_ja", "pos_doch", "pos_schon")

barplot(normalized_pos,
        names.arg = variable_names_pos,
        main = "positive case scores",
        xlab = "Variable",
        ylab = "Score")



# graph for negative cases.
variable_names_neg <- c("neg_mal", "neg_ja", "neg_doch", "neg_schon")

barplot(normalized_neg,
        names.arg = variable_names_neg ,
        main = "negative case scores",
        xlab = "Variable",
        ylab = "Score")


# graph for total. 
variable_names_total <- c("halt", "eben", "eigentlich", "wirklich", "ruhig", "nuneinmal", "pos_mal", "pos_ja", "pos_doch", "pos_schon",
                          "neg_mal", "neg_ja", "neg_doch", "neg_schon")

barplot(scores_total,
        names.arg = variable_names_total,
        main = "scores total",
        xlab = "Variable",
        ylab = "Score")




library(ggplot2)

df_nscores <- data.frame(
  Variable = variable_names_nscores,
  Score = normalized_scores
)

df_pos <- data.frame(
  Variable = variable_names_pos,
  Score = normalized_pos
)

df_neg <- data.frame(
  Variable = variable_names_neg,
  Score = normalized_neg
)

df_total <- data.frame(
  Variable = variable_names_total,
  Score = scores_total
)

# graph for modal particles except special pairs. 
ggplot(df_nscores, aes(x = Variable, y = Score)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Scores Without Special Cases", x = "Variable", y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# graph for positive cases. 
ggplot(df_pos, aes(x = Variable, y = Score)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(title = "Positive Case Scores", x = "Variable", y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

# graph for negative cases.
ggplot(df_neg, aes(x = Variable, y = Score)) +
  geom_bar(stat = "identity", fill = "salmon") +
  labs(title = "Negative Case Scores", x = "Variable", y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# graph for total, which is not yet adjusted.  
ggplot(df_total, aes(x = Variable, y = Score)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  labs(title = "Total Scores", x = "Variable", y = "Score") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

show(scores_total_adj)

# normalized_halt       normalized_eben normalized_eigentlich 
# -0.15714286           -0.01428571           -0.12142857 
# normalized_wirklich      normalized_ruhig  normalized_nuneinmal 
# 0.19285714            0.60714286            0.63571429 
# pos_normalized_mal     pos_normalized_ja   pos_normalized_doch 
# 0.71473214            0.87098214            0.49206349 
# pos_normalized_schon    neg_normalized_mal     neg_normalized_ja 
# 0.15873016           -0.50178571           -0.12678571 
# neg_normalized_doch  neg_normalized_schon 
# 0.14500000           -0.10500000 

#graph for final scores(adjusted total)
variable_names_total_adj <- c(
  "normalized_halt", "normalized_eben", "normalized_eigentlich", 
  "normalized_wirklich", "normalized_ruhig", "normalized_nuneinmal", 
  "pos_normalized_mal", "pos_normalized_ja", "pos_normalized_doch", 
  "pos_normalized_schon", "neg_normalized_mal", "neg_normalized_ja", 
  "neg_normalized_doch", "neg_normalized_schon"
)

# dataframe for total scores.
df <- data.frame(
  Variable = variable_names_total_adj, 
  Score = scores_total_adj
)

# plot for final scores.
ggplot(df, aes(x = Score, y = Variable, fill = Variable)) +
  geom_bar(stat = "identity") +
  labs(title = "Final scores", x = "scores", y = "variables") +
  scale_fill_hue() +  
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0))





#########################################################################################

#responses by cultural backgrounds

dcul <- d[, c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
               "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score","culturalbg")]
dcul

# culturalbg groupping.
dcul <- split(dcul[, sapply(dcul, is.numeric)], dcul$culturalbg)

# means for culturalbg groups.
group_means <- lapply(dcul, function(x) colMeans(x, na.rm = TRUE))


# plot
par(mfrow=c(2,2))
colors <- c("skyblue", "orange", "green")
for (i in 1:length(group_means)) {
  barplot(group_means[[i]], main = paste("Average Scores for Modal Particles -", names(group_means)[i]),
          ylab = "Average Score", col = colors[i])
}

##############################################################################################################################



# names
variable_names <- colnames(dcul[[1]])  

# dataframe
dfcul <- data.frame(
  culturalbg = rep(names(group_means), each = ncol(dcul[[1]])),
  variable = rep(colnames(dcul[[1]]), times = length(group_means)),
  average_score = unlist(lapply(group_means, function(x) x)))



# plot
ggplot(dfcul, aes(x = variable, y = average_score, color = culturalbg, group = culturalbg)) +
  geom_line(size = 1.5) +  
  geom_point(shape = 21, size = 3, fill = "white") +  
  labs(title = "Average Scores for Modal Particles by Cultural Background", x = "Variables", y = "Average Score") +
  scale_color_brewer(palette = "Pastel1", name = "Cultural Background") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# ANOVA
library(car)
dfcul

df_anova <- dfcul  

show(df_anova$average_score)

anova_result <- aov(average_score ~ culturalbg, data = df_anova)
summary(anova_result)

#             Df Sum Sq Mean Sq F value Pr(>F)
# culturalbg   2  0.494  0.2471   0.413  0.664
# Residuals   42 25.112  0.5979               



##################################################################################################################################


d$name_cult[d$culturalbg == "multi"]

# [1] Southkorea   Vietnam      <NA>         Russia       Vietnam     
# [6] <NA>         Libanon      Morocco      Spain        Switcherland

d$name_cult[d$culturalbg == "outofeu"]

# [1] Southkorea    Turkey     China      Iran     



###################################################################################################################################





d <- read.table("KoreanSurvey.csv", header=T, na.strings = c("NA"), sep=",", stringsAsFactors = T, fileEncoding = "UTF-8")

head(d)

# scores for each emojis gained by type 1) and 2) questions.


assign_score <- function(word) {
  if (is.na(word)) { 
    return(NA)}
  if (word == 1) {
    return(-2)
  } else if (word == 2) {
    return(-1)
  } else if (word == 3) {
    return(0)
  } else if (word == 4) {
    return(1)
  } else if (word == 5) {
    return(2)
  }
}



# Apply the assign_score function to each column of interest

word_columns <- c("conf", "sweat", "conf_sweat", "broken", "grim", "broken_grim", "kisscl", "kissm", "kisscl_kissm", 
                  "imp", "fear", "imp_fear", "kiss", "twoh", "kiss_twoh", "sungl", "smile", "sungl_smile", "joycat", "disa", "joycat_disa")

for (word_col in word_columns) {
  d[[paste0(word_col, "_score")]] <- sapply(d[[word_col]], assign_score)
}

head(d)

word_columns_with_na <- c()
for (word_col in word_columns) {
  if (any(is.na(d[[word_col]]))) {
    word_columns_with_na <- c(word_columns_with_na, word_col)
  }
}
print(word_columns_with_na)

# [1] "imp"       "imp_fear"  "kiss_twoh"


participants_with_na <- d$nr[apply(d, 1, function(row) any(is.na(row)))]
print(participants_with_na)

# [1] P54  P99  P100 P104 
# Four participant responses are excluded.


# subset without NA.

dsub <- d[complete.cases(d), ]

KR_raw <- dsub




dsub
length(unique(dsub$nr))
#[1] 112
# n = 112


# Calculate scores for t1 and t2.

calculate_score <- function(dsub) {
  score <- colSums(dsub[, c("conf_score", "sweat_score", "conf_sweat_score", "broken_score", "grim_score", "broken_grim_score", "kisscl_score", "kissm_score", "kisscl_kissm_score", 
                            "imp_score", "fear_score", "imp_fear_score", "kiss_score", "twoh_score", "kiss_twoh_score", "sungl_score", "smile_score", "sungl_smile_score", "joycat_score", "disa_score", "joycat_disa_score")])
  return(score)
}

score <- calculate_score(dsub)
print(score)


# conf_score        sweat_score   conf_sweat_score 
# -52                -74                 -9 
# broken_score         grim_score  broken_grim_score 
# -46                -43                 42 
# kisscl_score        kissm_score kisscl_kissm_score 
# 83                 80                 41 
# imp_score         fear_score     imp_fear_score 
# -53                -53                 52 
# kiss_score         twoh_score    kiss_twoh_score 
# 49                 88                -16 
# sungl_score        smile_score  sungl_smile_score 
# 37                 25                  8 
# joycat_score         disa_score  joycat_disa_score 
# 5                -43                 20 


# calculate means for each.


calculate_variable_means <- function(dsub) {
  variable_means <- colMeans(dsub[, c("conf_score", "sweat_score", "conf_sweat_score", "broken_score", "grim_score", "broken_grim_score", "kisscl_score", "kissm_score", "kisscl_kissm_score", 
                                      "imp_score", "fear_score", "imp_fear_score", "kiss_score", "twoh_score", "kiss_twoh_score", "sungl_score", "smile_score", "sungl_smile_score", "joycat_score", "disa_score", "joycat_disa_score")])
  return(variable_means)
}

variable_means <- calculate_variable_means(dsub)
print(variable_means)


# conf_score        sweat_score   conf_sweat_score 
# -0.46428571        -0.66071429        -0.08035714 
# broken_score         grim_score  broken_grim_score 
# -0.41071429        -0.38392857         0.37500000 
# kisscl_score        kissm_score kisscl_kissm_score 
# 0.74107143         0.71428571         0.36607143 
# imp_score         fear_score     imp_fear_score 
# -0.47321429        -0.47321429         0.46428571 
# kiss_score         twoh_score    kiss_twoh_score 
# 0.43750000         0.78571429        -0.14285714 
# sungl_score        smile_score  sungl_smile_score 
# 0.33035714         0.22321429         0.07142857 
# joycat_score         disa_score  joycat_disa_score 
# 0.04464286        -0.38392857         0.17857143 

head(dsub)


#make a subset including only scores and participant backgrounds. 

dscore <- dsub[,c("conf_score", "sweat_score", "conf_sweat_score", "broken_score", "grim_score", "broken_grim_score", "kisscl_score", "kissm_score", "kisscl_kissm_score", 
                  "imp_score", "fear_score", "imp_fear_score", "kiss_score", "twoh_score", "kiss_twoh_score", "sungl_score", "smile_score", "sungl_smile_score", "joycat_score", "disa_score", "joycat_disa_score",
                  "culturalbg", "fluency", "gender", "age")]

head(dscore)


#compare if the difference is significant.

calculate_diff_score <- function(variable_means) {
  t1_diff <- c(
    diff_conf_sweat_score =  variable_means["conf_score"] -  variable_means["sweat_score"], 
    diff_broken_grim_score = variable_means["broken_score"] - variable_means["grim_score"],
    diff_kisscl_kissm_score = variable_means["kisscl_score"] - variable_means["kissm_score"],
    diff_imp_fear_score = variable_means["imp_score"] - variable_means["fear_score"],
    diff_kiss_twoh_score = variable_means["kiss_score"] - variable_means["twoh_score"],
    diff_sungl_smile_score = variable_means["sungl_score"] - variable_means["smile_score"],
    diff_joycat_disa_score = variable_means["joycat_score"] - variable_means["disa_score"]
  )
  
  return(t1_diff)
}

t1_diffscores <- calculate_diff_score(variable_means)

show(t1_diffscores)

# diff_conf_sweat_score.conf_score            diff_broken_grim_score.broken_score 
# 0.19642857                                  -0.02678571 
# diff_kisscl_kissm_score.kisscl_score        diff_imp_fear_score.imp_score 
# 0.02678571                                  0.00000000 
# diff_kiss_twoh_score.kiss_score             diff_sungl_smile_score.sungl_score 
# -0.34821429                                 0.10714286 
# diff_joycat_disa_score.joycat_score 
# 0.42857143 




t1_scores <- c(
  variable_means["conf_score"], variable_means["sweat_score"],
  variable_means["broken_score"], variable_means["grim_score"],
  variable_means["kisscl_score"], variable_means["kissm_score"],
  variable_means["imp_score"], variable_means["fear_score"],
  variable_means["kiss_score"], variable_means["twoh_score"],
  variable_means["sungl_score"], variable_means["smile_score"],
  variable_means["joycat_score"], variable_means["disa_score"]
)

t2_scores <- c(
  variable_means["conf_sweat_score"], variable_means["broken_grim_score"],
  variable_means["kisscl_kissm_score"], variable_means["imp_fear_score"],
  variable_means["kiss_twoh_score"], variable_means["sungl_smile_score"],
  variable_means["joycat_disa_score"]
)


#save scores for t1 and t2 questions.

show(t1_scores)

# conf_score  sweat_score broken_score   grim_score kisscl_score 
# -0.46428571  -0.66071429  -0.41071429  -0.38392857   0.74107143 
# kissm_score    imp_score   fear_score   kiss_score   twoh_score 
# 0.71428571  -0.47321429  -0.47321429   0.43750000   0.78571429 
# sungl_score  smile_score joycat_score   disa_score 
# 0.33035714   0.22321429   0.04464286  -0.38392857 


show(t2_scores)

# conf_sweat_score  broken_grim_score kisscl_kissm_score 
# -0.08035714         0.37500000         0.36607143 
# imp_fear_score    kiss_twoh_score  sungl_smile_score 
# 0.46428571        -0.14285714         0.07142857 
# joycat_disa_score 
# 0.17857143 


diff_scores <- t2_scores - t1_diffscores

print(diff_scores)

# conf_sweat_score  broken_grim_score kisscl_kissm_score 
# -0.27678571         0.40178571         0.33928571 
# imp_fear_score    kiss_twoh_score  sungl_smile_score 
# 0.46428571         0.20535714        -0.03571429 
# joycat_disa_score 
# -0.25000000 


#now the diffscores are divided into half. 

diff_scores <- diff_scores/2


show(diff_scores)

# conf_sweat_score  broken_grim_score kisscl_kissm_score 
# -0.13839286         0.20089286         0.16964286 
# imp_fear_score    kiss_twoh_score  sungl_smile_score 
# 0.23214286         0.10267857        -0.01785714 
# joycat_disa_score 
# -0.12500000

show(t1_scores)


# diff scores is added to the raw score of first half of a modal particle pair, subtracted from the raw score of other half.

adjusted_scores <- c(
  conf_score_adjusted = t1_scores["conf_score"] + diff_scores["conf_sweat_score"],
  sweat_score_adjusted = t1_scores["sweat_score"] - diff_scores["conf_sweat_score"],
  broken_score_adjusted = t1_scores["broken_score"] + diff_scores["broken_grim_score"],
  grim_score_adjusted = t1_scores["grim_score"] - diff_scores["broken_grim_score"],
  kisscl_score_adjusted = t1_scores["kisscl_score"] + diff_scores["kisscl_kissm_score"],
  kissm_score_adjusted = t1_scores["kissm_score"] - diff_scores["kisscl_kissm_score"],
  imp_score_adjusted = t1_scores["imp_score"] + diff_scores["imp_fear_score"],
  fear_score_adjusted = t1_scores["fear_score"] - diff_scores["imp_fear_score"],
  kiss_score_adjusted = t1_scores["kiss_score"] + diff_scores["kiss_twoh_score"],
  twoh_score_adjusted = t1_scores["twoh_score"] - diff_scores["kiss_twoh_score"],
  sungl_score_adjusted = t1_scores["sungl_score"] + diff_scores["sungl_smile_score"],
  smile_score_adjusted = t1_scores["smile_score"] - diff_scores["sungl_smile_score"],
  joycat_score_adjusted = t1_scores["joycat_score"] + diff_scores["joycat_disa_score"],
  disa_score_adjusted = t1_scores["disa_score"] - diff_scores["joycat_disa_score"])

print(adjusted_scores)


# conf_score_adjusted.conf_score   sweat_score_adjusted.sweat_score 
# -0.60267857                        -0.52232143 
# broken_score_adjusted.broken_score     grim_score_adjusted.grim_score 
# -0.20982143                        -0.58482143 
# kisscl_score_adjusted.kisscl_score   kissm_score_adjusted.kissm_score 
# 0.91071429                         0.54464286 
# imp_score_adjusted.imp_score     fear_score_adjusted.fear_score 
# -0.24107143                        -0.70535714 
# kiss_score_adjusted.kiss_score     twoh_score_adjusted.twoh_score 
# 0.54017857                         0.68303571 
# sungl_score_adjusted.sungl_score   smile_score_adjusted.smile_score 
# 0.31250000                         0.24107143 
# joycat_score_adjusted.joycat_score     disa_score_adjusted.disa_score 
# -0.08035714                        -0.25892857 


# normalize using mean and sd.
mean_score <- mean(adjusted_scores)
sd_score <- sd(adjusted_scores)

normalized_scores_2 <- (adjusted_scores - mean_score) / sd_score

names(normalized_scores_2) <- c("conf_score", "sweat_score", "broken_score", "grim_score", 
                                "kisscl_score", "kissm_score", "imp_score", "fear_score",
                                "kiss_score", "twoh_score", "sungl_score", "smile_score",
                                "joycat_score", "disa_score")

print(normalized_scores_2)


#normalize using minmax.
min_score <- min(adjusted_scores)
max_score <- max(adjusted_scores)

normalized_scores_mm <- (adjusted_scores - min_score) / (max_score - min_score) * 2 - 1

names(normalized_scores_mm) <- c("conf_score", "sweat_score", "broken_score", "grim_score", 
                                 "kisscl_score", "kissm_score", "imp_score", "fear_score",
                                 "kiss_score", "twoh_score", "sungl_score", "smile_score",
                                 "joycat_score", "disa_score")

print(normalized_scores_mm)

# conf_score  sweat_score broken_score   grim_score     kisscl_score 
# -0.8729282   -0.7734807   -0.3867403   -0.8508287     1.0000000 
# kissm_score    imp_score   fear_score   kiss_score    twoh_score 
# 0.5469613   -0.4254144   -1.0000000    0.5414365      0.7182320 
# sungl_score  smile_score joycat_score   disa_score 
# 0.2596685    0.1712707   -0.2265193   -0.4475138 



# normalization with -1 and 1. 

min_score <- -1
max_score <- 1

normalized_conf <- (adjusted_scores[1]   - min_score) / (max_score - min_score)
normalized_sweat <- (adjusted_scores[2] - min_score) / (max_score - min_score)
normalized_broken <- (adjusted_scores[3] - min_score) / (max_score - min_score)
normalized_grim <- (adjusted_scores[4] - min_score) / (max_score - min_score)
normalized_kisscl <- (adjusted_scores[5]  - min_score) / (max_score - min_score)
normalized_kissm <- (adjusted_scores[6] - min_score) / (max_score - min_score)

normalized_imp <- (adjusted_scores[7]   - min_score) / (max_score - min_score)
normalized_fear <- (adjusted_scores[8] - min_score) / (max_score - min_score)
normalized_kiss <- (adjusted_scores[9] - min_score) / (max_score - min_score)
normalized_twoh <- (adjusted_scores[10] - min_score) / (max_score - min_score)
normalized_sungl <- (adjusted_scores[11]  - min_score) / (max_score - min_score)
normalized_smile <- (adjusted_scores[12] - min_score) / (max_score - min_score)

normalized_joycat <- (adjusted_scores[13]   - min_score) / (max_score - min_score)
normalized_disa <- (adjusted_scores[14] - min_score) / (max_score - min_score)


normalized_scores <- c(normalized_conf, normalized_sweat, normalized_broken, normalized_grim, 
                       normalized_kisscl, normalized_kissm, normalized_imp, normalized_fear,
                       normalized_kiss, normalized_twoh, normalized_sungl, normalized_smile,
                       normalized_joycat, normalized_disa)


names(normalized_scores) <- c("conf_score", "sweat_score", "broken_score", "grim_score", 
                              "kisscl_score", "kissm_score", "imp_score", "fear_score",
                              "kiss_score", "twoh_score", "sungl_score", "smile_score",
                              "joycat_score", "disa_score")


print(normalized_scores)

# conf_score    sweat_score   broken_score    grim_score kisscl_score 
# 0.1986607     0.2388393     0.3950893       0.2075893    0.9553571 
# kissm_score   imp_score     fear_score      kiss_score   twoh_score 
# 0.7723214     0.3794643     0.1473214       0.7700893    0.8415179 
# sungl_score   smile_score   joycat_score    disa_score 
# 0.6562500     0.6205357     0.4598214       0.3705357 



# # subtract 0.1 additionally to adjust the scores as modal particles did.
# scores_total_adj_KR <- normalized_scores - 0.1
# 
# 
# show(scores_total_adj_KR)
# conf_score  sweat_score broken_score   grim_score     kisscl_score
# 0.09866071   0.13883929   0.29508929   0.10758929     0.85535714
# kissm_score    imp_score   fear_score   kiss_score    twoh_score
# 0.67232143   0.27946429   0.04732143   0.67008929     0.74151786
# sungl_score  smile_score joycat_score   disa_score
# 0.55625000   0.52053571   0.35982143   0.27053571


scores_total_adj_DE

# normalized_halt       normalized_eben normalized_eigentlich 
# -0.15714286           -0.01428571           -0.12142857 
# normalized_wirklich      normalized_ruhig  normalized_nuneinmal 
# 0.19285714            0.60714286            0.63571429 
# pos_normalized_mal     pos_normalized_ja   pos_normalized_doch 
# 0.71473214            0.87098214            0.49206349 
# pos_normalized_schon    neg_normalized_mal     neg_normalized_ja 
# 0.15873016           -0.50178571           -0.12678571 
# neg_normalized_doch  neg_normalized_schon 
# 0.14500000           -0.10500000 





# adjust the values to match the modal particles score with emojis actual score. 

# There was score differences between the used emojis and modal particles, 
# so to reflect this, the score difference between modal particle and emoji was subtracted from the Korean emoji survey results.

# (KR scores) - (emojis sentiment ranking scores - modal particles scores)

conf_score <- as.numeric(normalized_scores["conf_score"]) - (-0.155 - as.numeric(scores_total_adj_DE["normalized_halt"]))
sweat_score <-  as.numeric(normalized_scores["sweat_score"]) - (-0.02 -  as.numeric(scores_total_adj_DE["normalized_eben"]))
broken_score <-  as.numeric(normalized_scores["broken_score"]) - (-0.121 -  as.numeric(scores_total_adj_DE["normalized_eigentlich"])) 
grim_score <-  as.numeric(normalized_scores["grim_score"]) - (0.194 -  as.numeric(scores_total_adj_DE["normalized_wirklich"]))
kiss_score <-  as.numeric(normalized_scores["kiss_score"]) - (0.611 -  as.numeric(scores_total_adj_DE["normalized_ruhig"]))   
twoh_score <-  as.numeric(normalized_scores["twoh_score"]) - (0.632 -  as.numeric(scores_total_adj_DE["normalized_nuneinmal"]))
kisscl_score <-  as.numeric(normalized_scores["kisscl_score"]) - (0.71 -  as.numeric(scores_total_adj_DE["pos_normalized_mal"]))
kissm_score <-  as.numeric(normalized_scores["kissm_score"]) - (0.778 -  as.numeric(scores_total_adj_DE["pos_normalized_ja"]))
sungl_score <-  as.numeric(normalized_scores["sungl_score"]) - (0.491 -  as.numeric(scores_total_adj_DE["pos_normalized_doch"]))  
smile_score <-  as.numeric(normalized_scores["smile_score"]) - (0.178 -  as.numeric(scores_total_adj_DE["pos_normalized_schon"]))
imp_score <-  as.numeric(normalized_scores["imp_score"]) - (-0.534 -  as.numeric(scores_total_adj_DE["neg_normalized_ja"]))   
fear_score <-  as.numeric(normalized_scores["fear_score"])  - (-0.14 -  as.numeric(scores_total_adj_DE["neg_normalized_ja"]))
joycat_score <-  as.numeric(normalized_scores["joycat_score"]) - (0.141 -  as.numeric(scores_total_adj_DE["neg_normalized_doch"]))   
disa_score <-  as.numeric(normalized_scores["disa_score"]) - (-0.118 -  as.numeric(scores_total_adj_DE["neg_normalized_schon"]))


scores_total_adj_KR <- c(conf_score, sweat_score, broken_score, grim_score, kiss_score,
                         twoh_score, kisscl_score, kissm_score, sungl_score, smile_score,
                         imp_score, fear_score, joycat_score, disa_score)

names(scores_total_adj_KR) <- c("conf_score", "sweat_score", "broken_score", "grim_score",
                                "kiss_score", "twoh_score", "kisscl_score", "kissm_score",
                                "sungl_score", "smile_score", "imp_score", "fear_score",
                                "joycat_score", "disa_score")


print(scores_total_adj_KR)

# conf_score  sweat_score broken_score   grim_score   kiss_score 
# 0.1965179    0.2445536    0.3946607    0.2064464    0.7662321 
# twoh_score kisscl_score  kissm_score  sungl_score  smile_score 
# 0.8452321    0.9600893    0.8653036    0.6573135    0.6012659 
# imp_score   fear_score joycat_score   disa_score 
# 0.7866786    0.1605357    0.4638214    0.3835357 

 # rearrange for comparison.
 krscore <- scores_total_adj_KR[c("conf_score",  "sweat_score",
                                         "broken_score",   "grim_score",
                                         "kiss_score",   "twoh_score",
                                  "kisscl_score", "kissm_score",
                                         "sungl_score",  "smile_score",
                                 
                                         "imp_score",   "fear_score",
                                         "joycat_score",   "disa_score")]






# compare the final scores for DE and KR.

 
descore <- scores_total_adj_DE

krscore
descore
 
library(proxy)

# cosine similarity

calculate_cosine_similarity <- function(vector1, vector2) {
  magnitude_vector1 <- sqrt(sum(vector1^2))
  magnitude_vector2 <- sqrt(sum(vector2^2))
  
  dot_product <- sum(vector1 * vector2)
  
  cosine_similarity <- dot_product / (magnitude_vector1 * magnitude_vector2)
  
  return(cosine_similarity)
}

descore_vector <- descore 
krscore_vector <- krscore


cosine_similarity <- calculate_cosine_similarity(descore_vector, krscore_vector)
print(cosine_similarity)
 
# [1] 0.6586884

 
# t-test
t_test_result <- t.test(descore_vector, krscore_vector)
print(t_test_result)


# Welch Two Sample t-test
# 
# data:  descore_vector and krscore_vector
# t = -2.5756, df = 23.109, p-value = 0.01687
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   -0.61061339 -0.06672845
# sample estimates:
#   mean of x mean of y 
# 0.1993424 0.5380133 





library(ggplot2)


descore
krscore

variable_names <- c("halt", "eben", "eigentlich", "wirklich", "ruhig", "nuneinmal", 
                    "pos_mal", "pos_ja", "pos_doch", "pos_schon",
                    "neg_mal", "neg_ja", "neg_doch", "neg_schon")


df <- data.frame(
  Variable = rep(variable_names, 2),
  Score = c(krscore, unlist(descore))
)


df$Dataset <- c(rep("krscore", length(krscore)), rep("descore", length(unlist(descore))))


ggplot(df, aes(x = Variable, y = Score, color = Dataset, group = Dataset)) +
  geom_line(size = 1.5) +                
  geom_point(size = 3) +                  
  labs(title = "Comparison of Scores", x = "Variable", y = "Score") +
  scale_color_manual(values = c("skyblue", "pink")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = "top")       




KR_raw

# correlations wirh cultural backgrounds.

KR_raw$culturalbg
# Levels: Asia EU KR Multi

########################################################################################################


#number of participants in each group.
count_per_value <- table(KR_raw$culturalbg)
print(count_per_value)

# Asia    EU    KR      Multi 
# 4       4     101     3 

relevant_columns <- c("conf_score" ,   #halt   
                      "sweat_score",    #eben 
                      
                      "broken_score" ,   #eigentlich
                      "grim_score",    #wirklich    
                      
                      "kiss_score" ,  #ruhig
                      "twoh_score",        #nuneinmal
                      
                      "kisscl_score"  , #malpos     
                      "kissm_score" ,       #japos
                      "sungl_score" ,       #dochpos
                      "smile_score"  ,    #schonpos
                      
                      "imp_score"  ,  #malneg      
                      "fear_score"  ,      #janeg
                      "joycat_score" ,    #dochneg
                      "disa_score"  ,     #schonneg
                      
                      "conf_sweat_score", #halt-eben
                      "broken_grim_score", #eig-wirk
                      "kiss_twoh_score", #ruh-ne
                      "kisscl_kissm_score", #mal-ja pos
                      "sungl_smile_score", #doch-schon pos
                      "imp_fear_score",  #mal- ja neg
                      "joycat_disa_score", #doch-schon neg
                      
                      "culturalbg")

relevant_data <- KR_raw[, relevant_columns]

# culturalbg to number, 1 = KR, 2 = Asia without KR, 3 = Multi, 4 = EU
convert_to_numeric <- function(x) {
  ifelse(x == "KR", 1,
         ifelse(x == "Asia", 2,
                ifelse(x == "Multi", 3,
                  ifelse(x == "EU", 4, NA))))
}

relevant_data$culturalbg_numeric <- convert_to_numeric(relevant_data$culturalbg)
head(relevant_data)

relevant_data2 <- relevant_data[, c("conf_score" ,   #halt   
                                    "sweat_score",    #eben 
                                    
                                    "broken_score" ,   #eigentlich
                                    "grim_score",    #wirklich    
                                    
                                    "kiss_score" ,  #ruhig
                                    "twoh_score",        #nuneinmal
                                    
                                    "kisscl_score"  , #malpos     
                                    "kissm_score" ,       #japos
                                    "sungl_score" ,       #dochpos
                                    "smile_score"  ,    #schonpos
                                    
                                    "imp_score"  ,  #malneg      
                                    "fear_score"  ,      #janeg
                                    "joycat_score" ,    #dochneg
                                    "disa_score"  ,     #schonneg
                                    
                                    "conf_sweat_score", #halt-eben
                                    "broken_grim_score", #eig-wirk
                                    "kiss_twoh_score", #ruh-ne
                                    "kisscl_kissm_score", #mal-ja pos
                                    "sungl_smile_score", #doch-schon pos
                                    "imp_fear_score",  #mal- ja neg
                                    "joycat_disa_score", #doch-schon neg
                                    
                                    "culturalbg_numeric")]

correlation_matrix <- cor(relevant_data2)

correlation_with_culturalbg <- correlation_matrix["culturalbg_numeric", -length(correlation_matrix)]

p_values <- apply(relevant_data2[, -length(relevant_data)], 2, function(x) {
  cor_test <- cor.test(x, relevant_data2$culturalbg_numeric)
  return(cor_test$p.value)
})

correlation_results <- data.frame(Correlation = correlation_with_culturalbg, P_Value = p_values)
print(correlation_results)


#                     Correlation     P_Value
# conf_score          0.17935023 0.058471832
# sweat_score         0.20153393 0.033103783 **
# broken_score        0.17154906 0.070517889
# grim_score          0.15629654 0.099833534
# kiss_score         -0.13830957 0.145860508
# twoh_score         -0.06796489 0.476447710
# kisscl_score       -0.24858333 0.008220749 ***
# kissm_score        -0.11091648 0.244319668
# sungl_score        -0.09424055 0.322975734
# smile_score         0.04945173 0.604602158
# imp_score           0.11957037 0.209214489
# fear_score          0.25666034 0.006301161 ***
# joycat_score        0.08280487 0.385402396
# disa_score          0.30680531 0.001001078 ***
# conf_sweat_score    0.02061272 0.829205877
# broken_grim_score   0.03186112 0.738765643
# kiss_twoh_score     0.13852309 0.145233300
# kisscl_kissm_score -0.11969716 0.208728448
# sungl_smile_score   0.03000204 0.753505518
# imp_fear_score     -0.08022148 0.400450849
# joycat_disa_score   0.24187451 0.010190069 **
# culturalbg_numeric  1.00000000 0.000000000


# Asia    EU    KR      Multi 
# 4       4     101     3 



############################################################################################################################







DE_raw
KR_raw






# adjust KR_raw dataset. 
KR_adj <- KR_raw[c("conf_score", "sweat_score", 
             "broken_score",   "grim_score", 
             "kiss_score",   "twoh_score",
             
             #mal-ja t1   
             "kisscl_score", "kissm_score", #positive ja  
             "sungl_score",  "smile_score", #negative ja
             
             #doch-schon t1   
             "imp_score",   "fear_score", #positive schon    
             "joycat_score",   "disa_score", #negative schon 
             
             
             "conf_sweat_score", 
             "broken_grim_score", 
             "kiss_twoh_score",
             
             #mal-ja t2
             "kisscl_kissm_score",
             "sungl_smile_score",
             
             #doch-schon t2
             "imp_fear_score",
             "joycat_disa_score",
             
             "fluency",
             "culturalbg",
             "name_cult",
             "gender",
             "age")]

# for pos+neg cases average
KR_adj$mal_score <- (KR_raw$kisscl_score + KR_raw$sungl_score)/2
KR_adj$ja_score <- (KR_raw$kissm_score + KR_raw$smile_score)/2
KR_adj$mal_ja_score <- (KR_raw$kisscl_kissm_score + KR_raw$sungl_smile_score)/2

KR_adj$doch_score <- (KR_raw$imp_score + KR_raw$joycat_score)/2 
KR_adj$schon_score <- (KR_raw$fear_score + KR_raw$disa_score)/2
KR_adj$doch_schon_score <- (KR_raw$imp_fear_score + KR_raw$joycat_disa_score)/2


DE_adj <- DE_raw[c("halt_score", "eben_score",                          
                   "eigentlich_score", "wirklich_score",
                   "ruhig_score", "nuneinmal_score",
                   
                   # special cases t1
                   "mal_score",
                   "ja_score",
                   
                   "doch_score", "schon_score",
                   
                   "halt_eben_score", 
                   "eigentlich_wirklich_score", 
                   "ruhig_nuneinmal_score", 
                   
                   # special cases t2
                   "mal_ja_score",
                   
                   "doch_schon_score",
                   
                   "kenntnisse",
                   "culturalbg",          
                   "name_cult",          
                   "gender",              
                   "age")]

#pos ja - mal ja
DE_adj$mal_posscore <- ifelse(DE_raw$ja_score == 0 | DE_raw$ja_score == -1 | DE_raw$ja_score == -2, 0, DE_raw$mal_score)
DE_adj$ja_posscore <- ifelse(DE_raw$ja_score == 0 | DE_raw$ja_score == -1 | DE_raw$ja_score == -2, 0, DE_raw$ja_score)
DE_adj$mal_ja_posscore <- ifelse(DE_raw$ja_score == 0 | DE_raw$ja_score == -1 | DE_raw$ja_score == -2, 0, DE_raw$mal_ja_score)

#neg ja - mal ja
DE_adj$mal_negscore <- ifelse(DE_raw$ja_score == 0 | DE_raw$ja_score == 1 | DE_raw$ja_score == 2, 0, DE_raw$mal_score)
DE_adj$ja_negscore <- ifelse(DE_raw$ja_score == 0 | DE_raw$ja_score == 1 | DE_raw$ja_score == 2, 0, DE_raw$ja_score)
DE_adj$mal_ja_negscore <- ifelse(DE_raw$ja_score == 0 | DE_raw$ja_score == 1 | DE_raw$ja_score == 2, 0, DE_raw$mal_ja_score)

#pos schon - doch schon
DE_adj$doch_posscore <- ifelse(DE_raw$schon_score == 0 | DE_raw$schon_score == -1 | DE_raw$schon_score == -2, 0, DE_raw$doch_score)
DE_adj$schon_posscore <- ifelse(DE_raw$schon_score == 0 | DE_raw$schon_score == -1 | DE_raw$schon_score == -2, 0, DE_raw$schon_score)
DE_adj$doch_schon_posscore <- ifelse(DE_raw$schon_score == 0 | DE_raw$schon_score == -1 | DE_raw$schon_score == -2, 0, DE_raw$doch_schon_score)

#neg schon - doch schon
DE_adj$doch_negscore <- ifelse(DE_raw$schon_score == 0 | DE_raw$schon_score == 1 | DE_raw$schon_score == 2, 0, DE_raw$doch_score)
DE_adj$schon_negscore <- ifelse(DE_raw$schon_score == 0 | DE_raw$schon_score == 1 | DE_raw$schon_score == 2, 0, DE_raw$schon_score)
DE_adj$doch_schon_negscore <- ifelse(DE_raw$schon_score == 0 | DE_raw$schon_score == 1 | DE_raw$schon_score == 2, 0, DE_raw$doch_schon_score)


colnames(KR_adj)
# [1] "conf_score"         "sweat_score"        "broken_score"      
# [4] "grim_score"         "kiss_score"         "twoh_score"        
# [7] "kisscl_score"       "kissm_score"        "sungl_score"       
# [10] "smile_score"        "imp_score"          "fear_score"        
# [13] "joycat_score"       "disa_score"         "conf_sweat_score"  
# [16] "broken_grim_score"  "kiss_twoh_score"    "kisscl_kissm_score"
# [19] "sungl_smile_score"  "imp_fear_score"     "joycat_disa_score" 
# [22] "fluency"            "culturalbg"         "name_cult"         
# [25] "gender"             "age"                "mal_score"         
# [28] "ja_score"           "mal_ja_score"       "doch_score"        
# [31] "schon_score"        "doch_schon_score"  


colnames(DE_adj)
# [1] "halt_score"                "eben_score"               
# [3] "eigentlich_score"          "wirklich_score"           
# [5] "ruhig_score"               "nuneinmal_score"          
# [7] "mal_score"                 "ja_score"                 
# [9] "doch_score"                "schon_score"              
# [11] "halt_eben_score"           "eigentlich_wirklich_score"
# [13] "ruhig_nuneinmal_score"     "mal_ja_score"             
# [15] "doch_schon_score"          "kenntnisse"               
# [17] "culturalbg"                "name_cult"                
# [19] "gender"                    "age"                      
# [21] "mal_posscore"              "ja_posscore"              
# [23] "mal_ja_posscore"           "doch_posscore"            
# [25] "schon_posscore"            "doch_schon_posscore"      
# [27] "doch_negscore"             "schon_negscore"           
# [29] "doch_schon_negscore"  





#variable names rearranged. 


kr <- KR_adj[,c("conf_score" ,   #halt   
                "sweat_score",    #eben 
                
                "broken_score" ,   #eigentlich
                "grim_score",    #wirklich    
                
                "kiss_score" ,  #ruhig
                "twoh_score",        #nuneinmal
                
                "kisscl_score"  , #malpos     
                "kissm_score" ,       #japos
                "sungl_score" ,       #dochpos
                "smile_score"  ,    #schonpos
                
                "imp_score"  ,  #malneg      
                "fear_score"  ,      #janeg
                "joycat_score" ,    #dochneg
                "disa_score"  ,     #schonneg
                
                "conf_sweat_score", #halt-eben
                "broken_grim_score", #eig-wirk
                "kiss_twoh_score", #ruh-ne
                "kisscl_kissm_score", #mal-ja pos
                "sungl_smile_score", #doch-schon pos
                "imp_fear_score",  #mal- ja neg
                "joycat_disa_score", #doch-schon neg
                
                
                "mal_score",         
                "ja_score",                  "doch_score",        
                "schon_score",      "mal_ja_score",  "doch_schon_score",  
                
                "fluency",            "culturalbg",         "name_cult",         
                "gender",             "age"      )]


de <- DE_adj[, c(
  
  "halt_score",                "eben_score",               
  "eigentlich_score",          "wirklich_score",           
  "ruhig_score",               "nuneinmal_score",          
  "mal_posscore",              "ja_posscore",  
  "doch_posscore" ,           "schon_posscore",
  "mal_negscore", "ja_negscore", 
  "doch_negscore",             "schon_negscore",    
  "halt_eben_score" ,          "eigentlich_wirklich_score",
  "ruhig_nuneinmal_score",      "mal_ja_posscore",
  "doch_schon_posscore",      
  "mal_ja_negscore", "doch_schon_negscore",           
  
  "mal_score",                 "ja_score",                 
  "doch_score",                "schon_score",     
  "mal_ja_score",             
  "doch_schon_score",          
  
  "kenntnisse",               
  "culturalbg",                "name_cult",                
  "gender",                    "age"            )]


kr
de


DE_selected <- de[, c( 
  
  "halt_score",                "eben_score",               
  "eigentlich_score",          "wirklich_score",           
  "ruhig_score",               "nuneinmal_score",          
  "mal_posscore",              "ja_posscore",  
  "doch_posscore" ,           "schon_posscore",
  "mal_negscore", "ja_negscore", 
  "doch_negscore",             "schon_negscore",    
  "halt_eben_score" ,          "eigentlich_wirklich_score",
  "ruhig_nuneinmal_score",      "mal_ja_posscore",
  "doch_schon_posscore",      
  "mal_ja_negscore", "doch_schon_negscore",           
  
  "mal_score",                 "ja_score",                 
  "doch_score",                "schon_score",     
  "mal_ja_score",             
  "doch_schon_score"         
  
)]

KR_selected <- kr[, c("conf_score" ,   #halt   
                      "sweat_score",    #eben 
                      
                      "broken_score" ,   #eigentlich
                      "grim_score",    #wirklich    
                      
                      "kiss_score" ,  #ruhig
                      "twoh_score",        #nuneinmal
                      
                      "kisscl_score"  , #malpos     
                      "kissm_score" ,       #japos
                      "sungl_score" ,       #dochpos
                      "smile_score"  ,    #schonpos
                      
                      "imp_score"  ,  #malneg      
                      "fear_score"  ,      #janeg
                      "joycat_score" ,    #dochneg
                      "disa_score"  ,     #schonneg
                      
                      "conf_sweat_score", #halt-eben
                      "broken_grim_score", #eig-wirk
                      "kiss_twoh_score", #ruh-ne
                      "kisscl_kissm_score", #mal-ja pos
                      "sungl_smile_score", #doch-schon pos
                      "imp_fear_score",  #mal- ja neg
                      "joycat_disa_score", #doch-schon neg
                      
                      
                      "mal_score",         
                      "ja_score",                  "doch_score",        
                      "schon_score",      "mal_ja_score",  "doch_schon_score"  
                      
                      
                      )]




#########################################################################################




#responses by cultural backgrounds

dcul <- de[, c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
              "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score","culturalbg")]

kcul <- kr[, c("conf_score" ,   #halt   
               "sweat_score",    #eben 
               
               "broken_score" ,   #eigentlich
               "grim_score",    #wirklich    
               
               "kiss_score" ,  #ruhig
               "twoh_score",        #nuneinmal
               
               "kisscl_score"  , #malpos     
               "kissm_score" ,       #japos
               "sungl_score" ,       #dochpos
               "smile_score"  ,    #schonpos
               
               "imp_score"  ,  #malneg      
               "fear_score"  ,      #janeg
               "joycat_score" ,    #dochneg
               "disa_score"  ,     #schonneg
               
               "conf_sweat_score", #halt-eben
               "broken_grim_score", #eig-wirk
               "kiss_twoh_score", #ruh-ne
               "kisscl_kissm_score", #mal-ja pos
               "sungl_smile_score", #doch-schon pos
               "imp_fear_score",  #mal- ja neg
               "joycat_disa_score", #doch-schon neg
               
               
               "mal_score",         
               "ja_score",                  "doch_score",        
               "schon_score",      "mal_ja_score",  "doch_schon_score"  ,"culturalbg")]
dcul
kcul

dcul <- split(dcul[, sapply(dcul, is.numeric)], dcul$culturalbg)
kcul <- split(kcul[, sapply(kcul, is.numeric)], kcul$culturalbg)

dcul
kcul


dgroup_means <- lapply(dcul, function(x) colMeans(x, na.rm = TRUE))
kgroup_means <- lapply(kcul, function(x) colMeans(x, na.rm = TRUE))
dgroup_means
kgroup_means



variable_names <- colnames(dcul[[1]]) 

dfcul <- data.frame(
  culturalbg = rep(names(dgroup_means), each = ncol(dcul[[1]])),
  variable = rep(colnames(dcul[[1]]), times = length(dgroup_means)),
  average_score = unlist(lapply(dgroup_means, function(x) x)))


ggplot(dfcul, aes(x = variable, y = average_score, color = culturalbg, group = culturalbg)) +
  geom_line(size = 1.5) +  
  geom_point(shape = 21, size = 3, fill = "white") +  
  labs(title = "Average Scores for Modal Particles by Cultural Background", x = "Variables", y = "Average Score") +
  scale_color_brewer(palette = "Pastel1", name = "Cultural Background") +   
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

variable_names_k <- colnames(kcul[[1]])  

kfcul <- data.frame(
  culturalbg = rep(names(kgroup_means), each = ncol(kcul[[1]])),
  variable = rep(colnames(kcul[[1]]), times = length(kgroup_means)),
  average_score = unlist(lapply(kgroup_means, function(x) x)))


ggplot(kfcul, aes(x = variable, y = average_score, color = culturalbg, group = culturalbg)) +
  geom_line(size = 1.5) +  
  geom_point(shape = 21, size = 3, fill = "white") +  
  labs(title = "Average Scores for Emojis by Cultural Background", x = "Variables", y = "Average Score") +
  scale_color_brewer(palette = "Pastel1", name = "Cultural Background") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  



##########################################################################################################################


# fluency 

dflu <- de[, c("halt_score", "eben_score", "eigentlich_score", "wirklich_score", "mal_score", "ja_score", "ruhig_score", "nuneinmal_score", "doch_score", "schon_score",  
               "halt_eben_score", "eigentlich_wirklich_score", "mal_ja_score", "ruhig_nuneinmal_score", "doch_schon_score","kenntnisse")]

kflu <- kr[, c("conf_score" ,   #halt   
               "sweat_score",    #eben 
               
               "broken_score" ,   #eigentlich
               "grim_score",    #wirklich    
               
               "kiss_score" ,  #ruhig
               "twoh_score",        #nuneinmal
               
               "kisscl_score"  , #malpos     
               "kissm_score" ,       #japos
               "sungl_score" ,       #dochpos
               "smile_score"  ,    #schonpos
               
               "imp_score"  ,  #malneg      
               "fear_score"  ,      #janeg
               "joycat_score" ,    #dochneg
               "disa_score"  ,     #schonneg
               
               "conf_sweat_score", #halt-eben
               "broken_grim_score", #eig-wirk
               "kiss_twoh_score", #ruh-ne
               "kisscl_kissm_score", #mal-ja pos
               "sungl_smile_score", #doch-schon pos
               "imp_fear_score",  #mal- ja neg
               "joycat_disa_score", #doch-schon neg
               
               "fluency")]
dflu
kflu

dflu <- split(dflu[, sapply(dflu, is.numeric)], dflu$kenntnisse)
kflu <- split(kflu[, sapply(kflu, is.numeric)], kflu$fluency)

dgroup_means <- lapply(dflu, function(x) colMeans(x, na.rm = TRUE))
kgroup_means <- lapply(kflu, function(x) colMeans(x, na.rm = TRUE))

dflu_sample_count <- lapply(dflu, function(x) length(x[[1]]))
kflu_sample_count <- lapply(kflu, function(x) length(x[[1]]))

dflu_sample_count
# $`1`
# [1] 1
# 
# $`2`
# [1] 1
# 
# $`3`
# [1] 1
# 
# $`4`
# [1] 4
# 
# $`5`
# [1] 28

kflu_sample_count
# $`1`
# [1] 3
# 
# $`2`
# [1] 3
# 
# $`3`
# [1] 26
# 
# $`4`
# [1] 55
# 
# $`5`
# [1] 25


vars <- dflu[[1]]

variable_names <- colnames(dflu[[1]])

dfflu <- data.frame(
  fluency = rep(names(dgroup_means), each = ncol(dflu[[1]])),
  variable = rep(variable_names, times = length(dgroup_means)),
  average_score = unlist(lapply(dgroup_means, function(x) x[variable_names])))

ggplot(dfflu, aes(x = variable, y = average_score, color = fluency, group = fluency)) +
  geom_line(size = 1.5) +  
  geom_point(shape = 21, size = 3, fill = "white") +  
  labs(title = "Average Scores for Modal Particles by fluency", x = "variable", y = "Average Score") +
  scale_color_brewer(palette = "Pastel1", name = "fluency") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  







variable_names_k <- colnames(kflu[[1]])  
variable_names_k

kfflu <- data.frame(
  fluency = rep(names(kgroup_means), each = ncol(kflu[[1]])),
  variable = rep(colnames(kflu[[1]]), times = length(kgroup_means)),
  average_score = unlist(lapply(kgroup_means, function(x) x)))

na_count <- sum(is.na(kfflu))
print(na_count)

ggplot(kfflu, aes(x = variable, y = average_score, color = fluency, group = fluency)) +
  geom_line(size = 1.5) +  
  geom_point(shape = 21, size = 3, fill = "white") + 
  labs(title = "Average Scores for Emojis by fluency", x = "variable", y = "Average Score") +
  scale_color_brewer(palette = "Pastel1", name = "fluency") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))  






#correlation test with fluency - Korean.

cor_test <- function(x, y) {
  result <- cor.test(x, y)
  return(c(result$estimate, result$p.value))
}

relevant_columns <- c("conf_score" ,   #halt   
                      "sweat_score",    #eben 
                      
                      "broken_score" ,   #eigentlich
                      "grim_score",    #wirklich    
                      
                      "kiss_score" ,  #ruhig
                      "twoh_score",        #nuneinmal
                      
                      "kisscl_score"  , #malpos     
                      "kissm_score" ,       #japos
                      "sungl_score" ,       #dochpos
                      "smile_score"  ,    #schonpos
                      
                      "imp_score"  ,  #malneg      
                      "fear_score"  ,      #janeg
                      "joycat_score" ,    #dochneg
                      "disa_score"  ,     #schonneg
                      
                      "conf_sweat_score", #halt-eben
                      "broken_grim_score", #eig-wirk
                      "kiss_twoh_score", #ruh-ne
                      "kisscl_kissm_score", #mal-ja pos
                      "sungl_smile_score", #doch-schon pos
                      "imp_fear_score",  #mal- ja neg
                      "joycat_disa_score", #doch-schon neg
                      
                      
                      "mal_score",         
                      "ja_score",                  "doch_score",        
                      "schon_score",      "mal_ja_score",  "doch_schon_score"  ,"fluency")



correlation_matrix <- cor(kr[, relevant_columns], method = "pearson")

correlation_with_fluency <- apply(kr[, relevant_columns], 2, function(x) cor_test(x, kr$fluency))

correlation_results <- data.frame(Correlation = correlation_with_fluency[1, ],
                                  P_Value = correlation_with_fluency[2, ])

print(correlation_results)



#                      Correlation      P_Value
# conf_score         -0.054321312 5.694554e-01
# sweat_score        -0.118283787 2.141921e-01
# broken_score       -0.015246656 8.732320e-01
# grim_score         -0.162512290 8.690139e-02
# kiss_score          0.106405011 2.641546e-01
# twoh_score          0.413056224 6.006951e-06 ***
# kisscl_score        0.213854125 2.356861e-02 **
# kissm_score         0.212267990 2.464629e-02 **
# sungl_score         0.269213017 4.100674e-03 ***
# smile_score         0.094252786 3.229126e-01
# imp_score          -0.080293546 4.000264e-01
# fear_score         -0.190879767 4.380044e-02 **
# joycat_score        0.068114610 4.754747e-01
# disa_score         -0.184679792 5.125442e-02
# conf_sweat_score    0.031929333 7.382266e-01
# broken_grim_score  -0.051251440 5.915009e-01
# kiss_twoh_score    -0.085107409 3.722815e-01
# kisscl_kissm_score -0.098866845 2.996828e-01
# sungl_smile_score  -0.007806916 9.348893e-01
# imp_fear_score      0.146842448 1.223491e-01
# joycat_disa_score  -0.063461487 5.062108e-01
# mal_score           0.305162036 1.069001e-03 ***
# ja_score            0.179773726 5.786942e-02
# doch_score         -0.012036005 8.997694e-01
# schon_score        -0.225211052 1.696674e-02 **
# mal_ja_score       -0.070498053 4.601310e-01
# doch_schon_score    0.051798352 5.875452e-01








par(mfrow=c(1,1))  

plot(1, type = "n", xlim = c(1, length(dgroup_means[[1]])), ylim = range(unlist(dgroup_means)), 
     xlab = "Variables", ylab = "Average Score", main = "Average Scores for Modal Particles")
dvariable_names <- names(dgroup_means[[1]])

for (i in 1:length(dgroup_means)) {
  lines(1:length(dgroup_means[[i]]), dgroup_means[[i]], type = "o", col = colors[i], pch = 19, lwd = 4, xaxt = "n")
}
axis(side = 1, at = 1:length(dvariable_names), labels = dvariable_names, cex.axis = 0.8)

legend("topleft", legend = names(dgroup_means), col = colors, lty = 1, cex = 0.8)



plot(1, type = "n", xlim = c(1, length(kgroup_means[[1]])), ylim = range(unlist(kgroup_means)), 
     xlab = "Variables", ylab = "Average Score", main = "Average Scores for emojis")

kvariable_names <- names(kgroup_means[[1]])

for (i in 1:length(kgroup_means)) {
  lines(1:length(kgroup_means[[i]]), kgroup_means[[i]], type = "o", col = colors[i], pch = 19, lwd = 4, xaxt = "n")
}
axis(side = 1, at = 1:length(kvariable_names), labels = kvariable_names, cex.axis = 0.8)

legend("top", legend = names(kgroup_means), col = colors, lty = 1, cex = 0.8)














##############################################################################################################################



variable_names <- colnames(dcul[[1]]) 

dfcul <- data.frame(
  culturalbg = rep(names(group_means), each = ncol(dcul[[1]])),
  variable = rep(colnames(dcul[[1]]), times = length(group_means)),
  average_score = unlist(lapply(group_means, function(x) x)))



ggplot(dfcul, aes(x = variable, y = average_score, color = culturalbg, group = culturalbg)) +
  geom_line(size = 1.5) +  # 선 두께 조정
  geom_point(shape = 21, size = 3, fill = "white") +  
  labs(title = "Average Scores for Modal Particles by Cultural Background", x = "Variables", y = "Average Score") +
  scale_color_brewer(palette = "Pastel1", name = "Cultural Background") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  


# ANOVA 

library(car)
dfcul

df_anova <- dfcul  
show(df_anova$average_score)

anova_result <- aov(average_score ~ culturalbg, data = df_anova)
summary(anova_result)

#             Df Sum Sq Mean Sq F value Pr(>F)
# culturalbg   2  0.494  0.2471   0.413  0.664
# Residuals   42 25.112  0.5979               



nrow(DE_selected) # [1] 35
nrow(KR_selected) # [1] 112


# Expand DE dataset.
DE_selected_expanded <- DE_selected[sample(1:nrow(DE_selected), 3 * nrow(DE_selected), replace = TRUE), ]
random_rows <- DE_selected_expanded[sample(1:nrow(DE_selected_expanded), 7), ]
DE_selected_combined <- rbind(DE_selected_expanded, random_rows)

nrow(DE_selected_combined) 


DE_selected <- as.data.frame(DE_selected)
KR_selected <- as.data.frame(KR_selected)

# plots 
par(mfrow=c(2, 2)) 

# hist
hist(DE_selected, col="blue", main="Histogram of DE_selected_combined", xlab="Value")
hist(KR_selected, col="red", main="Histogram of KR_selected", xlab="Value")

# box
boxplot(DE_selected, col="blue", main="Boxplot of DE_selected_combined", ylab="Value")
boxplot(KR_selected, col="red", main="Boxplot of KR_selected", ylab="Value")



#t-test for the dfs. 

for (i in 1:ncol(DE_selected_combined)) {
  variable_name_DE <- colnames(DE_selected_combined)[i]
  variable_name_KR <- colnames(KR_selected)[i]
  t_test_result <- t.test(DE_selected_combined[, i], KR_selected[, i])
  cat("Variable Name (DE): ", variable_name_DE, "\n")
  cat("Variable Name (KR): ", variable_name_KR, "\n")
  cat("Mean difference: ", t_test_result$estimate, "\n")
  cat("p-value: ", t_test_result$p.value, "\n")
  if(t_test_result$p.value < 0.05) {
    cat("Statistical Significance: Yes\n\n")
  } else {
    cat("Statistical Significance: No\n\n")
  }
}



# Variable Name (DE):  halt_score 
# Variable Name (KR):  conf_score 
# Mean difference:  -0.6160714 -0.4642857 
# p-value:  0.2554705 
# Statistical Significance: No
# 
# Variable Name (DE):  eben_score 
# Variable Name (KR):  sweat_score 
# Mean difference:  -1.357143 -0.6607143 
# p-value:  1.326008e-07 
# Statistical Significance: Yes
# 
# Variable Name (DE):  eigentlich_score 
# Variable Name (KR):  broken_score 
# Mean difference:  -0.3928571 -0.4107143 
# p-value:  0.8734622 
# Statistical Significance: No
# 
# Variable Name (DE):  wirklich_score 
# Variable Name (KR):  grim_score 
# Mean difference:  -1.089286 -0.3839286 
# p-value:  1.660563e-07 
# Statistical Significance: Yes
# 
# Variable Name (DE):  ruhig_score 
# Variable Name (KR):  kiss_score 
# Mean difference:  1.169643 0.4375 
# p-value:  3.312042e-09 
# Statistical Significance: Yes
# 
# Variable Name (DE):  nuneinmal_score 
# Variable Name (KR):  twoh_score 
# Mean difference:  -0.2321429 0.7857143 
# p-value:  8.545982e-13 
# Statistical Significance: Yes
# 
# Variable Name (DE):  mal_posscore 
# Variable Name (KR):  kisscl_score 
# Mean difference:  0.4553571 0.7410714 
# p-value:  0.004867901 
# Statistical Significance: Yes
# 
# Variable Name (DE):  ja_posscore 
# Variable Name (KR):  kissm_score 
# Mean difference:  0.5982143 0.7142857 
# p-value:  0.2535316 
# Statistical Significance: No
# 
# Variable Name (DE):  doch_posscore 
# Variable Name (KR):  sungl_score 
# Mean difference:  -0.125 0.3303571 
# p-value:  2.02492e-05 
# Statistical Significance: Yes
# 
# Variable Name (DE):  schon_posscore 
# Variable Name (KR):  smile_score 
# Mean difference:  0.3035714 0.2232143 
# p-value:  0.4893296 
# Statistical Significance: No
# 
# Variable Name (DE):  mal_negscore 
# Variable Name (KR):  imp_score 
# Mean difference:  0.2232143 -0.4732143 
# p-value:  5.084817e-09 
# Statistical Significance: Yes
# 
# Variable Name (DE):  ja_negscore 
# Variable Name (KR):  fear_score 
# Mean difference:  -0.2857143 -0.4732143 
# p-value:  0.07774834 
# Statistical Significance: No
# 
# Variable Name (DE):  doch_negscore 
# Variable Name (KR):  joycat_score 
# Mean difference:  -0.3571429 0.04464286 
# p-value:  0.0003700287 
# Statistical Significance: Yes
# 
# Variable Name (DE):  schon_negscore 
# Variable Name (KR):  disa_score 
# Mean difference:  -0.4732143 -0.3839286 
# p-value:  0.4518886 
# Statistical Significance: No
# 
# Variable Name (DE):  halt_eben_score 
# Variable Name (KR):  conf_sweat_score 
# Mean difference:  -0.3482143 -0.08035714 
# p-value:  0.04152462 
# Statistical Significance: Yes
# 
# Variable Name (DE):  eigentlich_wirklich_score 
# Variable Name (KR):  broken_grim_score 
# Mean difference:  -0.6785714 0.375 
# p-value:  1.252009e-12 
# Statistical Significance: Yes
# 
# Variable Name (DE):  ruhig_nuneinmal_score 
# Variable Name (KR):  kiss_twoh_score 
# Mean difference:  0.03571429 -0.1428571 
# p-value:  0.2783023 
# Statistical Significance: No
# 
# Variable Name (DE):  mal_ja_posscore 
# Variable Name (KR):  kisscl_kissm_score 
# Mean difference:  -0.1785714 0.3660714 
# p-value:  1.391264e-06 
# Statistical Significance: Yes
# 
# Variable Name (DE):  doch_schon_posscore 
# Variable Name (KR):  sungl_smile_score 
# Mean difference:  0.1875 0.07142857 
# p-value:  0.3367241 
# Statistical Significance: No
# 
# Variable Name (DE):  mal_ja_negscore 
# Variable Name (KR):  imp_fear_score 
# Mean difference:  -0.2232143 0.4642857 
# p-value:  1.207756e-07 
# Statistical Significance: Yes
# 
# Variable Name (DE):  doch_schon_negscore 
# Variable Name (KR):  joycat_disa_score 
# Mean difference:  0.2857143 0.1785714 
# p-value:  0.3572036 
# Statistical Significance: No
# 
# Variable Name (DE):  mal_score 
# Variable Name (KR):  mal_score 
# Mean difference:  0.7946429 0.5357143 
# p-value:  0.006081654 
# Statistical Significance: Yes
# 
# Variable Name (DE):  ja_score 
# Variable Name (KR):  ja_score 
# Mean difference:  0.3125 0.46875 
# p-value:  0.2067353 
# Statistical Significance: No
# 
# Variable Name (DE):  doch_score 
# Variable Name (KR):  doch_score 
# Mean difference:  -0.7053571 -0.2142857 
# p-value:  6.831979e-06 
# Statistical Significance: Yes
# 
# Variable Name (DE):  schon_score 
# Variable Name (KR):  schon_score 
# Mean difference:  -0.1696429 -0.4285714 
# p-value:  0.03624466 
# Statistical Significance: Yes
# 
# Variable Name (DE):  mal_ja_score 
# Variable Name (KR):  mal_ja_score 
# Mean difference:  -0.3214286 0.21875 
# p-value:  7.392803e-06 
# Statistical Significance: Yes
# 
# Variable Name (DE):  doch_schon_score 
# Variable Name (KR):  doch_schon_score 
# Mean difference:  0.6160714 0.3214286 
# p-value:  0.004269502 
# Statistical Significance: Yes





# correltaion: Since the Survey 1 and 2 is conducted in different Cultural contexts, correlation test has no significant meanings. 


for (i in 1:ncol(DE_selected_combined)) {
  variable_name_DE <- colnames(DE_selected_combined)[i]
  variable_name_KR <- colnames(KR_selected)[i]
  cor_test_result <- cor.test(DE_selected_combined[, i], KR_selected[, i])
  cor_result <- cor(DE_selected_combined[, i], KR_selected[, i])
  pearson_cor <- cor(DE_selected_combined[, i], KR_selected[, i], method = "pearson")
  cat("Variable Name (DE): ", variable_name_DE, "\n")
  cat("Variable Name (KR): ", variable_name_KR, "\n")
  cat("Correlation (Pearson): ", pearson_cor, "\n")
  cat("p-value: ", cor_test_result$p.value, "\n")
  if(cor_test_result$p.value < 0.05) {
    cat("Statistical Significance: Yes\n\n")
  } else {
    cat("Statistical Significance: No\n\n")
  }
}

# Variable Name (DE):  halt_score 
# Variable Name (KR):  conf_score 
# Correlation (Pearson):  -0.09114053 
# p-value:  0.3392183 
# Statistical Significance: No
# 
# Variable Name (DE):  eben_score 
# Variable Name (KR):  sweat_score 
# Correlation (Pearson):  -0.08335362 
# p-value:  0.3822503 
# Statistical Significance: No
# 
# Variable Name (DE):  eigentlich_score 
# Variable Name (KR):  broken_score 
# Correlation (Pearson):  -0.07821523 
# p-value:  0.4123741 
# Statistical Significance: No
# 
# Variable Name (DE):  wirklich_score 
# Variable Name (KR):  grim_score 
# Correlation (Pearson):  -0.007948741 
# p-value:  0.9337091 
# Statistical Significance: No
# 
# Variable Name (DE):  ruhig_score 
# Variable Name (KR):  kiss_score 
# Correlation (Pearson):  0.05359797 
# p-value:  0.5746149 
# Statistical Significance: No
# 
# Variable Name (DE):  nuneinmal_score 
# Variable Name (KR):  twoh_score 
# Correlation (Pearson):  0.3482908 
# p-value:  0.0001677877 
# Statistical Significance: Yes
# 
# Variable Name (DE):  mal_posscore 
# Variable Name (KR):  kisscl_score 
# Correlation (Pearson):  0.003404301 
# p-value:  0.9715824 
# Statistical Significance: No
# 
# Variable Name (DE):  ja_posscore 
# Variable Name (KR):  kissm_score 
# Correlation (Pearson):  0.08220906 
# p-value:  0.3888425 
# Statistical Significance: No
# 
# Variable Name (DE):  doch_posscore 
# Variable Name (KR):  sungl_score 
# Correlation (Pearson):  -0.1177956 
# p-value:  0.2161028 
# Statistical Significance: No
# 
# Variable Name (DE):  schon_posscore 
# Variable Name (KR):  smile_score 
# Correlation (Pearson):  -0.07875226 
# p-value:  0.4091623 
# Statistical Significance: No
# 
# Variable Name (DE):  mal_negscore 
# Variable Name (KR):  imp_score 
# Correlation (Pearson):  0.08093026 
# p-value:  0.3962879 
# Statistical Significance: No
# 
# Variable Name (DE):  ja_negscore 
# Variable Name (KR):  fear_score 
# Correlation (Pearson):  -0.1237781 
# p-value:  0.1935135 
# Statistical Significance: No
# 
# Variable Name (DE):  doch_negscore 
# Variable Name (KR):  joycat_score 
# Correlation (Pearson):  -0.003152292 
# p-value:  0.9736853 
# Statistical Significance: No
# 
# Variable Name (DE):  schon_negscore 
# Variable Name (KR):  disa_score 
# Correlation (Pearson):  -0.00436645 
# p-value:  0.9635558 
# Statistical Significance: No
# 
# Variable Name (DE):  halt_eben_score 
# Variable Name (KR):  conf_sweat_score 
# Correlation (Pearson):  0.03045134 
# p-value:  0.7499348 
# Statistical Significance: No
# 
# Variable Name (DE):  eigentlich_wirklich_score 
# Variable Name (KR):  broken_grim_score 
# Correlation (Pearson):  0.07400553 
# p-value:  0.4380567 
# Statistical Significance: No
# 
# Variable Name (DE):  ruhig_nuneinmal_score 
# Variable Name (KR):  kiss_twoh_score 
# Correlation (Pearson):  -0.05117777 
# p-value:  0.5920346 
# Statistical Significance: No
# 
# Variable Name (DE):  mal_ja_posscore 
# Variable Name (KR):  kisscl_kissm_score 
# Correlation (Pearson):  -0.06532326 
# p-value:  0.4937906 
# Statistical Significance: No
# 
# Variable Name (DE):  doch_schon_posscore 
# Variable Name (KR):  sungl_smile_score 
# Correlation (Pearson):  0.01949859 
# p-value:  0.8383081 
# Statistical Significance: No
# 
# Variable Name (DE):  mal_ja_negscore 
# Variable Name (KR):  imp_fear_score 
# Correlation (Pearson):  -0.1218856 
# p-value:  0.2004662 
# Statistical Significance: No
# 
# Variable Name (DE):  doch_schon_negscore 
# Variable Name (KR):  joycat_disa_score 
# Correlation (Pearson):  0.004972725 
# p-value:  0.9584998 
# Statistical Significance: No
# 
# Variable Name (DE):  mal_score 
# Variable Name (KR):  mal_score 
# Correlation (Pearson):  -0.03090327 
# p-value:  0.7463486 
# Statistical Significance: No
# 
# Variable Name (DE):  ja_score 
# Variable Name (KR):  ja_score 
# Correlation (Pearson):  0.1107853 
# p-value:  0.2448814 
# Statistical Significance: No
# 
# Variable Name (DE):  doch_score 
# Variable Name (KR):  doch_score 
# Correlation (Pearson):  0.1924618 
# p-value:  0.04205022 
# Statistical Significance: Yes
# 
# Variable Name (DE):  schon_score 
# Variable Name (KR):  schon_score 
# Correlation (Pearson):  -0.07157169 
# p-value:  0.4533097 
# Statistical Significance: No
# 
# Variable Name (DE):  mal_ja_score 
# Variable Name (KR):  mal_ja_score 
# Correlation (Pearson):  0.08747845 
# p-value:  0.3590594 
# Statistical Significance: No
# 
# Variable Name (DE):  doch_schon_score 
# Variable Name (KR):  doch_schon_score 
# Correlation (Pearson):  0.05174712 
# p-value:  0.5879152 
# Statistical Significance: No







