

#function to sample from a specific strata (province)
set.seed(81)
sample_province <- function(
  idx,
  vote_prob = c(38,38,24),
  party_prob = c(0.3312, 0.3434, 0.1598, 0.0763, 0.0655, 0.0164, 0.0067),
  age_prob =  c(0.07, 0.28, .31, 0.20,0.14)
){
  #sample size
  total_size <- 200000
  #probability of each province (percent of people in each province)
  province_prob <- c(0.1319, 0.1151, 0.0310, 0.0364, 0.3823, 0.2320,
                     0.0203, 0.0261, 0.0140, 0.0031, 0.0010, 0.0009, 
                     0.0009, 0.005)
  
  party_prob <- party_prob %>% append(0.02) #adding no response
  party_prob <- party_prob /sum(party_prob) #normalize error
  
  #categories of surveys
  parties <-c("Liberal", "Conservative", "NDP", "Green", 
              "Bloc","People's Party", "Other", 'N/A')
  provinces <- c("BC", "AB", "SK", "MB", "ON", "QC", "NB",
                 "NS", "NL","PE", "NW", "YK", "NU", 'N/A')
  age <- c("18 - 24","25 - 44","45 - 64","65 +", 'N/A')
  sizes = round(province_prob * total_size)
  
  survey_data <- tibble(
    party = sample(x=parties, size = sizes[idx], 
                   replace = TRUE,prob=party_prob),
    province = sample(x=provinces[idx], size = sizes[idx],
                      replace=TRUE, prob = 1 ),
    familiarity = round(rnorm(n=sizes[idx], mean=1.8, sd = 0.7)),
    opinion = sample(x=c("Yes","No","Unsure"), 
                     size=sizes[idx], replace=TRUE,prob=vote_prob),
    educate = sample(x=c("Yes","No", "N/A"), size=sizes[idx], 
                     replace=TRUE, prob=c(0.88,0.07, 0.05)),
    age = sample(x = age, replace=TRUE, size = sizes[idx], prob=age_prob),
    usage = rpois(sizes[idx], 1),
    usage_outlier = sample(x=c(420, 69, 42069,0), replace=TRUE, 
                           size=sizes[idx], prob=c(0.0142,0.0169,0.0089,0.96))
  )
  
  survey_data$usage <- with(survey_data, usage*((usage_outlier>2)*0 + 
                                                  (usage_outlier==0)*1) + 
                              usage_outlier)
  survey_data <- subset(survey_data, select=-c(usage_outlier))
  
  survey_data$familiarity <- pmax(0, survey_data$familiarity)
  survey_data$familiarity <- pmin(3, survey_data$familiarity)
  
  survey_data <-  survey_data %>% 
    mutate(familiarity = as.character(familiarity)) %>% 
    mutate(familiarity = replace(familiarity, familiarity == '0', 
                                 '0-Unknowledgable')) %>% 
    mutate(familiarity = replace(familiarity, familiarity == '1', 
                                 '1-Unfamiliar')) %>% 
    mutate(familiarity = replace(familiarity, familiarity == '2',
                                 '2-Familiar')) %>% 
    mutate(familiarity = replace(familiarity, familiarity == '3',
                                 '3-Knowledgable'))
  
  return (survey_data)
}


bc_survey <- sample_province(1, 
                             vote_prob = c(42, 39, 19), 
                             party_prob = 
                               c(0.27, 0.40, 0.265, 0.055, 0.0, 0.005, 0.005))
ab_survey <- sample_province(2, 
                             vote_prob = c(38, 34, 18),
                             party_prob = 
                               c(0.005, 0.97, 0.2, 0.0025, 0.0, 0.0025, 0.005))
sk_survey <- sample_province(3, 
                             vote_prob = c(32, 38, 30),
                             party_prob = 
                               c(0.005, 0.99, 0.001, 0.001, 0.00, 0.001, 0.004))
mb_survey <- sample_province(4, 
                             vote_prob = c(35, 38, 27),
                             party_prob = 
                               c(0.28, 0.49, 0.215, 0.001, 0.00, 0.002, 0.02))
on_survey <- sample_province(5, 
                             vote_prob = c(40, 30, 30),
                             party_prob = 
                               c(0.62, 0.29, 0.05, 0.01, 0.005, 0.005, 0.02))
qc_survey <- sample_province(6, 
                             vote_prob = c(36, 40, 24),
                             party_prob = 
                               c(0.44, 0.13, 0.01, 0.001, 0.41, 0.005, 0.004))
nb_survey <- sample_province(7, 
                             vote_prob = c(35, 35, 30),
                             party_prob = 
                               c(0.6, 3, 0.1, 0.000, 0.00, 0.00, 0.00))
ns_survey <- sample_province(8, 
                             vote_prob = c(40, 42, 28),
                             party_prob = 
                               c(0.90, 9, 0.01, 0.00, 0.00, 0.00, 0.00))
nl_survey <- sample_province(9, 
                             vote_prob = c(39, 36, 25),
                             party_prob = 
                               c(0.85, 0.14, 0.005, 0.001, 0.001, 0.001, 0.002))
pe_survey <- sample_province(10, 
                             vote_prob = c(40, 38, 32),
                             party_prob = 
                               c(0.99, 0.005, 0.003, 0.001, 0.00, 0.001, 0.00))
nw_survey <- sample_province(11, 
                             vote_prob = c(40, 40, 20),
                             party_prob = 
                               c(0.99, 0.005, 0.002, 0.001, 0.00, 0.00, 0.002))
yk_survey <- sample_province(12, 
                             vote_prob = c(35, 35, 30),
                             party_prob = 
                               c(0.99, 0.005, 0.002, 0.001, 0.00, 0.00, 0.002))
nu_survey <- sample_province(13, 
                             vote_prob = c(40, 35, 25),
                             party_prob = 
                               c(0.002, 0.005, 0.99, 0.001, 0.00, 0.00, 0.002))

survey_data <- bind_rows(ab_survey, bc_survey)
survey_data <- bind_rows(survey_data, sk_survey)
survey_data <- bind_rows(survey_data, mb_survey)
survey_data <- bind_rows(survey_data, on_survey)
survey_data <- bind_rows(survey_data, qc_survey)
survey_data <- bind_rows(survey_data, nb_survey)
survey_data <- bind_rows(survey_data, ns_survey)
survey_data <- bind_rows(survey_data, nl_survey)
survey_data <- bind_rows(survey_data, pe_survey)
survey_data <- bind_rows(survey_data, nw_survey)
survey_data <- bind_rows(survey_data, yk_survey)
survey_data <- bind_rows(survey_data, nu_survey)
write.csv(survey_data,"./assets/simulated.csv", row.names = TRUE)
