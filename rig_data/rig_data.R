require(tidyverse)

college_train = read_csv('1_Data/college_train.csv')

m = glm(factor(Private) ~ ., data = college_train, family = 'binomial')
ps = predict(m,type = 'response')
p_change = (1-abs(ps-.5)*2)



set.seed(100)
change = runif(nrow(college_train),0,1)<p_change
revers = ifelse(college_train$Private == "Yes", "No", "Yes")
college_train =  college_train %>% mutate(Grad.Rate = Grad.Rate + rnorm(n(), 0, 20), 
                                          Grad.Rate = ifelse(Grad.Rate<0, 0, Grad.Rate),
                                          Grad.Rate = ifelse(Grad.Rate<0, 0, Grad.Rate),
                                          Private = ifelse(change,revers,Private))

write_csv(college_train, '1_Data/college_train.csv')
write_csv(college_train, '_sessions/Fitting/1_Data/college_train.csv')
write_csv(college_train, '_sessions/Prediction/1_Data/college_train.csv')
write_csv(college_train, '_sessions/Optimization/1_Data/college_train.csv')