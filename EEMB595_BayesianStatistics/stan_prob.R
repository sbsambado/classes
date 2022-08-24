library(tidyverse)
library(rethinking)
library(rstan)



sink("test.stan")
cat("
    
data {
  int<lower=1> N;
  int death[N];
  int discharge[N];
  int coded_treatment[N];
  int Ntreats;
}

parameters {
  real<lower=0, upper=1> prob[Ntreats];
}


model {
//priors
for(i in 1:Ntreats){
prob[i] ~ uniform(0,1);
}

//likelihood
for(i in 1:N){
death[i] ~ binomial(death[i] + discharge[i], prob[coded_treatment[i]]);

}

}
 
    
    
    ",fill = TRUE)
sink()

linear_model <- stan_model('test.stan')




outcomes = tibble(treatment = c("HC","HC_AZ","None") , death = c(27,25,18) , discharge = c(70,88,140) ,coded_treatment = c(1,2,3))

stan_data <- c(outcomes[c("death","discharge", "coded_treatment")], list(N = length(outcomes$treatment), Ntreats = length(unique(outcomes$treatment))))

stanfit_linear <- sampling(linear_model, data = stan_data, chains = 3,
                           iter = 2000, seed = 2131231)

post<-as.data.frame(stanfit_linear)
names(post) <- c("t1", "t2", "t3", "lp")

library(tidybayes)
stanfit_linear %>%
  spread_draws(prob[i]) %>%
  median_qi(.width = c(.8, .95)) %>%
  ggplot(aes(x = i, y = prob)) +
  geom_pointinterval()

post %>% gather(treatment, estimate, -lp) %>%
  ggplot(aes(x = treatment, y = estimate))+
  geom_pointintervalh()




temp <- as.data.frame(rethinking::extract.samples(stanfit_linear, n = 1000, pars = c("prob")))


post <- extract(stanfit_linear,pars=c("prob"))
precis(post,depth=2)

