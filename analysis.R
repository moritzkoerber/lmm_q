## packages
library(lme4)
library(afex)

##### one factor, categorical fixed effect 
df1 <- read.csv2("one_factor_rm_cat.csv", header=T,dec=",")
df1$situation <- factor(df1$situation)

# lmer model, random intercept
model <- lmer(measure ~ situation + (1|id),df1)
summary(model)

# lmer model, random intercept + random slope
model <- lmer(measure ~ situation + (1+situation|id),df1)
summary(model)


##### one factor, contious fixed effect
df2 <- read.csv2("one_factor_rm_cont.csv", header=T,dec=",")

# fixed effect, random intercept
model <- lmer(delta_distance ~ acceleration + (1|id),df2)
summary(model)

# fixed effect, random intercept & slope
# rescale predictor (otherwise conversion warning)
mu_x <- mean(df2$acceleration)
sig_x <- sd(df2$acceleration)
df2$acceleration_s <- (df2$acceleration-mu_x)/sig_x # standardize predictor

model <- lmer(delta_distance ~ acceleration_s + (1+acceleration_s|id),df2)
summary(model)


##### two factors
df3 <- read.csv2("two_factor_rm.csv", header=T,dec=",")
df3$stage<-factor(df3$stage)
df3$group<-factor(df3$group)

# model, random intercept
model <- lmer(dv ~ group*stage+(1|id),df3)
summary(model)

# model, random intercept + random slope
model <- lmer(dv ~ group*stage+(1+group|id)+(1+stage|id),df3)
summary(model)

#maximum model?
model <- lmer(dv ~ group*stage+(group*stage|id),df3)
summary(model)