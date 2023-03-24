### This script replicates the analyses reported in the article 'Explaining the Gender Gap in COVID-19 Vaccination Attitudes' published in the European Journal of Public Health (2023)
### The data used in this article is available on the GESIS archive:
### European Commission, Brussels (2023). Eurobarometer 94.3 (2021). GESIS, Cologne. ZA7780 Data file Version 2.0.0, https://doi.org/10.4232/1.14076.
### European Commission, Brussels (2022). Flash Eurobarometer 494 (Attitudes on Vaccination against Covid-19). GESIS, Cologne. ZA7771 Data file Version 2.0.0, https://doi.org/10.4232/1.14030.
### Last updated 23 March 2023 by Dimiter Toshkov

# Libraries and functions -------------------------------------------------
library(dplyr)
library(haven)
library(questionr)
library(mgcv)
library(sjlabelled)
library(gtsummary)

# Graphical settings
library(countrycode) # to switch between country names and codes
library(extrafont) # to embed extra fonts
library(sysfonts) # to check available fonts and download fonts from google
library(showtext) # to use the extra fonts

## add custom fonts
font_add_google('Quattrocento') #get the fonts 
font_add_google('Quattrocento Sans')
font_families() #check that the fonts are installed and available

showtext_auto() #this is to turn on the custom fonts availability

s = 3 # scaling factor
## color settings
dark.color = rgb(24, 24, 38, max=255) # dark color: almost black

# 01a Data import and variables (May 2021) --------------------------------------------
d1<-read_sav('./data/ZA7771_v1-0-0.sav')
d1 <- d1 %>%
  mutate (country = substr(isocntry, 1, 2),
          weight = w1,
          
          never = ifelse (q1==4, 1, 0),
          uncertain = ifelse (q1==3 | q1==998 | q1==999, 1, 0),
          hesitant = ifelse (q1==4 | q1==3 | q1==998 | q1==999, 1, 0),
          vaccinated = ifelse (q1==5, 1, 0),
          
          region3 = recode (country, 'BG' = 'Eastern Europe','RO' = 'Eastern Europe','CZ' = 'Eastern Europe','PL' = 'Eastern Europe',
                            'SK' = 'Eastern Europe','HU' = 'Eastern Europe','SI' = 'Eastern Europe','HR' = 'Eastern Europe',
                            'EE' = 'Eastern Europe','LT' = 'Eastern Europe','LV' = 'Eastern Europe',
                            'PT' = 'Southern Europe', 'ES' = 'Southern Europe', 'IT' = 'Southern Europe', 'GR' = 'Southern Europe',
                            'CY' = 'Southern Europe', 'MT' = 'Southern Europe', .default = 'Western Europe'),
          
          male = ifelse (d2==1, 1, 0),
          age = d1,
          edu = ifelse (d4>82, NA, (ifelse(d4<14, NA, d4))),
          city = ifelse (d13==3, 1, 0),
          empl = dplyr::recode(d5r, '1'='self-employed','2'='employee', '3'='manual','4'='no activity', .default=NA_character_),
          
          v.safe = ifelse (sd2_1==1 | sd2_1==2, 1, 0),
          v.effective = ifelse (sd2_2==1 | sd2_2==2, 1, 0),
          v.ever = ifelse (sd1_1==1 | sd1_2==1, 1, 0),
          
          know.covid = ifelse (q9_1==1, 1, 0),
          know.ill.covid = ifelse (q9_2==1, 1, 0),
          had.covid = ifelse (q9_3==1, 1, 0),
          was.ill.covid = ifelse (q9_4==1, 1, 0),
          fear.covid = ifelse (q9_5==1, 1, 0),
          
          can.avoid.covid = ifelse (q5_1==1 | q5_1==2, 1, 0),
          v.compulsory = ifelse (q5_4==1 | q5_4==2, 1, 0),
          
          trust.eu.info = q6.1,
          trust.gov.info = q6.2,
          trust.health.info = q6.3,
          trust.local.info = q6.4,
          trust.doctors.info = q6.5,
          trust.media.info = q6.6,
          trust.web.info = q6.7,
          trust.networks.info = q6.8,
          trust.people.info = q6.9,
          
          reason.yes.covid.end =  ifelse (q2a_1==1 | q2a_1==2, 1, 0),
          reason.yes.protect.me =  ifelse (q2a_2==1 | q2a_2==2, 1, 0),
          reason.yes.protect.others =  ifelse (q2a_3==1 | q2a_3==2, 1, 0),
          reason.yes.to.work =  ifelse (q2a_4==1 | q2a_4==2, 1, 0),
          reason.yes.to.travel =  ifelse (q2a_5==1 | q2a_5==2, 1, 0),
          reason.yes.to.socialize =  ifelse (q2a_6==1 | q2a_6==2, 1, 0),
          reason.yes.to.go.out =  ifelse (q2a_7==1 | q2a_7==2, 1, 0),
          
          reason.no.covid.over =  ifelse (q2b_1==1 | q2b_1==2, 1, 0),
          reason.no.risk.me.low =  ifelse (q2b_2==1 | q2b_2==2, 1, 0),
          reason.no.risk.gen.over =  ifelse (q2b_3==1 | q2b_3==2, 1, 0),
          reason.no.side.effects =  ifelse (q2b_4==1 | q2b_4==2, 1, 0),
          reason.no.limit.test =  ifelse (q2b_5==1 | q2b_5==2, 1, 0),
          reason.no.limit.effect =  ifelse (q2b_6==1 | q2b_6==2, 1, 0),
          reason.no.gen.against =  ifelse (q2b_7==1 | q2b_7==2, 1, 0),
          
          v.benefits =  ifelse (q4_1==1 | q4_1==2, 1, 0),
          v.eu.safe =  ifelse (q4_2==1 | q4_2==2, 1, 0),
          v.toofast =  ifelse (q4_3==1 | q4_3==2, 1, 0),
          v.sideeffects =  ifelse (q4_4==1 | q4_4==2, 1, 0),
          v.onlyway =  ifelse (q4_5==1 | q4_5==2, 1, 0),
          v.duty =  ifelse (q5_3==1 | q5_3==2, 1, 0),
  )
d1$hesitant1 <- ifelse (d1$uncertain==1, NA, ifelse(d1$hesitant==1, 1, 0))

# 01b Data import and variables (February 2021) --------------------------------------------
e2<-read_sav('./data/ZA7780_v1-0-0.sav')
e2 <- e2 %>%
  mutate (defence = ifelse(qb6_2==1, 1, 0),
          country = substr(isocntry, 1, 2),
          weight=ifelse(isocntry=='DE', w3, w1),
          male = ifelse (d10==1, 1, 0),
          age = d11,
          never = ifelse (qa19==4, 1, 0),
          uncertain = ifelse (qa19==3 | qa19==5, 1, 0),
          hesitant = ifelse (qa19==4 | qa19==3 | qa19==5, 1, 0),
          vaccinated = ifelse (qa19==6 , 1, 0),
          lr = ifelse(d1>11, NA, d1),
          trust.gov = ifelse (qa6b_8==1, 1, 0),
          trust.health = ifelse (qa6b_7==1, 1, 0),
          trust.internet = ifelse(qa6a_4==1,1,0),
          trust.networks = ifelse(qa6a_5==1,1,0),
          trust.eu.info = qa20.1,
          trust.gov.info = qa20.2,
          trust.health.info = qa20.3,
          trust.local.info = qa20.4,
          trust.doctors.info = qa20.5,
          trust.media.info = qa20.6,
          trust.web.info = qa20.7,
          trust.networks.info = qa20.8,
          trust.people.info = qa20.9,
          
          v.tooquick = ifelse (qa18_1==1 | qa18_1==2, 1, 0),
          v.sideeffects = ifelse (qa18_2==1 | qa18_2==2, 1, 0),
          v.onlyway = ifelse (qa18_3==1 | qa18_3==2, 1, 0),
          
  )

e2<-e2 %>%
  filter(country!='AL', country!='BA', country!='CH', country!='GB', country!='IS', country!='ME', country!='MK',
         country!='NO', country!='RS', country!='TR')
# Raw means ---------------------------------------------------------------
# Get raw means of hesitancy per gender
round(prop.table(table(e2$hesitant, e2$male),2),2)
round(prop.table(table(d1$hesitant, d1$male),2),2) 

round(prop.table(table(e2$never, e2$male),2),2)
round(prop.table(table(d1$never, d1$male),2),2) 

# Statistical tests for difference of proportions
prop.test(table(e2$hesitant, e2$male)[2,], table(e2$male))
prop.test(table(d1$hesitant, d1$male)[2,], table(d1$male))

prop.test(table(e2$never, e2$male)[2,], table(e2$male))
prop.test(table(d1$never, d1$male)[2,], table(d1$male))

# Weighted means ---------------------------------------------------------------
# Get weighted means of hesitancy and refusal per gender
questionr::cprop(questionr::wtd.table(factor(e2$hesitant), factor(e2$male), weights=e2$weight, digits = 0, useNA = NULL),
                 digits=0, total=FALSE, n=FALSE, percent=TRUE)

questionr::cprop(questionr::wtd.table(factor(d1$hesitant), factor(d1$male), weights=d1$weight, digits = 0, useNA = NULL),
       digits=0, total=FALSE, n=FALSE, percent=TRUE)

questionr::cprop(questionr::wtd.table(factor(e2$never), factor(e2$male), weights=e2$weight, digits = 0, useNA = NULL),
                 digits=0, total=FALSE, n=FALSE, percent=TRUE)

questionr::cprop(questionr::wtd.table(factor(d1$never), factor(d1$male), weights=d1$weight, digits = 0, useNA = NULL),
                 digits=0, total=FALSE, n=FALSE, percent=TRUE)
# Comparing the gender gap across countries -------------------------------------------------------------
u = unique(d1$country)
t5 <- data.frame(matrix (NA, nrow=length(u), ncol=5))
for (i in 1:length(u)) {
  d.s<-d1[d1$country==u[i],]
  t5[i,1] <- u[i]
  t5[i,2] <- round(prop.table(table(d.s$v.safe, d.s$male),2),2)[2,1]
  t5[i,3] <- round(prop.table(table(d.s$v.safe, d.s$male),2),2)[2,2]
  t5[i,4] <- round(prop.table(table(d.s$v.effective, d.s$male),2),2)[2,1]
  t5[i,5] <- round(prop.table(table(d.s$v.effective, d.s$male),2),2)[2,2]
}

t5$gap.v.safe = t5$X2-t5$X3
t5$gap.v.effective = t5$X4-t5$X5

t5
# Raw means per country ---------------------------------------------------------------
# Vaccine hesitancy per country, unweighted
u = unique(e2$country)
t1 <- data.frame(matrix (NA, nrow=length(u), ncol=5))
for (i in 1:length(u)) {
  e.s<-e2[e2$country==u[i],]
  t1[i,1] <- u[i]
  t1[i,2] <- round(prop.table(table(e.s$hesitant, e.s$male),2),2)[2,1]
  t1[i,3] <- round(prop.table(table(e.s$hesitant, e.s$male),2),2)[2,2]
  t1[i,4] <- round(prop.table(table(e.s$never, e.s$male),2),2)[2,1]
  t1[i,5] <- round(prop.table(table(e.s$never, e.s$male),2),2)[2,2]
}

t1$gap.hesitant = t1$X2-t1$X3
t1$gap.never = t1$X4-t1$X5
t1[order(t1$gap.hesitant, decreasing = TRUE),]

# May 2021
u2 = unique(d1$country)
t2 <- data.frame(matrix (NA, nrow=length(u2), ncol=5))
for (i in 1:length(u2)) {
  d.s<-d1[d1$country==u2[i],]
  t2[i,1] <- u2[i]
  t2[i,2] <- round(prop.table(table(d.s$hesitant, d.s$male),2),2)[2,1]
  t2[i,3] <- round(prop.table(table(d.s$hesitant, d.s$male),2),2)[2,2]
  t2[i,4] <- round(prop.table(table(d.s$never, d.s$male),2),2)[2,1]
  t2[i,5] <- round(prop.table(table(d.s$never, d.s$male),2),2)[2,2]
}

t2$gap.hesitant = t2$X2-t2$X3
t2$gap.never = t2$X4-t2$X5
t2[order(t2$gap.hesitant, decreasing = TRUE),]

t3<-left_join (t1, t2, by='X1')
t3 <- t3[order(t3$X2.y, decreasing = TRUE),]

# Figure 1 attitudes per gender and per country ---------------------------------------------------------------
y.min=0
y.max=62
tiff ('./figures/F_gender_vaccince_hesitancy_country.tiff', width = 12.8*s, height = 9.055*s, units = 'in', res=300)

par(mfrow=c(1,1), # number and distribution of plots
    oma=c(0,0,0,0), # size of the outer margins in lines of text
    mar=c(2,5,1,1), # number of lines of margin to be specified on the four sides of the plot 
    bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE,
    family='Quattrocento'
)

plot(NULL, xlim=c(1, dim(t3)[1]), ylim=c(y.min, y.max), yaxt = 'n', xaxt = 'n') 

title(ylab='Percentage vaccine hesitant', font=2, cex.lab=1.15)

axis (2, 
      line = 0, # position
      tck = -0.01,
      lwd = 1*s,
      col = 'grey', # the actual axis (line) 
      col.axis = dark.color, # colors of the actual labels
      cex.axis = 1, 
      font=2, # font type (bold)
      at=seq(y.min, y.max, 10), # where to put labels  
      labels= paste0(seq(y.min, y.max, 10), "%"), # text of labels 
      las=1 # orientation of the labels
)


abline(h=seq(0,100,10), col='grey', lwd=0.5*s)
abline(v=seq(1.5,28,1), col='grey', lwd=0.5*s)

segments(x0=(1:dim(t3)[1])-0.25, x1=(1:dim(t3)[1])-0.25, y1=t3$X2.x*100, y0=t3$X3.x*100, col='red', lwd=1.9)
segments(x0=(1:dim(t3)[1])+0.25, x1=(1:dim(t3)[1])+0.25, y0=t3$X2.y*100, y1=t3$X3.y*100, col='blue', lwd=1.9)

points(x=(1:dim(t3)[1])-0.25, y=t3$X2.x*100, col='red', bg='red', cex=1.2, pch=25)
points(x=(1:dim(t3)[1])-0.25, y=t3$X3.x*100, col='red', bg='red', cex=1.2, pch=19)
points(x=(1:dim(t3)[1])+0.25, y=t3$X2.y*100, col='blue', bg='white', cex=1.2, pch=25)
points(x=(1:dim(t3)[1])+0.25, y=t3$X3.y*100, col='blue', bg='white', cex=1.2, pch=1)

par(xpd = TRUE)
for (i in 1:dim(t3)[1]){
  text (t3$X1[i], x = i, y = 0 - 1.5, font=2, cex=1, col='black')
}  

points (x= 1.75, y=18, pch=25, col='red', bg='red',cex=1.4)
points (x= 1.75, y=16, pch=19, col='red', bg='red', cex=1.4)
points (x= 1.75, y=14, pch=25, col='blue',  bg='white',cex=1.4)
points (x= 1.75, y=12, pch=1, col='blue',  bg='white',cex=1.4)

text (x= 1.75, y=18, "women in February 2021", col='red', cex=0.75, srt=0, pos=4)
text (x= 1.75, y=16, "men in February 2021", col='red', cex=0.75, srt=0, pos=4)
text (x= 1.75, y=14, "women in May 2021", col='blue', cex=0.75, srt=0, pos=4)
text (x= 1.75, y=12, "men in May 2021", col='blue', cex=0.75, srt=0, pos=4)

dev.off()
# GAM models of age and attitudes per gender ---------------------------------------------------------------
d.m<-d1[d1$male==1, ]
d.w<-d1[d1$male==0, ]
e.m<-e2[e2$male==1, ]
e.w<-e2[e2$male==0, ]

gam.m <- mgcv::gam(hesitant ~ s(age), data = d.m,family = binomial,method = "REML")
gam.w <- mgcv::gam(hesitant ~ s(age), data = d.w,family = binomial,method = "REML")
plotdata.m = plot(gam.m,  trans = plogis, shift = coef(gam.m)[1], seWithMean = TRUE, rug=F)
plotdata.w = plot(gam.w,  trans = plogis, shift = coef(gam.w)[1], seWithMean = TRUE, rug=F)
y.m<-plogis(plotdata.m[[1]]$fit+coef(gam.m)[1])
y.w<-plogis(plotdata.w[[1]]$fit+coef(gam.w)[1])

###
gam2.m <- mgcv::gam(never ~ s(age), data = d.m,family = binomial,method = "REML")
gam2.w <- mgcv::gam(never ~ s(age), data = d.w,family = binomial,method = "REML")
plotdata2.m = plot(gam2.m,  trans = plogis, shift = coef(gam2.m)[1], seWithMean = TRUE, rug=F)
plotdata2.w = plot(gam2.w,  trans = plogis, shift = coef(gam2.w)[1], seWithMean = TRUE, rug=F)
y2.m<-plogis(plotdata2.m[[1]]$fit+coef(gam2.m)[1])
y2.w<-plogis(plotdata2.w[[1]]$fit+coef(gam2.w)[1])

###
egam.m <- mgcv::gam(hesitant ~ s(age), data = e.m,family = binomial,method = "REML")
egam.w <- mgcv::gam(hesitant ~ s(age), data = e.w,family = binomial,method = "REML")
eplotdata.m = plot(egam.m,  trans = plogis, shift = coef(egam.m)[1], seWithMean = TRUE, rug=F)
eplotdata.w = plot(egam.w,  trans = plogis, shift = coef(egam.w)[1], seWithMean = TRUE, rug=F)
ey.m<-plogis(eplotdata.m[[1]]$fit+coef(egam.m)[1])
ey.w<-plogis(eplotdata.w[[1]]$fit+coef(egam.w)[1])

###
egam2.m <- mgcv::gam(never ~ s(age), data = e.m,family = binomial,method = "REML")
egam2.w <- mgcv::gam(never ~ s(age), data = e.w,family = binomial,method = "REML")
eplotdata2.m = plot(egam2.m,  trans = plogis, shift = coef(egam2.m)[1], seWithMean = TRUE, rug=F)
eplotdata2.w = plot(egam2.w,  trans = plogis, shift = coef(egam2.w)[1], seWithMean = TRUE, rug=F)
ey2.m<-plogis(eplotdata2.m[[1]]$fit+coef(egam2.m)[1])
ey2.w<-plogis(eplotdata2.w[[1]]$fit+coef(egam2.w)[1])
# Figure 2 age and attitudes per gender ---------------------------------------------------------------
tiff ('./figures/F_sex_age_edu_gams2_publication.tiff', width=12.8*s, height=9.055*s, units = 'in', res=300)

par(mfrow=c(2,2), # number and distribution of plots
    oma=c(0,0,1,0), # size of the outer margins in lines of text
    mar=c(4,4,1,1), # number of lines of margin to be specified on the four sides of the plot 
    #bty='n', # no box
    cex = 1.25*s, # magnification of text and symbols
    xpd = FALSE, # clipping of plotting to the figure region
    ann = FALSE,
    family = "Quattrocento"
)

# plot 1
plot(x=eplotdata.m[[1]]$x[1:83], y=ey.m[1:83], col='blue', type='l', ylim=c(0,0.50),  xlim=c(14,80), lwd=3*s, xlab = 'Age (in years)', ylab='Probability of vaccine hesitancy')
lines(x=eplotdata.w[[1]]$x[1:87], y=ey.w[1:87], col='red', lwd=3*s, lty=2)

title(ylab='Probability of vaccine hesitancy, Feb 21', xlab = "Age (in years)", cex.lab=0.9, line=2.5)
text ('Men', x=80, y=0.114, col='blue', cex=0.9, adj=1)
text ('Women', x=80, y=0.26, col='red', cex=0.9, adj=1)

# plot 2
plot(x=eplotdata2.m[[1]]$x[1:83], y=ey2.m[1:83], col='blue', type='l', ylim=c(0,0.20),  xlim=c(14,80), lwd=3*s, xlab = 'Age (in years)', ylab='Probability of vaccine refusal')
lines(x=eplotdata2.w[[1]]$x[1:87], y=ey2.w[1:87], col='red', lwd=3*s, lty=2)

title(ylab='Probability of vaccine refusal, Feb 21', xlab = "Age (in years)", cex.lab=0.9, line=2.5)
text ('Men', x=80, y=0.041, col='blue', cex=0.9, adj=1)
text ('Women', x=80, y=0.098, col='red', cex=0.9, adj=1)

# plot 3
plot(x=plotdata.m[[1]]$x[1:83], y=y.m[1:83], col='blue', type='l', ylim=c(0,0.40),  xlim=c(14,80), lwd=3*s, xlab = 'Age (in years)', ylab='Probability of vaccine hesitancy')
lines(x=plotdata.w[[1]]$x[1:87], y=y.w[1:87], col='red', lwd=3*s, lty=2)

title(ylab='Probability of vaccine hesitancy, May 21', xlab = "Age (in years)", cex.lab=0.9, line=2.5)
text ('Men', x=80, y=0.04, col='blue', cex=0.9, adj=1)
text ('Women', x=80, y=0.158, col='red', cex=0.9, adj=1)

# plot 4
plot(x=plotdata2.m[[1]]$x[1:83], y=y2.m[1:83], col='blue', type='l', ylim=c(0,0.15),  xlim=c(14,80), lwd=3*s, xlab = 'Age (in years)', ylab='Probability of vaccine refusal')
lines(x=plotdata2.w[[1]]$x[1:87], y=y2.w[1:87], col='red', lwd=3*s, lty=2)

title(ylab='Probability of vaccine refusal, May 21', xlab = "Age (in years)", cex.lab=0.9, line=2.5)
text ('Men', x=80, y=0.025, col='blue', cex=0.9, adj=1)
text ('Women', x=80, y=0.067, col='red', cex=0.9, adj=1)

dev.off()
# Comparison of relevant attitudes and beliefs per gender ---------------------------------------------------------------
# trust in health and doctors
prop.test(table(e2$trust.health.info, e2$male)[2,], table(d1$male))
prop.test(table(e2$trust.doctors.info, e2$male)[2,], table(d1$male))

prop.test(table(e2$trust.health, e2$male)[2,], table(d1$male))

prop.test(table(d1$trust.health.info, d1$male)[2,], table(d1$male))
prop.test(table(d1$trust.doctors.info, d1$male)[2,], table(d1$male))

# media consumption
prop.test(table(e2$trust.internet, e2$male)[2,], table(d1$male))
prop.test(table(e2$trust.networks, e2$male)[2,], table(d1$male))

prop.test(table(e2$trust.web.info, e2$male)[2,], table(d1$male))
prop.test(table(e2$trust.networks.info, e2$male)[2,], table(d1$male))
prop.test(table(e2$trust.people.info, e2$male)[2,], table(d1$male))

prop.test(table(d1$trust.web.info, d1$male)[2,], table(d1$male))
prop.test(table(d1$trust.networks.info, d1$male)[2,], table(d1$male))
prop.test(table(d1$trust.people.info, d1$male)[2,], table(d1$male))

# shares of vaccine beliefs per gender
prop.test(table(d1$fear.covid, d1$male)[2,], table(d1$male))
prop.test(table(d1$can.avoid.covid, d1$male)[2,], table(d1$male))

prop.test(table(e2$v.onlyway, e2$male)[2,], table(e2$male))
prop.test(table(d1$v.onlyway, d1$male)[2,], table(d1$male))

prop.test(table(e2$v.tooquick, e2$male)[2,], table(e2$male))
prop.test(table(d1$v.toofast, d1$male)[2,], table(d1$male))

prop.test(table(e2$v.sideeffects, e2$male)[2,], table(e2$male))
prop.test(table(d1$v.sideeffects, d1$male)[2,], table(d1$male))

prop.test(table(d1$v.safe, d1$male)[2,], table(d1$male))
prop.test(table(d1$v.effective, d1$male)[2,], table(d1$male))

prop.test(table(d1$v.eu.safe, d1$male)[2,], table(d1$male))

prop.test(table(d1$v.benefits, d1$male)[2,], table(d1$male))

prop.test(table(d1$v.duty, d1$male)[2,], table(d1$male))
prop.test(table(d1$v.compulsory, d1$male)[2,], table(d1$male))

# Logistic regression models of vaccine hesitancy (Table 2) ----------------------------------------------
d1f <- remove_all_labels(d1)
summary(gm1<-glm(hesitant ~ male, data=d1f, family='binomial')) # - 0.268
summary(gm2<-glm(hesitant ~ male + age + edu + empl + city + country, data=d1f, family='binomial')) #-0.289
summary(gm3<-glm(hesitant ~ male + age + edu + empl + city + country +
                   trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                   trust.media.info + trust.web.info + trust.networks.info + trust.people.info, data=d1f, family='binomial')) #-0.258
summary(gm4<-glm(hesitant ~ male + age + edu + empl + city + country +
                   trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                   trust.media.info + trust.web.info + trust.networks.info + trust.people.info +
                   v.safe + v.effective + fear.covid + can.avoid.covid + v.toofast + v.sideeffects, data=d1f, family='binomial')) #-0.258

summary(gm5<-glm(hesitant ~ male + age + edu + empl + city + country + 
                   trust.eu.info + trust.gov.info + trust.health.info + trust.local.info + trust.doctors.info + 
                   trust.media.info + trust.web.info + trust.networks.info + trust.people.info +
                   v.safe + v.effective + fear.covid + can.avoid.covid + v.toofast + v.sideeffects + v.benefits, data=d1f, family='binomial')) #-0.14

summary(gm6<-glm(hesitant ~ male + age*male + edu*male + empl*male + city*male + country*male + 
                   trust.eu.info*male + trust.gov.info*male + trust.health.info*male + trust.local.info*male + trust.doctors.info*male + 
                   trust.media.info*male + trust.web.info*male + trust.networks.info*male + trust.people.info*male +
                   v.safe*male + v.effective*male + fear.covid*male + can.avoid.covid*male + v.toofast*male + v.sideeffects*male + v.benefits*male, data=d1f, family='binomial')) #-0.14

t1 <- tbl_regression(gm1,  exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels() 
t2 <- tbl_regression(gm2, exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels()
t3 <- tbl_regression(gm3, exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels()
t4 <- tbl_regression(gm4, exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels()
t5 <- tbl_regression(gm5, exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels()

tbl <- tbl_merge(tbls = list(t1, t2, t3, t4, t5), tab_spanner = c("*Model 1**", "**Model 2**", "**Model 3**", "**Model 4**", "**Model 5**"))
tbl %>%
  as_gt() %>%
  gt::gtsave(filename = "./tables/gender_models_table1.html") 

# Logistic regression models of vaccine refusal (Table A1) ----------------------------------------------
gmr1<-update(gm1, never ~.)
gmr2<-update(gm2, never ~.)
gmr3<-update(gm3, never ~.)
gmr4<-update(gm4, never ~.)
gmr5<-update(gm5, never ~.)

tr1 <- tbl_regression(gmr1,  exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels() 
tr2 <- tbl_regression(gmr2, exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels()
tr3 <- tbl_regression(gmr3, exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels()
tr4 <- tbl_regression(gmr4, exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels()
tr5 <- tbl_regression(gmr5, exponentiate = FALSE, pvalue_fun = ~style_pvalue(.x, digits = 2)) %>% modify_column_hide(column=ci)  %>% bold_p(t = 0.10)  %>% italicize_levels()

tbl2 <- tbl_merge(tbls = list(tr1, tr2, tr3, tr4, tr5), tab_spanner = c("*Model 1**", "**Model 2**", "**Model 3**", "**Model 4**", "**Model 5**"))
tbl2 %>%
  as_gt() %>%
  gt::gtsave(filename = "./tables/gender_models_table2.html") 

