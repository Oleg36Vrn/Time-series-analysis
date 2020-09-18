### ГЛАВА 1 ###
'I love statistics and econometrics'
3+2
11*5
sqrt(81)
3*2 == 8
3*2 == 6
Z <- 78
78 -> Z
Z
sum (3,6,8)
90:99
seq(90,99)
sentence <- c('mack', 'the', 'knife')
sentence[3]
sentence[c(1,3)]
ranks <- 1:3
names(ranks) <- c('first', 'second', 'third')
ranks
ranks['first']
GDP <- c(1.17, 0.46, 0.85, 1.24, 1.25, 0.74, 1.45, 0.71, 0.92, 1.06)
InfantMortality <- c(3.4, 7.6, 2.4, 4.0, 3.2, 2.7, 3.3, 3.8, 2.8, 3.5)
base1 <- data.frame (GDP, InfantMortality)
base1
base1[9, 2]
base1$GDP
base1 [2, ]
base1 [ ,2]
settings <- c("Бельгия", "Болгария", "Чехия", "Дания", "Германия",
              "Эстония", "Ирландия", "Греция", "Испания", "Франция")
settings <-factor(settings)
settings
base2 <- data.frame(settings, GDP, InfantMortality)
base2
library(foreign)
read.dta(file="nameofstatafile.dta")
read.dta(file, convert.dates = TRUE, missing.type = FALSE,
         convert.underscore = TRUE, warn.missing.labels = TRUE)
read.spss(file='nameofspssfile.sav', use.value.labels = TRUE,
          to.data.frame = TRUE)
read.spss(file, use.value.labels = TRUE, to.data.frame = TRUE,
          max.value.labels = Inf, trim.factor.names = FALSE, trim_values = TRUE,
          reencode = NA, use.missings = to.data.frame)
?read.dta
?read.spss
yourfilename <- read.table('yourfilename.txt', header=T)
attach(yourfilename)
names(yourfilename)
dataname <- read.table(«dataname.csv», header=TRUE, sep = «,»)
attach(dataname)
names(dataname)
mean(base1$InfantMortality)
median(base1$InfantMortality)
sd(base1$InfantMortality)
min(base1$InfantMortality)
max(base1$InfantMortality)
v<- InfantMortality
getmode <- function(v) {
  + uniqv <- unique(v)
  + uniqv[which.max(tabulate(match(v, uniqv)))]}
getmode(v)
f <- base1
f
glimpse(f)
head(f)
tail(f)
describe(f)
v <- c(2, 3, 4, 5, 5, 7, 7, 8, 9, 10)
qplot(data=f, v, xlab="Эксперты", ylab="Число экспертов",
      main="Гистограмма оценки экспертов качества продукта")
f <- data.frame (GDP, InfantMortality, v)
base1 <- data.frame (GDP, InfantMortality)
hist(f$v, probability=TRUE, col="blue")
bull <- fitdistr(f$v, 'weibull')
bull$estimate
xvals <- seq(0, 10, 2)
lines (xvals, dweibull(xvals, shape=bull$estimate[1],
                       scale=bull$estimate[2]))
### ГЛАВА 2 ###
N <- 1:36
Urojainost <- c(10,
7,
20,
14,
14,
12,
10,
23,
17,
20,
14,
13,
11,
17,
21,
11,
16,
14,
17,
17,
19,
21,
7,
13,
0,
1,
7,
2,
3,
1,
2,
1,
3,
0,
1,
4)
VidUdobrenie <- c("A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C")
base2<-data.frame(N, Urojainost, VidUdobrenie)
N <- N + 37:72
N <- 1:72
N
Urojainost <- c(10,
                7,
                20,
                14,
                14,
                12,
                10,
                23,
                17,
                20,
                14,
                13,
                11,
                17,
                21,
                11,
                16,
                14,
                17,
                17,
                19,
                21,
                7,
                13,
                0,
                1,
                7,
                2,
                3,
                1,
                2,
                1,
                3,
                0,
                1,
                4,
                3,
                5,
                12,
                6,
                4,
                3,
                5,
                5,
                5,
                5,
                2,
                4,
                3,
                5,
                3,
                5,
                3,
                6,
                1,
                1,
                3,
                2,
                6,
                4,
                11,
                9,
                15,
                22,
                15,
                16,
                13,
                10,
                26,
                26,
                24,
                13)
VidUdobrenie <- c("A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "A",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "B",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  "C",
                  'D',
                  'D',
                  'D',
                  'D',
                  'D',
                  'D',
                  'D',
                  'D',
                  'D',
                  'D',
                  'D',
                  'D',
                  'E',
                  'E',
                  'E',
                  'E',
                  'E',
                  'E',
                  'E',
                  'E',
                  'E',
                  'E',
                  'E',
                  'E',
                  'F',
                  'F',
                  'F',
                  'F',
                  'F',
                  'F',
                  'F',
                  'F',
                  'F',
                  'F',
                  'F',
                  'F')
base2<-data.frame(N, Urojainost, VidUdobrenie)
base2
head(base2)
tail(base2)
attach(base2)
str(base2)
tapply(Urojainost, VidUdobrenie, mean)
tapply(Urojainost, VidUdobrenie, var)
tapply(Urojainost, VidUdobrenie, length)
boxplot(Urojainost ~ VidUdobrenie)
aov.out = aov(Urojainost ~ VidUdobrenie, data= base2)
summary(aov.out)
### ГЛАВА 3 ###
N <- 1:47
Country <- c('Norway',
             'Australia',
             'Switzerland',
             'Denmark',
             'Netherlands',
             'Germany',
             'Ireland',
             'United States',
             'Canada',
             'New Zealand',
             'Singapore',
             'Hong Kong, China (SAR)',
             'Sweden',
             'United Kingdom',
             'Iceland',
             'Korea (Republic of)',
             'Israel',
             'Luxembourg',
             'Japan',
             'Belgium',
             'France',
             'Austria',
             'Finland',
             'Slovenia',
             'Spain',
             'Italy',
             'Czech Republic',
             'Greece',
             'Estonia',
             'Brunei Darussalam',
             'Cyprus',
             'Qatar',
             'Slovakia',
             'Poland',
            'Lithuania',
             'Malta',
             'Saudi Arabia',
             'Argentina',
             'United Arab Emirates',
             'Chile',
             'Portugal',
             'Hungary',
             'Bahrain',
             'Latvia',
             'Croatia',
             'Kuwait',
             'Montenegro')
GNI  <-                     c(64992,
                             42261,
                             56431,
                             44025,
                             45435,
                             43919,
                             39568,
                             52947,
                             42155,
                             32689,
                             76628,
                             53959,
                             45636,
                             39267,
                             35182,
                             33890,
                             30676,
                             58711,
                             36927,
                             41187,
                             38056,
                             43869,
                             38695,
                             27852,
                             32045,
                             33030,
                             26660,
                             24524,
                             25214,
                             72570,
                             28633,
                             123124,
                             25845,
                             23177,
                             24500,
                             27930,
                             52821,
                             22050,
                             60868,
                             21290,
                             25757,
                             22916,
                             38599,
                             22281,
                             19409,
                             83961,
                             14558)
Employment   <-             c(62.6,
                              61.5,
                              65.2,
                              58.1,
                              60.1,
                              56.7,
                              52.6,
                              57.8,
                              61.5,
                              63.6,
                              65.9,
                              57,
                              58.9,
                              57.4,
                              69.8,
                              59.1,
                              59.4,
                              54.2,
                              56.8,
                              48.8,
                              50.1,
                              58,
                              54.9,
                              51.8,
                              43.3,
                              43.1,
                              55.4,
                              38.7,
                              56.5,
                              61.6,
                              53.6,
                              86.2,
                              51.1,
                              50.7,
                              53.8,
                              48.6,
                              51.8,
                              56.2,
                              76.9,
                              58.1,
                              50.4,
                              46.6,
                              65,
                              53.8,
                              42.2,
                              66.3,
                              40.1)
base3 <- data.frame(Country, Employment, GNI)
ggplot() + geom_point(aes(x=base3$Employ, y=base3$GNI), size = 2) +
  ylab("ВНД на душу населения, $ по ППС в ценах 2011 года")+
  xlab("Уровень занятости,%")+
  labs(title="Корреляционное поле")
shapiro.test(base3$Employment)
shapiro.test(base3$GNI)
cor(GNI, Employment)
model <- lm(data=base3, GNI~Employment)
model$coefficients
options(digits = 4)
model$residuals[1:8]
summary(model)
confint(model, level = 0.95)
options(digits = 6)
head(fitted(model))
RSS <- deviance(model)
RSS
y <- (base3$GNI)
TSS <- sum((y-mean(y))^2)
TSS
SSE <- TSS-RSS
SSE
ggplot() + geom_point(aes(x=N, y=model$residuals), size = 2)
bptest(model, data=base3, studentize=FALSE)
newdata = data.frame(Employment = 75)
predict(model, newdata)
predict(model, newdata, interval="predict")
### ГЛАВА 3 ###
N <- 1:39
Country <- c('Norway',
             'Australia',
             'Switzerland',
             'Denmark',
             'Netherlands',
             'Germany',
             'Ireland',
             'United States',
             'Canada',
             'New Zealand',
             'Singapore',
             'Sweden',
             'United Kingdom',
             'Iceland',
             'Korea (Republic of)',
             'Israel',
             'Japan',
             'Belgium',
             'France',
             'Austria',
             'Finland',
             'Slovenia',
             'Spain',
             'Italy',
             'Czech Republic',
             'Greece',
             'Estonia',
             'Cyprus',
             'Slovakia',
             'Poland',
             'Lithuania',
             'Malta',
             'Saudi Arabia',
             'Chile',
             'Portugal',
             'Hungary',
             'Latvia',
             'Croatia',
             'Kuwait')
HDI <- c(94.4,
         93.5,
         93,
         92.3,
         92.2,
         91.6,
         91.6,
         91.5,
         91.3,
         91.3,
         91.2,
         90.7,
         90.7,
         89.9,
         89.8,
         89.4,
         89.1,
         89,
         88.8,
         88.5,
         88.3,
         88,
         87.6,
         87.3,
         87,
         86.5,
         86.1,
         85,
         84.4,
         84.3,
         83.9,
         83.9,
         83.7,
         83.2,
         83,
         82.8,
         81.9,
         81.8,
         81.6)
Research <- c(1.7,
              2.4,
              2.9,
              3,
              2.2,
              2.9,
              1.7,
              2.8,
              1.7,
              1.3,
              2.1,
              3.4,
              1.7,
              2.6,
              4,
              3.9,
              3.4,
              2.2,
              2.3,
              2.8,
              3.5,
              2.8,
              1.3,
              1.3,
              1.9,
              0.7,
              2.2,
              0.5,
              0.8,
              0.9,
              0.9,
              0.8,
              0.1,
              0.4,
              1.5,
              1.3,
              0.7,
              0.8,
              0.1)
Health <- c(9.6,
                  9,
                  11.5,
                  10.6,
                  12.9,
                  11.3,
                  8.9,
                  17.1,
                  10.9,
                  9.7,
                  4.6,
                  9.7,
                  9.1,
                  9.1,
                  7.2,
                  7.2,
                  10.3,
                  11.2,
                  11.7,
                  11,
                  9.4,
                  9.2,
                  8.9,
                  9.1,
                  7.2,
                  9.8,
                  5.7,
                  7.4,
                  8.2,
                  6.7,
                  6.2,
                  8.7,
                  3.2,
                  7.7,
                  9.7,
                  8,
                  5.7,
                  7.3,
                  2.9)
Services <- c(77.4,
                       75.5,
                       72.5,
                       77.5,
                       71.5,
                       70.2,
                       76.9,
                       81.2,
                       76.5,
                       72.5,
                       77.1,
                       77.9,
                       78.9,
                       75.8,
                       76.4,
                       77.1,
                       69.7,
                       77.1,
                       74.9,
                       68.9,
                       72.7,
                       60.3,
                       74.9,
                       68.5,
                       58.8,
                       70.3,
                       64.1,
                       76.9,
                       59.2,
                       57,
                       65.9,
                       76.4,
                       70.7,
                       66.4,
                       63.8,
                       64.9,
                       68.1,
                       58.7,
                       76)
Agriculture <- c(2.2,
                          3.3,
                          3.5,
                          2.6,
                          2.5,
                          1.5,
                          4.7,
                          1.6,
                          2.4,
                          6.6,
                          1.1,
                          2,
                          1.2,
                          5.5,
                          6.6,
                          1.7,
                          3.7,
                          1.2,
                          2.9,
                          4.9,
                          4.1,
                          8.3,
                          4.4,
                          3.7,
                          3.1,
                          13,
                          4.7,
                          2.9,
                          3.2,
                          12.6,
                          8.9,
                          1,
                          4.7,
                          10.3,
                          10.5,
                          5.2,
                          8.4,
                          13.7,
                          2.7)
Education <- c(6.6,
                       5.1,
                       5.3,
                       8.7,
                       5.9,
                       5,
                       6.2,
                       5.2,
                       5.3,
                       7.4,
                       2.9,
                       6.8,
                       6,
                       7.4,
                       4.9,
                       5.6,
                       3.8,
                       6.5,
                       5.7,
                       5.8,
                       6.8,
                       5.7,
                       5,
                       4.3,
                       4.5,
                       4.1,
                       5.2,
                       7.2,
                       4.1,
                       4.9,
                       5.2,
                       8,
                       5.1,
                       4.6,
                       5.3,
                       4.7,
                       4.9,
                       4.2,
                       3.8)
base4 <- data.frame(Country, HDI, Research, Health, Services, Agriculture, Education)
glimpse(base4)
MR <- data.frame(HDI, Research, Health, Services, Agriculture, Education)
MR
cor(MR)
sjp.corr(MR)
ggpairs(MR)
pairs(MR, panel = panel.smooth)
MLModel <- lm(data=base4, HDI~Research+Health+Services)
MLModel
summary(MLModel)
ndRG = data.frame(Research =2.1, Health = 9.6, Services = 77.7)
predict(MLModel, ndRG)
predict(MLModel, ndRG, interval="predict")
ndRG2 = data.frame(Research=c(2.1, 2.5), Health=c(9.6, 10.5), Services
                   =c(77.7, 79.1))
predict(MLModel, ndRG2)
predict(MLModel, ndRG2, interval="predict")
### ГЛАВА 4 ###
Year <- 1999:2015
RussianGDP <- c(4832.2,
                7305.6,
                8943.6,
                10830.5,
                13208.2,
                17027.2,
                21609.8,
                26917.2,
                22247.5,
                41276.8,
                38807.2,
                46308.5,
                59698.1,
                61798.3,
                62588.9,
                63031.1,
                60682.1)
D <- data.frame(Year, RussianGDP)
D
RussianGDP <- ts(D$RussianGDP, start=1999, end=2015, frequency=1)
RussianGDP
plot(RussianGDP)
t=(1:17)
lmGDP=lm(RussianGDP~t)
lmGDP
summary(lmGDP)
lmGDP=lm(RussianGDP~t)
summary(lmGDP)
TrendyGDP = -4937.8+4255.3*t
ts.plot(RussianGDP,TrendyGDP,col=1:2,ylab="TrendyGDP vs
RussianGDP")
dwtest(lmGDP)
RussianGDP1 <- ts(D$RussianGDP, start=2000, end=2015, frequency=1)
RussianGDP2 <- ts(D$RussianGDP, start=1999, end=2014, frequency=1)
t2=(1:16)
lmGDP2= lm(RussianGDP2~0+RussianGDP1+t2)
summary(lmGDP2)
dwtest(lmGDP2)
nd=data.frame(RussianGDP1=60682.1,t2=17)
predict(lmGDP2, nd, interval="predict")
dt=RussianGDP-TrendyGDP
plot(dt)
