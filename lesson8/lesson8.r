b_c = read_csv("C:/Users/Kyle Taysom/Documents/GitHub/ds710spring2018assignment8/Best Cities.csv")
#This code seems needed because I percieve the variables in this data frame are the scores and the observations are the cities, but tranforming this data frame makes the remainder of the assignment pretty confusing, so I'm sticking with the cities as variables but using the transformed dataset to find the min and max values
b_c.t = as.data.frame(t(b_c[,-1]))
colnames(b_c.t) = b_c$People
b_c.t
b_c


# Parsed with column specification:
#   cols(
#     People = col_character(),
#     `Madison city, Wisconsin` = col_double(),
#     `Minneapolis city, Minnesota` = col_double(),
#     `San Francisco city, California` = col_double(),
#     `Austin city, Texas` = col_double(),
#     `Philadelphia city, Pennsylvania` = col_double(),
#     `New York city, New York` = col_double(),
#     `Los Angeles city, California` = col_double(),
#     `Seattle city, Washington` = col_double(),
#     `Portland city, Oregon` = col_double(),
#     `Miami city, Florida` = col_double(),
#     `Charlottesville city, Virginia` = col_double()
#   )

temp_ranges = range(b_c.t$`Min Temp, F, 2014`)
low_temp_score = 10
high_temp_score = -10
scoring_frame = data.frame(temps = c(-22,46),temp_scores = c(10,-10))
temp_score_lm = lm(scoring_frame$temp_scores~scoring_frame$temps)
temp_score_lm

ppsm_range = range(b_c.t$`Population per square mile, 2010`)
low_pop_score = 10
high_pop_score = -10
scoring_frame$pops = c(2653,27013)
scoring_frame$pop_scores = c(10,-10)
pop_score_lm = lm(scoring_frame$pop_scores~scoring_frame$pops)
pop_score_lm

bike_range = range(b_c.t$`Bikeability Score`)
scoring_frame$bike = c(bike_range[2],bike_range[1])
scoring_frame$bike_score = c(-10,10)
bike_score_lm = lm(scoring_frame$bike_score~scoring_frame$bike)
bike_score_lm

mmoc_range = range(b_c.t$`Median selected monthly owner costs -with a mortgage, 2009-2013`)
scoring_frame$mmoc = c(mmoc_range[1],mmoc_range[2])
scoring_frame$mmoc_scores = c(10,-10)
mmoc_score_lm = lm(scoring_frame$mmoc_scores~scoring_frame$mmoc)
mmoc_score_lm

score_KTcities = function(city_vector) (
  if (!is.vector(city_vector)){
    print("argument is not a vector, please try again")
  } 
  else if (length(city_vector)!=18){
    print(c("Vector needs to have a length of 18, but is:",length(city_vector),"Please try again."))
  }
  else {
    score = 0
    score = score + 1.5*(city_vector[18]*(-0.2941)+5.5294) #score for min temp. cold keeps the riff raff out
    score = score + 2*(city_vector[12]*(-0.000821)+12.178161) #score for population density. I'm closterfobic
    score = score + 0.5*(city_vector[16]*(-0.6897)+45.8621) #score for bikeability. Ride on.
    score = score + city_vector[5]*(0.01011)+22.62892 #score for median monthly owner costs with mortgage
    return(score)
  }
)


city_results = apply(b_c[,-1],2,FUN = function(y) (score_KTcities(y)))
city_results

#
# a. We want to use R to assess whether it is plausible that John Hancock was "A
# Mourner", based on his mean word length. Explain why a 2-sided, 2-sample
# t-test is appropriate for this.¶ A 2 sample t-test is appropriate because we
# have two independent samples, one from "A Mourner" and one from "other
# articles". It's a 2 sided test because we are only asking if the mean word
# length and standard deviation are equal, leaving open the possiblity that they
# are either greater or lesser than.
#
# b. Explain why the t.test() function is not appropriate for the data we have
# available. The t.test() function is not appropriate because we don't have the
# raw data from the "other articles", we only have the mean, standard deviation,
# and number of words

#2c
#needed inputs are mean1, sd1, n1, mean2, sd2, n2
ab_ttest = function(mean1,sd1,n1,mean2,sd2,n2) (
  if (!is.numeric(c(mean1,sd1,n1,mean2,sd2,n2))) {
    print("All arguments must be numeric. Please try again.")
  }
  else {
    se = sqrt(((sd1^2)/n1+(sd2^2/n2)))
    t = (mean1-mean2)/se
    df_num = (((sd1^2)/n1) +((sd2^2)/n2))^2
    df_den_b1 = (((sd1^2)/n1)^2)/(n1-1)
    df_den_b2 = (((sd2^2)/n2)^2)/(n2-1)
    df = df_num/((df_den_b1+df_den_b2))
    p = 2*pt(-abs(t),df)
    return(p)
  }
)
#2d
#random sample 1
sample_1 = rnorm(1560,4.673077,2.336701)
sample_2 = rnorm(1210,4.69,2.60)

ttest_results = t.test(sample_1,y=sample_2,alternative = "two.sided", mu=0)
ttest_results

#I ran this section once with sample_1 mean = 4.7377526 sd=2.277025, n=1560; sample_2 mean = 4.71024, sd = 2.556446, n=1210
#The t.test results were 2438.62 degrees of freedom and a p value of 0.7702
#inputing the mean,sd, and n's into my function returned the same degrees of freedome and p value
#Every time you run this section you'll get different samples from rnorm

ab_ttest(mean1 = 4.737526,sd1=2.277025,n1=1560,mean2=4.71024,sd2=2.556446,n2=1210)


#2e
Mourner_wll = c(3, 7, 8, 3, 7, 3, 3, 6, 2, 3, 3, 2, 3, 4, 3, 8, 10, 2, 3, 3, 7, 4, 2, 10, 6, 3, 4, 9, 3, 6, 4, 2, 4, 2, 6, 4, 3, 8, 5, 2, 5, 4, 8, 11, 2, 6, 4, 4, 3, 3, 7, 2, 7, 3, 4, 2, 11, 2, 6, 5, 4, 8, 2, 3, 7, 2, 4, 6, 4, 3, 5, 6, 2, 3, 5, 10, 5, 6, 5, 4, 8, 8, 8, 2, 3, 8, 7, 2, 3, 6, 3, 6, 2, 3, 9, 3, 6, 4, 3, 3, 7, 3, 5, 2, 9, 3, 8, 8, 2, 6, 4, 3, 4, 5, 2, 3, 3, 4, 2, 7, 5, 6, 8, 4, 3, 7, 6, 6, 5, 2, 3, 6, 12, 7, 6, 2, 5, 5, 5, 6, 2, 5, 2, 3, 1, 7, 6, 3, 5, 4, 4, 1, 6, 3, 1, 7)

mean_Mwll = mean(Mourner_wll)
sd_Mwll = sd(Mourner_wll)
length_Mwll = length(Mourner_wll)
mean_Mwll
sd_Mwll
length_Mwll



# #Running the t-test
# In [26]:
#   #inputs for this test are mean1 = 4.69, sd1 = 2.60, n=121 for the "other articles" and
#   # mean2 = 4.69, sd2 = 2.60, n2=121 for "A Mourner" from lesson 7
#   p_value = ab_ttest(mean1 = 4.673077,sd1=2.336701,n1=156,mean2=4.69,sd2=2.60,n2=121)
# p_value