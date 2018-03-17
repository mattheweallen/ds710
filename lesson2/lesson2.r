mycars <- read.csv(file="C:/Users/matt/source/repos/ds710/lesson2/Cars2005.csv")
attach(mycars)

# Print a list of types of cars that cost less than $10,000.

for(car_index in 1:length(Price)) { #iterate over the cars
    if(Price[car_index] < 10000) { #Check if car costs < $10,000
        #print(Type[car_index])
        
    } # end "if car costs < $10,000"
} # end iteration over cars