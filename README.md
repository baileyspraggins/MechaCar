# Mecha Car
### MPG Regression
The multiple linear regression I created takes in account for all variables in the data frame. Our equation is 
mpg = vehicle length + vehicle weight + spoiler angle + ground clearance + AWD

In this regression we see that the slope coeficient, length, and ground clearance are all statisticall significant
at the at .05 significance level and thus provide a non-random amount of variance to the mpg values in the dataset.

Weight was signicant at a .1 level.

The spoiler angle and AWD variables however are not statistically significant at any level, but should still be left 
in the regression model to increase our accurancy.

This model does a fairly good job of predicting the mpg of MechaCar prototypes effectively becasue its R-squared values
its 0.7149.

### Suspension Coil Summary
A quick summary of the Suspension Coil's PSI is shown below

Mean: 1452

Median: 1500

Variance: 62.29356

Standard Deviation: 7.892627

The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not 
exceed 100 pounds per inch. The current manufacturing data meets these specifications as the variance is 62.29356 with the 
standard deviation only being around 8.

### Suspension Coil T-Test
In order to determine if the suspension coil's pound-per-inch results are statistically different from the mean population 
results a one sample t-test must be performed. In this test a sample with a size of 30 was taken from the original population.
In this sample the t-score was calculated to be 0.51762 thus giving a p-value of 0.6087.

Because the samples p-value (0.6087) is greater than .05 (95% confidence interval), we must fail to reject the null hypothesis,
thus coming to a conclusion that the sample is a good representation of the population.


### Ideas for Market Comparision
In order to compare performance of the MechaCar prototype vehicle to other comparable vehicles on the market. It is first necessary
to determine the vehicles price, and category (sports, luxury, suv, etc.). After we must gather data on other cars that match the 
type of car the prototype is. Below is the additional data that is necessary to collect.

Additional Data:
Top Speed
Fuel efficiency: Range
Consumer Cost
0-60 time
Manufacturing Costs
Vehicle Braking Distance

While you can make quite a few analysis using this additional data a good example of a posible analysis is doing a two-way ANOVA test.

The set up of the test is outlined as follows:

Dependent Variable: 0 - 60 time

Independent variables: Top Speed and Manufacturing Costs

Null Hypothesis: The mean 0 - 60 times in all vehicle groups are equal

Alternative Hypothesis: The mean 0 - 60 times in all vehicle groups are different
