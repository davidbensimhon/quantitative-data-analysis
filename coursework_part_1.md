CS5801 Coursework Template Proforma
================
2039499

``` r
suppressMessages(library(e1071)) 
suppressMessages(library(tidyverse))
suppressMessages(library(geoR))
suppressMessages(library(rcompanion))
suppressMessages(library(ggpubr))
suppressMessages(library(cowplot))
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))
suppressMessages(library(caret))
suppressMessages(library(validate))
```

# 1. Organise and clean the data

## 1.1 Subset the data into the specific dataset allocated

``` r
#load the rda file
load(file = "CS5801_data.rda")

# Create a subset of the data with two teams based on the last two digits of my student ID (99 which becomes 90)
my_teams <- subset(CS5801.data, teamID.x=="KCA" | teamID.x=="BOS")

# Make a copy of the original dataframe that we can check back when necesssary after making any changes
df_copy <- subset(CS5801.data, teamID.x=="KCA" | teamID.x=="BOS")
```

## 1.2 Data quality analysis

# Implementation

1.  

``` r
#Examine the data structure and make sure the data has been read in correctly
str(my_teams)
```

    ## 'data.frame':    79 obs. of  15 variables:
    ##  $ playerID     : chr  "barnema01" "bettsmo01" "bogaexa01" "bradlja02" ...
    ##  $ teamID.x     : Factor w/ 149 levels "ALT","ANA","ARI",..: 16 16 16 16 16 16 66 66 16 16 ...
    ##  $ G            : int  32 145 156 74 45 18 45 140 80 2 ...
    ##  $ R            : int  0 92 84 43 0 0 6 101 35 0 ...
    ##  $ H            : int  0 174 196 55 0 0 17 169 69 0 ...
    ##  $ AB           : int  0 597 613 221 0 6 86 551 273 4 ...
    ##  $ RBI          : int  0 77 81 43 0 0 5 72 29 0 ...
    ##  $ weight       : int  210 180 210 200 190 190 205 205 195 220 ...
    ##  $ height       : int  76 69 73 70 72 75 73 74 69 75 ...
    ##  $ salary       : num  508500 514500 543000 528000 2000000 ...
    ##  $ birthDate    : Date, format: "1990-06-17" "1992-10-07" ...
    ##  $ career.length: num  0.312 0.509 1.366 1.752 9.443 ...
    ##  $ bats         : Factor w/ 3 levels "B","L","R": 3 3 3 2 2 2 3 3 3 2 ...
    ##  $ age          : num  24.5 22.2 22.3 24.7 34.4 ...
    ##  $ hit.ind      : num  0 1 1 1 0 0 1 1 1 0 ...

``` r
#it is good practice to quickly eyeball the dataframe
head(my_teams)
```

    ##     playerID teamID.x   G  R   H  AB RBI weight height   salary  birthDate
    ## 45 barnema01      BOS  32  0   0   0   0    210     76   508500 1990-06-17
    ## 64 bettsmo01      BOS 145 92 174 597  77    180     69   514500 1992-10-07
    ## 72 bogaexa01      BOS 156 84 196 613  81    210     73   543000 1992-10-01
    ## 81 bradlja02      BOS  74 43  55 221  43    200     70   528000 1990-04-19
    ## 84 breslcr01      BOS  45  0   0   0   0    190     72  2000000 1980-08-08
    ## 93 buchhcl01      BOS  18  0   0   6   0    190     75 12000000 1984-08-14
    ##    career.length bats      age hit.ind
    ## 45     0.3121150    R 24.54209       0
    ## 64     0.5092402    R 22.23409       1
    ## 72     1.3661875    R 22.25051       1
    ## 81     1.7522245    L 24.70363       1
    ## 84     9.4428474    L 34.39836       0
    ## 93     7.3757700    L 30.38193       0

``` r
#converting hit.ind into a factor
my_teams$hit.ind <- as.factor(my_teams$hit.ind)

#removes the extra levels from teamID
my_teams$teamID.x = droplevels(my_teams$teamID.x)

#convert hit.ind 0 into a no and hit.ind 1 into a yes
recode <- c(no = 0, yes = 1)
my_teams$hit.ind <- factor(my_teams$hit.ind, levels = recode, labels = names(recode))

#check for missing values
colSums(is.na(my_teams))
```

    ##      playerID      teamID.x             G             R             H 
    ##             0             0             0             0             0 
    ##            AB           RBI        weight        height        salary 
    ##             0             0             0             0             0 
    ##     birthDate career.length          bats           age       hit.ind 
    ##             0             0             0             0             0

2.  

``` r
#after viewing the dataset salary has been read in under scientific notation - the following code removes it
options(scipen = 999)

#change column names to make data more intuitive
colnames(my_teams)[3] <- 'games'
colnames(my_teams)[4] <- 'runs'
colnames(my_teams)[5] <- 'hits'
```

3.  

``` r
#confirming there are 3 values for bats - both arms, left arm and right arm
table(my_teams$bats)
```

    ## 
    ##  B  L  R 
    ##  7 19 53

``` r
#to examine the data and help spot outliers
summary(my_teams)
```

    ##    playerID         teamID.x     games             runs             hits       
    ##  Length:79          BOS:42   Min.   :  1.00   Min.   :  0.00   Min.   :  0.00  
    ##  Class :character   KCA:37   1st Qu.: 22.50   1st Qu.:  0.00   1st Qu.:  0.00  
    ##  Mode  :character            Median : 43.00   Median :  1.00   Median :  2.00  
    ##                              Mean   : 58.67   Mean   : 19.92   Mean   : 41.16  
    ##                              3rd Qu.: 91.50   3rd Qu.: 38.00   3rd Qu.: 68.50  
    ##                              Max.   :158.00   Max.   :101.00   Max.   :196.00  
    ##        AB             RBI            weight          height     
    ##  Min.   :  0.0   Min.   : -4.0   Min.   :165.0   Min.   :38.00  
    ##  1st Qu.:  0.0   1st Qu.:  0.0   1st Qu.:195.0   1st Qu.:71.00  
    ##  Median :  6.0   Median :  1.0   Median :210.0   Median :73.00  
    ##  Mean   :154.1   Mean   : 18.8   Mean   :212.5   Mean   :72.46  
    ##  3rd Qu.:298.0   3rd Qu.: 31.5   3rd Qu.:225.0   3rd Qu.:75.00  
    ##  Max.   :613.0   Max.   :108.0   Max.   :268.0   Max.   :82.00  
    ##      salary           birthDate          career.length     bats  
    ##  Min.   :     515   Min.   :1975-04-03   Min.   : 0.2902   B: 7  
    ##  1st Qu.:  561750   1st Qu.:1983-06-26   1st Qu.: 2.5968   L:19  
    ##  Median : 2000000   Median :1985-09-23   Median : 4.7310   R:53  
    ##  Mean   : 4620822   Mean   :1985-10-20   Mean   : 5.7752         
    ##  3rd Qu.: 7500000   3rd Qu.:1988-12-24   3rd Qu.: 8.3614         
    ##  Max.   :19750000   Max.   :2002-06-17   Max.   :35.6372         
    ##       age        hit.ind 
    ##  Min.   :22.23   no :37  
    ##  1st Qu.:26.17   yes:42  
    ##  Median :29.32           
    ##  Mean   :29.57           
    ##  3rd Qu.:31.70           
    ##  Max.   :39.75

``` r
#checks for duplicated vales
duplicated(my_teams$playerID)
```

    ##  [1] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [13] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [25] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [37] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [49] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
    ## [61] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE  TRUE FALSE FALSE
    ## [73] FALSE FALSE  TRUE FALSE FALSE FALSE FALSE

``` r
#finds the name of the player that is duplicated
my_teams$playerID[duplicated(my_teams$playerID)]
```

    ## [1] "pedrodu01" "moralke01"

``` r
#create a new dataframe with the duplicate rows removed
my_teams_1 <- my_teams[!duplicated(my_teams$playerID), ]
```

4.  

``` r
#I have created a new dataframe by using the transform function to replace negative values with NA. 
my_teams_2 <- transform(my_teams_1, RBI = ifelse(RBI < 0, NA, RBI))

View(my_teams_2)
#check constraints
val.check <- check_that(my_teams_2, 
                         age > 16 & age < 50,
                         height > 60,
                         AB >= hits,
                         career.length < age)

#we plot the data test to see if our checks failed or passed
barplot(val.check)
```

    ## Warning: The 'barplot' method for confrontation objects is deprecated. Use
    ## 'plot' instead

![](coursework_part_1_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

5.  

``` r
#any player under 60 inches will be converted into a NA value - I create a new dataset with the imputed value
my_teams_3 <- transform(my_teams_2, height = ifelse(height < 60, NA, height))

#replace values where career length is greater than age with NA
my_teams_4 <- transform(my_teams_3, career.length = ifelse(career.length < age, career.length, NA))
```

6.  

``` r
#helps us find which row this value is in
which(my_teams_4$birthDate == '2002-06-17')
```

    ## [1] 70

``` r
#converts our value into NA
my_teams_4$birthDate[70] <- NA
```

7.  

``` r
#examine salary
summary(my_teams_4$salary)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##      515   557000  2000000  4494090  7500000 19750000

``` r
#any salary under 300,000 will be converted into a NA value (reference_7)
my_teams_5 <- transform(my_teams_4, salary = ifelse(salary < 300000, NA, salary))
```

8.  

``` r
#remove birthdate as it is redundant in our model as we have age
my_teams_6 <- subset(my_teams_5, select = -birthDate )

#create a new variable hitRate
my_teams_6$hitRate <- my_teams_6$hits/my_teams_6$AB

#convert NaNs to 0
my_teams_6$hitRate[is.nan(my_teams_6$hitRate)]<-0

#remove hit.ind
my_teams_7 <- subset(my_teams_6, select = -hit.ind)

#at this point I can omit the rows with missing values
my_teams_clean <- na.omit(my_teams_7)
```

## 1.3 Data cleaning

1.  From comparing the structure to our metadata adendum I have
    identified two immediate problems with how our data has been read in
    by R - hit.ind should be a factor with two levels and teamID.x
    should be a factor with two levels. It is important for our analysis
    to correct these as we may want to understand the effects of each
    level on a variable. I removed them using the droplevels() function
    so they do not harm our analysis. (reference_12)

I used the str() function to look at the structure of the data and
ensure text variables are stored as text and numeric variables as
number. (reference_13). R has read in the rest of the data correctly
assigning numeric, char, Factor and Data appropriately.

Recoded the factors of hit.ind from 0(no hit) & 1(hit) into ‘no’ and
‘yes’ to make the variable more readable and intuitive (reference_13).

2.  After viewing the data I noticed salary was in scientific notation
    which I removed to make it easier for us to understand the numbers
    we are working with. I decided to make our dataset more intuitive
    and readable by changing a few of the column names.

3.  From the summary function I have spotted possible issues with:

    1.  RBI - having negative values (must be positive - set out in
        metadta)
    2.  height - having a min value of 38 inches - possible outlier.
    3.  salary - having a min value of 515 - possible outlier.
    4.  birthDate - having a max value of 2002-06-17 - possible outlier.
    5.  career.length - having a max value of 35.6372 - possible
        outlier.

I also checked for missing values and duplicated values. I use the
duplicated() function with player.id and found two players - “pedrodu01”
“moralke01” - that were duplicated. I removed the duplicates from our
data. (reference_14).

When I make any changes to the dataframe, I will create another
dataframe and not keep editing the original to make our code and data
re-traceable and easier to correct if any mistakes are made.

4.  After checking the metadata I have performed the following checks
    for referential constraints using the validate package
    (reference_12):
    1.  age is older than 16 and younger than 50.
    2.  Height is greater than 60 inches (average height is 72)
    3.  AB must be greater or equal to hits - as the player cannot hit
        without having the opportunity to hit.
    4.  career length must be less than age.
5.  Height has an outlier of 38 inches - It it safe to assume this is an
    error as 38 inches is 3 foot tall which is shorter than the shortest
    player in baseball history (note_2 & reference_1)

At this stage I have decided to convert these outliers to NA - NA is a
special character in R which will represent a missing value. The
benefits of using this character is the ability to use functions such as
is.na to detect NA’s. At a later stage I will decide what to do with
these NA’s - deletion or imputation.(reference_13)

‘Outliers do not mean errors’ (reference_13) - they could be interesting
edge cases and dont necessarily need to be removed, however, in our
cases each outlier has been proven to be an error/implausible which is
why I have convereted them to NA’s.

6.  Another obvious inconsistency is taking a players birthday and
    adding their age to make sure it does not pass 2020. Our dataset is
    small enough for me to eyeball and see that only one player fails
    this constraint which is our outlier on Birthdate. As the age falls
    well within our datasets range but the Birthdate falls well outside
    I have assumed that the error lies in birthDate and will now convert
    it into NA.

7.  As I am not familiar with baseball players salaries in the US, our
    dataset having such a large range made me feel the need to consult
    outside references to ensure the salary figures are plausible.

-   By checking I was able to determine the mean salary is suitable
    (reference_2)
-   I was able to determine that our maximum salary is suitable as it
    falls well below the max salary of any players (reference_3)
-   I was also able to determine that our outlier must be incorrect data
    as the minimum MLB salary from 2003-2020 was $300,000 (reference_4).
    Seeing as this player has played over a year we can safely assume
    this is an error in our dataset.

8.  I removed birthDate as it is superfluous given we have Age. I also
    explored creating a new variable hitRate dividing the number of hits
    by the opportunities to hit. I wanted to explore if this would be a
    good predictor for salary. The result was many NaN values as we are
    diving in many cases by 0 so I converted these to 0.

I removed hit.ind as whether a player makes a hit in the 2015 season or
not would not impact their salary for the 2015 season, if hit.ind was
from the previous year then it would make sense to include in our model.
I believe it is safe to assume salaries of the 2015 season are decided
before the 2015 season starts.

I will treat our missing values with case wise deletion as it should not
be problematic as we only have a few cases of missing variables -
specifically 7. I decided against imputation methods(mean,regression
etc) as I do not know for certain the values are missing at random,
moreover, when building regression models I do not want to affect the
dispersion or variance of our data.

# 2. Exploratory Data Analysis (EDA)

## EDA and summary of results

I checked the data visually using ggplot to build different graphs.

All of the histograms below show a rough bell curve implying normality
in our variables: games, weight, height, career.length and age.

``` r
#to remind me what data types I am working with
str(my_teams_clean)
```

    ## 'data.frame':    72 obs. of  14 variables:
    ##  $ playerID     : chr  "barnema01" "bettsmo01" "bogaexa01" "bradlja02" ...
    ##  $ teamID.x     : Factor w/ 2 levels "BOS","KCA": 1 1 1 1 1 1 2 2 1 1 ...
    ##  $ games        : int  32 145 156 74 45 18 45 140 80 2 ...
    ##  $ runs         : int  0 92 84 43 0 0 6 101 35 0 ...
    ##  $ hits         : int  0 174 196 55 0 0 17 169 69 0 ...
    ##  $ AB           : int  0 597 613 221 0 6 86 551 273 4 ...
    ##  $ RBI          : int  0 77 81 43 0 0 5 72 29 0 ...
    ##  $ weight       : int  210 180 210 200 190 190 205 205 195 220 ...
    ##  $ height       : int  76 69 73 70 72 75 73 74 69 75 ...
    ##  $ salary       : num  508500 514500 543000 528000 2000000 ...
    ##  $ career.length: num  0.312 0.509 1.366 1.752 9.443 ...
    ##  $ bats         : Factor w/ 3 levels "B","L","R": 3 3 3 2 2 2 3 3 3 2 ...
    ##  $ age          : num  24.5 22.2 22.3 24.7 34.4 ...
    ##  $ hitRate      : num  0 0.291 0.32 0.249 0 ...
    ##  - attr(*, "na.action")= 'omit' Named int [1:5] 71 73 74 75 76
    ##   ..- attr(*, "names")= chr [1:5] "921" "923" "935" "936" ...

``` r
#histogram for games played
ggplot(my_teams_clean, aes(x = games)) + geom_histogram(binwidth = 5, fill = 'steelblue') + ggtitle('Number Of Games Played') + xlab('Games') + ylab('Count') + theme_classic()
```

![](coursework_part_1_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#histogram for career length of players
ggplot(my_teams_clean, aes(x = career.length)) + geom_histogram(binwidth = 0.5, fill = 'steelblue') + ggtitle('Career Length of Players') + xlab('Lenght In Years') + ylab('Count') + theme_classic()
```

![](coursework_part_1_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
#histogram for age of players
ggplot(my_teams_clean, aes(x = age)) + geom_histogram(fill = 'steelblue') + ggtitle('Age of Players') + xlab('Age In Years') + ylab('Count') + theme_classic()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](coursework_part_1_files/figure-gfm/unnamed-chunk-11-3.png)<!-- -->

``` r
#histogram for the salary of players
ggplot(my_teams_clean, aes(x = salary)) + geom_histogram(fill = 'steelblue') + ggtitle('Salary of Players') + xlab('Salary In Dollars') + ylab('Count') + theme_classic()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](coursework_part_1_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#scatterplot of salary vs career length by team
ggplot(my_teams_clean, aes(x = salary, y = career.length )) + geom_point(aes(color = teamID.x)) + ggtitle('Salary Vs Career Length by Team') + xlab('Salary') + ylab('Career Length in Years') + theme_classic()
```

![](coursework_part_1_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

``` r
#scatterplot of salary vs games played by team
ggplot(my_teams_clean, aes(x = salary, y = games )) + geom_point(aes(color = teamID.x)) + ggtitle('Salary Vs Games Played by Team') + xlab('Salary') + ylab('Games Played') + theme_classic()
```

![](coursework_part_1_files/figure-gfm/unnamed-chunk-13-2.png)<!-- -->

``` r
#scatterplot of salary vs hits length by team
ggplot(my_teams_clean, aes(x = salary, y = hits )) + geom_point(aes(color = teamID.x)) + ggtitle('Salary Vs Hits by Team') + xlab('Salary') + ylab('Hits') + theme_classic()
```

![](coursework_part_1_files/figure-gfm/unnamed-chunk-13-3.png)<!-- -->

With the help of boxplots, we are able to understand the location,
dispersion and skewness of the dataset.

``` r
#boxplot of salary vs team
ggplot(my_teams_clean, aes(x = teamID.x, y = salary)) + geom_boxplot(aes(fill = teamID.x))  + ggtitle('Salary by Team') + xlab('BOS VS KCA') + ylab('Salary') + theme_classic()
```

![](coursework_part_1_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
#boxplot of games played per team
ggplot(my_teams_clean, aes(x = teamID.x, y = games)) + geom_boxplot(aes(fill = teamID.x))  + ggtitle('Games played per Team') + xlab('BOS vs KCA') + ylab('Games') + theme_classic()
```

![](coursework_part_1_files/figure-gfm/unnamed-chunk-14-2.png)<!-- -->

``` r
#boxplot of career length per team
ggplot(my_teams_clean, aes(x = teamID.x, y = career.length)) + geom_boxplot(aes(fill = teamID.x))  + ggtitle('Career Length per Team') + xlab('BOS vs KCA') + ylab('Years') + theme_classic()
```

![](coursework_part_1_files/figure-gfm/unnamed-chunk-14-3.png)<!-- -->
