# modeler

`modeler` is an R Package for helping users perform modeling procedures. There
are 4 main types of analysis in this package. 

1. Exploratory/Summary
2. Variable Transformations
3. Examining Relationships
4. Measuring Model Performance

Most ideas and functions in this package aren't novel but have been written to
make actions that I use frequently easier to use. A summary of the functions in
each section is included below. 

### Exploratory Analysis

Everyone has their own EDA steps and ideas but these are some I use a lot.

#### peruse

`peruse` examines the variables in a data frame and returns basic summary info
about the individual variables.

```r
cars_summary <- peruse(mtcars)
head(cars_summary)
#    Variable   Class    Type Num_Missing Num_Unique             data
#       <chr>   <chr>   <chr>       <chr>      <chr>           <list>
# 1       mpg Numeric Numeric           0         25 <tibble [1 x 7]>
# 2       cyl Numeric Numeric           0          3 <tibble [1 x 7]>
# 3      disp Numeric Numeric           0         27 <tibble [1 x 7]>
```

Extra information is returned in the `data` column but will differ between the
`Variable` types. Simply use `tidyr`'s `unnest` function to view the extra data.

```r
tidyr::unnest(cars_summary, data) %>% head(3)
#   Variable   Class    Type Num_Missing Num_Unique First_Quartile   Max  Mean
#      <chr>   <chr>   <chr>       <chr>      <chr>          <chr> <chr> <chr>
# 1      mpg Numeric Numeric           0         25          15.42  33.9 20.09
# 2      cyl Numeric Numeric           0          3              4     8 6.188
# 3     disp Numeric Numeric           0         27          120.8   472 230.7
#   Median   Min               SD Third_Quartile
#    <chr> <chr>            <chr>          <chr>
# 1   19.2  10.4  6.0269480520891           22.8
# 2      6     4 1.78592164694654              8
# 3  196.3  71.1 123.938693831382            326
```

The `profile` function will preform this on a single variable. 

#### sample_groups

`sample_groups` allows you to sample at the group level. So instead of using 
`sample_n` from dplyr to sample `n` observations you can use `sample_groups` to 
return a certain number of groups. This is usually applied when you have nested 
or hierarchical data. For example if I have a data set with attendance
information at a team-game-fan level I may want to return all attendance
observations from only a few games (for close data inspection or plotting). I
can use sample_groups to accomplish this in one step.

```r
single_iris <- sample_groups(iris, Species, n = 1)
table(single_iris$Species)
# setosa versicolor  virginica 
#      0         50          0 
```

#### helpers

I've also included various helper functions:

* `deciles` - returns the deciles of a numeric vector
* `how_many_nas` - returns how many nas are in each column of a data frame
* `multiplot` - plot multiple plots as one
* `tableNA` - table function that includes NA
* `view` - like head or tail but returns a random number of observations

### Variable Transformations

These aren't necessarily typical regression transformations like log or polynomial transformations. 

#### add_pca

`add_pca` will append a data frame with the pca loadings from a specified set 
of variables. You can include the new column names (or let them default to `.pc1`, `.pc2`, etc...) and specifiy the number of loadings to return (default is to include
all).

```r
add_pca(mtcars, mpg:wt, new_column = "car_specs", n = 3) %>% head(3)
#    mpg cyl disp  hp drat    wt  qsec vs am gear carb car_specs.pc1
# 1 21.0   6  160 110 3.90 2.620 16.46  0  1    4    4    -1.0177186
# 2 21.0   6  160 110 3.90 2.875 17.02  0  1    4    4    -0.9093556
# 3 22.8   4  108  93 3.85 2.320 18.61  1  1    4    1    -1.9968043
#   car_specs.pc2 car_specs.pc3
# 1    -0.1514974   -0.02970885
# 2    -0.1032240    0.14450082
# 3     0.2073380    0.07414376
```

#### impact_code

`impact_code` implements some of the methods in [this
paper](http://helios.mm.di.uoa.gr/~rouvas/ssi/sigkdd/sigkdd.vol3.1/barreca.pdf)
to deal with high cardinality categorical variables. The basic thought is that
it finds the average of the dependent variable for each level and then shrinks
that estimate to the overall average depending on how much evidence is in the
data. This is a data processing step that should be done on calibration data and
then applied on a training data set to be used in model building.

```r
impact_code(mtcars, am ~ cyl)
#     cyl  estimate
#   <dbl>     <dbl>
# 1     4 0.6844089
# 2     6 0.4294859
# 3     8 0.1771094
dplyr::group_by(mtcars, cyl) %>% dplyr::summarise(mean_am = mean(am))
#     cyl   mean_am
#   <dbl>     <dbl>
# 1     4 0.7272727
# 2     6 0.4285714
# 3     8 0.1428571
```

So the impact code estimates are all shrinked towards the overall mean of 40.6%

#### model_matrix

`model_matrix` is a wrapper around `model.matrix` but will preserve all levels 
of character or factor variables. 

```r
model.matrix(Petal.Width ~ Petal.Length + Species, data = iris) %>% head(3)
#   (Intercept) Petal.Length Speciesversicolor Speciesvirginica
# 1           1          1.4                 0                0
# 2           1          1.4                 0                0
# 3           1          1.3                 0                0
model_matrix(Petal.Width ~ Petal.Length + Species, .data = iris) %>% head(3)
#   (Intercept) Petal.Length Speciessetosa Speciesversicolor Speciesvirginica
# 1           1          1.4             1                 0                0
# 2           1          1.4             1                 0                0
# 3           1          1.3             1                 0                0
```

### Variable Relationships

#### find_woe

Weight of Evidence and Information Value are modeling tools that allow you to
measure the strength of the relationship between independent variables and
a dependent variable. This measure can detect non-linear relationships and 
handles categorical variables and missing values. 

```r
find_woe(mtcars, mpg:qsec, y = am)
#   variable       iv              data
#      <chr>    <dbl>            <list>
# 1      mpg 1.875599 <tibble [10 x 2]>
# 2      cyl 1.067032  <tibble [3 x 2]>
# 3     disp 1.959199 <tibble [10 x 2]>
# 4       hp 1.341193 <tibble [10 x 2]>
# 5     drat 2.513143 <tibble [10 x 2]>
# 6       wt 1.995249 <tibble [10 x 2]>
# 7     qsec 1.201222 <tibble [10 x 2]>
```
The `data` column contains the actual splits used. 

```r
find_woe(mtcars, mpg:qsec, y = am) %>% 
  dplyr::slice(2) %>% 
  tidyr::unnest(data)
#   variable       iv   bins         woe
#      <chr>    <dbl> <fctr>       <dbl>
# 1      cyl 1.067032      4  1.23357943
# 2      cyl 1.067032      6  0.09496181
# 3      cyl 1.067032      8 -1.26316168
```




