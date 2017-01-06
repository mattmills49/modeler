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

Everyone has their own EDA packages and ideas and these are some I use a lot.

#### peruse

`peruse` examines the variables in a data frame and returns basic summary info
about the individual variables.

```
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

```
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


