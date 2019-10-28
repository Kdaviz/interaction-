Writing Functions
================
Keyanna Davis
10/28/2019

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
set.seed(1)
```

We're going to write some functions

Here's zscores

``` r
x = rnorm( n = 30, mean = 5, sd = 2.3)

x_again =  rnorm( n = 30, mean = 6, sd = .3)

(x - mean(x)) / sd(x)
```

    ##  [1] -0.767120446  0.109493425 -0.993470503  1.637039863  0.267334741
    ##  [6] -0.977065475  0.438222871  0.709719461  0.533829741 -0.419692475
    ## [11]  1.546684110  0.332624325 -0.761479160 -2.485776741  1.128069748
    ## [16] -0.137851865 -0.106748415  0.932105430  0.799422547  0.553437533
    ## [21]  0.905205442  0.757128408 -0.008541293 -2.241925304  0.581490604
    ## [26] -0.149966223 -0.257816586 -1.680744021 -0.606639531  0.363029790

Now a function

``` r
z_score = function(x) {
  
 if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) > 3 ) {
    stop("Z scores cannot be computed for length 1 vectors")
  }
  
  (x-mean(x)) / sd(x)
  
  
}
```

Try out the function

``` r
#z_score(x=x_again)
#z_score(x=3)
#z_score(x = "my name is Keyanna")
#z_score(x = c(TRUE, TRUE, FALSE, TRUE))
#z_score(x = iris)
```

Multiple outputs
----------------

``` r
mean_and_sd = function(input_x) {
  if (!is.numeric(input_x)) {
    stop("x should be numeric")
  } else if (length(input_x) < 3) {
    stop("x should be longer than 3")
  }
  list( 
  mean_input = mean(input_x),
  sd_input =sd(input_x),
  z_score = (input_x - mean(input_x)) / sd(input_x)
  )
}
```

test this function

``` r
mean_and_sd(input_x = x_again)
```

    ## $mean_input
    ## [1] 6.039832
    ## 
    ## $sd_input
    ## [1] 0.2385965
    ## 
    ## $z_score
    ##  [1]  1.5413954 -0.2961850  0.3204956 -0.2345965 -1.8983946 -0.6887392
    ##  [7] -0.6627062 -0.2415224  1.2161757  0.7926368 -0.3738088 -0.4855096
    ## [13]  0.7093845  0.5329776 -1.0329536 -1.0565157  0.2914637  0.7993727
    ## [19] -0.3082034  0.9409190  0.3336151 -0.9364778  0.2619634 -1.5869527
    ## [25]  1.6348722  2.3231173 -0.6286716 -1.4797904  0.5493942 -0.3367558

Multiple inputs
