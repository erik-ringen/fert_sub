To run this code, you will need to [install R](https://www.r-project.org/) and the following R packages:
  
```
install.packages(c(
 "tidyverse",
    "targets",
    "rethinking",
    "rstan",
    "patchwork",
    "loo",
    "bayesplot",
    "HDInterval",
    "future",
    "furrr",
    "RColorBrewer")
```

You will also require the `rethinking` package, which you can install with the following code (see [here](https://github.com/rmcelreath/rethinking) for more details):
  
  ```
# from https://github.com/rmcelreath/rethinking
install.packages(c("coda", "mvtnorm", "devtools", "loo", "dagitty"))
devtools::install_github("rmcelreath/rethinking")
```

### Executing code

1. Set your working directory in R to this code repository
2. Load the `targets` package with `library(targets)`
3. To run all analyses, run `tar_make()`
4. To load individual targets into your environment, run `tar_load(targetName)`

To visualize how the various code and functions work together in the pipeline, run `targets::tar_visnetwork()`.

