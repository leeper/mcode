# Merge and recode across multiple variables

The **mcode** package aims to provide tools for easily merging and recoding across multiple variables, tasks that are commonly needed in the analysis of complex social science data (e.g., survey forms with branching, experimental designs involving analogous variables in each condition, etc.). The package is inspired, in part, by the `recode` function of the **[car](http://cran.r-project.org/web/packages/car/index.html)** package, but aims to be more flexible.


## Functionality

So far the package includes two functions:

### `mergeNA`
`mergeNA` merges vectors with mutually exclusive missingness. For example:

```
> mergeNA(c(1,2,NA,NA,NA),c(NA,NA,NA,4,5))
[1] 1 2 NA 4 5
```

This is useful when it is necessary to merge a potentially large number of variables (e.g., from branched survey questions) into a single variable.

### `mcode`

`mcode` is still experimental. Building on `car::recode`, the function aims to streamline recoding of variables in potentially complex ways. Where `car::recode` converts an input vector into an output vector following a set of recoding rules, `mcode` aims to recode an arbitrary number of vectors into a single output vector. For example one may want to create a categorical variable representing age and gender categories. Normally this would require two calls to `recode` and additional operations (e.g., `*`, `+`, or `interaction`). `mcode` will consolidate these steps into a single operation.
