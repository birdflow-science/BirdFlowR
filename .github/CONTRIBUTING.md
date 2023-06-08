# Contributing to BirdFlowR

We welcome contributions and suggestions!

## Proposing changes

* Feel free to propose changes by 
[submitting issues](https://github.com/birdflow-science/BirdFlowR/issues)
regardless of whether you intend to submit a pull request.  

* If you do intend to to submit a pull request submitting an
issue is a good way to get early feedback on your ideas before you write any code.

## Pull request process

We've documented our complete process but given our small size and paucity of pull requests we aren't sticklers and will consider any pull request with useful code even if it does not include everything here.

* **Fork and clone the package** onto your computer. If you haven't done this before you can use `usethis::create_from_github("birdflow-science/BirdFlowR", fork = TRUE)`.

* **Install development dependencies** with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
    
*   **Create a Git branch** for your pull request (PR). You can use: `usethis::pr_init("brief-description-of-change")` or your preferred git tools.

*   **Edit the code**. See also the [Code style] section below.

* **Create or update documentation**.  BirdFlowR uses [roxygen2](https://cran.r-project.org/package=roxygen2) to document functions in structured comments above the function code.  If you are adding a new function start documenting (in RStudio) by positioning the cursor in the function body and then selecting  "Insert Roxygen Skeleton" from the "Code" menu.  Currently all functions except trivial helper functions should be documented with Roxygen comments.  If the function is not meant to be public delete the "@export" from the Roxygen comment block and add "@keywords internal".

* **Add tests**. Tests use the [testthat](https://cran.r-project.org/package=testthat) package and are stored in "/tests/testhat/ "  with a file that starts with "test-" and the code's file name (usually the function name).  The easiest way to do this (in RStudio) is to have the new file open and active in the Source Editor Pane and then in the console run `usethis::usetest()` it will create the appropriate file and open it with an example test that you can then edit (or open the file if it already exists).  

  If your pull request addresses a bug or issue you may want to creat a failing test that demonstrates the issue before you make any changes to the package.

  You can run all tests on the package with `devtools::test()` or (in RStudio) 
  test the currently open file with the "Run Tests" button on the upper right of the source editor.  Tests will also be run while checking the package. 

* **Rebuild documentation** either using the "Document" item in the "More" menu within the "Build" tab in RStudio.. Ctr+Shift+D, or with `devtools::document()`.  This will convert the Roxygen comments into .Rd files within "man/".  The .Rd files should never be edited directly.  

* **Increment version** in the `DESCRIPTION` file.

* **Add news bullets** to the top of `NEWS.md` under a new heading for the 
  current version.  It's a good idea to use the "preview" button at the top
  of the code to verify that it is rendered correctly.

* **Lint** the function with for example `lintr::lint("R/function_name.R")`. Please 
do not lint unrelated code as that will make it harder to see what your pull
request is doing.

* **Check** the package with `devtools::check()`.

* **Commit** to git, and then **create a pull request**.  One way of doing this is
running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

* Optionally **check code coverage** About 10 minuntes after submitting your pull request your branch will appear near the top of the "Branch Context" drop down on the [coverage page](https://app.codecov.io/gh/birdflow-science/BirdFlowR). If your new code is not covered by tests consider adding additional tests and pushing a new commit to the pull request branch - this will update the pull request and rerun the coverage.


## Code style

* Code should follow the tidyverse [style guide](https://style.tidyverse.org). 
We use `lintr::lint()` to lint files with new code.  Do not lint otherwise 
unchanged code.

* In general each function is stored in a file that matches its name. This rule can be broken with closely related sets of exported functions and helper functions.

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.  

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.  

---

The first draft of this document was created by 
`usethis::use_tidy_contributing()`.  It has been heavily edited but some of the text
is still a direct copy of the
resulting tidyverse contributing guidelines.  Both **usethis** and **BirdFlowR** 
are released under an MIT license.


   
