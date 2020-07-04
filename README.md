# ptm
Custom Functions for the Martone Lab

To install: `devtools::install_github("martonelab/ptm")`

## Quick overview
Most of these functions help make your life easier when dealing with the masterlist, finding photos and getting higher taxonomic information.

- `masterlist()` - reads in the PTM Masterlist
- `taxonomy()` - Gets the higher taxonomy and authorship of the species using the Worms database (unfortunately AlgaeBase does not open up their API and this is the next best thing)

For more function information can be found on the website: https://martonelab.github.io/ptm/

## Bugs?
File them in the [issues](https://github.com/martonelab/ptm/issues) tab!

## How to contribute
- `fork` this repo on github to work on a copy of the package on your own account
- While you are writing new code, run: `devtools::document()` and `devtools::check()` to make sure nothing is broken
- Add the changes by submitting a `Pull Request`