## prepRECA

Contains code for adapting common formats used in European fisheries science to common use-cases of the R-ECA library for estimating catch at age for commercial fisheries.

R-ECA is available at: <https://github.com/NorskRegnesentral/Reca>. The simple installation routine described below works for Windows and Linux, for R-ECA also works on Mac, bur currently require customised compiling.

install Reca using (Windows or Linux):
devtools::install_github("https://github.com/NorskRegnesentral/Reca")

install prepRECA using:
devtools::install_github("https://github.com/edvinf/prepRECA")

in order to build the vignette (tutorial), run with options:
devtools::install_github("https://github.com/edvinf/prepRECA", build_opts = c("--no-resave-data", "--no-manual"))