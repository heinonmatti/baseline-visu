
# To start data analysis, we set up the basics to enable compiling the document:
  
## Remember to tell R where your packages are.
# .libPaths("C:/rlibs/3.4.2")

# Due to some absurd reason, RStudio has started to set the working directory to a temp folder. Will set manually, then:
setwd("C:/LocalData/hema/git-projects/baseline-visu")

# If pacman-package is not installed, install it and then load it.
if (!require(pacman)) install.packages("pacman")
#library(pacman)

CRANpacks <- c("viridis", "bookdown", "knitr", "tidyverse", "haven", "lme4", 
               "userfriendlyscience", "ufs", "sm", "sjstats", "gridExtra", "igraph", 
               "devtools","EstimateGroupNetwork", "bootnet", "qgraph","rstanarm",
               "brms", "mlmRev", "rstan", "sandwich", "visreg", "broom", 
               "EstimateGroupNetwork", "bootnet", "qgraph", # for networks
               "corrgram", "sjPlot", "DT") 

instpacks <- setdiff(CRANpacks, pacman::p_library())

# Use pacman to install the needed packages.
if (length(instpacks)>0) install.packages(instpacks)

if (!require(papaja)) pacman::p_install_gh("crsh/papaja")
if (!require(mgm)) pacman::p_install_gh("jmbh/mgm")
if (!require(brmstools)) pacman::p_install_gh("mvuorre/brmstools")
if (!require(NetworkComparisonTest)) pacman::p_install_gh("sachaepskamp/NetworkComparisonTest")
if (!require(ggstatsplot)) pacman::p_install_gh("IndrajeetPatil/ggstatsplot")
if (!require(patchwork)) pacman::p_install_gh("thomasp85/patchwork") # so that plot1 + plot2 creates a single side-by-side plot

# Packages for the figures which show Bayesian credible intervals of classes
#pacman::p_install(c("rstanarm", "brms", "mlmRev"))

pacman::p_load(knitr, tidyverse)
# pacman::p_install_gh("clauswilke/ggridges")
library(ggridges)
library(patchwork)

knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE,
                      message = FALSE,
                      error = TRUE,
                      cache = FALSE, 
                      collapse = TRUE,
                      eval = TRUE,
                      dpi = 300)
knitr::opts_chunk$set(root.dir = ".")  # Always set project root as working directory
knitr::opts_knit$set(root.dir = ".")  # This is needed for some versions of RStudio
# knitr::opts_chunk$set(echo = TRUE, rows.print=15)

ggplot2::theme_set(papaja::theme_apa())

# This is supposed to center all data table cells...
options(DT.options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))

# Read data.

#lmi <- haven::read_sav("data/LMI_data_korjattu_syntaksilla_nimetpoistettu.sav", user_na = FALSE)

lmi <- haven::read_sav("data/LetsMoveIt_dataT1-T4_umlauts_removed_MANUALLY.sav", user_na = FALSE)
# ses <- haven::read_sav("data/baselinedata_vainnuoret SPSS_160226_accfac fixed.sav", user_na = FALSE)
# ses <- read_csv2("data/baselinedata_vainnuoret SPSS_160226_accfac fixed.csv")


# write_csv(lmi, "Z:/Desktop/LMI DATA/LMI_data_korjattu_syntaksilla_CSV.csv")

# to get rid of DLL warning, these don't work: 
# Sys.setenv("R_MAX_NUM_DLLS" = 1000)

# Unloading all packages gives LAPACK error:
# pkgs <- names(sessionInfo()$otherPkgs)
# pkgs <- paste('package:', pkgs, sep = "")
# lapply(pkgs, detach, character.only = TRUE, unload = TRUE)

# What seems to work is create a file .Renviron in the project root, which contains e.g. R_MAX_NUM_DLLS = 300.
# But better to have all code written as package::function() instead of function(), 
# and not calling most with library(package) at all.

## Package sm modifications

# The package sm has a nice tool for visualising densities and doing a bootstrap test on their equivalence. We modify the function so that we can change the color of the equivalence band.

# Changing the sm density compare function to allow different color of the band of equality. Copied from https://web.archive.org/web/20170222214214/https://stat.ethz.ch/pipermail/r-help//2009-March/416920.html.

sm.density.compare2 <- function (x, group, h, model = "none", bandcol =
                                   'cyan', lwd = par("lwd"), usePolyg = NULL, asp=NA, 
                                 xlab=opt$xlab, ylab=opt$ylab, ...) 
{
  if (!is.vector(x)) 
    stop("sm.density.compare can handle only 1-d data")
  opt <- sm:::sm.options(list(...))
  sm:::replace.na(opt, ngrid, 50)                 
  ## These all changed from replace.na() --> sm:::
  sm:::replace.na(opt, display, "line")
  sm:::replace.na(opt, xlab, deparse(substitute(x)))
  sm:::replace.na(opt, ylab, "Density")
  sm:::replace.na(opt, xlim, c(min(x) - diff(range(x))/4, max(x) + 
                                 diff(range(x))/4))
  sm:::replace.na(opt, eval.points, seq(opt$xlim[1], opt$xlim[2], 
                                        length = opt$ngrid))
  if (is.na(opt$band)) {
    if (model == "none") 
      opt$band <- FALSE
    else opt$band <- TRUE
  }
  if ((model == "none") && opt$band) 
    opt$band <- FALSE
  band <- opt$band
  ngrid <- opt$ngrid
  xlim <- opt$xlim
  nboot <- opt$nboot
  y <- x
  if (is.na(opt$test)) {
    if (model == "none") 
      opt$test <- FALSE
    else opt$test <- TRUE
  }
  if ((model == "none") && opt$test) 
    opt$test <- FALSE
  test <- opt$test
  if (opt$display %in% "none") 
    band <- FALSE
  fact <- factor(group)
  fact.levels <- levels(fact)
  nlev <- length(fact.levels)
  ni <- table(fact)
  if (band & (nlev > 2)) {
    cat("Reference band available to compare two groups only.", 
        "\n")
    band <- FALSE
  }
  if (length(opt$lty) < nlev) 
    opt$lty <- 1:nlev
  if (length(opt$col) < nlev) 
    opt$col <- 2:(nlev + 1)
  if (missing(h)) 
    h <- sm:::h.select(x, y = NA, group = group, ...)
  opt$band <- band
  opt$test <- test
  estimate <- matrix(0, ncol = opt$ngrid, nrow = nlev)
  se <- matrix(0, ncol = opt$ngrid, nrow = nlev)
  for (i in 1:nlev) {
    sm <- sm:::sm.density(y[fact == fact.levels[i]], h = h, display = "none", 
                          eval.points = opt$eval.points)
    estimate[i, ] <- sm$estimate
    se[i, ] <- sm$se
  }
  eval.points <- sm$eval.points
  if (!(opt$display %in% "none" | band)) {
    plot(xlim, c(0, 1.1 * max(as.vector(estimate))), xlab = opt$xlab, 
         ylab = opt$ylab, type = "n")
    #for (i in 1:nlev) lines(eval.points, estimate[i, ], lty = opt$lty[i], 
    #    col = opt$col[i])
    for (i in 1:nlev) lines(eval.points, estimate[i, ], lty =
                              opt$lty[i],   ## lwd hacked in
                            col = opt$col[i], lwd = lwd[i])
  }
  est <- NULL
  p <- NULL
  if (model == "equal" & test) {
    if (nlev == 2) {
      ts <- sum((estimate[1, ] - estimate[2, ])^2)
    }
    else {
      sm.mean <- sm:::sm.density(y, h = h, xlim = opt$xlim, 
                                 ngrid = opt$ngrid, display = "none")$estimate
      ts <- 0
      for (i in 1:nlev) ts <- ts + ni[i] * sum((estimate[i, 
                                                         ] - sm.mean)^2)
    }
    p <- 0
    est.star <- matrix(0, ncol = opt$ngrid, nrow = nlev)
    for (iboot in 1:nboot) {
      ind <- (1:length(y))
      for (i in 1:nlev) {
        indi <- sample((1:length(ind)), ni[i])
        est.star[i, ] <- sm:::sm.density(y[ind[indi]], h = h, 
                                         ngrid = opt$ngrid, xlim = opt$xlim, display =
                                           "none")$estimate
        ind <- ind[-indi]
      }
      if (nlev == 2) {
        ts.star <- sum((est.star[1, ] - est.star[2, ])^2)
      }
      else {
        sm.mean <- sm:::sm.density(y, h = h, xlim = opt$xlim, 
                                   ngrid = opt$ngrid, display = "none")$estimate
        ts.star <- 0
        for (i in 1:nlev) {
          ts.star <- ts.star + ni[i] * sum((est.star[i, 
                                                     ] - sm.mean)^2)
        }
      }
      if (ts.star > ts) 
        p <- p + 1
      if (opt$verbose > 1) {
        cat(iboot)
        cat(" ")
      }
    }
    p <- p/nboot
    cat("\nTest of equal densities:  p-value = ", round(p, 
                                                        5), "\n")
    est <- list(p = p, h = h)
  }
  if (model == "equal" & band) {
    av <- (sqrt(estimate[1, ]) + sqrt(estimate[2, ]))/2
    se <- sqrt(se[1, ]^2 + se[2, ]^2)
    upper <- (av + se)^2
    lower <- pmax(av - se, 0)^2
    plot(xlim, c(0, 1.1 * max(as.vector(estimate), upper)), 
         xlab = xlab, ylab = ylab, type = "n", asp=asp, ...)     
    ## ... and asp added; was opt$xlab and opt$ylab
    polygon(c(eval.points, rev(eval.points)), c(upper, rev(lower)), 
            col = bandcol, border = 0)                                      
    ## was col = "cyan"
    if (is.null(usePolyg)) {
      lines(eval.points, estimate[1, ], lty = opt$lty[1], col =
              opt$col[1], lwd = lwd[1])
      lines(eval.points, estimate[2, ], lty = opt$lty[2], col =
              opt$col[2], lwd = lwd[2])
    }
    else {
      polygon(eval.points, estimate[1, ], lty = opt$lty[1], col =
                opt$col[1], lwd = lwd[1])
      polygon(eval.points, estimate[2, ], lty = opt$lty[2], col =
                opt$col[2], lwd = lwd[2])
    }
    est <- list(p = p, upper = upper, lower = lower, h = h)
  }
  invisible(est)
}

# Prepare data with variables of interest

# We do data manipulation and cleaning to come up with a dataset with our variables of interests.

d <- lmi %>% dplyr::select(id = ID,
                           # No answers or not asked T3: PA_opportunitiesReverseCoded_08_T3 = Kys0105.3,
                           # No answers or not asked T4: PA_opportunitiesReverseCoded_08_T4 = Kys0105.4,
                           # Not included in the new dataset: runStepsAccelerometer_T1 = JuoksuT1_ka,
                           # Not included in the new dataset: runStepsAccelerometer_T3 = JuoksuT3_ka,
                           # Not included in the new dataset: runStepsAccelerometer_T4 = JuoksuT1_ka,
                           # PA_actCop_01_T1 = Kys0115.1,
                           # PA_actCop_01_T3 = Kys0115.3,
                           # PA_actCop_01_T4 = Kys0115.4,
                           # PA_actCop_02_T1 = Kys0116.1,
                           # PA_actCop_02_T3 = Kys0116.3,
                           # PA_actCop_02_T4 = Kys0116.4,
                           # PA_actCop_03_T1 = Kys0117.1,
                           # PA_actCop_03_T3 = Kys0117.3,
                           # PA_actCop_03_T4 = Kys0117.4,
                           # PA_actCop_04_T1 = Kys0118.1,
                           # PA_actCop_04_T3 = Kys0118.3,
                           # PA_actCop_04_T4 = Kys0118.4,
                           # PA_actCop_05_T1 = Kys0119.1,
                           # PA_actCop_05_T3 = Kys0119.3,
                           # PA_actCop_05_T4 = Kys0119.4,
                           # PA_actCop_06_T1 = Kys0120.1,
                           # PA_actCop_06_T3 = Kys0120.3,
                           # PA_actCop_06_T4 = Kys0120.4,
                           # PA_actCop_07_T1 = Kys0121.1,
                           # PA_actCop_07_T3 = Kys0121.3,
                           # PA_actCop_07_T4 = Kys0121.4,
                           # PA_actCop_08_T1 = Kys0122.1,
                           # PA_actCop_08_T3 = Kys0122.3,
                           # PA_actCop_08_T4 = Kys0122.4,
                           # weartime_min4d10h_T1 = min4d10hT1, # After T4 data changed, these are no longer in
                           # weartime_min4d10h_T3 = min4d10hT3
                           big5agreeableness_01_T1 = Kys0155.1,
                           big5agreeablenessReverseCoded_02_T1 = Kys0150.1,
                           big5conscientiousness_01_T1 = Kys0151.1,
                           big5conscientiousnessReverseCoded_02_T1 = Kys0156.1,
                           big5extraversion_01_T1 = Kys0149.1,
                           big5extraversionReverseCoded_02_T1 = Kys0154.1,
                           big5neuroticism_01_T1 = Kys0152.1,
                           big5neuroticismReverseCoded_02_T1 = Kys0157.1,
                           big5openness_01_T1 = Kys0153.1,
                           big5opennessReverseCoded_02_T1 = Kys0158.1,
                           fatpct_T1 = Rasva,
                           girl = Kys0013.1,
                           group = ryhmakoodiID,
                           groupFeelsSafe_T1 = Kys0219.1,
                           groupFeelsSafe_T3 = Kys0219.3,
                           groupFeelsSafe_T4 = Kys0219.4,
                           groupListensToMe_T1 = Kys0216.1,
                           groupListensToMe_T3 = Kys0216.3,
                           groupListensToMe_T4 = Kys0216.4,
                           groupSupportsMe_T1 = Kys0215.1,
                           groupSupportsMe_T3 = Kys0215.3,
                           groupSupportsMe_T4 = Kys0215.4,
                           groupUnderstandsMe_T1 = Kys0217.1,
                           groupUnderstandsMe_T3 = Kys0217.3,
                           groupUnderstandsMe_T4 = Kys0217.4,
                           groupValuesMe_T1 = Kys0218.1,
                           groupValuesMe_T3 = Kys0218.3,
                           groupValuesMe_T4 = Kys0218.4,
                           intervention = ryhma,
                           lpaAccelerometer_T1 = Kev1m_ka.1,
                           lpaAccelerometer_T3 = Kev1m_ka.3,
                           lpaAccelerometer_T4 = Kev1m_ka.4,
                           mpaAccelerometer_T1 = Rei1m_ka.1,
                           mpaAccelerometer_T3 = Rei1m_ka.3,
                           mpaAccelerometer_T4 = Rei1m_ka.4,
                           mvpaAccelerometer_T1 = ReRa1m_ka.1,
                           mvpaAccelerometer_T3 = ReRa1m_ka.3,
                           mvpaAccelerometer_T4 = ReRa1m_ka.4,
                           PA_actionPlanning_01_T1 = Kys0115.1,
                           PA_actionPlanning_01_T3 = Kys0115.3,
                           PA_actionPlanning_01_T4 = Kys0115.4,
                           PA_actionPlanning_02_T1 = Kys0116.1,
                           PA_actionPlanning_02_T3 = Kys0116.3,
                           PA_actionPlanning_02_T4 = Kys0116.4,
                           PA_actionPlanning_03_T1 = Kys0117.1,
                           PA_actionPlanning_03_T3 = Kys0117.3,
                           PA_actionPlanning_03_T4 = Kys0117.4,
                           PA_actionPlanning_04_T1 = Kys0118.1,
                           PA_actionPlanning_04_T3 = Kys0118.3,
                           PA_actionPlanning_04_T4 = Kys0118.4,
                           PA_agreementDependentBCT_01_T1 = Kys0128.1,
                           PA_agreementDependentBCT_01_T3 = Kys0128.3,
                           PA_agreementDependentBCT_01_T4 = Kys0128.4,
                           PA_agreementDependentBCT_02_T1 = Kys0129.1,
                           PA_agreementDependentBCT_02_T3 = Kys0129.3,
                           PA_agreementDependentBCT_02_T4 = Kys0129.4,
                           PA_agreementDependentBCT_03_T1 = Kys0130.1,
                           PA_agreementDependentBCT_03_T3 = Kys0130.3,
                           PA_agreementDependentBCT_03_T4 = Kys0130.4,
                           PA_agreementDependentBCT_04_T1 = Kys0131.1,
                           PA_agreementDependentBCT_04_T3 = Kys0131.3,
                           PA_agreementDependentBCT_04_T4 = Kys0131.4,
                           PA_agreementDependentBCT_05_T1 = Kys0132.1,
                           PA_agreementDependentBCT_05_T3 = Kys0132.3,
                           PA_agreementDependentBCT_05_T4 = Kys0132.4,
                           PA_agreementDependentBCT_06_T1 = Kys0133.1,
                           PA_agreementDependentBCT_06_T3 = Kys0133.3,
                           PA_agreementDependentBCT_06_T4 = Kys0133.4,
                           PA_agreementDependentBCT_07_T1 = Kys0134.1,
                           PA_agreementDependentBCT_07_T3 = Kys0134.3,
                           PA_agreementDependentBCT_07_T4 = Kys0134.4,
                           PA_agreementDependentBCT_08_T1 = Kys0135.1,
                           PA_agreementDependentBCT_08_T3 = Kys0135.3,
                           PA_agreementDependentBCT_08_T4 = Kys0135.4,
                           PA_agreementDependentBCT_09_T1 = Kys0136.1,
                           PA_agreementDependentBCT_09_T3 = Kys0136.3,
                           PA_agreementDependentBCT_09_T4 = Kys0136.4,
                           PA_agreementDependentBCT_10_T1 = Kys0137.1,
                           PA_agreementDependentBCT_10_T3 = Kys0137.3,
                           PA_agreementDependentBCT_10_T4 = Kys0137.4,
                           PA_amotivation_01_T1 = Kys0082.1,
                           PA_amotivation_01_T3 = Kys0082.3,
                           PA_amotivation_01_T4 = Kys0082.4,
                           PA_amotivation_02_T1 = Kys0086.1,
                           PA_amotivation_02_T3 = Kys0086.3,
                           PA_amotivation_02_T4 = Kys0086.4,
                           PA_amotivation_03_T1 = Kys0096.1,
                           PA_amotivation_03_T3 = Kys0096.3,
                           PA_amotivation_03_T4 = Kys0096.4,
                           PA_amotivation_04_T1 = Kys0097.1,
                           PA_amotivation_04_T3 = Kys0097.3,
                           PA_amotivation_04_T4 = Kys0097.4,
                           PA_autonomous_01_T1 = Kys0087.1,
                           PA_autonomous_01_T3 = Kys0087.3,
                           PA_autonomous_01_T4 = Kys0087.4,
                           PA_autonomous_02_T1 = Kys0088.1,
                           PA_autonomous_02_T3 = Kys0088.3,
                           PA_autonomous_02_T4 = Kys0088.4,
                           PA_autonomous_03_T1 = Kys0090.1,
                           PA_autonomous_03_T3 = Kys0090.3,
                           PA_autonomous_03_T4 = Kys0090.4,
                           PA_autonomous_04_T1 = Kys0089.1,
                           PA_autonomous_04_T3 = Kys0089.3,
                           PA_autonomous_04_T4 = Kys0089.4,
                           PA_autonomous_05_T1 = Kys0092.1,
                           PA_autonomous_05_T3 = Kys0092.3,
                           PA_autonomous_05_T4 = Kys0092.4,
                           PA_autonomous_06_T1 = Kys0094.1,
                           PA_autonomous_06_T3 = Kys0094.3,
                           PA_autonomous_06_T4 = Kys0094.4,
                           PA_autonomous_07_T1 = Kys0091.1,
                           PA_autonomous_07_T3 = Kys0091.3,
                           PA_autonomous_07_T4 = Kys0091.4,
                           PA_autonomous_08_T1 = Kys0093.1,
                           PA_autonomous_08_T3 = Kys0093.3,
                           PA_autonomous_08_T4 = Kys0093.4,
                           PA_autonomous_09_T1 = Kys0095.1,
                           PA_autonomous_09_T3 = Kys0095.3,
                           PA_autonomous_09_T4 = Kys0095.4,
                           PA_controlled_01_T1 = Kys0080.1,
                           PA_controlled_01_T3 = Kys0080.3,
                           PA_controlled_01_T4 = Kys0080.4,
                           PA_controlled_02_T1 = Kys0081.1,
                           PA_controlled_02_T3 = Kys0081.3,
                           PA_controlled_02_T4 = Kys0081.4,
                           PA_controlled_03_T1 = Kys0083.1,
                           PA_controlled_03_T3 = Kys0083.3,
                           PA_controlled_03_T4 = Kys0083.4,
                           PA_controlled_04_T1 = Kys0084.1,
                           PA_controlled_04_T3 = Kys0084.3,
                           PA_controlled_04_T4 = Kys0084.4,
                           PA_controlled_05_T1 = Kys0085.1,
                           PA_controlled_05_T3 = Kys0085.3,
                           PA_controlled_05_T4 = Kys0085.4,
                           PA_copingPlanning_01_T1 = Kys0119.1,
                           PA_copingPlanning_01_T3 = Kys0119.3,
                           PA_copingPlanning_01_T4 = Kys0119.4,
                           PA_copingPlanning_02_T1 = Kys0120.1,
                           PA_copingPlanning_02_T3 = Kys0120.3,
                           PA_copingPlanning_02_T4 = Kys0120.4,
                           PA_copingPlanning_03_T1 = Kys0121.1,
                           PA_copingPlanning_03_T3 = Kys0121.3,
                           PA_copingPlanning_03_T4 = Kys0121.4,
                           PA_copingPlanning_04_T1 = Kys0122.1,
                           PA_copingPlanning_04_T3 = Kys0122.3,
                           PA_copingPlanning_04_T4 = Kys0122.4,
                           PA_descriptiveNorm_01_T1 = Kys0106.1,
                           PA_descriptiveNorm_01_T3 = Kys0106.3,
                           PA_descriptiveNorm_01_T4 = Kys0106.4,
                           PA_descriptiveNorm_02_T1 = Kys0107.1,
                           PA_descriptiveNorm_02_T4 = Kys0107.4,
                           PA_descriptiveNormparents_02_T3 = Kys0107.3,
                           PA_frequencyDependentBCT_01_T1 = Kys0138.1,
                           PA_frequencyDependentBCT_01_T3 = Kys0138.3,
                           PA_frequencyDependentBCT_01_T4 = Kys0138.4,
                           PA_frequencyDependentBCT_02_T1 = Kys0139.1,
                           PA_frequencyDependentBCT_02_T3 = Kys0139.3,
                           PA_frequencyDependentBCT_02_T4 = Kys0139.4,
                           PA_frequencyDependentBCT_03_T1 = Kys0140.1,
                           PA_frequencyDependentBCT_03_T3 = Kys0140.3,
                           PA_frequencyDependentBCT_03_T4 = Kys0140.4,
                           PA_frequencyDependentBCT_04_T1 = Kys0141.1,
                           PA_frequencyDependentBCT_04_T3 = Kys0141.3,
                           PA_frequencyDependentBCT_04_T4 = Kys0141.4,
                           PA_frequencyDependentBCT_05_T1 = Kys0142.1,
                           PA_frequencyDependentBCT_05_T3 = Kys0142.3,
                           PA_frequencyDependentBCT_05_T4 = Kys0142.4,
                           PA_frequencyDependentBCT_06_T1 = Kys0143.1,
                           PA_frequencyDependentBCT_06_T3 = Kys0143.3,
                           PA_frequencyDependentBCT_06_T4 = Kys0143.4,
                           PA_frequencyDependentBCT_07_T1 = Kys0144.1,
                           PA_frequencyDependentBCT_07_T3 = Kys0144.3,
                           PA_frequencyDependentBCT_07_T4 = Kys0144.4,
                           PA_frequencyDependentBCT_08_T1 = Kys0145.1,
                           PA_frequencyDependentBCT_08_T3 = Kys0145.3,
                           PA_frequencyDependentBCT_08_T4 = Kys0145.4,
                           PA_frequencyDependentBCT_09_T1 = Kys0146.1,
                           PA_frequencyDependentBCT_09_T3 = Kys0146.3,
                           PA_frequencyDependentBCT_09_T4 = Kys0146.4,
                           PA_goal_01_T1 = Kys0147.1,
                           PA_goal_01_T3 = Kys0147.3,
                           PA_goal_01_T4 = Kys0147.4,
                           PA_injunctiveNorm_01_T1 = Kys0108.1,
                           PA_injunctiveNorm_01_T3 = Kys0108.3,
                           PA_injunctiveNorm_01_T4 = Kys0108.4,
                           PA_intention_01_T1 = Kys0113.1,
                           PA_intention_01_T3 = Kys0113.3,
                           PA_intention_01_T4 = Kys0113.4,
                           PA_intention_02_T1 = Kys0114.1,
                           PA_intention_02_T3 = Kys0114.3,
                           PA_intention_02_T4 = Kys0114.4,
                           PA_opportunities_01_T1 = Kys0098.1,
                           PA_opportunities_01_T3 = Kys0098.3,
                           PA_opportunities_01_T4 = Kys0098.4,
                           PA_opportunities_02_T1 = Kys0099.1,
                           PA_opportunities_02_T3 = Kys0099.3,
                           PA_opportunities_02_T4 = Kys0099.4,
                           PA_opportunities_04_T1 = Kys0101.1,
                           PA_opportunities_04_T3 = Kys0101.3,
                           PA_opportunities_04_T4 = Kys0101.4,
                           PA_opportunities_05_T1 = Kys0102.1,
                           PA_opportunities_05_T3 = Kys0102.3,
                           PA_opportunities_05_T4 = Kys0102.4,
                           PA_opportunities_07_T1 = Kys0104.1,
                           PA_opportunities_07_T3 = Kys0104.3,
                           PA_opportunities_07_T4 = Kys0104.4,
                           PA_opportunitiesReverseCoded_03_T1 = Kys0100.1,
                           PA_opportunitiesReverseCoded_03_T3 = Kys0100.3,
                           PA_opportunitiesReverseCoded_03_T4 = Kys0100.4,
                           PA_opportunitiesReverseCoded_06_T1 = Kys0103.1,
                           PA_opportunitiesReverseCoded_06_T3 = Kys0103.3,
                           PA_opportunitiesReverseCoded_06_T4 = Kys0103.4,
                           PA_opportunitiesReverseCoded_08_T1 = Kys0105.1,
                           PA_outcomeExpectations_01_T1 = Kys0068.1,
                           PA_outcomeExpectations_01_T3 = Kys0068.3,
                           PA_outcomeExpectations_01_T4 = Kys0068.4,
                           PA_outcomeExpectations_03_T1 = Kys0070.1,
                           PA_outcomeExpectations_03_T3 = Kys0070.3,
                           PA_outcomeExpectations_03_T4 = Kys0070.4,
                           PA_outcomeExpectations_04_T1 = Kys0071.1,
                           PA_outcomeExpectations_04_T3 = Kys0071.3,
                           PA_outcomeExpectations_04_T4 = Kys0071.4,
                           PA_outcomeExpectations_05_T1 = Kys0072.1,
                           PA_outcomeExpectations_05_T3 = Kys0072.3,
                           PA_outcomeExpectations_05_T4 = Kys0072.4,
                           PA_outcomeExpectations_06_T1 = Kys0073.1,
                           PA_outcomeExpectations_06_T3 = Kys0073.3,
                           PA_outcomeExpectations_06_T4 = Kys0073.4,
                           PA_outcomeExpectations_07_T1 = Kys0074.1,
                           PA_outcomeExpectations_07_T3 = Kys0074.3,
                           PA_outcomeExpectations_07_T4 = Kys0074.4,
                           PA_outcomeExpectations_10_T1 = Kys0077.1,
                           PA_outcomeExpectations_10_T3 = Kys0077.3,
                           PA_outcomeExpectations_10_T4 = Kys0077.4,
                           PA_outcomeExpectations_11_T1 = Kys0078.1,
                           PA_outcomeExpectations_11_T3 = Kys0078.3,
                           PA_outcomeExpectations_11_T4 = Kys0078.4,
                           PA_outcomeExpectations_12_T1 = Kys0079.1,
                           PA_outcomeExpectations_12_T3 = Kys0079.3,
                           PA_outcomeExpectations_12_T4 = Kys0079.4,
                           PA_outcomeExpectationsNegativeReverseCoded_02_T1 = Kys0069.1,
                           PA_outcomeExpectationsNegativeReverseCoded_02_T3 = Kys0069.3,
                           PA_outcomeExpectationsNegativeReverseCoded_02_T4 = Kys0069.4,
                           PA_outcomeExpectationsNegativeReverseCoded_08_T1 = Kys0075.1,
                           PA_outcomeExpectationsNegativeReverseCoded_08_T3 = Kys0075.3,
                           PA_outcomeExpectationsNegativeReverseCoded_08_T4 = Kys0075.4,
                           PA_outcomeExpectationsNegativeReverseCoded_09_T1 = Kys0076.1,
                           PA_outcomeExpectationsNegativeReverseCoded_09_T3 = Kys0076.3,
                           PA_outcomeExpectationsNegativeReverseCoded_09_T4 = Kys0076.4,
                           PA_perceivedBehaviouralControl_01_T1 = Kys0125.1,
                           PA_perceivedBehaviouralControl_01_T3 = Kys0125.3,
                           PA_perceivedBehaviouralControl_01_T4 = Kys0125.4,
                           PA_perceivedBehaviouralControl_03_T1 = Kys0127.1,
                           PA_perceivedBehaviouralControl_03_T3 = Kys0127.3,
                           PA_perceivedBehaviouralControl_03_T4 = Kys0127.4,
                           PA_perceivedBehaviouralControlReverseCoded_02_T1 = Kys0126.1,
                           PA_perceivedBehaviouralControlReverseCoded_02_T3 = Kys0126.3,
                           PA_perceivedBehaviouralControlReverseCoded_02_T4 = Kys0126.4,
                           PA_selfefficacy_01_T1 = Kys0123.1,
                           PA_selfefficacy_01_T3 = Kys0123.3,
                           PA_selfefficacy_01_T4 = Kys0123.4,
                           PA_selfefficacyReverseCoded_02_T1 = Kys0124.1,
                           PA_selfefficacyReverseCoded_02_T3 = Kys0124.3,
                           PA_selfefficacyReverseCoded_02_T4 = Kys0124.4,
                           padaysLastweek_T1 = Kys0045.1,
                           padaysLastweek_T3 = Kys0045.3,
                           padaysLastweek_T4 = Kys0045.4,
                           pafreqUsually_T1 = Kys0048.1,
                           pafreqUsually_T3 = Kys0048.3,
                           pafreqUsually_T4 = Kys0048.4,
                           pahrsLastweek_T1 = Kys0046.1,
                           pahrsLastweek_T3 = Kys0046.3,
                           pahrsLastweek_T4 = Kys0046.4,
                           pahrsUsually_T1 = Kys0049.1,
                           pahrsUsually_T3 = Kys0049.3,
                           pahrsUsually_T4 = Kys0049.4,
                           paminLastweek_T1 = Kys0047.1,
                           paminLastweek_T3 = Kys0047.3,
                           paminLastweek_T4 = Kys0047.4,
                           SB_avgDailySelfRepSittingHoursLastWeekWeekday_T1 = Kys0159.1,
                           SB_avgDailySelfRepSittingHoursLastWeekWeekday_T3 = Kys0159.3,
                           SB_avgDailySelfRepSittingHoursLastWeekWeekday_T4 = Kys0159.4,
                           SB_avgDailySelfRepSittingHoursLastWeekWeekend_T1 = Kys0161.1,
                           SB_avgDailySelfRepSittingHoursLastWeekWeekend_T3 = Kys0161.3,
                           SB_avgDailySelfRepSittingHoursLastWeekWeekend_T4 = Kys0161.4,
                           SB_avgDailySelfRepSittingMinutesLastWeekWeekday_T1 = Kys0160.1,
                           SB_avgDailySelfRepSittingMinutesLastWeekWeekday_T3 = Kys0160.3,
                           SB_avgDailySelfRepSittingMinutesLastWeekWeekday_T4 = Kys0160.4,
                           SB_avgDailySelfRepSittingMinutesLastWeekWeekend_T1 = Kys0162.1,
                           SB_avgDailySelfRepSittingMinutesLastWeekWeekend_T3 = Kys0162.3,
                           SB_avgDailySelfRepSittingMinutesLastWeekWeekend_T4 = Kys0162.4,
                           SB_classesInterruptSitting_T1 = Kys0203.1,
                           SB_classesInterruptSitting_T3 = Kys0203.3,
                           SB_classesInterruptSitting_T4 = Kys0203.4,
                           SB_classesProvideOpportunities_T1 = Kys0202.1,
                           SB_classesProvideOpportunities_T3 = Kys0202.3,
                           SB_classesProvideOpportunities_T4 = Kys0202.4,
                           SB_descriptiveNorm_01_T1 = Kys0178.1,
                           SB_descriptiveNorm_01_T3 = Kys0178.3,
                           SB_descriptiveNorm_01_T4 = Kys0178.4,
                           SB_descriptiveNorm_02_T1 = Kys0179.1,
                           SB_descriptiveNorm_02_T3 = Kys0179.3,
                           SB_descriptiveNorm_02_T4 = Kys0179.4,
                           SB_injunctiveNorm_01_T1 = Kys0180.1,
                           SB_injunctiveNorm_01_T3 = Kys0180.3,
                           SB_injunctiveNorm_01_T4 = Kys0180.4,
                           SB_injunctiveNorm_02_T1 = Kys0181.1,
                           SB_injunctiveNorm_02_T3 = Kys0181.3,
                           SB_injunctiveNorm_02_T4 = Kys0181.4,
                           SB_intention_01_T1 = Kys0187.1,
                           SB_intention_01_T3 = Kys0187.3,
                           SB_intention_01_T4 = Kys0187.4,
                           SB_intention_02_T1 = Kys0188.1,
                           SB_intention_02_T3 = Kys0188.3,
                           SB_intention_02_T4 = Kys0188.4,
                           SB_intention_03_T1 = Kys0189.1,
                           SB_intention_03_T3 = Kys0189.3,
                           SB_intention_03_T4 = Kys0189.4,
                           SB_intention_04_T1 = Kys0190.1,
                           SB_intention_04_T3 = Kys0190.3,
                           SB_intention_04_T4 = Kys0190.4,
                           SB_outcomeExpectations_02_T1 = Kys0172.1,
                           SB_outcomeExpectations_02_T3 = Kys0172.3,
                           SB_outcomeExpectations_02_T4 = Kys0172.4,
                           SB_outcomeExpectations_03_T1 = Kys0173.1,
                           SB_outcomeExpectations_03_T3 = Kys0173.3,
                           SB_outcomeExpectations_03_T4 = Kys0173.4,
                           SB_outcomeExpectations_04_T1 = Kys0174.1,
                           SB_outcomeExpectations_04_T3 = Kys0174.3,
                           SB_outcomeExpectations_04_T4 = Kys0174.4,
                           SB_outcomeExpectations_05_T1 = Kys0175.1,
                           SB_outcomeExpectations_05_T3 = Kys0175.3,
                           SB_outcomeExpectations_05_T4 = Kys0175.4,
                           SB_outcomeExpectations_06_T1 = Kys0176.1,
                           SB_outcomeExpectations_06_T3 = Kys0176.3,
                           SB_outcomeExpectations_06_T4 = Kys0176.4,
                           SB_outcomeExpectationsNegativeReverseCoded_01_T1 = Kys0171.1,
                           SB_outcomeExpectationsNegativeReverseCoded_01_T3 = Kys0171.3,
                           SB_outcomeExpectationsNegativeReverseCoded_01_T4 = Kys0171.4,
                           SB_outcomeExpectationsNegativeReverseCoded_07_T1 = Kys0177.1,
                           SB_outcomeExpectationsNegativeReverseCoded_07_T3 = Kys0177.3,
                           SB_outcomeExpectationsNegativeReverseCoded_07_T4 = Kys0177.4,
                           SB_perceivedTooMuchSitting_T1 = Kys0170.1,
                           SB_perceivedTooMuchSitting_T3 = Kys0170.3,
                           SB_perceivedTooMuchSitting_T4 = Kys0170.4,
                           SB_selfEfficacyperceivedBehaviouralControl_01_T1 = Kys0182.1,
                           SB_selfEfficacyperceivedBehaviouralControl_01_T3 = Kys0182.3,
                           SB_selfEfficacyperceivedBehaviouralControl_01_T4 = Kys0182.4,
                           SB_selfEfficacyperceivedBehaviouralControl_02_T1 = Kys0183.1,
                           SB_selfEfficacyperceivedBehaviouralControl_02_T3 = Kys0183.3,
                           SB_selfEfficacyperceivedBehaviouralControl_02_T4 = Kys0183.4,
                           SB_selfEfficacyperceivedBehaviouralControl_03_T1 = Kys0184.1,
                           SB_selfEfficacyperceivedBehaviouralControl_03_T3 = Kys0184.3,
                           SB_selfEfficacyperceivedBehaviouralControl_03_T4 = Kys0184.4,
                           SB_selfEfficacyperceivedBehaviouralControl_04_T1 = Kys0185.1,
                           SB_selfEfficacyperceivedBehaviouralControl_04_T3 = Kys0185.3,
                           SB_selfEfficacyperceivedBehaviouralControl_04_T4 = Kys0185.4,
                           SB_selfEfficacyperceivedBehaviouralControl_05_T1 = Kys0186.1,
                           SB_selfEfficacyperceivedBehaviouralControl_05_T3 = Kys0186.3,
                           SB_selfEfficacyperceivedBehaviouralControl_05_T4 = Kys0186.4,
                           SB_selfRepSitbreaksDidNotSit30min_T1 = Kys0169.1,
                           SB_selfRepSitbreaksDidNotSit30min_T3 = Kys0169.3,
                           SB_selfRepSitbreaksDidNotSit30min_T4 = Kys0169.4,
                           SB_selfRepSitbreaksDuringClass_T1 = Kys0164.1,
                           SB_selfRepSitbreaksDuringClass_T3 = Kys0164.3,
                           SB_selfRepSitbreaksDuringClass_T4 = Kys0164.4,
                           SB_selfRepSitbreaksHomeComputer_T1 = Kys0166.1,
                           SB_selfRepSitbreaksHomeComputer_T3 = Kys0166.3,
                           SB_selfRepSitbreaksHomeComputer_T4 = Kys0166.4,
                           SB_selfRepSitbreaksTVDVD_T1 = Kys0165.1,
                           SB_selfRepSitbreaksTVDVD_T3 = Kys0165.3,
                           SB_selfRepSitbreaksTVDVD_T4 = Kys0165.4,
                           SB_selfRepSitbreaksWithFriends_T1 = Kys0167.1,
                           SB_selfRepSitbreaksWithFriends_T3 = Kys0167.3,
                           SB_selfRepSitbreaksWithFriends_T4 = Kys0167.4,
                           SB_selfRepSitbreaksWorkPlacement_T1 = Kys0168.1,
                           SB_selfRepSitbreaksWorkPlacement_T3 = Kys0168.3,
                           SB_selfRepSitbreaksWorkPlacement_T4 = Kys0168.4,
                           SB_selfRepSittingMinutesDuringClass_T1 = Kys0163.1,
                           SB_selfRepSittingMinutesDuringClass_T3 = Kys0163.3,
                           SB_selfRepSittingMinutesDuringClass_T4 = Kys0163.4,
                           SB_classroomOpportunities_T1 = Kys0202.1,
                           SB_classroomOpportunities_T3 = Kys0202.3, 
                           SB_classroomOpportunities_T4 = Kys0202.4, 
                           SB_classroomSitbreaksEvery30min_T1 = Kys0203.1,
                           SB_classroomSitbreaksEvery30min_T3 = Kys0203.3, 
                           SB_classroomSitbreaksEvery30min_T4 = Kys0203.4,
                           school = Aineisto.1,
                           sitBreaksAccelerometer_T1 = Ylos_ka.1,
                           sitBreaksAccelerometer_T3 = Ylos_ka.3,
                           sitBreaksAccelerometer_T4 = Ylos_ka.4,
                           sitLieAccelerometer_T1 = MaIs1m19h_ka.1,
                           sitLieAccelerometer_T3 = MaIs1m19h_ka.3,
                           sitLieAccelerometer_T4 = MaIs1m19h_ka.4,
                           standingAccelerometer_T1 = Sei1m_ka.1,
                           standingAccelerometer_T3 = Sei1m_ka.3,
                           standingAccelerometer_T4 = Sei1m_ka.4,
                           symptom_headAche_T1 = Kys0037.1,
                           symptom_headAche_T3 = Kys0037.3,
                           symptom_headAche_T4 = Kys0037.4,
                           symptom_irritabilityAngerbursts_T1 = Kys0035.1,
                           symptom_irritabilityAngerbursts_T3 = Kys0035.3,
                           symptom_irritabilityAngerbursts_T4 = Kys0035.4,
                           symptom_lowerBackPain_T1 = Kys0032.1,
                           symptom_lowerBackPain_T3 = Kys0032.3,
                           symptom_lowerBackPain_T4 = Kys0032.4,
                           symptom_neckShoulderPain_T1 = Kys0031.1,
                           symptom_neckShoulderPain_T3 = Kys0031.3,
                           symptom_neckShoulderPain_T4 = Kys0031.4,
                           symptom_sleepDifficulty_T1 = Kys0036.1,
                           symptom_sleepDifficulty_T3 = Kys0036.3,
                           symptom_sleepDifficulty_T4 = Kys0036.4,
                           symptom_stomachAche_T1 = Kys0033.1,
                           symptom_stomachAche_T3 = Kys0033.3,
                           symptom_stomachAche_T4 = Kys0033.4,
                           symptom_tensionNervousness_T1 = Kys0034.1,
                           symptom_tensionNervousness_T3 = Kys0034.3,
                           symptom_tensionNervousness_T4 = Kys0034.4,
                           symptom_tirednessFaintness_T1 = Kys0038.1,
                           symptom_tirednessFaintness_T3 = Kys0038.3,
                           symptom_tirednessFaintness_T4 = Kys0038.4,
                           walkStepsAccelerometer_T1 = Askel_ka.1,
                           walkStepsAccelerometer_T3 = Askel_ka.3,
                           walkStepsAccelerometer_T4 = Askel_ka.4,
                           weartimeAccelerometer_T1 = Summa19h_ka.1,
                           weartimeAccelerometer_T3 = Summa19h_ka.3,
                           weartimeAccelerometer_T4 = Summa19h_ka.4,
                           numberOfDaysAccelerometerWasWornOver10h_T1 = Npv.1,
                           numberOfDaysAccelerometerWasWornOver10h_T3 = Npv.3,
                           numberOfDaysAccelerometerWasWornOver10h_T4 = Npv.4,
                           vpaAccelerometer_T1 = Ras1m_ka.1,
                           vpaAccelerometer_T3 = Ras1m_ka.3,
                           vpaAccelerometer_T4 = Ras1m_ka.4
                           )

# Delete acclerometer results from participants who wore the device for a fewer number of days (i.e. 4) than required
d <- d %>% dplyr::mutate_at(vars(contains("accelerometer"), -contains("over10hrWearDays"), -contains("_T3"),  -contains("_T4")), 
                       funs(ifelse(numberOfDaysAccelerometerWasWornOver10h_T1 < 4, NA, .)))

d <- d %>% dplyr::mutate_at(vars(contains("accelerometer"), -contains("over10hrWearDays"), -contains("_T1"),  -contains("_T4")), 
                       funs(ifelse(numberOfDaysAccelerometerWasWornOver10h_T3 < 4, NA, .)))

d <- d %>% dplyr::mutate_at(vars(contains("accelerometer"), -contains("over10hrWearDays"), -contains("_T1"),  -contains("_T3")), 
                       funs(ifelse(numberOfDaysAccelerometerWasWornOver10h_T4 < 4, NA, .))) 

d$lpaAccelerometer_noCutOff_T1 <- lmi$Kev1m_ka.1
d$mpaAccelerometer_noCutOff_T1 <- lmi$Rei1m_ka.1
d$mvpaAccelerometer_noCutOff_T1 <- lmi$ReRa1m_ka.1
d$sitBreaksAccelerometer_noCutOff_T1 <- lmi$Ylos_ka.1
d$sitLieAccelerometer_noCutOff_T1 <- lmi$MaIs1m19h_ka.1
d$standingAccelerometer_noCutOff_T1 <- lmi$Sei1m_ka.1
d$walkStepsAccelerometer_noCutOff_T1 <- lmi$Askel_ka.1
d$weartimeAccelerometer_noCutOff_T1 <- lmi$Summa19h_ka.1
d$vpaAccelerometer_noCutOff_T1 <- lmi$Ras1m_ka.1

d$lpaAccelerometer_noCutOff_T3 <- lmi$Kev1m_ka.3
d$mpaAccelerometer_noCutOff_T3 <- lmi$Rei1m_ka.3
d$mvpaAccelerometer_noCutOff_T3 <- lmi$ReRa1m_ka.3
d$sitBreaksAccelerometer_noCutOff_T3 <- lmi$Ylos_ka.3
d$sitLieAccelerometer_noCutOff_T3 <- lmi$MaIs1m19h_ka.3
d$standingAccelerometer_noCutOff_T3 <- lmi$Sei1m_ka.3
d$walkStepsAccelerometer_noCutOff_T3 <- lmi$Askel_ka.3
d$weartimeAccelerometer_noCutOff_T3 <- lmi$Summa19h_ka.3
d$vpaAccelerometer_noCutOff_T3 <- lmi$Ras1m_ka.3

d$lpaAccelerometer_noCutOff_T4 <- lmi$Kev1m_ka.4
d$mpaAccelerometer_noCutOff_T4 <- lmi$Rei1m_ka.4
d$mvpaAccelerometer_noCutOff_T4 <- lmi$ReRa1m_ka.4
d$sitBreaksAccelerometer_noCutOff_T4 <- lmi$Ylos_ka.4
d$sitLieAccelerometer_noCutOff_T4 <- lmi$MaIs1m19h_ka.4
d$standingAccelerometer_noCutOff_T4 <- lmi$Sei1m_ka.4
d$walkStepsAccelerometer_noCutOff_T4 <- lmi$Askel_ka.4
d$weartimeAccelerometer_noCutOff_T4 <- lmi$Summa19h_ka.4
d$vpaAccelerometer_noCutOff_T4 <- lmi$Ras1m_ka.4

# Reverse coded items to normal: 
# Take vars that contain "ReverseCoded_", substract them from 8 (each scale is 1-7)
d <- d %>% dplyr::mutate_at(dplyr::vars(contains("ReverseCoded_")), funs(8 - .))

# To check:
# d %>% dplyr::select(contains("ReverseCoded_")), contains("Rev")) %>% View
identical(as.numeric(8 - lmi$Kys0154.1), as.numeric(d$big5extraversionReverseCoded_02_T1))
identical(as.numeric(8 - lmi$Kys0100.1), as.numeric(d$PA_opportunitiesReverseCoded_03_T1))
identical(as.numeric(8 - lmi$Kys0100.4), as.numeric(d$PA_opportunitiesReverseCoded_03_T4))


# Group variable has empty missings
d <- d %>% dplyr::mutate(group = ifelse(group == "", NA, group))

# PA goal and injunctive norm (single items) were part of a scale, but was removed for codebook to work. 
# Duplicating the variable name here to make the dependencies work.
d <- d %>% dplyr::mutate(PA_goal_T1 = PA_goal_01_T1,
                         PA_goal_T3 = PA_goal_01_T3,
                         PA_goal_T4 = PA_goal_01_T4,
                         PA_injunctiveNorm_T1 = PA_injunctiveNorm_01_T1,
                         PA_injunctiveNorm_T3 = PA_injunctiveNorm_01_T3,
                         PA_injunctiveNorm_T4 = PA_injunctiveNorm_01_T4)

# This item (how often one has considered what went wrong, when not reaching goals) has option 7: I have reached my goals.
d <- d %>% dplyr::mutate(haveReachedMyActivityGoal = ifelse(PA_frequencyDependentBCT_09_T1 == 7, 1, 0),
                         PA_frequencyDependentBCT_09_T1 = ifelse(PA_frequencyDependentBCT_09_T1 == 7, NA, PA_frequencyDependentBCT_09_T1),
                         haveReachedMyActivityGoal = ifelse(PA_frequencyDependentBCT_09_T1 == 7, 1, 0),
                         PA_frequencyDependentBCT_09_T3 = ifelse(PA_frequencyDependentBCT_09_T3 == 7, NA, PA_frequencyDependentBCT_09_T3),
                         haveReachedMyActivityGoal = ifelse(PA_frequencyDependentBCT_09_T1 == 7, 1, 0),
                         PA_frequencyDependentBCT_09_T4 = ifelse(PA_frequencyDependentBCT_09_T4 == 7, NA, PA_frequencyDependentBCT_09_T4))
  
  
# Fix intervention and gender variables
d <- d %>% dplyr::mutate(intervention = ifelse(intervention == 1, 1, 0),
                         intervention = factor(intervention),
                         girl = ifelse(girl == 2, 1, 0),
                         girl = factor(girl, levels = c("1", "0"), labels = c("girl", "boy")),
                         school = factor(school, levels = c("1", "2", "3", "4", "5")))

# Create the self-reported PA time variable
d <- d %>% dplyr::mutate(leisuretimeMvpaHoursLastweek_T1 = ((pahrsLastweek_T1 * 60) + ifelse(paminLastweek_T1 == 2, 30, 0)) / 60,
                         leisuretimeMvpaHoursLastweek_T3 = ((pahrsLastweek_T3 * 60) + ifelse(paminLastweek_T3 == 2, 30, 0)) / 60,
                         leisuretimeMvpaHoursLastweek_T4 = ((pahrsLastweek_T4 * 60) + ifelse(paminLastweek_T4 == 2, 30, 0)) / 60 #,
                         # mvpaAccelerometer_T1 = mvpaAccelerometer_T1 / 60, # Changes to hours reverted, as it leads to problems in making tables.
                         # mvpaAccelerometer_T3 = mvpaAccelerometer_T3 / 60,
                         # weartimeAccelerometer_T1 = weartimeAccelerometer_T1 /60,
                         # weartimeAccelerometer_T3 = weartimeAccelerometer_T3 / 60,
                         # sitLieAccelerometer_T1 = sitLieAccelerometer_T1 / 60,
                         # sitLieAccelerometer_T3 = sitLieAccelerometer_T3 / 60
                         )

# Insert track variable with those who answered "other" with one of the actual category labels given the appropriate category:
track <- lmi %>% dplyr::select(Kys0016.1, Kys0017.1) %>% dplyr::mutate(
  Kys0016.1 = as.character(Kys0016.1), # Because I had an Evaluation error: `x` and `labels` must be same type.
  Kys0017.1 = as.character(Kys0017.1),
  Kys0016.1 = ifelse(Kys0017.1 == "Merkonomi" | Kys0017.1 == "merkonomi", 3,
                     ifelse(Kys0017.1 == "Datanomi" | Kys0017.1 == "datanomi", 2, Kys0016.1)),
  track = factor(Kys0016.1, # Fix track labels first
                 levels = c(0, 1, 2, 3, 4),
                 labels = c("Other", "IT", "BA", "HRC", "Nur"))) %>% 
  dplyr::select(-Kys0016.1, -Kys0017.1)

d <- bind_cols(d, track)

# Separate variables for scale-creation purposes
dT1 <- d %>% dplyr::select(contains("T1", ignore.case = FALSE))
dT3 <- d %>% dplyr::select(contains("T3", ignore.case = FALSE))
dT4 <- d %>% dplyr::select(contains("T4", ignore.case = FALSE))

# TEMPORARILY EXTRACTED MAKESCALES FROM https://raw.githubusercontent.com/Matherion/userfriendlyscience/master/R/makeScales.R

makeScales_NEW <- function(dat, scales, append=TRUE) {
  resDat <- dat[, FALSE];
  for (currentScale in 1:length(scales)) {
    if (length(unlist(scales[currentScale])) > 1) {
      resDat[[names(scales[currentScale])]] <-
        rowMeans(dat[, unlist(scales[currentScale])], na.rm=TRUE);
      resDat[[names(scales[currentScale])]] <-
        ifelse(is.nan(resDat[[names(scales[currentScale])]]),
               NA,
               resDat[[names(scales[currentScale])]]);
      attributes(resDat[[names(scales[currentScale])]])$scale_item_names <-
        unname(unlist(scales[currentScale]));
    }
    else if (length(unlist(scales[currentScale])) == 1) {
      resDat[[names(scales[currentScale])]] <- dat[[unlist(scales[currentScale])]];
      attributes(resDat[[names(scales[currentScale])]])$scale_item_names <-
        unname(unlist(scales[currentScale]));
    }
  }
  if (append) {
    return(cbind(dat, resDat));
  } else {
    return(resDat);
  }
}

# Create T1 scales

scales_T1 <- list(big5agreeableness_T1 = grep('big5agreeableness', names(dT1), value = TRUE),
                  big5conscientiousness_T1 = grep('big5conscientiousness', names(dT1), value = TRUE),
                  big5extraversion_T1 = grep('big5extraversion', names(dT1), value = TRUE),
                  big5neuroticism_T1 = grep('big5neuroticism', names(dT1), value = TRUE),
                  big5openness_T1 = grep('big5openness', names(dT1), value = TRUE),
                  # PA_actCop_T1 = grep('PA_actCop', names(dT1), value = TRUE), # Action and coping planning are separate in data
                  PA_agreementDependentBCT_T1 = grep('PA_agreementDependentBCT', names(dT1), value = TRUE),
                  PA_amotivation_T1 = grep('PA_amotivation', names(dT1), value = TRUE),
                  PA_autonomous_T1 = grep('PA_autonomous', names(dT1), value = TRUE),
                  PA_controlled_T1 = grep('PA_controlled', names(dT1), value = TRUE),
                  PA_descriptiveNorm_T1 = grep('PA_descriptiveNorm', names(dT1), value = TRUE),
                  PA_frequencyDependentBCT_T1 = grep('PA_frequencyDependentBCT', names(dT1), value = TRUE),
                  # PA_goal_T1 = grep('PA_goal', names(dT1), value = TRUE),
                  # PA_injunctiveNorm_T1 = grep('PA_injunctiveNorm', names(dT1), value = TRUE),
                  PA_intention_T1 = grep('PA_intention', names(dT1), value = TRUE),
                  PA_outcomeExpectations_T1 = grep('PA_outcomeExpectations', names(dT1), value = TRUE),
                  PA_opportunities_T1 = grep('PA_opportunities', names(dT1), value = TRUE),
                  PA_perceivedBehaviouralControl_T1 = grep('PA_perceivedBehaviouralControl', names(dT1), value = TRUE),
                  PA_selfefficacy_T1 = grep('PA_selfefficacy', names(dT1), value = TRUE),
                  SB_descriptiveNorm_T1 = grep('SB_descriptiveNorm', names(dT1), value = TRUE),
                  SB_injunctiveNorm_T1 = grep('SB_injunctiveNorm', names(dT1), value = TRUE),
                  SB_intention_T1 = grep('SB_intention', names(dT1), value = TRUE),
                  SB_outcomeExpectations_T1 = grep('SB_outcomeExpectations', names(dT1), value = TRUE),
                  SB_selfEfficacyperceivedBehaviouralControl_T1 = grep('SB_selfEfficacyperceivedBehaviouralControl', names(dT1), value = TRUE),
                  symptom_T1 = grep('symptom', names(dT1), value = TRUE),
                  PA_actionplan_T1 = grep('actionPlanning', names(dT1), value = TRUE),
                  PA_copingplan_T1 = grep('copingPlanning', names(dT1), value = TRUE)
)

# Append the aggregate variables to the data frame
newdf <- makeScales_NEW(dT1, scales_T1)

# Create T3 scales

scales_T3 <- list(
                  # big5agreeableness_T3 = grep('big5agreeableness', names(dT3), value = TRUE),
                  # big5conscientiousness_T3 = grep('big5conscientiousness', names(dT3), value = TRUE),
                  # big5extraversion_T3 = grep('big5extraversion', names(dT3), value = TRUE),
                  # big5neuroticism_T3 = grep('big5neuroticism', names(dT3), value = TRUE),
                  # big5openness_T3 = grep('big5openness', names(dT3), value = TRUE),
                  # PA_actCop_T3 = grep('PA_actCop', names(dT3), value = TRUE), # Action and coping planning are separate in data
                  PA_agreementDependentBCT_T3 = grep('PA_agreementDependentBCT', names(dT3), value = TRUE),
                  PA_amotivation_T3 = grep('PA_amotivation', names(dT3), value = TRUE),
                  PA_autonomous_T3 = grep('PA_autonomous', names(dT3), value = TRUE),
                  PA_controlled_T3 = grep('PA_controlled', names(dT3), value = TRUE),
                  PA_descriptiveNorm_T3 = grep('PA_descriptiveNorm', names(dT3), value = TRUE),
                  PA_frequencyDependentBCT_T3 = grep('PA_frequencyDependentBCT', names(dT3), value = TRUE),
                  # PA_goal_T3 = grep('PA_goal', names(dT3), value = TRUE),
                  # PA_injunctiveNorm_T3 = grep('PA_injunctiveNorm', names(dT3), value = TRUE),
                  PA_intention_T3 = grep('PA_intention', names(dT3), value = TRUE),
                  PA_outcomeExpectations_T3 = grep('PA_outcomeExpectations', names(dT3), value = TRUE),
                  PA_opportunities_T3 = grep('PA_opportunities', names(dT3), value = TRUE),
                  PA_perceivedBehaviouralControl_T3 = grep('PA_perceivedBehaviouralControl', names(dT3), value = TRUE),
                  PA_selfefficacy_T3 = grep('PA_selfefficacy', names(dT3), value = TRUE),
                  SB_descriptiveNorm_T3 = grep('SB_descriptiveNorm', names(dT3), value = TRUE),
                  SB_injunctiveNorm_T3 = grep('SB_injunctiveNorm', names(dT3), value = TRUE),
                  SB_intention_T3 = grep('SB_intention', names(dT3), value = TRUE),
                  SB_outcomeExpectations_T3 = grep('SB_outcomeExpectations', names(dT3), value = TRUE),
                  SB_selfEfficacyperceivedBehaviouralControl_T3 = grep('SB_selfEfficacyperceivedBehaviouralControl', names(dT3), value = TRUE),
                  symptom_T3 = grep('symptom', names(dT3), value = TRUE),
                  PA_actionplan_T3 = grep('actionPlanning', names(dT3), value = TRUE),
                  PA_copingplan_T3 = grep('copingPlanning', names(dT3), value = TRUE)
)

# Append the aggregate variables to the data frame
newdf2 <- makeScales_NEW(dT3, scales_T3)

# Create T4 scales

scales_T4 <- list(
  # big5agreeableness_T3 = grep('big5agreeableness', names(dT3), value = TRUE),
  # big5conscientiousness_T3 = grep('big5conscientiousness', names(dT3), value = TRUE),
  # big5extraversion_T3 = grep('big5extraversion', names(dT3), value = TRUE),
  # big5neuroticism_T3 = grep('big5neuroticism', names(dT3), value = TRUE),
  # big5openness_T3 = grep('big5openness', names(dT3), value = TRUE),
  # PA_actCop_T4 = grep('PA_actCop', names(dT4), value = TRUE), # Action and coping planning are separate in data
  PA_agreementDependentBCT_T4 = grep('PA_agreementDependentBCT', names(dT4), value = TRUE),
  PA_amotivation_T4 = grep('PA_amotivation', names(dT4), value = TRUE),
  PA_autonomous_T4 = grep('PA_autonomous', names(dT4), value = TRUE),
  PA_controlled_T4 = grep('PA_controlled', names(dT4), value = TRUE),
  PA_descriptiveNorm_T4 = grep('PA_descriptiveNorm', names(dT4), value = TRUE),
  PA_frequencyDependentBCT_T4 = grep('PA_frequencyDependentBCT', names(dT4), value = TRUE),
  # PA_goal_T4 = grep('PA_goal', names(dT4), value = TRUE),
  # PA_injunctiveNorm_T4 = grep('PA_injunctiveNorm', names(dT4), value = TRUE),
  PA_intention_T4 = grep('PA_intention', names(dT4), value = TRUE),
  PA_outcomeExpectations_T4 = grep('PA_outcomeExpectations', names(dT4), value = TRUE),
  PA_opportunities_T4 = grep('PA_opportunities', names(dT4), value = TRUE),
  PA_perceivedBehaviouralControl_T4 = grep('PA_perceivedBehaviouralControl', names(dT4), value = TRUE),
  PA_selfefficacy_T4 = grep('PA_selfefficacy', names(dT4), value = TRUE),
  SB_descriptiveNorm_T4 = grep('SB_descriptiveNorm', names(dT4), value = TRUE),
  SB_injunctiveNorm_T4 = grep('SB_injunctiveNorm', names(dT4), value = TRUE),
  SB_intention_T4 = grep('SB_intention', names(dT4), value = TRUE),
  SB_outcomeExpectations_T4 = grep('SB_outcomeExpectations', names(dT4), value = TRUE),
  SB_selfEfficacyperceivedBehaviouralControl_T4 = grep('SB_selfEfficacyperceivedBehaviouralControl', names(dT4), value = TRUE),
  symptom_T4 = grep('symptom', names(dT4), value = TRUE),
  PA_actionplan_T4 = grep('actionPlanning', names(dT4), value = TRUE),
  PA_copingplan_T4 = grep('copingPlanning', names(dT4), value = TRUE)
)

# Append the aggregate variables to the data frame
newdf3 <- makeScales_NEW(dT4, scales_T4)

# Take the demographic variables from d, combine with the variables with T1 or T3 in the name. 
df <- cbind(d[ , c("id", "intervention", "group", "school", "girl", "track")], newdf[, ], newdf2[, ], newdf3[, ])

# Create composite of self-efficacy and perceived behavioural control
df <- df %>% rowwise %>%
  mutate(PA_selfEfficacyperceivedBehaviouralControl_T1 = mean(c(PA_perceivedBehaviouralControl_T1, PA_selfefficacy_T1), na.rm = T),
         PA_selfEfficacyperceivedBehaviouralControl_T3 = mean(c(PA_perceivedBehaviouralControl_T3, PA_selfefficacy_T3), na.rm = T),
         PA_selfEfficacyperceivedBehaviouralControl_T4 = mean(c(PA_perceivedBehaviouralControl_T4, PA_selfefficacy_T4), na.rm = T))
## Fix the "NaN"s
df$PA_selfEfficacyperceivedBehaviouralControl_T1[is.nan(df$PA_selfEfficacyperceivedBehaviouralControl_T1)] <- NA
df$PA_selfEfficacyperceivedBehaviouralControl_T3[is.nan(df$PA_selfEfficacyperceivedBehaviouralControl_T3)] <- NA
df$PA_selfEfficacyperceivedBehaviouralControl_T4[is.nan(df$PA_selfEfficacyperceivedBehaviouralControl_T4)] <- NA

# Create composite of action planning and coping planning
df <- df %>% rowwise %>%
  mutate(PA_actCop_T1 = mean(c(PA_actionplan_T1, PA_copingplan_T1), na.rm = T),
         PA_actCop_T3 = mean(c(PA_actionplan_T3, PA_copingplan_T3), na.rm = T),
         PA_actCop_T4 = mean(c(PA_actionplan_T4, PA_copingplan_T4), na.rm = T))
## Fix the "NaN"s
df$PA_actCop_T1[is.nan(df$PA_actCop_T1)] <- NA
df$PA_actCop_T3[is.nan(df$PA_actCop_T3)] <- NA
df$PA_actCop_T4[is.nan(df$PA_actCop_T4)] <- NA

# Create variables for motivational regulations

regulationVariables_T1 <- lmi %>% dplyr::select(
  PA_extrinsic_01_T1 = Kys0080.1,
  PA_extrinsic_02_T1 = Kys0081.1,
  PA_extrinsic_03_T1 = Kys0083.1,
  PA_introjected_01_T1 = Kys0084.1,
  PA_introjected_02_T1 = Kys0085.1,
  PA_identified_01_T1 = Kys0087.1,
  PA_identified_02_T1 = Kys0088.1,
  PA_identified_03_T1 = Kys0090.1,
  PA_integrated_01_T1 = Kys0089.1,
  PA_integrated_02_T1 = Kys0092.1,
  PA_integrated_03_T1 = Kys0094.1,
  PA_intrinsic_01_T1 = Kys0091.1,
  PA_intrinsic_02_T1 = Kys0093.1,
  PA_intrinsic_03_T1 = Kys0095.1)

regulationVariables_T3 <- lmi %>% dplyr::select(
  PA_extrinsic_01_T3 = Kys0080.3,
  PA_extrinsic_02_T3 = Kys0081.3,
  PA_extrinsic_03_T3 = Kys0083.3,
  PA_introjected_01_T3 = Kys0084.3,
  PA_introjected_02_T3 = Kys0085.3,
  PA_identified_01_T3 = Kys0087.3,
  PA_identified_02_T3 = Kys0088.3,
  PA_identified_03_T3 = Kys0090.3,
  PA_integrated_01_T3 = Kys0089.3,
  PA_integrated_02_T3 = Kys0092.3,
  PA_integrated_03_T3 = Kys0094.3,
  PA_intrinsic_01_T3 = Kys0091.3,
  PA_intrinsic_02_T3 = Kys0093.3,
  PA_intrinsic_03_T3 = Kys0095.3
)

regulationVariables_T4 <- lmi %>% dplyr::select(
  PA_extrinsic_01_T4 = Kys0080.4,
  PA_extrinsic_02_T4 = Kys0081.4,
  PA_extrinsic_03_T4 = Kys0083.4,
  PA_introjected_01_T4 = Kys0084.4,
  PA_introjected_02_T4 = Kys0085.4,
  PA_identified_01_T4 = Kys0087.4,
  PA_identified_02_T4 = Kys0088.4,
  PA_identified_03_T4 = Kys0090.4,
  PA_integrated_01_T4 = Kys0089.4,
  PA_integrated_02_T4 = Kys0092.4,
  PA_integrated_03_T4 = Kys0094.4,
  PA_intrinsic_01_T4 = Kys0091.4,
  PA_intrinsic_02_T4 = Kys0093.4,
  PA_intrinsic_03_T4 = Kys0095.4
)

motiscales_T1 <- list(
  PA_intrinsic_T1 = grep('intrinsic', names(regulationVariables_T1), value = TRUE),
  PA_integrated_T1 = grep('integrated', names(regulationVariables_T1), value = TRUE),
  PA_identified_T1 = grep('identified', names(regulationVariables_T1), value = TRUE),
  PA_introjected_T1 = grep('introjected', names(regulationVariables_T1), value = TRUE),
  PA_extrinsic_T1 = grep('extrinsic', names(regulationVariables_T1), value = TRUE))

motiscales_T3 <- list(
  PA_intrinsic_T3 = grep('intrinsic', names(regulationVariables_T3), value = TRUE),
  PA_integrated_T3 = grep('integrated', names(regulationVariables_T3), value = TRUE),
  PA_identified_T3 = grep('identified', names(regulationVariables_T3), value = TRUE),
  PA_introjected_T3 = grep('introjected', names(regulationVariables_T3), value = TRUE),
  PA_extrinsic_T3 = grep('extrinsic', names(regulationVariables_T3), value = TRUE))

motiscales_T4 <- list(
  PA_intrinsic_T4 = grep('intrinsic', names(regulationVariables_T4), value = TRUE),
  PA_integrated_T4 = grep('integrated', names(regulationVariables_T4), value = TRUE),
  PA_identified_T4 = grep('identified', names(regulationVariables_T4), value = TRUE),
  PA_introjected_T4 = grep('introjected', names(regulationVariables_T4), value = TRUE),
  PA_extrinsic_T4 = grep('extrinsic', names(regulationVariables_T4), value = TRUE))

motidf1 <- userfriendlyscience::makeScales(regulationVariables_T1, motiscales_T1)
motidf2 <- userfriendlyscience::makeScales(regulationVariables_T3, motiscales_T3)
motidf3 <- userfriendlyscience::makeScales(regulationVariables_T4, motiscales_T4)

df <- cbind(df, motidf1, motidf2, motidf3)

# Create change scores; see https://stackoverflow.com/questions/47478125/create-new-columns-by-substracting-column-pairs-from-each-other-in-r
t1_vars <- grep("_T1", colnames(df), value = TRUE)
t1_vars <- grep("big5|fat|paLastweek|PA_opportunitiesReverseCoded_08", t1_vars, value = TRUE, invert = TRUE) # drop variables not in T3
t3_vars <- grep("_T3", colnames(df), value = TRUE)
# t3_vars <- grep("teachers", t3_vars, value = TRUE, invert = TRUE) # drop variables not in T1
t4_vars <- grep("_T4", colnames(df), value = TRUE)

df[, paste0(stringr::str_sub(t1_vars, end = -4), "_diffT3T1")] <- df[, t3_vars] - df[, t1_vars] # This creates the change score for each variable
df[, paste0(stringr::str_sub(t1_vars, end = -4), "_diffT4T1")] <- df[, t4_vars] - df[, t1_vars] # This creates the change score for each variable

# Create a combination of track and school variables
df <- df %>% dplyr::mutate(trackSchool = paste0(track, school)) 

save(df, file = "./data/df_T1-T4.Rdata")
haven::write_sav(df, path = "./data/df_T1-T4.sav")

# Remove change scores and other time points from the baseline data frame
df <- df %>% dplyr::select(-contains("_diffT3T1"), -contains("_diffT4T1"), -contains("_T3"), -contains("_T4"))

save(df, file = "./data/df.Rdata")
haven::write_sav(df, path = "./data/df.sav")

# readr::write_rds(df, path = "./data/df_for_codebook.RDS")
