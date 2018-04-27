
# To start data analysis, we set up the basics to enable compiling the document:
  
## Remember to tell R where your packages are.
# .libPaths("C:/rlibs/3.4.2")

# If pacman-package is not installed, install it and then load it.
if (!require(pacman)) install.packages("pacman")
#library(pacman)

CRANpacks <- c("viridis", "bookdown", "knitr", "tidyverse", "haven", "lme4", 
               "userfriendlyscience", "sm", "sjstats", "gridExtra", "igraph", 
               "devtools","EstimateGroupNetwork", "bootnet", "qgraph","rstanarm",
               "brms", "mlmRev", "rstan", "sandwich", "visreg", "broom", 
               "EstimateGroupNetwork", "bootnet", "qgraph", # for networks
               "corrgram", "sjPlot", "DT") 

instpacks <- setdiff(CRANpacks, pacman::p_library())

# Use pacman to install the needed packages.
if (length(instpacks)>0) install.packages(instpacks)

if (!require(papaja)) pacman::p_install_gh("crsh/papaja")
if (!require(ggridges)) pacman::p_install_gh("clauswilke/ggridges")
if (!require(brmstools)) pacman::p_install_gh("mvuorre/brmstools")
if (!require(NetworkComparisonTest)) pacman::p_install_gh("sachaepskamp/NetworkComparisonTest")
if (!require(ggstatsplot)) pacman::p_install_gh("IndrajeetPatil/ggstatsplot")
if (!require(patchwork)) pacman::p_install_gh("thomasp85/patchwork") # so that plot1 + plot2 creates a single side-by-side plot

# Packages for the figures which show Bayesian credible intervals of classes
#pacman::p_install(c("rstanarm", "brms", "mlmRev"))

pacman::p_load(knitr, tidyverse)

knitr::opts_chunk$set(echo = TRUE, 
                      warning = TRUE,
                      error = TRUE,
                      cache = TRUE, 
                      collapse = TRUE,
                      eval = TRUE,
                      dpi = 300)
knitr::opts_chunk$set(root.dir = ".")  # Always set project root as working directory
knitr::opts_knit$set(root.dir = ".")  # This is needed for some versions of RStudio
knitr::opts_chunk$set(echo = TRUE, rows.print=15)

ggplot2::theme_set(papaja::theme_apa())

options(DT.options = list(columnDefs = list(list(className = 'dt-center', targets = "_all"))))

# Read data.

#lmi <- haven::read_sav("data/LMI_data_korjattu_syntaksilla_nimetpoistettu.sav", user_na = FALSE)

lmi <- haven::read_sav("data/LMI_data_korjattu_syntaksilla_nimetpoistettu - skandit muutettu MANUAALISESTI.sav", user_na = FALSE)
# ses <- haven::read_sav("data/baselinedata_vainnuoret SPSS_160226_accfac fixed.sav", user_na = FALSE)
# ses <- read_csv2("data/baselinedata_vainnuoret SPSS_160226_accfac fixed.csv")


# write_csv(lmi, "Z:/Desktop/LMI DATA/LMI_data_korjattu_syntaksilla_CSV.csv")

# to get rid of DLL warning, these don't work: 
# Sys.setenv("R_MAX_NUM_DLLS" = 1000)

# Unloading all packages gives LAPACK error:
# pkgs <- names(sessionInfo()$otherPkgs)
# pkgs <- paste('package:', pkgs, sep = "")
# lapply(pkgs, detach, character.only = TRUE, unload = TRUE)

# What seems to work is create a file .Renviron in the project root, which contains
# 

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

# We do data manipulation and cleaning to come up with a dataaset with our variables of interests.

#library(userfriendlyscience)

d <- lmi %>% dplyr::select(id = ID,
                           intervention = ryhma,
                           group = ryhmakoodi_korjattu,
                           school = Aineisto.1,
                           girl = Kys0013.1,
                           big5agreeableness_01_T1 = Kys0155.1,
                           big5agrReverseCoded_02_T1 = Kys0150.1,
                           big5conscientiousness_01_T1 = Kys0151.1,
                           big5consReverseCoded_02_T1 = Kys0156.1,
                           big5extraversion_01_T1 = Kys0149.1,
                           big5extReverseCoded_02_T1 = Kys0154.1,
                           big5neuroticism_01_T1 = Kys0152.1,
                           big5neurReverseCoded_02_T1 = Kys0157.1,
                           big5openness_01_T1 = Kys0153.1,
                           big5openReverseCoded_02_T1 = Kys0158.1,
                           fatpct_T1 = Rasva,
                           PA_actCop_01_T1 = Kys0115.1,
                           PA_actCop_01_T3 = Kys0115.3,
                           PA_actCop_02_T1 = Kys0116.1,
                           PA_actCop_02_T3 = Kys0116.3,
                           PA_actCop_03_T1 = Kys0117.1,
                           PA_actCop_03_T3 = Kys0117.3,
                           PA_actCop_04_T1 = Kys0118.1,
                           PA_actCop_04_T3 = Kys0118.3,
                           PA_agrbct_01_T1 = Kys0128.1,
                           PA_agrbct_01_T3 = Kys0128.3,
                           PA_agrbct_02_T1 = Kys0129.1,
                           PA_agrbct_02_T3 = Kys0129.3,
                           PA_agrbct_03_T1 = Kys0130.1,
                           PA_agrbct_03_T3 = Kys0130.3,
                           PA_agrbct_04_T1 = Kys0131.1,
                           PA_agrbct_04_T3 = Kys0131.3,
                           PA_agrbct_05_T1 = Kys0132.1,
                           PA_agrbct_05_T3 = Kys0132.3,
                           PA_agrbct_06_T1 = Kys0133.1,
                           PA_agrbct_06_T3 = Kys0133.3,
                           PA_agrbct_07_T1 = Kys0134.1,
                           PA_agrbct_07_T3 = Kys0134.3,
                           PA_agrbct_08_T1 = Kys0135.1,
                           PA_agrbct_08_T3 = Kys0135.3,
                           PA_agrbct_09_T1 = Kys0136.1,
                           PA_agrbct_09_T3 = Kys0136.3,
                           PA_agrbct_10_T1 = Kys0137.1,
                           PA_agrbct_10_T3 = Kys0137.3,
                           PA_amotivation_01_T1 = Kys0082.1,
                           PA_amotivation_01_T3 = Kys0082.3,
                           PA_amotivation_02_T1 = Kys0086.1,
                           PA_amotivation_02_T3 = Kys0086.3,
                           PA_amotivation_03_T1 = Kys0096.1,
                           PA_amotivation_03_T3 = Kys0096.3,
                           PA_amotivation_04_T1 = Kys0097.1,
                           PA_amotivation_04_T3 = Kys0097.3,
                           PA_actCop_05_T1 = Kys0119.1,
                           PA_actCop_05_T3 = Kys0119.3,
                           PA_actCop_06_T1 = Kys0120.1,
                           PA_actCop_06_T3 = Kys0120.3,
                           PA_actCop_07_T1 = Kys0121.1,
                           PA_actCop_07_T3 = Kys0121.3,
                           PA_actCop_08_T1 = Kys0122.1,
                           PA_actCop_08_T3 = Kys0122.3,
                           PA_dnorm_02_T1 = Kys0107.1,
                           PA_dnormparents_02_T3 = Kys0107.3,
                           PA_dnorm_01_T1 = Kys0106.1,
                           PA_dnorm_01_T3 = Kys0106.3,
                           PA_controlled_01_T1 = Kys0080.1,
                           PA_controlled_01_T3 = Kys0080.3,
                           PA_controlled_02_T1 = Kys0081.1,
                           PA_controlled_02_T3 = Kys0081.3,
                           PA_controlled_03_T1 = Kys0083.1,
                           PA_controlled_03_T3 = Kys0083.3,
                           PA_frqbct_01_T1 = Kys0138.1,
                           PA_frqbct_01_T3 = Kys0138.3,
                           PA_frqbct_02_T1 = Kys0139.1,
                           PA_frqbct_02_T3 = Kys0139.3,
                           PA_frqbct_03_T1 = Kys0140.1,
                           PA_frqbct_03_T3 = Kys0140.3,
                           PA_frqbct_04_T1 = Kys0141.1,
                           PA_frqbct_04_T3 = Kys0141.3,
                           PA_frqbct_05_T1 = Kys0142.1,
                           PA_frqbct_05_T3 = Kys0142.3,
                           PA_frqbct_06_T1 = Kys0143.1,
                           PA_frqbct_06_T3 = Kys0143.3,
                           PA_frqbct_07_T1 = Kys0144.1,
                           PA_frqbct_07_T3 = Kys0144.3,
                           PA_frqbct_08_T1 = Kys0145.1,
                           PA_frqbct_08_T3 = Kys0145.3,
                           PA_frqbct_09_T1 = Kys0146.1,
                           PA_frqbct_09_T3 = Kys0146.3,
                           PA_goal_01_T1 = Kys0147.1,
                           PA_goal_01_T3 = Kys0147.3,
                           PA_autonomous_01_T1 = Kys0087.1,
                           PA_autonomous_01_T3 = Kys0087.3,
                           PA_autonomous_02_T1 = Kys0088.1,
                           PA_autonomous_02_T3 = Kys0088.3,
                           PA_autonomous_03_T1 = Kys0090.1,
                           PA_autonomous_03_T3 = Kys0090.3,
                           PA_inorm_01_T1 = Kys0108.1,
                           PA_inorm_01_T3 = Kys0108.3,
                           PA_autonomous_04_T1 = Kys0089.1,
                           PA_autonomous_04_T3 = Kys0089.3,
                           PA_autonomous_05_T1 = Kys0092.1,
                           PA_autonomous_05_T3 = Kys0092.3,
                           PA_autonomous_06_T1 = Kys0094.1,
                           PA_autonomous_06_T3 = Kys0094.3,
                           PA_intention_01_T1 = Kys0113.1,
                           PA_intention_01_T3 = Kys0113.3,
                           PA_intention_02_T1 = Kys0114.1,
                           PA_intention_02_T3 = Kys0114.3,
                           PA_autonomous_07_T1 = Kys0091.1,
                           PA_autonomous_07_T3 = Kys0091.3,
                           PA_autonomous_08_T1 = Kys0093.1,
                           PA_autonomous_08_T3 = Kys0093.3,
                           PA_autonomous_09_T1 = Kys0095.1,
                           PA_autonomous_09_T3 = Kys0095.3,
                           PA_controlled_04_T1 = Kys0084.1,
                           PA_controlled_04_T3 = Kys0084.3,
                           PA_controlled_05_T1 = Kys0085.1,
                           PA_controlled_05_T3 = Kys0085.3,
                           PA_outcomeExpectations_01_T1 = Kys0068.1,
                           PA_outcomeExpectations_01_T3 = Kys0068.3,
                           PA_outcomeExpectations_03_T1 = Kys0070.1,
                           PA_outcomeExpectations_03_T3 = Kys0070.3,
                           PA_outcomeExpectations_04_T1 = Kys0071.1,
                           PA_outcomeExpectations_04_T3 = Kys0071.3,
                           PA_outcomeExpectations_05_T1 = Kys0072.1,
                           PA_outcomeExpectations_05_T3 = Kys0072.3,
                           PA_outcomeExpectations_06_T1 = Kys0073.1,
                           PA_outcomeExpectations_06_T3 = Kys0073.3,
                           PA_outcomeExpectations_07_T1 = Kys0074.1,
                           PA_outcomeExpectations_07_T3 = Kys0074.3,
                           PA_outcomeExpectations_10_T1 = Kys0077.1,
                           PA_outcomeExpectations_10_T3 = Kys0077.3,
                           PA_outcomeExpectations_11_T1 = Kys0078.1,
                           PA_outcomeExpectations_11_T3 = Kys0078.3,
                           PA_outcomeExpectations_12_T1 = Kys0079.1,
                           PA_outcomeExpectations_12_T3 = Kys0079.3,
                           PA_outcomeExpectationsNegative_02_T1 = Kys0069.1,
                           PA_outcomeExpectationsNegative_02_T3 = Kys0069.3,
                           PA_outcomeExpectationsNegative_08_T1 = Kys0075.1,
                           PA_outcomeExpectationsNegative_08_T3 = Kys0075.3,
                           PA_outcomeExpectationsNegative_09_T1 = Kys0076.1,
                           PA_outcomeExpectationsNegative_09_T3 = Kys0076.3,
                           PA_opportunities_01_T1 = Kys0098.1,
                           PA_opportunities_01_T3 = Kys0098.3,
                           PA_opportunities_02_T1 = Kys0099.1,
                           PA_opportunities_02_T3 = Kys0099.3,
                           PA_opportunities_04_T1 = Kys0101.1,
                           PA_opportunities_04_T3 = Kys0101.3,
                           PA_opportunities_05_T1 = Kys0102.1,
                           PA_opportunities_05_T3 = Kys0102.3,
                           PA_opportunities_07_T1 = Kys0104.1,
                           PA_opportunities_07_T3 = Kys0104.3,
                           PA_opportunitiesReverseCoded_03_T1 = Kys0100.1,
                           PA_opportunitiesReverseCoded_03_T3 = Kys0100.3,
                           PA_opportunitiesReverseCoded_06_T1 = Kys0103.1,
                           PA_opportunitiesReverseCoded_06_T3 = Kys0103.3,
                           PA_opportunitiesReverseCoded_08_T1 = Kys0105.1,
                           # No answers or not asked T3: PA_opportunitiesReverseCoded_08_T3 = Kys0105.3,
                           PA_pbc_01_T1 = Kys0125.1,
                           PA_pbc_01_T3 = Kys0125.3,
                           PA_pbc_03_T1 = Kys0127.1,
                           PA_pbc_03_T3 = Kys0127.3,
                           PA_pbcReverseCoded_02_T1 = Kys0126.1,
                           PA_pbcReverseCoded_02_T3 = Kys0126.3,
                           PA_selfefficacy_01_T1 = Kys0123.1,
                           PA_selfefficacy_01_T3 = Kys0123.3,
                           PA_selfefficacyReverseCoded_02_T1 = Kys0124.1,
                           PA_selfefficacyReverseCoded_02_T3 = Kys0124.3,
                           paAccelerometer_T1 = LiikuntaT1_ka,
                           paAccelerometer_T3 = LiikuntaT3_ka,
                           padaysLastweek_T1 = Kys0045.1,
                           padaysLastweek_T3 = Kys0045.3,
                           pafreqUsually_T1 = Kys0048.1,
                           pafreqUsually_T3 = Kys0048.3,
                           pahrsLastweek_T1 = Kys0046.1,
                           pahrsLastweek_T3 = Kys0046.3,
                           pahrsUsually_T1 = Kys0049.1,
                           pahrsUsually_T3 = Kys0049.3,
                           paminLastweek_T1 = Kys0047.1,
                           paminLastweek_T3 = Kys0047.3,
                           SB_dnorm_01_T1 = Kys0178.1,
                           SB_dnorm_01_T3 = Kys0178.3,
                           SB_dnorm_02_T1 = Kys0179.1,
                           SB_dnorm_02_T3 = Kys0179.3,
                           SB_inorm_01_T1 = Kys0180.1,
                           SB_inorm_01_T3 = Kys0180.3,
                           SB_inorm_02_T1 = Kys0181.1,
                           SB_inorm_02_T3 = Kys0181.3,
                           SB_intention_01_T1 = Kys0187.1,
                           SB_intention_01_T3 = Kys0187.3,
                           SB_intention_02_T1 = Kys0188.1,
                           SB_intention_02_T3 = Kys0188.3,
                           SB_intention_03_T1 = Kys0189.1,
                           SB_intention_03_T3 = Kys0189.3,
                           SB_intention_04_T1 = Kys0190.1,
                           SB_intention_04_T3 = Kys0190.3,
                           SB_outcomeExpectationsNegative_01_T1 = Kys0171.1,
                           SB_outcomeExpectationsNegative_01_T3 = Kys0171.3,
                           SB_outcomeExpectations_02_T1 = Kys0172.1,
                           SB_outcomeExpectations_02_T3 = Kys0172.3,
                           SB_outcomeExpectations_03_T1 = Kys0173.1,
                           SB_outcomeExpectations_03_T3 = Kys0173.3,
                           SB_outcomeExpectations_04_T1 = Kys0174.1,
                           SB_outcomeExpectations_04_T3 = Kys0174.3,
                           SB_outcomeExpectations_05_T1 = Kys0175.1,
                           SB_outcomeExpectations_05_T3 = Kys0175.3,
                           SB_outcomeExpectations_06_T1 = Kys0176.1,
                           SB_outcomeExpectations_06_T3 = Kys0176.3,
                           SB_outcomeExpectationsNegative_07_T1 = Kys0177.1,
                           SB_outcomeExpectationsNegative_07_T3 = Kys0177.3,
                           SB_sePbc_01_T1 = Kys0182.1,
                           SB_sePbc_01_T3 = Kys0182.3,
                           SB_sePbc_02_T1 = Kys0183.1,
                           SB_sePbc_02_T3 = Kys0183.3,
                           SB_sePbc_03_T1 = Kys0184.1,
                           SB_sePbc_03_T3 = Kys0184.3,
                           SB_sePbc_04_T1 = Kys0185.1,
                           SB_sePbc_04_T3 = Kys0185.3,
                           SB_sePbc_05_T1 = Kys0186.1,
                           SB_sePbc_05_T3 = Kys0186.3,
                           sitLieAccelerometer_T1 = MaIsT1_ka,
                           sitLieAccelerometer_T3 = MaIsT3_ka,
                           symptom_neckShoulderPain_T1 = Kys0031.1,
                           symptom_neckShoulderPain_T3 = Kys0031.3,
                           symptom_lowerBackPain_T1 = Kys0032.1,
                           symptom_lowerBackPain_T3 = Kys0032.3,
                           symptom_stomachAche_T1 = Kys0033.1,
                           symptom_stomachAche_T3 = Kys0033.3,
                           symptom_tensionNervousness_T1 = Kys0034.1,
                           symptom_tensionNervousness_T3 = Kys0034.3,
                           symptom_irritabilityAngerbursts_T1 = Kys0035.1,
                           symptom_irritabilityAngerbursts_T3 = Kys0035.3,
                           symptom_sleepDifficulty_T1 = Kys0036.1,
                           symptom_sleepDifficulty_T3 = Kys0036.3,
                           symptom_headAche_T1 = Kys0037.1,
                           symptom_headAche_T3 = Kys0037.3,
                           symptom_tirednessFaintness_T1 = Kys0038.1,
                           symptom_tirednessFaintness_T3 = Kys0038.3,
                           PA_actplan_01_T1 = Kys0115.1,
                           PA_actplan_02_T1 = Kys0116.1,
                           PA_actplan_03_T1 = Kys0117.1,
                           PA_actplan_04_T1 = Kys0118.1,
                           PA_copplan_01_T1 = Kys0119.1,
                           PA_copplan_02_T1 = Kys0120.1,
                           PA_copplan_03_T1 = Kys0121.1,
                           PA_copplan_04_T1 = Kys0122.1,
                           PA_actplan_01_T3 = Kys0115.3,
                           PA_actplan_02_T3 = Kys0116.3,
                           PA_actplan_03_T3 = Kys0117.3,
                           PA_actplan_04_T3 = Kys0118.3,
                           PA_copplan_01_T3 = Kys0119.3,
                           PA_copplan_02_T3 = Kys0120.3,
                           PA_copplan_03_T3 = Kys0121.3,
                           PA_copplan_04_T3 = Kys0122.3,
                           sitBreaks_T1 = YlosT1_ka
)

# Reverse coded items to normal: 
# Take vars that contain "ReverseCoded_", substract them from 8 (each scale is 1-7)
d <- d %>% dplyr::mutate_at(dplyr::vars(contains("ReverseCoded_")), funs(8 - .))

# To check:
# d %>% dplyr::select(contains("ReverseCoded_")), contains("Rev")) %>% View
identical(as.numeric(8 - lmi$Kys0154.1), as.numeric(d$big5extReverseCoded_02_T1))
identical(as.numeric(8 - lmi$Kys0100.1), as.numeric(d$PA_opportunitiesReverseCoded_03_T1))

# Group variable has empty missings
d <- d %>% dplyr::mutate(group = ifelse(group == "", NA, group))

# Create grouping variable, which indicates  

# Fix intervention and gender variables
d <- d %>% dplyr::mutate(intervention = ifelse(intervention == 1, 1, 0),
                         intervention = factor(intervention),
                         girl = ifelse(girl == 2, 1, 0),
                         girl = factor(girl, levels = c("1", "0"), labels = c("girl", "boy")),
                         school = factor(school, levels = c("1", "2", "3", "4", "5")))

# Create the self-reported PA time variable
d <- d %>% dplyr::mutate(paLastweek_T1 = (pahrsLastweek_T1 * 60) + ifelse(paminLastweek_T1 == 2, 30, 0))

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
dT1 <- d %>% dplyr::select(contains("T1"))
dT3 <- d %>% dplyr::select(contains("T3"))

# Create T1 scales

scales_T1 <- list(big5agreeableness_T1 = grep('big5agreeableness', names(dT1), value = TRUE),
                  big5conscientiousness_T1 = grep('big5conscientiousness', names(dT1), value = TRUE),
                  big5extraversion_T1 = grep('big5extraversion', names(dT1), value = TRUE),
                  big5neuroticism_T1 = grep('big5neuroticism', names(dT1), value = TRUE),
                  big5openness_T1 = grep('big5openness', names(dT1), value = TRUE),
                  PA_actCop_T1 = grep('PA_actCop', names(dT1), value = TRUE),
                  PA_agrbct_T1 = grep('PA_agrbct', names(dT1), value = TRUE),
                  PA_amotivation_T1 = grep('PA_amotivation', names(dT1), value = TRUE),
                  PA_autonomous_T1 = grep('PA_autonomous', names(dT1), value = TRUE),
                  PA_controlled_T1 = grep('PA_controlled', names(dT1), value = TRUE),
                  PA_dnorm_T1 = grep('PA_dnorm', names(dT1), value = TRUE),
                  PA_frqbct_T1 = grep('PA_frqbct', names(dT1), value = TRUE),
                  PA_goal_T1 = grep('PA_goal', names(dT1), value = TRUE),
                  PA_inorm_T1 = grep('PA_inorm', names(dT1), value = TRUE),
                  PA_intention_T1 = grep('PA_intention', names(dT1), value = TRUE),
                  PA_outcomeExpectations_T1 = grep('PA_outcomeExpectations', names(dT1), value = TRUE),
                  PA_opportunities_T1 = grep('PA_opportunities', names(dT1), value = TRUE),
                  PA_pbc_T1 = grep('PA_pbc', names(dT1), value = TRUE),
                  PA_selfefficacy_T1 = grep('PA_selfefficacy', names(dT1), value = TRUE),
                  SB_dnorm_T1 = grep('SB_dnorm', names(dT1), value = TRUE),
                  SB_inorm_T1 = grep('SB_inorm', names(dT1), value = TRUE),
                  SB_intention_T1 = grep('SB_intention', names(dT1), value = TRUE),
                  SB_outcomeExpectations_T1 = grep('SB_outcomeExpectations', names(dT1), value = TRUE),
                  SB_sePbc_T1 = grep('SB_sePbc', names(dT1), value = TRUE),
                  symptom_T1 = grep('symptom', names(dT1), value = TRUE),
                  PA_actionplan_T1 = grep('actplan', names(dT1), value = TRUE),
                  PA_copingplan_T1 = grep('copplan', names(dT1), value = TRUE)
)

# Append the aggregate variables to the data frame
newdf <- userfriendlyscience::makeScales(dT1, scales_T1)

# Create T3 scales

scales_T3 <- list(big5agreeableness_T3 = grep('big5agr', names(dT3), value = TRUE),
                  big5conscientiousness_T3 = grep('big5cons', names(dT3), value = TRUE),
                  big5extraversion_T3 = grep('big5ext', names(dT3), value = TRUE),
                  big5neuroticism_T3 = grep('big5neur', names(dT3), value = TRUE),
                  big5openness_T3 = grep('big5open', names(dT3), value = TRUE),
                  PA_actCop_T3 = grep('PA_actCop', names(dT3), value = TRUE),
                  PA_agrbct_T3 = grep('PA_agrbct', names(dT3), value = TRUE),
                  PA_amotivation_T3 = grep('PA_amotivation', names(dT3), value = TRUE),
                  PA_autonomous_T3 = grep('PA_autonomous', names(dT3), value = TRUE),
                  PA_controlled_T3 = grep('PA_controlled', names(dT3), value = TRUE),
                  PA_dnorm_T3 = grep('PA_dnorm', names(dT3), value = TRUE),
                  PA_frqbct_T3 = grep('PA_frqbct', names(dT3), value = TRUE),
                  PA_goal_T3 = grep('PA_goal', names(dT3), value = TRUE),
                  PA_inorm_T3 = grep('PA_inorm', names(dT3), value = TRUE),
                  PA_intention_T3 = grep('PA_intention', names(dT3), value = TRUE),
                  PA_outcomeExpectations_T3 = grep('PA_outcomeExpectations', names(dT3), value = TRUE),
                  PA_opportunities_T3 = grep('PA_opportunities', names(dT3), value = TRUE),
                  PA_pbc_T3 = grep('PA_pbc', names(dT3), value = TRUE),
                  PA_selfefficacy_T3 = grep('PA_selfefficacy', names(dT3), value = TRUE),
                  SB_dnorm_T3 = grep('SB_dnorm', names(dT3), value = TRUE),
                  SB_inorm_T3 = grep('SB_inorm', names(dT3), value = TRUE),
                  SB_intention_T3 = grep('SB_intention', names(dT3), value = TRUE),
                  SB_outcomeExpectations_T3 = grep('SB_outcomeExpectations', names(dT3), value = TRUE),
                  SB_sePbc_T3 = grep('SB_sePbc', names(dT3), value = TRUE),
                  symptom_T3 = grep('symptom', names(dT3), value = TRUE),
                  PA_actionplan_T3 = grep('actplan', names(dT3), value = TRUE),
                  PA_copingplan_T3 = grep('copplan', names(dT3), value = TRUE)
)

# Append the aggregate variables to the data frame
newdf2 <- userfriendlyscience::makeScales(dT3, scales_T3)

# Take the demographic variables from d, combine with the variables with T1 or T3 in the name. 
df <- cbind(d[ , c("id", "intervention", "group", "school", "girl", "track")], newdf[, ], newdf2[, ])

# Create composite of self-efficacy and perceived behavioural control
df <- df %>% rowwise %>%
  mutate(PA_sePbc_T1 = mean(c(PA_pbc_T1, PA_selfefficacy_T1), na.rm = T),
         PA_sePbc_T3 = mean(c(PA_pbc_T3, PA_selfefficacy_T3), na.rm = T))
## Fix the "NaN"s
df$PA_sePbc_T1[is.nan(df$PA_sePbc_T1)] <- NA
df$PA_sePbc_T3[is.nan(df$PA_sePbc_T3)] <- NA

# Create composite of action planning and coping planning
df <- df %>% rowwise %>%
  mutate(PA_actCop_T1 = mean(c(PA_actionplan_T1, PA_copingplan_T1), na.rm = T),
         PA_actCop_T3 = mean(c(PA_actionplan_T3, PA_copingplan_T3), na.rm = T))
## Fix the "NaN"s
df$PA_actCop_T1[is.nan(df$PA_actCop_T1)] <- NA
df$PA_actCop_T3[is.nan(df$PA_actCop_T3)] <- NA

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

motidf1 <- userfriendlyscience::makeScales(regulationVariables_T1, motiscales_T1)
motidf2 <- userfriendlyscience::makeScales(regulationVariables_T3, motiscales_T3)

df <- cbind(df, motidf1, motidf2)

# Create change scores
t1_vars <- grep("_T1", colnames(df), value = TRUE)
t1_vars <- grep("big5|fat|paLastweek|PA_opportunitiesReverseCoded_08|sitBreaks_T1", t1_vars, value = TRUE, invert = TRUE) # drop variables not in T3
t3_vars <- grep("_T3", colnames(df), value = TRUE)
df[, paste0(stringr::str_sub(t1_vars, end = -4), "_diff")] <- df[, t3_vars] - df[, t1_vars]

# Create a combination of track and school variables
df <- df %>% dplyr::mutate(trackSchool = paste0(track, school)) 


