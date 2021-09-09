## ----packages, echo=FALSE, message=FALSE--------------------------------------
library(knitr)
library(dplyr)
library(magrittr)
library(tibble)
library(usethis)
library(here)
# library(ggplot2)   # loaded by gganimate
library(gganimate)
library(cowplot)
library(ggtext)
library(common)
library(photoec)
library(conflicted)
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("Position", "ggplot2")

## ----global_options, echo=FALSE, message=FALSE------------------------------------
options(
   digits   = 7,
   width    = 84,
   continue = " ",
   prompt   = "> ",
   warn = 0,
   stringsAsFactors = FALSE)
opts_chunk$set(
   # with 'svg', the multi-spectra plot was making intro.html > 5 Mb
   dev        = 'png',
   fig.width  = 7.10,
   fig.height = 4.39,
   fig.align  = 'center',
   echo       = FALSE,
   eval       = TRUE,
   cache      = FALSE,
   collapse   = TRUE,
   results    = 'hide',
   message    = FALSE,
   warning    = FALSE,
   tidy       = FALSE)

## ----parameters, echo=FALSE-------------------------------------------------------
r            <- 0.5
lowE.limits  <- c(3.0, 3.4)
highE.limits <- c(3.9, 4.3)
bg.limits    <- c(0.3, 0.8)
anim_path    <- here("man/figures/animation.gif")

## ----sketch, results="markup", out.width="100%"-----------------------------------
include_graphics(here("man/figures/sketch.png"))

## ----datareset, eval=FALSE, echo=FALSE--------------------------------------------
#  # note for posterity - this is how data/UVVisDataZnO.rda was created
#  # original data source: /media/bay/taha/chepec/papers/01_drafts/P12-ZnO-QDs-sizes-photocatalysis/data/abs-pc-spectra-N04H-small-nostir.rda.gz
#  # was manually copied to inst/extdata/spectra.rda.gz
#  # exp_data <- common::LoadRData2Variable(here("inst/extdata/spectra.rda.gz"))
#  # UVVisDataZnO <-
#  #    exp_data %>%
#  #    filter(
#  #       wavelength < photoec::energy2wavelength(2.5) &
#  #       wavelength > photoec::energy2wavelength(4.5)) %>%
#  #    select(-range, -DateTime) %>%
#  #    mutate(time = sampleid %>% gsub("^N04H-", "", .) %>% as.numeric()) %>%
#  #    select(sampleid, time, everything())
#  # usethis::use_data(UVVisDataZnO, compress = "bzip2", overwrite = TRUE)

## ----ref.label="parameters", echo=TRUE, eval=FALSE--------------------------------
#  r            <- 0.5
#  lowE.limits  <- c(3.0, 3.4)
#  highE.limits <- c(3.9, 4.3)
#  bg.limits    <- c(0.3, 0.8)
#  anim_path    <- here("man/figures/animation.gif")

## ----dataset, echo=TRUE, results="markup"-----------------------------------------
this.data <- uvvistauc::UVVisDataZnO
# convert wavelength in nm to energy in eV
this.data %<>% mutate(energy = photoec::wavelength2energy(wavelength))

## ----single-spectrum-call, echo=TRUE, results="markup"----------------------------
this.spectrum <-
   this.data %>%
   filter(sampleid == "N04H-00000")
spectrum <-
   uvvistauc::tauc(
      energy       = this.spectrum$energy,
      absorbance   = this.spectrum$intensity,
      r            = r,
      lowE.limits  = lowE.limits,
      highE.limits = highE.limits,
      bg.limits    = bg.limits)

## ----single-spectrum-plot, echo=FALSE---------------------------------------------
ggplot(spectrum) +
   coord_cartesian(ylim = c(0, 7), xlim = c(3.0, 4.3)) +
   geom_line(aes(energy, fit.tauc), colour = "orange", size = 1.5, alpha = 0.65) +
   geom_point(aes(energy, absorbance)) +
   geom_line(aes(energy, absorbance)) +
   geom_line(aes(energy, floor), colour = "red") +
   geom_line(aes(energy, ceiling), colour = "#008000") +
   geom_point(
      aes(energy, absorbance),
      colour = "orange", size = 1.5,
      data = spectrum %>% filter(edge == TRUE)) +
   labs(x = "E/eV", y = paste0("(Abs)<sup>", 1/r, "</sup>")) +
   theme(axis.title.y = element_markdown())

## ----multi-spectra-plot, echo=TRUE------------------------------------------------
ggplot(spectra, aes(group = sampleid)) +
   coord_cartesian(ylim = c(0, 7), xlim = c(3, 4.4)) +
   geom_line(aes(energy, floor), colour = "red", size = 0.2, alpha = 0.4) +
   geom_line(aes(energy, ceiling), colour = "#008000", size = 0.2, alpha = 0.4) +
   geom_line(aes(energy, absorbance), colour = "black", size = 0.3) +
   geom_line(aes(energy, fit.tauc), colour = "orange", size = 0.3, alpha = 0.6) +
   labs(x = "E/eV", y = paste0("(Abs)<sup>", 1/r, "</sup>")) +
   theme(axis.title.y = element_markdown())

## ----fitstat, echo=TRUE, results="markup"-----------------------------------------
summary(spectra$fit.adj_rsq)
summary(spectra$fit.points)

## ----fitstatplot, echo=TRUE-------------------------------------------------------
p1 <- ggplot(spectra) +
   geom_point(aes(time, fit.Eg)) +
   theme(axis.text = element_text(size = 11), axis.title.x = element_blank())
p2 <- ggplot(spectra) +
   geom_point(aes(time, fit.adj_rsq)) +
   theme(axis.text = element_text(size = 11), axis.title.x = element_blank())
p3 <- ggplot(spectra) +
   geom_point(aes(time, fit.points)) +
   labs(x = "Time/min") +
   theme(axis.text = element_text(size = 11))
plot_grid(p1, p2, p3, nrow = 3, align = "v")

## ---- results="markup", out.width="100%"------------------------------------------
include_graphics(anim_path)

