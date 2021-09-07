## ----packages, echo=FALSE, message=FALSE--------------------------------------
library(knitr)
library(dplyr)
library(magrittr)
library(usethis)
library(here)
library(ggplot2)
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
   dev        = 'svg',
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

## ---- results="markup", out.width="100%"------------------------------------------
include_graphics(here("man/figures/sketch.png"))

## ---- eval=FALSE, echo=FALSE------------------------------------------------------
#  # note for posterity - this is how data/UVVisDataZnO.rda was created
#  # original data source: /media/bay/taha/chepec/papers/01_drafts/P12-ZnO-QDs-sizes-photocatalysis/data/abs-pc-spectra-N04H-small-nostir.rda.gz
#  # was manually copied to inst/extdata/spectra.rda.gz
#  # exp_data <- common::LoadRData2Variable(here::here("inst/extdata/spectra.rda.gz"))
#  # UVVisDataZnO <-
#  #    exp_data %>%
#  #    filter(wavelength < photoec::energy2wavelength(2.5)) %>%
#  #    select(-range, -DateTime)
#  # usethis::use_data(UVVisDataZnO, compress = "bzip2", overwrite = TRUE)

## ---- echo=TRUE, results="markup"-------------------------------------------------
UVVisDataZnO <- common::LoadRData2Variable(here::here("data/UVVisDataZnO.rda"))

## ----echo=TRUE, results="markup"--------------------------------------------------
spectrum <-
   UVVisDataZnO %>%
   filter(sampleid == "N04H-00000") %>%
   filter(wavelength < energy2wavelength(3))
spectrum$energy <- photoec::wavelength2energy(spectrum$wavelength)
resdf <-
   uvvistauc::tauc(
      spectrum$energy, 
      spectrum$intensity, 
      lowE.limits = c(2.8, 3.4),
      highE.limits = c(3.9, 4.3))

## ----echo=FALSE-------------------------------------------------------------------
ggplot(resdf) +
   coord_cartesian(ylim = c(0, 7), xlim = c(3.0, 4.3)) +
   geom_line(aes(energy, fit.tauc), colour = "orange", size = 1.5, alpha = 0.65) +
   geom_point(aes(energy, absorbance)) +
   geom_line(aes(energy, absorbance)) +
   geom_line(aes(energy, floor), colour = "red") +
   geom_line(aes(energy, ceiling), colour = "green") +
   geom_point(
      aes(energy, absorbance),
      colour = "orange", size = 1.5,
      data = resdf %>% filter(edge == TRUE)) +
   labs(x = "E/eV", y = "(Abs)<sup>1/r</sup>") +
   theme(axis.title.y = element_markdown())

