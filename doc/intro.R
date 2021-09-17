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
   geom_point(aes(energy, abs.2)) +
   geom_line(aes(energy, abs.2)) +
   geom_line(aes(energy, floor), colour = "red") +
   geom_line(aes(energy, ceiling), colour = "#008000") +
   geom_point(
      aes(energy, abs.2),
      colour = "orange", size = 1.5,
      data = spectrum %>% filter(edge == TRUE)) +
   labs(x = "E/eV", y = paste0("(Abs)<sup>", 1/r, "</sup>")) +
   theme(axis.title.y = element_markdown())

## ----multi-spectra-call, cache=TRUE-----------------------------------------------
# individual spectra in the dataset are identified by their sampleid
sampleids <- this.data %>% pull(sampleid) %>% unique()
# create a tibble of sampleids and input params
# if you want to customise *.limits for each spectrum, supply a vector
# for each column instead of a single value
tauc.params <-
   tibble(
      sampleid = sampleids,
      lowE.min = min(lowE.limits), lowE.max = max(lowE.limits),
      highE.min = min(highE.limits), highE.max = max(highE.limits),
      bg.min = min(bg.limits), bg.max = max(bg.limits))
# empty dataframe that we will populate in the loop
spectra <- data.frame()
for (s in 1:length(sampleids)) {
   spectra <- 
      bind_rows(
         spectra,
         bind_cols(
            # we want to save sampleid and time columns from the data dataframe into the spectra df
            sampleid = sampleids[s],
            time = (this.data %>% pull(time) %>% unique())[s],
            uvvistauc::tauc(
               energy = this.data %>% filter(sampleid == sampleids[s]) %>% pull(energy),
               absorbance = this.data %>% filter(sampleid == sampleids[s]) %>% pull(intensity),
               r = 0.5,
               lowE.limits = 
                  c(tauc.params %>% filter(sampleid == sampleids[s]) %>% pull(lowE.min),
                    tauc.params %>% filter(sampleid == sampleids[s]) %>% pull(lowE.max)),
               highE.limits = 
                  c(tauc.params %>% filter(sampleid == sampleids[s]) %>% pull(highE.min),
                    tauc.params %>% filter(sampleid == sampleids[s]) %>% pull(highE.max)),
               bg.limits = 
                  c(tauc.params %>% filter(sampleid == sampleids[s]) %>% pull(bg.min),
                    tauc.params %>% filter(sampleid == sampleids[s]) %>% pull(bg.max)))))
   message(paste("Completed", sampleids[s]))
}

## ----multi-spectra-plot, echo=TRUE------------------------------------------------
ggplot(spectra, aes(group = sampleid)) +
   coord_cartesian(ylim = c(0, 7), xlim = c(3, 4.4)) +
   geom_line(aes(energy, floor), colour = "red", size = 0.2, alpha = 0.4) +
   geom_line(aes(energy, ceiling), colour = "#008000", size = 0.2, alpha = 0.4) +
   geom_line(aes(energy, abs.2), colour = "black", size = 0.3) +
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

## ----animation, echo=FALSE, cache=TRUE--------------------------------------------
p <-
   ggplot() +
   # let's concentrate on the band edge
   coord_cartesian(
      ylim = c(0, 6),
      xlim = c(unique(tauc.params$lowE.min), unique(tauc.params$highE.max))) +
   # x-axis # layer 1
   geom_hline(yintercept = 0, colour = "grey40") +
   # "floor" # layer 2
   geom_line(
      data = spectra, aes(energy, floor, group = sampleid),
      colour = "red", size = 0.60) +
   # "ceiling" # layer 3
   geom_line(
      data = spectra, aes(energy, ceiling, group = sampleid),
      colour = "#008000", size = 0.60) +
   # the UV-Vis spectra # layer 4
   geom_line(
      data = spectra, aes(energy, abs.2, group = sampleid),
      colour = "black", size = 0.50) +
   # the Tauc fit # layer 5
   geom_line(
      data = spectra, aes(energy, fit.tauc, group = sampleid),
      colour = "orange", size = 1.0) +
   # the spectral datapoints fitted selected by our algorithm for Tauc fitting # layer 6
   geom_point(
      data = spectra %>% filter(edge == TRUE),
      aes(energy, abs.2, group = sampleid),
      fill = "orange", colour = "white", shape = 21, size = 2.0) +
   # the point where fitted Tauc line intercepts x-axis, i.e., optical band gap # layer 7
   geom_point(
      data = spectra, aes(x = fit.Eg, group = sampleid), y = 0,
      fill = NA, colour = "black", shape = 21, size = 5) +
   # text annotation # layer 8
   geom_richtext(
      data = spectra, x = 3.0, y = 6, size = 4.5,
      fill = NA, label.color = NA, hjust = 0, vjust = 1,
      aes(label = paste("t =", time, "min"))) +
   labs(x = "E/eV", y = paste0("(Abs)<sup>", 1/r, "</sup>")) +
   theme(
      axis.text = element_text(size = 11),
      axis.title.y = element_markdown())
p_anim <-
   p + 
   transition_time(time) +
   # it appears that if you issue multiple shadow_mark() statements, only the last one takes effect
   # so therefore I could not achieve separate shadow *colours* for ceiling/floor/spectrum
   # I would have liked each layer's shadow to be a faded version of its original colour...
   # https://github.com/thomasp85/gganimate/issues/163 (sort of related issue)
   shadow_mark(
      colour = "grey30", alpha = 0.4, size = 0.15,
      exclude_layer = c(2, 3, 5, 6, 7, 8),
      past = TRUE, future = FALSE)
p_anim_gif <-
   animate(
      plot = p_anim,
      width = 1294, height = 800, units = "px", # produces gif approx 5 Mb
      res = 150,
      fps = 1, # default is 10
      end_pause = 10,
      # make one frame per UV/Vis spectrum
      nframes = spectra %>% pull(time) %>% n_distinct())
anim_save(anim_path)

## ---- results="markup", out.width="100%"------------------------------------------
include_graphics(anim_path)

