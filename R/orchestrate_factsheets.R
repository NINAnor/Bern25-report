### 
###  Orchestrating script for the production of species and habitat fact sheets for the Bern 25 delivery
###   
###  project: Bern_2025
###  Balint Czucz, NINA, 2026
###
###########################################

### Changelog:
# 260320  - initial version: feature factsheets (parametric qmd)  
# 260429  - rewrite for the final submitted data structures 
#         - create merged output pdf

library(quarto)
library(tidyverse)
library(glue)
library(sf)
library(fs)
library(qpdf)
# library(gt)

ninaServer <- F
pdrive <- if(ninaServer) "~/Mounts/P-Prosjekter2/112549_bern_2025" else "P:/112549_bern_2025"
# ahome <-  "assessment" %>% path(pdrive, .) # working dir for the experts
# sthome <- "data/st_all" %>% path(pdrive, .) # a repo folder for the "full" contents of all "simple templates" (st)
# jsonhome <- "../Bern25-harvest/out/" # the home of the json files harvested from the simple templates (...does not work on NINA server!!!)
logdir <- "data/xlsx_log" %>% path(pdrive, .) # a repo folder for the "full" contents of all "simple templates" (st)
tpldir0 <- "data/_templates" %>% path(pdrive, .) # working dir for templates (for e.g. reflov names, etc.)
gishome <- "data/gis" %>% path(pdrive, .) # diverse input gis data
maphome <- "data/maps" %>% path(pdrive, .) # output maps  
outhome <- "data/_output" %>% path(pdrive, .) # (~final) output files 
fashdir <- format(Sys.time(), "%y%m%d") %>% path("output/factsheets", .) #folder for the factsheets 
dir_create(fashdir)

f_templ <- "qmd/fash_templ.qmd"
f_mrg0  <- format(Sys.time(), "%y%m%d") %>%  # merged pdf factsheet (for report annex)
  str_c("fash_merged-", ., ".pdf") %>% path("output/factsheets", .) 
f_mrg1 <- "fash_merged-newest.pdf" %>% path("output/factsheets", .) 


# debug <- F # extra diagnostics #--> not implemented

### extra typologies
fgrps_en <- c(ma= "Mammals", fa= "Fishes & amphibians", im= "Insects and molluscs",
              vp= "Vascular plants", mo= "Mosses", ht= "Habitat types", all="All")

### helpers
h_lkp <- function(x, tab) { # generic lookup (x: any vec; tab: a df or mat w cols 1/2 containing lkp keys/values)
  tab[[2]][match(as.character(x), as.character(tab[[1]]))]
  } 
h_cap1st <- function(x) x %>% #capitalise just the first letters of a chr vector (all subsequent letters kept)
  str_sub(1,1) %>% toupper %>% str_c(str_sub(x,2)) 
h_oie <- function(x, y=NULL) { #"only if exists": if x=="" or NA or NULL then return y (=NULL) else return x[1]
  if (is.null(x) || is.na(x[1]) || x[1]=="") y else x[1]
  }
h_ie  <- function(x) !is.null(h_oie(x))   # is existing? 


###
### READ data
###

dat2 <- read_rds(path(outhome, "xt_posthr2-newest.rds"))    # the final assessment outcomes 
chklw <- read_rds(path(pdrive,"data/chklist-newest.rds"))   # only needed for the species groups
track1  <- read_csv(path(logdir, "track_progress-newest.csv"), col_types=cols(.default = "c")) #needed for expert full names & xlsx completion times
pmcodes <- read_csv(path(tpldir0, "pres_meas_codes-newest.csv"))  # descriptive names for the pres & meas categories
maphulls <- "sp_range_hulls-newest.rds" %>% path(maphome, .) %>% read_rds # the range polygons (for the map plots)

no_land <- read_rds(path(gishome, "Norge_kyst_simplified.rds")) %>% #simple boundaries for plotting
  st_transform(3035)
gr10_3035 <- read_rds(path(gishome, "grid50_kyst_bgr.rds")) %>% #a polygon version of the EEA grid
  select(-ends_with("Origin")) %>%
  st_transform(3035)
pal_bgr <- colorspace::qualitative_hcl(4, palette = "Dark 3") %>%
  set_names(gr10_3035$bgr %>% unique %>% sort)


###
### Compile output structures
###
#
# pp: a "params" list with a single top-level element  
# ..$data: a structured list
#   ..$[ftid]: structured list 
#     ..$titl: chr[1]
#     ..$head: tibble for gt()
#     ..$body: repeat list //BGRs
#       ..$[BGR1]: semi-structured list with zero or one of the following components:
#         ..$Occ<oblig>, Ran, Pop, Are, H4S, SnF, Fpr: semi structured lists consisting of:
#           ..$status: FV/U1/U2/XX (or for Occ: PRE/MAR/SCR/...)
#           ..$trend:  I/S/D/U (shd always be present, except for Occ & Fpr)
#           ..$..: any number of further elements, each of which will be bullet points (exact text)
#     ..$tail: structured list 
#       ..$pres: chr() with each element structured as "CODE: Pressure name (BGRs)"
#       ..$meas: chr() with each element structured as "CODE: Measure name (BGRs)"
#       ..$srcs: chr() with sources (one by one)


# ii="1927"; jj="ALP"
for (ii in dat2$sp$ftid) { #[(1:10)*7]) {
  if (ii %in% c("1083","1903","1927","1130","1086")) next # skipping the "unassessed" species (which would otherwise produce extremely data-poor templates) 
  d0 <- dat2 %>% pluck("sp") %>% filter(ftid==ii) %>% # feature-level tables (was: cw)
    mutate(ft_name0= dat2$spr %>% filter(ftid==ii) %>% pluck("species_code_label",1)) %>% # misplaced field (should be in sp not spr)
    mutate(fgrp=   chklw  %>% filter(ftid==ii) %>% pluck("fgrp")) %>%
    mutate(expert= track1 %>% filter(ftid==ii) %>% pluck("expert",1)) %>%
    mutate(done=   track1 %>% filter(ftid==ii) %>% pluck("done") %>% (\(x) if(length(x)) max(x) else NA)) #to get rid of warning for species without full report (1083, 1903, etc.)
  dr  <- dat2 %>% pluck("spr")  %>% filter(ftid==ii) #region-level tables (including the final SPC checklist table...)
  drp <- dat2 %>% pluck("sprp") %>% filter(ftid==ii) #region-pressure-level; empty for spp w/o full report
  drm <- dat2 %>% pluck("sprm") %>% filter(ftid==ii) #region-measure-level; empty for spp w/o full report
  mg1 <- dat2 %>% pluck("spg")  %>% filter(species_code==ii) %>%  #gridcell-level; empty for spp w/o full report
    rename(CellCode=gridcell_code) %>% left_join(gr10_3035, by= join_by(CellCode)) %>% st_as_sf
  mh1 <- maphulls %>% filter(ftid==ii) #range tool graphical outputs; empty for spp w/o map (i.e. full record...)
  
  f_tmp <- file_temp(ext = ".rds") # temp file to save the ggplot
  f_out <- str_c(d0$ftid,"_",d0$ft_name1,".pdf") #%>% path(fashdir, .)
  
  ptitl <- glue("{d0$species_code}: {d0$ft_name0}")
  phead <- c(`Relevant synonyms` = d0$alternative_name %>% na_if(""),  # no such names for habitats: this & the next line are dropped 
             `Norwegian name`= d0$common_name %>% na_if(""), #%>% h_cap1st,  
             `Feature group`= fgrps_en[d0$fgrp] %>% unname,
             `Expert(s)` = d0$expert %>% h_oie("---"), Date = d0$done %>% h_oie("---")
             )  %>% replace_na("---") %>% enframe
  pbody <- NULL
  for (jj in dr$bgr) { # this also works for checklist items with part/none reporting 
    d1 <- dr %>% filter(bgr==jj) 
    p1 <- list(Occ= list(status= d1$occurrence_code, d1$comments %>% h_oie), 
               OvC= list(status= d1$S_11_5_conclusion_overall_assessment %>% h_oie, trend= d1$S_11_6_conclusion_overall_trend %>% h_oie),
               Ran= list(status= d1$S_11_1_conclusion_range %>% h_oie, trend= d1$S_5_4_short_term_trend_direction %>% h_oie),
               Pop= list(status= d1$S_11_2_conclusion_population %>% h_oie, trend= d1$S_6_10_short_term_trend_direction %>% h_oie),
               H4s= list(status= d1$S_11_3_conclusion_species_habitat %>% h_oie, trend= d1$S_7_4_short_term_trend_direction %>% h_oie),
               Fpr= list(status= d1$S_11_4_conclusion_future_prospects %>% h_oie)) %>%
      discard(\(x) x %>% unlist() %>% is.null) # drop all of the above that is empty
      # p1 %>% map(unlist)
    pbody <- list(p1) %>% set_names(jj) %>% c(pbody, .)
    }
  ptail <- NULL
  ptail$pres <- drp %>% mutate(s1= S_8_1_a_pressure_code) %>%
    group_by(s1) %>% summarise(bgrs= str_c(bgr, collapse=", ")) %>%
    mutate(label= str_c(s1,": ",h_lkp(s1, pmcodes)," [",bgrs,"]")) %>%
    pluck("label") %>% as.list
  ptail$meas <- drm %>% mutate(s1= S_9_6_measure_code) %>%
    group_by(s1) %>% summarise(bgrs= str_c(region_code, collapse=", ")) %>%
    mutate(label= str_c(s1,": ",h_lkp(s1, pmcodes)," [",bgrs,"]")) %>%
    pluck("label") %>% as.list
  ptail$srcs <- dr %>% pluck("S_4_4_source", 1) %>% #just from the 1st tab!
    str_split_1(fixed(";  ")) %>% as.list 
  pp <- list(data=list(titl= ptitl, head=phead, body=pbody, tail=ptail)) # parameters list
  
  if (nrow(mg1) > 0) { # produce a distribution map plot & add to the end of the param list
    tmp <- str_c(d0$ftid," ",d0$ft_name0) %>% # plot title 
      str_c(if (d0$S_2_4_distribution_method == "estimatePartial") "\n  (low accuracy)" else "") #la marking (B1b="mostly_inaccurate"), if needed
    gg1 <- ggplot() +
      geom_sf(data= no_land) +
      geom_sf(data= mg1, aes(fill=bgr)) +
      geom_sf(data= mh1, fill=adjustcolor("yellow", alpha.f = 0.1), 
              color=adjustcolor("yellow", alpha.f = 0.5), aes(linewidth="range")) +
      scale_fill_manual(values= pal_bgr, name= "Biogeographic \n regions:") +
      scale_linewidth_manual(values= c(range=1), name= "Range hull:") +
      labs(title= NULL, y=NULL, x= d0$S_2_6_additional_information %>% str_wrap(width= 95)) + # ~caption
      geom_text(aes(x=4000000, y=5450000), label= tmp, size=4, hjust= 0, vjust= .5) +
      theme_bw() +
      theme(legend.position= "inside", legend.position.inside= c(0.75, 0.3), 
            axis.title.x= element_text(size= 10))
    # gg1
    gg1 %>% write_rds(f_tmp)
    pp <- c(pp, plotfile=f_tmp)
    }

  message(paste("Rendering:", f_out))
  quarto_render(f_templ, "typst", f_out, execute_params= pp, quiet=TRUE)
  file_copy(path("output/qmd", f_out), fashdir, overwrite=T)
  }
  

# ii="G1A4"; jj="BOR"
for (ii in dat2$ha$ftid) { 
  d0 <- dat2 %>% pluck("ha") %>% filter(ftid==ii) %>% # feature-level tables (was: cw)
    mutate(ft_name0= dat2$har %>% filter(ftid==ii) %>% pluck("habitat_code_label",1)) %>% # misplaced field (should be in sp not spr)
    mutate(fgrp=   chklw  %>% filter(ftid==ii) %>% pluck("fgrp")) %>% # "habitat type"
    mutate(expert= track1 %>% filter(ftid==ii) %>% pluck("expert",1)) %>%
    mutate(done=   track1 %>% filter(ftid==ii) %>% pluck("done") %>% (\(x) if(length(x)) max(x) else NA)) #to get rid of warning for species without full report (1083, 1903, etc.)
  dr  <- dat2 %>% pluck("har")  %>% filter(ftid==ii) #region-level tables (including the final SPC checklist table...)
  drp <- dat2 %>% pluck("harp") %>% filter(ftid==ii) #region-pressure-level; empty for spp w/o full report
  drm <- dat2 %>% pluck("harm") %>% filter(ftid==ii) #region-measure-level; empty for spp w/o full report
  mg1 <- dat2 %>% pluck("hag")  %>% filter(str_replace(habitat_code, fixed("."),"")==ii) %>%  #gridcell-level; empty for spp w/o full report
    rename(CellCode=gridcell_code) %>% left_join(gr10_3035, by= join_by(CellCode)) %>% st_as_sf
  mh1 <- maphulls %>% filter(ftid==ii) #range tool graphical outputs; empty for spp w/o map (i.e. full record...)
  
  f_tmp <- file_temp(ext = ".rds") # temp file to save the ggplot
  f_out <- str_c(d0$ftid,"_",d0$ft_name1,".pdf") #%>% path(fashdir, .)
  
  ptitl <- glue("{d0$habitat_code}: {d0$ft_name0}")
  phead <- c(`Feature group`= fgrps_en[d0$fgrp] %>% unname, 
             `Expert(s)`= d0$expert %>% h_oie("---"), 
              Date= d0$done%>% h_oie("---"))  %>% enframe
  pbody <- NULL
  for (jj in dr$bgr) { # this also works for checklist items with part/none reporting 
    d1 <- dr %>% filter(bgr==jj) 
    p1 <- list(Occ= list(status= d1$occurrence_code, d1$comments %>% h_oie), 
               OvC= list(status= d1$H_10_5_conclusion_overall_assessment %>% h_oie, trend= d1$H_10_6_conclusion_overall_trend %>% h_oie),
               Ran= list(status= d1$H_10_1_conclusion_range %>% h_oie, trend= d1$H_4_4_short_term_trend_direction %>% h_oie),
               Are= list(status= d1$H_10_2_conclusion_area %>% h_oie, trend= d1$H_5_7_short_term_trend_direction %>% h_oie),
               SnF= list(status= d1$H_10_3_conclusion_structure_and_function %>% h_oie, trend= d1$H_6_4_condition_good_short_term_trend_direction %>% h_oie),
               Fpr= list(status= d1$H_10_4_conclusion_future_prospects %>% h_oie)) %>%
      discard(\(x) x %>% unlist() %>% is.null) # drop all of the above that is empty
    # p1 %>% map(unlist)
    pbody <- list(p1) %>% set_names(jj) %>% c(pbody, .)
    }
  ptail <- NULL
  ptail$pres <- drp %>% mutate(s1= H_7_1_a_pressure_code) %>%
    group_by(s1) %>% summarise(bgrs= str_c(bgr, collapse=", ")) %>%
    mutate(label= str_c(s1,": ",h_lkp(s1, pmcodes)," [",bgrs,"]")) %>%
    pluck("label") %>% as.list
  ptail$meas <- drm %>% mutate(s1= H_8_6_measure_code) %>%
    group_by(s1) %>% summarise(bgrs= str_c(region_code, collapse=", ")) %>%
    mutate(label= str_c(s1,": ",h_lkp(s1, pmcodes)," [",bgrs,"]")) %>%
    pluck("label") %>% as.list
  ptail$srcs <- dr %>% pluck("H_3_4_sources", 1) %>% #just from the 1st tab!
    str_split_1(fixed(";  ")) %>% as.list 
  pp <- list(data=list(titl= ptitl, head=phead, body=pbody, tail=ptail)) # parameters list
  
  if (nrow(mg1) > 0) { # produce a distribution map plot & add to the end of the param list
    tmp <- str_c(d0$ftid," ",d0$ft_name0) %>% # plot title 
      str_c(if (d0$H_2_3_distribution_method == "estimatePartial") "\n  (low accuracy)" else "") #la marking (B1b="mostly_inaccurate"), if needed
    gg1 <- ggplot() +
      geom_sf(data= no_land) +
      geom_sf(data= mg1, aes(fill=bgr)) +
      geom_sf(data= mh1, fill=adjustcolor("yellow", alpha.f = 0.1), 
              color=adjustcolor("yellow", alpha.f = 0.5), aes(linewidth="range")) +
      scale_fill_manual(values= pal_bgr, name= "Biogeographic \n regions:") +
      scale_linewidth_manual(values= c(range=1), name= "Range hull:") +
      labs(title= NULL, y=NULL, x= d0$H_2_5_additional_information_maps %>% str_wrap(width= 100)) + # ~caption
      geom_text(aes(x=4000000, y=5450000), label= tmp, size=4, hjust= 0, vjust= .5) +
      theme_bw() +
      theme(legend.position= "inside", legend.position.inside= c(0.75, 0.3), 
            axis.title.x= element_text(size= 10))
    # gg1
    gg1 %>% write_rds(f_tmp)
    pp <- c(pp, plotfile=f_tmp)
    }
  
  message(paste("Rendering:", f_out))
  quarto_render(f_templ, "typst", f_out, execute_params= pp, quiet=TRUE)
  file_copy(path("output/qmd", f_out), fashdir, overwrite=T)
  }

###
### stitch the factsheets together into a single pdf ( --> annex to the final report)
###

fff <- dir_ls(fashdir) %>%
  tibble(fpath= ., fname=path_file(.)) %>%
  mutate(ftid= str_split_i(fname, "_", 1)) %>%
  mutate(fgrp= h_lkp(ftid, select(chklw, ftid, fgrp)) %>% fct(names(fgrps_en))) %>%
  filter(!is.na(fgrp)) %>%
  arrange(fgrp, ftid) 

# Create new merged pdf
pdf_combine(input= pluck(fff, "fpath"), output= f_mrg0)
file_copy(f_mrg0, f_mrg1, overwrite=T)



