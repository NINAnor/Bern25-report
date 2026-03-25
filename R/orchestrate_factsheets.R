### 
###  Orchestrating script for the production of species and habitat fact sheets for the Bern 25 delivery
###   
###  project: Bern_2025
###  Balint Czucz, NINA, 2026
###
###########################################

### Changelog:
# 260320  - initial version: feature factsheets (parametric qmd)  

library(quarto)
library(tidyverse)
library(glue)
library(sf)
library(fs)
# library(gt)

ninaServer <- F
pdrive <- if(ninaServer) "~/Mounts/P-Prosjekter2/112549_bern_2025" else "P:/112549_bern_2025"
# ahome <-  "assessment" %>% path(pdrive, .) # working dir for the experts
tpldir0 <- "data/_templates" %>% path(pdrive, .) # working dir for templates
# sthome <- "data/st_all" %>% path(pdrive, .) # a repo folder for the "full" contents of all "simple templates" (st)
jsonhome <- "../Bern25-harvest/out/" # the home of the json files harvested from the simple templates (...does not work on NINA server!!!)
gishome <- "data/gis" %>% path(pdrive, .) # diverse input gis data
maphome <- "data/maps" %>% path(pdrive, .) # output maps  
logdir <- "data/xlsx_log" %>% path(pdrive, .) # a repo folder for the "full" contents of all "simple templates" (st)
fashdir <- format(Sys.time(), "%y%m%d") %>% path("output/factsheets", .) #folder for the factsheets 

dir_create(fashdir)
f_templ <- "qmd/fash_templ.qmd"
# debug <- F # extra diagnostics

### extra typologies
fgrps_en <- c(ma= "Mammals", fa= "Fishes & amphibians", im= "Insects and molluscs",
              vp= "Vascular plants", mo= "Mosses", ht= "Habitat types", all="All")
ook <- c("PRE", "EXa", "TAX", "SCR") # the occurrence codes requiring a full survey (plus SCR2)

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

### the checklist, xt reflovs and st lookups
chklw <- read_rds(path(pdrive,"data/chklist-newest.rds"))   #wide
chkll <- read_rds(path(pdrive,"data/chkl_long-newest.rds")) #long
reflovs <- read_csv(path(tpldir0, "xt_reflovs-260126.csv")) %>% rename(id= rd_id)
stcodes <- read_csv(path(tpldir0, "st_codelists-newest.csv"))
pmcodes <- read_csv(path(tpldir0, "pres_meas_codes-newest.csv"))
track1  <- read_csv(path(logdir, "track_progress-newest.csv"), col_types=cols(.default = "c")) #xlsx modif times
 


### lists of all extracted fields from the last versions of the two xlsx templates
tmp <- read_rds(path(tpldir0, "t_struc.rds")) # t_struc
fds <- NULL # ~fields
fds$sp <- tmp %>% keep_at(\(i) str_detect(i,"^sp")) %>% pluck(-1) %>% select(-icell) 
fds$ha <- tmp %>% keep_at(\(i) str_detect(i,"^ha")) %>% pluck(-1) %>% select(-icell) 

### the json files "harvested" from the simple templates
tmp <- chklw %>%
  with(str_c("b25_", exp0,"_", ftid, ".json")) %>% 
  path(jsonhome, .) %>%
  map(possibly(\(x) jsonlite::read_json(here::here(x)), otherwise=NULL)) %>%
  discard(is.null)

tmp1 <- tibble(ftid=  map_chr(tmp, \(x) pluck(x, 1, "ftid"))) %>%
  # mutate(ft_name0=   map_chr(tmp, \(x) pluck(x, 1, "ft_name0"))) %>%
  mutate(t_ver=   map_chr(tmp, \(x) pluck(x, 1, "t_ver"))) %>%
  mutate(tabs=    map(tmp, \(x) pluck(x, 1, "tabs"))) %>%
  mutate(tabs= map(tabs, \(x) list_transpose(x, default=NA))) %>%
  mutate(tabs= map(tabs, as_tibble)) 

### two full "data cubes" (for sp & ha separately) 
dat <- NULL # the two "data cubes" -- with everything (except for ranges, etc.)
dat$sp <- tmp1 %>% filter(str_detect(t_ver, "^sp")) %>% unnest(tabs) %>%
  bind_rows(set_names(fds$sp$f_id) %>% as.list %>% as_tibble %>% slice(-1), .) %>% #fix col order & explicit NAs
  # left_join(chkll, by=join_by(ftid, bgr)) %>%
  # left_join(select(chklw, -any_of(names(select(chkll, -ftid)))), by=join_by(ftid)) %>%
  # mutate(fgrp= fct(fgrp, names(fgrps_en))) %>%
  # relocate(ftid, bgr, fgrp, exp0, occ1, ft_name0, status)
  relocate(ftid, bgr)

dat$ha <- tmp1 %>% filter(str_detect(t_ver, "^ha")) %>% unnest(tabs) %>%
  bind_rows(set_names(fds$ha$f_id) %>% as.list %>% as_tibble %>% slice(-1), .) %>% #fix col order & explicit NAs
  # left_join(chkll, by=join_by(ftid, bgr)) %>%
  # left_join(select(chklw, -any_of(names(select(chkll, -ftid)))), by=join_by(ftid)) %>%
  # mutate(fgrp= fct(fgrp, names(fgrps_en))) %>%
  # relocate(ftid, bgr, fgrp, exp0, occ1, ft_name0, status)
  relocate(ftid, bgr)

### spatial data
mapstats <- "sp_mapstats-newest.rds" %>% path(maphome, .) %>% read_rds 
mapgrids <- "sp_mapgrids-newest.rds" %>% path(maphome, .) %>% read_rds 
maphulls <- "sp_range_hulls-newest.rds" %>% path(maphome, .) %>% read_rds 

no_land <- read_rds(path(gishome, "Norge_kyst_simplified.rds")) %>% #simple boundaries for plotting
  st_transform(3035)
gr10_3035 <- read_rds(path(gishome, "grid50_kyst_bgr.rds")) %>% #a polygon version of the EEA grid
  select(-ends_with("Origin")) %>%
  st_transform(3035)
# bgrs_3035 <- readRDS(path(gishome, "Norge_BGR_buff.rds")) %>% #BGR with marine buffer for exact.extract
#   arrange(bgr) %>% st_transform(3035) #transformed version 
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


# ii="D41"; jj="BOR"
for (ii in chklw$ftid) { #[(1:10)*7]) {
  cw <- chklw %>% filter(ftid==ii)
  if (cw$bgr_i2025_final=="") next # 1083 Lcervus & 1903 L loeselei kiejtese
  d0 <- dat %>% pluck(cw$ftt) %>% filter(ftid==ii)
  mg1 <- mapgrids %>% filter(ftid==ii) %>%
    mutate(geometry= h_lkp(CellCode, select(gr10_3035, CellCode, geometry))) %>%
    st_as_sf
  mh1 <- maphulls %>% filter(ftid==ii)
  ptitl <- glue("{cw$b_id}: {cw$ft_name0}")
  phead <- c(`Relevant synonyms` = d0$F1a[1],          # no F1a-F1b for habitats: this & the next line are dropped 
             `Norwegian name`= d0$F1b[1] %>% h_cap1st,  
             `Feature group`= fgrps_en[cw$fgrp] %>% unname,
             `Expert(s)` = d0$expert[1],
              Date = track1 %>% filter(ftid==ii) %>% {c("",.$done)} %>% max
             ) %>% replace_na("---") %>% enframe
  pbody <- NULL
  for (jj in d0$bgr) {
    dd <- d0 %>% filter(bgr==jj) 
    cl <- chkll %>% filter(ftid==ii, bgr==jj) 
    ms1  <- mapstats %>% filter(ftid==ii, bgr==jj) 
    oo <- cl$occ1 %>% str_sub(1,3) #occurrence code
    p1 <- NULL
    # p1$titl <- stcodes %>% filter(id=="n_BGR") %>% select(sc,sl) %>% deframe %>% {.[jj]} %>% str_c(" (",jj,")")
   #Occ:
    ll <- list(status=oo)
    ll <- c(ll, h_oie(cl$comment))
    p1$Occ <- ll 
   #OvC:
    ll <- list(status= dd$E1a.0 %>% h_oie, trend= dd$E2a.0 %>% str_sub(1,1) %>% h_oie)
    # ll <- ll %>% c(h_oie(dd$E3a)) 
    # ll <- ll %>% c(h_oie(dd$F5a))
    if (oo %in% ook) p1$OvC <- ll 
   #Ran:
    ll <- list(status= dd$B7a %>% h_oie, trend= dd$B3a %>% str_sub(1,1) %>% h_oie)
    # if (h_ie(tt <- mms1$range)) ll <- c(ll, glue("Current range: {tt} km^2^"))
    # if (h_ie(tt <- dd$B5a) && dd$B5a!="unk") 
    #   ll <- stcodes %>% filter(id=="rd_FRR_Predefined") %>% select(sc,sl) %>% deframe %>% {.[tt]} %>% c(ll, .)
    # ll <- ll %>% c(h_oie(dd$B6a)) 
    # ll <- ll %>% c(h_oie(dd$B7b))
    if (oo %in% ook) p1$Ran <- ll 
   #Pop:
    if (oo %in% ook && cw$ftt=="sp") p1$Pop <- list(status= dd$A8a %>% h_oie, trend= dd$A4a %>% str_sub(1,1) %>% h_oie)
      # if (h_ie(dd$A2c)) tmp <- 
      #     c(dd$A2c, dd$A2c.2) %>% na_if("") %>% na.omit %>% str_c(collapse="--") %>%
      #     str_c(" [",dd$A1b,"]") %>% glue("Current population: {.}")
      #  ...etc (not finished, not enough time)
   #Are:
    if (oo %in% ook && cw$ftt=="ha") p1$Are <- list(status= dd$A6a %>% h_oie, trend= dd$A2a %>% str_sub(1,1) %>% h_oie)
   #H4s:
    if (oo %in% ook && cw$ftt=="sp") p1$H4s <- list(status= dd$C5a %>% h_oie, trend= dd$C3a %>% str_sub(1,1) %>% h_oie)
   #SnF:
    if (oo %in% ook && cw$ftt=="ha") p1$SnF <- list(status= dd$C5a.0 %>% h_oie, trend= dd$C3a %>% str_sub(1,1) %>% h_oie)
   #Fpr: 
    if (oo %in% ook) p1$Fpr <- list(status= dd$D4d.0 %>% h_oie)
    
    pbody <- list(p1) %>% set_names(jj) %>% c(pbody, .)
    }
  ptail <- NULL
  ptail$pres <- d0 %>% 
    select(bgr, starts_with("D1a"), D1b01, D1c) %>%  #D1b01, D1c: to bring back the IAS 
    mutate(D1a21= if_else(is.na(D1b01),NA,"PI01"), D1a21.2=if_else(is.na(D1b01),"","pres"),
           D1a22= if_else(is.na(D1c),NA,"PI02"), D1a22.2= if_else(is.na(D1c),"","pres")) %>% 
    select(-D1b01,-D1c) %>% #TODO: next time keep these codes in the list :(
    pivot_longer(starts_with("D1a")) %>%
    separate_wider_delim(name, ".", names=c("n_tmp", "subcol"), too_few="align_start") %>%
    mutate(subcol= subcol %>% replace_na("1") %>% str_c("s",.)) %>%
    pivot_wider(names_from=subcol, values_from=value) %>%
    filter(!is.na(s1)) %>%
    # filter(str_detect(s2,"pres")) %>% #to filter out the past & future threats
    group_by(s1) %>% summarise(bgrs= str_c(bgr, collapse=", ")) %>%
    mutate(label= str_c(s1,": ",h_lkp(s1, pmcodes)," [",bgrs,"]")) %>%
    {.$label} %>% as.list
  ptail$meas <- d0 %>% 
    select(bgr, starts_with("D2c")) %>%   
    pivot_longer(starts_with("D2c")) %>%
    separate_wider_delim(name, ".", names=c("n_tmp", "subcol"), too_few="align_start") %>%
    mutate(subcol= subcol %>% replace_na("1") %>% str_c("s",.)) %>%
    pivot_wider(names_from=subcol, values_from=value) %>%
    filter(!is.na(s1)) %>%
    group_by(s1) %>% summarise(bgrs= str_c(bgr, collapse=", ")) %>%
    mutate(label= str_c(s1,": ",h_lkp(s1, pmcodes)," [",bgrs,"]")) %>%
    {.$label} %>% as.list
  ptail$srcs <- d0 %>% slice(1) %>% #just from the 1st tab!
    select(starts_with(c(sp="F4a", ha="F2a")[cw$ftt])) %>%   
    pivot_longer(everything()) %>%
    filter(!is.na(value)) %>%
    {.$value} %>% as.list
  
  tmp <- str_c(cw$ftid," ",cw$ft_name0) %>% # plot title 
    str_c(if (ms1$map_la == "la") "\n  (low accuracy)" else "") #la marking (B1b="mostly_inaccurate"), if needed
  f_tmp <- file_temp(ext = ".rds") # temp file to save the ggplot
  f_out <- str_c(cw$ftid,"_",cw$ft_name1,".pdf") #%>% path(fashdir, .)
  pp <- list(data=list(titl= ptitl, head=phead, body=pbody, tail=ptail))
  
  gg1 <- ggplot() +
    geom_sf(data= no_land) +
    geom_sf(data= mg1, aes(fill=bgr)) +
    geom_sf(data= mh1, fill=adjustcolor("yellow", alpha.f = 0.1), 
            color=adjustcolor("yellow", alpha.f = 0.5), aes(linewidth="range")) +
    scale_fill_manual(values= pal_bgr, name= "Biogeographic \n regions:") +
    scale_linewidth_manual(values= c(range=1), name= "Range hull:") +
    labs(title= NULL, y=NULL, x= ms1$map_comment_out %>% str_wrap(width= 100)) + # ~caption
    geom_text(aes(x=4000000, y=5450000), label= tmp, size=4, hjust= 0, vjust= .5) +
    theme_bw() +
    theme(legend.position= "inside", legend.position.inside= c(0.75, 0.3), 
          axis.title.x= element_text(size= 10))
  # gg1
  gg1 %>% write_rds(f_tmp)
  pp <- c(pp, plotfile=f_tmp)

  message(paste("Rendering:", f_out))
  quarto_render(f_templ, "typst", f_out, execute_params= pp, quiet=TRUE)
  file_copy(path("output/qmd", f_out), fashdir, overwrite=T)
  }
  


dir_create(fashdir)
f_templ <- "qmd/fash_templ.qmd"
# debug <- F # extra diagnostics


