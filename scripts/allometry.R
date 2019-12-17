########################
## Mangrove allometry ##
########################

## A helper script that generates functions to apply allometric equations
## to diameter measurements. A library of species-specific allometric 
## equations is given at the bottom of this script.

## The Sections of this script include:
##   Section 1: Specify a library of mangrove allometric equations
##   Section 2: Generate a look up table for allometric equations


##-----------------------------------------------------------------------------------
## Section 1 - Mangrove allometry library

# Allometric equation functions, labelled by <species_code>_<aboveground/belowground>_<authoryear>

# Example: total aboveground biomass equation for Rhizophora apiculata by Ong et al., 2004:
#   - rhap_ag_ong2004

# General functions, Komiyama et al., 2005, function of diameter at breast height and wood density

general_ag_komiyama2005 <- function(dbh, dens) { 0.251 * dens * (dbh ^ 2.46) }
general_bg_komiyama2005 <- function(dbh, dens) { 0.199 * (dens ^ 0.899) * (dbh ^ 2.22) }

# Aegiceras corniculatum, Tam et al., 1995 (China)

aeco_ag_tam1995 <- function(dbh, dens) {exp(1)^( 1.496 + 0.465 * log(dbh ^ 2 * hgt) )}
aeco_bg_tam1995 <- function(dbh, dens) {exp(1)^( 0.967 + 0.303 * log(dbh ^ 2 * hgt) )}

# Avicennia marina, Comley et al., 2005 (Australia)

avma_ag_comley2005 <- function(dbh, dens) { 10^(-0.511 + 2.113 * log10(dbh)) }
avma_ag_clough1997 <- function(dbh, dens) { 10^(-0.7506 + 2.299 * log10(dbh)) }

avma_bg_comley2005 <- function(dbh, dens) { 10^(0.106 + 1.171 * log10(dbh)) }

# Bruguiera gymnorrhiza, Clough & Scott, 1989

brgy_ag_clough1989 <- function(dbh, dens) { 10^(-0.7309 + 2.3055 * log10(dbh)) }

# Bruguiera parviflora, Clough & Scott, 1989

brpa_ag_clough1989 <- function(dbh, dens) { 10^(-0.7749 + 2.4167 * log10(dbh)) }

# Excoecaria agallocha, Hossain et al., 2015

exag_ag_hossain2015 <- function(dbh, dens) {exp(1)^(1.0996 * (log(dbh^2)) - 0.8572)}

# Lumnitzera racemosa, Kangkuso et al., 2015

lura_ag_kangkuso2015 <- function(dbh, dens) {1.184 * dbh^2.384}

# Rhizophora apiculata, Ong et al., 2004 (Malaysia)

rhap_ag_ong2004 <- function(dbh, dens) {
  if(dbh >= 15) { 
    exp(1)^(2.420 * log(dbh) - 1.832) 
  } else { 
    exp(1)^(2.318 * log(dbh) - 1.671) 
  }
}

rhap_bg_ong2004 <- function(dbh, dens) {
  ## multiple equations for total belowground biomass dependent upon DBH
  
  if(dbh >= 15) {
    exp(1)^(2.611 * log(dbh) - 3.454)
  } else {
    exp(1)^(1.522 * log(dbh) - 1.707)
  }
  
}

rhap_prop_ong2004 <- function(dbh, dens) {
  
  if(dbh >= 15) {
    exp(1)^(2.546 * log(dbh) - 2.94)
  }
  
}

# Rhizophora stylosa, Comley et al., 2005 (Australia)

rhst_ag_comley2005 <- function(dbh, dens) { 
  
  10^(-0.696 + 2.465 * log10(dbh)) 
  
}

rhst_bg_comley2005 <- function(dbh, dens) {
  10^(-0.583 + 1.860 * log10(dbh)) 
}


# Xylocarpus granatum, Clough and Scott, 1989; Poungparn, 2002

xygr_ag_clough1989 <- function(dbh, dens) { 10^(-1.0844 + 2.5883 * log10(dbh))}
xygr_ag_tarlan2008 <- function(dbh, dens) { 0.1832 * (dbh ^ 2.21)}    # max dbh = 41.0

xygr_bg_poungparn2002 <- function(dbh, dens) { 0.145 * dbh ^ 2.55 }


##-----------------------------------------------------------
## Section 2 - Generate a look up table for the allometric equations

library(tidyverse)

allom_lookup <- tibble(genus = character(), 
                       species = character(), 
                       sps_code = character(),
                       density = double(),
                       ag_form = list(), 
                       bg_form = list(), 
                       ag_ref = character(),
                       bg_ref = character()
                       )

allom_lookup <- allom_lookup %>%
  add_row(genus = "acanthus", species = "ebracteatus", sps_code = "aceb", density = NA,
          ag_form = c(NA), 
          bg_form = c(NA), 
          ag_ref = NA, bg_ref = NA) %>%
  add_row(genus = "aegiceras", species = "corniculatum", sps_code = "aeco", density = NA,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "avicennia", species = "alba", sps_code = "aval", density = 0.587, 
          ag_form = c(avma_ag_comley2005), 
          bg_form = c(avma_bg_comley2005), 
          ag_ref = "comley2005", bg_ref = "comley2005") %>%
  add_row(genus = "avicennia", species = "marina", sps_code = "avma", density = 0.65,
          ag_form = c(avma_ag_comley2005), 
          bg_form = c(avma_bg_comley2005), 
          ag_ref = "comley2005", bg_ref = "comley2005") %>%
  add_row(genus = "avicennia", species = "officinalis", sps_code = "avof", density = 0.605,
          ag_form = c(avma_ag_comley2005), 
          bg_form = c(avma_bg_comley2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "bruguiera", species = "cylindrica", sps_code = "brcy", density = 0.720,
          ag_form = c(brgy_ag_clough1989), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "bruguiera", species = "gymnorrhiza", sps_code = "brgy", density = 0.710,
          ag_form = c(brgy_ag_clough1989), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "bruguiera", species = "parviflora", sps_code = "brpa", density = 0.760,
          ag_form = c(brpa_ag_clough1989), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "clough1989", bg_ref = "komiyama2005") %>%
  add_row(genus = "bruguiera", species = "sexangula", sps_code = "brse", density = 0.740,
          ag_form = c(brpa_ag_clough1989), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "calamus", species = "erinaceus", sps_code = "caer", density = NA,
          ag_form = c(function(dbh, dens) { dbh * 2.8929 }), 
          bg_form = c(function(dbh, dens) { dbh * 0 }), 
          ag_ref = "matsui2014", bg_ref = "matsui2014") %>%
  add_row(genus = "ceriops", species = "tagal", sps_code = "ceta", density = 0.780,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "cynometra", species = "ramiflora", sps_code = "cyra", density = 0.787,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "diospyrus", species = "maritima", sps_code = "dima", density = 0.560,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "excoecaria", species = "agallocha", sps_code = "exag", density = 0.416,
          ag_form = c(exag_ag_hossain2015), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "hossain2015", bg_ref = "komiyama2005") %>%
  add_row(genus = "heritiera", species = "littoralis", sps_code = "heli", density = 0.848,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "heritiera", species = "sylvatica", sps_code = "hesy", density = 0.700,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "intsia", species = "bijuga", sps_code = "inbi", density = 0.720,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "kandelia", species = "obovata", sps_code = "kaob", density = 0.525,
          ag_form = c(function(dbh, dens){0.251*0.525*(dbh^2.46)}), 
          bg_form = c(general_bg_komiyama2005),
          ag_ref = "khan????", bg_ref = "komiyama2005") %>%
  add_row(genus = "lumnitzera", species = "littorea", sps_code = "luli", density = 0.670,
          ag_form = c(function(dbh, dens){1.184+(dbh)^2.384}), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "kangkuso2015", bg_ref = "komiyama2005") %>%
  add_row(genus = "lumnitzera", species = "racemosa", sps_code = "lura", density = 0.710,
          ag_form = c(lura_ag_kangkuso2015), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "kangkuso2015", bg_ref = "komiyama2005") %>%
  add_row(genus = "maniltoa", species = "psilogyne", sps_code = "maps", density = 0.663,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "myrmecodia", species = "tuberosa", sps_code = "mytu", density = 0.663,
          ag_form = NA, 
          bg_form = NA, 
          ag_ref = NA, bg_ref = NA) %>%
  add_row(genus = "nypa", species = "fruticans", sps_code = "nyfr", density = NA,
          ag_form = c(function(dbh, dens) { dbh * 2.8929 }), 
          bg_form = c(function(dbh, dens) { dbh * 0 }), 
          ag_ref = "matsui2014", bg_ref = "matsui2014") %>%
  add_row(genus = "phoenix", species = "paludosa", sps_code = "phpa", density = NA,
          ag_form = c(function(dbh, dens) { dbh * 2.8929 }), 
          bg_form = c(function(dbh, dens) { dbh * 0 }), 
          ag_ref = "matsui2014", bg_ref = "matsui2014") %>%
  add_row(genus = "pongamia", species = "pinnata", sps_code = "popi", density = 0.595,
          ag_form = c(general_ag_komiyama2005),
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "rhizophora", species = "apiculata", sps_code = "rhap", density = 0.850,
          ag_form = c(rhap_ag_ong2004), 
          bg_form = c(rhap_bg_ong2004), 
          ag_ref = "ong2004", bg_ref = "ong2004") %>%
  add_row(genus = "rhizophora", species = "mucronata", sps_code = "rhmu", density = 0.821,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "rhizophora", species = "stylosa", sps_code = "rhst", density = 0.840,
          ag_form = c(rhst_ag_comley2005), 
          bg_form = c(rhst_bg_comley2005), 
          ag_ref = "comley2005", bg_ref = "comley2005") %>%
  add_row(genus = "scyphiphora", species = "hydrophyllacea", sps_code = "schy", density = 0.90,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "sonneratia", species = "caseolaris", sps_code = "soca", density = 0.389,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "sonneratia", species = "alba", sps_code = "soal", density = 0.509,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "sonneratia", species = "ovata", sps_code = "soov", density = 0.370,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "sonneratia", species = "unknown", sps_code = "soun", density = 0.380,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "unknown", species = "unknown", sps_code = "unun", density = NA,
          ag_form = c(general_ag_komiyama2005), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "komiyama2005", bg_ref = "komiyama2005") %>%
  add_row(genus = "xylocarpus", species = "granatum", sps_code = "xygr", density = 0.567,
          ag_form = c(xygr_ag_clough1989), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "clough1989", bg_ref = "komiyama2005") %>%
  add_row(genus = "xylocarpus", species = "moluccensis", sps_code = "xymo", density = 0.611,
          ag_form = c(xygr_ag_clough1989), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "clough1989", bg_ref = "komiyama2005") %>%
  add_row(genus = "xylocarpus", species = "unknown", sps_code = "xyun", density = 0.589,
          ag_form = c(xygr_ag_clough1989), 
          bg_form = c(general_bg_komiyama2005), 
          ag_ref = "clough1989", bg_ref = "komiyama2005")
