print("global.R")
print(getwd())

# LIBRARIES
# Load your libraries here
print("Loading libraries...")
{
  library(shiny)
  library(rgdal)
  library(leaflet)
  library(plyr)
  library(dplyr)
  library(RColorBrewer)
}


# TEMPLATE PARAMETERS
{
  App_title<-"Shiny app (leaflet map) for technical dashboard P&C insurance multinational"
}


# GENERAL FUNCTIONS
print("Loading general functions...")
{
  # You can load some global functions here...
}


# INITIALIZATION
print("Initializing environment...")
{
  
  # read-in shape file
  world <- readOGR("../www/spatial", 'ne_50m_admin_0_countries', encoding='UTF-8')
  
  # filter countries where insurance company has some business
  sel <- c("Angola", "United Arab Emirates", "Bolivia", "Brazil", "Czech Rep.", "Algeria",
           "Estonia", "Colombia", "Greece", "Hungary", "Lebanon", "Lithuania", "Luxembourg", "Latvia",
           "Namibia", "Poland", "Russia", "Senegal", "Slovakia", "Ukraine", "Qatar", "Oman", "Saudi Arabia", 
           "Mexico", "Morocco", "Turkey")           
  
  company <- subset(world, name %in% sel)
  
  # drop unused levels
  company$name <- droplevels(company$name)
  
  ## simulate financial report data
  n <- nrow(company@data)

  countries <- c("Ang", "UAE", "Bol", "Bra", "Cze", "Alg", 
    "Est", "Col", "Gre", "Hun", "Leb", "Lit", "Lux", "Lat",
    "Nam", "Pol", "Rus", "Sen", "Slo", "Ukr", "Qat", "Oma",
    "Sau", "Mex", "Mor", "Tur"
  )
  
  set.seed(123)
        
  # personal line, forecast 2
  gwp_f2_pl <- sapply(sample(x = rnorm(100, 10, 3), size = n), function(i) max(0.1, i))
  lr_f2_pl <- sapply(sample(x = rnorm(100, 0.65, 0.3), size = n), function(i) max(0.2, i))
  er_f2_pl <- sapply(sample(x = rnorm(100, 0.1, 0.1), size = n), function(i) max(0.02, i))
  ar_f2_pl <- sapply(sample(x = rnorm(100, 0.15, 0.1), size = n), function(i) max(0.05, i))
  chcr_f2_pl <- sapply(sample(x = rnorm(100, 0.05, 0.01), size = n), function(i) max(0.01, i))
  cr_f2_pl <- lr_f2_pl + er_f2_pl + ar_f2_pl + chcr_f2_pl
  ep_f2_pl <- gwp_f2_pl * 0.75
  tech_marg_f2_pl <- ep_f2_pl * (1 - cr_f2_pl)
  
  # personal line, revised budget
  gwp_rb_pl <- sapply(gwp_f2_pl + rnorm(n, 0, 0.01), function(i) max(0.1, i))
  lr_rb_pl <- sapply(lr_f2_pl + rnorm(n, 0, 0.01), function(i) max(0.2, i))
  er_rb_pl <- sapply(er_f2_pl + rnorm(n, 0, 0.01), function(i) max(0.02, i))
  ar_rb_pl <- sapply(ar_f2_pl + rnorm(n, 0, 0.01), function(i) max(0.05, i))
  chcr_rb_pl <- sapply(chcr_f2_pl + rnorm(n, 0, 0.005), function(i) max(0.01, i))
  cr_rb_pl <- lr_rb_pl + er_rb_pl + ar_rb_pl + chcr_rb_pl
  ep_rb_pl <- gwp_rb_pl * 0.75
  tech_marg_rb_pl <- ep_rb_pl * (1 - cr_rb_pl)
  
  # commercial line, forecast 2
  gwp_f2_cl <- sapply(sample(x = rnorm(100, 7, 4), size = n), function(i) max(0.01, i))
  lr_f2_cl <- sapply(sample(x = rnorm(100, 0.35, 0.5), size = n), function(i) max(0.15, i))
  er_f2_cl <- sapply(sample(x = rnorm(100, 0.1, 0.1), size = n), function(i) max(0.02, i))
  ar_f2_cl <- sapply(sample(x = rnorm(100, 0.25, 0.05), size = n), function(i) max(0.1, i))
  chcr_f2_cl <- sapply(sample(x = rnorm(100, 0.01, 0.001), size = n), function(i) max(0.005, i))
  cr_f2_cl <- lr_f2_cl + er_f2_cl + ar_f2_cl + chcr_f2_cl
  ep_f2_cl <- gwp_f2_cl * 0.75
  tech_marg_f2_cl <- ep_f2_cl * (1 - cr_f2_cl)
  
  # commercial line, revised budget
  gwp_rb_cl <- sapply(gwp_f2_cl + rnorm(n, 0, 0.01), function(i) max(0.1, i))
  lr_rb_cl <- sapply(lr_f2_cl + rnorm(n, 0, 0.01), function(i) max(0.2, i))
  er_rb_cl <- sapply(er_f2_cl + rnorm(n, 0, 0.01), function(i) max(0.02, i))
  ar_rb_cl <- sapply(ar_f2_cl + rnorm(n, 0, 0.01), function(i) max(0.05, i))
  chcr_rb_cl <- sapply(chcr_f2_cl + rnorm(n, 0, 0.005), function(i) max(0.01, i))
  cr_rb_cl <- lr_rb_cl + er_rb_cl + ar_rb_cl + chcr_rb_cl
  ep_rb_cl <- gwp_rb_cl * 0.75
  tech_marg_rb_cl <- ep_rb_cl * (1 - cr_rb_cl)
  
  report_data <- data.frame(entity = countries,
                            gwp_f2_pl, lr_f2_pl, er_f2_pl, ar_f2_pl, chcr_f2_pl, cr_f2_pl, ep_f2_pl, tech_marg_f2_pl,
                            gwp_rb_pl, lr_rb_pl, er_rb_pl, ar_rb_pl, chcr_rb_pl, cr_rb_pl, ep_rb_pl, tech_marg_rb_pl,
                            gwp_f2_cl, lr_f2_cl, er_f2_cl, ar_f2_cl, chcr_f2_cl, cr_f2_cl, ep_f2_cl, tech_marg_f2_cl,
                            gwp_rb_cl, lr_rb_cl, er_rb_cl, ar_rb_cl, chcr_rb_cl, cr_rb_cl, ep_rb_cl, tech_marg_rb_cl
                            )
  

  ## Clean up data for merging with shapefile
  
  # couple entity with relative country name (for Gulf I choose Saudi Arabia, To Be Improved)
  lookup= data.frame( 
        entity=c("Ang", "UAE", "Bol", "Bra", "Cze", "Alg", 
                 "Est", "Col", "Gre", "Hun", "Leb", "Lit", "Lux", "Lat",
                 "Nam", "Pol", "Rus", "Sen", "Slo", "Ukr", "Qat", "Oma",
                 "Sau", "Mex", "Mor", "Tur"
                 ), 
        name=c("Angola", "United Arab Emirates", "Bolivia", "Brazil", "Czech Rep.", "Algeria",
               "Estonia", "Colombia", "Greece", "Hungary", "Lebanon", "Lithuania", "Luxembourg", "Latvia",
               "Namibia", "Poland", "Russia", "Senegal", "Slovakia", "Ukraine", "Qatar", "Oman", "Saudi Arabia", 
               "Mexico", "Morocco", "Turkey"), stringsAsFactors = TRUE
        )

  report_data <- left_join(report_data, lookup)

  # Merge company report data with shape file
  company@data <-left_join(company@data, report_data)
  

}
