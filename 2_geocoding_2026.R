library(tidyverse)
library(RJSONIO)
library(jsonlite)

df_trastos <- read_csv("./outdata/raw_scrapped_data20260127_temp.csv")

df_trastos <- df_trastos %>%
  mutate(lon = NA,
         lat = NA,

         osm_id = NA,
         class = NA,
         type = NA,
         display_name = NA,

         lon_specific = NA,
         lat_specific = NA,

         osm_id_specific = NA,
         place_id_specific = NA,
         type_specific = NA,
         display_name_specific = NA) %>%

  distinct(df_name,k,df_name_specific,.keep_all = T)

nrow <- nrow(df_trastos)

CityName <- gsub(' ','%20',"Barcelona")
CountryCode <- "ES"

osm_url <- "https://nominatim.openstreetmap.org/search"
agentText <- "BarcelonaTrash v2 contact [EMAIL HERE]" ## enter your email here

for(i in 1:nrow(df_trastos)){

  if(is.na(df_trastos$lon[i])){

    ###=======###==###==###==###==###
    ### Get Coords for input name
    ###==###==###==###==###==###==###

    ## Old method

    # url <- paste(
    #   "http://nominatim.openstreetmap.org/search?street="
    #   ,df_trastos$df_name_specific[i]
    #   ,"&city="
    #   , CityName
    #   , "&countrycodes="
    #   , CountryCode
    #   ,"&viewbox=2.024231,41.271098,2.270737,41.467428"
    #   , "&limit=9&format=json"
    #   , sep="")
    #
    # x <- fromJSON(URLencode(url))

    ## New method

    x <- httr2::request(osm_url) |>
      httr2::req_user_agent(agentText) |>
      httr2::req_url_query(
        street = df_trastos$df_name_specific[i],
        city = CityName,
        countrycodes = CountryCode,
        viewbox = "2.024231,41.271098,2.270737,41.467428",
        limit = 9,
        format = "json") %>%
      httr2::req_perform(.) %>%
      httr2::resp_body_json(.) %>%
      dplyr::bind_rows(.) %>%
      slice(1)

    if(length(x)>0){

      x <- x %>%
        dplyr::select(lat,lon, display_name,class,type,osm_id)

      df_trastos$lon[i] <- x$lon
      df_trastos$lat[i] <- x$lat

      df_trastos$osm_id[i] <- x$osm_id
      df_trastos$class[i] <- x$class
      df_trastos$type[i]<- x$type
      df_trastos$display_name[i]<- x$display_name

    }


  }

  if(i %% 200 == 0){

    write_csv(df_trastos,paste0("./geocoded_scrapped_data",
                                format(Sys.Date(),"%Y%m%d"),".csv"))
  }

  print(paste0(i," / ",nrow(df_trastos)))
  Sys.sleep(1)

}

write_csv(df_trastos,paste0("./geocoded_scrapped_data",
                            format(Sys.Date(),"%Y%m%d"),".csv"))
