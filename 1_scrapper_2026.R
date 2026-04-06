# ================================
# ==========  Libraries ==========
# ================================

library(RSelenium)
library(tidyverse)
library(rvest)

# ================================
# ============  SETUP ============
# ================================

# This whole script could probably be (extremely) optimized, but it does the work rn.
rD <- rsDriver(port= sample(7600)[1], browser=c("firefox"),
               chromever = NULL,
               check = F,
               verbose = FALSE)

remDr <- rD$client

# Read street data, taken from Open Data BCN (callejero database)
# https://opendata-ajuntament.barcelona.cat/data/es/dataset/carrerer
df <- read_csv("./indata/carrerer_BarOPENDATA.csv") %>%
  rename(name = nom_oficial) %>%
  filter(!is.na(nre_min))

# Empty table to store the information
df_temp <- data.frame(

  i = as.integer(),
  df_name = as.character(),
  k = as.integer(),
  df_name_specific = as.character(),

  Time = as.character(),
  CalendarURL = as.character(),
  Location = as.character(),
  stringsAsFactors = FALSE
)


# If the script crashes
# I use this to "restart" from a pre-determined point

# df_temp <- read_csv("./df_temp_trashdata_20260125_2000.csv")

if(is.infinite(max(df_temp$i))){start_n <- 1} else {start_n <- max(df_temp$i)}

# ================================
# =============  RUN =============
# ================================

## Iterate through all the ~4,000 street names in Barcelona

for(i in start_n:nrow(df)){

  tryCatch({

    ## Navigate to website
    remDr$navigate("https://ajuntament.barcelona.cat/cercador-de-residus/es")
    Sys.sleep(2)

    ## Select Muebles and Trastos
    inputElement <- remDr$findElement(using = "id", value = "generic-search")
    inputElement$sendKeysToElement(list("Muebles y trastos"))
    Sys.sleep(1)

    ## Select Street
    inputElement <- remDr$findElement(using = "id", value = "cerca")
    inputElement$sendKeysToElement(list(df$name[i]))
    Sys.sleep(1)
    inputElement$sendKeysToElement(list(key = "space"))
    Sys.sleep(2)

    ## Check if street is valid
    tryCatch({
      carrer_exists <- remDr$findElement(using = "xpath",
                                         value = '//*[@id="err_cerca" and contains(text(), "Sin resultados")]')
    }, error = function(e) {NULL})

    ## Only continue if street is valid
    if(!exists("carrer_exists")){

      ## List all the street numbers available
      opts <- remDr$findElements(using = "css selector",
                                 value = "li.ui-menu-item a")

      df_list <- data.frame(
        idx   = seq_along(opts),
        label = vapply(opts,
                       function(x) x$getElementText()[[1]],
                       character(1)
        ), stringsAsFactors = FALSE)

      street_numbers <- as.numeric(df$nre_max[i]) - as.numeric(df$nre_min[i])

      if(street_numbers - nrow(df_list) > 15){
        k_total = street_numbers
      } else {
        k_total = nrow(df_list)
      }

      ## Iterate through SOME of the street numbers
      ## Here I check for every 5 (e.g. Carrer 1, Carrer 5, Carrer 10...)
      for(k in seq(0,k_total,5)){

        if(nrow(df_list)>=1){ # Sanity check - already validated above

          if(exists("df_list") & k > 25){rm(df_list)}

          if(k==0){k<-1} # If k = 0, then make it 1.

          ## Select Street

          if(k>25){

            inputElement$sendKeysToElement(list(as.character(k)))
            Sys.sleep(0.5)
            inputElement$sendKeysToElement(list(key = "space"))
            Sys.sleep(2)

            ## List all the street numbers available
            opts <- remDr$findElements(using = "css selector",
                                       value = "li.ui-menu-item a")
            df_list <-  data.frame(
              idx   = seq_along(opts),
              label = vapply(opts,
                             function(x) x$getElementText()[[1]],
                             character(1)
              ), stringsAsFactors = FALSE)

            ## Check if street is valid
            tryCatch({
              carrer_exists_2 <- remDr$findElement(using = "xpath",
                                                   value = '//*[@id="err_cerca" and contains(text(), "Sin resultados")]')
            }, error = function(e) {NULL})

            if(!exists("carrer_exists_2")){
              opts[[1]]$clickElement()
            }

          } else {
            opts[[k]]$clickElement()
            Sys.sleep(0.5)
            remDr$executeScript( "arguments[0].scrollIntoView(true);",
                                 list(opts[[k]]))
            Sys.sleep(0.5)
            opts[[k]]$clickElement()
          }

          Sys.sleep(1)

          if(!exists("carrer_exists_2")){

            ## Scroll to "Confirm" button
            webElem <- remDr$findElement(using = "css selector", "a.submit.btn.btn-dark.do-search-residu")
            remDr$executeScript("arguments[0].scrollIntoView(true);", list(webElem))

            ## Navigate to "Confirm" page
            href_value <- as.character(webElem$getElementAttribute("href"))
            remDr$navigate(paste0("https://ajuntament.barcelona.cat/cercador-de-residus/es/resid/RM0093/",
                                  last_numbers <- sub(".*/([^/]+)$", "\\1", href_value)))
            Sys.sleep(1.5)

            tryCatch({
              v_dataexists <- try(remDr$findElement(using = "xpath", "//div[@class='info-altres quan porta-porta']//p[1]"))
              if(class(v_dataexists)[1] != "webElement"){

                target_xpath <- "//h3[contains(text(), 'Información adicional')]/following-sibling::p[3]"
                v_dataexists <- remDr$findElements(using = "xpath", target_xpath)
                location <- v_dataexists[[1]]$getElementText()[[1]]
                time <- v_dataexists[[1]]$getElementText()[[1]]

              } else if(class(v_dataexists)[1] == "webElement"){

                time <- remDr$findElement(using = "xpath", "//div[@class='info-altres quan porta-porta']//p[1]")$getElementText()
                calendar_url <- remDr$findElement(using = "xpath", "//a[@class='link']")$getElementAttribute("href")
                location <- remDr$findElement(using = "xpath", "//div[@class='info-altres quan porta-porta']//p[2]")$getElementText()

              } else if(class(v_dataexists)[1] == "try-error"){

                time <- NA
                calendar_url <- NA
                location <- NA
              }


            }, error = function(e) {

              time <- NA
              calendar_url <- NA
              location <- NA

            }, warning = function(w) {

              time <- NA
              calendar_url <- NA
              location <- NA

            })
          }

          if(exists("carrer_exists_2")){rm(carrer_exists_2)}

          if(k>25){k_index<-1} else {k_index<-k}

          ## Add data to table
          df_temp <- bind_rows(df_temp,
                               data.frame(
                                 i = as.integer(i),
                                 df_name = df$name[i],

                                 k = as.integer(k),
                                 df_name_specific = df_list$label[k_index],

                                 Time = time[[1]],
                                 CalendarURL = calendar_url[[1]],
                                 Location = location[[1]],
                                 stringsAsFactors = FALSE
                               )
          )

          ##==##==## Return to Yojamba ##==##==##
          # Navigate back to the beginning.
          # Leaves you at the beginning of the loop.

          if(exists("time")){rm(time)}

          if(k < max(seq(0,k_total,5))){
            remDr$navigate("https://ajuntament.barcelona.cat/cercador-de-residus/es")
            Sys.sleep(2)

            inputElement <- remDr$findElement(using = "id", value = "generic-search")
            inputElement$sendKeysToElement(list("Muebles y trastos"))
            #Sys.sleep(2)

            inputElement <- remDr$findElement(using = "id", value = "cerca")
            inputElement$sendKeysToElement(list(df$name[i]))

            Sys.sleep(1)
            inputElement$sendKeysToElement(list(key = "space"))
            Sys.sleep(1)

            opts <- remDr$findElements(using = "css selector",
                                       value = "li.ui-menu-item a")
          }
        }
      }
    }

  }, error = function(e) {NULL})

  if(exists("carrer_exists")){rm(carrer_exists)}
  if(exists("carrer_exists_2")){rm(carrer_exists_2)}
  if(exists("time")){rm(time)}

  if(i%%100 == 0){

    write.csv(df_temp,
              paste0("./df_temp_trashdata_",
                     format(Sys.Date(),"%Y%m%d"),
                     "_",i,".csv")
    )

  }
}

## Save final output
write.csv(df_temp,
          paste0("./raw_scrapped_data",
                 format(Sys.Date(),"%Y%m%d"),
                 "_temp.csv")
)
