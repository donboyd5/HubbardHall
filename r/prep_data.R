

# setup -------------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# constants ---------------------------------------------------------------
rawdir <- here::here("data", "raw")
rdsdir <- here::here("data", "rds")


# get donations -----------------------------------------------------------
# fndon <- "HH Donations 2011-2023.xlsx"
fndon <- "HH Donations 2011-2023 with ID.xlsx"
# excel_sheets(path(rawdir, fndon))

don1 <- read_excel(path(rawdir, fndon), sheet="export")
names(don1)

# [1] "Account ID"                    "Account Type"                  "First Name"                    "Last Name"                    
# [5] "Household Name"                "Company Name"                  "Company Primary Contact Name"  "Address Line 1"               
# [9] "Address Line 2"                "City"                          "State/Province"                "Zip Code"                     
# [13] "Email 1"                       "Phone 1 Full Number (F)"       "Donation Date"                 "Donation Amount"              
# [17] "Campaign Name"                 "Donor Covered Fees"            "First Donation Date"           "First Donation Amount"        
# [21] "Last Donation Date"            "Last Donation Amount"          "Recurring Donation"            "Tender Type"                  
# [25] "What have they sponsored? (C)" "Deceased"                      "Do Not Contact"   


# get attendance ----------------------------------------------------------
# fnattold <- "HH Event and Class Registrations 2013-2023.xlsx"
fnatt <- "HH Event and Class Registrations 2013-2023 with ID.xlsx"
# fpathatt <- path(rawdir, fnatt)
# excel_sheets(path(rawdir, fnatt))

att1 <- read_excel(path(rawdir, fnatt), sheet="export")
names(att1)

# [1] "Account ID"                               "Account Type"                             "First Name"                              
# [4] "Last Name"                                "Household Name"                           "Company Name"                            
# [7] "Address Line 1"                           "Address Line 2"                           "City"                                    
# [10] "State/Province"                           "Zip Code"                                 "Email 1"                                 
# [13] "Phone 1 Full Number (F)"                  "Event Name"                               "Event Registration Attendee Count"       
# [16] "Attendee Detail"                          "Event Registration Amount"                "Event Category Name"                     
# [19] "Event Start Date"                         "Event End Date"                           "Registration Date/Time"                  
# [22] "Event Ticket"                             "Instructor"                               "Tender Type"                             
# [25] "How did you hear about Hubbard Hall? (C)" "Event Registration Status"                "Donor Covered Fees"                      
# [28] "Event Registration Coupon Code"           "Deceased"                                 "Do Not Contact"    


# create common variable names -------------------------------------------------------
intersect(names(don1), names(att1))
# [1] "Account ID"              "Account Type"            "First Name"              "Last Name"               "Household Name"         
# [6] "Company Name"            "Address Line 1"          "Address Line 2"          "City"                    "State/Province"         
# [11] "Zip Code"                "Email 1"                 "Phone 1 Full Number (F)" "Donor Covered Fees"      "Tender Type"            
# [16] "Deceased"                "Do Not Contact"   
common_names <- read_csv(
"vname, vdesc
account, Account ID
acctype, Account Type
fname, First Name
lname, Last Name
hhname, Household Name
coname, Company Name
addr1, Address Line 1
addr2, Address Line 2 
city, City
state, State/Province
zip, Zip Code 
email1, Email 1 
phone1, Phone 1 Full Number (F)
donorfees, Donor Covered Fees 
tender, Tender Type
deceased, Deceased
nocontact, Do Not Contact
")
common_names

setdiff(names(don1), names(att1))
# [1] "Company Primary Contact Name"  "Donation Date"                 "Donation Amount"               "Campaign Name"                
# [5] "First Donation Date"           "First Donation Amount"         "Last Donation Date"            "Last Donation Amount"         
# [9] "Recurring Donation"            "What have they sponsored? (C)"
donationonly_names <- read_csv(
"vname, vdesc
copcname, Company Primary Contact Name
dondate, Donation Date
donation, Donation Amount
campaign, Campaign Name
firstdondate, First Donation Date
firstdonamount, First Donation Amount 
lastdondate, Last Donation Date
lastdonation, Last Donation Amount
recurrdonation, Recurring Donation
sponsored, What have they sponsored? (C)
")
donationonly_names

setdiff(names(att1), names(don1))
# [1] "Event Name"                               "Event Registration Attendee Count"        "Attendee Detail"                         
# [4] "Event Registration Amount"                "Event Category Name"                      "Event Start Date"                        
# [7] "Event End Date"                           "Registration Date/Time"                   "Event Ticket"                            
# [10] "Instructor"                               "How did you hear about Hubbard Hall? (C)" "Event Registration Status"               
# [13] "Event Registration Coupon Code"    
attendanceonly_names  <- read_csv(
"vname, vdesc
evname, Event Name
evcount, Event Registration Attendee Count
evdetail, Attendee Detail
evamount, Event Registration Amount
evcategory, Event Category Name
evdate_start, Event Start Date
evdate_end, Event End Date
regdtime, Registration Date/Time
evticket, Event Ticket
instructor, Instructor
howheardhh, How did you hear about Hubbard Hall? (C)
evregstatus, Event Registration Status
evregcoupon, Event Registration Coupon Code
")

(donnames <- bind_rows(common_names, donationonly_names))
(attnames <- bind_rows(common_names, attendanceonly_names))

allnames <- bind_rows(
  common_names |> mutate(vtype="common"),
  donationonly_names |> mutate(vtype="donation"),
  attendanceonly_names |> mutate(vtype="attendance")
)
allnames
saveRDS(allnames, here::here("data","rds", "vnames.rds"))


# create files with variable names ----------------------------------------

allnames <- readRDS(here::here("data","rds", "vnames.rds"))

## donations ----
# create named vector to use for renaming
dnames1 <- allnames |> 
  filter(vtype %in% c("common", "donation")) |> 
  select(newname=vname, oldname=vdesc)
(dnames <- setNames(dnames1$oldname, dnames1$newname))

don2 <- don1 |> 
  rename(all_of(dnames))
glimpse(don2)

## attendance ----
anames1 <- allnames |> 
  filter(vtype %in% c("common", "attendance")) |> 
  select(newname=vname, oldname=vdesc)
(anames <- setNames(anames1$oldname, anames1$newname))

att2 <- att1 |> 
  rename(all_of(anames))
glimpse(att2)

# att2 |> 
#   count(evname, howheardhh) |> 
#   count(howheardhh, sort = TRUE)

# notes:
#   some households fall into multiple conames -- NA and named (company names)
#   do any people fall into multiple households? (check)
#   do any people fall into multiple companies? (check)

tmp <- count(don3, hhname, coname)

# create person ids (can be multiple) ----
pid1 <- bind_rows(
  don2 |> select(account, acctype, lname, fname, hhname, coname, addr1, addr2, city, state, zip, deceased),
  att2 |> select(account, acctype, lname, fname, hhname, coname, addr1, addr2, city, state, zip, deceased)) |> 
  mutate(across(where(is.character), str_trim)) |> 
  distinct() |> 
  arrange(lname, fname, city, hhname, coname)

count(pid1, acctype)
# account acctype lname
# 35207 Individual Ovington - Deceased  should it be company account?
# 38499 Individual Hitchen Marta (should be deceased)

## correct known errors and save ----------------------------------------------------
# coname Cafe Lena should be Caffe Lena; email for Sarah Craig is sarah@caffelena.org
# pid1 |> filter(str_detect(coname, "Lena")) # hmm not in there

# 39220 Company Administrator Linda Faria NA
pid1 |> filter(account==39220)

# 36958 39193

corrections <- read_csv(
"account, vname, correction
35207, lname, Ovington

36958, lname, Bauer

37765, lname, Gariepy
37765, fname, James
37765, addr1, 73 West Main St
37765, city, Cambridge
37765, state, NY
37765, zip, 12816

37791, lname, Brooks
37791, fname, Constance
37791, addr1, 15 East Main St.
37791, city, Cambridge
37791, state, NY
37791, zip, 12816

38499, deceased, Yes

38974, addr1, P.O. Box 454
38974, city, Cambridge
38974, state, NY
38974, zip, 12816

39193, addr1, 2531 State Route 40
39193, city, Greenwich
39193, state, NY
39193, zip, 12834

39220, lname, Faria
39220, fname, Linda
39720, coname, NA_character_
") |> 
  mutate(correction=str_trim(correction)) |> 
  pivot_wider(names_from = vname, values_from = correction)
corrections

f <- function(original, new) {
  ifelse(!is.na(new), new, original)
}

don_clean <- don2 |>
  left_join(corrections, by = "account", suffix = c("", "_new")) |> 
  mutate(across(c(lname, fname, coname, addr1, city, state, zip, deceased), 
                ~ f(.x, get(paste0(cur_column(), "_new"))))) |> 
  mutate(coname=ifelse(account==39720, NA_character_, coname),
         addr2=ifelse(account==37791, NA_character_, addr2)) |> 
  select(-ends_with("_new"))
saveRDS(don_clean, here::here("data", "rds", "donations.rds"))

att_clean <- att2 |>
  left_join(corrections, by = "account", suffix = c("", "_new")) |> 
  mutate(across(c(lname, fname, coname, addr1, city, state, zip, deceased), 
                ~ f(.x, get(paste0(cur_column(), "_new"))))) |> 
  mutate(coname=ifelse(account==39720, NA_character_, coname),
         addr2=ifelse(account==37791, NA_character_, addr2)) |> 
  select(-ends_with("_new"))
saveRDS(att_clean, here::here("data", "rds", "attendance.rds"))


# geocode -----------------------------------------------------------------
library(ggmap)
library(purrr)
gmaps_apikey <- "AIzaSyDFWxscpciI2HDziBJ3CIhqxKWEqbaD6VM"
register_google(key = gmaps_apikey, write = TRUE)
google_key()

# get unique addresses
add1 <- bind_rows(
  don_clean |> select(addr1, addr2, city, state, zip) |> distinct(),
  att_clean |> select(addr1, addr2, city, state, zip) |> distinct()) |> 
  distinct()

add2 <- add1 |> 
  mutate(across(c(addr1, addr2, city, state, zip), ~ ifelse(is.na(.x), "", .x))) |> 
  unite(location, addr1, addr2, city, state, zip, sep=", ", remove = FALSE)

dfaddr <- add2 |> 
  ggmap::mutate_geocode(location)

saveRDS(dfaddr, here::here("data", "rds", "addresses_geocoded.rds"))

# Hubbard Hall, 25 E Main St, Cambridge, NY 12816
(hhaddr <- ggmap::geocode("25 E Main St, Cambridge, NY 12816"))
# -73.37994 43.02811 lon lat
saveRDS(hhaddr, here::here("data", "HHaddress_geocoded.rds"))


# distances ---------------------------------------------------------------
geoaddr <- readRDS(here::here("data", "rds", "addresses_geocoded.rds"))
don <- readRDS(here::here("data", "rds", "donations.rds"))
att <- readRDS(here::here("data", "rds", "attendance.rds"))
hhaddr <- readRDS(here::here("data", "rds", "HHaddress_geocoded.rds"))

f <- function(lon1, lat1, lon2, lat2) {
  geosphere::distGeo(cbind(lon1, lat1), cbind(lon2, lat2)) / 1609.34
}

don2 <- don |> 
  mutate(across(c(addr1, addr2, city, state, zip), ~ ifelse(is.na(.x), "", .x))) |> 
  unite(location, addr1, addr2, city, state, zip, sep=", ", remove = FALSE) |> 
  left_join(geoaddr, by = join_by(location)) |> 
  mutate(distance=f(lon, lat, hhaddr$lon, hhaddr$lat),
         across(contains("date"), as.Date),
         fyear=ifelse(month(dondate) > 6, year(dondate) + 1, year(dondate)),
         donation=ifelse(is.na(donation), 0, donation))
glimpse(don2)
skim(don2)
saveRDS(don2, here::here("data", "rds", "donations_geo.rds"))

att2 <- att |> 
  mutate(across(c(addr1, addr2, city, state, zip), ~ ifelse(is.na(.x), "", .x))) |> 
  unite(location, addr1, addr2, city, state, zip, sep=", ", remove = FALSE) |> 
  left_join(geoaddr, by = join_by(location)) |> 
  mutate(distance=f(lon, lat, hhaddr$lon, hhaddr$lat),
         across(contains("date"), as.Date),
         fyear_start=ifelse(month(evdate_start) > 6, year(evdate_start) + 1, year(evdate_start)),
         fyear_end=ifelse(month(evdate_end) > 6, year(evdate_end) + 1, year(evdate_end)))
glimpse(att2)
skim(att2)
saveRDS(att2, here::here("data", "rds", "attendance_geo.rds"))


# OLD below here ----


pid2 <- pid1 |>
  left_join(corrections, by = "account", suffix = c("", "_new")) |> 
  mutate(across(c(lname, fname, coname, addr1, city, state, zip, deceased), 
                ~ f(.x, get(paste0(cur_column(), "_new"))))) |> 
  mutate(coname=ifelse(account==39720, NA_character_, coname),
         addr2=ifelse(account==37791, NA_character_, addr2)) |> 
  select(-ends_with("_new"))

pid3 <- pid2 |> 
  distinct()
  
tmp <- bind_rows(
  pid1 |> filter(account %in% unique(corrections$account)) |> mutate(type="pid1"),
  pid3 |> filter(account %in% unique(corrections$account)) |> mutate(type="pid3")) |> 
  arrange(account, type)
tmp

saveRDS(pid3, here::here("data", "rds", "persons.rds"))

## RETURN TO THIS: get wide person file showing multiple companies ----
# get people with multiple companies
multcos <- pid3 |> 
  select(lname, fname, addr1, addr2, city, state, zip, coname) |> 
  group_by(lname, fname, addr1, addr2, city, state, zip) |> 
  mutate(ncos=n())

multcos |> 
  filter(ncos > 1)

# put corrected info on donor and attendance files ----
persons <- readRDS(here::here("data", "rds", "persons.rds"))

(dropnames <- setdiff(names(persons), c("account", "acctype")))

don_clean <- don2 |> 
  select(-all_of(dropnames)) |> 
  left_join(persons)

bad_rows <- don2 |> filter(row_number() %in% c(17, 381))

don2 |> filter(row_number()==17)
persons |> filter(account==39193)




## donations ----
tmp <- count(don2, hhname, coname)
# visual inspection shows that coname should not be "--none--
don3 <- don2 |> 
  mutate(coname=case_when(
    coname=="--None--" ~ NA_character_,
    TRUE ~ coname
    ))
tmp <- count(don3, hhname, coname)

## attendance ----
tmp <- count(att2, hhname, coname)
tmp <- count(att2, lname, fname, addr1, addr2, city, state, zip) |> 
  mutate(n=n(), .by=lname)

# tmp <- count(att2, lname, fname, hhname, coname, addr1, addr2, city, state, zip) |> 
#   mutate(n=n(), .by=c(lname, fname)) |> 
#   filter(n > 1)
# tmp <- att2 |> 
#   filter(fname=="Mary", lname=="Broderick")
# tmp <- att2 |> 
#   filter(fname=="Alexina", lname=="Jones")
# tmp <- don2 |> 
#   filter(fname=="Alexina", lname=="Jones")


att3 <- att2 |> 
  mutate(coname=case_when(
    coname=="--None--" ~ NA_character_,
    TRUE ~ coname
  ))
tmp <- count(att3, hhname, coname)




# do any individuals have multiple non-NA hhnames?
tmp <- upid1 |> 
  filter(!is.na(hhname)) |> 
  mutate(hhnum=length(unique(hhname)), .by=c(lname, fname, addr1, city, state, zip))
tmp |> filter(hhnum > 1) # no, none do (mother daughter named Alix Jones)

# do any individuals have multiple non-NA conames? # yes, two people do
tmp <- upid1 |> 
  filter(!is.na(coname), !is.na(lname)) |> 
  mutate(conum=length(unique(coname)), .by=c(lname, fname, addr1, city, state, zip))
tmp |> filter(conum > 1)

# lname      fname hhname coname                       addr1                   city       state zip   conum
# <chr>      <chr> <chr>  <chr>                        <chr>                   <chr>      <chr> <chr> <int>
#   1 E. Secor   NA    NA     MSK Engineering & Design Inc PO Box 10               Bennington VT    05201     2
# 2 E. Secor   NA    NA     The Secor Group LLC          PO Box 10               Bennington VT    05201     2
# 3 Napolitano Paul  NA     Critter Cleanup NY, LLC      471 North Broadway #308 Jericho    NY    11753     2
# 4 Napolitano Paul  NA     Two Sons Environmental LLC   471 North Broadway #308 Jericho    NY    11753     2




## create household hhname ids ----
uhhdon <- 


## create person ids ----
upiddon <- don3 |> 
  select(lname, fname, addr1, addr2, city, state, zip) |> 
  distinct()

uattendance <- att2 |> 
  select(lname, fname, hhname, coname, addr1, addr2, city, state, zip) |> 
  distinct()

uids1 <- bind_rows(
  uattendance |> 
    mutate(src="attendance"),
  udonation |> 
    mutate(src="donor")) |>
  mutate(coname=ifelse(coname=="--None--", NA_character_, coname)) |> 
  arrange(lname, fname, hhname, coname) |> 
  group_by(lname, fname, hhname, coname) |> 
  mutate(ptype=case_when(
    n()==1 & src=="attendance" ~ "attend_only",
    n()==1 & src=="donor" ~ "donor_only",
    n()>=2 ~ "both",
    TRUE ~ "error")) |> 
  ungroup()

count(uids1, ptype)
uids1 |> filter(ptype=="error")


uids <- uids1 |> 
  select(lname, fname, hhname, coname, ptype, addr1, addr2, city, state, zip) |> 
  distinct() |> 
  arrange(lname, fname, hhname, coname) |> 
  mutate(uid=row_number()) |> 
  relocate(uid)
count(uids, ptype)

count(uids, coname)

saveRDS(uids, here::here("data","rds", "uids.rds"))


# put unique id on each file and save -------------------------------------
don3 <- don2 |> 
  mutate(cyear=year(dondate),
         month=month(dondate),
         fyear=ifelse(month > 6, cyear + 1, cyear)) |> 
  left_join(uids |> 
              select(-c(addr1, addr2, city, state, zip)),
            by = join_by(fname, lname, hhname, coname),
            relationship = "many-to-many") |> 
  relocate(uid, ptype)
glimpse(don3)
saveRDS(don3, here::here("data", "rds", "donations.rds"))

att3 <- att2 |> 
  mutate(evdate_start=as.Date(evdate_start),
         evdate_end=as.Date(evdate_end),
         cyear=year(evdate_start),
         month=month(evdate_start),
         fyear=ifelse(month > 6, cyear + 1, cyear)) |> 
  left_join(uids |> 
              select(-c(addr1, addr2, city, state, zip)),
            by = join_by(fname, lname, hhname, coname),
            relationship = "many-to-many") |> 
  mutate(evdate_start=as.Date(evdate_start),
         evdate_end=as.Date(evdate_end)) |> 
  relocate(uid, ptype)
glimpse(att3)
saveRDS(att3, here::here("data", "rds", "attendance.rds"))



# geocode -----------------------------------------------------------------
library(ggmap)
library(purrr)
gmaps_apikey <- "AIzaSyDFWxscpciI2HDziBJ3CIhqxKWEqbaD6VM"
register_google(key = gmaps_apikey, write = TRUE)
google_key()


uids <- readRDS(here::here("data","rds", "uids.rds"))

df <- uids |> 
  filter(ptype != "attend_only") |> 
  # filter(row_number() <= 5) |> 
  mutate(across(c(addr1, addr2, city, state, zip), ~ ifelse(is.na(.x), "", .x))) |> 
  rowwise() |> 
  mutate(location = reduce(across(c(addr1, addr2, city, state, zip)), ~paste(.x, .y, sep = ", ")))

df$location

dfaddr <- df |> 
  select(location) |> 
  distinct() |> 
  ggmap::mutate_geocode(location)

saveRDS(dfaddr, here::here("data", "addresses_geocoded.rds"))

# Hubbard Hall, 25 E Main St, Cambridge, NY 12816
(hhaddr <- ggmap::geocode("25 E Main St, Cambridge, NY 12816"))
# -73.37994 43.02811 lon lat
saveRDS(hhaddr, here::here("data", "HHaddress_geocoded.rds"))


# distances ---------------------------------------------------------------
uids <- readRDS(here::here("data","rds", "uids.rds"))
geoaddr <- readRDS(here::here("data", "addresses_geocoded.rds"))
don <- readRDS(here::here("data", "rds", "donations.rds"))
hhaddr <- readRDS(here::here("data", "HHaddress_geocoded.rds"))

glimpse(uids)
uids1 <- uids |> 
  filter(ptype != "attend_only") |> 
  mutate(across(c(addr1, addr2, city, state, zip), ~ ifelse(is.na(.x), "", .x))) |> 
  rowwise() |> 
  mutate(location = reduce(across(c(addr1, addr2, city, state, zip)), ~paste(.x, .y, sep = ", "))) |> 
  left_join(geoaddr, by=join_by(location))


don2 <- don |> 
  select(uid, hhname, coname, ptype, addr1, addr2, city, state, zip, fyear, donation) |> 
  left_join(uids1 |> select(uid, location),
            by=join_by(uid)) |> 
  left_join(geoaddr, by=join_by(location)) |> 
  mutate(hhlon=hhaddr$lon, hhlat=hhaddr$lat)

glimpse(don2)

# distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
# 
# distHaversine(c(0,0),c(90,90))
# 
# point1 <- c(lon = -123.3656, lat = 48.4284)  # Example coordinates for Victoria, BC
# point2 <- c(lon = -123.1216, lat = 49.2827)  # Example coordinates for Vancouver, BC
# 
# # Calculate the distance in meters using distHaversine
# distance_meters <- distHaversine(point1, point2)
# 
# # Convert the distance to miles
# distance_miles <- distance_meters / 1609.34

# f <- function(lon1, lat1, lon2, lat2) {
#   distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1609.34
# }

f <- function(lon1, lat1, lon2, lat2) {
  geosphere::distGeo(cbind(lon1, lat1), cbind(lon2, lat2)) / 1609.34
}

# (lon1 <- don2$lon[1:4])
# (lat1 <- don2$lat[1:4])
# (lon2 <- don2$hhlon[1:4])
# (lat2 <- don2$hhlat[1:4])
# 
# distGeo(cbind(lon1, lat1), cbind(lon2, lat2)) / 1609.34
# distHaversine(cbind(lon1, lat1), cbind(lon2, lat2)) / 1609.34

# system.time(don2a <- don2 |> 
#               rowwise() |> 
#               mutate(distance=f(lon, lat, hhlon, hhlat)) |> 
#               ungroup())

# system.time(don2b <- don2 |> 
#   mutate(distance = pmap_dbl(list(lon, lat, hhlon, hhlat), f)))

don2c <- don2 |> 
  mutate(distance=f(lon, lat, hhlon, hhlat))

# glimpse(don2a)
# glimpse(don2b)

tmp <- don2c |> 
  filter(!is.na(distance), donation > 0) |> 
  select(uid, hhname, coname, location, distance, donation) |> 
  summarise(donation=sum(donation), .by=c(uid, hhname, coname, location, distance)) |> 
  arrange(distance)

tmp |> filter(str_detect(location, "Box"),
              str_detect(location, "Cambridge")) |> 
  arrange(desc(donation)) # 26 Cambridge PO boxes


tmp |> 
  arrange(desc(donation)) 

# lifetime amounts
loc <- "PO Box 402" # Wolski and Battenkill Chorale $12,677
loc <- "PO Box 341" # Richard Bump & Bud Kelleher / Cambridge Antiques Center $19k
loc <- "Box 454" # Lila Brown and MFS $2,200
loc <- "44 Central Ave." # Frances Gruber Preservation League Albany, $22k
loc <- "106 Scotch Hill" # Lynn Caponera Maurice Sendak Foundation, $14.9k
loc <- "PO Box 435" # Amy Potter, Stewarts, Saratoga, $10.9k
loc <- "1 E. Main St." #  Besse Gracie Lynn, Robert H. Wentorf Foundation, Alan Wrigley (Treasurer), $10k [RHW funds educational programs for forest owners]
loc <- "11480 Commerce Park Drive" # Indiegogo/First Giving Reston VA, $8.2k
loc <- "PO Box 511"  # Joshua Levy, Phantom Laboratory, Salem $7.6k [biomedical stuff https://www.linkedin.com/company/phantom-laboratory-the/ https://www.lwseddon.com/portfolio/the-phantom-laboratory/]
loc <- "100 Hospital Dr" # Ray Smith, Southwestern Vermont Medical Center, Bennington $7.2k
loc <- "55 Walls Drive" # Linda Faria, CR Wood, Fairfield CT, $7k
loc <- "31 Church Street" # Marcus Maringola, Robert Ward, AMSURE Adk Trust, Saratoga $6.5k
loc <- ""


uids1 |> filter(str_detect(location, loc)) 


quantile(tmp$distance)

tmp |> 
  filter(str_detect(hhname, "Boyd"))
