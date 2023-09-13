### pool waterhole for voronoi diagram

library(sf)
library(tidyverse)

#load data 
# get blocks
blocks <- st_read("figures/map/data_gis/blocks/Blocks.shp")
#waterpoints
hey<-st_read("figures/map/map/coordinates_waterhole/final.shp")
#border
bord<-st_read("figures/map/data_gis/borders/Border.shp")


## pool waterhole that have to be 
pool1<-hey %>% filter (waterpoint == "limpandi_1" |  waterpoint== "little_zivanini") %>% mutate(tess_name = "littledzi",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool2<-hey %>% filter (waterpoint == "basha" |  waterpoint== "leasha_dam") %>% mutate(tess_name = "basha",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool3<-hey %>% filter (waterpoint == "josivanini" ) %>% mutate(tess_name = waterpoint,meanx = GPS_X,meany= GPS_Y)

pool4<-hey %>% filter (waterpoint == "lememba" |  waterpoint== "shabi_shabi_vlei"  |  waterpoint== "shakwanki_1"  |  waterpoint== "mtswiri"  |  waterpoint== "tambontundlha") %>% 
  mutate(tess_name = "shabe",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool5<-hey %>% filter (waterpoint == "makona" |  waterpoint== "mpisi"  ) %>% 
  mutate(tess_name = "makona",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool6to10<-hey %>% filter (waterpoint == "secheche" |  waterpoint== "mbazu"  | waterpoint == "beaver" |  waterpoint== "mandiseka" | waterpoint =="mfagazaan"   ) %>% 
  mutate(tess_name = waterpoint,meanx = GPS_X,meany= GPS_Y)

pool11<-hey %>% filter (waterpoint == "wexcau" |  waterpoint== "ngamo"  |  waterpoint== "ngamo_2_major"  ) %>% 
  mutate(tess_name = "wexcau",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool12<-hey %>% filter (waterpoint == "sanyati_pans" |  waterpoint== "njekwa"  |  waterpoint== "mahoboti"  |  waterpoint== "dina"  |  waterpoint== "triga_vlei"  ) %>% 
  mutate(tess_name = "njekwa",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool13<-hey %>% filter (waterpoint == "tendele" |  waterpoint== "dadada"  |  waterpoint== "skova"  |  waterpoint== "danga"  |  waterpoint== "nehimba"  |  waterpoint== "nehimba_seep"   |  waterpoint== "nehimba_grannies"   ) %>% 
  mutate(tess_name = "nehimba",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool14<-hey %>% filter (waterpoint == "shapi" |  waterpoint== "giraffe_s"    ) %>% 
  mutate(tess_name = "shapi",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool15<-hey %>% filter (waterpoint == "mopane" ) %>% 
  mutate(tess_name = "mopane",meanx = GPS_X,meany= GPS_Y)

pool16<-hey %>% filter (waterpoint == "bembesi" |  waterpoint== "white_hills"  |  waterpoint== "shallow_grave"   ) %>% 
  mutate(tess_name = "whitehills",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool17to18<-hey %>% filter (waterpoint == "guvalala" |  waterpoint== "mabuya_mabena"  ) %>% 
  mutate(tess_name = c("guvalala", "mabuya"),meanx = GPS_X,meany= GPS_Y)

pool19<-hey %>% filter (waterpoint == "tshompani_pan" |  waterpoint== "tshompani_dam"  ) %>% 
  mutate(tess_name = "tshompani",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool20<-hey %>% filter (waterpoint == "tshakabika"  |  waterpoint== "tshakabika_2" |  waterpoint== "tshakabika_3" ) %>% 
  mutate(tess_name = "tshakabika",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool22<-hey %>% filter (waterpoint == "inyantue"  ) %>% 
  mutate(tess_name = "inyantue",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool23<-hey %>% filter (waterpoint == "baobab" | waterpoint == "kashawe_viewing"| waterpoint == "kashawe_river" ) %>% 
  mutate(tess_name = "baobab",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool24<-hey %>% filter (waterpoint == "salt_spring"  ) %>% 
  mutate(tess_name = "saltspring",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool25<-hey %>% filter (waterpoint == "mandavu" | waterpoint == "mandavu_west" | waterpoint == "mandavu_south" | waterpoint == "mandavu_car_park" ) %>% 
  mutate(tess_name = "mandavu",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool26<-hey %>% filter (waterpoint == "masuma" | waterpoint == "kapula" | waterpoint == "bumbumtsa"  ) %>% 
  mutate(tess_name = "masuma",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool27<-hey %>% filter (waterpoint == "shumba_banana" | waterpoint == "shumba_pan"   | waterpoint == "dwarf_goose" | waterpoint == "roan" ) %>% 
  mutate(tess_name = "shumba",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool28to31<-hey %>% filter (waterpoint == "manga_3" | waterpoint == "manga_2"   | waterpoint == "manga_1" | waterpoint == "jambile" ) %>% 
  mutate(tess_name = waterpoint,meanx = GPS_X,meany= GPS_Y)

pool32<-hey %>% filter (waterpoint == "dopi" | waterpoint == "dopi_nat"  ) %>% 
  mutate(tess_name = "dopi",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool33to35<-hey %>% filter (waterpoint == "nyamandhlovu" | waterpoint == "madundumela"   | waterpoint == "linkwasha" ) %>% 
  mutate(tess_name = waterpoint,meanx = GPS_X,meany= GPS_Y)

pool36<-hey %>% filter (waterpoint == "makololo" | waterpoint == "back_pan"  | waterpoint == "makololo_madison"  ) %>% 
  mutate(tess_name = "makololov",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool38<-hey %>% filter (waterpoint == "ostrich" | waterpoint == "mbiza"  | waterpoint == "little_mbiza" | waterpoint == "kennedy_hide"  ) %>% 
  mutate(tess_name = "mbiza",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool39<-hey %>% filter (waterpoint == "sinanga" | waterpoint == "kennedy_1"   ) %>% 
  mutate(tess_name = "sinanga",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool40to41<-hey %>% filter (waterpoint == "kennedy_2" | waterpoint == "ngweshla"   ) %>% 
  mutate(tess_name = waterpoint,meanx = GPS_X,meany= GPS_Y)

pool42<-hey %>% filter (waterpoint == "broken_rifle" | waterpoint == "samavundhla"  | waterpoint == "little_makololo" | waterpoint == "little_samavundhla"  ) %>% 
  mutate(tess_name = "samavund",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool43to46<-hey %>% filter (waterpoint == "tsamhole" | waterpoint == "reedbuck_vlei" | waterpoint == "nantwich"   ) %>% 
  mutate(tess_name = waterpoint,meanx = GPS_X,meany= GPS_Y)

pool47<-hey %>% filter (waterpoint == "deka_home_vlei" | waterpoint == "deka_camp" ) %>% 
  mutate(tess_name = "dekacamp",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool48<-hey %>% filter (waterpoint == "mahohoma_seep_2" | waterpoint == "deka_little_deka_junction" | waterpoint == "mahohoma_seep_3" |waterpoint == "mahohoma_seep_1"  ) %>% 
  mutate(tess_name = "mahohoma_2",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool49<-hey %>% filter (waterpoint == "robins_camp_wier" | waterpoint == "croc_pools" | waterpoint == "croc_pools_2" ) %>% 
  mutate(tess_name = "robins",meanx = mean(GPS_X),meany= mean(GPS_Y)) 

pool50<-hey %>% filter (waterpoint == "big_toms" | waterpoint == "dandari_1" | waterpoint == "dandari_2" | waterpoint == "dandari_3" | waterpoint == "dandari_4" | waterpoint == "dandari_5") %>% 
  mutate(tess_name = "bigtoms",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool51<-hey %>% filter (waterpoint == "little_toms") %>% 
  mutate(tess_name = "littletoms",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool52<-hey %>% filter (waterpoint == "chingahobe_dam"| waterpoint == "dolilo"| waterpoint == "dombashura_spring"| waterpoint == "salt_spring_2"| waterpoint == "tshowe") %>% 
  mutate(tess_name = "chingahobe",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool53<-hey %>% filter (waterpoint == "bumboosie_4"| waterpoint == "bumboosie_river" ) %>% 
  mutate(tess_name = "bumboo",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool54<-hey %>% filter (waterpoint == "bumboosie_3" | waterpoint == "bumboosie_spring"| waterpoint == "bumboosie_2"|waterpoint == "bumboosie_1" ) %>% 
  mutate(tess_name = "bumboosp1",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool55to56<-hey %>% filter (waterpoint == "makwa" | waterpoint == "mtoa"   ) %>% 
  mutate(tess_name = waterpoint,meanx = GPS_X,meany= GPS_Y)

pool57<-hey %>% filter (waterpoint == "manzichisa" | waterpoint == "salt_pan" ) %>% 
  mutate(tess_name = "saltpan",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool58<-hey %>% filter (waterpoint == "bejane" | waterpoint == "deteema_dam" | waterpoint == "deteema_seep" ) %>% 
  mutate(tess_name = "detepicnic",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool59<-hey %>% filter (waterpoint == "sedina" | waterpoint == "main_camp_cess_pools"  ) %>% 
  mutate(tess_name = "sedina",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool60to61<-hey %>% filter (waterpoint == "umkawazaan" | waterpoint == "dom"  ) %>% 
  mutate(tess_name = c("dom", "umkhawu"),meanx = GPS_X,meany= GPS_Y)

pool62<-hey %>% filter (waterpoint == "tshebe_tshebe" | waterpoint == "ngwenya"  ) %>% 
  mutate(tess_name = "tshebe",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool63<-hey %>% filter (waterpoint == "tshabema" | waterpoint == "kaoshe_pan"  ) %>% 
  mutate(tess_name = "tshabema",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool64<-hey %>% filter (waterpoint == "umtshibi"  ) %>% 
  mutate(tess_name = waterpoint,meanx = mean(GPS_X),meany= mean(GPS_Y))

pool65<-hey %>% filter (waterpoint == "caterpillar" | waterpoint == "dynamite"  ) %>% 
  mutate(tess_name = "caterpillar",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool68<-hey %>% filter (waterpoint == "sibaya" | waterpoint == "livingi"  ) %>% 
  mutate(tess_name = "livingi",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool71<-hey %>% filter (waterpoint == "balla_balla"  ) %>% 
  mutate(tess_name = "balla",meanx = mean(GPS_X),meany= mean(GPS_Y))

pool72 <-hey %>% filter (waterpoint == "mashambo" | waterpoint == "manzimbomvu"  ) %>% 
  mutate(tess_name = "new_mashambo",meanx = mean(GPS_X),meany= mean(GPS_Y))


to_tess<-rbind (pool1, pool2, pool3, pool4, pool5, pool6to10, pool11, pool12, pool13, pool14, 
                pool15, pool16, pool17to18, pool19, pool20, pool22, pool23, pool24, pool25, 
                pool26, pool27, pool28to31, pool32, pool33to35, pool36, pool38, pool39, pool40to41,
                pool42, pool43to46, pool47, pool48, pool49, pool50, pool51, pool52, pool53, 
                pool54, pool55to56, pool57, pool58, pool59, pool60to61, pool62, pool63, pool64, 
                pool65, pool68, pool71, pool72)
