library(tidyverse)
library(baseballr)
library(GeomMLBStadiums)
library(viridis)
library(caret)
library(DescTools)
library(splines)
library(janitor)
library(class)
library(lme4)
library(randomForest)
library(ggrepel)
library(scales)
library(pROC)
library(ROCR)
library(rayshader)
library(gbm)

load("~/Full year Statcast data/2018 Hitting data.RData")
data2018=data
load("~/Full year Statcast data/2019 Hitting data.RData")
data2019=data
load("~/Full year Statcast data/2020 Hitting data.RData")
data2020=season2020
load("~/Full year Statcast data/2021 hitting data.RData")
data2021=data
data=rbind(data2021,data2020,data2019,data2018)
rm(data2018,data2019,data2020,data2021,season2020)

load("~/Weller Stats Lab/Defensive Alignment - Summer Project/knn accuracy.RData")
# spring speed
speed = c()
for(i in 18:21){
  speed1 = read_csv(paste0("sprint_speed_20",i,".csv"))%>%
    select(player_id, sprint_speed)%>%
    mutate(year = paste0(20,i))
  speed = rbind(speed, speed1)
  rm(speed1)
  rm(i)
}

# clean, filter, and plot

data = data%>%mlbam_xy_transformation()%>%
  filter(balls != 4, strikes !=3, hc_y_>0)%>%
  mutate(shift=case_when(
    if_fielding_alignment == "Infield shift" ~ 1,
    if_fielding_alignment %in% c("Strategic", "Standard") ~ 0,
    TRUE ~ NA_real_),
    if_fielding_alignment=case_when(
      if_fielding_alignment == "Infield shift" ~ "Infield shift",
      if_fielding_alignment %in% c("Strategic", "Standard") ~ "Standard",
      TRUE ~ NA_character_),
    hit_angle=atan(hc_x_/(hc_y_))*(180/pi),
    hard_hit=ifelse(launch_speed>=95,1,0),
    hard_hit=as.factor(hard_hit),
    touched=case_when(
      hit_location %in% c(1,2,3,4,5,6) |
        # one double play was hit to outfielder playing in infield and fielders choice were infield errors?
        events %in% c("grounded_into_double_play", "fielders_choice") ~ "Infield",
      hit_location %in% c(7,8,9) ~ "Outfield",
      TRUE ~ NA_character_),
    touched=as.factor(touched), 
    result=case_when(
      events %in% c("single","triple","double") |
        # Outfielders cant get to a ball but runners think they do so they drop and get force out. 
        # There also some missed catches
        (touched == "Outfield" & events == "force_out") ~ "Hit", 
      # Errors are outs BUT they can get deflected so location is different
      events %in% c("field_error") ~ "Out",
      events == "home_run" ~ NA_character_,
      description %in% c("hit_into_play") ~ "Out",
      TRUE ~ NA_character_),
    game_year = as.character(game_year),
    fielding_team = case_when(
      inning_topbot == "Top" ~ home_team,
      inning_topbot == "Bot" ~ away_team,
      TRUE ~ NA_character_),
    on_1b = case_when(
      !is.na(on_1b) ~ "1",
      TRUE ~ "0"),
    on_2b = case_when(
      !is.na(on_2b) ~ "1",
      TRUE ~ "0"),
    on_3b = case_when(
      !is.na(on_3b) ~ "1",
      TRUE ~ "0"),
    on_1b = as.factor(on_1b),
    on_2b = as.factor(on_2b),
    on_3b = as.factor(on_3b))%>%
  left_join(speed, by=c("batter"="player_id", "game_year"="year"))%>%
  separate(player_name, into=c("last","first"), sep=", ", remove=FALSE)%>%
  unite("count", c("balls","strikes"), sep = "-", remove=F)%>%
  drop_na(result, sprint_speed, hit_distance_sc)




# Home parks
data = data%>%
  mutate(park = case_when(
    home_team == "ARI" ~ "diamondbacks",
    home_team == "LAA" ~ "angels",
    home_team == "HOU" ~ "astros",
    home_team == "OAK" ~ "athletics",
    home_team == "TOR" ~ "blue_jays",
    home_team == "ATL" ~ "braves",
    home_team == "MIL" ~ "brewers",
    home_team == "STL" ~ "cardinals",
    home_team == "CHC" ~ "cubs",
    home_team == "LAD" ~ "dodgers",
    home_team == "SF" ~ "giants",
    home_team == "CLE" ~ "indians",
    home_team == "SEA" ~ "mariners",
    home_team == "MIA" ~ "marlins",
    home_team == "NYM" ~ "mets",
    home_team == "WSH" ~ "nationals",
    home_team == "SD" ~ "padres",
    home_team == "PHI" ~ "phillies",
    home_team == "PIT" ~ "pirates",
    home_team == "TEX" ~ "rangers",
    home_team == "TB" ~ "rays",
    home_team == "BOS" ~ "red_sox",
    home_team == "CIN" ~ "reds",
    home_team == "COL" ~ "rockies",
    home_team == "KC" ~ "royals",
    home_team == "DET" ~ "tigers",
    home_team == "MIN" ~ "twins",
    home_team == "CWS" ~ "white_sox",
    home_team == "NYY" ~ "yankees",
    home_team == "BAL" ~ "orioles",
    TRUE ~ "generic"),
    park=as.factor(park))

# THE GOOD STUFF
sample = c()
for(i in 18:21){
  sample1 = read_csv(paste0("20",i,"_position_player_coordinates.csv"))
  sample = rbind(sample, sample1)
  rm(sample1)
  rm(i)
}




sample=sample%>%
  mutate(pos9_start_pos_x = as.numeric(pos9_start_pos_x),
         pos9_start_pos_y = as.numeric(pos9_start_pos_y),
         pos8_start_pos_x = as.numeric(pos8_start_pos_x),
         pos8_start_pos_y = as.numeric(pos8_start_pos_y),
         pos7_start_pos_x = as.numeric(pos7_start_pos_x),
         pos7_start_pos_y = as.numeric(pos7_start_pos_y),
         pos6_start_pos_x = as.numeric(pos6_start_pos_x),
         pos6_start_pos_y = as.numeric(pos6_start_pos_y),
         pos5_start_pos_x = as.numeric(pos5_start_pos_x),
         pos5_start_pos_y = as.numeric(pos5_start_pos_y),
         pos4_start_pos_x = as.numeric(pos4_start_pos_x),
         pos4_start_pos_y = as.numeric(pos4_start_pos_y),
         pos3_start_pos_x = as.numeric(pos3_start_pos_x),
         pos3_start_pos_y = as.numeric(pos3_start_pos_y),
         pos2_start_pos_x = as.numeric(pos2_start_pos_x),
         pos2_start_pos_y = as.numeric(pos2_start_pos_y),
         pos1_start_pos_x = as.numeric(pos1_start_pos_x),
         pos1_start_pos_y = as.numeric(pos1_start_pos_y),
         right_angle=atan(pos9_start_pos_x/(pos9_start_pos_y))*(180/pi),
         center_angle=atan(pos8_start_pos_x/(pos8_start_pos_y))*(180/pi),
         left_angle=atan(pos7_start_pos_x/(pos7_start_pos_y))*(180/pi),
         ss_angle=atan(pos6_start_pos_x/(pos6_start_pos_y))*(180/pi),
         third_angle=atan(pos5_start_pos_x/(pos5_start_pos_y))*(180/pi),
         second_angle=atan(pos4_start_pos_x/(pos4_start_pos_y))*(180/pi),
         first_angle=atan(pos3_start_pos_x/(pos3_start_pos_y))*(180/pi),
         right_distance = sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
         center_distance = sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
         left_distance = sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
         ss_distance = sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
         third_distance = sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
         second_distance = sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
         first_distance = sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
         ss_base_distance_x = pos6_start_pos_x - sin(45*(pi/180))*90,
         ss_base_distance_y = pos6_start_pos_y - cos(45*(pi/180))*90,
         ss_base_distance = sqrt(ss_base_distance_x^2+ss_base_distance_y^2),
         second_base_distance_x = pos4_start_pos_x - sin(45*(pi/180))*90,
         second_base_distance_y = pos4_start_pos_y - cos(45*(pi/180))*90,
         second_base_distance = sqrt(second_base_distance_x^2+second_base_distance_y^2),
         third_base_distance_x = pos5_start_pos_x - sin(45*(pi/180))*90,
         third_base_distance_y = pos5_start_pos_y - cos(45*(pi/180))*90,
         third_base_distance = sqrt(third_base_distance_x^2+third_base_distance_y^2),
         first_base_distance_x = pos3_start_pos_x - sin(45*(pi/180))*90,
         first_base_distance_y = pos3_start_pos_y - cos(45*(pi/180))*90,
         first_base_distance = sqrt(first_base_distance_x^2+first_base_distance_y^2),
         left_base_distance_x = pos7_start_pos_x - sin(45*(pi/180))*90,
         left_base_distance_y = pos7_start_pos_y - cos(45*(pi/180))*90,
         left_base_distance = sqrt(left_base_distance_x^2+left_base_distance_y^2),
         center_base_distance_x = pos8_start_pos_x - sin(45*(pi/180))*90,
         center_base_distance_y = pos8_start_pos_y - cos(45*(pi/180))*90,
         center_base_distance = sqrt(center_base_distance_x^2+center_base_distance_y^2),
         right_base_distance_x = pos9_start_pos_x - sin(45*(pi/180))*90,
         right_base_distance_y = pos9_start_pos_y - cos(45*(pi/180))*90,
         right_base_distance = sqrt(right_base_distance_x^2+right_base_distance_y^2))%>%
  inner_join(data, by=c("game_pk", "player_at_bat"="batter", "pitcher"="pitcher","at_bat_number",
                        "stringer_hit_trajectory"="bb_type", "game_date"))%>%
  drop_na(pos9_start_pos_x)%>%
  mutate(hit_distance = sqrt(hc_x_^2+hc_y_^2))


sample = sample%>%
  filter(hit_angle > (-60) & hit_angle < 60, !hit_location %in% c(1,2), !grepl("foul terr", des))%>%
  mutate(adjusted_hc_x_ = sin(hit_angle*(pi/180))*hit_distance_sc,
         adjusted_hc_y_ = cos(hit_angle*(pi/180))*hit_distance_sc,
         
         ss_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             ss_distance < 230 & ss_base_distance < 180 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | ss_distance > 230 |
             ss_base_distance > 180 ~ hit_distance_sc),
         
         ss_intercept_point_x = sin(hit_angle*(pi/180))*ss_intercept_point_from_home, 
         ss_intercept_point_y = cos(hit_angle*(pi/180))*ss_intercept_point_from_home,
         ss_distance_to_IP_x = ss_intercept_point_x - pos6_start_pos_x,
         ss_distance_to_IP_y = ss_intercept_point_y - pos6_start_pos_y,
         ss_distance_to_intercept = sqrt(ss_distance_to_IP_x^2 + ss_distance_to_IP_y^2),
         
         third_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             third_distance < 230 & third_base_distance < 180 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | third_distance > 230 |
             third_base_distance > 180 ~ hit_distance_sc),
         
         third_intercept_point_x = sin(hit_angle*(pi/180))*third_intercept_point_from_home, 
         third_intercept_point_y = cos(hit_angle*(pi/180))*third_intercept_point_from_home,
         third_distance_to_IP_x = third_intercept_point_x - pos5_start_pos_x,
         third_distance_to_IP_y = third_intercept_point_y - pos5_start_pos_y,
         third_distance_to_intercept = sqrt(third_distance_to_IP_x^2 + third_distance_to_IP_y^2),
         
         second_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             second_distance < 230 & second_base_distance < 180 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | second_distance > 230 |
             second_base_distance > 180 ~ hit_distance_sc),
         
         second_intercept_point_x = sin(hit_angle*(pi/180))*second_intercept_point_from_home, 
         second_intercept_point_y = cos(hit_angle*(pi/180))*second_intercept_point_from_home,
         second_distance_to_IP_x = second_intercept_point_x - pos4_start_pos_x,
         second_distance_to_IP_y = second_intercept_point_y - pos4_start_pos_y,
         second_distance_to_intercept = sqrt(second_distance_to_IP_x^2 + second_distance_to_IP_y^2),
         
         first_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             first_distance < 230 & first_base_distance < 180 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | first_distance > 230 |
             first_base_distance > 180 ~ hit_distance_sc),
         
         first_intercept_point_x = sin(hit_angle*(pi/180))*first_intercept_point_from_home, 
         first_intercept_point_y = cos(hit_angle*(pi/180))*first_intercept_point_from_home,
         first_distance_to_IP_x = first_intercept_point_x - pos3_start_pos_x,
         first_distance_to_IP_y = first_intercept_point_y - pos3_start_pos_y,
         first_distance_to_intercept = sqrt(first_distance_to_IP_x^2 + first_distance_to_IP_y^2),
         
         left_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             left_distance < 230 & left_base_distance < 180 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | left_distance > 230 |
             left_base_distance > 180 ~ hit_distance_sc),
         
         left_intercept_point_x = sin(hit_angle*(pi/180))*left_intercept_point_from_home, 
         left_intercept_point_y = cos(hit_angle*(pi/180))*left_intercept_point_from_home,
         left_distance_to_IP_x = left_intercept_point_x - pos7_start_pos_x,
         left_distance_to_IP_y = left_intercept_point_y - pos7_start_pos_y,
         left_distance_to_intercept = sqrt(left_distance_to_IP_x^2 + left_distance_to_IP_y^2),
         
         center_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             center_distance < 230 & center_base_distance < 180 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | center_distance > 230 |
             center_base_distance > 180 ~ hit_distance_sc),
         
         center_intercept_point_x = sin(hit_angle*(pi/180))*center_intercept_point_from_home, 
         center_intercept_point_y = cos(hit_angle*(pi/180))*center_intercept_point_from_home,
         center_distance_to_IP_x = center_intercept_point_x - pos8_start_pos_x,
         center_distance_to_IP_y = center_intercept_point_y - pos8_start_pos_y,
         center_distance_to_intercept = sqrt(center_distance_to_IP_x^2 + center_distance_to_IP_y^2),
         
         right_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             right_distance < 230 & right_base_distance < 180 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | right_distance > 230 |
             right_base_distance > 180 ~ hit_distance_sc),
         
         right_intercept_point_x = sin(hit_angle*(pi/180))*right_intercept_point_from_home, 
         right_intercept_point_y = cos(hit_angle*(pi/180))*right_intercept_point_from_home,
         right_distance_to_IP_x = right_intercept_point_x - pos9_start_pos_x,
         right_distance_to_IP_y = right_intercept_point_y - pos9_start_pos_y,
         right_distance_to_intercept = sqrt(right_distance_to_IP_x^2 + right_distance_to_IP_y^2),
         # locate fielder responsible for batted ball
         intercept_hit_location = case_when(
           first_distance_to_intercept<ss_distance_to_intercept & 
             first_distance_to_intercept<second_distance_to_intercept &
             first_distance_to_intercept<third_distance_to_intercept &
             first_distance_to_intercept<right_distance_to_intercept & 
             first_distance_to_intercept<center_distance_to_intercept &
             first_distance_to_intercept<right_distance_to_intercept &
             result == "Hit" ~ 3,
           
           second_distance_to_intercept<ss_distance_to_intercept & 
             second_distance_to_intercept<first_distance_to_intercept &
             second_distance_to_intercept<third_distance_to_intercept & 
             second_distance_to_intercept<right_distance_to_intercept & 
             second_distance_to_intercept<left_distance_to_intercept &
             second_distance_to_intercept<center_distance_to_intercept &
             result == "Hit" ~ 4,
           
           ss_distance_to_intercept<first_distance_to_intercept & 
             ss_distance_to_intercept<second_distance_to_intercept &
             ss_distance_to_intercept<third_distance_to_intercept &
             ss_distance_to_intercept<right_distance_to_intercept & 
             ss_distance_to_intercept<left_distance_to_intercept &
             ss_distance_to_intercept<center_distance_to_intercept &
             result == "Hit"  ~ 6,
           
           third_distance_to_intercept<ss_distance_to_intercept & 
             third_distance_to_intercept<second_distance_to_intercept &
             third_distance_to_intercept<first_distance_to_intercept &
             third_distance_to_intercept<right_distance_to_intercept & 
             third_distance_to_intercept<left_distance_to_intercept &
             third_distance_to_intercept<center_distance_to_intercept &
             result == "Hit" ~ 5,
           
           center_distance_to_intercept<ss_distance_to_intercept & 
             center_distance_to_intercept<second_distance_to_intercept &
             center_distance_to_intercept<first_distance_to_intercept &
             center_distance_to_intercept<right_distance_to_intercept & 
             center_distance_to_intercept<left_distance_to_intercept &
             center_distance_to_intercept<third_distance_to_intercept  &
             result == "Hit" ~ 8,
           
           left_distance_to_intercept<ss_distance_to_intercept & 
             left_distance_to_intercept<second_distance_to_intercept &
             left_distance_to_intercept<first_distance_to_intercept &
             left_distance_to_intercept<right_distance_to_intercept & 
             left_distance_to_intercept<third_distance_to_intercept &
             left_distance_to_intercept<center_distance_to_intercept &
             result == "Hit" ~ 7,
           
           right_distance_to_intercept<ss_distance_to_intercept & 
             right_distance_to_intercept<second_distance_to_intercept &
             right_distance_to_intercept<first_distance_to_intercept &
             right_distance_to_intercept<third_distance_to_intercept & 
             right_distance_to_intercept<left_distance_to_intercept &
             right_distance_to_intercept<center_distance_to_intercept &
             result == "Hit" ~ 9,
           
           hit_location == 3 & result == "Out" ~ 3,
           hit_location == 4 & result == "Out" ~ 4,
           hit_location == 5 & result == "Out" ~ 5,
           hit_location == 6 & result == "Out" ~ 6,
           hit_location == 7 & result == "Out" ~ 7,
           hit_location == 8 & result == "Out" ~ 8,
           hit_location == 9 & result == "Out" ~ 9),
         #intercept point on x 
         intercept_point_x = case_when(
           intercept_hit_location == 3 ~ first_intercept_point_x,
           intercept_hit_location == 4 ~ second_intercept_point_x,
           intercept_hit_location == 6 ~ ss_intercept_point_x,
           intercept_hit_location == 5 ~ third_intercept_point_x,
           intercept_hit_location == 7 ~ left_intercept_point_x,
           intercept_hit_location == 8 ~ center_intercept_point_x,
           intercept_hit_location == 9 ~ right_intercept_point_x),
         #intercept point on y
         intercept_point_y = case_when(
           intercept_hit_location == 3 ~ first_intercept_point_y,
           intercept_hit_location == 4 ~ second_intercept_point_y,
           intercept_hit_location == 6 ~ ss_intercept_point_y,
           intercept_hit_location == 5 ~ third_intercept_point_y,
           intercept_hit_location == 7 ~ left_intercept_point_y,
           intercept_hit_location == 8 ~ center_intercept_point_y,
           intercept_hit_location == 9 ~ right_intercept_point_y),
         # intercpet point distance from home
         intercept_point_distance = sqrt(intercept_point_x^2 + intercept_point_y^2), 
         # intercept point angle from home
         intercept_point_angle = hit_angle,
         # intercept fielder angle from home
         intercept_fielder_angle = case_when(
           intercept_hit_location  == 3 ~ atan((pos3_start_pos_x)/
                                                 (pos3_start_pos_y))*(180/pi),
           intercept_hit_location  == 4 ~ atan((pos4_start_pos_x)/
                                                 (pos4_start_pos_y))*(180/pi),
           intercept_hit_location  == 5 ~ atan((pos5_start_pos_x)/
                                                 (pos5_start_pos_y))*(180/pi),
           intercept_hit_location  == 6 ~ atan((pos6_start_pos_x)/
                                                 (pos6_start_pos_y))*(180/pi),
           intercept_hit_location  == 7 ~ atan((pos7_start_pos_x)/
                                                 (pos7_start_pos_y))*(180/pi),
           intercept_hit_location  == 8 ~ atan((pos8_start_pos_x)/
                                                 (pos8_start_pos_y))*(180/pi),
           intercept_hit_location  == 9 ~ atan((pos9_start_pos_x)/
                                                 (pos9_start_pos_y))*(180/pi),
           TRUE ~ NaN),
         # intercept fielder located on x
         intercept_fielder_x = case_when(
           intercept_hit_location == 3 ~ pos3_start_pos_x,
           intercept_hit_location == 4 ~ pos4_start_pos_x,
           intercept_hit_location == 5 ~ pos5_start_pos_x,
           intercept_hit_location == 6 ~ pos6_start_pos_x,
           intercept_hit_location == 7 ~ pos7_start_pos_x,
           intercept_hit_location == 8 ~ pos8_start_pos_x,
           intercept_hit_location == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         # intercept fielder located on y
         intercept_fielder_y = case_when(
           intercept_hit_location == 3 ~ pos3_start_pos_y,
           intercept_hit_location == 4 ~ pos4_start_pos_y,
           intercept_hit_location == 5 ~ pos5_start_pos_y,
           intercept_hit_location == 6 ~ pos6_start_pos_y,
           intercept_hit_location == 7 ~ pos7_start_pos_y,
           intercept_hit_location == 8 ~ pos8_start_pos_y,
           intercept_hit_location == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         # intercept fielder distance from home plate
         intercept_fielder_distance = case_when(
           intercept_hit_location == 3 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
           intercept_hit_location == 4 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
           intercept_hit_location == 5 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
           intercept_hit_location == 6 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
           intercept_hit_location == 7 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
           intercept_hit_location == 8 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
           intercept_hit_location == 9 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on x
         fielder_distance_to_intercept_x = case_when(
           intercept_hit_location == 3 ~ first_distance_to_IP_x,
           intercept_hit_location == 4 ~ second_distance_to_IP_x,
           intercept_hit_location == 5 ~ third_distance_to_IP_x,
           intercept_hit_location == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location == 7 ~ left_distance_to_IP_x,
           intercept_hit_location == 8 ~ center_distance_to_IP_x,
           intercept_hit_location == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on y
         fielder_distance_to_intercept_y = case_when(
           intercept_hit_location == 3 ~ first_distance_to_IP_y,
           intercept_hit_location == 4 ~ second_distance_to_IP_y,
           intercept_hit_location == 5 ~ third_distance_to_IP_y,
           intercept_hit_location == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location == 7 ~ left_distance_to_IP_y,
           intercept_hit_location == 8 ~ center_distance_to_IP_y,
           intercept_hit_location == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
         # where fielder who touched the ball is located on x
         # fielder_x = case_when(
         #   hit_location == 1 ~ pos1_start_pos_x,
         #   hit_location == 2 ~ pos2_start_pos_x,
         #   hit_location == 3 ~ pos3_start_pos_x,
         #   hit_location == 4 ~ pos4_start_pos_x,
         #   hit_location == 5 ~ pos5_start_pos_x,
         #   hit_location == 6 ~ pos6_start_pos_x,
         #   hit_location == 7 ~ pos7_start_pos_x,
         #   hit_location == 8 ~ pos8_start_pos_x,
         #   hit_location == 9 ~ pos9_start_pos_x,
         #   TRUE ~ NaN),
         # # where fielder who touched the ball is located on y
         # fielder_y = case_when(
         #   hit_location == 1 ~ pos1_start_pos_y,
         #   hit_location == 2 ~ pos2_start_pos_y,
         #   hit_location == 3 ~ pos3_start_pos_y,
         #   hit_location == 4 ~ pos4_start_pos_y,
         #   hit_location == 5 ~ pos5_start_pos_y,
         #   hit_location == 6 ~ pos6_start_pos_y,
         #   hit_location == 7 ~ pos7_start_pos_y,
         #   hit_location == 8 ~ pos8_start_pos_y,
         #   hit_location == 9 ~ pos9_start_pos_y,
         #   TRUE ~ NaN),
         # # fielder who touched distance from home
         # fielder_distance = case_when(
         #   hit_location == 1 ~ sqrt(pos1_start_pos_x^2 + pos1_start_pos_y^2),
         #   hit_location == 2 ~ sqrt(pos2_start_pos_x^2 + pos2_start_pos_y^2),
         #   hit_location == 3 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
         #   hit_location == 4 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
         #   hit_location == 5 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
         #   hit_location == 6 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
         #   hit_location == 7 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
         #   hit_location == 8 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
         #   hit_location == 9 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
         #   TRUE ~ NaN),
         # fielder_angle = case_when(
         #   hit_location == 1 ~ atan((pos1_start_pos_x)/
         #                              (pos1_start_pos_y))*(180/pi),
         #   hit_location == 2 ~ atan((pos2_start_pos_x)/
         #                              (pos2_start_pos_y))*(180/pi),
         #   hit_location == 3 ~ atan((pos3_start_pos_x)/
         #                              (pos3_start_pos_y))*(180/pi),
         #   hit_location == 4 ~ atan((pos4_start_pos_x)/
         #                              (pos4_start_pos_y))*(180/pi),
         #   hit_location == 5 ~ atan((pos5_start_pos_x)/
         #                              (pos5_start_pos_y))*(180/pi),
         #   hit_location == 6 ~ atan((pos6_start_pos_x)/
         #                              (pos6_start_pos_y))*(180/pi),
         #   hit_location == 7 ~ atan((pos7_start_pos_x)/
         #                              (pos7_start_pos_y))*(180/pi),
         #   hit_location == 8 ~ atan((pos8_start_pos_x)/
         #                              (pos8_start_pos_y))*(180/pi),
         #   hit_location == 9 ~ atan((pos9_start_pos_x)/
         #                              (pos9_start_pos_y))*(180/pi),
         #   TRUE ~ NaN),
         # # first base distance from fielder who touched it
         # base_distance_x = fielder_x - sin(45*(pi/180))*90,
         # base_distance_y = fielder_y - cos(45*(pi/180))*90,
         # base_distance = sqrt(base_distance_x^2+base_distance_y^2),
         # first base distance from intercept fielder
         intercept_base_distance_x = intercept_fielder_x - sin(45*(pi/180))*90,
         intercept_base_distance_y = intercept_fielder_y - cos(45*(pi/180))*90,
         intercept_base_distance = sqrt(intercept_base_distance_x^2+intercept_base_distance_y^2),
         # first base distance from the intercept point
         intercept_point_base_distance_x = intercept_point_x - sin(45*(pi/180))*90,
         intercept_point_base_distance_y = intercept_point_y - cos(45*(pi/180))*90,
         intercept_point_base_distance = sqrt(intercept_point_base_distance_x^2+intercept_point_base_distance_y^2),
         # fielder angle to intercept point
         fielder_distance_to_intercept = sqrt(fielder_distance_to_intercept_x^2 + fielder_distance_to_intercept_y^2),
         hit_angle_zero = hit_angle - intercept_fielder_angle,
         intercept_fielder_zero_x = sin(0*(pi/180))*intercept_fielder_distance,
         intercept_fielder_zero_y = cos(0*(pi/180))*intercept_fielder_distance,
         intercept_point_zero_x = sin(hit_angle_zero*(pi/180))*intercept_point_distance,
         intercept_point_zero_y = cos(hit_angle_zero*(pi/180))*intercept_point_distance,
         fielder_distance_to_intercept_zero_x = intercept_point_zero_x - intercept_fielder_zero_x,
         fielder_distance_to_intercept_zero_y = intercept_point_zero_y - intercept_fielder_zero_y,
         
         fielder_angle_to_intercept = case_when(
           intercept_point_zero_x < intercept_fielder_zero_x &
             intercept_point_zero_y < intercept_fielder_zero_y ~ atan(abs(fielder_distance_to_intercept_zero_x)/
                                                              (abs(fielder_distance_to_intercept_zero_y)))*(180/pi),
           intercept_point_zero_x < intercept_fielder_zero_x &
             intercept_point_zero_y > intercept_fielder_zero_y ~ 90 + atan(abs(fielder_distance_to_intercept_zero_y)/
                                                                   (abs(fielder_distance_to_intercept_zero_x)))*(180/pi),
           intercept_point_zero_x > intercept_fielder_zero_x &
             intercept_point_zero_y > intercept_fielder_zero_y ~ 180 + atan(abs(fielder_distance_to_intercept_zero_x)/
                                                                    (abs(fielder_distance_to_intercept_zero_y)))*(180/pi),
           intercept_point_zero_x > intercept_fielder_zero_x &
             intercept_point_zero_y < intercept_fielder_zero_y ~ 270 + atan(abs(fielder_distance_to_intercept_zero_y)/
                                                                    (abs(fielder_distance_to_intercept_zero_x)))*(180/pi)),
         fielder_angle_to_intercept_group = case_when(
           fielder_angle_to_intercept >=0 & fielder_angle_to_intercept < 45 ~ 1,
           fielder_angle_to_intercept >=45 & fielder_angle_to_intercept < 90 ~ 2,
           fielder_angle_to_intercept >=90 & fielder_angle_to_intercept < 135 ~ 3,
           fielder_angle_to_intercept >=135 & fielder_angle_to_intercept < 180 ~ 4,
           fielder_angle_to_intercept >=180 & fielder_angle_to_intercept < 225 ~ 5,
           fielder_angle_to_intercept >=225 & fielder_angle_to_intercept < 270 ~ 6,
           fielder_angle_to_intercept >=270 & fielder_angle_to_intercept < 315 ~ 7,
           fielder_angle_to_intercept >=315 & fielder_angle_to_intercept <= 360 ~ 8),
         fielder_angle_to_intercept_group = as.factor(fielder_angle_to_intercept_group))%>%
  
  select(game_date, year, player_name, home_team, park, launch_speed, launch_angle,
         sprint_speed, intercept_hit_location, intercept_point_x, intercept_point_y, intercept_point_distance,
         intercept_point_angle, intercept_fielder_angle, intercept_fielder_distance, intercept_fielder_x,
         intercept_fielder_y, intercept_base_distance_x, intercept_base_distance_y, intercept_point_base_distance,
         intercept_point_base_distance_x, intercept_point_base_distance_y,
         intercept_fielder_zero_x, intercept_fielder_zero_y, intercept_point_zero_x, intercept_point_zero_y,
         intercept_base_distance, hit_angle, hit_location, pos3_start_pos_x,
         pos3_start_pos_y, pos4_start_pos_x, pos4_start_pos_y, pos5_start_pos_x, pos5_start_pos_y, pos6_start_pos_x,
         pos6_start_pos_y, pos7_start_pos_x, pos7_start_pos_y, pos8_start_pos_x, pos8_start_pos_y, pos9_start_pos_x,
         pos9_start_pos_y, fielder_angle_to_intercept, fielder_angle_to_intercept_group,
         fielder_distance_to_intercept, fielder_distance_to_intercept_x, fielder_distance_to_intercept_y,
         hit_distance_sc, hit_distance,des, stringer_hit_trajectory, hc_x_, hc_y_, adjusted_hc_y_,
         adjusted_hc_x_, hit_angle_zero, result, fielder_distance_to_intercept_zero_x, 
         fielder_distance_to_intercept_zero_y, fielder_distance_to_intercept, fielder_distance_to_intercept_x,
         fielder_distance_to_intercept_y, right_angle,center_angle,left_angle,ss_angle,third_angle,second_angle,
         first_angle,right_distance,center_distance,left_distance,ss_distance,third_distance,
         second_distance,first_distance,ss_base_distance_x,ss_base_distance_y,ss_base_distance,
         second_base_distance_x,second_base_distance_y,second_base_distance,third_base_distance_x,
         third_base_distance_y,third_base_distance , first_base_distance_x,first_base_distance_y,
         first_base_distance ,left_base_distance_x,left_base_distance_y,left_base_distance,
         center_base_distance_x,center_base_distance_y, center_base_distance, right_base_distance_x,
         right_base_distance_y,right_base_distance, center_intercept_point_y, center_intercept_point_x,
         right_intercept_point_y, right_intercept_point_x, left_intercept_point_y, left_intercept_point_x,
         ss_intercept_point_y, ss_intercept_point_x, third_intercept_point_y, third_intercept_point_x,
         second_intercept_point_y, second_intercept_point_x, first_intercept_point_x, first_intercept_point_x,
         stand, if_fielding_alignment, of_fielding_alignment)%>%
  mutate(dist=hit_distance_sc - hit_distance,
         Hit = ifelse(result=="Hit","1","0"),
         delta_angle = hit_angle - intercept_fielder_angle,
         delta_distance = intercept_point_distance - intercept_fielder_distance)


gallo2%>%slice(8)
ggplot(gallo%>%slice(74))+
  geom_point(aes(x=intercept_point_x, y=intercept_point_y))+
  geom_point(aes(x=hc_x_, y=hc_y_), size=3, color="forestgreen")+
  geom_point(aes(x=intercept_fielder_x, y=intercept_fielder_y), size=2, color="goldenrod2")+
  geom_point(aes(x=intercept_point_x, y=intercept_point_y))+
  
  geom_point(aes(x=adjusted_hc_x_, y=adjusted_hc_y_), color="red", size=2)+
  
  geom_mlb_stadium(stadium_ids = "rays", stadium_transform_coords = T, stadium_segments = "all")+
  geom_segment(aes(x=intercept_point_x,y=intercept_point_y, xend=intercept_fielder_x, yend=intercept_fielder_y))+
  geom_segment(aes(x=0, y=0, xend=sin(hit_angle*(pi/180))*400, yend=cos(hit_angle*(pi/180))*400),
               linetype=2)


ggplot(gallo2%>%slice(100))+
  geom_point(aes(x=intercept_point_zero_x, y=intercept_point_zero_y), size=4)+
  geom_point(aes(x=hc_x_, y=hc_y_), size=3, color="forestgreen")+
  geom_point(aes(x=intercept_fielder_zero_x, y=intercept_fielder_zero_y))+
  geom_segment(aes(x=intercept_fielder_zero_x,y=intercept_fielder_zero_x, 
                   xend=intercept_point_zero_x, yend=intercept_point_zero_x))+
  geom_segment(aes(x=0, y=0, xend=sin(hit_angle_zero*(pi/180))*400, yend=cos(hit_angle_zero*(pi/180))*400),
               linetype=2)+
  geom_mlb_stadium(stadium_ids = "rays", stadium_transform_coords = T, stadium_segments = "all")


sample%>%filter(fielder_angle_to_intercept_group==8)%>%
  ggplot()+geom_point(aes(x=intercept_point_zero_x, y=intercept_point_zero_y))+
  geom_point(aes(x=intercept_fielder_zero_x, y=intercept_fielder_zero_y), color="blue")+
  geom_mlb_stadium(stadium_ids = "rays", stadium_transform_coords = T, stadium_segments = "all")+
  geom_segment(aes(x=intercept_fielder_zero_x,y=intercept_fielder_zero_y, 
                   xend=intercept_point_zero_x, yend=intercept_point_zero_y), color="red")

# LETS DO IT

# together


  

set.seed(100)
vector = createDataPartition(sample$Hit, p = 0.5, list=F)%>%as.numeric()
train = sample[vector,]
test = sample[-vector,]

model3 = randomForest(Hit ~ launch_speed * launch_angle + sprint_speed +
                       delta_angle_1 + intercept_point_distance_1 +  intercept_fielder_distance_1 +
                       intercept_point_base_distance_1 + intercept_base_distance_1 +
                       fielder_distance_to_intercept_1 +
                       fielder_distance_to_intercept_zero_x_1 *
                       fielder_distance_to_intercept_zero_y_1 * intercept_fielder_x_1 *
                       intercept_fielder_y_1,
                     data = train, ntree=451, nodesize = 20,
                     mtry=4)

model = gbm(Hit ~ launch_speed * launch_angle + sprint_speed +
              delta_angle + intercept_point_distance +  intercept_fielder_distance +
              intercept_point_base_distance + intercept_base_distance +
              fielder_distance_to_intercept +
              fielder_distance_to_intercept_zero_x *
              fielder_distance_to_intercept_zero_y * intercept_fielder_x *
              intercept_fielder_y, data = sample, distribution = "bernoulli", n.trees = 451,
            interaction.depth = 40, cv.folds = 5)

model2 = gbm(Hit ~ launch_speed * launch_angle + sprint_speed +
              delta_angle + intercept_point_distance +  intercept_fielder_distance +
              intercept_point_base_distance + intercept_base_distance +
              fielder_distance_to_intercept +
              fielder_distance_to_intercept_zero_x *
              fielder_distance_to_intercept_zero_y * intercept_fielder_x *
              intercept_fielder_y, data = train, distribution = "bernoulli", n.trees = 451,
            interaction.depth = 40, shrinkage = 0.4)

test%>% # test accuracy
  mutate(pred=predict(model2,test,type="response"),
         Hit=ifelse(Hit==1,1,0),
         pred2=1*(pred>=0.5))%>%
  summarize(accuracy=mean(Hit==pred2, na.rm = T))

test%>% # test accuracy
  mutate(pred=predict(model3, test))%>%
  summarize(accuracy=mean(Hit==pred, na.rm = T))



predict.gbm(model, test, type="response")

varImp(model)%>%arrange(desc(Overall))

rf_pr_test = prediction(predict(model, type="prob", newdata = test)[,2], test$Hit)
r_auc_train1 = performance(rf_pr_test, measure = "auc")@y.values[[1]] 
r_auc_train1

test = test%>% 
  mutate(pred=predict(model, test, type="response")>=0.5,
         pred = 1*pred,
         pred = as.character(pred))

test = test%>% 
  mutate(pred=predict(model3, test))

confusionMatrix(factor(test$pred), factor(test$Hit))

test = test%>% 
  mutate(pred=predict(model, test, type="prob")[,2])

test%>%
  select(game_date, player_name, pred,Hit, launch_speed,launch_angle,intercept_hit_location, intercept_fielder_distance, intercept_base_distance)%>%
  arrange(desc(intercept_base_distance))


sample = sample%>%
  mutate(pred = predict(model, sample, type = "prob")[,2])



###

sample = sample%>%
  mutate(pred = predict(model, sample, type = "prob")[,2])

gallo = sample%>%
  filter(player_name == "Gallo, Joey")

gallo2 = gallo%>%
  mutate(pos9_start_pos_x = mean(sample$pos9_start_pos_x, na.rm=T),
         pos9_start_pos_y = mean(sample$pos9_start_pos_y, na.rm=T),
         pos8_start_pos_x = mean(sample$pos8_start_pos_x, na.rm=T),
         pos8_start_pos_y = mean(sample$pos8_start_pos_y, na.rm=T),
         pos7_start_pos_x = mean(sample$pos7_start_pos_x, na.rm=T),
         pos7_start_pos_y = mean(sample$pos7_start_pos_y, na.rm=T),
         pos6_start_pos_x = mean(sample$pos6_start_pos_x, na.rm=T),
         pos6_start_pos_y = mean(sample$pos6_start_pos_y, na.rm=T),
         pos5_start_pos_x = mean(sample$pos5_start_pos_x, na.rm=T),
         pos5_start_pos_y = mean(sample$pos5_start_pos_y, na.rm=T),
         pos4_start_pos_x = mean(sample$pos4_start_pos_x, na.rm=T),
         pos4_start_pos_y = mean(sample$pos4_start_pos_y, na.rm=T),
         pos3_start_pos_x = mean(sample$pos3_start_pos_x, na.rm=T),
         pos3_start_pos_y = mean(sample$pos3_start_pos_y, na.rm=T),
         right_angle=atan(pos9_start_pos_x/(pos9_start_pos_y))*(180/pi),
         center_angle=atan(pos8_start_pos_x/(pos8_start_pos_y))*(180/pi),
         left_angle=atan(pos7_start_pos_x/(pos7_start_pos_y))*(180/pi),
         ss_angle=atan(pos6_start_pos_x/(pos6_start_pos_y))*(180/pi),
         third_angle=atan(pos5_start_pos_x/(pos5_start_pos_y))*(180/pi),
         second_angle=atan(pos4_start_pos_x/(pos4_start_pos_y))*(180/pi),
         first_angle=atan(pos3_start_pos_x/(pos3_start_pos_y))*(180/pi),
         right_distance = sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
         center_distance = sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
         left_distance = sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
         ss_distance = sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
         third_distance = sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
         second_distance = sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
         first_distance = sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
         ss_base_distance_x = pos6_start_pos_x - sin(45*(pi/180))*90,
         ss_base_distance_y = pos6_start_pos_y - cos(45*(pi/180))*90,
         ss_base_distance = sqrt(ss_base_distance_x^2+ss_base_distance_y^2),
         second_base_distance_x = pos4_start_pos_x - sin(45*(pi/180))*90,
         second_base_distance_y = pos4_start_pos_y - cos(45*(pi/180))*90,
         second_base_distance = sqrt(second_base_distance_x^2+second_base_distance_y^2),
         third_base_distance_x = pos5_start_pos_x - sin(45*(pi/180))*90,
         third_base_distance_y = pos5_start_pos_y - cos(45*(pi/180))*90,
         third_base_distance = sqrt(third_base_distance_x^2+third_base_distance_y^2),
         first_base_distance_x = pos3_start_pos_x - sin(45*(pi/180))*90,
         first_base_distance_y = pos3_start_pos_y - cos(45*(pi/180))*90,
         first_base_distance = sqrt(first_base_distance_x^2+first_base_distance_y^2),
         left_base_distance_x = pos7_start_pos_x - sin(45*(pi/180))*90,
         left_base_distance_y = pos7_start_pos_y - cos(45*(pi/180))*90,
         left_base_distance = sqrt(left_base_distance_x^2+left_base_distance_y^2),
         center_base_distance_x = pos8_start_pos_x - sin(45*(pi/180))*90,
         center_base_distance_y = pos8_start_pos_y - cos(45*(pi/180))*90,
         center_base_distance = sqrt(center_base_distance_x^2+center_base_distance_y^2),
         right_base_distance_x = pos9_start_pos_x - sin(45*(pi/180))*90,
         right_base_distance_y = pos9_start_pos_y - cos(45*(pi/180))*90,
         right_base_distance = sqrt(right_base_distance_x^2+right_base_distance_y^2))%>%
  filter(!hit_location %in% c(1,2), hit_angle > (-60) & hit_angle < 60)


gallo2 = gallo2%>%
  mutate(ss_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             ss_distance < 230 & ss_base_distance < 180 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | ss_distance > 230 |
             ss_base_distance > 180 ~ hit_distance_sc),
         
         ss_intercept_point_x = sin(hit_angle*(pi/180))*ss_intercept_point_from_home, 
         ss_intercept_point_y = cos(hit_angle*(pi/180))*ss_intercept_point_from_home,
         ss_distance_to_IP_x = ss_intercept_point_x - pos6_start_pos_x,
         ss_distance_to_IP_y = ss_intercept_point_y - pos6_start_pos_y,
         ss_distance_to_intercept = sqrt(ss_distance_to_IP_x^2 + ss_distance_to_IP_y^2),
         
         third_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             third_distance < 230 & third_base_distance < 180 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | third_distance > 230 |
             third_base_distance > 180 ~ hit_distance_sc),
         
         third_intercept_point_x = sin(hit_angle*(pi/180))*third_intercept_point_from_home, 
         third_intercept_point_y = cos(hit_angle*(pi/180))*third_intercept_point_from_home,
         third_distance_to_IP_x = third_intercept_point_x - pos5_start_pos_x,
         third_distance_to_IP_y = third_intercept_point_y - pos5_start_pos_y,
         third_distance_to_intercept = sqrt(third_distance_to_IP_x^2 + third_distance_to_IP_y^2),
         
         second_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             second_distance < 230 & second_base_distance < 180 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | second_distance > 230 |
             second_base_distance > 180 ~ hit_distance_sc),
         
         second_intercept_point_x = sin(hit_angle*(pi/180))*second_intercept_point_from_home, 
         second_intercept_point_y = cos(hit_angle*(pi/180))*second_intercept_point_from_home,
         second_distance_to_IP_x = second_intercept_point_x - pos4_start_pos_x,
         second_distance_to_IP_y = second_intercept_point_y - pos4_start_pos_y,
         second_distance_to_intercept = sqrt(second_distance_to_IP_x^2 + second_distance_to_IP_y^2),
         
         first_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             first_distance < 230 & first_base_distance < 180 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | first_distance > 230 |
             first_base_distance > 180 ~ hit_distance_sc),
         
         first_intercept_point_x = sin(hit_angle*(pi/180))*first_intercept_point_from_home, 
         first_intercept_point_y = cos(hit_angle*(pi/180))*first_intercept_point_from_home,
         first_distance_to_IP_x = first_intercept_point_x - pos3_start_pos_x,
         first_distance_to_IP_y = first_intercept_point_y - pos3_start_pos_y,
         first_distance_to_intercept = sqrt(first_distance_to_IP_x^2 + first_distance_to_IP_y^2),
         
         left_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             left_distance < 230 & left_base_distance < 180 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | left_distance > 230 |
             left_base_distance > 180 ~ hit_distance_sc),
         
         left_intercept_point_x = sin(hit_angle*(pi/180))*left_intercept_point_from_home, 
         left_intercept_point_y = cos(hit_angle*(pi/180))*left_intercept_point_from_home,
         left_distance_to_IP_x = left_intercept_point_x - pos7_start_pos_x,
         left_distance_to_IP_y = left_intercept_point_y - pos7_start_pos_y,
         left_distance_to_intercept = sqrt(left_distance_to_IP_x^2 + left_distance_to_IP_y^2),
         
         center_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             center_distance < 230 & center_base_distance < 180 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | center_distance > 230 |
             center_base_distance > 180 ~ hit_distance_sc),
         
         center_intercept_point_x = sin(hit_angle*(pi/180))*center_intercept_point_from_home, 
         center_intercept_point_y = cos(hit_angle*(pi/180))*center_intercept_point_from_home,
         center_distance_to_IP_x = center_intercept_point_x - pos8_start_pos_x,
         center_distance_to_IP_y = center_intercept_point_y - pos8_start_pos_y,
         center_distance_to_intercept = sqrt(center_distance_to_IP_x^2 + center_distance_to_IP_y^2),
         
         right_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             right_distance < 230 & right_base_distance < 180 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | right_distance > 230 |
             right_base_distance > 180 ~ hit_distance_sc),
         
         right_intercept_point_x = sin(hit_angle*(pi/180))*right_intercept_point_from_home, 
         right_intercept_point_y = cos(hit_angle*(pi/180))*right_intercept_point_from_home,
         right_distance_to_IP_x = right_intercept_point_x - pos9_start_pos_x,
         right_distance_to_IP_y = right_intercept_point_y - pos9_start_pos_y,
         right_distance_to_intercept = sqrt(right_distance_to_IP_x^2 + right_distance_to_IP_y^2),
         # locate fielder responsible for batted ball
         intercept_hit_location = case_when(
           first_distance_to_intercept<ss_distance_to_intercept & 
             first_distance_to_intercept<second_distance_to_intercept &
             first_distance_to_intercept<third_distance_to_intercept &
             first_distance_to_intercept<right_distance_to_intercept & 
             first_distance_to_intercept<center_distance_to_intercept &
             first_distance_to_intercept<right_distance_to_intercept ~ 3,
           
           second_distance_to_intercept<ss_distance_to_intercept & 
             second_distance_to_intercept<first_distance_to_intercept &
             second_distance_to_intercept<third_distance_to_intercept & 
             second_distance_to_intercept<right_distance_to_intercept & 
             second_distance_to_intercept<left_distance_to_intercept &
             second_distance_to_intercept<center_distance_to_intercept ~ 4,
           
           ss_distance_to_intercept<first_distance_to_intercept & 
             ss_distance_to_intercept<second_distance_to_intercept &
             ss_distance_to_intercept<third_distance_to_intercept &
             ss_distance_to_intercept<right_distance_to_intercept & 
             ss_distance_to_intercept<left_distance_to_intercept &
             ss_distance_to_intercept<center_distance_to_intercept ~ 6,
           
           third_distance_to_intercept<ss_distance_to_intercept & 
             third_distance_to_intercept<second_distance_to_intercept &
             third_distance_to_intercept<first_distance_to_intercept &
             third_distance_to_intercept<right_distance_to_intercept & 
             third_distance_to_intercept<left_distance_to_intercept &
             third_distance_to_intercept<center_distance_to_intercept ~ 5,
           
           center_distance_to_intercept<ss_distance_to_intercept & 
             center_distance_to_intercept<second_distance_to_intercept &
             center_distance_to_intercept<first_distance_to_intercept &
             center_distance_to_intercept<right_distance_to_intercept & 
             center_distance_to_intercept<left_distance_to_intercept &
             center_distance_to_intercept<third_distance_to_intercept ~ 8,
           
           left_distance_to_intercept<ss_distance_to_intercept & 
             left_distance_to_intercept<second_distance_to_intercept &
             left_distance_to_intercept<first_distance_to_intercept &
             left_distance_to_intercept<right_distance_to_intercept & 
             left_distance_to_intercept<third_distance_to_intercept &
             left_distance_to_intercept<center_distance_to_intercept ~ 7,
           
           right_distance_to_intercept<ss_distance_to_intercept & 
             right_distance_to_intercept<second_distance_to_intercept &
             right_distance_to_intercept<first_distance_to_intercept &
             right_distance_to_intercept<third_distance_to_intercept & 
             right_distance_to_intercept<left_distance_to_intercept &
             right_distance_to_intercept<center_distance_to_intercept ~ 9),
         #intercept point on x 
         intercept_point_x = case_when(
           intercept_hit_location == 3 ~ first_intercept_point_x,
           intercept_hit_location == 4 ~ second_intercept_point_x,
           intercept_hit_location == 6 ~ ss_intercept_point_x,
           intercept_hit_location == 5 ~ third_intercept_point_x,
           intercept_hit_location == 7 ~ left_intercept_point_x,
           intercept_hit_location == 8 ~ center_intercept_point_x,
           intercept_hit_location == 9 ~ right_intercept_point_x),
         #intercept point on y
         intercept_point_y = case_when(
           intercept_hit_location == 3 ~ first_intercept_point_y,
           intercept_hit_location == 4 ~ second_intercept_point_y,
           intercept_hit_location == 6 ~ ss_intercept_point_y,
           intercept_hit_location == 5 ~ third_intercept_point_y,
           intercept_hit_location == 7 ~ left_intercept_point_y,
           intercept_hit_location == 8 ~ center_intercept_point_y,
           intercept_hit_location == 9 ~ right_intercept_point_y),
         # intercpet point distance from home
         intercept_point_distance = sqrt(intercept_point_x^2 + intercept_point_y^2), 
         # intercept point angle from home
         intercept_point_angle = hit_angle,
         # intercept fielder angle from home
         intercept_fielder_angle = case_when(
           intercept_hit_location  == 3 ~ atan((pos3_start_pos_x)/
                                                 (pos3_start_pos_y))*(180/pi),
           intercept_hit_location  == 4 ~ atan((pos4_start_pos_x)/
                                                 (pos4_start_pos_y))*(180/pi),
           intercept_hit_location  == 5 ~ atan((pos5_start_pos_x)/
                                                 (pos5_start_pos_y))*(180/pi),
           intercept_hit_location  == 6 ~ atan((pos6_start_pos_x)/
                                                 (pos6_start_pos_y))*(180/pi),
           intercept_hit_location  == 7 ~ atan((pos7_start_pos_x)/
                                                 (pos7_start_pos_y))*(180/pi),
           intercept_hit_location  == 8 ~ atan((pos8_start_pos_x)/
                                                 (pos8_start_pos_y))*(180/pi),
           intercept_hit_location  == 9 ~ atan((pos9_start_pos_x)/
                                                 (pos9_start_pos_y))*(180/pi),
           TRUE ~ NaN),
         # intercept fielder located on x
         intercept_fielder_x = case_when(
           intercept_hit_location == 3 ~ pos3_start_pos_x,
           intercept_hit_location == 4 ~ pos4_start_pos_x,
           intercept_hit_location == 5 ~ pos5_start_pos_x,
           intercept_hit_location == 6 ~ pos6_start_pos_x,
           intercept_hit_location == 7 ~ pos7_start_pos_x,
           intercept_hit_location == 8 ~ pos8_start_pos_x,
           intercept_hit_location == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         # intercept fielder located on y
         intercept_fielder_y = case_when(
           intercept_hit_location == 3 ~ pos3_start_pos_y,
           intercept_hit_location == 4 ~ pos4_start_pos_y,
           intercept_hit_location == 5 ~ pos5_start_pos_y,
           intercept_hit_location == 6 ~ pos6_start_pos_y,
           intercept_hit_location == 7 ~ pos7_start_pos_y,
           intercept_hit_location == 8 ~ pos8_start_pos_y,
           intercept_hit_location == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         # intercept fielder distance from home plate
         intercept_fielder_distance = case_when(
           intercept_hit_location == 3 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
           intercept_hit_location == 4 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
           intercept_hit_location == 5 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
           intercept_hit_location == 6 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
           intercept_hit_location == 7 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
           intercept_hit_location == 8 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
           intercept_hit_location == 9 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on x
         fielder_distance_to_intercept_x = case_when(
           intercept_hit_location == 3 ~ first_distance_to_IP_x,
           intercept_hit_location == 4 ~ second_distance_to_IP_x,
           intercept_hit_location == 5 ~ third_distance_to_IP_x,
           intercept_hit_location == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location == 7 ~ left_distance_to_IP_x,
           intercept_hit_location == 8 ~ center_distance_to_IP_x,
           intercept_hit_location == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on y
         fielder_distance_to_intercept_y = case_when(
           intercept_hit_location == 3 ~ first_distance_to_IP_y,
           intercept_hit_location == 4 ~ second_distance_to_IP_y,
           intercept_hit_location == 5 ~ third_distance_to_IP_y,
           intercept_hit_location == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location == 7 ~ left_distance_to_IP_y,
           intercept_hit_location == 8 ~ center_distance_to_IP_y,
           intercept_hit_location == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
         # first base distance from intercept fielder
         intercept_base_distance_x = intercept_fielder_x - sin(45*(pi/180))*90,
         intercept_base_distance_y = intercept_fielder_y - cos(45*(pi/180))*90,
         intercept_base_distance = sqrt(intercept_base_distance_x^2+intercept_base_distance_y^2),
         # first base distance from the intercept point
         intercept_point_base_distance_x = intercept_point_x - sin(45*(pi/180))*90,
         intercept_point_base_distance_y = intercept_point_y - cos(45*(pi/180))*90,
         intercept_point_base_distance = sqrt(intercept_point_base_distance_x^2+intercept_point_base_distance_y^2),
         # fielder angle to intercept point
         fielder_distance_to_intercept = sqrt(fielder_distance_to_intercept_x^2 + fielder_distance_to_intercept_y^2),
         hit_angle_zero = hit_angle - intercept_fielder_angle,
         intercept_fielder_zero_x = sin(0*(pi/180))*intercept_fielder_distance,
         intercept_fielder_zero_y = cos(0*(pi/180))*intercept_fielder_distance,
         intercept_point_zero_x = sin(hit_angle_zero*(pi/180))*intercept_point_distance,
         intercept_point_zero_y = cos(hit_angle_zero*(pi/180))*intercept_point_distance,
         fielder_distance_to_intercept_zero_x = intercept_point_zero_x - intercept_fielder_zero_x,
         fielder_distance_to_intercept_zero_y = intercept_point_zero_y - intercept_fielder_zero_y,
         
         fielder_angle_to_intercept = case_when(
           intercept_point_zero_x < intercept_fielder_zero_x &
             intercept_point_zero_y < intercept_fielder_zero_y ~ atan(abs(fielder_distance_to_intercept_zero_x)/
                                                                        (abs(fielder_distance_to_intercept_zero_y)))*(180/pi),
           intercept_point_zero_x < intercept_fielder_zero_x &
             intercept_point_zero_y > intercept_fielder_zero_y ~ 90 + atan(abs(fielder_distance_to_intercept_zero_y)/
                                                                             (abs(fielder_distance_to_intercept_zero_x)))*(180/pi),
           intercept_point_zero_x > intercept_fielder_zero_x &
             intercept_point_zero_y > intercept_fielder_zero_y ~ 180 + atan(abs(fielder_distance_to_intercept_zero_x)/
                                                                              (abs(fielder_distance_to_intercept_zero_y)))*(180/pi),
           intercept_point_zero_x > intercept_fielder_zero_x &
             intercept_point_zero_y < intercept_fielder_zero_y ~ 270 + atan(abs(fielder_distance_to_intercept_zero_y)/
                                                                              (abs(fielder_distance_to_intercept_zero_x)))*(180/pi)),
         fielder_angle_to_intercept_group = case_when(
           fielder_angle_to_intercept >=0 & fielder_angle_to_intercept < 45 ~ 1,
           fielder_angle_to_intercept >=45 & fielder_angle_to_intercept < 90 ~ 2,
           fielder_angle_to_intercept >=90 & fielder_angle_to_intercept < 135 ~ 3,
           fielder_angle_to_intercept >=135 & fielder_angle_to_intercept < 180 ~ 4,
           fielder_angle_to_intercept >=180 & fielder_angle_to_intercept < 225 ~ 5,
           fielder_angle_to_intercept >=225 & fielder_angle_to_intercept < 270 ~ 6,
           fielder_angle_to_intercept >=270 & fielder_angle_to_intercept < 315 ~ 7,
           fielder_angle_to_intercept >=315 & fielder_angle_to_intercept <= 360 ~ 8),
         fielder_angle_to_intercept_group = as.factor(fielder_angle_to_intercept_group))%>%
  
  select(game_date, year, player_name, home_team, park, launch_speed, launch_angle,
         sprint_speed, intercept_hit_location, intercept_point_x, intercept_point_y, intercept_point_distance,
         intercept_point_angle, intercept_fielder_angle, intercept_fielder_distance, intercept_fielder_x,
         intercept_fielder_y, intercept_base_distance_x, intercept_base_distance_y, intercept_point_base_distance,
         intercept_point_base_distance_x, intercept_point_base_distance_y,
         intercept_fielder_zero_x, intercept_fielder_zero_y, intercept_point_zero_x, intercept_point_zero_y,
         intercept_base_distance, hit_angle, hit_location, pos3_start_pos_x,
         pos3_start_pos_y, pos4_start_pos_x, pos4_start_pos_y, pos5_start_pos_x, pos5_start_pos_y, pos6_start_pos_x,
         pos6_start_pos_y, pos7_start_pos_x, pos7_start_pos_y, pos8_start_pos_x, pos8_start_pos_y, pos9_start_pos_x,
         pos9_start_pos_y, fielder_angle_to_intercept, fielder_angle_to_intercept_group,
         fielder_distance_to_intercept, fielder_distance_to_intercept_x, fielder_distance_to_intercept_y,
         hit_distance_sc, hit_distance,des, stringer_hit_trajectory, hc_x_, hc_y_, adjusted_hc_y_,
         adjusted_hc_x_, hit_angle_zero, result, fielder_distance_to_intercept_zero_x, 
         fielder_distance_to_intercept_zero_y, fielder_distance_to_intercept, fielder_distance_to_intercept_x,
         fielder_distance_to_intercept_y, right_angle,center_angle,left_angle,ss_angle,third_angle,second_angle,
         first_angle,right_distance,center_distance,left_distance,ss_distance,third_distance,
         second_distance,first_distance,ss_base_distance_x,ss_base_distance_y,ss_base_distance,
         second_base_distance_x,second_base_distance_y,second_base_distance,third_base_distance_x,
         third_base_distance_y,third_base_distance , first_base_distance_x,first_base_distance_y,
         first_base_distance ,left_base_distance_x,left_base_distance_y,left_base_distance,
         center_base_distance_x,center_base_distance_y, center_base_distance, right_base_distance_x,
         right_base_distance_y,right_base_distance)%>%
  mutate(dist=hit_distance_sc - hit_distance,
       Hit = factor(ifelse(result=="Hit",1,0)),
       delta_angle = hit_angle - intercept_fielder_angle,
       delta_distance = intercept_point_distance - intercept_fielder_distance)

gallo2 = gallo2%>%
  mutate(pred = predict(model3, gallo2, type = "prob")[,2])

probs2=rep(0,10000)

for(i in 1:10000){
  probs2[i]=mean(rbinom(nrow(gallo2),1,gallo2$pred), na.rm=T)
}

ggplot()+
  geom_histogram(aes(x = probs2), fill="blue", alpha = 0.5)


gallo3 = sample%>%
  filter(player_name == "Gallo, Joey")%>%
  mutate(pos9_start_pos_x = mean(gallo$pos9_start_pos_x, na.rm=T),
         pos9_start_pos_y = mean(gallo$pos9_start_pos_y, na.rm=T),
         pos8_start_pos_x = mean(gallo$pos8_start_pos_x, na.rm=T),
         pos8_start_pos_y = mean(gallo$pos8_start_pos_y, na.rm=T),
         pos7_start_pos_x = mean(gallo$pos7_start_pos_x, na.rm=T),
         pos7_start_pos_y = mean(gallo$pos7_start_pos_y, na.rm=T),
         pos6_start_pos_x = mean(gallo$pos6_start_pos_x, na.rm=T),
         pos6_start_pos_y = mean(gallo$pos6_start_pos_y, na.rm=T),
         pos5_start_pos_x = mean(gallo$pos5_start_pos_x, na.rm=T),
         pos5_start_pos_y = mean(gallo$pos5_start_pos_y, na.rm=T),
         pos4_start_pos_x = mean(gallo$pos4_start_pos_x, na.rm=T),
         pos4_start_pos_y = mean(gallo$pos4_start_pos_y, na.rm=T),
         pos3_start_pos_x = mean(gallo$pos3_start_pos_x, na.rm=T),
         pos3_start_pos_y = mean(gallo$pos3_start_pos_y, na.rm=T),
         right_angle=atan(pos9_start_pos_x/(pos9_start_pos_y))*(180/pi),
         center_angle=atan(pos8_start_pos_x/(pos8_start_pos_y))*(180/pi),
         left_angle=atan(pos7_start_pos_x/(pos7_start_pos_y))*(180/pi),
         ss_angle=atan(pos6_start_pos_x/(pos6_start_pos_y))*(180/pi),
         third_angle=atan(pos5_start_pos_x/(pos5_start_pos_y))*(180/pi),
         second_angle=atan(pos4_start_pos_x/(pos4_start_pos_y))*(180/pi),
         first_angle=atan(pos3_start_pos_x/(pos3_start_pos_y))*(180/pi),
         right_distance = sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
         center_distance = sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
         left_distance = sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
         ss_distance = sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
         third_distance = sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
         second_distance = sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
         first_distance = sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
         ss_base_distance_x = pos6_start_pos_x - sin(45*(pi/180))*90,
         ss_base_distance_y = pos6_start_pos_y - cos(45*(pi/180))*90,
         ss_base_distance = sqrt(ss_base_distance_x^2+ss_base_distance_y^2),
         second_base_distance_x = pos4_start_pos_x - sin(45*(pi/180))*90,
         second_base_distance_y = pos4_start_pos_y - cos(45*(pi/180))*90,
         second_base_distance = sqrt(second_base_distance_x^2+second_base_distance_y^2),
         third_base_distance_x = pos5_start_pos_x - sin(45*(pi/180))*90,
         third_base_distance_y = pos5_start_pos_y - cos(45*(pi/180))*90,
         third_base_distance = sqrt(third_base_distance_x^2+third_base_distance_y^2),
         first_base_distance_x = pos3_start_pos_x - sin(45*(pi/180))*90,
         first_base_distance_y = pos3_start_pos_y - cos(45*(pi/180))*90,
         first_base_distance = sqrt(first_base_distance_x^2+first_base_distance_y^2),
         left_base_distance_x = pos7_start_pos_x - sin(45*(pi/180))*90,
         left_base_distance_y = pos7_start_pos_y - cos(45*(pi/180))*90,
         left_base_distance = sqrt(left_base_distance_x^2+left_base_distance_y^2),
         center_base_distance_x = pos8_start_pos_x - sin(45*(pi/180))*90,
         center_base_distance_y = pos8_start_pos_y - cos(45*(pi/180))*90,
         center_base_distance = sqrt(center_base_distance_x^2+center_base_distance_y^2),
         right_base_distance_x = pos9_start_pos_x - sin(45*(pi/180))*90,
         right_base_distance_y = pos9_start_pos_y - cos(45*(pi/180))*90,
         right_base_distance = sqrt(right_base_distance_x^2+right_base_distance_y^2))%>%
  filter(!hit_location %in% c(1,2), hit_angle > (-60) & hit_angle < 60)


gallo3 = gallo3%>%
  filter(!grepl("foul terr", des))%>%
  mutate(ss_intercept_point_from_home = case_when(
    hit_distance_sc <= sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) &
      (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
      ss_distance < 230 & ss_base_distance < 180 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
    
    hit_distance_sc > sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) | 
      stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | ss_distance > 230 |
      ss_base_distance > 180 ~ hit_distance_sc),
    
    ss_intercept_point_x = sin(hit_angle*(pi/180))*ss_intercept_point_from_home, 
    ss_intercept_point_y = cos(hit_angle*(pi/180))*ss_intercept_point_from_home,
    ss_distance_to_IP_x = ss_intercept_point_x - pos6_start_pos_x,
    ss_distance_to_IP_y = ss_intercept_point_y - pos6_start_pos_y,
    ss_distance_to_intercept = sqrt(ss_distance_to_IP_x^2 + ss_distance_to_IP_y^2),
    
    third_intercept_point_from_home = case_when(
      hit_distance_sc <= sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2) &
        (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
        third_distance < 230 & third_base_distance < 180 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
      
      hit_distance_sc > sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2) | 
        stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | third_distance > 230 |
        third_base_distance > 180 ~ hit_distance_sc),
    
    third_intercept_point_x = sin(hit_angle*(pi/180))*third_intercept_point_from_home, 
    third_intercept_point_y = cos(hit_angle*(pi/180))*third_intercept_point_from_home,
    third_distance_to_IP_x = third_intercept_point_x - pos5_start_pos_x,
    third_distance_to_IP_y = third_intercept_point_y - pos5_start_pos_y,
    third_distance_to_intercept = sqrt(third_distance_to_IP_x^2 + third_distance_to_IP_y^2),
    
    second_intercept_point_from_home = case_when(
      hit_distance_sc <= sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2) &
        (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
        second_distance < 230 & second_base_distance < 180 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
      
      hit_distance_sc > sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2) | 
        stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | second_distance > 230 |
        second_base_distance > 180 ~ hit_distance_sc),
    
    second_intercept_point_x = sin(hit_angle*(pi/180))*second_intercept_point_from_home, 
    second_intercept_point_y = cos(hit_angle*(pi/180))*second_intercept_point_from_home,
    second_distance_to_IP_x = second_intercept_point_x - pos4_start_pos_x,
    second_distance_to_IP_y = second_intercept_point_y - pos4_start_pos_y,
    second_distance_to_intercept = sqrt(second_distance_to_IP_x^2 + second_distance_to_IP_y^2),
    
    first_intercept_point_from_home = case_when(
      hit_distance_sc <= sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2) &
        (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
        first_distance < 230 & first_base_distance < 180 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
      
      hit_distance_sc > sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2) | 
        stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | first_distance > 230 |
        first_base_distance > 180 ~ hit_distance_sc),
    
    first_intercept_point_x = sin(hit_angle*(pi/180))*first_intercept_point_from_home, 
    first_intercept_point_y = cos(hit_angle*(pi/180))*first_intercept_point_from_home,
    first_distance_to_IP_x = first_intercept_point_x - pos3_start_pos_x,
    first_distance_to_IP_y = first_intercept_point_y - pos3_start_pos_y,
    first_distance_to_intercept = sqrt(first_distance_to_IP_x^2 + first_distance_to_IP_y^2),
    
    left_intercept_point_from_home = case_when(
      hit_distance_sc <= sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2) &
        (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
        left_distance < 230 & left_base_distance < 180 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
      
      hit_distance_sc > sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2) | 
        stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | left_distance > 230 |
        left_base_distance > 180 ~ hit_distance_sc),
    
    left_intercept_point_x = sin(hit_angle*(pi/180))*left_intercept_point_from_home, 
    left_intercept_point_y = cos(hit_angle*(pi/180))*left_intercept_point_from_home,
    left_distance_to_IP_x = left_intercept_point_x - pos7_start_pos_x,
    left_distance_to_IP_y = left_intercept_point_y - pos7_start_pos_y,
    left_distance_to_intercept = sqrt(left_distance_to_IP_x^2 + left_distance_to_IP_y^2),
    
    center_intercept_point_from_home = case_when(
      hit_distance_sc <= sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2) &
        (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
        center_distance < 230 & center_base_distance < 180 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
      
      hit_distance_sc > sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2) | 
        stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | center_distance > 230 |
        center_base_distance > 180 ~ hit_distance_sc),
    
    center_intercept_point_x = sin(hit_angle*(pi/180))*center_intercept_point_from_home, 
    center_intercept_point_y = cos(hit_angle*(pi/180))*center_intercept_point_from_home,
    center_distance_to_IP_x = center_intercept_point_x - pos8_start_pos_x,
    center_distance_to_IP_y = center_intercept_point_y - pos8_start_pos_y,
    center_distance_to_intercept = sqrt(center_distance_to_IP_x^2 + center_distance_to_IP_y^2),
    
    right_intercept_point_from_home = case_when(
      hit_distance_sc <= sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2) &
        (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
        right_distance < 230 & right_base_distance < 180 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
      
      hit_distance_sc > sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2) | 
        stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | right_distance > 230 |
        right_base_distance > 180 ~ hit_distance_sc),
    
    right_intercept_point_x = sin(hit_angle*(pi/180))*right_intercept_point_from_home, 
    right_intercept_point_y = cos(hit_angle*(pi/180))*right_intercept_point_from_home,
    right_distance_to_IP_x = right_intercept_point_x - pos9_start_pos_x,
    right_distance_to_IP_y = right_intercept_point_y - pos9_start_pos_y,
    right_distance_to_intercept = sqrt(right_distance_to_IP_x^2 + right_distance_to_IP_y^2),
    # locate fielder responsible for batted ball
    intercept_hit_location = case_when(
      first_distance_to_intercept<ss_distance_to_intercept & 
        first_distance_to_intercept<second_distance_to_intercept &
        first_distance_to_intercept<third_distance_to_intercept &
        first_distance_to_intercept<right_distance_to_intercept & 
        first_distance_to_intercept<center_distance_to_intercept &
        first_distance_to_intercept<right_distance_to_intercept ~ 3,
      
      second_distance_to_intercept<ss_distance_to_intercept & 
        second_distance_to_intercept<first_distance_to_intercept &
        second_distance_to_intercept<third_distance_to_intercept & 
        second_distance_to_intercept<right_distance_to_intercept & 
        second_distance_to_intercept<left_distance_to_intercept &
        second_distance_to_intercept<center_distance_to_intercept ~ 4,
      
      ss_distance_to_intercept<first_distance_to_intercept & 
        ss_distance_to_intercept<second_distance_to_intercept &
        ss_distance_to_intercept<third_distance_to_intercept &
        ss_distance_to_intercept<right_distance_to_intercept & 
        ss_distance_to_intercept<left_distance_to_intercept &
        ss_distance_to_intercept<center_distance_to_intercept ~ 6,
      
      third_distance_to_intercept<ss_distance_to_intercept & 
        third_distance_to_intercept<second_distance_to_intercept &
        third_distance_to_intercept<first_distance_to_intercept &
        third_distance_to_intercept<right_distance_to_intercept & 
        third_distance_to_intercept<left_distance_to_intercept &
        third_distance_to_intercept<center_distance_to_intercept ~ 5,
      
      center_distance_to_intercept<ss_distance_to_intercept & 
        center_distance_to_intercept<second_distance_to_intercept &
        center_distance_to_intercept<first_distance_to_intercept &
        center_distance_to_intercept<right_distance_to_intercept & 
        center_distance_to_intercept<left_distance_to_intercept &
        center_distance_to_intercept<third_distance_to_intercept ~ 8,
      
      left_distance_to_intercept<ss_distance_to_intercept & 
        left_distance_to_intercept<second_distance_to_intercept &
        left_distance_to_intercept<first_distance_to_intercept &
        left_distance_to_intercept<right_distance_to_intercept & 
        left_distance_to_intercept<third_distance_to_intercept &
        left_distance_to_intercept<center_distance_to_intercept  ~ 7,
      
      right_distance_to_intercept<ss_distance_to_intercept & 
        right_distance_to_intercept<second_distance_to_intercept &
        right_distance_to_intercept<first_distance_to_intercept &
        right_distance_to_intercept<third_distance_to_intercept & 
        right_distance_to_intercept<left_distance_to_intercept &
        right_distance_to_intercept<center_distance_to_intercept ~ 9),
    #intercept point on x 
    intercept_point_x = case_when(
      intercept_hit_location == 3 ~ first_intercept_point_x,
      intercept_hit_location == 4 ~ second_intercept_point_x,
      intercept_hit_location == 6 ~ ss_intercept_point_x,
      intercept_hit_location == 5 ~ third_intercept_point_x,
      intercept_hit_location == 7 ~ left_intercept_point_x,
      intercept_hit_location == 8 ~ center_intercept_point_x,
      intercept_hit_location == 9 ~ right_intercept_point_x),
    #intercept point on y
    intercept_point_y = case_when(
      intercept_hit_location == 3 ~ first_intercept_point_y,
      intercept_hit_location == 4 ~ second_intercept_point_y,
      intercept_hit_location == 6 ~ ss_intercept_point_y,
      intercept_hit_location == 5 ~ third_intercept_point_y,
      intercept_hit_location == 7 ~ left_intercept_point_y,
      intercept_hit_location == 8 ~ center_intercept_point_y,
      intercept_hit_location == 9 ~ right_intercept_point_y),
    # intercpet point distance from home
    intercept_point_distance = sqrt(intercept_point_x^2 + intercept_point_y^2), 
    # intercept point angle from home
    intercept_point_angle = hit_angle,
    # intercept fielder angle from home
    intercept_fielder_angle = case_when(
      intercept_hit_location  == 3 ~ atan((pos3_start_pos_x)/
                                            (pos3_start_pos_y))*(180/pi),
      intercept_hit_location  == 4 ~ atan((pos4_start_pos_x)/
                                            (pos4_start_pos_y))*(180/pi),
      intercept_hit_location  == 5 ~ atan((pos5_start_pos_x)/
                                            (pos5_start_pos_y))*(180/pi),
      intercept_hit_location  == 6 ~ atan((pos6_start_pos_x)/
                                            (pos6_start_pos_y))*(180/pi),
      intercept_hit_location  == 7 ~ atan((pos7_start_pos_x)/
                                            (pos7_start_pos_y))*(180/pi),
      intercept_hit_location  == 8 ~ atan((pos8_start_pos_x)/
                                            (pos8_start_pos_y))*(180/pi),
      intercept_hit_location  == 9 ~ atan((pos9_start_pos_x)/
                                            (pos9_start_pos_y))*(180/pi),
      TRUE ~ NaN),
    # intercept fielder located on x
    intercept_fielder_x = case_when(
      intercept_hit_location == 3 ~ pos3_start_pos_x,
      intercept_hit_location == 4 ~ pos4_start_pos_x,
      intercept_hit_location == 5 ~ pos5_start_pos_x,
      intercept_hit_location == 6 ~ pos6_start_pos_x,
      intercept_hit_location == 7 ~ pos7_start_pos_x,
      intercept_hit_location == 8 ~ pos8_start_pos_x,
      intercept_hit_location == 9 ~ pos9_start_pos_x,
      TRUE ~ NaN),
    # intercept fielder located on y
    intercept_fielder_y = case_when(
      intercept_hit_location == 3 ~ pos3_start_pos_y,
      intercept_hit_location == 4 ~ pos4_start_pos_y,
      intercept_hit_location == 5 ~ pos5_start_pos_y,
      intercept_hit_location == 6 ~ pos6_start_pos_y,
      intercept_hit_location == 7 ~ pos7_start_pos_y,
      intercept_hit_location == 8 ~ pos8_start_pos_y,
      intercept_hit_location == 9 ~ pos9_start_pos_y,
      TRUE ~ NaN),
    # intercept fielder distance from home plate
    intercept_fielder_distance = case_when(
      intercept_hit_location == 3 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
      intercept_hit_location == 4 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
      intercept_hit_location == 5 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
      intercept_hit_location == 6 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
      intercept_hit_location == 7 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
      intercept_hit_location == 8 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
      intercept_hit_location == 9 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
      TRUE ~ NaN),
    # intercept fielder distance to intercept point on x
    fielder_distance_to_intercept_x = case_when(
      intercept_hit_location == 3 ~ first_distance_to_IP_x,
      intercept_hit_location == 4 ~ second_distance_to_IP_x,
      intercept_hit_location == 5 ~ third_distance_to_IP_x,
      intercept_hit_location == 6 ~ ss_distance_to_IP_x,
      intercept_hit_location == 7 ~ left_distance_to_IP_x,
      intercept_hit_location == 8 ~ center_distance_to_IP_x,
      intercept_hit_location == 9 ~ right_distance_to_IP_x,
      TRUE ~ NaN),
    # intercept fielder distance to intercept point on y
    fielder_distance_to_intercept_y = case_when(
      intercept_hit_location == 3 ~ first_distance_to_IP_y,
      intercept_hit_location == 4 ~ second_distance_to_IP_y,
      intercept_hit_location == 5 ~ third_distance_to_IP_y,
      intercept_hit_location == 6 ~ ss_distance_to_IP_y,
      intercept_hit_location == 7 ~ left_distance_to_IP_y,
      intercept_hit_location == 8 ~ center_distance_to_IP_y,
      intercept_hit_location == 9 ~ right_distance_to_IP_y,
      TRUE ~ NaN),
    # first base distance from intercept fielder
    intercept_base_distance_x = intercept_fielder_x - sin(45*(pi/180))*90,
    intercept_base_distance_y = intercept_fielder_y - cos(45*(pi/180))*90,
    intercept_base_distance = sqrt(intercept_base_distance_x^2+intercept_base_distance_y^2),
    # first base distance from the intercept point
    intercept_point_base_distance_x = intercept_point_x - sin(45*(pi/180))*90,
    intercept_point_base_distance_y = intercept_point_y - cos(45*(pi/180))*90,
    intercept_point_base_distance = sqrt(intercept_point_base_distance_x^2+intercept_point_base_distance_y^2),
    # fielder angle to intercept point
    fielder_distance_to_intercept = sqrt(fielder_distance_to_intercept_x^2 + fielder_distance_to_intercept_y^2),
    hit_angle_zero = hit_angle - intercept_fielder_angle,
    intercept_fielder_zero_x = sin(0*(pi/180))*intercept_fielder_distance,
    intercept_fielder_zero_y = cos(0*(pi/180))*intercept_fielder_distance,
    intercept_point_zero_x = sin(hit_angle_zero*(pi/180))*intercept_point_distance,
    intercept_point_zero_y = cos(hit_angle_zero*(pi/180))*intercept_point_distance,
    fielder_distance_to_intercept_zero_x = intercept_point_zero_x - intercept_fielder_zero_x,
    fielder_distance_to_intercept_zero_y = intercept_point_zero_y - intercept_fielder_zero_y,
    
    fielder_angle_to_intercept = case_when(
      intercept_point_zero_x < intercept_fielder_zero_x &
        intercept_point_zero_y < intercept_fielder_zero_y ~ atan(abs(fielder_distance_to_intercept_zero_x)/
                                                                   (abs(fielder_distance_to_intercept_zero_y)))*(180/pi),
      intercept_point_zero_x < intercept_fielder_zero_x &
        intercept_point_zero_y > intercept_fielder_zero_y ~ 90 + atan(abs(fielder_distance_to_intercept_zero_y)/
                                                                        (abs(fielder_distance_to_intercept_zero_x)))*(180/pi),
      intercept_point_zero_x > intercept_fielder_zero_x &
        intercept_point_zero_y > intercept_fielder_zero_y ~ 180 + atan(abs(fielder_distance_to_intercept_zero_x)/
                                                                         (abs(fielder_distance_to_intercept_zero_y)))*(180/pi),
      intercept_point_zero_x > intercept_fielder_zero_x &
        intercept_point_zero_y < intercept_fielder_zero_y ~ 270 + atan(abs(fielder_distance_to_intercept_zero_y)/
                                                                         (abs(fielder_distance_to_intercept_zero_x)))*(180/pi)),
    fielder_angle_to_intercept_group = case_when(
      fielder_angle_to_intercept >=0 & fielder_angle_to_intercept < 45 ~ 1,
      fielder_angle_to_intercept >=45 & fielder_angle_to_intercept < 90 ~ 2,
      fielder_angle_to_intercept >=90 & fielder_angle_to_intercept < 135 ~ 3,
      fielder_angle_to_intercept >=135 & fielder_angle_to_intercept < 180 ~ 4,
      fielder_angle_to_intercept >=180 & fielder_angle_to_intercept < 225 ~ 5,
      fielder_angle_to_intercept >=225 & fielder_angle_to_intercept < 270 ~ 6,
      fielder_angle_to_intercept >=270 & fielder_angle_to_intercept < 315 ~ 7,
      fielder_angle_to_intercept >=315 & fielder_angle_to_intercept <= 360 ~ 8),
    fielder_angle_to_intercept_group = as.factor(fielder_angle_to_intercept_group))%>%
  
  select(game_date, year, player_name, home_team, park, launch_speed, launch_angle,
         sprint_speed, intercept_hit_location, intercept_point_x, intercept_point_y, intercept_point_distance,
         intercept_point_angle, intercept_fielder_angle, intercept_fielder_distance, intercept_fielder_x,
         intercept_fielder_y, intercept_base_distance_x, intercept_base_distance_y, intercept_point_base_distance,
         intercept_point_base_distance_x, intercept_point_base_distance_y,
         intercept_fielder_zero_x, intercept_fielder_zero_y, intercept_point_zero_x, intercept_point_zero_y,
         intercept_base_distance, hit_angle, hit_location, pos3_start_pos_x,
         pos3_start_pos_y, pos4_start_pos_x, pos4_start_pos_y, pos5_start_pos_x, pos5_start_pos_y, pos6_start_pos_x,
         pos6_start_pos_y, pos7_start_pos_x, pos7_start_pos_y, pos8_start_pos_x, pos8_start_pos_y, pos9_start_pos_x,
         pos9_start_pos_y, fielder_angle_to_intercept, fielder_angle_to_intercept_group,
         fielder_distance_to_intercept, fielder_distance_to_intercept_x, fielder_distance_to_intercept_y,
         hit_distance_sc, hit_distance,des, stringer_hit_trajectory, hc_x_, hc_y_, adjusted_hc_y_,
         adjusted_hc_x_, hit_angle_zero, result, fielder_distance_to_intercept_zero_x, 
         fielder_distance_to_intercept_zero_y, fielder_distance_to_intercept, fielder_distance_to_intercept_x,
         fielder_distance_to_intercept_y, right_angle,center_angle,left_angle,ss_angle,third_angle,second_angle,
         first_angle,right_distance,center_distance,left_distance,ss_distance,third_distance,
         second_distance,first_distance,ss_base_distance_x,ss_base_distance_y,ss_base_distance,
         second_base_distance_x,second_base_distance_y,second_base_distance,third_base_distance_x,
         third_base_distance_y,third_base_distance , first_base_distance_x,first_base_distance_y,
         first_base_distance ,left_base_distance_x,left_base_distance_y,left_base_distance,
         center_base_distance_x,center_base_distance_y, center_base_distance, right_base_distance_x,
         right_base_distance_y,right_base_distance)%>%
  mutate(dist=hit_distance_sc - hit_distance,
         Hit = factor(ifelse(result=="Hit",1,0)),
         delta_angle = hit_angle - intercept_fielder_angle,
         delta_distance = intercept_point_distance - intercept_fielder_distance)

gallo3 = gallo3%>%
  mutate(pred = predict(model, gallo3, type = "response"))

probs3=rep(0,10000)

for(i in 1:10000){
  probs3[i]=mean(rbinom(nrow(gallo3),1,gallo3$pred), na.rm=T)
}

ggplot()+
  geom_histogram(aes(x = probs2), fill="blue", alpha = 0.5)+
  geom_histogram(aes(x = probs3), fill="forestgreen", alpha = 0.5)

gallo$pred2 = gallo3$pred

gallo%>%mutate(pred_dif = pred - pred2)%>%
  select(game_date, launch_speed,launch_angle,pred_dif, actual_pred = pred, sim_pred = pred2, des)%>%
  arrange(desc(pred_dif))

gallo%>%mutate(pred_dif = pred - pred2)%>%
  select(game_date, launch_speed,launch_angle,pred_dif, actual_pred = pred, sim_pred = pred2, des)%>%
  arrange(pred_dif)

ggplot(gallo%>%mutate(pred_dif = pred - pred2)%>%
         arrange(pred_dif)%>%head(20))+
  geom_point(aes(x=intercept_point_x, y=intercept_point_y))+
  geom_point(aes(x=intercept_fielder_x, y=intercept_fielder_y), size=2, color="goldenrod2")+
  geom_point(aes(x=intercept_point_x, y=intercept_point_y))+
  
  geom_point(aes(x=adjusted_hc_x_, y=adjusted_hc_y_), color="red", size=2)+
  
  geom_mlb_stadium(stadium_ids = "rays", stadium_transform_coords = T, stadium_segments = "all")+
  geom_segment(aes(x=intercept_point_x,y=intercept_point_y, xend=intercept_fielder_x, yend=intercept_fielder_y))+
  geom_segment(aes(x=0, y=0, xend=sin(hit_angle*(pi/180))*400, yend=cos(hit_angle*(pi/180))*400),
               linetype=2)+
  labs(title = "Actual Fielder's and Intercept Point",
       subtitle = "Joey Gallo - Largest Difference in Hit Prob")+
  theme_minimal()+
  
  theme(
    plot.title = element_text(hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank())+
  coord_equal()


ggplot(gallo%>%mutate(pred_dif = pred - pred2)%>%
         arrange(pred_dif)%>%head(20)%>%
         inner_join(gallo3, by = c("game_date", "launch_angle", "launch_speed")))+
  geom_point(aes(x=intercept_fielder_x.y, y=intercept_fielder_y.y), size=2, color="goldenrod2")+
  geom_point(aes(x=intercept_point_x.y, y=intercept_point_y.y))+
  
  geom_point(aes(x=adjusted_hc_x_.y, y=adjusted_hc_y_.y), color="red", size=2)+
  
  geom_mlb_stadium(stadium_ids = "rays", stadium_transform_coords = T, stadium_segments = "all")+
  geom_segment(aes(x=intercept_point_x.y,y=intercept_point_y.y, xend=intercept_fielder_x.y, yend=intercept_fielder_y.y))+
  geom_segment(aes(x=0, y=0, xend=sin(hit_angle.y*(pi/180))*400, yend=cos(hit_angle.y*(pi/180))*400),
               linetype=2)+
  labs(title = "Simulated Fielder's and Intercept Point",
       subtitle = "Joey Gallo - Largest Difference in Hit Prob")+
  theme_minimal()+
  
  theme(
    plot.title = element_text(hjust=0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank())+
  coord_equal()

g = gallo%>%mutate(pred_dif = pred - pred2)%>%
  arrange(pred_dif)%>%head(20)%>%
  inner_join(gallo3, by = c("game_date", "launch_angle", "launch_speed"))

g%>%
  select(game_date, launch_speed,launch_angle,pred_dif,pred.x, pred2, des.x)%>%
  arrange(pred_dif)

###

sample = sample%>%
  mutate(dist=hit_distance_sc - hit_distance,
         Hit = factor(ifelse(result=="Hit",1,0)),
         delta_angle = hit_angle - intercept_fielder_angle,
         delta_distance = intercept_point_distance - intercept_fielder_distance)

sample = sample%>%
  mutate(pred = predict(model, sample, type = "prob")[,2])

gallo = data%>%
  filter(player_name == "Gallo, Joey")%>%
  mutate(Hit = ifelse(result=="Hit",1,0))

probs=rep(0,10000)

for(i in 1:10000){
  probs[i]=mean(rbinom(nrow(gallo),1,gallo$pred), na.rm=T)
}

sample = sample%>%
  mutate(Hit2 = ifelse(Hit==1,1,0))

#

j = 0
accuracy = rep(0,21)
for(i in 1:21){
  message("threshold = ", j)
  t=test%>% mutate(pred = predict(model, test, type="prob")[,2],
                   Hit=ifelse(Hit==1,1,0),
                   pred2=1*(pred>=j))%>%
    summarize(accuracy=mean(Hit==pred2, na.rm = T))
  accuracy[i] = t$accuracy
  
  j = j + 0.05
}

accuracy = data.frame(accuracy, threshold = seq(0,1,0.05))

ggplot(accuracy, aes(x=threshold, y=accuracy))+geom_line() +
  labs(title="Random Forest accuracy by threshold")

#
sample = sample%>%
  filter(!hit_location %in% c(1,2), hit_angle > (-60) & hit_angle < 60)%>%
  mutate(dist=hit_distance_sc - hit_distance,
         Hit = factor(ifelse(result=="Hit",1,0)),
         delta_angle = hit_angle - intercept_fielder_angle,
         delta_distance = intercept_point_distance - intercept_fielder_distance)

log_model = glm(Hit ~ ns(launch_speed,10) * ns(launch_angle,10), data=sample, family = binomial)

launch_angle_seq=seq(-50,50, by=0.1)
launch_speed_seq=seq(60,120, by=10)
ex_data=expand.grid(launch_angle=launch_angle_seq, launch_speed=launch_speed_seq)



ex_data%>% 
  mutate(pred=predict(log_model,ex_data,type="response"))%>%
  ggplot(aes(x=launch_angle, y=pred, color=launch_speed))+geom_point()+
  scale_color_viridis(option = "C")+
  labs(x="Launch Angle", y="Hit Probability", title="Hit Probability by Launch Angle and Exit Velocity",
       color="Exit Velocity")+
  theme_bw()

#
sample%>%mutate(dist = hit_distance_sc - hit_distance)%>%
  filter(dist > 75, hit_distance<200)%>%
  ggplot()+
  geom_segment(aes(x = adjusted_hc_x_, y = adjusted_hc_y_, xend = hc_x_, yend=hc_y_))+
  geom_point(aes(x=adjusted_hc_x_, y=adjusted_hc_y_), color = "blue")+
  geom_point(aes(x=hc_x_, y=hc_y_), color="red")+
  geom_mlb_stadium(stadium_ids = "braves", stadium_segments = "all", stadium_transform_coords = T)




# 2 intercept points
sample = sample%>%
  filter(hit_angle > (-60) & hit_angle < 60, !hit_location %in% c(1,2), !grepl("foul terr", des))%>%
  mutate(adjusted_hc_x_ = sin(hit_angle*(pi/180))*hit_distance_sc,
         adjusted_hc_y_ = cos(hit_angle*(pi/180))*hit_distance_sc,
         
         ss_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             ss_distance < 230 & ss_base_distance < 180 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | ss_distance > 230 |
             ss_base_distance > 180 ~ hit_distance_sc),
         
         ss_intercept_point_x = sin(hit_angle*(pi/180))*ss_intercept_point_from_home, 
         ss_intercept_point_y = cos(hit_angle*(pi/180))*ss_intercept_point_from_home,
         ss_distance_to_IP_x = ss_intercept_point_x - pos6_start_pos_x,
         ss_distance_to_IP_y = ss_intercept_point_y - pos6_start_pos_y,
         ss_distance_to_intercept = sqrt(ss_distance_to_IP_x^2 + ss_distance_to_IP_y^2),
         
         third_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             third_distance < 230 & third_base_distance < 180 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | third_distance > 230 |
             third_base_distance > 180 ~ hit_distance_sc),
         
         third_intercept_point_x = sin(hit_angle*(pi/180))*third_intercept_point_from_home, 
         third_intercept_point_y = cos(hit_angle*(pi/180))*third_intercept_point_from_home,
         third_distance_to_IP_x = third_intercept_point_x - pos5_start_pos_x,
         third_distance_to_IP_y = third_intercept_point_y - pos5_start_pos_y,
         third_distance_to_intercept = sqrt(third_distance_to_IP_x^2 + third_distance_to_IP_y^2),
         
         second_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             second_distance < 230 & second_base_distance < 180 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | second_distance > 230 |
             second_base_distance > 180 ~ hit_distance_sc),
         
         second_intercept_point_x = sin(hit_angle*(pi/180))*second_intercept_point_from_home, 
         second_intercept_point_y = cos(hit_angle*(pi/180))*second_intercept_point_from_home,
         second_distance_to_IP_x = second_intercept_point_x - pos4_start_pos_x,
         second_distance_to_IP_y = second_intercept_point_y - pos4_start_pos_y,
         second_distance_to_intercept = sqrt(second_distance_to_IP_x^2 + second_distance_to_IP_y^2),
         
         first_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             first_distance < 230 & first_base_distance < 180 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | first_distance > 230 |
             first_base_distance > 180 ~ hit_distance_sc),
         
         first_intercept_point_x = sin(hit_angle*(pi/180))*first_intercept_point_from_home, 
         first_intercept_point_y = cos(hit_angle*(pi/180))*first_intercept_point_from_home,
         first_distance_to_IP_x = first_intercept_point_x - pos3_start_pos_x,
         first_distance_to_IP_y = first_intercept_point_y - pos3_start_pos_y,
         first_distance_to_intercept = sqrt(first_distance_to_IP_x^2 + first_distance_to_IP_y^2),
         
         left_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             left_distance < 230 & left_base_distance < 180 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | left_distance > 230 |
             left_base_distance > 180 ~ hit_distance_sc),
         
         left_intercept_point_x = sin(hit_angle*(pi/180))*left_intercept_point_from_home, 
         left_intercept_point_y = cos(hit_angle*(pi/180))*left_intercept_point_from_home,
         left_distance_to_IP_x = left_intercept_point_x - pos7_start_pos_x,
         left_distance_to_IP_y = left_intercept_point_y - pos7_start_pos_y,
         left_distance_to_intercept = sqrt(left_distance_to_IP_x^2 + left_distance_to_IP_y^2),
         
         center_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             center_distance < 230 & center_base_distance < 180 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | center_distance > 230 |
             center_base_distance > 180 ~ hit_distance_sc),
         
         center_intercept_point_x = sin(hit_angle*(pi/180))*center_intercept_point_from_home, 
         center_intercept_point_y = cos(hit_angle*(pi/180))*center_intercept_point_from_home,
         center_distance_to_IP_x = center_intercept_point_x - pos8_start_pos_x,
         center_distance_to_IP_y = center_intercept_point_y - pos8_start_pos_y,
         center_distance_to_intercept = sqrt(center_distance_to_IP_x^2 + center_distance_to_IP_y^2),
         
         right_intercept_point_from_home = case_when(
           hit_distance_sc <= sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             right_distance < 230 & right_base_distance < 180 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
           
           hit_distance_sc > sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | right_distance > 230 |
             right_base_distance > 180 ~ hit_distance_sc),
         
         right_intercept_point_x = sin(hit_angle*(pi/180))*right_intercept_point_from_home, 
         right_intercept_point_y = cos(hit_angle*(pi/180))*right_intercept_point_from_home,
         right_distance_to_IP_x = right_intercept_point_x - pos9_start_pos_x,
         right_distance_to_IP_y = right_intercept_point_y - pos9_start_pos_y,
         right_distance_to_intercept = sqrt(right_distance_to_IP_x^2 + right_distance_to_IP_y^2),
         # locate fielder responsible for batted ball
         intercept_hit_location_1 = case_when(
           first_distance_to_intercept<ss_distance_to_intercept & 
             first_distance_to_intercept<second_distance_to_intercept &
             first_distance_to_intercept<third_distance_to_intercept &
             first_distance_to_intercept<right_distance_to_intercept & 
             first_distance_to_intercept<center_distance_to_intercept &
             first_distance_to_intercept<left_distance_to_intercept ~ 3,
           
           second_distance_to_intercept<ss_distance_to_intercept & 
             second_distance_to_intercept<first_distance_to_intercept &
             second_distance_to_intercept<third_distance_to_intercept & 
             second_distance_to_intercept<right_distance_to_intercept & 
             second_distance_to_intercept<left_distance_to_intercept &
             second_distance_to_intercept<center_distance_to_intercept~ 4,
           
           ss_distance_to_intercept<first_distance_to_intercept & 
             ss_distance_to_intercept<second_distance_to_intercept &
             ss_distance_to_intercept<third_distance_to_intercept &
             ss_distance_to_intercept<right_distance_to_intercept & 
             ss_distance_to_intercept<left_distance_to_intercept &
             ss_distance_to_intercept<center_distance_to_intercept ~ 6,
           
           third_distance_to_intercept<ss_distance_to_intercept & 
             third_distance_to_intercept<second_distance_to_intercept &
             third_distance_to_intercept<first_distance_to_intercept &
             third_distance_to_intercept<right_distance_to_intercept & 
             third_distance_to_intercept<left_distance_to_intercept &
             third_distance_to_intercept<center_distance_to_intercept ~ 5,
           
           center_distance_to_intercept<ss_distance_to_intercept & 
             center_distance_to_intercept<second_distance_to_intercept &
             center_distance_to_intercept<first_distance_to_intercept &
             center_distance_to_intercept<right_distance_to_intercept & 
             center_distance_to_intercept<left_distance_to_intercept &
             center_distance_to_intercept<third_distance_to_intercept ~ 8,
           
           left_distance_to_intercept<ss_distance_to_intercept & 
             left_distance_to_intercept<second_distance_to_intercept &
             left_distance_to_intercept<first_distance_to_intercept &
             left_distance_to_intercept<right_distance_to_intercept & 
             left_distance_to_intercept<third_distance_to_intercept &
             left_distance_to_intercept<center_distance_to_intercept ~ 7,
           
           right_distance_to_intercept<ss_distance_to_intercept & 
             right_distance_to_intercept<second_distance_to_intercept &
             right_distance_to_intercept<first_distance_to_intercept &
             right_distance_to_intercept<third_distance_to_intercept & 
             right_distance_to_intercept<left_distance_to_intercept &
             right_distance_to_intercept<center_distance_to_intercept ~ 9),
         
         # fielder 2
         intercept_hit_location_2 = case_when(
           intercept_hit_location_1 != 3 &
             (first_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6) & 
             (first_distance_to_intercept<second_distance_to_intercept |intercept_hit_location_1 == 4) &
             (first_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5) &
             (first_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9) & 
             (first_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8) &
             (first_distance_to_intercept<left_distance_to_intercept  | intercept_hit_location_1 == 7) ~ 3,
           
           intercept_hit_location_1 != 4 &
             (second_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6) & 
             (second_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3) &
             (second_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5) & 
             (second_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9) & 
             (second_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7) &
             (second_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8) ~ 4,
           
           intercept_hit_location_1 != 6 & 
             (ss_distance_to_intercept<first_distance_to_intercept |  intercept_hit_location_1 == 3)& 
             (ss_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4) &
             (ss_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5) &
             (ss_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9) & 
             (ss_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7) &
             (ss_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8) ~ 6,
           
           intercept_hit_location_1 != 5 & 
             (third_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6) & 
             (third_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4) &
             (third_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3) &
             (third_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9) & 
             (third_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7) &
             (third_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8) ~ 5,
           
           intercept_hit_location_1 != 8 & 
             (center_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6) & 
             (center_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4) &
             (center_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3) &
             (center_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9) & 
             (center_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7) &
             (center_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5) ~ 8,
           
           intercept_hit_location_1 != 7 & 
             (left_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6) & 
             (left_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4) &
             (left_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3) &
             (left_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9) & 
             (left_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5) &
             (left_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8) ~ 7,
           
           intercept_hit_location_1 != 9 & 
             (right_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6) & 
             (right_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4) &
             (right_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3) &
             (right_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5) & 
             (right_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7) &
             (right_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8) ~ 9),
         
         
         # fielder 3
         intercept_hit_location_3 = case_when(
           (intercept_hit_location_1 != 3 & intercept_hit_location_2 != 3) &
             (first_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6) & 
             (first_distance_to_intercept<second_distance_to_intercept |intercept_hit_location_1 == 4 | 
                intercept_hit_location_2 == 4) &
             (first_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5) &
             (first_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9) & 
             (first_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8) &
             (first_distance_to_intercept<left_distance_to_intercept  | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7) ~ 3,
           
           (intercept_hit_location_1 != 4 & intercept_hit_location_2 != 4) &
             (second_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6) & 
             (second_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3) &
             (second_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5) & 
             (second_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9) & 
             (second_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7) &
             (second_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8) ~ 4,
           
           (intercept_hit_location_1 != 6 & intercept_hit_location_2 != 6) &
             (ss_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3)& 
             (ss_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4) &
             (ss_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5) &
             (ss_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9) & 
             (ss_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7) &
             (ss_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8) ~ 6,
           
           (intercept_hit_location_1 != 5 & intercept_hit_location_2 != 5) &
             (third_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6) & 
             (third_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4) &
             (third_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3) &
             (third_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9) & 
             (third_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7) &
             (third_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8) ~ 5,
           
           (intercept_hit_location_1 != 8 & intercept_hit_location_2 != 8)&
             (center_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6) & 
             (center_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4) &
             (center_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3) &
             (center_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9) & 
             (center_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7) &
             (center_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5) ~ 8,
           
           (intercept_hit_location_1 != 7 & intercept_hit_location_2 != 7) &
             (left_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6) & 
             (left_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4) &
             (left_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3) &
             (left_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9) & 
             (left_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5) &
             (left_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8) ~ 7,
           
           (intercept_hit_location_1 != 9 & intercept_hit_location_2 != 9) & 
             (right_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6) & 
             (right_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4) &
             (right_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3) &
             (right_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5) & 
             (right_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7) &
             (right_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8) ~ 9),
         
         # fielder 4
         intercept_hit_location_4 = case_when(
           (intercept_hit_location_1 != 3 & intercept_hit_location_2 != 3 & intercept_hit_location_3 !=3) &
             (first_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6) & 
             (first_distance_to_intercept<second_distance_to_intercept |intercept_hit_location_1 == 4 | 
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4) &
             (first_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5) &
             (first_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9) & 
             (first_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8) &
             (first_distance_to_intercept<left_distance_to_intercept  | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7) ~ 3,
           
           (intercept_hit_location_1 != 4 & intercept_hit_location_2 != 4 & intercept_hit_location_3 != 4) &
             (second_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6) & 
             (second_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3) &
             (second_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5) & 
             (second_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9) & 
             (second_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7) &
             (second_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8) ~ 4,
           
           (intercept_hit_location_1 != 6 & intercept_hit_location_2 != 6 & intercept_hit_location_3 != 6) &
             (ss_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3| intercept_hit_location_3 == 3)& 
             (ss_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4) &
             (ss_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5) &
             (ss_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9) & 
             (ss_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7) &
             (ss_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8) ~ 6,
           
           (intercept_hit_location_1 != 5 & intercept_hit_location_2 != 5 & intercept_hit_location_3 != 5) &
             (third_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6) & 
             (third_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4) &
             (third_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3) &
             (third_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9) & 
             (third_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7) &
             (third_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8) ~ 5,
           
           (intercept_hit_location_1 != 8 & intercept_hit_location_2 != 8 & intercept_hit_location_3 != 8)&
             (center_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6) & 
             (center_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4) &
             (center_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3) &
             (center_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9) & 
             (center_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7) &
             (center_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5) ~ 8,
           
           (intercept_hit_location_1 != 7 & intercept_hit_location_2 != 7 & intercept_hit_location_3 != 7) &
             (left_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6) & 
             (left_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4) &
             (left_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3) &
             (left_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9) & 
             (left_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5) &
             (left_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8) ~ 7,
           
           (intercept_hit_location_1 != 9 & intercept_hit_location_2 != 9 & intercept_hit_location_3 != 9) & 
             (right_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6) & 
             (right_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4) &
             (right_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3) &
             (right_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5) & 
             (right_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7) &
             (right_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8) ~ 9),
         
         # fielder 5
         intercept_hit_location_5 = case_when(
           (intercept_hit_location_1 != 3 & intercept_hit_location_2 != 3 & intercept_hit_location_3 !=3 &
              intercept_hit_location_4 != 3) &
             (first_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6) & 
             (first_distance_to_intercept<second_distance_to_intercept |intercept_hit_location_1 == 4 | 
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4) &
             (first_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5) &
             (first_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9) & 
             (first_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8) &
             (first_distance_to_intercept<left_distance_to_intercept  | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7) ~ 3,
           
           (intercept_hit_location_1 != 4 & intercept_hit_location_2 != 4 & intercept_hit_location_3 != 4 &
              intercept_hit_location_4 != 4) &
             (second_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6) & 
             (second_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3) &
             (second_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5) & 
             (second_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9) & 
             (second_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7) &
             (second_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8) ~ 4,
           
           (intercept_hit_location_1 != 6 & intercept_hit_location_2 != 6 & intercept_hit_location_3 != 6 &
              intercept_hit_location_4 != 6) &
             (ss_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3| intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3)& 
             (ss_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4) &
             (ss_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5) &
             (ss_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9) & 
             (ss_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7) &
             (ss_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8) ~ 6,
           
           (intercept_hit_location_1 != 5 & intercept_hit_location_2 != 5 & intercept_hit_location_3 != 5 &
              intercept_hit_location_4 != 5) &
             (third_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6) & 
             (third_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4) &
             (third_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3) &
             (third_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9) & 
             (third_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7) &
             (third_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8) ~ 5,
           
           (intercept_hit_location_1 != 8 & intercept_hit_location_2 != 8 & intercept_hit_location_3 != 8 &
              intercept_hit_location_4 != 8)&
             (center_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6) & 
             (center_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4) &
             (center_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3) &
             (center_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9) & 
             (center_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7) &
             (center_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5) ~ 8,
           
           (intercept_hit_location_1 != 7 & intercept_hit_location_2 != 7 & intercept_hit_location_3 != 7 &
              intercept_hit_location_4 != 7) &
             (left_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6) & 
             (left_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4) &
             (left_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3) &
             (left_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9) & 
             (left_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5) &
             (left_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8) ~ 7,
           
           (intercept_hit_location_1 != 9 & intercept_hit_location_2 != 9 & intercept_hit_location_3 != 9 &
              intercept_hit_location_4 != 9) & 
             (right_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6) & 
             (right_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4) &
             (right_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3) &
             (right_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5) & 
             (right_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7) &
             (right_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8) ~ 9),
         
         # fielder 6
         intercept_hit_location_6 = case_when(
           (intercept_hit_location_1 != 3 & intercept_hit_location_2 != 3 & intercept_hit_location_3 !=3 &
              intercept_hit_location_4 != 3  & intercept_hit_location_5 != 3) &
             (first_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6) & 
             (first_distance_to_intercept<second_distance_to_intercept |intercept_hit_location_1 == 4 | 
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4) &
             (first_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5) &
             (first_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9) & 
             (first_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8) &
             (first_distance_to_intercept<left_distance_to_intercept  | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7) ~ 3,
           
           (intercept_hit_location_1 != 4 & intercept_hit_location_2 != 4 & intercept_hit_location_3 != 4 &
              intercept_hit_location_4 != 4  & intercept_hit_location_5 != 4) &
             (second_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6) & 
             (second_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3) &
             (second_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5) & 
             (second_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9) & 
             (second_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7) &
             (second_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8) ~ 4,
           
           (intercept_hit_location_1 != 6 & intercept_hit_location_2 != 6 & intercept_hit_location_3 != 6 &
              intercept_hit_location_4 != 6  & intercept_hit_location_5 != 6) &
             (ss_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3| intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3)& 
             (ss_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4) &
             (ss_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5) &
             (ss_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9) & 
             (ss_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7) &
             (ss_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8) ~ 6,
           
           (intercept_hit_location_1 != 5 & intercept_hit_location_2 != 5 & intercept_hit_location_3 != 5 &
              intercept_hit_location_4 != 5  & intercept_hit_location_5 != 5) &
             (third_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6) & 
             (third_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4) &
             (third_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3) &
             (third_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9) & 
             (third_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7) &
             (third_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8) ~ 5,
           
           (intercept_hit_location_1 != 8 & intercept_hit_location_2 != 8 & intercept_hit_location_3 != 8 &
              intercept_hit_location_4 != 8  & intercept_hit_location_5 != 8)&
             (center_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6) & 
             (center_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4) &
             (center_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3) &
             (center_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9) & 
             (center_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7) &
             (center_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5) ~ 8,
           
           (intercept_hit_location_1 != 7 & intercept_hit_location_2 != 7 & intercept_hit_location_3 != 7 &
              intercept_hit_location_4 != 7  & intercept_hit_location_5 != 7) &
             (left_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6) & 
             (left_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4) &
             (left_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3) &
             (left_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9) & 
             (left_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5) &
             (left_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8) ~ 7,
           
           (intercept_hit_location_1 != 9 & intercept_hit_location_2 != 9 & intercept_hit_location_3 != 9 &
              intercept_hit_location_4 != 9  & intercept_hit_location_5 != 9) & 
             (right_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6) & 
             (right_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4) &
             (right_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3) &
             (right_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5) & 
             (right_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7) &
             (right_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8) ~ 9),
         
         
         # fielder 7
         intercept_hit_location_7 = case_when(
           (intercept_hit_location_1 != 3 & intercept_hit_location_2 != 3 & intercept_hit_location_3 !=3 &
              intercept_hit_location_4 != 3  & intercept_hit_location_5 != 3 & intercept_hit_location_6 != 3) &
             (first_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6 | intercept_hit_location_6 == 6) & 
             (first_distance_to_intercept<second_distance_to_intercept |intercept_hit_location_1 == 4 | 
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4 | intercept_hit_location_6 == 4) &
             (first_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5 | intercept_hit_location_6 == 5) &
             (first_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9 | intercept_hit_location_6 == 9) & 
             (first_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8 | intercept_hit_location_6 == 8) &
             (first_distance_to_intercept<left_distance_to_intercept  | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7 | intercept_hit_location_6 == 7) ~ 3,
           
           (intercept_hit_location_1 != 4 & intercept_hit_location_2 != 4 & intercept_hit_location_3 != 4 &
              intercept_hit_location_4 != 4  & intercept_hit_location_5 != 4 & intercept_hit_location_6 != 4) &
             (second_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6 | intercept_hit_location_6 == 6) & 
             (second_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3 | intercept_hit_location_6 == 3) &
             (second_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5 | intercept_hit_location_6 == 5) & 
             (second_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9 | intercept_hit_location_6 == 9) & 
             (second_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7 | intercept_hit_location_6 == 7) &
             (second_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8 | intercept_hit_location_6 == 8) ~ 4,
           
           (intercept_hit_location_1 != 6 & intercept_hit_location_2 != 6 & intercept_hit_location_3 != 6 &
              intercept_hit_location_4 != 6  & intercept_hit_location_5 != 6 & intercept_hit_location_6 != 6) &
             (ss_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3| intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3 | intercept_hit_location_6 == 3)& 
             (ss_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4 | intercept_hit_location_6 == 4) &
             (ss_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5 | intercept_hit_location_6 == 5) &
             (ss_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9 | intercept_hit_location_6 == 9) & 
             (ss_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7 | intercept_hit_location_6 == 7) &
             (ss_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8 | intercept_hit_location_6 == 8) ~ 6,
           
           (intercept_hit_location_1 != 5 & intercept_hit_location_2 != 5 & intercept_hit_location_3 != 5 &
              intercept_hit_location_4 != 5  & intercept_hit_location_5 != 5 & intercept_hit_location_6 != 5) &
             (third_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6 | intercept_hit_location_6 == 6) & 
             (third_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4 | intercept_hit_location_6 == 4) &
             (third_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3 | intercept_hit_location_6 == 3) &
             (third_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9 | intercept_hit_location_6 == 9) & 
             (third_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7 | intercept_hit_location_6 == 7) &
             (third_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8 | intercept_hit_location_6 == 8) ~ 5,
           
           (intercept_hit_location_1 != 8 & intercept_hit_location_2 != 8 & intercept_hit_location_3 != 8 &
              intercept_hit_location_4 != 8  & intercept_hit_location_5 != 8 & intercept_hit_location_6 != 8)&
             (center_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6 | intercept_hit_location_6 == 6) & 
             (center_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4 | intercept_hit_location_6 == 4) &
             (center_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3 | intercept_hit_location_6 == 3) &
             (center_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9 | intercept_hit_location_6 == 9) & 
             (center_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7 | intercept_hit_location_6 == 7) &
             (center_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5 | intercept_hit_location_6 == 5) ~ 8,
           
           (intercept_hit_location_1 != 7 & intercept_hit_location_2 != 7 & intercept_hit_location_3 != 7 &
              intercept_hit_location_4 != 7  & intercept_hit_location_5 != 7 & intercept_hit_location_6 != 7) &
             (left_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6 | intercept_hit_location_6 == 6) & 
             (left_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4 | intercept_hit_location_6 == 4) &
             (left_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3 | intercept_hit_location_6 == 3) &
             (left_distance_to_intercept<right_distance_to_intercept | intercept_hit_location_1 == 9 |
                intercept_hit_location_2 == 9 | intercept_hit_location_3 == 9 | intercept_hit_location_4 == 9 |
                intercept_hit_location_5 == 9 | intercept_hit_location_6 == 9) & 
             (left_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5 | intercept_hit_location_6 == 5) &
             (left_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8 | intercept_hit_location_6 == 8) ~ 7,
           
           (intercept_hit_location_1 != 9 & intercept_hit_location_2 != 9 & intercept_hit_location_3 != 9 &
              intercept_hit_location_4 != 9  & intercept_hit_location_5 != 9 & intercept_hit_location_6 != 9) & 
             (right_distance_to_intercept<ss_distance_to_intercept | intercept_hit_location_1 == 6 |
                intercept_hit_location_2 == 6 | intercept_hit_location_3 == 6 | intercept_hit_location_4 == 6 |
                intercept_hit_location_5 == 6 | intercept_hit_location_6 == 6) & 
             (right_distance_to_intercept<second_distance_to_intercept | intercept_hit_location_1 == 4 |
                intercept_hit_location_2 == 4 | intercept_hit_location_3 == 4 | intercept_hit_location_4 == 4 |
                intercept_hit_location_5 == 4 | intercept_hit_location_6 == 4) &
             (right_distance_to_intercept<first_distance_to_intercept | intercept_hit_location_1 == 3 |
                intercept_hit_location_2 == 3 | intercept_hit_location_3 == 3 | intercept_hit_location_4 == 3 |
                intercept_hit_location_5 == 3 | intercept_hit_location_6 == 3) &
             (right_distance_to_intercept<third_distance_to_intercept | intercept_hit_location_1 == 5 |
                intercept_hit_location_2 == 5 | intercept_hit_location_3 == 5 | intercept_hit_location_4 == 5 |
                intercept_hit_location_5 == 5 | intercept_hit_location_6 == 5) & 
             (right_distance_to_intercept<left_distance_to_intercept | intercept_hit_location_1 == 7 |
                intercept_hit_location_2 == 7 | intercept_hit_location_3 == 7 | intercept_hit_location_4 == 7 |
                intercept_hit_location_5 == 7 | intercept_hit_location_6 == 7) &
             (right_distance_to_intercept<center_distance_to_intercept | intercept_hit_location_1 == 8 |
                intercept_hit_location_2 == 8 | intercept_hit_location_3 == 8 | intercept_hit_location_4 == 8 |
                intercept_hit_location_5 == 8 | intercept_hit_location_6 == 8) ~ 9),
         
         
         #intercept point on x 
         intercept_point_x_1 = case_when(
           intercept_hit_location_1 == 3 ~ first_intercept_point_x,
           intercept_hit_location_1 == 4 ~ second_intercept_point_x,
           intercept_hit_location_1 == 6 ~ ss_intercept_point_x,
           intercept_hit_location_1 == 5 ~ third_intercept_point_x,
           intercept_hit_location_1 == 7 ~ left_intercept_point_x,
           intercept_hit_location_1 == 8 ~ center_intercept_point_x,
           intercept_hit_location_1 == 9 ~ right_intercept_point_x),
         #intercept point on y
         intercept_point_y_1 = case_when(
           intercept_hit_location_1 == 3 ~ first_intercept_point_y,
           intercept_hit_location_1 == 4 ~ second_intercept_point_y,
           intercept_hit_location_1 == 6 ~ ss_intercept_point_y,
           intercept_hit_location_1 == 5 ~ third_intercept_point_y,
           intercept_hit_location_1 == 7 ~ left_intercept_point_y,
           intercept_hit_location_1 == 8 ~ center_intercept_point_y,
           intercept_hit_location_1 == 9 ~ right_intercept_point_y),
         # intercpet point distance from home
         intercept_point_distance_1 = sqrt(intercept_point_x_1^2 + intercept_point_y_1^2), 
         # intercept point angle from home
         intercept_point_angle = hit_angle,
         # intercept fielder angle from home
         intercept_fielder_angle_1 = case_when(
           intercept_hit_location_1  == 3 ~ atan((pos3_start_pos_x)/
                                                 (pos3_start_pos_y))*(180/pi),
           intercept_hit_location_1  == 4 ~ atan((pos4_start_pos_x)/
                                                 (pos4_start_pos_y))*(180/pi),
           intercept_hit_location_1  == 5 ~ atan((pos5_start_pos_x)/
                                                 (pos5_start_pos_y))*(180/pi),
           intercept_hit_location_1  == 6 ~ atan((pos6_start_pos_x)/
                                                 (pos6_start_pos_y))*(180/pi),
           intercept_hit_location_1  == 7 ~ atan((pos7_start_pos_x)/
                                                 (pos7_start_pos_y))*(180/pi),
           intercept_hit_location_1  == 8 ~ atan((pos8_start_pos_x)/
                                                 (pos8_start_pos_y))*(180/pi),
           intercept_hit_location_1  == 9 ~ atan((pos9_start_pos_x)/
                                                 (pos9_start_pos_y))*(180/pi),
           TRUE ~ NaN),
         # intercept fielder located on x
         intercept_fielder_x_1 = case_when(
           intercept_hit_location_1 == 3 ~ pos3_start_pos_x,
           intercept_hit_location_1 == 4 ~ pos4_start_pos_x,
           intercept_hit_location_1 == 5 ~ pos5_start_pos_x,
           intercept_hit_location_1 == 6 ~ pos6_start_pos_x,
           intercept_hit_location_1 == 7 ~ pos7_start_pos_x,
           intercept_hit_location_1 == 8 ~ pos8_start_pos_x,
           intercept_hit_location_1 == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         # intercept fielder located on y
         intercept_fielder_y_1 = case_when(
           intercept_hit_location_1 == 3 ~ pos3_start_pos_y,
           intercept_hit_location_1 == 4 ~ pos4_start_pos_y,
           intercept_hit_location_1 == 5 ~ pos5_start_pos_y,
           intercept_hit_location_1 == 6 ~ pos6_start_pos_y,
           intercept_hit_location_1 == 7 ~ pos7_start_pos_y,
           intercept_hit_location_1 == 8 ~ pos8_start_pos_y,
           intercept_hit_location_1 == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         # intercept fielder distance from home plate
         intercept_fielder_distance_1 = case_when(
           intercept_hit_location_1 == 3 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
           intercept_hit_location_1 == 4 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
           intercept_hit_location_1 == 5 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
           intercept_hit_location_1 == 6 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
           intercept_hit_location_1 == 7 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
           intercept_hit_location_1 == 8 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
           intercept_hit_location_1 == 9 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on x
         fielder_distance_to_intercept_x_1 = case_when(
           intercept_hit_location_1 == 3 ~ first_distance_to_IP_x,
           intercept_hit_location_1 == 4 ~ second_distance_to_IP_x,
           intercept_hit_location_1 == 5 ~ third_distance_to_IP_x,
           intercept_hit_location_1 == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location_1 == 7 ~ left_distance_to_IP_x,
           intercept_hit_location_1 == 8 ~ center_distance_to_IP_x,
           intercept_hit_location_1 == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on y
         fielder_distance_to_intercept_y_1 = case_when(
           intercept_hit_location_1 == 3 ~ first_distance_to_IP_y,
           intercept_hit_location_1 == 4 ~ second_distance_to_IP_y,
           intercept_hit_location_1 == 5 ~ third_distance_to_IP_y,
           intercept_hit_location_1 == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location_1 == 7 ~ left_distance_to_IP_y,
           intercept_hit_location_1 == 8 ~ center_distance_to_IP_y,
           intercept_hit_location_1 == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
        
         # first base distance from intercept fielder
         intercept_base_distance_x_1 = intercept_fielder_x_1 - sin(45*(pi/180))*90,
         intercept_base_distance_y_1 = intercept_fielder_y_1 - cos(45*(pi/180))*90,
         intercept_base_distance_1 = sqrt(intercept_base_distance_x_1^2+intercept_base_distance_y_1^2),
         # first base distance from the intercept point
         intercept_point_base_distance_x_1 = intercept_point_x_1 - sin(45*(pi/180))*90,
         intercept_point_base_distance_y_1 = intercept_point_y_1 - cos(45*(pi/180))*90,
         intercept_point_base_distance_1 = sqrt(intercept_point_base_distance_x_1^2+intercept_point_base_distance_y_1^2),
         # fielder angle to intercept point
         fielder_distance_to_intercept_1 = sqrt(fielder_distance_to_intercept_x_1^2 + fielder_distance_to_intercept_y_1^2),
         hit_angle_zero_1 = hit_angle - intercept_fielder_angle_1,
         intercept_fielder_zero_x_1 = sin(0*(pi/180))*intercept_fielder_distance_1,
         intercept_fielder_zero_y_1 = cos(0*(pi/180))*intercept_fielder_distance_1,
         intercept_point_zero_x_1 = sin(hit_angle_zero_1*(pi/180))*intercept_point_distance_1,
         intercept_point_zero_y_1 = cos(hit_angle_zero_1*(pi/180))*intercept_point_distance_1,
         fielder_distance_to_intercept_zero_x_1 = intercept_point_zero_x_1 - intercept_fielder_zero_x_1,
         fielder_distance_to_intercept_zero_y_1 = intercept_point_zero_y_1 - intercept_fielder_zero_y_1,
         
         # TWO
         intercept_point_x_2 = case_when(
           intercept_hit_location_2 == 3 ~ first_intercept_point_x,
           intercept_hit_location_2 == 4 ~ second_intercept_point_x,
           intercept_hit_location_2 == 6 ~ ss_intercept_point_x,
           intercept_hit_location_2 == 5 ~ third_intercept_point_x,
           intercept_hit_location_2 == 7 ~ left_intercept_point_x,
           intercept_hit_location_2 == 8 ~ center_intercept_point_x,
           intercept_hit_location_2 == 9 ~ right_intercept_point_x),
         #intercept point on y
         intercept_point_y_2 = case_when(
           intercept_hit_location_2 == 3 ~ first_intercept_point_y,
           intercept_hit_location_2 == 4 ~ second_intercept_point_y,
           intercept_hit_location_2 == 6 ~ ss_intercept_point_y,
           intercept_hit_location_2 == 5 ~ third_intercept_point_y,
           intercept_hit_location_2 == 7 ~ left_intercept_point_y,
           intercept_hit_location_2 == 8 ~ center_intercept_point_y,
           intercept_hit_location_2 == 9 ~ right_intercept_point_y),
         # intercpet point distance from home
         intercept_point_distance_2 = sqrt(intercept_point_x_2^2 + intercept_point_y_2^2), 
         # intercept fielder angle from home
         intercept_fielder_angle_2 = case_when(
           intercept_hit_location_2  == 3 ~ atan((pos3_start_pos_x)/
                                                   (pos3_start_pos_y))*(180/pi),
           intercept_hit_location_2  == 4 ~ atan((pos4_start_pos_x)/
                                                   (pos4_start_pos_y))*(180/pi),
           intercept_hit_location_2  == 5 ~ atan((pos5_start_pos_x)/
                                                   (pos5_start_pos_y))*(180/pi),
           intercept_hit_location_2  == 6 ~ atan((pos6_start_pos_x)/
                                                   (pos6_start_pos_y))*(180/pi),
           intercept_hit_location_2  == 7 ~ atan((pos7_start_pos_x)/
                                                   (pos7_start_pos_y))*(180/pi),
           intercept_hit_location_2  == 8 ~ atan((pos8_start_pos_x)/
                                                   (pos8_start_pos_y))*(180/pi),
           intercept_hit_location_2  == 9 ~ atan((pos9_start_pos_x)/
                                                   (pos9_start_pos_y))*(180/pi),
           TRUE ~ NaN),
         # intercept fielder located on x
         intercept_fielder_x_2 = case_when(
           intercept_hit_location_2 == 3 ~ pos3_start_pos_x,
           intercept_hit_location_2 == 4 ~ pos4_start_pos_x,
           intercept_hit_location_2 == 5 ~ pos5_start_pos_x,
           intercept_hit_location_2 == 6 ~ pos6_start_pos_x,
           intercept_hit_location_2 == 7 ~ pos7_start_pos_x,
           intercept_hit_location_2 == 8 ~ pos8_start_pos_x,
           intercept_hit_location_2 == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         # intercept fielder located on y
         intercept_fielder_y_2 = case_when(
           intercept_hit_location_2 == 3 ~ pos3_start_pos_y,
           intercept_hit_location_2 == 4 ~ pos4_start_pos_y,
           intercept_hit_location_2 == 5 ~ pos5_start_pos_y,
           intercept_hit_location_2 == 6 ~ pos6_start_pos_y,
           intercept_hit_location_2 == 7 ~ pos7_start_pos_y,
           intercept_hit_location_2 == 8 ~ pos8_start_pos_y,
           intercept_hit_location_2 == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         # intercept fielder distance from home plate
         intercept_fielder_distance_2 = case_when(
           intercept_hit_location_2 == 3 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
           intercept_hit_location_2 == 4 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
           intercept_hit_location_2 == 5 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
           intercept_hit_location_2 == 6 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
           intercept_hit_location_2 == 7 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
           intercept_hit_location_2 == 8 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
           intercept_hit_location_2 == 9 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on x
         fielder_distance_to_intercept_x_2 = case_when(
           intercept_hit_location_2 == 3 ~ first_distance_to_IP_x,
           intercept_hit_location_2 == 4 ~ second_distance_to_IP_x,
           intercept_hit_location_2 == 5 ~ third_distance_to_IP_x,
           intercept_hit_location_2 == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location_2 == 7 ~ left_distance_to_IP_x,
           intercept_hit_location_2 == 8 ~ center_distance_to_IP_x,
           intercept_hit_location_2 == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on y
         fielder_distance_to_intercept_y_2 = case_when(
           intercept_hit_location_2 == 3 ~ first_distance_to_IP_y,
           intercept_hit_location_2 == 4 ~ second_distance_to_IP_y,
           intercept_hit_location_2 == 5 ~ third_distance_to_IP_y,
           intercept_hit_location_2 == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location_2 == 7 ~ left_distance_to_IP_y,
           intercept_hit_location_2 == 8 ~ center_distance_to_IP_y,
           intercept_hit_location_2 == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
         
         # first base distance from intercept fielder
         intercept_base_distance_x_2 = intercept_fielder_x_2 - sin(45*(pi/180))*90,
         intercept_base_distance_y_2 = intercept_fielder_y_2 - cos(45*(pi/180))*90,
         intercept_base_distance_2 = sqrt(intercept_base_distance_x_2^2+intercept_base_distance_y_2^2),
         # first base distance from the intercept point
         intercept_point_base_distance_x_2 = intercept_point_x_2 - sin(45*(pi/180))*90,
         intercept_point_base_distance_y_2 = intercept_point_y_2 - cos(45*(pi/180))*90,
         intercept_point_base_distance_2 = sqrt(intercept_point_base_distance_x_2^2+intercept_point_base_distance_y_2^2),
         # fielder angle to intercept point
         fielder_distance_to_intercept_2 = sqrt(fielder_distance_to_intercept_x_2^2 + fielder_distance_to_intercept_y_2^2),
         hit_angle_zero_2 = hit_angle - intercept_fielder_angle_2,
         intercept_fielder_zero_x_2 = sin(0*(pi/180))*intercept_fielder_distance_2,
         intercept_fielder_zero_y_2 = cos(0*(pi/180))*intercept_fielder_distance_2,
         intercept_point_zero_x_2 = sin(hit_angle_zero_2*(pi/180))*intercept_point_distance_2,
         intercept_point_zero_y_2 = cos(hit_angle_zero_2*(pi/180))*intercept_point_distance_2,
         fielder_distance_to_intercept_zero_x_2 = intercept_point_zero_x_2 - intercept_fielder_zero_x_2,
         fielder_distance_to_intercept_zero_y_2 = intercept_point_zero_y_2 - intercept_fielder_zero_y_2,
         
         # THREE
         intercept_point_x_3 = case_when(
           intercept_hit_location_3 == 3 ~ first_intercept_point_x,
           intercept_hit_location_3 == 4 ~ second_intercept_point_x,
           intercept_hit_location_3 == 6 ~ ss_intercept_point_x,
           intercept_hit_location_3 == 5 ~ third_intercept_point_x,
           intercept_hit_location_3 == 7 ~ left_intercept_point_x,
           intercept_hit_location_3 == 8 ~ center_intercept_point_x,
           intercept_hit_location_3 == 9 ~ right_intercept_point_x),
         #intercept point on y
         intercept_point_y_3 = case_when(
           intercept_hit_location_3 == 3 ~ first_intercept_point_y,
           intercept_hit_location_3 == 4 ~ second_intercept_point_y,
           intercept_hit_location_3 == 6 ~ ss_intercept_point_y,
           intercept_hit_location_3 == 5 ~ third_intercept_point_y,
           intercept_hit_location_3 == 7 ~ left_intercept_point_y,
           intercept_hit_location_3 == 8 ~ center_intercept_point_y,
           intercept_hit_location_3 == 9 ~ right_intercept_point_y),
         # intercpet point distance from home
         intercept_point_distance_3 = sqrt(intercept_point_x_3^2 + intercept_point_y_3^2), 
         # intercept fielder angle from home
         intercept_fielder_angle_3 = case_when(
           intercept_hit_location_3  == 3 ~ atan((pos3_start_pos_x)/
                                                   (pos3_start_pos_y))*(180/pi),
           intercept_hit_location_3  == 4 ~ atan((pos4_start_pos_x)/
                                                  (pos4_start_pos_y))*(180/pi),
           intercept_hit_location_3  == 5 ~ atan((pos5_start_pos_x)/
                                                   (pos5_start_pos_y))*(180/pi),
           intercept_hit_location_3  == 6 ~ atan((pos6_start_pos_x)/
                                                   (pos6_start_pos_y))*(180/pi),
           intercept_hit_location_3  == 7 ~ atan((pos7_start_pos_x)/
                                                   (pos7_start_pos_y))*(180/pi),
           intercept_hit_location_3  == 8 ~ atan((pos8_start_pos_x)/
                                                   (pos8_start_pos_y))*(180/pi),
           intercept_hit_location_3  == 9 ~ atan((pos9_start_pos_x)/
                                                   (pos9_start_pos_y))*(180/pi),
           TRUE ~ NaN),
         # intercept fielder located on x
         intercept_fielder_x_3 = case_when(
           intercept_hit_location_3 == 3 ~ pos3_start_pos_x,
           intercept_hit_location_3 == 4 ~ pos4_start_pos_x,
           intercept_hit_location_3 == 5 ~ pos5_start_pos_x,
           intercept_hit_location_3 == 6 ~ pos6_start_pos_x,
           intercept_hit_location_3 == 7 ~ pos7_start_pos_x,
           intercept_hit_location_3 == 8 ~ pos8_start_pos_x,
           intercept_hit_location_3 == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         # intercept fielder located on y
         intercept_fielder_y_3 = case_when(
           intercept_hit_location_3 == 3 ~ pos3_start_pos_y,
           intercept_hit_location_3 == 4 ~ pos4_start_pos_y,
           intercept_hit_location_3 == 5 ~ pos5_start_pos_y,
           intercept_hit_location_3 == 6 ~ pos6_start_pos_y,
           intercept_hit_location_3 == 7 ~ pos7_start_pos_y,
           intercept_hit_location_3 == 8 ~ pos8_start_pos_y,
           intercept_hit_location_3 == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         # intercept fielder distance from home plate
         intercept_fielder_distance_3 = sqrt(intercept_fielder_x_3^2 + intercept_fielder_y_3^2),
         # intercept fielder distance to intercept point on x
         fielder_distance_to_intercept_x_3 = case_when(
           intercept_hit_location_3 == 3 ~ first_distance_to_IP_x,
           intercept_hit_location_3 == 4 ~ second_distance_to_IP_x,
           intercept_hit_location_3 == 5 ~ third_distance_to_IP_x,
           intercept_hit_location_3 == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location_3 == 7 ~ left_distance_to_IP_x,
           intercept_hit_location_3 == 8 ~ center_distance_to_IP_x,
           intercept_hit_location_3 == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on y
         fielder_distance_to_intercept_y_3 = case_when(
           intercept_hit_location_3 == 3 ~ first_distance_to_IP_y,
           intercept_hit_location_3 == 4 ~ second_distance_to_IP_y,
           intercept_hit_location_3 == 5 ~ third_distance_to_IP_y,
           intercept_hit_location_3 == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location_3 == 7 ~ left_distance_to_IP_y,
           intercept_hit_location_3 == 8 ~ center_distance_to_IP_y,
           intercept_hit_location_3 == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
         
         # first base distance from intercept fielder
         intercept_base_distance_x_3 = intercept_fielder_x_3 - sin(45*(pi/180))*90,
         intercept_base_distance_y_3 = intercept_fielder_y_3 - cos(45*(pi/180))*90,
         intercept_base_distance_3 = sqrt(intercept_base_distance_x_3^2+intercept_base_distance_y_3^2),
         # first base distance from the intercept point
         intercept_point_base_distance_x_3 = intercept_point_x_3 - sin(45*(pi/180))*90,
         intercept_point_base_distance_y_3 = intercept_point_y_3 - cos(45*(pi/180))*90,
         intercept_point_base_distance_3 = sqrt(intercept_point_base_distance_x_3^2+intercept_point_base_distance_y_3^2),
         # fielder angle to intercept point
         fielder_distance_to_intercept_3 = sqrt(fielder_distance_to_intercept_x_3^2 + fielder_distance_to_intercept_y_3^2),
         hit_angle_zero_3 = hit_angle - intercept_fielder_angle_3,
         intercept_fielder_zero_x_3 = sin(0*(pi/180))*intercept_fielder_distance_3,
         intercept_fielder_zero_y_3 = cos(0*(pi/180))*intercept_fielder_distance_3,
         intercept_point_zero_x_3 = sin(hit_angle_zero_3*(pi/180))*intercept_point_distance_3,
         intercept_point_zero_y_3 = cos(hit_angle_zero_3*(pi/180))*intercept_point_distance_3,
         fielder_distance_to_intercept_zero_x_3 = intercept_point_zero_x_3 - intercept_fielder_zero_x_3,
         fielder_distance_to_intercept_zero_y_3 = intercept_point_zero_y_3 - intercept_fielder_zero_y_3,
         
         # FOUR
         intercept_point_x_4 = case_when(
           intercept_hit_location_4 == 3 ~ first_intercept_point_x,
           intercept_hit_location_4 == 4 ~ second_intercept_point_x,
           intercept_hit_location_4 == 6 ~ ss_intercept_point_x,
           intercept_hit_location_4 == 5 ~ third_intercept_point_x,
           intercept_hit_location_4 == 7 ~ left_intercept_point_x,
           intercept_hit_location_4 == 8 ~ center_intercept_point_x,
           intercept_hit_location_4 == 9 ~ right_intercept_point_x),
         #intercept point on y
         intercept_point_y_4 = case_when(
           intercept_hit_location_4 == 3 ~ first_intercept_point_y,
           intercept_hit_location_4 == 4 ~ second_intercept_point_y,
           intercept_hit_location_4 == 6 ~ ss_intercept_point_y,
           intercept_hit_location_4 == 5 ~ third_intercept_point_y,
           intercept_hit_location_4 == 7 ~ left_intercept_point_y,
           intercept_hit_location_4 == 8 ~ center_intercept_point_y,
           intercept_hit_location_4 == 9 ~ right_intercept_point_y),
         # intercpet point distance from home
         intercept_point_distance_4 = sqrt(intercept_point_x_4^2 + intercept_point_y_4^2), 
         # intercept fielder angle from home
         intercept_fielder_angle_4 = case_when(
           intercept_hit_location_4  == 3 ~ atan((pos3_start_pos_x)/
                                                   (pos3_start_pos_y))*(180/pi),
           intercept_hit_location_4  == 4 ~ atan((pos4_start_pos_x)/
                                                   (pos4_start_pos_y))*(180/pi),
           intercept_hit_location_4  == 5 ~ atan((pos5_start_pos_x)/
                                                   (pos5_start_pos_y))*(180/pi),
           intercept_hit_location_4  == 6 ~ atan((pos6_start_pos_x)/
                                                   (pos6_start_pos_y))*(180/pi),
           intercept_hit_location_4  == 7 ~ atan((pos7_start_pos_x)/
                                                   (pos7_start_pos_y))*(180/pi),
           intercept_hit_location_4  == 8 ~ atan((pos8_start_pos_x)/
                                                   (pos8_start_pos_y))*(180/pi),
           intercept_hit_location_4  == 9 ~ atan((pos9_start_pos_x)/
                                                   (pos9_start_pos_y))*(180/pi),
           TRUE ~ NaN),
         # intercept fielder located on x
         intercept_fielder_x_4 = case_when(
           intercept_hit_location_4 == 3 ~ pos3_start_pos_x,
           intercept_hit_location_4 == 4 ~ pos4_start_pos_x,
           intercept_hit_location_4 == 5 ~ pos5_start_pos_x,
           intercept_hit_location_4 == 6 ~ pos6_start_pos_x,
           intercept_hit_location_4 == 7 ~ pos7_start_pos_x,
           intercept_hit_location_4 == 8 ~ pos8_start_pos_x,
           intercept_hit_location_4 == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         # intercept fielder located on y
         intercept_fielder_y_4 = case_when(
           intercept_hit_location_4 == 3 ~ pos3_start_pos_y,
           intercept_hit_location_4 == 4 ~ pos4_start_pos_y,
           intercept_hit_location_4 == 5 ~ pos5_start_pos_y,
           intercept_hit_location_4 == 6 ~ pos6_start_pos_y,
           intercept_hit_location_4 == 7 ~ pos7_start_pos_y,
           intercept_hit_location_4 == 8 ~ pos8_start_pos_y,
           intercept_hit_location_4 == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         # intercept fielder distance from home plate
         intercept_fielder_distance_4 = sqrt(intercept_fielder_x_4^2 + intercept_fielder_y_4^2),
         # intercept fielder distance to intercept point on x
         fielder_distance_to_intercept_x_4 = case_when(
           intercept_hit_location_4 == 3 ~ first_distance_to_IP_x,
           intercept_hit_location_4 == 4 ~ second_distance_to_IP_x,
           intercept_hit_location_4 == 5 ~ third_distance_to_IP_x,
           intercept_hit_location_4 == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location_4 == 7 ~ left_distance_to_IP_x,
           intercept_hit_location_4 == 8 ~ center_distance_to_IP_x,
           intercept_hit_location_4 == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on y
         fielder_distance_to_intercept_y_4 = case_when(
           intercept_hit_location_4 == 3 ~ first_distance_to_IP_y,
           intercept_hit_location_4 == 4 ~ second_distance_to_IP_y,
           intercept_hit_location_4 == 5 ~ third_distance_to_IP_y,
           intercept_hit_location_4 == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location_4 == 7 ~ left_distance_to_IP_y,
           intercept_hit_location_4 == 8 ~ center_distance_to_IP_y,
           intercept_hit_location_4 == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
         
         # first base distance from intercept fielder
         intercept_base_distance_x_4 = intercept_fielder_x_4 - sin(45*(pi/180))*90,
         intercept_base_distance_y_4 = intercept_fielder_y_4 - cos(45*(pi/180))*90,
         intercept_base_distance_4 = sqrt(intercept_base_distance_x_4^2+intercept_base_distance_y_4^2),
         # first base distance from the intercept point
         intercept_point_base_distance_x_4 = intercept_point_x_4 - sin(45*(pi/180))*90,
         intercept_point_base_distance_y_4 = intercept_point_y_4 - cos(45*(pi/180))*90,
         intercept_point_base_distance_4 = sqrt(intercept_point_base_distance_x_4^2+intercept_point_base_distance_y_4^2),
         # fielder angle to intercept point
         fielder_distance_to_intercept_4 = sqrt(fielder_distance_to_intercept_x_4^2 + fielder_distance_to_intercept_y_4^2),
         hit_angle_zero_4 = hit_angle - intercept_fielder_angle_4,
         intercept_fielder_zero_x_4 = sin(0*(pi/180))*intercept_fielder_distance_4,
         intercept_fielder_zero_y_4 = cos(0*(pi/180))*intercept_fielder_distance_4,
         intercept_point_zero_x_4 = sin(hit_angle_zero_4*(pi/180))*intercept_point_distance_4,
         intercept_point_zero_y_4 = cos(hit_angle_zero_4*(pi/180))*intercept_point_distance_4,
         fielder_distance_to_intercept_zero_x_4 = intercept_point_zero_x_4 - intercept_fielder_zero_x_4,
         fielder_distance_to_intercept_zero_y_4 = intercept_point_zero_y_4 - intercept_fielder_zero_y_4,
         
         # FIVE
         intercept_point_x_5 = case_when(
           intercept_hit_location_5== 3 ~ first_intercept_point_x,
           intercept_hit_location_5 == 4 ~ second_intercept_point_x,
           intercept_hit_location_5 == 6 ~ ss_intercept_point_x,
           intercept_hit_location_5 == 5 ~ third_intercept_point_x,
           intercept_hit_location_5 == 7 ~ left_intercept_point_x,
           intercept_hit_location_5 == 8 ~ center_intercept_point_x,
           intercept_hit_location_5 == 9 ~ right_intercept_point_x),
         #intercept point on y
         intercept_point_y_5 = case_when(
           intercept_hit_location_5 == 3 ~ first_intercept_point_y,
           intercept_hit_location_5 == 4 ~ second_intercept_point_y,
           intercept_hit_location_5 == 6 ~ ss_intercept_point_y,
           intercept_hit_location_5 == 5 ~ third_intercept_point_y,
           intercept_hit_location_5 == 7 ~ left_intercept_point_y,
           intercept_hit_location_5 == 8 ~ center_intercept_point_y,
           intercept_hit_location_5 == 9 ~ right_intercept_point_y),
         # intercpet point distance from home
         intercept_point_distance_5 = sqrt(intercept_point_x_5^2 + intercept_point_y_5^2), 
         # intercept fielder angle from home
         intercept_fielder_angle_5 = case_when(
           intercept_hit_location_5  == 3 ~ atan((pos3_start_pos_x)/
                                                   (pos3_start_pos_y))*(180/pi),
           intercept_hit_location_5  == 4 ~ atan((pos4_start_pos_x)/
                                                   (pos4_start_pos_y))*(180/pi),
           intercept_hit_location_5  == 5 ~ atan((pos5_start_pos_x)/
                                                   (pos5_start_pos_y))*(180/pi),
           intercept_hit_location_5  == 6 ~ atan((pos6_start_pos_x)/
                                                   (pos6_start_pos_y))*(180/pi),
           intercept_hit_location_5  == 7 ~ atan((pos7_start_pos_x)/
                                                   (pos7_start_pos_y))*(180/pi),
           intercept_hit_location_5  == 8 ~ atan((pos8_start_pos_x)/
                                                   (pos8_start_pos_y))*(180/pi),
           intercept_hit_location_5  == 9 ~ atan((pos9_start_pos_x)/
                                                   (pos9_start_pos_y))*(180/pi),
           TRUE ~ NaN),
         # intercept fielder located on x
         intercept_fielder_x_5 = case_when(
           intercept_hit_location_5 == 3 ~ pos3_start_pos_x,
           intercept_hit_location_5 == 4 ~ pos4_start_pos_x,
           intercept_hit_location_5 == 5 ~ pos5_start_pos_x,
           intercept_hit_location_5 == 6 ~ pos6_start_pos_x,
           intercept_hit_location_5 == 7 ~ pos7_start_pos_x,
           intercept_hit_location_5 == 8 ~ pos8_start_pos_x,
           intercept_hit_location_5 == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         # intercept fielder located on y
         intercept_fielder_y_5 = case_when(
           intercept_hit_location_5 == 3 ~ pos3_start_pos_y,
           intercept_hit_location_5 == 4 ~ pos4_start_pos_y,
           intercept_hit_location_5 == 5 ~ pos5_start_pos_y,
           intercept_hit_location_5 == 6 ~ pos6_start_pos_y,
           intercept_hit_location_5 == 7 ~ pos7_start_pos_y,
           intercept_hit_location_5 == 8 ~ pos8_start_pos_y,
           intercept_hit_location_5 == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         # intercept fielder distance from home plate
         intercept_fielder_distance_5 = sqrt(intercept_fielder_x_5^2 + intercept_fielder_y_5^2),
         # intercept fielder distance to intercept point on x
         fielder_distance_to_intercept_x_5 = case_when(
           intercept_hit_location_5 == 3 ~ first_distance_to_IP_x,
           intercept_hit_location_5 == 4 ~ second_distance_to_IP_x,
           intercept_hit_location_5 == 5 ~ third_distance_to_IP_x,
           intercept_hit_location_5 == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location_5 == 7 ~ left_distance_to_IP_x,
           intercept_hit_location_5 == 8 ~ center_distance_to_IP_x,
           intercept_hit_location_5 == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on y
         fielder_distance_to_intercept_y_5 = case_when(
           intercept_hit_location_5 == 3 ~ first_distance_to_IP_y,
           intercept_hit_location_5 == 4 ~ second_distance_to_IP_y,
           intercept_hit_location_5 == 5 ~ third_distance_to_IP_y,
           intercept_hit_location_5 == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location_5 == 7 ~ left_distance_to_IP_y,
           intercept_hit_location_5 == 8 ~ center_distance_to_IP_y,
           intercept_hit_location_5 == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
         
         # first base distance from intercept fielder
         intercept_base_distance_x_5 = intercept_fielder_x_5 - sin(45*(pi/180))*90,
         intercept_base_distance_y_5 = intercept_fielder_y_5 - cos(45*(pi/180))*90,
         intercept_base_distance_5 = sqrt(intercept_base_distance_x_5^2+intercept_base_distance_y_5^2),
         # first base distance from the intercept point
         intercept_point_base_distance_x_5 = intercept_point_x_5 - sin(45*(pi/180))*90,
         intercept_point_base_distance_y_5 = intercept_point_y_5 - cos(45*(pi/180))*90,
         intercept_point_base_distance_5 = sqrt(intercept_point_base_distance_x_5^2+intercept_point_base_distance_y_5^2),
         # fielder angle to intercept point
         fielder_distance_to_intercept_5 = sqrt(fielder_distance_to_intercept_x_5^2 + fielder_distance_to_intercept_y_5^2),
         hit_angle_zero_5 = hit_angle - intercept_fielder_angle_5,
         intercept_fielder_zero_x_5 = sin(0*(pi/180))*intercept_fielder_distance_5,
         intercept_fielder_zero_y_5 = cos(0*(pi/180))*intercept_fielder_distance_5,
         intercept_point_zero_x_5 = sin(hit_angle_zero_5*(pi/180))*intercept_point_distance_5,
         intercept_point_zero_y_5 = cos(hit_angle_zero_5*(pi/180))*intercept_point_distance_5,
         fielder_distance_to_intercept_zero_x_5 = intercept_point_zero_x_5 - intercept_fielder_zero_x_5,
         fielder_distance_to_intercept_zero_y_5 = intercept_point_zero_y_5 - intercept_fielder_zero_y_5,
         
         # SIX
         intercept_point_x_6 = case_when(
           intercept_hit_location_6 == 3 ~ first_intercept_point_x,
           intercept_hit_location_6 == 4 ~ second_intercept_point_x,
           intercept_hit_location_6 == 6 ~ ss_intercept_point_x,
           intercept_hit_location_6 == 5 ~ third_intercept_point_x,
           intercept_hit_location_6 == 7 ~ left_intercept_point_x,
           intercept_hit_location_6 == 8 ~ center_intercept_point_x,
           intercept_hit_location_6 == 9 ~ right_intercept_point_x),
         #intercept point on y
         intercept_point_y_6 = case_when(
           intercept_hit_location_6 == 3 ~ first_intercept_point_y,
           intercept_hit_location_6 == 4 ~ second_intercept_point_y,
           intercept_hit_location_6 == 6 ~ ss_intercept_point_y,
           intercept_hit_location_6 == 5 ~ third_intercept_point_y,
           intercept_hit_location_6 == 7 ~ left_intercept_point_y,
           intercept_hit_location_6 == 8 ~ center_intercept_point_y,
           intercept_hit_location_6 == 9 ~ right_intercept_point_y),
         # intercpet point distance from home
         intercept_point_distance_6 = sqrt(intercept_point_x_6^2 + intercept_point_y_6^2), 
         # intercept fielder angle from home
         intercept_fielder_angle_6 = case_when(
           intercept_hit_location_6  == 3 ~ atan((pos3_start_pos_x)/
                                                   (pos3_start_pos_y))*(180/pi),
           intercept_hit_location_6  == 4 ~ atan((pos4_start_pos_x)/
                                                   (pos4_start_pos_y))*(180/pi),
           intercept_hit_location_6  == 5 ~ atan((pos5_start_pos_x)/
                                                   (pos5_start_pos_y))*(180/pi),
           intercept_hit_location_6  == 6 ~ atan((pos6_start_pos_x)/
                                                   (pos6_start_pos_y))*(180/pi),
           intercept_hit_location_6  == 7 ~ atan((pos7_start_pos_x)/
                                                   (pos7_start_pos_y))*(180/pi),
           intercept_hit_location_6  == 8 ~ atan((pos8_start_pos_x)/
                                                   (pos8_start_pos_y))*(180/pi),
           intercept_hit_location_6  == 9 ~ atan((pos9_start_pos_x)/
                                                   (pos9_start_pos_y))*(180/pi),
           TRUE ~ NaN),
         # intercept fielder located on x
         intercept_fielder_x_6 = case_when(
           intercept_hit_location_6 == 3 ~ pos3_start_pos_x,
           intercept_hit_location_6 == 4 ~ pos4_start_pos_x,
           intercept_hit_location_6 == 5 ~ pos5_start_pos_x,
           intercept_hit_location_6 == 6 ~ pos6_start_pos_x,
           intercept_hit_location_6 == 7 ~ pos7_start_pos_x,
           intercept_hit_location_6 == 8 ~ pos8_start_pos_x,
           intercept_hit_location_6 == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         # intercept fielder located on y
         intercept_fielder_y_6 = case_when(
           intercept_hit_location_6 == 3 ~ pos3_start_pos_y,
           intercept_hit_location_6 == 4 ~ pos4_start_pos_y,
           intercept_hit_location_6 == 5 ~ pos5_start_pos_y,
           intercept_hit_location_6 == 6 ~ pos6_start_pos_y,
           intercept_hit_location_6 == 7 ~ pos7_start_pos_y,
           intercept_hit_location_6 == 8 ~ pos8_start_pos_y,
           intercept_hit_location_6 == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         # intercept fielder distance from home plate
         intercept_fielder_distance_6 = sqrt(intercept_fielder_x_6^2 + intercept_fielder_y_6^2),
         # intercept fielder distance to intercept point on x
         fielder_distance_to_intercept_x_6 = case_when(
           intercept_hit_location_6 == 3 ~ first_distance_to_IP_x,
           intercept_hit_location_6 == 4 ~ second_distance_to_IP_x,
           intercept_hit_location_6 == 5 ~ third_distance_to_IP_x,
           intercept_hit_location_6 == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location_6 == 7 ~ left_distance_to_IP_x,
           intercept_hit_location_6 == 8 ~ center_distance_to_IP_x,
           intercept_hit_location_6 == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on y
         fielder_distance_to_intercept_y_6 = case_when(
           intercept_hit_location_6 == 3 ~ first_distance_to_IP_y,
           intercept_hit_location_6 == 4 ~ second_distance_to_IP_y,
           intercept_hit_location_6 == 5 ~ third_distance_to_IP_y,
           intercept_hit_location_6 == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location_6 == 7 ~ left_distance_to_IP_y,
           intercept_hit_location_6 == 8 ~ center_distance_to_IP_y,
           intercept_hit_location_6 == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
         
         # first base distance from intercept fielder
         intercept_base_distance_x_6 = intercept_fielder_x_6 - sin(45*(pi/180))*90,
         intercept_base_distance_y_6 = intercept_fielder_y_6 - cos(45*(pi/180))*90,
         intercept_base_distance_6 = sqrt(intercept_base_distance_x_6^2+intercept_base_distance_y_6^2),
         # first base distance from the intercept point
         intercept_point_base_distance_x_6 = intercept_point_x_6 - sin(45*(pi/180))*90,
         intercept_point_base_distance_y_6 = intercept_point_y_6 - cos(45*(pi/180))*90,
         intercept_point_base_distance_6 = sqrt(intercept_point_base_distance_x_6^2+intercept_point_base_distance_y_6^2),
         # fielder angle to intercept point
         fielder_distance_to_intercept_6 = sqrt(fielder_distance_to_intercept_x_6^2 + fielder_distance_to_intercept_y_6^2),
         hit_angle_zero_6 = hit_angle - intercept_fielder_angle_6,
         intercept_fielder_zero_x_6 = sin(0*(pi/180))*intercept_fielder_distance_6,
         intercept_fielder_zero_y_6 = cos(0*(pi/180))*intercept_fielder_distance_6,
         intercept_point_zero_x_6 = sin(hit_angle_zero_6*(pi/180))*intercept_point_distance_6,
         intercept_point_zero_y_6 = cos(hit_angle_zero_6*(pi/180))*intercept_point_distance_6,
         fielder_distance_to_intercept_zero_x_6 = intercept_point_zero_x_6 - intercept_fielder_zero_x_6,
         fielder_distance_to_intercept_zero_y_6 = intercept_point_zero_y_6 - intercept_fielder_zero_y_6,
         
         # SEVEN 
         intercept_point_x_7 = case_when(
           intercept_hit_location_7 == 3 ~ first_intercept_point_x,
           intercept_hit_location_7 == 4 ~ second_intercept_point_x,
           intercept_hit_location_7 == 6 ~ ss_intercept_point_x,
           intercept_hit_location_7 == 5 ~ third_intercept_point_x,
           intercept_hit_location_7 == 7 ~ left_intercept_point_x,
           intercept_hit_location_7 == 8 ~ center_intercept_point_x,
           intercept_hit_location_7 == 9 ~ right_intercept_point_x),
         #intercept point on y
         intercept_point_y_7 = case_when(
           intercept_hit_location_7 == 3 ~ first_intercept_point_y,
           intercept_hit_location_7 == 4 ~ second_intercept_point_y,
           intercept_hit_location_7 == 6 ~ ss_intercept_point_y,
           intercept_hit_location_7 == 5 ~ third_intercept_point_y,
           intercept_hit_location_7 == 7 ~ left_intercept_point_y,
           intercept_hit_location_7 == 8 ~ center_intercept_point_y,
           intercept_hit_location_7 == 9 ~ right_intercept_point_y),
         # intercpet point distance from home
         intercept_point_distance_7 = sqrt(intercept_point_x_6^2 + intercept_point_y_6^2), 
         # intercept fielder angle from home
         intercept_fielder_angle_7 = case_when(
           intercept_hit_location_7  == 3 ~ atan((pos3_start_pos_x)/
                                                   (pos3_start_pos_y))*(180/pi),
           intercept_hit_location_7  == 4 ~ atan((pos4_start_pos_x)/
                                                   (pos4_start_pos_y))*(180/pi),
           intercept_hit_location_7  == 5 ~ atan((pos5_start_pos_x)/
                                                   (pos5_start_pos_y))*(180/pi),
           intercept_hit_location_7  == 6 ~ atan((pos6_start_pos_x)/
                                                   (pos6_start_pos_y))*(180/pi),
           intercept_hit_location_7  == 7 ~ atan((pos7_start_pos_x)/
                                                   (pos7_start_pos_y))*(180/pi),
           intercept_hit_location_7  == 8 ~ atan((pos8_start_pos_x)/
                                                   (pos8_start_pos_y))*(180/pi),
           intercept_hit_location_7  == 9 ~ atan((pos9_start_pos_x)/
                                                   (pos9_start_pos_y))*(180/pi),
           TRUE ~ NaN),
         # intercept fielder located on x
         intercept_fielder_x_7 = case_when(
           intercept_hit_location_7 == 3 ~ pos3_start_pos_x,
           intercept_hit_location_7 == 4 ~ pos4_start_pos_x,
           intercept_hit_location_7 == 5 ~ pos5_start_pos_x,
           intercept_hit_location_7 == 6 ~ pos6_start_pos_x,
           intercept_hit_location_7 == 7 ~ pos7_start_pos_x,
           intercept_hit_location_7 == 8 ~ pos8_start_pos_x,
           intercept_hit_location_7 == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         # intercept fielder located on y
         intercept_fielder_y_7 = case_when(
           intercept_hit_location_7 == 3 ~ pos3_start_pos_y,
           intercept_hit_location_7 == 4 ~ pos4_start_pos_y,
           intercept_hit_location_7 == 5 ~ pos5_start_pos_y,
           intercept_hit_location_7 == 6 ~ pos6_start_pos_y,
           intercept_hit_location_7 == 7 ~ pos7_start_pos_y,
           intercept_hit_location_7 == 8 ~ pos8_start_pos_y,
           intercept_hit_location_7 == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         # intercept fielder distance from home plate
         intercept_fielder_distance_7 = sqrt(intercept_fielder_x_7^2 + intercept_fielder_y_7^2),
         # intercept fielder distance to intercept point on x
         fielder_distance_to_intercept_x_7 = case_when(
           intercept_hit_location_7 == 3 ~ first_distance_to_IP_x,
           intercept_hit_location_7 == 4 ~ second_distance_to_IP_x,
           intercept_hit_location_7 == 5 ~ third_distance_to_IP_x,
           intercept_hit_location_7 == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location_7 == 7 ~ left_distance_to_IP_x,
           intercept_hit_location_7 == 8 ~ center_distance_to_IP_x,
           intercept_hit_location_7 == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         # intercept fielder distance to intercept point on y
         fielder_distance_to_intercept_y_7 = case_when(
           intercept_hit_location_7 == 3 ~ first_distance_to_IP_y,
           intercept_hit_location_7 == 4 ~ second_distance_to_IP_y,
           intercept_hit_location_7 == 5 ~ third_distance_to_IP_y,
           intercept_hit_location_7 == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location_7 == 7 ~ left_distance_to_IP_y,
           intercept_hit_location_7 == 8 ~ center_distance_to_IP_y,
           intercept_hit_location_7 == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
         
         # first base distance from intercept fielder
         intercept_base_distance_x_7 = intercept_fielder_x_7 - sin(45*(pi/180))*90,
         intercept_base_distance_y_7 = intercept_fielder_y_7 - cos(45*(pi/180))*90,
         intercept_base_distance_7 = sqrt(intercept_base_distance_x_7^2+intercept_base_distance_y_7^2),
         # first base distance from the intercept point
         intercept_point_base_distance_x_7 = intercept_point_x_7 - sin(45*(pi/180))*90,
         intercept_point_base_distance_y_7 = intercept_point_y_7 - cos(45*(pi/180))*90,
         intercept_point_base_distance_7 = sqrt(intercept_point_base_distance_x_7^2+intercept_point_base_distance_y_7^2),
         # fielder angle to intercept point
         fielder_distance_to_intercept_7 = sqrt(fielder_distance_to_intercept_x_7^2 + fielder_distance_to_intercept_y_7^2),
         hit_angle_zero_7 = hit_angle - intercept_fielder_angle_7,
         intercept_fielder_zero_x_7 = sin(0*(pi/180))*intercept_fielder_distance_7,
         intercept_fielder_zero_y_7 = cos(0*(pi/180))*intercept_fielder_distance_7,
         intercept_point_zero_x_7 = sin(hit_angle_zero_7*(pi/180))*intercept_point_distance_7,
         intercept_point_zero_y_7 = cos(hit_angle_zero_7*(pi/180))*intercept_point_distance_7,
         fielder_distance_to_intercept_zero_x_7 = intercept_point_zero_x_7 - intercept_fielder_zero_x_7,
         fielder_distance_to_intercept_zero_y_7 = intercept_point_zero_y_7 - intercept_fielder_zero_y_7)%>%
  
  mutate(dist=hit_distance_sc - hit_distance,
         Hit = factor(ifelse(result=="Hit",1,0)),
         delta_angle_1 = hit_angle - intercept_fielder_angle_1,
         delta_distance_1 = intercept_point_distance_1 - intercept_fielder_distance_1,
         delta_angle_2 = hit_angle - intercept_fielder_angle_2,
         delta_distance_2 = intercept_point_distance_2 - intercept_fielder_distance_2,
         delta_angle_3 = hit_angle - intercept_fielder_angle_3,
         delta_distance_3 = intercept_point_distance_3 - intercept_fielder_distance_3,
         delta_angle_4 = hit_angle - intercept_fielder_angle_4,
         delta_distance_4 = intercept_point_distance_4 - intercept_fielder_distance_4,
         delta_angle_5 = hit_angle - intercept_fielder_angle_5,
         delta_distance_5 = intercept_point_distance_5 - intercept_fielder_distance_5,
         delta_angle_6 = hit_angle - intercept_fielder_angle_6,
         delta_distance_6 = intercept_point_distance_6 - intercept_fielder_distance_6,
         
         delta_angle_7 = hit_angle - intercept_fielder_angle_7,
         delta_distance_7 = intercept_point_distance_7 - intercept_fielder_distance_7)


ggplot(sample%>%filter(player_name == "Gallo, Joey")%>%slice(191))+
  geom_point(aes(x=intercept_point_x_1, y=intercept_point_y_1))+
  geom_point(aes(x=intercept_point_x_2, y=intercept_point_y_2))+
  geom_point(aes(x=hc_x_, y=hc_y_), size=3, color="forestgreen")+
  geom_point(aes(x=intercept_fielder_x_1, y=intercept_fielder_y_1), size=2, color="goldenrod2")+
  geom_point(aes(x=intercept_fielder_x_2, y=intercept_fielder_y_2), size=2, color="goldenrod2")+
  
  geom_point(aes(x=adjusted_hc_x_, y=adjusted_hc_y_), color="red", size=2)+
  
  geom_mlb_stadium(stadium_ids = "rays", stadium_transform_coords = T, stadium_segments = "all")+
  geom_point(aes(x=intercept_point_x_3, y=intercept_point_y_3))+
  geom_point(aes(x=intercept_fielder_x_3, y=intercept_fielder_y_3), size=2, color="goldenrod2")+
  geom_segment(aes(x=intercept_point_x_3,y=intercept_point_y_3, xend=intercept_fielder_x_3, yend=intercept_fielder_y_3))+
  geom_point(aes(x=intercept_point_x_4, y=intercept_point_y_4))+
  geom_point(aes(x=intercept_fielder_x_4, y=intercept_fielder_y_4), size=2, color="goldenrod2")+
  geom_segment(aes(x=intercept_point_x_4,y=intercept_point_y_4, xend=intercept_fielder_x_4, yend=intercept_fielder_y_4))+
  geom_point(aes(x=intercept_point_x_5, y=intercept_point_y_5))+
  geom_point(aes(x=intercept_fielder_x_5, y=intercept_fielder_y_5), size=2, color="goldenrod2")+
  geom_segment(aes(x=intercept_point_x_5,y=intercept_point_y_5, xend=intercept_fielder_x_5, yend=intercept_fielder_y_5))+
  geom_point(aes(x=intercept_point_x_6, y=intercept_point_y_6))+
  geom_point(aes(x=intercept_fielder_x_6, y=intercept_fielder_y_6), size=2, color="goldenrod2")+
  geom_segment(aes(x=intercept_point_x_6,y=intercept_point_y_6, xend=intercept_fielder_x_6, yend=intercept_fielder_y_6))+
  geom_point(aes(x=intercept_point_x_7, y=intercept_point_y_7))+
  geom_point(aes(x=intercept_fielder_x_7, y=intercept_fielder_y_7), size=2, color="goldenrod2")+
  geom_segment(aes(x=intercept_point_x_7,y=intercept_point_y_6, xend=intercept_fielder_x_7, yend=intercept_fielder_y_7))+
  geom_segment(aes(x=intercept_point_x_1,y=intercept_point_y_1, xend=intercept_fielder_x_1, yend=intercept_fielder_y_1))+
  geom_segment(aes(x=intercept_point_x_2,y=intercept_point_y_2, xend=intercept_fielder_x_2, yend=intercept_fielder_y_2))+
  geom_segment(aes(x=0, y=0, xend=sin(hit_angle*(pi/180))*400, yend=cos(hit_angle*(pi/180))*400),
               linetype=2)

sample%>%filter(player_name == "Gallo, Joey")%>%slice(191)%>%
  select(fielder_distance_to_intercept_1, fielder_distance_to_intercept_2, fielder_distance_to_intercept_3, 
         fielder_distance_to_intercept_4, fielder_distance_to_intercept_5, fielder_distance_to_intercept_6, 
         fielder_distance_to_intercept_7)

