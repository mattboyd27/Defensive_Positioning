library(tidyverse)
library(baseballr)
library(GeomMLBStadiums)
library(caret)
library(janitor)
library(scales)
library(gbm)

# Scrape 2018 Statcast data
data1=scrape_statcast_savant(start_date="2018-03-28",end_date="2018-04-05",player_type="batter")
data2=scrape_statcast_savant(start_date="2018-04-06",end_date="2018-04-13",player_type="batter")
data3=scrape_statcast_savant(start_date="2018-04-14",end_date="2018-04-20",player_type="batter")
data4=scrape_statcast_savant(start_date="2018-04-18",end_date="2018-04-28",player_type="batter")
data5=scrape_statcast_savant(start_date="2018-04-29",end_date="2018-05-06",player_type="batter")
data6=scrape_statcast_savant(start_date="2018-05-07",end_date="2018-05-14",player_type="batter")
data7=scrape_statcast_savant(start_date="2018-05-15",end_date="2018-05-22",player_type="batter")
data8=scrape_statcast_savant(start_date="2018-05-23",end_date="2018-05-29",player_type="batter")
data9=scrape_statcast_savant(start_date="2018-05-30",end_date="2018-06-06",player_type="batter")
data10=scrape_statcast_savant(start_date="2018-06-07",end_date="2018-06-15",player_type="batter")
data11=scrape_statcast_savant(start_date="2018-06-16",end_date="2018-06-23",player_type="batter")
data12=scrape_statcast_savant(start_date="2018-06-24",end_date="2018-06-29",player_type="batter")
data13=scrape_statcast_savant(start_date="2018-06-30",end_date="2018-07-07",player_type="batter")
data14=scrape_statcast_savant(start_date="2018-07-08",end_date="2018-07-15",player_type="batter")
data15=scrape_statcast_savant(start_date="2018-07-16",end_date="2018-07-23",player_type="batter")
data16=scrape_statcast_savant(start_date="2018-07-24",end_date="2018-07-29",player_type="batter")
data17=scrape_statcast_savant(start_date="2018-07-30",end_date="2018-08-07",player_type="batter")
data18=scrape_statcast_savant(start_date="2018-08-08",end_date="2018-08-15",player_type="batter")
data19=scrape_statcast_savant(start_date="2018-08-16",end_date="2018-08-23",player_type="batter")
data20=scrape_statcast_savant(start_date="2018-08-24",end_date="2018-09-01",player_type="batter")
data21=scrape_statcast_savant(start_date="2018-09-02",end_date="2018-09-10",player_type="batter")
data22=scrape_statcast_savant(start_date="2018-09-11",end_date="2018-09-18",player_type="batter")
data23=scrape_statcast_savant(start_date="2018-09-18",end_date="2018-09-26",player_type="batter")
data24=scrape_statcast_savant(start_date="2018-09-27",end_date="2018-10-04",player_type="batter")

data2018=rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10, data11,data12,
           data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24)

rm(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10, data11,data12,
   data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24)


# Scrape 2019 Statcast data
data1=scrape_statcast_savant(start_date="2019-03-28",end_date="2019-04-05",player_type="batter")
data2=scrape_statcast_savant(start_date="2019-04-06",end_date="2019-04-13",player_type="batter")
data3=scrape_statcast_savant(start_date="2019-04-14",end_date="2019-04-20",player_type="batter")
data4=scrape_statcast_savant(start_date="2019-04-19",end_date="2019-04-28",player_type="batter")
data5=scrape_statcast_savant(start_date="2019-04-29",end_date="2019-05-06",player_type="batter")
data6=scrape_statcast_savant(start_date="2019-05-07",end_date="2019-05-14",player_type="batter")
data7=scrape_statcast_savant(start_date="2019-05-15",end_date="2019-05-22",player_type="batter")
data8=scrape_statcast_savant(start_date="2019-05-23",end_date="2019-05-29",player_type="batter")
data9=scrape_statcast_savant(start_date="2019-05-30",end_date="2019-06-06",player_type="batter")
data10=scrape_statcast_savant(start_date="2019-06-07",end_date="2019-06-15",player_type="batter")
data11=scrape_statcast_savant(start_date="2019-06-16",end_date="2019-06-23",player_type="batter")
data12=scrape_statcast_savant(start_date="2019-06-24",end_date="2019-06-29",player_type="batter")
data13=scrape_statcast_savant(start_date="2019-06-30",end_date="2019-07-07",player_type="batter")
data14=scrape_statcast_savant(start_date="2019-07-08",end_date="2019-07-15",player_type="batter")
data15=scrape_statcast_savant(start_date="2019-07-16",end_date="2019-07-23",player_type="batter")
data16=scrape_statcast_savant(start_date="2019-07-24",end_date="2019-07-29",player_type="batter")
data17=scrape_statcast_savant(start_date="2019-07-30",end_date="2019-08-07",player_type="batter")
data18=scrape_statcast_savant(start_date="2019-08-08",end_date="2019-08-15",player_type="batter")
data19=scrape_statcast_savant(start_date="2019-08-16",end_date="2019-08-23",player_type="batter")
data20=scrape_statcast_savant(start_date="2019-08-24",end_date="2019-09-01",player_type="batter")
data21=scrape_statcast_savant(start_date="2019-09-02",end_date="2019-09-10",player_type="batter")
data22=scrape_statcast_savant(start_date="2019-09-11",end_date="2019-09-18",player_type="batter")
data23=scrape_statcast_savant(start_date="2019-09-19",end_date="2019-09-26",player_type="batter")
data24=scrape_statcast_savant(start_date="2019-09-27",end_date="2019-10-04",player_type="batter")

data2019=rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10, data11,data12,
           data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24)

rm(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10, data11,data12,
   data13,data14,data15,data16,data17,data18,data19,data20,data21,data22,data23,data24)

# Scrape 2020 data
data1=scrape_statcast_savant(start_date="2020-07-22",end_date="2020-07-30",player_type="batter")
data2=scrape_statcast_savant(start_date="2020-08-01",end_date="2020-08-09",player_type="batter")
data3=scrape_statcast_savant(start_date="2020-08-10",end_date="2020-08-17",player_type="batter")
data4=scrape_statcast_savant(start_date="2020-08-18",end_date="2020-08-25",player_type="batter")
data5=scrape_statcast_savant(start_date="2020-08-26",end_date="2020-09-02",player_type="batter")
data6=scrape_statcast_savant(start_date="2020-09-03",end_date="2020-09-10",player_type="batter")
data7=scrape_statcast_savant(start_date="2020-09-11",end_date="2020-09-19",player_type="batter")
data8=scrape_statcast_savant(start_date="2020-09-20",end_date="2020-09-28",player_type="batter")

data2020=rbind(data1,data2,data3,data4,data5,data6,data7,data8)

rm(data1,data2,data3,data4,data5,data6,data7,data8)

# Scrape 2021 data up until June 27, 2021
data1=scrape_statcast_savant(start_date="2021-03-28",end_date="2021-04-05",player_type="batter")
data2=scrape_statcast_savant(start_date="2021-04-06",end_date="2021-04-13",player_type="batter")
data3=scrape_statcast_savant(start_date="2021-04-14",end_date="2021-04-20",player_type="batter")
data4=scrape_statcast_savant(start_date="2021-04-21",end_date="2021-04-28",player_type="batter")
data5=scrape_statcast_savant(start_date="2021-04-29",end_date="2021-05-06",player_type="batter")
data6=scrape_statcast_savant(start_date="2021-05-07",end_date="2021-05-14",player_type="batter")
data7=scrape_statcast_savant(start_date="2021-05-15",end_date="2021-05-22",player_type="batter")
data8=scrape_statcast_savant(start_date="2021-05-23",end_date="2021-05-29",player_type="batter")
data9=scrape_statcast_savant(start_date="2021-05-30",end_date="2021-06-06",player_type="batter")
data10=scrape_statcast_savant(start_date="2021-06-07",end_date="2021-06-15",player_type="batter")
data11=scrape_statcast_savant(start_date="2021-06-16",end_date="2021-06-23",player_type="batter")
data12=scrape_statcast_savant(start_date="2021-06-24",end_date="2021-06-27",player_type="batter")


data2021=rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10, data11,data12)

rm(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10, data11,data12)


# Combine years
data=rbind(data2021,data2020,data2019,data2018)
rm(data2018,data2019,data2020,data2021,season2020)



# Load in sprint speed for all players from 2018-2021 from Baseball Savant csv
speed = c()
for(i in 18:21){
  speed1 = read_csv(paste0("sprint_speed_20",i,".csv"))%>%
    select(player_id, sprint_speed)%>%
    mutate(year = paste0(20,i))
  speed = rbind(speed, speed1)
  rm(speed1)
  rm(i)
}


data = data%>%
  # Transform data to coordinates provided by MLBAM
  mlbam_xy_transformation()%>%
  # Filter balls that aren't 4 and strikes that aren't 3 and batted balls that are hit in front of home plate
  filter(balls != 4, strikes !=3, hc_y_>0)%>%
  mutate(
    # Indicator if infield shift was used
    shift=case_when(
      if_fielding_alignment == "Infield shift" ~ 1,
      if_fielding_alignment %in% c("Strategic", "Standard") ~ 0,
      TRUE ~ NA_real_),
    # Make strategic shifts considered standard
    if_fielding_alignment=case_when(
      if_fielding_alignment == "Infield shift" ~ "Infield shift",
      if_fielding_alignment %in% c("Strategic", "Standard") ~ "Standard",
      TRUE ~ NA_character_),
    # Create spray angle from batted ball coordinates
    hit_angle=atan(hc_x_/(hc_y_))*(180/pi),
    # Indicator if the ball was hard hit
    hard_hit=ifelse(launch_speed>=95,1,0),
    hard_hit=as.factor(hard_hit),
    # Column for if infield or outfield player touched the ball first
    touched=case_when(
      hit_location %in% c(1,2,3,4,5,6) |
        events %in% c("grounded_into_double_play", "fielders_choice") ~ "Infield",
      hit_location %in% c(7,8,9) ~ "Outfield",
      TRUE ~ NA_character_),
    touched=as.factor(touched), 
    # Column if the ball was a hit or not
    result=case_when(
      events %in% c("single","triple","double") |
        # Outfielders cant get to a ball but runners think they do so they drop and get force out. 
        # There also some missed catches
        (touched == "Outfield" & events == "force_out") ~ "Hit", 
      # Errors are outs BUT they can get deflected so location is different
      events %in% c("field_error") ~ "Out",
      # Counting home runs as NA to get rid of them
      events == "home_run" ~ NA_character_,
      description %in% c("hit_into_play") ~ "Out",
      TRUE ~ NA_character_),
    # Result but more specific to outcome
    result2 = case_when(
      events == "triple" ~ "triple",
      events == "double" ~ "double",
      events == "single" ~ "single",
      result == "Out" ~ "out",
      TRUE ~ NA_character_),
    game_year = as.character(game_year),
    # Fielding team 
    fielding_team = case_when(
      inning_topbot == "Top" ~ home_team,
      inning_topbot == "Bot" ~ away_team,
      TRUE ~ NA_character_),
    # Indicator if player is on first
    on_1b = case_when(
      !is.na(on_1b) ~ "1",
      TRUE ~ "0"),
    # Indicator if player is on second
    on_2b = case_when(
      !is.na(on_2b) ~ "1",
      TRUE ~ "0"),
    # Indicator if player is on third
    on_3b = case_when(
      !is.na(on_3b) ~ "1",
      TRUE ~ "0"),
    on_1b = as.factor(on_1b),
    on_2b = as.factor(on_2b),
    on_3b = as.factor(on_3b))%>%
  # Join sprint speed data
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

# Load in position player coordinates for all plays from March 28, 2018 - June 27, 2021
sample = c()
for(i in 18:21){
  sample1 = read_csv(paste0("20",i,"_position_player_coordinates.csv"))
  sample = rbind(sample, sample1)
  rm(sample1)
  rm(i)
}




sample=sample%>%
  mutate(
    # Make X and Y coordinates a numeric value
    pos9_start_pos_x = as.numeric(pos9_start_pos_x),
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
    
    # Create an angle from home plate to each fielders position in the field
    right_angle=atan(pos9_start_pos_x/(pos9_start_pos_y))*(180/pi),
    center_angle=atan(pos8_start_pos_x/(pos8_start_pos_y))*(180/pi),
    left_angle=atan(pos7_start_pos_x/(pos7_start_pos_y))*(180/pi),
    ss_angle=atan(pos6_start_pos_x/(pos6_start_pos_y))*(180/pi),
    third_angle=atan(pos5_start_pos_x/(pos5_start_pos_y))*(180/pi),
    second_angle=atan(pos4_start_pos_x/(pos4_start_pos_y))*(180/pi),
    first_angle=atan(pos3_start_pos_x/(pos3_start_pos_y))*(180/pi),
    
    # Create distance from home plate to each fielders position in the field
    right_distance = sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
    center_distance = sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
    left_distance = sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
    ss_distance = sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
    third_distance = sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
    second_distance = sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
    first_distance = sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
    # Create distance from home plate to each fielders position in the field
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
  # Join the data with the Statcast data to combine plays together
  inner_join(data, by=c("game_pk", "player_at_bat"="batter", "pitcher"="pitcher","at_bat_number",
                        "stringer_hit_trajectory"="bb_type", "game_date"))%>%
  # Remove any values with no fielder coordinates
  drop_na(pos9_start_pos_x)%>%
  # Create alternative hit distance based on the X and Y coordinates of the batted ball
  mutate(hit_distance = sqrt(hc_x_^2+hc_y_^2))


sample = sample%>%
  
  # Filter spray angle betweeen -60 and 60, the pitcher or catcher the first ones to touch it, and no foul outs
  filter(hit_angle > (-60) & hit_angle < 60, !hit_location %in% c(1,2), !grepl("foul terr", des))%>%
  
  # Creating a new hit coordinatw with the statcast hit distance as the actual distance from home
  mutate(adjusted_hc_x_ = sin(hit_angle*(pi/180))*hit_distance_sc,
         adjusted_hc_y_ = cos(hit_angle*(pi/180))*hit_distance_sc,
         
         # Creating an intercept point for a given batted ball where the SS is playing
         ss_intercept_point_from_home = case_when(
           
           # If the hit distance is less than the distance the fielder is from home, IP distance is the same as fielder distance
           hit_distance_sc <= sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) &
             (stringer_hit_trajectory != "popup" & stringer_hit_trajectory != "fly_ball") & 
             ss_distance < 230 & ss_base_distance < 180 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
           
           # If the fielder is far from home or its a popup, the intercept point is where the ball lands (Statcast distance)
           hit_distance_sc > sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2) | 
             stringer_hit_trajectory == "popup" | stringer_hit_trajectory == "fly_ball" | ss_distance > 230 |
             ss_base_distance > 180 ~ hit_distance_sc),
         
         # Creating the intercept point X and Y
         ss_intercept_point_x = sin(hit_angle*(pi/180))*ss_intercept_point_from_home, 
         ss_intercept_point_y = cos(hit_angle*(pi/180))*ss_intercept_point_from_home,
         
         # Distance the SS is to their own intercept point
         ss_distance_to_IP_x = ss_intercept_point_x - pos6_start_pos_x,
         ss_distance_to_IP_y = ss_intercept_point_y - pos6_start_pos_y,
         ss_distance_to_intercept = sqrt(ss_distance_to_IP_x^2 + ss_distance_to_IP_y^2),
         
         # The same process occurs for all other positions, doesn't matter if it's infield or outfield.
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
         
         # Fielder 1 is labeled as the fielder closest to their own intercept point
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
         
         # Fielder 2 is labeled as the next closest fielder to their own intercept point
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
         
         
         # Fielder 3 is labeled as the next closest fielder to their own intercept point
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
         
         # # Fielder 4 is labeled as the next closest fielder to their own intercept point
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
         
         # f# Fielder 5 is labeled as the next closest fielder to their own intercept point
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
         
         # # Fielder 6 is labeled as the next closest fielder to their own intercept point
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
         
         
         # # Fielder 7 is labeled as the fielder farthest from their intercept point
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
         
         
         # Finding the intercept point X for fielder 1
         intercept_point_x_1 = case_when(
           intercept_hit_location_1 == 3 ~ first_intercept_point_x,
           intercept_hit_location_1 == 4 ~ second_intercept_point_x,
           intercept_hit_location_1 == 6 ~ ss_intercept_point_x,
           intercept_hit_location_1 == 5 ~ third_intercept_point_x,
           intercept_hit_location_1 == 7 ~ left_intercept_point_x,
           intercept_hit_location_1 == 8 ~ center_intercept_point_x,
           intercept_hit_location_1 == 9 ~ right_intercept_point_x),
         
         # Finding the intercept point Y for fielder 1
         intercept_point_y_1 = case_when(
           intercept_hit_location_1 == 3 ~ first_intercept_point_y,
           intercept_hit_location_1 == 4 ~ second_intercept_point_y,
           intercept_hit_location_1 == 6 ~ ss_intercept_point_y,
           intercept_hit_location_1 == 5 ~ third_intercept_point_y,
           intercept_hit_location_1 == 7 ~ left_intercept_point_y,
           intercept_hit_location_1 == 8 ~ center_intercept_point_y,
           intercept_hit_location_1 == 9 ~ right_intercept_point_y),
         
         # intercept point distance from home for fielder 1
         intercept_point_distance_1 = sqrt(intercept_point_x_1^2 + intercept_point_y_1^2), 
         
         
         # Fielder 1 angle from home
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
         
         # Fielder 1 location
         intercept_fielder_x_1 = case_when(
           intercept_hit_location_1 == 3 ~ pos3_start_pos_x,
           intercept_hit_location_1 == 4 ~ pos4_start_pos_x,
           intercept_hit_location_1 == 5 ~ pos5_start_pos_x,
           intercept_hit_location_1 == 6 ~ pos6_start_pos_x,
           intercept_hit_location_1 == 7 ~ pos7_start_pos_x,
           intercept_hit_location_1 == 8 ~ pos8_start_pos_x,
           intercept_hit_location_1 == 9 ~ pos9_start_pos_x,
           TRUE ~ NaN),
         intercept_fielder_y_1 = case_when(
           intercept_hit_location_1 == 3 ~ pos3_start_pos_y,
           intercept_hit_location_1 == 4 ~ pos4_start_pos_y,
           intercept_hit_location_1 == 5 ~ pos5_start_pos_y,
           intercept_hit_location_1 == 6 ~ pos6_start_pos_y,
           intercept_hit_location_1 == 7 ~ pos7_start_pos_y,
           intercept_hit_location_1 == 8 ~ pos8_start_pos_y,
           intercept_hit_location_1 == 9 ~ pos9_start_pos_y,
           TRUE ~ NaN),
         
         # Fielder 1 distance from home
         intercept_fielder_distance_1 = case_when(
           intercept_hit_location_1 == 3 ~ sqrt(pos3_start_pos_x^2 + pos3_start_pos_y^2),
           intercept_hit_location_1 == 4 ~ sqrt(pos4_start_pos_x^2 + pos4_start_pos_y^2),
           intercept_hit_location_1 == 5 ~ sqrt(pos5_start_pos_x^2 + pos5_start_pos_y^2),
           intercept_hit_location_1 == 6 ~ sqrt(pos6_start_pos_x^2 + pos6_start_pos_y^2),
           intercept_hit_location_1 == 7 ~ sqrt(pos7_start_pos_x^2 + pos7_start_pos_y^2),
           intercept_hit_location_1 == 8 ~ sqrt(pos8_start_pos_x^2 + pos8_start_pos_y^2),
           intercept_hit_location_1 == 9 ~ sqrt(pos9_start_pos_x^2 + pos9_start_pos_y^2),
           TRUE ~ NaN),
         
         # Fielder 1 distance to intercept point on X axis
         fielder_distance_to_intercept_x_1 = case_when(
           intercept_hit_location_1 == 3 ~ first_distance_to_IP_x,
           intercept_hit_location_1 == 4 ~ second_distance_to_IP_x,
           intercept_hit_location_1 == 5 ~ third_distance_to_IP_x,
           intercept_hit_location_1 == 6 ~ ss_distance_to_IP_x,
           intercept_hit_location_1 == 7 ~ left_distance_to_IP_x,
           intercept_hit_location_1 == 8 ~ center_distance_to_IP_x,
           intercept_hit_location_1 == 9 ~ right_distance_to_IP_x,
           TRUE ~ NaN),
         
         # Fielder 1 distance to intercept point on Y axis
         fielder_distance_to_intercept_y_1 = case_when(
           intercept_hit_location_1 == 3 ~ first_distance_to_IP_y,
           intercept_hit_location_1 == 4 ~ second_distance_to_IP_y,
           intercept_hit_location_1 == 5 ~ third_distance_to_IP_y,
           intercept_hit_location_1 == 6 ~ ss_distance_to_IP_y,
           intercept_hit_location_1 == 7 ~ left_distance_to_IP_y,
           intercept_hit_location_1 == 8 ~ center_distance_to_IP_y,
           intercept_hit_location_1 == 9 ~ right_distance_to_IP_y,
           TRUE ~ NaN),
         
         # Fielder 1 distance to first base
         intercept_base_distance_x_1 = intercept_fielder_x_1 - sin(45*(pi/180))*90,
         intercept_base_distance_y_1 = intercept_fielder_y_1 - cos(45*(pi/180))*90,
         intercept_base_distance_1 = sqrt(intercept_base_distance_x_1^2+intercept_base_distance_y_1^2),
         
         # Distance from intercept point to first base
         intercept_point_base_distance_x_1 = intercept_point_x_1 - sin(45*(pi/180))*90,
         intercept_point_base_distance_y_1 = intercept_point_y_1 - cos(45*(pi/180))*90,
         intercept_point_base_distance_1 = sqrt(intercept_point_base_distance_x_1^2+intercept_point_base_distance_y_1^2),
         
         # Fielder angle to intercept point
         fielder_distance_to_intercept_1 = sqrt(fielder_distance_to_intercept_x_1^2 + fielder_distance_to_intercept_y_1^2),
         hit_angle_zero_1 = hit_angle - intercept_fielder_angle_1,
         intercept_fielder_zero_x_1 = sin(0*(pi/180))*intercept_fielder_distance_1,
         intercept_fielder_zero_y_1 = cos(0*(pi/180))*intercept_fielder_distance_1,
         intercept_point_zero_x_1 = sin(hit_angle_zero_1*(pi/180))*intercept_point_distance_1,
         intercept_point_zero_y_1 = cos(hit_angle_zero_1*(pi/180))*intercept_point_distance_1,
         fielder_distance_to_intercept_zero_x_1 = intercept_point_zero_x_1 - intercept_fielder_zero_x_1,
         fielder_distance_to_intercept_zero_y_1 = intercept_point_zero_y_1 - intercept_fielder_zero_y_1,
         
         
         # THE SAME PROCESS OCCURS FOR ALL FIELDERS
         # Fielder 2
         intercept_point_x_2 = case_when(
           intercept_hit_location_2 == 3 ~ first_intercept_point_x,
           intercept_hit_location_2 == 4 ~ second_intercept_point_x,
           intercept_hit_location_2 == 6 ~ ss_intercept_point_x,
           intercept_hit_location_2 == 5 ~ third_intercept_point_x,
           intercept_hit_location_2 == 7 ~ left_intercept_point_x,
           intercept_hit_location_2 == 8 ~ center_intercept_point_x,
           intercept_hit_location_2 == 9 ~ right_intercept_point_x),
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
         
         
         # Fielder 3
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
         
         
         # Fielder 4
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
         
         # Fielder 5
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
         
         # Fielder 6
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
         
         # Fielder 7
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
  
  mutate(Hit = ifelse(result=="Hit","1","0"),
         # Creating difference in angle between the spray angle and the angle of the fielder
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



# Training gradient boosted model with if the batted ball is a hit as the response
model = gbm(Hit ~ launch_speed + launch_angle + sprint_speed +
                delta_angle_1 + intercept_point_distance_1 +  intercept_fielder_distance_1 +
                intercept_point_base_distance_1 + intercept_base_distance_1 +
                fielder_distance_to_intercept_1 +
                fielder_distance_to_intercept_zero_x_1 +
                fielder_distance_to_intercept_zero_y_1 + intercept_fielder_x_1 +
                intercept_fielder_y_1 + 
                
                delta_angle_2 + intercept_point_distance_2 +  
                intercept_fielder_distance_2 +
                intercept_point_base_distance_2 + intercept_base_distance_2 +
                fielder_distance_to_intercept_2 +
                fielder_distance_to_intercept_zero_x_2 +
                fielder_distance_to_intercept_zero_y_2 + intercept_fielder_x_2 +
                intercept_fielder_y_2 +
                
                delta_angle_3 + intercept_point_distance_3 +  intercept_fielder_distance_3 +
                intercept_point_base_distance_3 + intercept_base_distance_3 +
                fielder_distance_to_intercept_3 +
                fielder_distance_to_intercept_zero_x_3 +
                fielder_distance_to_intercept_zero_y_3 + intercept_fielder_x_3 +
                intercept_fielder_y_3 +
                
                delta_angle_4 + intercept_point_distance_4 +  intercept_fielder_distance_4 +
                intercept_point_base_distance_4 + intercept_base_distance_4 +
                fielder_distance_to_intercept_4 +
                fielder_distance_to_intercept_zero_x_4 +
                fielder_distance_to_intercept_zero_y_4 + intercept_fielder_x_4 +
                intercept_fielder_y_4 +
                
                delta_angle_5 + intercept_point_distance_5 +  intercept_fielder_distance_5 +
                intercept_point_base_distance_5 + intercept_base_distance_5 +
                fielder_distance_to_intercept_5 +
                fielder_distance_to_intercept_zero_x_5 +
                fielder_distance_to_intercept_zero_y_5 + intercept_fielder_x_5 +
                intercept_fielder_y_5 +
                
                delta_angle_6 + intercept_point_distance_6 +  intercept_fielder_distance_6 +
                intercept_point_base_distance_6 + intercept_base_distance_6 +
                fielder_distance_to_intercept_6 +
                fielder_distance_to_intercept_zero_x_6 +
                fielder_distance_to_intercept_zero_y_6 + intercept_fielder_x_6 +
                intercept_fielder_y_6 +
                
                delta_angle_7 + intercept_point_distance_7 +  intercept_fielder_distance_7 +
                intercept_point_base_distance_7 + intercept_base_distance_7 +
                fielder_distance_to_intercept_7 +
                fielder_distance_to_intercept_zero_x_7 +
                fielder_distance_to_intercept_zero_y_7 + intercept_fielder_x_7 + 
                intercept_fielder_y_7, data = sample, distribution = "bernoulli", n.trees = 1300,
              cv.folds = 5, interaction.depth = 8)

# Testing the cross validation error
min(model$cv.error)
gbm.perf(model)


# Creating a calibration plot from the model
sample = sample%>%
  mutate(Hit2 = ifelse(Hit == 1,1,0),
         pred = predict(model, sample, type="response"))

df = sample %>%
  mutate(
    group = case_when(
      pred < 0.1 ~ 1,
      pred < 0.2 ~ 2,
      pred < 0.3 ~ 3,
      pred < 0.4 ~ 4,
      pred < 0.5 ~ 5,
      pred < 0.6 ~ 6,
      pred < 0.7 ~ 7,
      pred < 0.8 ~ 8,
      pred < 0.9 ~ 9,
      pred < 1 ~ 10)) %>%
  group_by(group) %>%
  summarise(
    n = n(),
    x = mean(pred),
    y = mean(Hit2))


ggplot(df, aes(x = x, y=y))+
  geom_line(color = "blue")+
  geom_point(aes(size=n))+
  geom_abline(slope = 1, intercept = 0)+
  labs(x = "Predicted Probability",
       y = "Observed Probability",
       title = "Observed Probability vs. Predicted Probability")+
  theme_bw()+
  guides(size=F)

