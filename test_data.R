#####
# packages
#####

require(tidyverse)
require(ggplot2)
require(ggpubr)

#####
# functions # 
#####

read_in <- function(file) {
  dat <- read.csv(file) %>% 
    select(1:3,6:13,16,29:50,54,57:70,75:77) %>% 
    rename(
      pitch_number = ï..No,
      pitch_speed = Pitch.Speed..mph., # mph
      pitch_la_v = Pitch.Launch..V...Â.., # degrees
      pitch_la_h = Pitch.Launch..H...Â.., # degrees 
      pitch_rpm = Pitch.Spin..rpm., # rpm
      pitch_spin_direction = Pitch.Spin.Direction..Â.., # degrees 
      pitch_spin_tilt = Pitch.Spin.Tilt, # time equivalent on 12hr clock
      pitch_release_height = Pitch.Release.Height..ft., # feet 
      pitch_release_side = Pitch.Release.Side..ft., # feet
      pitch_extension = Pitch.Extension..ft., # feet 
      pitch_vb = Pitch.Break..V...in., # inches 
      pitch_vb_ind = Pitch.Break.Ind..V...in., # iches 
      pitch_hb = Pitch.Break..H...in., # inches
      plate_z = Pitch.Strike.Zone...Height..ft., # feet
      plate_x = Pitch.Strike.Zone...Offset..ft., # feet
      pitch_speed_plate = Pitch.Zone.Speed..mph., # mph
      pitch_approach_v = Pitch.Approach..V...Â.., # degrees
      pitch_aproach_h = Pitch.Approach..H...Â.., # degrees
      pitch_time = Pitch.Time..s., # seconds
      ev = Hit.Ball.Speed..mph., # mph
      la_v = Hit.Ball.Launch..V...Â.., # degrees
      la_h = Hit.Ball.Launch..H...Â.., # degrees
      hit_spin = Hit.Spin..rpm., # rpm
      hit_dist = Hit.Carry.Distance..ft., # feet
      hit_flight_time = Hit.Ball.Flight.Time..s., # seconds
      pfx_x = pfxx..in., # pitch movement x at 40 ft, inches
      pfx_z = pfxz..in., # pitch movement y at 40 ft, inches
      x0 = x0..ft., # location at 50 ft, feet
      y0 = y0..ft., # location at 50 ft, feet 
      z0 = z0..ft., # location at 50 ft, feet 
      vx0 = vx0..ft.s., # velo at 50 ft, ft/s
      vy0 = vy0..ft.s., # velo at 50 ft, ft/s
      vz0 = vz0..ft.s., # velo at 50 ft, ft/s
      ax = ax..ft.sÂ²., # accel at 50 ft, ft/s/s
      ay = ay..ft.sÂ²., # accel at 50 ft, ft/s/s
      az = az..ft.sÂ²., # accel at 50 ft, ft/s/s
      px = px..ft., # catcher view zone location, ft
      pz = pz..ft.  # catcher view zone location, ft
    ) %>% 
    rename_with(tolower) %>% 
    drop_na(pitch_speed)
}

graph_tilt <- function(data, pitcher) {
  
  ggplot(dat[dat$pitcher == pitcher,], aes(x = pitch_spin_tilt)) + 
    geom_bar() + 
    scale_x_discrete(drop = F, breaks = hour_levels) +
    coord_polar(start = .47) +
    ggtitle(paste(pitcher,'spin axis grpah')) +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank())
  
}

graph_release_point <- function(data, pitcher) {
  
  ggplot(data[data$pitcher == pitcher,], aes(x = pitch_release_side, y = pitch_release_height)) +
    geom_point() +
    ylim(0,8) +
    xlim(-2,2) + 
    ggtitle(paste(pitcher,'release points'))
  
}

graph_zone_location <- function() {
  
}

graph_movement <- function() {
  
}
  
graph_direction <- function() {
  
}

#####
# data set up #
#####

dat1 <- read_in('9-11-21 Orange v Bridgeton FS Data.csv')
dat2 <- read_in('9-11-21 Navy v Royal FS Data.csv')
dat3 <- read_in('9-11-21 Black v Purple FS Data.csv')

dat <- rbind(dat1, dat2, dat3)

# only pitch specific data # 
spin_stuff <- dat %>% 
  select(pitcher, pitcher.handedness, pitch_speed, pitch_rpm, pitch_spin_direction, x0, z0,
         pitch_spin_tilt, pitch_vb, pitch_vb_ind, pitch_hb, pitch_release_height, pitch_release_side) %>% 
  na.omit()

#####
# random set up stuff #
#####

# makes list to reassign levels for pitch_spin_tilt #
time_levels <- c()
for (i in 1:12) {
  for(j in 0:3) {
    if (j == 0) {
      time_levels <- append(time_levels, paste0(i,':','00'))
    } else {
      time_levels <- append(time_levels, paste0(i,':',j*15))
    }
  }
}
hour_levels <- time_levels[seq(1, length(time_levels), 4)]
dat$pitch_spin_tilt <- factor(dat$pitch_spin_tilt, levels = time_levels)

# plot of zone locatoin for ct #
x = c(-.71, -.71, .71, .71)
y = c(1.75, 4.5, 4.5, 1.75)
strikezone = data.frame(x = x, y = y)


#####
# polts of spin tilt and direction #
#####

# pitch location of connor treybig #
ggplot(dat[dat$pitcher == 'Connor Treybig',], aes(x = plate_x, y = plate_z)) + 
  geom_point() +
  geom_polygon(aes(x = x, y = y), fill = NA, color = 'black', data = strikezone)

# spin direction by handedness #
ggplot(dat, aes(x = pitch_spin_direction)) +
  geom_density(bw = 15, aes(fill = pitcher.handedness), alpha = .5) + 
  xlim(0,360)

# spin tilt by handedness (gross) #
ggplot(dat, aes(x = pitch_spin_tilt)) +
  geom_bar(aes(fill = pitcher.handedness)) + 
  coord_polar()

#####
# connor trey big tests #
#####

# one pitcher and most fastballs # 
ctff <- spin_stuff[spin_stuff$pitcher == 'Connor Treybig' & spin_stuff$pitch_speed > 71,]

# same, but breaking balls # 
ctbb <- spin_stuff[spin_stuff$pitcher == 'Connor Treybig' & spin_stuff$pitch_speed < 71 & spin_stuff$pitch_speed > 55,]

# put the tagged stuff together #
ctff$pitch_guess <- 'fb'
ctbb$pitch_guess <- 'bb'
ct <- bind_rows(ctff, ctbb)


ggplot(ctff, aes(x = pitch_spin_tilt)) + 
  geom_bar() + 
  scale_x_discrete(drop = F, breaks = hour_levels) +
  coord_polar(start = .45) 

ggplot(ctff, aes(x = pitch_spin_direction)) + 
  geom_density(bw = 7) + 
  xlim(0,360)

ggplot(ct, aes(x = pitch_spin_tilt)) +
  geom_bar(aes(fill = pitch_guess)) +
  scale_x_discrete(drop = F, breaks = hour_levels) +
  coord_polar(start = .45) 

ggplot(ct, aes(x = pitch_spin_direction)) +
  geom_density(bw = 15, aes(fill = pitch_guess), alpha = .5) +
  xlim(0,360)

#####
# release point graphs 
#####

# plot of relase points, fb #
ggplot(ctff, aes(x = pitch_release_side, y = pitch_release_height)) +
  geom_point() +
  ylim(0,8) +
  xlim(-2,2)

# plot of relase points, bb #
ggplot(ctbb, aes(x = pitch_release_side, y = pitch_release_height)) +
  geom_point() +
  ylim(0,8) +
  xlim(-2,2)

ggplot(ct, aes(x = pitch_release_side, y = pitch_release_height)) +
  geom_point(aes(color = pitch_guess)) +
  ylim(0,8) +
  xlim(-4,4)

#####
# batted ball location
#####

ggplot(dat, aes(x = la_h, y = hit_dist)) +
  geom_point() + 
  coord_polar(start = 3.14) +
  xlim(-180,180) +
  ylim(0,310)

dat %>% 
  ggplot(aes(x = x0, y = plate_x, color = pitcher.handedness)) +
  geom_point()

dat %>% 
  ggplot(aes(x = z0, y = plate_z, color = pitcher.handedness)) +
  geom_point()

#####
# la v spin stuff 
#####

# la vs spin
ggplot(dat, aes(x = la_v, y = pitch_rpm)) +
  geom_point() +
  geom_smooth()

ggplot(dat[dat$pitch_speed > 78,], aes(x = la_v, y = pitch_rpm)) +
  geom_point() +
  geom_smooth() +
  ggtitle('pitch spin vs launch angle (pitch speed > 78)')

#####
# pitch movemnet stuff
#####

# spin tilt v vb_ind #
ggplot(dat,aes(x = pitch_spin_tilt, y = pitch_vb_ind, color = pitch_speed)) + 
  geom_point() +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(breaks = hour_levels) +
  scale_color_stepsn(colors = c('red','blue','green'))

# set up testing df and test metrics #
tester <- dat
tester$mvmt_diff <- tester$pitch_vb_ind - tester$pitch_vb
tester$miss_dist <- sqrt(tester$px ^ 2 + (3 - tester$pz) ^ 2)

# miss dist v mvmt diff #
tester %>% 
  filter(pitch_speed > 55) %>% 
  ggplot(aes(x = miss_dist, y = mvmt_diff, color = pitch_speed)) +
  geom_point() +
  scale_color_stepsn(colors = c('red','blue','green'))

# pitch speed v mvmt diff #
ggplot(tester, aes(x = pitch_speed, y = mvmt_diff)) + 
  geom_point() +
  scale_x_continuous(trans = 'log') +
  scale_y_continuous(trans = 'log')

# pitch speed v pitch vb colored by tilt #
tester %>% 
  filter(pitch_speed > 55) %>% 
  ggplot(aes(x = pitch_speed, y = pitch_vb, color = pitch_spin_tilt)) + 
  geom_point() 

tester %>% 
  filter(pitch_speed > 55) %>% 
  ggplot(aes(x = pitch_speed, y = pitch_vb_ind, color = pitch_spin_direction)) + 
  geom_point() +
  scale_color_stepsn(colors = c('red','blue','green','orange'))

tester %>% 
  filter(pitch_speed > 55) %>% 
  ggplot(aes(x = pfx_z, y = pitch_vb_ind)) + 
  geom_point() +
  geom_abline(slope = 1.65)

tester %>% 
  filter(pitch_speed > 55) %>% 
  ggplot(aes(x = pfx_z, y = pitch_vb + 40, color = pitch_speed)) +
  geom_point() + 
  geom_abline(slope = 1.65)


#####
# x0, y0, z0 shit #
#####

dat %>% 
  ggplot(aes(x = vz0)) +
  geom_density(bw = .1)

dat %>% 
  ggplot(aes(x = az)) +
  geom_density(bw = .1) +
  geom_vline(xintercept = -32)




#####
# approach angle shit #
#####

dat %>% 
  filter(pitch_speed > 55) %>% 
  ggplot(aes(x = pitch_speed, y = pitch_approach_v, color = pitch_spin_direction)) +
  geom_point() +
  scale_color_steps2()

