# note: 
# step 1: identify the minimum value 
# step 2: if the minimum value is less than -5, all measurements are adjusted by adding −5−(min value) and no upper bound is set for the maximum value; 
# otherwise, the data remains unchanged.

# note: Dec 16, 2024
# for patients 1,4,8,12,13,20,25, and 28, their values need to be -180 degrees first
# then adjusted by adding −5−(min value)


library(readxl)
library(writexl)
library(ggplot2)
library(tableone)
library(forcats)
library(dplyr)
library(emmeans)
library(data.table)

options(warn=1)

setwd("C:/Users/RXY149/OneDrive - MedStar Health/Project 1")

# get path names for each of the data files:
list_excel_files <- list.files(path = "Participant Data", full.names = TRUE)

final_results_flexex_re <- vector("list", length = length(list_excel_files))

for (i in 1:length(list_excel_files)) {
  print(paste("Processing: ", i, sep = ""))
  # hard coding: for patients 1,4,8,12,13,20,28
  # use a different adjustment strategy: x-180+(-5-min(x))
  
  # elbow data of the ith subject: rename all.elbow.data as "joint_angles_zxy"
  joint_angles_zxy <- readxl::read_xlsx(list_excel_files[i], sheet = "Joint Angles ZXY")
  joint_angles_zxy <- joint_angles_zxy[,c("Frame","Left Elbow Pronation/Supination", "Left Elbow Flexion/Extension",
                                          "Right Elbow Pronation/Supination", "Right Elbow Flexion/Extension")]
  # elbow markers of the ith subject:
  all.elbow.markers <- readxl::read_xlsx(list_excel_files[i], sheet = "Markers")
  
  # normalize the data:
  
  # drop the 1st column (frame)
  data_temp_1 <- joint_angles_zxy[,-1]
  
  # identify the min value, and adjust the min value to be -5 when necessary
  if (min(data_temp_1) < -5) {
    data_temp_2 = data_temp_1 + (-5-(min(data_temp_1)))
  } else {
    data_temp_2 = data_temp_1
  }
  
  # create a new all.elbow.data
  all.elbow.data <- cbind.data.frame(joint_angles_zxy[,1], data_temp_2)
  
  # export all.elbow.data for each patient:
  # file_name <- paste0("data_EROM_", i, "_adjusted_all_elbow_v2.csv")
  # write.csv(all.elbow.data, file.path("Adjusted Data ver2", file_name), row.names = FALSE)
  # print(paste("Exported:", file_name))
  
  # this creates a table of all left elbow data
  left.elbow <-
    all.elbow.data %>%
    select(Frame, "Left Elbow Pronation/Supination", "Left Elbow Flexion/Extension")
  
  # table with both elbows
  both.elbows <-
    all.elbow.data %>%
    select(Frame, "Left Elbow Pronation/Supination", "Left Elbow Flexion/Extension",
           "Right Elbow Pronation/Supination", "Right Elbow Flexion/Extension")
  
  if (!i %in% c(1,4,8,12,13,20,25, 28)) {
    # All the code to record max and mins for vertex task
    vertex.start <- apply(all.elbow.markers,1,function(row) any(row %like% "a_s"))
    vertex.start.true <- all.elbow.markers[vertex.start,]
    a_s <- max(as.data.frame(vertex.start.true[1]), na.rm = T)
    
    vertex.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "a_e"))
    vertex.stop.true <- all.elbow.markers[vertex.stop,]
    a_e <- max(as.data.frame(vertex.stop.true[1]), na.rm = T)
    
    elbow.vertex <- subset(both.elbows,Frame>=a_s & Frame<=a_e)
    
    max.re.vertex.flexex <- max(select(elbow.vertex,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.vertex.flexex <- min(select(elbow.vertex,"Right Elbow Flexion/Extension"), na.rm = T)
    re.vertex.flexex.range <- max.re.vertex.flexex - min.re.vertex.flexex
    
    # All the code to record max and mins for occiput tasks
    occiput.start <- apply(all.elbow.markers,1,function(row) any(row %like% "b_s"))
    occiput.start.true <- all.elbow.markers[occiput.start,]
    b_s <- max(as.data.frame(occiput.start.true[1]), na.rm = T)
    
    occiput.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "b_e"))
    occiput.stop.true <- all.elbow.markers[occiput.stop,]
    b_e <- max(as.data.frame(occiput.stop.true[1]), na.rm = T)
    
    elbow.occiput <- subset(both.elbows,Frame>=b_s & Frame<=b_e)
    
    max.re.occiput.flexex <- max(select(elbow.occiput,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.occiput.flexex <- min(select(elbow.occiput,"Right Elbow Flexion/Extension"), na.rm = T)
    re.occiput.flexex.range <-max.re.occiput.flexex-min.re.occiput.flexex
    
    # All the code to record max and mins for canthus tasks
    canthus.start <- apply(all.elbow.markers,1,function(row) any(row %like% "c_s"))
    canthus.start.true <- all.elbow.markers[canthus.start,]
    c_s <- max(as.data.frame(canthus.start.true[1]), na.rm = T)
    
    canthus.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "c_e"))
    canthus.stop.true <- all.elbow.markers[canthus.stop,]
    c_e <- max(as.data.frame(canthus.stop.true[1]), na.rm = T)
    
    elbow.canthus <- subset(both.elbows,Frame>=c_s & Frame<=c_e)
    
    max.re.canthus.flexex <- max(select(elbow.canthus,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.canthus.flexex <- min(select(elbow.canthus,"Right Elbow Flexion/Extension"), na.rm = T)
    re.canthus.flexex.range <-max.re.canthus.flexex-min.re.canthus.flexex
    
    
    # All the code to record max and mins for cbone tasks
    cbone.start <- apply(all.elbow.markers,1,function(row) any(row %like% "d_s"))
    cbone.start.true <- all.elbow.markers[cbone.start,]
    d_s <- max(as.data.frame(cbone.start.true[1]), na.rm = T)
    
    cbone.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "d_e"))
    cbone.stop.true <- all.elbow.markers[cbone.stop,]
    d_e <- max(as.data.frame(cbone.stop.true[1]), na.rm = T)
    
    elbow.cbone <- subset(both.elbows,Frame>=d_s & Frame<=d_e)
    
    max.re.cbone.flexex <- max(select(elbow.cbone,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.cbone.flexex <- min(select(elbow.cbone,"Right Elbow Flexion/Extension"), na.rm = T)
    re.cbone.flexex.range <-max.re.cbone.flexex-min.re.cbone.flexex
    
    
    # All the code to record max and mins for sternum tasks
    sternum.start <- apply(all.elbow.markers,1,function(row) any(row %like% "e_s"))
    sternum.start.true <- all.elbow.markers[sternum.start,]
    e_s <- max(as.data.frame(sternum.start.true[1]), na.rm = T)
    
    sternum.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "e_e"))
    sternum.stop.true <- all.elbow.markers[sternum.stop,]
    e_e <- max(as.data.frame(sternum.stop.true[1]), na.rm = T)
    
    elbow.sternum <- subset(both.elbows,Frame>=e_s & Frame<=e_e)
    
    max.re.sternum.flexex <- max(select(elbow.sternum,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.sternum.flexex <- min(select(elbow.sternum,"Right Elbow Flexion/Extension"), na.rm = T)
    re.sternum.flexex.range <-max.re.sternum.flexex-min.re.sternum.flexex
    
    
    # All the code to record max and mins for buckle tasks
    buckle.start <- apply(all.elbow.markers,1,function(row) any(row %like% "f_s"))
    buckle.start.true <- all.elbow.markers[buckle.start,]
    f_s <- max(as.data.frame(buckle.start.true[1]), na.rm = T)
    
    buckle.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "f_e"))
    buckle.stop.true <- all.elbow.markers[buckle.stop,]
    f_e <- max(as.data.frame(buckle.stop.true[1]), na.rm = T)
    
    elbow.buckle <- subset(both.elbows,Frame>=f_s & Frame<=f_e)
    
    max.re.buckle.flexex <- max(select(elbow.buckle,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.buckle.flexex <- min(select(elbow.buckle,"Right Elbow Flexion/Extension"), na.rm = T)
    re.buckle.flexex.range <-max.re.buckle.flexex-min.re.buckle.flexex
    
    
    # All the code to record max and mins for scapula tasks
    scapula.start <- apply(all.elbow.markers,1,function(row) any(row %like% "g_s"))
    scapula.start.true <- all.elbow.markers[scapula.start,]
    g_s <- max(as.data.frame(scapula.start.true[1]), na.rm = T)
    
    scapula.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "g_e"))
    scapula.stop.true <- all.elbow.markers[scapula.stop,]
    g_e <- max(as.data.frame(scapula.stop.true[1]), na.rm = T)
    
    elbow.scapula <- subset(both.elbows,Frame>=g_s & Frame<=g_e)
    
    max.re.scapula.flexex <- max(select(elbow.scapula,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.scapula.flexex <- min(select(elbow.scapula,"Right Elbow Flexion/Extension"), na.rm = T)
    re.scapula.flexex.range <-max.re.scapula.flexex-min.re.scapula.flexex
    
    # All the code to record max and mins for sacrum tasks
    sacrum.start <- apply(all.elbow.markers,1,function(row) any(row %like% "h_s"))
    sacrum.start.true <- all.elbow.markers[sacrum.start,]
    h_s <- max(as.data.frame(sacrum.start.true[1]), na.rm = T)
    
    sacrum.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "h_e"))
    sacrum.stop.true <- all.elbow.markers[sacrum.stop,]
    h_e <- max(as.data.frame(sacrum.stop.true[1]), na.rm = T)
    
    elbow.sacrum <- subset(both.elbows,Frame>=h_s & Frame<=h_e)
    
    max.re.sacrum.flexex <- max(select(elbow.sacrum,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.sacrum.flexex <- min(select(elbow.sacrum,"Right Elbow Flexion/Extension"), na.rm = T)
    re.sacrum.flexex.range <-max.re.sacrum.flexex-min.re.sacrum.flexex
    
    
    # All the code to record max and mins for door tasks
    door.start <- apply(all.elbow.markers,1,function(row) any(row %like% "i_s"))
    door.start.true <- all.elbow.markers[door.start,]
    i_s <- max(as.data.frame(door.start.true[1]), na.rm = T)
    
    door.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "i_e"))
    door.stop.true <- all.elbow.markers[door.stop,]
    i_e <- max(as.data.frame(door.stop.true[1]), na.rm = T)
    
    elbow.door <- subset(both.elbows,Frame>=i_s & Frame<=i_e)
    
    max.re.door.flexex <- max(select(elbow.door,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.door.flexex <- min(select(elbow.door,"Right Elbow Flexion/Extension"), na.rm = T)
    re.door.flexex.range <-max.re.door.flexex-min.re.door.flexex
    
    
    # All the code to record max and mins for pubis tasks
    pubis.start <- apply(all.elbow.markers,1,function(row) any(row %like% "j_s"))
    pubis.start.true <- all.elbow.markers[pubis.start,]
    j_s <- max(as.data.frame(pubis.start.true[1]), na.rm = T)
    
    pubis.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "j_e"))
    pubis.stop.true <- all.elbow.markers[pubis.stop,]
    j_e <- max(as.data.frame(pubis.stop.true[1]), na.rm = T)
    
    elbow.pubis <- subset(both.elbows,Frame>=j_s & Frame<=j_e)
    
    
    max.re.pubis.flexex <- max(select(elbow.pubis,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.pubis.flexex <- min(select(elbow.pubis,"Right Elbow Flexion/Extension"), na.rm = T)
    re.pubis.flexex.range <-max.re.pubis.flexex-min.re.pubis.flexex
    
    
    # All the code to record max and mins for shoe tasks
    shoe.start <- apply(all.elbow.markers,1,function(row) any(row %like% "k_s"))
    shoe.start.true <- all.elbow.markers[shoe.start,]
    k_s <- max(as.data.frame(shoe.start.true[1]), na.rm = T)
    
    shoe.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "k_e"))
    shoe.stop.true <- all.elbow.markers[shoe.stop,]
    k_e <- max(as.data.frame(shoe.stop.true[1]), na.rm = T)
    
    elbow.shoe <- subset(both.elbows,Frame>=k_s & Frame<=k_e)
    
    max.re.shoe.flexex <- max(select(elbow.shoe,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.shoe.flexex <- min(select(elbow.shoe,"Right Elbow Flexion/Extension"), na.rm = T)
    re.shoe.flexex.range <-max.re.shoe.flexex-min.re.shoe.flexex
    
    
    
    # All the code to record max and mins for keys tasks
    keys.start <- apply(all.elbow.markers,1,function(row) any(row %like% "l_s"))
    keys.start.true <- all.elbow.markers[keys.start,]
    l_s <- max(as.data.frame(keys.start.true[1]), na.rm = T)
    
    keys.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "l_e"))
    keys.stop.true <- all.elbow.markers[keys.stop,]
    l_e <- max(as.data.frame(keys.stop.true[1]), na.rm = T)
    
    elbow.keys <- subset(both.elbows,Frame>=l_s & Frame<=l_e)
    
    
    max.re.keys.flexex <- max(select(elbow.keys,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.keys.flexex <- min(select(elbow.keys,"Right Elbow Flexion/Extension"), na.rm = T)
    re.keys.flexex.range <-max.re.keys.flexex-min.re.keys.flexex
    
    
    # All the code to record max and mins for mouse tasks
    mouse.start <- apply(all.elbow.markers,1,function(row) any(row %like% "m_s"))
    mouse.start.true <- all.elbow.markers[mouse.start,]
    m_s <- max(as.data.frame(mouse.start.true[1]), na.rm = T)
    
    mouse.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "m_e"))
    mouse.stop.true <- all.elbow.markers[mouse.stop,]
    m_e <- max(as.data.frame(mouse.stop.true[1]), na.rm = T)
    
    elbow.mouse <- subset(both.elbows,Frame>=m_s & Frame<=m_e)
    
    
    max.re.mouse.flexex <- max(select(elbow.mouse,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.mouse.flexex <- min(select(elbow.mouse,"Right Elbow Flexion/Extension"), na.rm = T)
    re.mouse.flexex.range <-max.re.mouse.flexex-min.re.mouse.flexex
    
    
    
    # All the code to record max and mins for desk tasks
    desk.start <- apply(all.elbow.markers,1,function(row) any(row %like% "n_s"))
    desk.start.true <- all.elbow.markers[desk.start,]
    n_s <- max(as.data.frame(desk.start.true[1]), na.rm = T)
    
    desk.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "n_e"))
    desk.stop.true <- all.elbow.markers[desk.stop,]
    n_e <- max(as.data.frame(desk.stop.true[1]), na.rm = T)
    
    elbow.desk <- subset(both.elbows,Frame>=n_s & Frame<=n_e)
    
    max.re.desk.flexex <- max(select(elbow.desk,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.desk.flexex <- min(select(elbow.desk,"Right Elbow Flexion/Extension"), na.rm = T)
    re.desk.flexex.range <-max.re.desk.flexex-min.re.desk.flexex
    
    
    # All the code to record max and mins for desk.ear tasks
    desk.ear.start <- apply(all.elbow.markers,1,function(row) any(row %like% "o_s"))
    desk.ear.start.true <- all.elbow.markers[desk.ear.start,]
    o_s <- max(as.data.frame(desk.ear.start.true[1]), na.rm = T)
    
    desk.ear.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "o_e"))
    desk.ear.stop.true <- all.elbow.markers[desk.ear.stop,]
    o_e <- max(as.data.frame(desk.ear.stop.true[1]), na.rm = T)
    
    elbow.desk.ear <- subset(both.elbows,Frame>=o_s & Frame<=o_e)
    
    max.re.desk.ear.flexex <- max(select(elbow.desk.ear,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.desk.ear.flexex <- min(select(elbow.desk.ear,"Right Elbow Flexion/Extension"), na.rm = T)
    re.desk.ear.flexex.range <-max.re.desk.ear.flexex-min.re.desk.ear.flexex
    
    
    # All the code to record max and mins for mobile tasks
    mobile.start <- apply(all.elbow.markers,1,function(row) any(row %like% "p_s"))
    mobile.start.true <- all.elbow.markers[mobile.start,]
    p_s <- max(as.data.frame(mobile.start.true[1]), na.rm = T)
    
    mobile.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "p_e"))
    mobile.stop.true <- all.elbow.markers[mobile.stop,]
    p_e <- max(as.data.frame(mobile.stop.true[1]), na.rm = T)
    
    elbow.mobile <- subset(both.elbows,Frame>=p_s & Frame<=p_e)
    
    max.re.mobile.flexex <- max(select(elbow.mobile,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.mobile.flexex <- min(select(elbow.mobile,"Right Elbow Flexion/Extension"), na.rm = T)
    re.mobile.flexex.range <-max.re.mobile.flexex-min.re.mobile.flexex
    
    
    # All the code to record max and mins for mobile.ear tasks
    mobile.ear.start <- apply(all.elbow.markers,1,function(row) any(row %like% "q_s"))
    mobile.ear.start.true <- all.elbow.markers[mobile.ear.start,]
    q_s <- max(as.data.frame(mobile.ear.start.true[1]), na.rm = T)
    
    mobile.ear.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "q_e"))
    mobile.ear.stop.true <- all.elbow.markers[mobile.ear.stop,]
    q_e <- max(as.data.frame(mobile.ear.stop.true[1]), na.rm = T)
    
    elbow.mobile.ear <- subset(both.elbows,Frame>=q_s & Frame<=q_e)
    
    max.re.mobile.ear.flexex <- max(select(elbow.mobile.ear,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.mobile.ear.flexex <- min(select(elbow.mobile.ear,"Right Elbow Flexion/Extension"), na.rm = T)
    re.mobile.ear.flexex.range <-max.re.mobile.ear.flexex-min.re.mobile.ear.flexex
    
    
    
    # All the code to record max and mins for cut tasks
    cut.start <- apply(all.elbow.markers,1,function(row) any(row %like% "r_s"))
    cut.start.true <- all.elbow.markers[cut.start,]
    r_s <- max(as.data.frame(cut.start.true[1]), na.rm = T)
    
    cut.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "r_e"))
    cut.stop.true <- all.elbow.markers[cut.stop,]
    r_e <- max(as.data.frame(cut.stop.true[1]), na.rm = T)
    
    elbow.cut <- subset(both.elbows,Frame>=r_s & Frame<=r_e)
    
    max.re.cut.flexex <- max(select(elbow.cut,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.cut.flexex <- min(select(elbow.cut,"Right Elbow Flexion/Extension"), na.rm = T)
    re.cut.flexex.range <-max.re.cut.flexex-min.re.cut.flexex
    
    
    # All the code to record max and mins for fork tasks
    fork.start <- apply(all.elbow.markers,1,function(row) any(row %like% "s_s"))
    fork.start.true <- all.elbow.markers[fork.start,]
    s_s <- max(as.data.frame(fork.start.true[1]), na.rm = T)
    
    fork.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "s_e"))
    fork.stop.true <- all.elbow.markers[fork.stop,]
    s_e <- max(as.data.frame(fork.stop.true[1]), na.rm = T)
    
    elbow.fork <- subset(both.elbows,Frame>=s_s & Frame<=s_e)
    
    max.re.fork.flexex <- max(select(elbow.fork,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.fork.flexex <- min(select(elbow.fork,"Right Elbow Flexion/Extension"), na.rm = T)
    re.fork.flexex.range <-max.re.fork.flexex-min.re.fork.flexex
    
    
    # All the code to record max and mins for folder tasks
    folder.start <- apply(all.elbow.markers,1,function(row) any(row %like% "t_s"))
    folder.start.true <- all.elbow.markers[folder.start,]
    t_s <- max(as.data.frame(folder.start.true[1]), na.rm = T)
    
    folder.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "t_e"))
    folder.stop.true <- all.elbow.markers[folder.stop,]
    t_e <- max(as.data.frame(folder.stop.true[1]), na.rm = T)
    
    elbow.folder <- subset(both.elbows,Frame>=t_s & Frame<=t_e)
    
    max.re.folder.flexex <- max(select(elbow.folder,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.folder.flexex <- min(select(elbow.folder,"Right Elbow Flexion/Extension"), na.rm = T)
    re.folder.flexex.range <-max.re.folder.flexex-min.re.folder.flexex
    
    
    # All the code to record max and mins for jar tasks
    jar.start <- apply(all.elbow.markers,1,function(row) any(row %like% "u_s"))
    jar.start.true <- all.elbow.markers[jar.start,]
    u_s <- max(as.data.frame(jar.start.true[1]), na.rm = T)
    
    jar.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "u_e"))
    jar.stop.true <- all.elbow.markers[jar.stop,]
    u_e <- max(as.data.frame(jar.stop.true[1]), na.rm = T)
    
    elbow.jar <- subset(both.elbows,Frame>=u_s & Frame<=u_e)
    
    max.re.jar.flexex <- max(select(elbow.jar,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.jar.flexex <- min(select(elbow.jar,"Right Elbow Flexion/Extension"), na.rm = T)
    re.jar.flexex.range <-max.re.jar.flexex-min.re.jar.flexex
    
    
    # All the code to record max and mins for drink tasks
    drink.start <- apply(all.elbow.markers,1,function(row) any(row %like% "v_s"))
    drink.start.true <- all.elbow.markers[drink.start,]
    v_s <- max(as.data.frame(drink.start.true[1]), na.rm = T)
    
    drink.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "v_e"))
    drink.stop.true <- all.elbow.markers[drink.stop,]
    v_e <- max(as.data.frame(drink.stop.true[1]), na.rm = T)
    
    elbow.drink <- subset(both.elbows,Frame>=v_s & Frame<=v_e)
    
    max.re.drink.flexex <- max(select(elbow.drink,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.drink.flexex <- min(select(elbow.drink,"Right Elbow Flexion/Extension"), na.rm = T)
    re.drink.flexex.range <-max.re.drink.flexex-min.re.drink.flexex
    
    
    
    # All the code to record max and mins for stand tasks
    stand.start <- apply(all.elbow.markers,1,function(row) any(row %like% "w_s"))
    stand.start.true <- all.elbow.markers[stand.start,]
    w_s <- max(as.data.frame(stand.start.true[1]), na.rm = T)
    
    stand.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "w_e"))
    stand.stop.true <- all.elbow.markers[stand.stop,]
    w_e <- max(as.data.frame(stand.stop.true[1]), na.rm = T)
    
    elbow.stand <- subset(both.elbows,Frame>=w_s & Frame<=w_e)
    
    max.re.stand.flexex <- max(select(elbow.stand,"Right Elbow Flexion/Extension"), na.rm = T)
    min.re.stand.flexex <- min(select(elbow.stand,"Right Elbow Flexion/Extension"), na.rm = T)
    re.stand.flexex.range <-max.re.stand.flexex-min.re.stand.flexex
    
    ##################################################################################
    # This is building the flexion extension rows (left elbow)
    re.flexex.max.list <-c(i, max.re.vertex.flexex,max.re.occiput.flexex,max.re.canthus.flexex,
                           max.re.cbone.flexex,max.re.sternum.flexex,max.re.buckle.flexex,max.re.scapula.flexex,
                           max.re.sacrum.flexex,max.re.pubis.flexex, max.re.door.flexex, max.re.shoe.flexex, max.re.keys.flexex,
                           max.re.mouse.flexex, max.re.desk.flexex, max.re.desk.ear.flexex, max.re.mobile.flexex, max.re.mobile.ear.flexex,
                           max.re.cut.flexex, max.re.fork.flexex, max.re.folder.flexex, max.re.jar.flexex, max.re.drink.flexex, max.re.stand.flexex)
    
    re.flexex.min.list <-c(i, min.re.vertex.flexex,min.re.occiput.flexex,min.re.canthus.flexex,
                           min.re.cbone.flexex,min.re.sternum.flexex,min.re.buckle.flexex,min.re.scapula.flexex,
                           min.re.sacrum.flexex,min.re.pubis.flexex, min.re.door.flexex, min.re.shoe.flexex, min.re.keys.flexex,
                           min.re.mouse.flexex, min.re.desk.flexex, min.re.desk.ear.flexex, min.re.mobile.flexex, min.re.mobile.ear.flexex,
                           min.re.cut.flexex, min.re.fork.flexex, min.re.folder.flexex, min.re.jar.flexex, min.re.drink.flexex, min.re.stand.flexex)
    
    re.flexex.range.list <- c(i, re.vertex.flexex.range, re.occiput.flexex.range, re.canthus.flexex.range,
                              re.cbone.flexex.range,re.sternum.flexex.range,re.buckle.flexex.range,
                              re.scapula.flexex.range,re.sacrum.flexex.range,re.pubis.flexex.range,
                              re.door.flexex.range, re.shoe.flexex.range, re.keys.flexex.range,
                              re.mouse.flexex.range, re.desk.flexex.range, re.desk.ear.flexex.range, re.mobile.flexex.range, re.mobile.ear.flexex.range,
                              re.cut.flexex.range, re.fork.flexex.range, re.folder.flexex.range, re.jar.flexex.range, re.drink.flexex.range, re.stand.flexex.range)
    
    re.flexex.combo.list <- c(i, 
                              max.re.vertex.flexex,
                              min.re.vertex.flexex,
                              re.vertex.flexex.range,
                              max.re.occiput.flexex,
                              min.re.occiput.flexex,
                              re.occiput.flexex.range,
                              max.re.canthus.flexex,
                              min.re.canthus.flexex,
                              re.canthus.flexex.range,
                              max.re.cbone.flexex,
                              min.re.cbone.flexex,
                              re.cbone.flexex.range,
                              max.re.sternum.flexex,
                              min.re.sternum.flexex,
                              re.sternum.flexex.range,
                              max.re.buckle.flexex,
                              min.re.buckle.flexex,
                              re.buckle.flexex.range,
                              max.re.scapula.flexex,
                              min.re.scapula.flexex,
                              re.scapula.flexex.range,
                              max.re.sacrum.flexex,
                              min.re.sacrum.flexex,
                              re.sacrum.flexex.range,
                              max.re.pubis.flexex,
                              min.re.pubis.flexex,
                              re.pubis.flexex.range,
                              max.re.door.flexex,
                              min.re.door.flexex,
                              re.door.flexex.range,
                              max.re.shoe.flexex,
                              min.re.shoe.flexex,
                              re.shoe.flexex.range,
                              max.re.keys.flexex,
                              min.re.keys.flexex,
                              re.keys.flexex.range,
                              max.re.mouse.flexex,
                              min.re.mouse.flexex,
                              re.mouse.flexex.range,
                              max.re.desk.flexex,
                              min.re.desk.flexex,
                              re.desk.flexex.range,
                              max.re.desk.ear.flexex,
                              min.re.desk.ear.flexex,
                              re.desk.ear.flexex.range,
                              max.re.mobile.flexex,
                              min.re.mobile.flexex,
                              re.mobile.flexex.range,
                              max.re.mobile.ear.flexex,
                              min.re.mobile.ear.flexex,
                              re.mobile.ear.flexex.range,
                              max.re.cut.flexex,
                              min.re.cut.flexex,
                              re.cut.flexex.range,
                              max.re.fork.flexex,
                              min.re.fork.flexex,
                              re.fork.flexex.range,
                              max.re.folder.flexex,
                              min.re.folder.flexex,
                              re.folder.flexex.range,
                              max.re.jar.flexex,
                              min.re.jar.flexex,
                              re.jar.flexex.range,
                              max.re.drink.flexex,
                              min.re.drink.flexex,
                              re.drink.flexex.range,
                              max.re.stand.flexex,
                              min.re.stand.flexex,
                              re.stand.flexex.range
    )
    
    range.cols <- c('Particpant Number',
                    'Vertex Max','Vertex Min','Vertex Range',
                    'Occiput Max','Occiput Min','Occiput Range',
                    'Canthus Max','Canthus Min','Canthus Range',
                    'Collar Bone Max','Collar Bone Min','Collar Bone Range',
                    'Sternum Max','Sternum Min','Sternum Range',
                    'Belt Buckle Max','Belt Buckle Min','Belt Buckle Range',
                    'Scapula Max','Scapula Min','Scapula Range',
                    'Sacrum Max','Sacrum Min','Sacrum Range',
                    'Pubis Max', 'Pubis Min','Pubis Range',
                    'Door Knob Max','Door Knob Min','Door Knob Range',
                    'Shoe Max','Shoe Min','Shoe Range',
                    'Keys Max','Keys Min','Keys Range',
                    'Mouse Max','Mouse Min','Mouse Range',
                    'Desk Max','Desk Min','Desk Range',
                    'Desk Ear Max','Desk Ear Min','Desk Ear Range',
                    'Mobile Max','Mobile Min','Mobile Range',
                    'Mobile Ear Max','Mobile Ear Min','Mobile Ear Range',
                    'Cut Max','Cut Min','Cut Range',
                    'Fork Max','Fork Min','Fork Range',
                    'Folder Max','Folder Min','Folder Range',
                    'Jar Max','Jar Min','Jar Range',
                    'Drink Max','Drink Min','Drink Range',
                    'Stand Max','Stand Min','Stand Range'
    )
    
    flexex.matrix.re <- matrix(re.flexex.combo.list,
                               nrow = 1,
                               ncol = 70,
                               dimnames = NULL)
    colnames(flexex.matrix.re) <- range.cols
    final_results_flexex_re[[i]] <- as.data.frame(flexex.matrix.re)
  } else {
    ##### when i not equal to 1,4,8,12,13,20,28
    # All the code to record max and mins for vertex task
    vertex.start <- apply(all.elbow.markers,1,function(row) any(row %like% "a_s"))
    vertex.start.true <- all.elbow.markers[vertex.start,]
    a_s <- max(as.data.frame(vertex.start.true[1]), na.rm = T)
    
    vertex.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "a_e"))
    vertex.stop.true <- all.elbow.markers[vertex.stop,]
    a_e <- max(as.data.frame(vertex.stop.true[1]), na.rm = T)
    
    elbow.vertex <- subset(both.elbows,Frame>=a_s & Frame<=a_e)
    
    
    max.re.vertex.flexex <- max(select(elbow.vertex,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.vertex.flexex <- min(select(elbow.vertex,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.vertex.flexex.range <- max.re.vertex.flexex - min.re.vertex.flexex
    if (min.re.vertex.flexex < -5) {
      max.re.vertex.flexex <- max.re.vertex.flexex + (-5-min.re.vertex.flexex)
      min.re.vertex.flexex <- -5
    }else {
      max.re.vertex.flexex <- max.re.vertex.flexex
      min.re.vertex.flexex <- min.re.vertex.flexex
    }
    
    # All the code to record max and mins for occiput tasks
    occiput.start <- apply(all.elbow.markers,1,function(row) any(row %like% "b_s"))
    occiput.start.true <- all.elbow.markers[occiput.start,]
    b_s <- max(as.data.frame(occiput.start.true[1]), na.rm = T)
    
    occiput.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "b_e"))
    occiput.stop.true <- all.elbow.markers[occiput.stop,]
    b_e <- max(as.data.frame(occiput.stop.true[1]), na.rm = T)
    
    elbow.occiput <- subset(both.elbows,Frame>=b_s & Frame<=b_e)
    
    
    max.re.occiput.flexex <- max(select(elbow.occiput,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.occiput.flexex <- min(select(elbow.occiput,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.occiput.flexex.range <-max.re.occiput.flexex-min.re.occiput.flexex
    if (min.re.occiput.flexex < -5) {
      max.re.occiput.flexex <- max.re.occiput.flexex + (-5-min.re.occiput.flexex)
      min.re.occiput.flexex <- -5
    } else {
      max.re.occiput.flexex <- max.re.occiput.flexex
      min.re.occiput.flexex <- min.re.occiput.flexex
    }
    
    
    
    # All the code to record max and mins for canthus tasks
    canthus.start <- apply(all.elbow.markers,1,function(row) any(row %like% "c_s"))
    canthus.start.true <- all.elbow.markers[canthus.start,]
    c_s <- max(as.data.frame(canthus.start.true[1]), na.rm = T)
    
    canthus.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "c_e"))
    canthus.stop.true <- all.elbow.markers[canthus.stop,]
    c_e <- max(as.data.frame(canthus.stop.true[1]), na.rm = T)
    
    elbow.canthus <- subset(both.elbows,Frame>=c_s & Frame<=c_e)
    
    
    max.re.canthus.flexex <- max(select(elbow.canthus,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.canthus.flexex <- min(select(elbow.canthus,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.canthus.flexex.range <-max.re.canthus.flexex-min.re.canthus.flexex
    if (min.re.canthus.flexex < -5) {
      max.re.canthus.flexex <- max.re.canthus.flexex + (-5-min.re.canthus.flexex)
      min.re.canthus.flexex <- -5
    } else {
      max.re.canthus.flexex <- max.re.canthus.flexex
      min.re.canthus.flexex <- min.re.canthus.flexex
    }
    
    
    # All the code to record max and mins for cbone tasks
    cbone.start <- apply(all.elbow.markers,1,function(row) any(row %like% "d_s"))
    cbone.start.true <- all.elbow.markers[cbone.start,]
    d_s <- max(as.data.frame(cbone.start.true[1]), na.rm = T)
    
    cbone.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "d_e"))
    cbone.stop.true <- all.elbow.markers[cbone.stop,]
    d_e <- max(as.data.frame(cbone.stop.true[1]), na.rm = T)
    
    elbow.cbone <- subset(both.elbows,Frame>=d_s & Frame<=d_e)
    
    
    max.re.cbone.flexex <- max(select(elbow.cbone,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.cbone.flexex <- min(select(elbow.cbone,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.cbone.flexex.range <-max.re.cbone.flexex-min.re.cbone.flexex
    if (min.re.cbone.flexex < -5) {
      max.re.cbone.flexex <- max.re.cbone.flexex + (-5-min.re.cbone.flexex)
      min.re.cbone.flexex <- -5
    } else {
      max.re.cbone.flexex <- max.re.cbone.flexex
      min.re.cbone.flexex <- min.re.cbone.flexex
    }
    
    
    
    # All the code to record max and mins for sternum tasks
    sternum.start <- apply(all.elbow.markers,1,function(row) any(row %like% "e_s"))
    sternum.start.true <- all.elbow.markers[sternum.start,]
    e_s <- max(as.data.frame(sternum.start.true[1]), na.rm = T)
    
    sternum.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "e_e"))
    sternum.stop.true <- all.elbow.markers[sternum.stop,]
    e_e <- max(as.data.frame(sternum.stop.true[1]), na.rm = T)
    
    elbow.sternum <- subset(both.elbows,Frame>=e_s & Frame<=e_e)
    
    
    max.re.sternum.flexex <- max(select(elbow.sternum,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.sternum.flexex <- min(select(elbow.sternum,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.sternum.flexex.range <-max.re.sternum.flexex-min.re.sternum.flexex
    if (min.re.sternum.flexex < -5) {
      max.re.sternum.flexex <- max.re.sternum.flexex + (-5-min.re.sternum.flexex)
      min.re.sternum.flexex <- -5
    } else {
      max.re.sternum.flexex <- max.re.sternum.flexex
      min.re.sternum.flexex <- min.re.sternum.flexex
    }
    
    
    
    # All the code to record max and mins for buckle tasks
    buckle.start <- apply(all.elbow.markers,1,function(row) any(row %like% "f_s"))
    buckle.start.true <- all.elbow.markers[buckle.start,]
    f_s <- max(as.data.frame(buckle.start.true[1]), na.rm = T)
    
    buckle.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "f_e"))
    buckle.stop.true <- all.elbow.markers[buckle.stop,]
    f_e <- max(as.data.frame(buckle.stop.true[1]), na.rm = T)
    
    elbow.buckle <- subset(both.elbows,Frame>=f_s & Frame<=f_e)
    
    
    max.re.buckle.flexex <- max(select(elbow.buckle,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.buckle.flexex <- min(select(elbow.buckle,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.buckle.flexex.range <-max.re.buckle.flexex-min.re.buckle.flexex
    if (min.re.buckle.flexex < -5) {
      max.re.buckle.flexex <- max.re.buckle.flexex + (-5-min.re.buckle.flexex)
      min.re.buckle.flexex <- -5
    } else {
      max.re.buckle.flexex <- max.re.buckle.flexex
      min.re.buckle.flexex <- min.re.buckle.flexex
    }
    
    
    
    # All the code to record max and mins for scapula tasks
    scapula.start <- apply(all.elbow.markers,1,function(row) any(row %like% "g_s"))
    scapula.start.true <- all.elbow.markers[scapula.start,]
    g_s <- max(as.data.frame(scapula.start.true[1]), na.rm = T)
    
    scapula.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "g_e"))
    scapula.stop.true <- all.elbow.markers[scapula.stop,]
    g_e <- max(as.data.frame(scapula.stop.true[1]), na.rm = T)
    
    elbow.scapula <- subset(both.elbows,Frame>=g_s & Frame<=g_e)
    
    
    
    max.re.scapula.flexex <- max(select(elbow.scapula,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.scapula.flexex <- min(select(elbow.scapula,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.scapula.flexex.range <-max.re.scapula.flexex-min.re.scapula.flexex
    if (min.re.scapula.flexex < -5) {
      max.re.scapula.flexex <- max.re.scapula.flexex + (-5-min.re.scapula.flexex)
      min.re.scapula.flexex <- -5
    } else {
      max.re.scapula.flexex <- max.re.scapula.flexex
      min.re.scapula.flexex <- min.re.scapula.flexex
    }
    
    
    
    
    # All the code to record max and mins for sacrum tasks
    sacrum.start <- apply(all.elbow.markers,1,function(row) any(row %like% "h_s"))
    sacrum.start.true <- all.elbow.markers[sacrum.start,]
    h_s <- max(as.data.frame(sacrum.start.true[1]), na.rm = T)
    
    sacrum.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "h_e"))
    sacrum.stop.true <- all.elbow.markers[sacrum.stop,]
    h_e <- max(as.data.frame(sacrum.stop.true[1]), na.rm = T)
    
    elbow.sacrum <- subset(both.elbows,Frame>=h_s & Frame<=h_e)
    
    
    
    max.re.sacrum.flexex <- max(select(elbow.sacrum,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.sacrum.flexex <- min(select(elbow.sacrum,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.sacrum.flexex.range <-max.re.sacrum.flexex-min.re.sacrum.flexex
    if (min.re.sacrum.flexex < -5) {
      max.re.sacrum.flexex <- max.re.sacrum.flexex + (-5-min.re.sacrum.flexex)
      min.re.sacrum.flexex <- -5
    } else {
      max.re.sacrum.flexex <- max.re.sacrum.flexex
      min.re.sacrum.flexex <- min.re.sacrum.flexex
    }
    
    
    # All the code to record max and mins for door tasks
    door.start <- apply(all.elbow.markers,1,function(row) any(row %like% "i_s"))
    door.start.true <- all.elbow.markers[door.start,]
    i_s <- max(as.data.frame(door.start.true[1]), na.rm = T)
    
    door.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "i_e"))
    door.stop.true <- all.elbow.markers[door.stop,]
    i_e <- max(as.data.frame(door.stop.true[1]), na.rm = T)
    
    elbow.door <- subset(both.elbows,Frame>=i_s & Frame<=i_e)
    
    
    max.re.door.flexex <- max(select(elbow.door,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.door.flexex <- min(select(elbow.door,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.door.flexex.range <-max.re.door.flexex-min.re.door.flexex
    if (min.re.door.flexex < -5) {
      max.re.door.flexex <- max.re.door.flexex + (-5-min.re.door.flexex)
      min.re.door.flexex <- -5
    } else {
      max.re.door.flexex <- max.re.door.flexex
      min.re.door.flexex <- min.re.door.flexex
    }
    
    
    # All the code to record max and mins for pubis tasks
    pubis.start <- apply(all.elbow.markers,1,function(row) any(row %like% "j_s"))
    pubis.start.true <- all.elbow.markers[pubis.start,]
    j_s <- max(as.data.frame(pubis.start.true[1]), na.rm = T)
    
    pubis.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "j_e"))
    pubis.stop.true <- all.elbow.markers[pubis.stop,]
    j_e <- max(as.data.frame(pubis.stop.true[1]), na.rm = T)
    
    elbow.pubis <- subset(both.elbows,Frame>=j_s & Frame<=j_e)
    
    
    max.re.pubis.flexex <- max(select(elbow.pubis,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.pubis.flexex <- min(select(elbow.pubis,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.pubis.flexex.range <-max.re.pubis.flexex-min.re.pubis.flexex
    if (min.re.pubis.flexex < -5) {
      max.re.pubis.flexex <- max.re.pubis.flexex + (-5-min.re.pubis.flexex)
      min.re.pubis.flexex <- -5
    } else {
      max.re.pubis.flexex <- max.re.pubis.flexex
      min.re.pubis.flexex <- min.re.pubis.flexex
    }
    
    
    # All the code to record max and mins for shoe tasks
    shoe.start <- apply(all.elbow.markers,1,function(row) any(row %like% "k_s"))
    shoe.start.true <- all.elbow.markers[shoe.start,]
    k_s <- max(as.data.frame(shoe.start.true[1]), na.rm = T)
    
    shoe.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "k_e"))
    shoe.stop.true <- all.elbow.markers[shoe.stop,]
    k_e <- max(as.data.frame(shoe.stop.true[1]), na.rm = T)
    
    elbow.shoe <- subset(both.elbows,Frame>=k_s & Frame<=k_e)
    
    
    max.re.shoe.flexex <- max(select(elbow.shoe,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.shoe.flexex <- min(select(elbow.shoe,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.shoe.flexex.range <-max.re.shoe.flexex-min.re.shoe.flexex
    if (min.re.shoe.flexex < -5) {
      max.re.shoe.flexex <- max.re.shoe.flexex + (-5-min.re.shoe.flexex)
      min.re.shoe.flexex <- -5
    } else {
      max.re.shoe.flexex <- max.re.shoe.flexex
      min.re.shoe.flexex <- min.re.shoe.flexex
    }
    
    
    
    # All the code to record max and mins for keys tasks
    keys.start <- apply(all.elbow.markers,1,function(row) any(row %like% "l_s"))
    keys.start.true <- all.elbow.markers[keys.start,]
    l_s <- max(as.data.frame(keys.start.true[1]), na.rm = T)
    
    keys.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "l_e"))
    keys.stop.true <- all.elbow.markers[keys.stop,]
    l_e <- max(as.data.frame(keys.stop.true[1]), na.rm = T)
    
    elbow.keys <- subset(both.elbows,Frame>=l_s & Frame<=l_e)
    
    max.re.keys.flexex <- max(select(elbow.keys,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.keys.flexex <- min(select(elbow.keys,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.keys.flexex.range <-max.re.keys.flexex-min.re.keys.flexex
    if (min.re.keys.flexex < -5) {
      max.re.keys.flexex <- max.re.keys.flexex + (-5-min.re.keys.flexex)
      min.re.keys.flexex <- -5
    } else {
      max.re.keys.flexex <- max.re.keys.flexex
      min.re.keys.flexex <- min.re.keys.flexex
    }
    
    
    # All the code to record max and mins for mouse tasks
    mouse.start <- apply(all.elbow.markers,1,function(row) any(row %like% "m_s"))
    mouse.start.true <- all.elbow.markers[mouse.start,]
    m_s <- max(as.data.frame(mouse.start.true[1]), na.rm = T)
    
    mouse.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "m_e"))
    mouse.stop.true <- all.elbow.markers[mouse.stop,]
    m_e <- max(as.data.frame(mouse.stop.true[1]), na.rm = T)
    
    elbow.mouse <- subset(both.elbows,Frame>=m_s & Frame<=m_e)
    
    
    max.re.mouse.flexex <- max(select(elbow.mouse,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.mouse.flexex <- min(select(elbow.mouse,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.mouse.flexex.range <-max.re.mouse.flexex-min.re.mouse.flexex
    if (min.re.mouse.flexex < -5) {
      max.re.mouse.flexex <- max.re.mouse.flexex + (-5-min.re.mouse.flexex)
      min.re.mouse.flexex <- -5
    } else {
      max.re.mouse.flexex <- max.re.mouse.flexex
      min.re.mouse.flexex <- min.re.mouse.flexex
    }
    
    
    # All the code to record max and mins for desk tasks
    desk.start <- apply(all.elbow.markers,1,function(row) any(row %like% "n_s"))
    desk.start.true <- all.elbow.markers[desk.start,]
    n_s <- max(as.data.frame(desk.start.true[1]), na.rm = T)
    
    desk.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "n_e"))
    desk.stop.true <- all.elbow.markers[desk.stop,]
    n_e <- max(as.data.frame(desk.stop.true[1]), na.rm = T)
    
    elbow.desk <- subset(both.elbows,Frame>=n_s & Frame<=n_e)
    
    max.re.desk.flexex <- max(select(elbow.desk,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.desk.flexex <- min(select(elbow.desk,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.desk.flexex.range <-max.re.desk.flexex-min.re.desk.flexex
    if (min.re.desk.flexex < -5) {
      max.re.desk.flexex <- max.re.desk.flexex + (-5-min.re.desk.flexex)
      min.re.desk.flexex <- -5
    }
    
    
    # All the code to record max and mins for desk.ear tasks
    desk.ear.start <- apply(all.elbow.markers,1,function(row) any(row %like% "o_s"))
    desk.ear.start.true <- all.elbow.markers[desk.ear.start,]
    o_s <- max(as.data.frame(desk.ear.start.true[1]), na.rm = T)
    
    desk.ear.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "o_e"))
    desk.ear.stop.true <- all.elbow.markers[desk.ear.stop,]
    o_e <- max(as.data.frame(desk.ear.stop.true[1]), na.rm = T)
    
    elbow.desk.ear <- subset(both.elbows,Frame>=o_s & Frame<=o_e)
    
    max.re.desk.ear.flexex <- max(select(elbow.desk.ear,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.desk.ear.flexex <- min(select(elbow.desk.ear,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.desk.ear.flexex.range <-max.re.desk.ear.flexex-min.re.desk.ear.flexex
    if (min.re.desk.ear.flexex < -5) {
      max.re.desk.ear.flexex <- max.re.desk.ear.flexex + (-5-min.re.desk.ear.flexex)
      min.re.desk.ear.flexex <- -5
    } else {
      max.re.desk.ear.flexex <- max.re.desk.ear.flexex
      min.re.desk.ear.flexex <- min.re.desk.ear.flexex
    }
    
    
    # All the code to record max and mins for mobile tasks
    mobile.start <- apply(all.elbow.markers,1,function(row) any(row %like% "p_s"))
    mobile.start.true <- all.elbow.markers[mobile.start,]
    p_s <- max(as.data.frame(mobile.start.true[1]), na.rm = T)
    
    mobile.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "p_e"))
    mobile.stop.true <- all.elbow.markers[mobile.stop,]
    p_e <- max(as.data.frame(mobile.stop.true[1]), na.rm = T)
    
    elbow.mobile <- subset(both.elbows,Frame>=p_s & Frame<=p_e)
    
    
    max.re.mobile.flexex <- max(select(elbow.mobile,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.mobile.flexex <- min(select(elbow.mobile,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.mobile.flexex.range <-max.re.mobile.flexex-min.re.mobile.flexex
    if (min.re.mobile.flexex < -5) {
      max.re.mobile.flexex <- max.re.mobile.flexex + (-5-min.re.mobile.flexex)
      min.re.mobile.flexex <- -5
    } else {
      max.re.mobile.flexex <- max.re.mobile.flexex
      min.re.mobile.flexex <- min.re.mobile.flexex
    }
    
    
    # All the code to record max and mins for mobile.ear tasks
    mobile.ear.start <- apply(all.elbow.markers,1,function(row) any(row %like% "q_s"))
    mobile.ear.start.true <- all.elbow.markers[mobile.ear.start,]
    q_s <- max(as.data.frame(mobile.ear.start.true[1]), na.rm = T)
    
    mobile.ear.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "q_e"))
    mobile.ear.stop.true <- all.elbow.markers[mobile.ear.stop,]
    q_e <- max(as.data.frame(mobile.ear.stop.true[1]), na.rm = T)
    
    elbow.mobile.ear <- subset(both.elbows,Frame>=q_s & Frame<=q_e)
    
    
    max.re.mobile.ear.flexex <- max(select(elbow.mobile.ear,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.mobile.ear.flexex <- min(select(elbow.mobile.ear,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.mobile.ear.flexex.range <-max.re.mobile.ear.flexex-min.re.mobile.ear.flexex
    if (min.re.mobile.ear.flexex < -5) {
      max.re.mobile.ear.flexex <- max.re.mobile.ear.flexex + (-5-min.re.mobile.ear.flexex)
      min.re.mobile.ear.flexex <- -5
    } else {
      max.re.mobile.ear.flexex <- max.re.mobile.ear.flexex
      min.re.mobile.ear.flexex <- min.re.mobile.ear.flexex
    }
    
    
    # All the code to record max and mins for cut tasks
    cut.start <- apply(all.elbow.markers,1,function(row) any(row %like% "r_s"))
    cut.start.true <- all.elbow.markers[cut.start,]
    r_s <- max(as.data.frame(cut.start.true[1]), na.rm = T)
    
    cut.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "r_e"))
    cut.stop.true <- all.elbow.markers[cut.stop,]
    r_e <- max(as.data.frame(cut.stop.true[1]), na.rm = T)
    
    elbow.cut <- subset(both.elbows,Frame>=r_s & Frame<=r_e)
    
    
    max.re.cut.flexex <- max(select(elbow.cut,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.cut.flexex <- min(select(elbow.cut,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.cut.flexex.range <-max.re.cut.flexex-min.re.cut.flexex
    if (min.re.cut.flexex < -5) {
      max.re.cut.flexex <- max.re.cut.flexex + (-5-min.re.cut.flexex)
      min.re.cut.flexex <- -5
    } else {
      max.re.cut.flexex <- max.re.cut.flexex
      min.re.cut.flexex <- min.re.cut.flexex
    }
    
    
    # All the code to record max and mins for fork tasks
    fork.start <- apply(all.elbow.markers,1,function(row) any(row %like% "s_s"))
    fork.start.true <- all.elbow.markers[fork.start,]
    s_s <- max(as.data.frame(fork.start.true[1]), na.rm = T)
    
    fork.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "s_e"))
    fork.stop.true <- all.elbow.markers[fork.stop,]
    s_e <- max(as.data.frame(fork.stop.true[1]), na.rm = T)
    
    elbow.fork <- subset(both.elbows,Frame>=s_s & Frame<=s_e)
    
    max.re.fork.flexex <- max(select(elbow.fork,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.fork.flexex <- min(select(elbow.fork,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.fork.flexex.range <-max.re.fork.flexex-min.re.fork.flexex
    if (min.re.fork.flexex < -5) {
      max.re.fork.flexex <- max.re.fork.flexex + (-5-min.re.fork.flexex)
      min.re.fork.flexex <- -5
    } else {
      max.re.fork.flexex <- max.re.fork.flexex
      min.re.fork.flexex <- min.re.fork.flexex
    }
    
    
    # All the code to record max and mins for folder tasks
    folder.start <- apply(all.elbow.markers,1,function(row) any(row %like% "t_s"))
    folder.start.true <- all.elbow.markers[folder.start,]
    t_s <- max(as.data.frame(folder.start.true[1]), na.rm = T)
    
    folder.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "t_e"))
    folder.stop.true <- all.elbow.markers[folder.stop,]
    t_e <- max(as.data.frame(folder.stop.true[1]), na.rm = T)
    
    elbow.folder <- subset(both.elbows,Frame>=t_s & Frame<=t_e)
    
    
    
    max.re.folder.flexex <- max(select(elbow.folder,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.folder.flexex <- min(select(elbow.folder,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.folder.flexex.range <-max.re.folder.flexex-min.re.folder.flexex
    if (min.re.folder.flexex < -5) {
      max.re.folder.flexex <- max.re.folder.flexex + (-5-min.re.folder.flexex)
      min.re.folder.flexex <- -5
    } else {
      max.re.folder.flexex <- max.re.folder.flexex
      min.re.folder.flexex <- min.re.folder.flexex
    }
    
    
    
    # All the code to record max and mins for jar tasks
    jar.start <- apply(all.elbow.markers,1,function(row) any(row %like% "u_s"))
    jar.start.true <- all.elbow.markers[jar.start,]
    u_s <- max(as.data.frame(jar.start.true[1]), na.rm = T)
    
    jar.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "u_e"))
    jar.stop.true <- all.elbow.markers[jar.stop,]
    u_e <- max(as.data.frame(jar.stop.true[1]), na.rm = T)
    
    elbow.jar <- subset(both.elbows,Frame>=u_s & Frame<=u_e)
    
    max.re.jar.flexex <- max(select(elbow.jar,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.jar.flexex <- min(select(elbow.jar,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.jar.flexex.range <-max.re.jar.flexex-min.re.jar.flexex
    if (min.re.jar.flexex < -5) {
      max.re.jar.flexex <- max.re.jar.flexex + (-5-min.re.jar.flexex)
      min.re.jar.flexex <- -5
    } else {
      max.re.jar.flexex <- max.re.jar.flexex
      min.re.jar.flexex <- min.re.jar.flexex
    }
    
    
    # All the code to record max and mins for drink tasks
    drink.start <- apply(all.elbow.markers,1,function(row) any(row %like% "v_s"))
    drink.start.true <- all.elbow.markers[drink.start,]
    v_s <- max(as.data.frame(drink.start.true[1]), na.rm = T)
    
    drink.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "v_e"))
    drink.stop.true <- all.elbow.markers[drink.stop,]
    v_e <- max(as.data.frame(drink.stop.true[1]), na.rm = T)
    
    elbow.drink <- subset(both.elbows,Frame>=v_s & Frame<=v_e)
    
    max.re.drink.flexex <- max(select(elbow.drink,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.drink.flexex <- min(select(elbow.drink,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.drink.flexex.range <-max.re.drink.flexex-min.re.drink.flexex
    if (min.re.drink.flexex < -5) {
      max.re.drink.flexex <- max.re.drink.flexex + (-5-min.re.drink.flexex)
      min.re.drink.flexex <- -5
    }else {
      max.re.drink.flexex <- max.re.drink.flexex
      min.re.drink.flexex <- min.re.drink.flexex
    }
    
    
    # All the code to record max and mins for stand tasks
    stand.start <- apply(all.elbow.markers,1,function(row) any(row %like% "w_s"))
    stand.start.true <- all.elbow.markers[stand.start,]
    w_s <- max(as.data.frame(stand.start.true[1]), na.rm = T)
    
    stand.stop <- apply(all.elbow.markers,1,function(row) any(row %like% "w_e"))
    stand.stop.true <- all.elbow.markers[stand.stop,]
    w_e <- max(as.data.frame(stand.stop.true[1]), na.rm = T)
    
    elbow.stand <- subset(both.elbows,Frame>=w_s & Frame<=w_e)
    
    
    max.re.stand.flexex <- max(select(elbow.stand,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    min.re.stand.flexex <- min(select(elbow.stand,"Right Elbow Flexion/Extension"), na.rm = T) - 180
    re.stand.flexex.range <-max.re.stand.flexex-min.re.stand.flexex
    if (min.re.stand.flexex < -5) {
      max.re.stand.flexex <- max.re.stand.flexex + (-5-min.re.stand.flexex)
      min.re.stand.flexex <- -5
    } else {
      max.re.stand.flexex <- max.re.stand.flexex
      min.re.stand.flexex <- min.re.stand.flexex
    }
    ##################################################################################
    # This is building the flexion extension rows (left elbow)
    re.flexex.max.list <-c(i, max.re.vertex.flexex,max.re.occiput.flexex,max.re.canthus.flexex,
                           max.re.cbone.flexex,max.re.sternum.flexex,max.re.buckle.flexex,max.re.scapula.flexex,
                           max.re.sacrum.flexex,max.re.pubis.flexex, max.re.door.flexex, max.re.shoe.flexex, max.re.keys.flexex,
                           max.re.mouse.flexex, max.re.desk.flexex, max.re.desk.ear.flexex, max.re.mobile.flexex, max.re.mobile.ear.flexex,
                           max.re.cut.flexex, max.re.fork.flexex, max.re.folder.flexex, max.re.jar.flexex, max.re.drink.flexex, max.re.stand.flexex)
    
    re.flexex.min.list <-c(i, min.re.vertex.flexex,min.re.occiput.flexex,min.re.canthus.flexex,
                           min.re.cbone.flexex,min.re.sternum.flexex,min.re.buckle.flexex,min.re.scapula.flexex,
                           min.re.sacrum.flexex,min.re.pubis.flexex, min.re.door.flexex, min.re.shoe.flexex, min.re.keys.flexex,
                           min.re.mouse.flexex, min.re.desk.flexex, min.re.desk.ear.flexex, min.re.mobile.flexex, min.re.mobile.ear.flexex,
                           min.re.cut.flexex, min.re.fork.flexex, min.re.folder.flexex, min.re.jar.flexex, min.re.drink.flexex, min.re.stand.flexex)
    
    re.flexex.range.list <- c(i, re.vertex.flexex.range, re.occiput.flexex.range, re.canthus.flexex.range,
                              re.cbone.flexex.range,re.sternum.flexex.range,re.buckle.flexex.range,
                              re.scapula.flexex.range,re.sacrum.flexex.range,re.pubis.flexex.range,
                              re.door.flexex.range, re.shoe.flexex.range, re.keys.flexex.range,
                              re.mouse.flexex.range, re.desk.flexex.range, re.desk.ear.flexex.range, re.mobile.flexex.range, re.mobile.ear.flexex.range,
                              re.cut.flexex.range, re.fork.flexex.range, re.folder.flexex.range, re.jar.flexex.range, re.drink.flexex.range, re.stand.flexex.range)
    
    re.flexex.combo.list <- c(i, 
                              max.re.vertex.flexex,
                              min.re.vertex.flexex,
                              re.vertex.flexex.range,
                              max.re.occiput.flexex,
                              min.re.occiput.flexex,
                              re.occiput.flexex.range,
                              max.re.canthus.flexex,
                              min.re.canthus.flexex,
                              re.canthus.flexex.range,
                              max.re.cbone.flexex,
                              min.re.cbone.flexex,
                              re.cbone.flexex.range,
                              max.re.sternum.flexex,
                              min.re.sternum.flexex,
                              re.sternum.flexex.range,
                              max.re.buckle.flexex,
                              min.re.buckle.flexex,
                              re.buckle.flexex.range,
                              max.re.scapula.flexex,
                              min.re.scapula.flexex,
                              re.scapula.flexex.range,
                              max.re.sacrum.flexex,
                              min.re.sacrum.flexex,
                              re.sacrum.flexex.range,
                              max.re.pubis.flexex,
                              min.re.pubis.flexex,
                              re.pubis.flexex.range,
                              max.re.door.flexex,
                              min.re.door.flexex,
                              re.door.flexex.range,
                              max.re.shoe.flexex,
                              min.re.shoe.flexex,
                              re.shoe.flexex.range,
                              max.re.keys.flexex,
                              min.re.keys.flexex,
                              re.keys.flexex.range,
                              max.re.mouse.flexex,
                              min.re.mouse.flexex,
                              re.mouse.flexex.range,
                              max.re.desk.flexex,
                              min.re.desk.flexex,
                              re.desk.flexex.range,
                              max.re.desk.ear.flexex,
                              min.re.desk.ear.flexex,
                              re.desk.ear.flexex.range,
                              max.re.mobile.flexex,
                              min.re.mobile.flexex,
                              re.mobile.flexex.range,
                              max.re.mobile.ear.flexex,
                              min.re.mobile.ear.flexex,
                              re.mobile.ear.flexex.range,
                              max.re.cut.flexex,
                              min.re.cut.flexex,
                              re.cut.flexex.range,
                              max.re.fork.flexex,
                              min.re.fork.flexex,
                              re.fork.flexex.range,
                              max.re.folder.flexex,
                              min.re.folder.flexex,
                              re.folder.flexex.range,
                              max.re.jar.flexex,
                              min.re.jar.flexex,
                              re.jar.flexex.range,
                              max.re.drink.flexex,
                              min.re.drink.flexex,
                              re.drink.flexex.range,
                              max.re.stand.flexex,
                              min.re.stand.flexex,
                              re.stand.flexex.range
    )
    
    range.cols <- c('Particpant Number',
                    'Vertex Max','Vertex Min','Vertex Range',
                    'Occiput Max','Occiput Min','Occiput Range',
                    'Canthus Max','Canthus Min','Canthus Range',
                    'Collar Bone Max','Collar Bone Min','Collar Bone Range',
                    'Sternum Max','Sternum Min','Sternum Range',
                    'Belt Buckle Max','Belt Buckle Min','Belt Buckle Range',
                    'Scapula Max','Scapula Min','Scapula Range',
                    'Sacrum Max','Sacrum Min','Sacrum Range',
                    'Pubis Max', 'Pubis Min','Pubis Range',
                    'Door Knob Max','Door Knob Min','Door Knob Range',
                    'Shoe Max','Shoe Min','Shoe Range',
                    'Keys Max','Keys Min','Keys Range',
                    'Mouse Max','Mouse Min','Mouse Range',
                    'Desk Max','Desk Min','Desk Range',
                    'Desk Ear Max','Desk Ear Min','Desk Ear Range',
                    'Mobile Max','Mobile Min','Mobile Range',
                    'Mobile Ear Max','Mobile Ear Min','Mobile Ear Range',
                    'Cut Max','Cut Min','Cut Range',
                    'Fork Max','Fork Min','Fork Range',
                    'Folder Max','Folder Min','Folder Range',
                    'Jar Max','Jar Min','Jar Range',
                    'Drink Max','Drink Min','Drink Range',
                    'Stand Max','Stand Min','Stand Range'
    )
    
    flexex.matrix.re <- matrix(re.flexex.combo.list,
                               nrow = 1,
                               ncol = 70,
                               dimnames = NULL)
    colnames(flexex.matrix.re) <- range.cols
    final_results_flexex_re[[i]] <- as.data.frame(flexex.matrix.re)
  }
}


final_results_table_flexex_re = do.call(rbind, final_results_flexex_re)

write.csv(final_results_table_flexex_re, "Results/Results_all_patients_neg5_re_flexex_v3.csv", row.names = F)
