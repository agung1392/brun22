#' Visualisasi DES output - tanpa replikasi
#'
#' lihat link:
#' \href{https://binusianorg-my.sharepoint.com/personal/i_yudistira_binus_ac_id/_layouts/15/guestaccess.aspx?docid=02922ecc2fe594582a1bcecdbfbf1d3a1&authkey=AV8ib0HpB_D9aA2RhpkfID4&e=7tNyZe}{Deskripsi}
#'
#' @param AK adalah sebuah fungsi yang memberikan waktu antar kedatangan suatu entiti
#' @param Lama adalah sebuah fungsi yang memberikan lamanya proses layanan di server
#' @param RUN adalah sebuah nilai numerik yang memberikan rentang waktu simulasi, nilai defaultnya adalah 100
#' @param graph choose salah satu dari "None", Queue", "Utility", "System", atau "All", untuk menayangkan atau tidak menayangkan grafik
#'
#' @return
#' \strong{dataset} "out_going_output", "resource_ouput", "wide_stat_value","long_stat_value"
#' @return
#' \strong{grafik} "Que Length", "Total_Entities_in System" and "Server_Utility"
#'
#' @export
#'
#' @examples
#' brun_viz_single(AK = function() round(rexp(n = 1, rate = 0.40),3),
#'           Lama = function() round(rexp(n = 1, rate = 0.45),3), graph = "System")
#'
#'

brun_viz_single <- function(AK, Lama, RUN=100, graph = c("None", "Queue", "Utility", "System", "All"),
                        seed=as.numeric(Sys.time())) {
  set.seed(seed)
  env <- simmer("single_server")

  # Trajectory
  lintas <- trajectory() %>%
    seize("Server") %>%
    timeout(Lama) %>%
    release("Server")

  # resource and entity generation
  env %>%
    add_resource("Server", 1) %>%
    add_generator("ent", lintas, AK, mon=2) %>%
    run(RUN) %>% invisible

  # Simulation output for entity arrival state, with ongoing=TRUE
  out <- env %>%
    get_mon_arrivals(ongoing=TRUE) %>%
    subset(start_time>0) %>%
    transform(serv_start_time = end_time - activity_time) %>%
    transform(flow_time = round(end_time - start_time,3)) %>%
    transform(wait_time = round(flow_time - activity_time,3)) %>%
    .[order(.$start_time),]

  row.names(out) <- 1:dim(out)[1]

  px <- env %>% peek(Inf, verbose = TRUE)
  wt <- px$time[px$process %in% out$name]
  endT <- max(out$end_time, na.rm=TRUE)
  k <- sum(is.na(out$end_time))

  if(k > 0){
    servTime <- numeric(k)
    endTime <- numeric(k)
    actvTime <- numeric(k)
    waitTime <- numeric(k)
    outF <- filter(out, finished == FALSE)
    for(i in 1:k){
      if(outF$start_time[i] < endT){
        servTime[i] <- endT
        if(i == 1){
          endTime[i] <- wt
          actvTime[i] <- endTime[i] - servTime[i]
          endT <- endTime[i]
        }else{
          actvTime[i] <- round(rexp(n=1, rate=0.45),3)
          endTime[i] <- servTime[i] + actvTime[i]
          endT <- endTime[i]
        }
      }else{
        servTime[i] <- outF$start_time[i]
        actvTime[i] <- round(rexp(n=1, rate=0.45),3)
        endTime[i] <- servTime[i] + actvTime[i]
        endT <- endTime[i]
      }
    }
    out$end_time[is.na(out$end_time)] <- endTime
    out$activity_time[is.na(out$activity_time)] <- actvTime
    out$serv_start_time[is.na(out$serv_start_time)] <- servTime
  }
  out$flow_time[is.na(out$flow_time)] <-
    round(out$end_time[out$finished==FALSE] - out$start_time[out$finished==FALSE],3)
  out$wait_time[is.na(out$wait_time)] <-
    round(out$flow_time[out$finished==FALSE] - out$activity_time[out$finished==FALSE],3)

  out2 <- out # creating data frame out2

# Data out ========================================================

  # Discrete event simulation with input from data frame "out2"
  env2 <- simmer("out2")

  lintas <- trajectory() %>%
    seize("Server", 1) %>%
    timeout_from_attribute("activity_time") %>%
    release("Server",1)

  env2 %>%
    add_resource("Server", 1) %>%
    add_dataframe("ent", lintas, data=out2, mon=2,
                  col_time="start_time", time = "absolute",
                  col_attributes = "activity_time") %>%
    run() %>% invisible

  # Simulation output for server state
  out3 <- get_mon_arrivals(env2) %>%
    transform(serv_start_time = end_time - activity_time) %>%
    transform(flow_time = round(end_time - start_time,3)) %>%
    transform(wait_time = round(flow_time - activity_time,3)) %>%
    .[order(.$start_time),] %>%
    dplyr::select(-finished)

  Last_time <- out3$end_time[length(out3$end_time)] # system last time

  out_res <- env2 %>% get_mon_resources()
  if (out_res$time[1] != 0){
    TIME <- c(0,out_res$time)
    QUEUE <- c(0, out_res$queue)
    SERVER <- c(0, out_res$server)
    SYSTEM <- c(0, out_res$system)
  } else {
    TIME <- out_res$time
    QUEUE <- out_res$queue
    SERVER <- out_res$server
    SYSTEM <- out_res$system
  }
  if (Last_time <= RUN){
    TIME <- c(TIME, RUN)
    QUEUE <- c(QUEUE, 0)
    SERVER <- c(SERVER, 0)
    SYSTEM <- c(SYSTEM, 0)
  }

  # creating grafik
  p1 <- ggplot()+
    geom_step(mapping=aes(x=TIME, y=QUEUE), direction = "hv", col="darkgreen") +
    ylab("Queue length") + xlab("Time") +
    labs(title="Queue Length")

  p2 <- ggplot()+
    geom_step(mapping=aes(x=TIME, y=SERVER), direction = "hv", lwd=0.9, col="navy")+
    scale_y_continuous(breaks=c(0, 1))+
    ylab("Utility Rate") + xlab("Time") +
    labs(title = "Utility: 0 = idle, 1 = busy")

  p3 <- ggplot()+
    geom_step(mapping=aes(x=TIME, y=SYSTEM), direction = "hv", col="darkmagenta")+
    ylab("Total entity in system") + xlab("Time") +
    labs(title="Total Entities in System")

  # Graphics show
  if (all(graph == c("None", "Queue", "Utility", "System", "All"))){
    graph = "None"
  }
    switch(graph,
           None = NULL,
           Queue = show(p1+scale_y_continuous(breaks=seq(0,max(QUEUE)))),
           Utility = show(p2),
           System = show(p3+scale_y_continuous(breaks=seq(0,max(SYSTEM)))),
           All = {
             pushViewport(viewport(layout = grid.layout(2, 2)))
             print(p2, vp = viewport(layout.pos.row = 1,layout.pos.col =
                                       c(1,2)))
             print(p1+ggplot2::ylim(0,max(SYSTEM)),
                   vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
             print(p3+ggplot2::ylim(0,max(SYSTEM)),
                   vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
           })


  # simulation output statistics
  rentang <- diff(range(TIME)) # time range
  Q_rate <- sum(diff(TIME) * QUEUE[-length(QUEUE)])/rentang # queue Rate
  Utility <- sum(diff(TIME) * SERVER[-length(SERVER)])/rentang # utility rate

  flow_time <- mean(out3$flow_time) # average flow time
  max_flow <- max(out3$flow_time)
  waiting_time <- mean(out3$wait_time) # average waiting time
  max_wait <- max(out3$wait_time) # longest waiting time
  queue_mean <- round(mean(out_res$queue),3) # average queue length
  entity_in_system <- round(mean(out_res$system),3)
  server_downtime <- max(0,Last_time - RUN) # length of time between server downtime and system end time

  nama <- c("queue_rate",
            "utility_rate",
            "system_last_time",
            "avg_flow_time",
            "avg_waiting_time",
            "longest_flow_time",
            "longest_waiting_time",
            "average_queue_length",
            "avg_entity_in_system",
            "server_downtime")

  val <- c(Q_rate,
           Utility,
           Last_time,
           flow_time,
           waiting_time,
           max_flow,
           max_wait,
           queue_mean,
           entity_in_system,
           server_downtime)

  stat_value1 <- tibble(name1 = nama[1:5], value1 = val[1:5])
  stat_value2 <- tibble(name2 = nama[6:10], value2 = val[6:10])

  Wide_stat_value <- bind_cols(stat_value1, stat_value2)
  Long_Stat_value <- tibble(name = nama, value = val)

  # function output
  has <- list(out_going_output = out,
              resource_output = out_res,
              wide_stat_value1 = Wide_stat_value,
              long_stat_value2 = Long_Stat_value
  )
  return(has)

}

# End Function ============================================================

brun_viz_single(AK = function() round(rexp(n=1, rate=0.4),3),
           Lama = function() round(rexp(n=1, rate=0.45),3), graph="System")
