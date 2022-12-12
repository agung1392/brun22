#' Visualisasi Simulasi Sistem Kejadian Diskret dengan satu Server dan n kali replikasi
#'
#' Penjelasan lebih jauh ada pada link berikut ini
#' \href{https://binusianorg-my.sharepoint.com/personal/i_yudistira_binus_ac_id/_layouts/15/guestaccess.aspx?docid=02922ecc2fe594582a1bcecdbfbf1d3a1&authkey=AV8ib0HpB_D9aA2RhpkfID4&e=vQxLPz}{Visualisasi}
#'
#' @param AK adalah sebuah fungsi sebaran peluang / nilai konstan untuk waktu antar kedatangan entiti
#' @param Lama adalah sebuah fungsi sebaran peluang /nilai konstan untuk lamanya proses di server
#' @param RUN adalah sebuah nilai numerik, yang memberikan rentang waktu simulasi nilai terpasangnya adalah 100
#' @param n adalah sebuah nilai numerik, yang memberikan banyaknya replikasi, nilai terpasangnya adalah 30
#' @param graph pilih salah satu dari "None", "Queue", "Utility", "Wait_Time", "Flow_Time", "Total_Serv", "Avg_Flow", "Avg_Waiting", "Queue_Rate",
#'"Server_Downtime","Avg_Queue_Length"
#' @param seed sebuah nilai numerik untuk bibit angka acak, nilai terpasangnya adalah as.numeric(sys.time)
#'
#' @return
#' \strong{dataset} arrival, resources,
#' @return statistics yang meliputi
#' \emph{total_serv, avg_flow_time, avg_wait_time, longest_flow_time, longest_waiting_time
#' last_time, server_downtime, queue_rate, utility_rate, avg_queue_length, avg_entity_in_system}
#' @export
#'
#' @examples
#' brun_viz_n_single(AK = function() round(rexp(n=1, rate=0.40),3),
#'          Lama = function() round(rexp(n=1, rate=0.45),3), graph="Utility")

brun_viz_n_single <- function(AK, Lama, RUN=100, n = 30,
                              graph=c("None", "Utility", "Wait_Time", "Flow_Time", "Total_Serv",
                                      "Avg_Flow", "Avg_Waiting", "Queue_Rate", "Server_Downtime",
                                      "Avg_Queue_Length"), seed=as.numeric(Sys.time())) {

  set.seed(seed)
  env <- simmer("single_server")

  # Trajectory
  lintas <- trajectory() %>%
    seize("Server") %>%
    timeout(Lama) %>%
    release("Server")

  # resource and entity generation
  envs <- lapply(1:n, function(i) {
    simmer("single_server") %>%
      add_resource("Server", 1) %>%
      add_generator("ent", lintas, AK, mon=2) %>%
      run(RUN) %>% invisible
  })

  envs %>%
    get_mon_arrivals(ongoing=TRUE) %>%
    filter(start_time>0) %>%
    group_by(replication) %>% arrange(start_time, .by_group = TRUE)

  # Simulation output for entity arrival state, with ongoing=TRUE
  result <- NULL
  for (j in 1:n){
    out <- envs[[j]] %>%
      get_mon_arrivals(ongoing=TRUE) %>%
      subset(start_time>0) %>%
      transform(serv_start_time = end_time - activity_time) %>%
      transform(flow_time = round(end_time - start_time,3)) %>%
      transform(wait_time = round(flow_time - activity_time,3)) %>%
      group_by(replication) %>% arrange(start_time, .by_group = TRUE)

    px <- envs[[j]] %>% peek(Inf, verbose = TRUE)
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
      out$end_time[out$finished==FALSE] - out$start_time[out$finished==FALSE]
    out$wait_time[is.na(out$wait_time)] <-
      out$flow_time[out$finished==FALSE] - out$activity_time[out$finished==FALSE]

    out <- out %>% mutate(replication=j)
    result <- bind_rows(result,out)
  }
  tot_serv <- result %>% group_by(replication) %>% count(replication,
                                                         name="total_serv")

  avg_flow_time <- result %>% group_by(replication) %>%
    summarize(avg_flow_time = mean(flow_time))
  avg_wait_time <- result %>% group_by(replication) %>%
    summarize(avg_wait_time = mean(wait_time))
  max_flow <- result %>% group_by(replication) %>%
    summarize(longest_flow_time = max(flow_time))
  max_wait <- result %>% group_by(replication) %>%
    summarize(longest_waiting_time = max(wait_time))
  Last_time <- result %>% group_by(replication) %>% slice_tail(n=1) %>%
    mutate(last_time = end_time) %>%
    dplyr::select(replication, last_time)

  server_downtime <- Last_time %>% group_by(replication) %>%
    .$last_time - RUN
  server_downtime <- tibble(replication= Last_time$replication,
                            server_downtime=pmax(0,server_downtime))

  has_result <-  left_join(tot_serv, avg_flow_time, "replication") %>%
    left_join(avg_wait_time, "replication") %>%
    left_join(max_flow, "replication") %>% left_join(max_wait, "replication") %>%
    left_join(Last_time, "replication") %>% left_join(server_downtime, "replication")

# output processing ========================================================

  # Discrete event simulation with input from data frame "out2"
  result2 <- hasRes <- OUT_Res <- NULL
  for(rpl in 1: n){
    out2 <- result %>% filter(replication == rpl)

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
    if (Last_time$last_time[rpl] <= RUN){
      TIME <- c(TIME, RUN)
      QUEUE <- c(QUEUE, 0)
      SERVER <- c(SERVER, 0)
      SYSTEM <- c(SYSTEM, 0)
    }
    OUTRes <- tibble(replication=rpl, time=TIME, queue=QUEUE, server=SERVER,
                        system=SYSTEM)

    # simulation output statistics
    rentang <- diff(range(TIME)) # time range
    Q_rate <- sum(diff(TIME) * QUEUE[-length(QUEUE)])/rentang # queue Rate
    Utility <- sum(diff(TIME) * SERVER[-length(SERVER)])/rentang # utility rate
    queue_mean <- round(mean(out_res$queue),3) # average queue length
    entity_in_system <- round(mean(out_res$system),3)

    # statistics output
    has <- tibble(replication = rpl,
                  queue_rate = Q_rate,
                  utility_rate = Utility,
                  avg_queue_length = queue_mean,
                  avg_entity_in_system = entity_in_system )

    OUT_Res <- bind_rows(OUT_Res, OUTRes)
    out_res$replication <- rpl
    hasRes <- bind_rows(hasRes, out_res)
    result2 <- bind_rows(result2, has)
  }
  hasRes <- tibble(hasRes)
  out_stat <- left_join(has_result, result2, "replication")
  OUT <- list(arrival_out = result, resources_out = hasRes,
              statistics_out = out_stat, rsc_out = OUT_Res)

# Graph from out_stat (statistics_out), of system parameters
# average length of time (flow time) the entity is in the system for each replication
  uavg_flowT <- summary(OUT[[3]]$avg_flow_time)
  p_avg_flowT <- OUT[[3]] %>%
    ggplot(aes(x=as.numeric(replication), y=avg_flow_time)) +
    geom_line(color="navy") +
    geom_point() +
    geom_hline(yintercept = uavg_flowT[c(2,3,5)], linetype="dashed",
               col=c("red4", "darkblue", "red4")) +
    annotate("text", x=c(3,3,3), y=uavg_flowT[c(2,3,5)],
             label=c(paste("Q1= ", round(uavg_flowT[2],2)),
                     paste("Med= ",round(uavg_flowT[3],2)),
                     paste("Q3= ",round(uavg_flowT[5],2))),
             col = "darkgreen", size = 3.5, vjust=1.20 ) + ylab("Avg Flow Time") +
    xlab("Replication") + scale_x_continuous(breaks = 1:n) +
    labs(title= " Average Flow Time",
         subtitle = paste("Min= ", round(uavg_flowT[1],2),
                          ", Mean= ", round(uavg_flowT[4],2),
                          ", Max= ",round(uavg_flowT[6],2),
                          " (Whole Replication)",sep="")) +
    theme(plot.title = element_text(size = 12, color = "black"),
          plot.subtitle = element_text(size = 10, color = "cyan4"))

# average length of time spent in the queue on each replication
  uavg_waitT <- summary(OUT[[3]]$avg_wait_time)
  p_avg_waitT <- OUT[[3]] %>%
    ggplot(aes(x=as.numeric(replication), y=avg_wait_time)) +
    geom_line(color="navy") +
    geom_point() +
    geom_hline(yintercept = uavg_waitT[c(2,3,5)], linetype="dashed",
               col=c("red4", "darkblue", "red4")) +
    annotate("text", x=c(3,3,3), y=uavg_waitT[c(2,3,5)],
             label=c(paste("Q1= ", round(uavg_waitT[2],2)),
                     paste("Med= ",round(uavg_waitT[3],2)),
                     paste("Q3= ",round(uavg_waitT[5],2))),
             col = "darkgreen", size = 3.5, vjust=1.20 ) + ylab("Avg waiting Time in Queue") +
    xlab("Replication") + scale_x_continuous(breaks = 1:n) +
    labs(title= " Average Length of Time Spent in Queue",
         subtitle = paste("Min= ", round(uavg_waitT[1],2),
                          ", Mean= ", round(uavg_waitT[4],2),
                          ", Max= ",round(uavg_waitT[6],2),
                          " (Whole Replication)",sep="")) +
    theme(plot.title = element_text(size = 12, color = "black"),
          plot.subtitle = element_text(size = 10, color = "cyan4"))

  ur <- summary(OUT[[3]]$utility_rate)
  p_utility <- OUT[[3]] %>%
    ggplot(aes(x=as.numeric(replication), y=utility_rate)) +
    geom_line(color="navy") +
    geom_point() +
    geom_hline(yintercept = ur[c(2, 3, 5)], linetype="dashed",
               col=c("red4", "darkblue", "red4")) +
    annotate("text", x=c(3,3,3), y=ur[c(2,3,5)], label=c(paste("Q1= ", round(ur[2],2)),
              paste("Med= ",round(ur[3],2)), paste("Q3= ",round(ur[5],2))),
              col = "darkgreen", size = 3.5, vjust=1.20 ) + ylab("Utility Rate") +
              xlab("Replication") + ylim(c(min(OUT[[3]]$utility_rate)-0.025, 1)) +
    scale_x_continuous(breaks = 1:n) +
    labs(title= " Utility Rate",
         subtitle = paste("Min= ", round(ur[1],2),
                          ", Mean= ", round(ur[4],2),
                          ", Max= ",round(ur[6],2),
                          " (Whole Replication)",sep="")) +
    theme(plot.title = element_text(size = 12, color = "black"),
          plot.subtitle = element_text(size = 10, color = "cyan4"))

# queue_rate on each replication
  uqueue_rate <- summary(OUT[[3]]$queue_rate)
  p_queue_rate <- OUT[[3]] %>%
    ggplot(aes(x=as.numeric(replication), y=queue_rate)) +
    geom_line(color="navy") +
    geom_point() +
    geom_hline(yintercept = uqueue_rate[c(2,3,5)], linetype="dashed",
               col=c("red4", "darkblue", "red4")) +
    annotate("text", x=c(3,3,3), y=uqueue_rate[c(2,3,5)],
             label=c(paste("Q1= ", round(uqueue_rate[2],2)),
                     paste("Med= ",round(uqueue_rate[3],2)),
                     paste("Q3= ",round(uqueue_rate[5],2))),
             col = "darkgreen", size = 3.5, vjust=1.20 ) +
    ylab("Queue Rate") +
    xlab("Replication") + scale_x_continuous(breaks = 1:n) +
    labs(title= "Queue Rate on each Replication",
         subtitle = paste("Min= ", round(uqueue_rate[1],2),
                          ", Mean= ", round(uqueue_rate[4],2),
                          ", Max= ",round(uqueue_rate[6],2),
                          " (Whole Replication)",sep="")) +
    theme(plot.title = element_text(size = 12, color = "black"),
          plot.subtitle = element_text(size = 10, color = "cyan4"))

# range between end time of simulation and server downtime
  userver_downtime <- summary(OUT[[3]]$server_downtime)
  p_server_downtime <- OUT[[3]] %>%
    ggplot(aes(x=as.numeric(replication), y=server_downtime)) +
    geom_line(color="navy") +
    geom_point() +
    geom_hline(yintercept = userver_downtime[c(2,3,5)], linetype="dashed",
               col=c("red4", "darkblue", "red4")) +
    annotate("text", x=c(3,3,3), y=userver_downtime[c(2,3,5)],
             label=c(paste("Q1= ", round(userver_downtime[2],2)),
                     paste("Med= ",round(userver_downtime[3],2)),
                     paste("Q3= ",round(userver_downtime[5],2))),
             col = "darkgreen", size = 3.5, vjust=1.20 ) +
    ylab("Range of Time") +
    xlab("Replication") + scale_x_continuous(breaks = 1:n) +
    labs(title= "Range Between Simulation End Time and Server Downtime",
         subtitle = paste("Min= ", round(userver_downtime[1],2),
                          ", Mean= ", round(userver_downtime[4],2),
                          ", Max= ",round(userver_downtime[6],2),
                          " (Whole Replication)",sep="")) +
    theme(plot.title = element_text(size = 12, color = "black"),
          plot.subtitle = element_text(size = 10, color = "cyan4"))


# bar chart of total entities that have been served on the system
  serv <- summary(OUT[[3]]$total_serv)
  p_entity_serv <- ggplot(OUT[[3]], aes(x = replication, y = total_serv)) +
    geom_bar(stat = "identity", fill=gray(0.45)) +
    geom_text(aes(label = total_serv), vjust = 1, col="white")+
    scale_x_continuous(breaks=1:n) + xlab("Replication") +
    ylab("Total Served") +
    labs(title=paste("Q1= ", round(serv[2],2),
                     ", Median= ",round(serv[3],2),", Q3= ",round(serv[5],2), sep=""),
         subtitle = paste("Min= ", round(serv[1],2),", Mean= ", round(serv[4],2),
                          ", Max= ",round(serv[6],2)," (Whole Replication)",sep=""))+
    theme(plot.title = element_text(size = 12, color = "black"),
          plot.subtitle = element_text(size = 10, color = "cyan4"))

# Average queue length and average entity in system
  queueL <- summary(OUT[[3]]$avg_queue_length)
  systemL <- summary(OUT[[3]]$avg_entity_in_system)

  df1 <- OUT[[3]] %>% dplyr::select(replication, avg_queue_length, avg_entity_in_system)
  df <- as.data.frame(df1)
  dfx <- melt(df, id.vars = "replication")
  levels(dfx$variable) <- c("Avg Queue","Avg Entity in System")

  p_avg_queue <- ggplot(dfx, aes(x = replication, y = value, linetype=variable)) +
    geom_line() + geom_point() +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks=1:n) +
    scale_linetype_discrete(name=" ") + ylab("Average") + xlab("Replication") +
    labs(title=paste("Avg Queue Length: ", "Median= ", round(queueL[3],2),
                     ", Mean= ", round(queueL[4],2), sep=""),
         subtitle = paste("Avg Entity in System: ", "Median= ", round(systemL[3],2),
                          ", Mean= ", round(systemL[4],2), sep=""))+
    theme(plot.title = element_text(size = 10, color = "black"),
          plot.subtitle = element_text(size = 10, color = "gray40"))

#  urw <- quantile(OUT[[1]]$wait_time, probs = c(0, 0.25,0.50, 0.75, 1))
  urw <- summary(OUT[[1]]$wait_time)
  p_wait_time <- OUT[[1]] %>% ggplot(aes(x=factor(replication), y=wait_time)) +
    geom_boxplot(fill="chartreuse4") +
    geom_hline(yintercept = urw[c(2,3,5)], linetype="dashed")+
    labs(title=paste("Q1= ", round(urw[2],2),
               ", Median= ",round(urw[3],2),", Q3= ",round(urw[5],2), sep=""),
         subtitle = paste("Min= ", round(urw[1],2),", Mean= ", round(urw[4],2),
                          ", Max= ",round(urw[6],2)," (Whole Replication)",sep="")) +
    ylab("Wait Time") + xlab("Replication") +
    theme(plot.title = element_text(size = 12, color = "black"),
          plot.subtitle = element_text(size = 10, color = "cyan4"))

  urf <- summary(OUT[[1]]$flow_time)
  p_flow_time <- OUT[[1]] %>% ggplot(aes(x=factor(replication), y=flow_time)) +
    geom_boxplot(fill="cadetblue4") +
    geom_hline(yintercept = urf[c(2,3,5)], linetype="dashed")+
    labs(title=paste("Q1= ", round(urf[2],2),", Median= ",round(urf[3],2),
                     ", Q3= ",round(urf[5],2), sep=""),
         subtitle = paste("Min= ", round(urf[1],2),", Mean= ", round(urf[4],2),
                          ", Max= ",round(urf[6],2)," (Whole Replication)",sep="")) +
    ylab("Flow Time") + xlab("Replication") +
    theme(plot.title = element_text(size = 12, color = "black"),
          plot.subtitle = element_text(size = 10, color = "cyan4"))

  if (all(graph == c("None", "Utility", "Wait_Time", "Flow_Time", "Total_Serv",
                 "Avg_Flow", "Avg_Waiting", "Queue_Rate", "Server_Downtime",
                 "Avg_Queue_Length"))){
    graph = "None"
  }
    switch(graph,
           None = NULL,
           Utility = show(p_utility),
           Wait_Time = show(p_wait_time),
           Flow_Time = show(p_flow_time),
           Total_Serv = show(p_entity_serv),
           Avg_Flow = show(p_avg_flowT),
           Avg_Waiting = show(p_avg_waitT),
           Queue_Rate = show(p_queue_rate),
           Server_Downtime = show(p_server_downtime),
           Avg_Queue_Length = show(p_avg_queue)
    )

  return(OUT)
}

# end ================================================================



