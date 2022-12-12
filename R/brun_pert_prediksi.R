#' Prediksi waktu penyelesaian proyek dengan pendekatan Simulasi Kejadian Diskret.
#'
#' @description  Silahkan baca link berikut ini:
#' \href{https://binusianorg-my.sharepoint.com/personal/i_yudistira_binus_ac_id/_layouts/15/guestaccess.aspx?docid=03e28f10006114071bb8a92e45815a86e&authkey=AV72oobwHx-nMnz1CrfFCog&e=eLyBJ2}{Deskripsi}
#'
#' @param n suatu numerik, nilai n ini menentukan banyaknya replikasi.
#' @return \strong{dataset} waktu penyelesaian proyek per ulangan
#' @return Ringkasan Statistik
#' @return Selang_Kepercayaan
#' @examples
#' brun_pert_prediksi(n = 100)

######################################################
# Program : brun_pert_prediksi.R                     #
# Tujuan  : Melakukan simulasi untuk mendapatkan     #
#           prediksi lamanya waktu penyelesaian      #
#           suatu proyeks dengan menggunakan         #
#           simulasi sistem kejadian diskret         #
# Tanggal : 20 Juli 2022                             #
# Oleh    : I G.A. Anom Yudistira                    #
# Versi   : v 0.1.0                                  #
######################################################

# require(simmer)
# require(EnvStats)
# require(parallel)

brun_pert_prediksi <- function(n = 100)
  {
      pert2 <- simmer("PERT2")

      durasi_1 <- function() rtri(1, 1, 5, 3)
      durasi_2 <- function() rtri(1, 3, 9, 6)
      durasi_3 <- function() rtri(1, 10, 19, 13)
      durasi_4 <- function() rtri(1, 3, 12, 9)
      durasi_5 <- function() rtri(1, 1, 8, 3)
      durasi_6 <- function() rtri(1, 8, 16, 9)
      durasi_7 <- function() rtri(1, 4, 13, 7)
      durasi_8 <- function() rtri(1, 3, 9, 6)
      durasi_9 <- function() rtri(1, 1, 8, 3)

      traj_1 <- trajectory() %>%
        set_global("waktu_1",durasi_1)

      traj_2 <- trajectory() %>%
        set_global("waktu_2", durasi_2)

      traj_3 <- trajectory() %>%
        set_global("waktu_3", durasi_3)

      traj_4 <- trajectory() %>%
        set_global("waktu_4",durasi_4)

      traj_5 <- trajectory() %>%
        set_global("waktu_5", durasi_5)

      traj_6 <- trajectory() %>%
        set_global("waktu_6", durasi_6)

      traj_7 <- trajectory() %>%
        set_global("waktu_7",durasi_7)

      traj_8 <- trajectory() %>%
        set_global("waktu_8", durasi_8)

      traj_9 <- trajectory() %>%
        set_global("waktu_9", durasi_9)

      traj_148 <- trajectory() %>%
        timeout_from_global("waktu_1") %>%
        timeout_from_global("waktu_4") %>%
        timeout_from_global("waktu_8") %>%
        set_attribute("waktu_148", function() now(pert2))

      traj_156 <- trajectory() %>%
        timeout_from_global("waktu_1") %>%
        timeout_from_global("waktu_5") %>%
        timeout_from_global("waktu_6") %>%
        set_attribute("waktu_156", function() now(pert2))

      traj_1579 <- trajectory() %>%
        timeout_from_global("waktu_1") %>%
        timeout_from_global("waktu_5") %>%
        timeout_from_global("waktu_7") %>%
        timeout_from_global("waktu_9") %>%
        set_attribute("waktu_1579", function() now(pert2))

      traj_26 <- trajectory() %>%
        timeout_from_global("waktu_2") %>%
        timeout_from_global("waktu_6") %>%
        set_attribute("waktu_26", function() now(pert2))

      traj_279 <- trajectory() %>%
        timeout_from_global("waktu_2") %>%
        timeout_from_global("waktu_7") %>%
        timeout_from_global("waktu_9") %>%
        set_attribute("waktu_279", function() now(pert2))

      traj_39 <- trajectory() %>%
        timeout_from_global("waktu_3") %>%
        timeout_from_global("waktu_9") %>%
        set_attribute("waktu_39", function() now(pert2))

      traj_pert2 <- trajectory() %>%
        clone(
          n = 6,
          traj_148,
          traj_156,
          traj_1579,
          traj_26,
          traj_279,
          traj_39
        ) %>%
        synchronize(wait = TRUE) %>%
        set_attribute("waktu_pelaksanaan", function() now(pert2))

      traj_awal <- trajectory() %>%
        clone(
          n = 9,
          traj_1,
          traj_2,
          traj_3,
          traj_4,
          traj_5,
          traj_6,
          traj_7,
          traj_8,
          traj_9
        )

      pert2 <- mclapply(1:n, function(i) {
        simmer("PERT2") %>%
          add_generator("dummy1", traj_awal, at(0), mon=2) %>%
          add_generator("dummy2", traj_pert2, at(0), mon=2) %>%
          run() %>% invisible %>%
          wrap()
      })
      has <- get_mon_attributes(pert2)
      hasil <- subset(has, key == "waktu_pelaksanaan",
                      select = c(key, time, replication))
      rownames(hasil) <- NULL
      rks <- summary(hasil$time)
      SE <- sd(hasil$time)
      SK <- c(mean(hasil$time) + c(-1, 1) * 2*SE)
      out <- list(WP_untuk_semua_ulangan =hasil, Ringkasan = rks, Selang_Kepercayaan = SK)
      return(out)
}




