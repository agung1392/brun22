#' Simulasi Sistem Kejadian Diskret dengan Percabangan (kasus 3)
#'
#' Pemrosesan akhir dari suatu pabrik perakitan TV, digambarkan sebagai berikut:
#' TV yang datang dari lini perakitan akan diperiksa kesesuaian mutunya oleh stasiun
#' pemeriksa (\emph{inspector}), dalam kasus ini tersedia dua stasiun pemeriksa yang bekerja
#' paralel dan identik, yang mana lamanya proses pemeriksaan menyebar \strong{Uniform(6, 12)}.
#' Berdasarkan pengalaman 15% dari hasil pemeriksaan memerlukan perbaikan, yang dilakukan
#' oleh stasiun perbaikan (\emph{adjustor}) dan sisanya 85% siap dikirim ke konsumen.
#' Tersedia dua stasiun perbaikan (identik) yang bekerja paralel, dengan lama proses perbaikan
#' merupakan peubah acak yang menyebar \strong{Uniform(20, 40)}. TV set yang berasal dari stasiun perbaikan
#' kembali dikirim ke antrian stasiun pemeriksaan. Waktu antar kedatangan TV set dari lini produksi
#' adalah \strong{Uniform(3.5, 7.5)}.
#'
#' @param L_insp adalah berupa fungsi yang memberikan nilai untuk lamanya aktivitas pemeriksaan oleh insp
#' @param L_adjt adalah berupa fungsi yang memberikan nilai untuk lamanya aktivitas perbaikan oleh adjt
#' @param ak adalah berupa fungsi yang digunakan untuk membangkitkan waktu antar kedatangan TV Set
#' @param chek adalah sebuah nilai numerik yang memberikan peluang sebuah TV Set yang telah diperiksa memerlukan perbaikan
#' @param rsc adalah vektor numerik integer, yang memberikan nilai besarnya kapasitas \emph{inspector} dan \emph{adjustor}
#' @param rn adalah sebuah nilai numerik untuk rentang waktu simulasi, nilai terpasangnya adalah 300
#' @param ongoing sebuah nilai logical, yang mana nilai TRUE, berarti mencatat kedatangan entiti yang masih sedang diproses di sistem.  Hal ini ditunjukkan oleh nilai kolom finished FALSE
#'
#' @return \strong{dataset} Arrivals dan Attributes
#' @export
#'
#' @examples
#' brun_branch_03(function() runif(1, 6, 12), function() runif(1, 20, 40), function() runif(1, 3.5, 7.5))

brun_branch_03 <- function(L_insp, L_adjt, ak, chek = 0.15, rsc=c(2, 2), rn=200, ongoing=FALSE)
  {
      if (!is.function(L_insp) || !is.function(L_adjt) || !is.function(ak))
      {
        stop("L_insp, L_adjt dan ak harus berupa fungsi numeric")
      }

      CHECK_TV <- function() runif(1) < chek
      envs <- simmer("TV")

      insp <- trajectory()

      for (j in 1:2){
        adjt <- trajectory() %>%
          set_global("count_adjt", 1, mod="+") %>%
          seize("adjustor",1) %>%
          timeout(L_adjt) %>%
          release("adjustor") %>%
          join(insp)

        kirim <- trajectory() %>%
          set_global("count_kirim",1, mod="+") %>%
          set_attribute("system_time",
                        function() now(envs) - get_attribute(envs, "waktu.tiba"))
          # log_("selesai ...")

        insp <- trajectory() %>%
          set_global("count_insp", 1, mod="+") %>%
          seize("inspector",1) %>%
          timeout(L_insp) %>%
          release("inspector") %>%
          branch(
            CHECK_TV, FALSE,
            join(adjt)
          ) %>%
          join(kirim)

        lintas <- trajectory() %>%
          # log_("mulai ...") %>%
          set_attribute("waktu.tiba", function() now(envs)) %>%
          join(insp)
      }
      envs %>%
        add_resource("inspector", rsc[1]) %>%
        add_resource("adjustor", rsc[2]) %>%
        add_generator("TV_set", lintas, ak, mon=2) %>%
        run(rn) %>% invisible()

      out1 <- as_tibble(get_mon_arrivals(envs, ongoing))
      out2 <- as_tibble(get_mon_attributes(envs))
      out <- list(Arrivals = out1, Attributes = out2)
      return(out)
  }

# brun_branch_03(function() runif(1, 6, 12), function() runif(1, 20, 40), function() runif(1, 3.5, 7.5))
