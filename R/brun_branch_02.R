#' Simulasi Sistem Kejadian Diskret dengan Percabangan (kasus 2)
#'
#' Suatu sistem pengolahan makanan melakukan pemrosesan dari material mentah sampai
#' pengemasan (produk siap pasar). Material yang datang bisa langsung masuk antrian
#' mesinA, apabila antrian di mesinA ≤ 5, dengan lama waktu pemrosesannya diset
#' sama dengan 6 menit. Jika antrian di mesinA > 5 tetapi < 9 dan antrian di mesinB <= 2,
#' maka lamanya pemrosesan di mesinA adalah sama dengan 8 menit. Apabila salah satu dari
#' kedua kondisi di atas tidak terpenuhi, material dikembalikan (tolak) .
#' Material yang telah selesai diproses oleh mesinA, 70% diantaranya berhasil menjadi
#' produk yang siap disitribusikan ke pasar. Sedangkan 30% harus masuk ke antrian
#' mesinB untuk pengolahan lebih lanjut, dengan lama proses menyebar \strong{Exponensial
#' (rate=0.1 per menit)}. Waktu antar kedatangan material adalah 5 menit, jumlah
#' mesinA dan mesin B yang tersedia adalah masing-masing 1 unit. Simulasi
#' dijalankan sampai 300 menit.
#'
#' @param L_mesinB adalah berbentuk fungsi yang menentukan lamanya waktu proses material di mesinB
#' @param ak adalah berbentuk fungsi yang memberikan nilai waktu antar kedatangan material
#' @param chek adalah nilai numerik lebih besar dari 0 dan kurang dari 1 yang merupakan peluang material masuk ke mesinB, nilai terpasangnya asalah 0.30
#' @param rsc adalah vektor numerik yang memberikan kapasitas untuk mesinA dan mesinB, nilai terpasangnya adalah c(1,1)
#' @param rn adalah sebuah nilai numerik yang memberikan rentang waktu simulasi, dengan nilai terpasang 300
#' @param ongoing sebuah nilai logical, yang mana nilai TRUE, berarti mencatat kedatangan entiti yang masih sedang diproses di sistem.  Hal ini ditunjukkan oleh nilai kolom finished FALSE
#'
#' @return \strong{dataset} Arrivals, Resources, dan Attributes
#' @export
#'
#' @examples
#' brun_branch_02(function() rexp(1, 0.1), function() 5)

brun_branch_02 <- function(L_mesinB, ak, chek = 0.30, rsc=c(1, 1), rn=300, ongoing=FALSE)
 {
  if (!is.function(L_mesinB) || !is.function(ak)) {
    stop("L_mesinB dan ak harus berupa fungsi numeric")
  }

  env <- simmer("percabangan2")

  Chek_01 <- function() ifelse(get_queue_count(env, "mesinA") <= 5,
                               0, ifelse(get_queue_count(env, "mesinA") <= 9 &&
                                           get_queue_count(env, "mesinB") <= 2, 1, 2))
  Chek_02 <- function() runif(1) < chek

  lintasB <- trajectory()

  lintasA <- trajectory() %>%
    # log_("material tiba di mesinA ...") %>%
    seize("mesinA") %>%
    timeout_from_attribute("lamaA") %>%
    release("mesinA") %>%
    # log_("selesai di mesinA ..") %>%
    branch(
      Chek_02, continue=FALSE,
      trajectory() %>%
        join(lintasB)) %>%
    set_global("pack", 1, mod="+")

  lintasB <- trajectory() %>%
    # log_("material tiba di mesinB ...") %>%
    seize("mesinB") %>%
    timeout(L_mesinB) %>%
    release("mesinB") %>%
    # log_("selesai di mesinB ..") %>%
    join(lintasA)

  lintas <- trajectory() %>%
    branch(Chek_01, continue=c(FALSE, FALSE),
           trajectory() %>%
             set_attribute("lamaA", 8) %>%
             join(lintasA),
           trajectory() %>%
             set_global("tolak", 1, mod="+")
    ) %>%
    set_attribute("lamaA", 6) %>%
    join(lintasA)

  env %>%
    add_resource("mesinA", rsc[1]) %>%
    add_resource("mesinB", rsc[1]) %>%
    add_generator("material", lintas, ak, mon=2) %>%
    run(rn) %>% invisible()

  out1 <- as_tibble(get_mon_arrivals(env))
  out2 <- as_tibble(get_mon_attributes(env))
  out3 <- as_tibble(get_mon_resources(env))
  out <- list(Arrivals = out1, Resources = out3, Attributes = out2)
  return(out)
}

# brun_branch_02(function() rexp(1, 0.1), function() 5)

