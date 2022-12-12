#' Simulasi Melingkar (Sirkular) : Kasus Perusahaan Pertambangan
#'
#' Sebuah perusahaan tambang mengoperasikan truk, untuk mengakut bijih tambang
#' dari tiga tempat penambangan ("shovels") ke sebuah "crusher". Ada dua tipe truk
#' berbeda yang dioperasikan, yaitu truk 20 ton dan truk 50 ton. Ukuran truk ini
#' mempengaruhi lamanya waktu yang diperlukan untuk loading di "shovel", waktu
#' tempuh ke "crusher", lamanya waktu bongkar muat di "crusher" dan perjalanan kembali
#' ke "shovel". Ukuran truk juga mempengaruhi destinasi shovelnya, dari tiga "shovel"
#' yang tersedia, dua "shovel" diperuntukkan bagi truk 20 ton, dan satu "shovel" untuk truk
#' 50 ton. Disipilin antrian di "shovel" adalah FIFO. Sedangkan antrian di "crusher" berdasarkan
#' ukuran truk, prioritas diberikan pada truk dengan ukuran besar. Operasi tambang ini
#' menggunakan 6 truk berukuran 20 ton dan 3 truk berukuran 50 ton. Nilai-nilai input
#' untuk truk 20 ton adalah sebagai berikut: a) loading time adalah \strong{Exponesial} dengan
#' \strong{rate 1/5 per menit}, b) waktu tempuh dari "shovel" ke "crusher" adalah 150 menit,
#' c) lamanya proses bongkar di "crusher" adalah \strong{Exponesial(1/2)}, d) waktu tempuh
#' kembali ke "shovel" adalah 90 menit. Sedangkan nilai-nilai input untuk truk 50 ton
#' adalah sebagai berikut: a) loading time adalah \strong{Exponesial(1/10)}, b) waktu tempuh
#' dari "shovel" ke "crusher" adalah 180 menit, c) lamanya proses bongkar di "crusher"
#' adalah \strong{Exponesial(1/4)}, d) waktu tempuh kembali ke "shovel" adalah 120 menit.
#' hitung total bijih yang diangkut dalam 20 jam (1200 menit) kerja tambang. Dimana kapasitas muat
#' dan realisasi muat, untuk Truk 20 ton adalah 2 ton dan 2 + \strong{Normal(0, 0.05)}.  Sedangkan
#' untuk Truk 50 ton kapasitasnya adalah 5 ton, sedangkan realisasinya adalah 5 + \strong{Normal(0, 0.1)}
#'
#' @param MUAT20 sebuah fungsi yang memberikan lamanya proses muat Truk 20 ton di shovel
#' @param MUAT50 sebuah fungsi yang memberikan lamanya proses muat Truk 50 ton di shovel
#' @param BONGKAR20 sebuah fungsi yang memberikan lamanya proses bongkar Truk 20 ton di crusher
#' @param BONGKAR50 sebuah fungsi yang memberikan lamanya proses bongkar Truk 50 ton di crusher
#' @param TravelSC20 sebuah fungsi yang memberikan lamanya perjalanan Truk 20 ton menuju crusher
#' @param TravelSC50 sebuah fungsi yang memberikan lamanya perjalanan Truk 50 ton menuju crusher
#' @param TravelCS20 sebuah fungsi yang memberikan lamanya perjalanan Truk 20 ton menuju shovel
#' @param TravelCS50 sebuah fungsi yang memberikan lamanya perjalanan Truk 50 ton menuju shovel
#' @param Beban20 sebuah fungsi yang memberikan realisasi Beban muat Truk 20 ton
#' @param Beban50 sebuah fungsi yang memberikan realisasi Beban muat Truk 50 ton
#' @param rsc_shovel sebuah vektor numerik yang meberikan kapasitas shovel50 dan shovel20, nilai terpasangnya c(1,2)
#' @param rsc_crusher sebuah nilai numerik yang memberikan kapasitas crusher, nilai terpasangnya 1
#' @param rn sebuah nilai numerik yang merupakan rentang waktu simulasi
#' @param ongoing sebuah nilai logical, yang mana nilai TRUE, berarti mencatat kedatangan entiti yang masih sedang diproses di sistem.  Hal ini ditunjukkan oleh nilai kolom finished FALSE
#'
#' @return \strong{dataset} Arrivals dan Attributes
#' @export
#'
#' @examples
#' brun_tambang(MUAT20 = function() rexp(1, 1/5), MUAT50 = function() rexp(1, 1/10),
#'              BONGKAR20 = function() rexp(1, 1/2), BONGKAR50 = function() rexp(1, 1/4),
#'              TravelSC20 = function() 150, TravelSC50 = function() 180, TravelCS20 = function() 90,
#'              TravelCS50 = function() 120, Beban20 = function() round(2 + rnorm(1, 0, 0.05), 2),
#'              Beban50 = function() round(5 + rnorm(1, 0, 0.1), 2), rsc_shovel=c(1, 2),
#'              rsc_crusher = 1, rn=1200, ongoing=FALSE)

brun_tambang <- function(MUAT20, MUAT50, BONGKAR20, BONGKAR50, TravelSC20, TravelSC50,
                             TravelCS20, TravelCS50, Beban20, Beban50, rsc_shovel=c(1, 2),
                             rsc_crusher = 1, rn=1200, ongoing=FALSE)
{
  tambang <- simmer("tambang")

  Shovel50 <- trajectory() %>%
    # log_("sampai di shovel ..") %>%
    seize("SHOVEL_50") %>%
    timeout(MUAT50) %>%
    release("SHOVEL_50") %>%
    set_attribute("Beban", Beban50) %>% # menambah muatan Tru50
    # log_("keluar shovel ..") %>%
    # log_("perjalanan menuju crusher ..") %>%
    set_attribute("BONGKAR", BONGKAR50) %>%
    set_attribute("TRAVEL_CS", TravelCS50) %>%
    timeout(TravelSC50)

  Shovel20 <- trajectory() %>%
    # log_("sampai di shovel ..") %>%
    seize("SHOVEL_20") %>%
    timeout(MUAT20) %>%
    release("SHOVEL_20") %>%
    set_attribute("Beban", Beban20) %>% # menambah muatan Truk20
    # log_("keluar shovel ..") %>%
    # log_("perjalanan menuju crusher ..") %>%
    set_attribute("BONGKAR", BONGKAR20) %>%
    set_attribute("TRAVEL_CS", TravelCS20) %>%
    timeout(TravelSC20)

  Crusher <- trajectory() %>%
    # log_("sampai di crusher ..") %>%
    seize("CRUSHER") %>%
    timeout_from_attribute("BONGKAR") %>%
    release("CRUSHER") %>%
    set_global("Total", function() get_global(tambang, "Total")+
                 get_attribute(tambang, "Beban")) %>% # Total bijih yang diangkut
    set_attribute("Beban", 0) %>% # Beban muatan habis
    # log_("keluar cruser ..") %>%
    # log_("perjalanan menuju shovel ..") %>%
    timeout_from_attribute("TRAVEL_CS")

  traj00 <- trajectory() %>%
    set_global("Total", 0) # Setting awal variabel global Total = 0

  traj50A <- trajectory() %>%
    join(Shovel50, Crusher) %>%
    activate("Truk50A")

  traj50B <- trajectory() %>%
    join(Shovel50, Crusher) %>%
    activate("Truk50B")

  traj50C <- trajectory() %>%
    join(Shovel50, Crusher) %>%
    activate("Truk50C")

  traj20A <- trajectory() %>%
    join(Shovel20, Crusher) %>%
    activate("Truk20A")

  traj20B <- trajectory() %>%
    join(Shovel20, Crusher) %>%
    activate("Truk20B")

  traj20C <- trajectory() %>%
    join(Shovel20, Crusher) %>%
    activate("Truk20C")

  traj20D <- trajectory() %>%
    join(Shovel20, Crusher) %>%
    activate("Truk20D")

  traj20E <- trajectory() %>%
    join(Shovel20, Crusher) %>%
    activate("Truk20E")

  traj20F <- trajectory() %>%
    join(Shovel20, Crusher) %>%
    activate("Truk20F")

  tambang %>%
    add_resource("SHOVEL_50", rsc_shovel[1]) %>%
    add_resource("SHOVEL_20", rsc_shovel[2]) %>%
    add_resource("CRUSHER", rsc_crusher) %>%
    add_generator("dummy", traj00, at(0), mon=2) %>% # mengenarate variabel global Total=0
    add_generator("Truk50A", traj50A, at(0), mon=2,
                  priority = 1) %>%
    add_generator("Truk50B", traj50B, at(0), mon=2,
                  priority = 1) %>%
    add_generator("Truk50C", traj50C, at(0), mon=2,
                  priority = 1) %>%
    add_generator("Truk20A", traj20A, at(0), mon=2) %>%
    add_generator("Truk20B", traj20B, at(0), mon=2) %>%
    add_generator("Truk20C", traj20C, at(0), mon=2) %>%
    add_generator("Truk20D", traj20D, at(0), mon=2) %>%
    add_generator("Truk20E", traj20E, at(0), mon=2) %>%
    add_generator("Truk20F", traj20F, at(0), mon=2) %>%
    run(rn) %>% invisible()

  out1 <- as_tibble(get_mon_arrivals(tambang, ongoing))
  out2 <- as_tibble(get_mon_attributes(tambang))
  out <- list(Arrivals = out1, Attributes = out2)
  return(out)
}


# brun_tambang(MUAT20 = function() rexp(1, 1/5), MUAT50 = function() rexp(1, 1/10),
#              BONGKAR20 = function()rexp(1, 1/2), BONGKAR50 = function()rexp(1, 1/4),
#              TravelSC20 = function() 150, TravelSC50 = function() 180, TravelCS20 = function() 90,
#              TravelCS50 = function() 120, Beban20 = function() round(2 + rnorm(1, 0, 0.05), 2),
#              Beban50 = function() round(5 + rnorm(1, 0, 0.1), 2), rsc_shovel=c(1, 2),
#              rsc_crusher = 1, rn=1200, ongoing=FALSE)




