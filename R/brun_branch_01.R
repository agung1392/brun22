#' Simulasi Sistem Kejadian Diskret dengan Percabangan (kasus 1)
#'
#' Sebuah proses manufaktur dilakukan dalam dua tahap, pertama-tama material
#' diperiksa oleh seorang operator, jika material yang diperiksa kondisinya baik
#' maka akan diteruskan untuk diproses dengan mesinA, tetapi bila material kondisinya rusak,
#' maka akan diteruskan ke mesin B untuk dilakukan koreksi, peluang sebuah material itu
#' rusak adalah 20%. Setelah selesai dari mesinB, material kembali diperiksa oleh
#' operator, bila material berhasil dikoreksi, maka akan diteruskan ke mesinA. Peluang
#' keberhasilan proses koreksi oleh mesinB adalah 90%, material yang tidak berhasil
#' dikoreksi akan dibuang sebagai limbah \emph{scrap}. Lamanya waktu pemeriksaan oleh
#' operator menyebar \strong{Triangular(min=0.2, max=0.8, mode=0.4)}. Lamanya waktu
#' proses oleh mesinA adalah \strong{Uniform(1.5, 2.0)}, sedangkan lamanya waktu
#' pengkoreksian oleh mesinB adalah \strong{Uniform(2.0, 3.0)}. Pabrik tersebut menyediakan
#' dua orang operator, satu unit mesinA dan satu unit mesinB. Material tiba untuk
#' diperoses setiap 2 menit.
#'
#' @param L_opr adalah berbentuk fungsi yang menentukan lamanya waktu proses material di operator
#' @param L_mesinA adalah berbentuk fungsi yang menentukan lamanya waktu proses material di mesinA
#' @param L_mesinB adalah berbentuk fungsi yang menentukan lamanya waktu proses material di mesinB
#' @param chek sebuah vektor numerik yang nilai-nilainya antara 0 dan 1, yang memberikan besarnya peluang pada percabangan di lintasan
#' @param rsc adalah sebuah vektor numerik untuk memberikan nilai besarnya kapasitas sumber-sumber daya
#' @param rn adalah sebuah nilai numerik yang meberikan rentang waktu simulasi, nilai terpasangnya adalah 70
#' @param ongoing sebuah nilai logical, yang mana nilai TRUE, berarti mencatat kedatangan entiti yang masih dalam proses di sistem
#'
#' @return \strong{dataset} Arrival dan Resources
#' @export
#'
#' @examples
#' brun_branch_01(function() rtri(1, 0.2, 0.8, 0.4), function() runif(1, 1.5, 2.0) ,
#'                function() runif(1, 2.0, 3.0))

brun_branch_01 <- function(L_opr, L_mesinA, L_mesinB, chek = c(0.2, 0.1), rsc=c(2, 1, 1),
                           rn=70, ongoing=FALSE)
  {

  if (!is.function(L_opr) || !is.function(L_mesinA) || !is.function(L_mesinB)) {
    stop("L_opr atau L_mesinA atau L_mesin B harus berupa fungsi numeric")
  }
  env <- simmer("percabangan")

  Chek_01 <- function() runif(1) < chek[1]
  Chek_02 <- function() runif(1) < chek[2]

  opr <- trajectory() %>%
    # log_("material tiba ...") %>%
    seize("operator") %>%
    timeout(L_opr) %>%
    release("operator")
    # log_("selesai diperiksa opr ..")

  lintasA <- trajectory() %>%
    # log_("material tiba di mesinA ...") %>%
    seize("mesinA") %>%
    timeout(L_mesinA) %>%
    release("mesinA")
    # log_("selesai di mesinA ..")

  lintasB <- trajectory() %>%
    # log_("material tiba di mesinB ...") %>%
    seize("mesinB") %>%
    timeout(L_mesinB) %>%
    release("mesinB")
    # log_("selesai di mesinB ..")

  lintas <- trajectory() %>%
    # log_("material tiba ...") %>%
    seize("operator") %>%
    timeout(L_opr) %>%
    release("operator") %>%
    # log_("selesai diperiksa 1 ..") %>%
    branch(
      Chek_01, continue = TRUE,
      join(lintasB) %>%
        join(opr) %>%
        branch(
          Chek_02, continue=FALSE,
          trajectory()
            # log_("material dibuang ..")
        )
    ) %>%
    join(lintasA)

  env %>%
    add_resource("operator", rsc[1]) %>%
    add_resource("mesinA", rsc[2]) %>%
    add_resource("mesinB", rsc[3]) %>%
    add_generator("material", lintas, function() 2) %>%
    run(rn) %>% invisible()

  output.dat1 <- as_tibble(get_mon_arrivals(env, ongoing = FALSE))
  out1 <- subset(output.dat1, start_time>0)
  output.dat2 <- as_tibble(get_mon_resources(env))
  out <- list(Arrivals = out1, Resources = output.dat2)

  return(out)
 }

# brun_branch_01(function() rtri(1, 0.2, 0.8, 0.4), function() runif(1, 1.5, 2.0) ,
#                function() runif(1, 2.0, 3.0))
