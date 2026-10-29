# veriye_bakis.R: Bir veri setine ilk bakış için beş sayfalık rapor (A4, PDF).
# Fonksiyonlar başka bir veriyle de aynı biçimde kullanılabilir:
#   source("veriye_bakis.R")
#   veriye_bakis(veri_adi, grup = sonuc_degiskeni)                    # beş sayfa tek PDF'te
#   veriye_bakis(veri_adi, grup = sonuc_degiskeni, birlestir = FALSE) # beş ayrı PDF
#   veriye_bakis()                                                    # seçenekler ve örnekler
#
# Sayfalar ve fonksiyonlar (her biri tek başına da çağrılabilir):
#   1. veri_karti()          Veri kimlik kartı: her değişkenin türü, eksiği, kategorilerin yüzdesi,
#                            sayısal değişkenlerin ortalama ± SS ve ortanca (Q1–Q3) değeri
#   2. kategorik_sayfasi()   Kategorik değişkenler: kategori payları, her kategorinin yüzdesi ve n
#   3. normallik_sayfasi()   Sayısal değişkenler: tanımlayıcılar, dört grafik ve dört normallik testi
#   4. grup_sayfasi()        Sonuca göre profil: her kategoride sonucun yüzdesi, %95 GA, p değeri ve
#                            ikili karşılaştırma
#   5. grup_sayisal_sayfasi() Sonuca göre sayısal değişkenler: grupların dağılımı, tanımlayıcıları
#                            ve testleri
# Gereken paketler: ggplot2, patchwork, nortest.
# Değişken etiketleri (attr(x, "label")) varsa sayfalarda değişken adı yerine etiket yazılır.

# ---- Ortak yardımcılar ------------------------------------------------

# Türkçe sayı yazımı: ondalık ayracı virgül
vb_sayi <- function(x, basamak = 1) formatC(x, format = "f", digits = basamak, decimal.mark = ",")
vb_yuzde <- function(x, basamak = 1) paste0("%", vb_sayi(x, basamak))

# p değeri: p<0,001 ya da p=0,034
vb_p <- function(p) ifelse(is.na(p), "", ifelse(p < 0.001, "p<0,001", paste0("p=", vb_sayi(p, 3))))

# Değişkenin etiketi; etiket yoksa adı
vb_etiket <- function(veri, ad) {
  e <- attr(veri[[ad]], "label")
  if (is.null(e)) ad else e
}

# Kategorik sayılan sütunlar: faktör ve metin; farklı değer sayısı sınırı aşanlar
# (kimlik numarası gibi) dışarıda kalır
vb_kategorikler <- function(veri, en_cok = 15) {
  aday <- names(veri)[vapply(veri, function(x) is.factor(x) || is.character(x), logical(1))]
  aday[vapply(aday, function(ad) length(unique(stats::na.omit(veri[[ad]]))) <= en_cok, logical(1))]
}

# Sayısal sütunlar; her değeri farklı tam sayılar (kimlik numarası gibi) dışarıda kalır
vb_sayisallar <- function(veri) {
  aday <- names(veri)[vapply(veri, is.numeric, logical(1))]
  kimlik <- vapply(aday, function(ad) {
    x <- stats::na.omit(veri[[ad]])
    length(x) > 15 && all(x == round(x)) && !anyDuplicated(x)
  }, logical(1))
  aday[!kimlik]
}

# Sütun adı tırnaklı ("dusme_12ay"), tırnaksız (dusme_12ay) ya da bir değişkende
# saklı olarak verilebilir
vb_sutun_adi <- function(ifade, veri, ortam) {
  if (is.null(ifade)) return(NULL)
  if (is.character(ifade)) return(ifade)
  if (is.name(ifade) && deparse(ifade) %in% names(veri)) return(deparse(ifade))
  eval(ifade, ortam)
}

# Kategori sırası: faktörde düzey sırası, metinde alfabetik sıra
vb_duzeyler <- function(x) if (is.factor(x)) levels(x) else sort(unique(stats::na.omit(x)))

# Kategori renkleri: açık maviden koyu maviye
vb_renkler <- function(k) if (k == 1) "#2c6e9e" else grDevices::colorRampPalette(c("#a6cbe3", "#1f4e79"))(k)

# Verideki ondalık basamak sayısı (en çok 3): 74 → 0, 27,4 → 1, 0,85 → 2
vb_basamak <- function(x) {
  x <- x[is.finite(x)]
  for (b in 0:2) if (all(abs(x * 10^b - round(x * 10^b)) < 1e-6)) return(b)
  3
}

# Sayısal özet. Ortalama ve SS en az bir ondalıkla, ortanca ve çeyrekler verinin kendi
# ondalığıyla yazılır (tam sayılı veride çeyrek 22,5 gibi çıkarsa bir ondalıkla).
# Çeyrekler gtsummary ile aynı yöntemle (quantile type = 2) hesaplanır; Tablo 1 ile aynıdır.
vb_ozet <- function(x, b = vb_basamak(x)) {
  x <- x[!is.na(x)]
  q <- stats::quantile(x, c(0.25, 0.5, 0.75), type = 2, names = FALSE)
  bq <- if (b == 0) min(1, vb_basamak(q)) else b
  list(ortalama_ss = paste0(vb_sayi(mean(x), max(1, b)), " ± ", vb_sayi(stats::sd(x), max(1, b))),
       ortanca_ceyrek = paste0(vb_sayi(q[2], bq), " (", vb_sayi(q[1], bq), "–", vb_sayi(q[3], bq), ")"),
       aralik = paste0(vb_sayi(min(x), b), "–", vb_sayi(max(x), b)))
}

vb_tema <- function(boyut = 8) {
  ggplot2::theme_minimal(base_size = boyut) +
    ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                   strip.text = ggplot2::element_text(face = "bold", hjust = 0),
                   plot.title = ggplot2::element_text(face = "bold", size = boyut + 1))
}

vb_sayfa_temasi <- ggplot2::theme(plot.title.position = "plot", plot.caption.position = "plot",
                                  plot.title = ggplot2::element_text(face = "bold", size = 12),
                                  plot.subtitle = ggplot2::element_text(size = 8),
                                  plot.caption = ggplot2::element_text(size = 6.5, hjust = 0, lineheight = 1.1))

vb_kaydet <- function(sayfa, dosya) {
  if (!is.null(dosya)) {
    # cairo_pdf aygıtı Türkçe harfleri (ı, ş, ğ) PDF'te doğru basar
    ggplot2::ggsave(dosya, sayfa, width = 21, height = 29.7, units = "cm", device = cairo_pdf)
    message("Kaydedildi: ", dosya)
  }
  print(sayfa)
}


# ---- 1. Veri kimlik kartı ---------------------------------------------

# Her değişken için türü, dolu ve eksik sayısını, farklı değer sayısını ve özetini tek
# tabloda verir (ggplot2 ile çizilir). Kategorik değişkende kategorilerin yüzdesi, sayısal
# değişkende aralık, ortalama ± SS ve ortanca (Q1–Q3) yazılır; son sütundaki küçük grafik
# dağılımı gösterir. Değişkenler türlerine göre gruplanır; Sıra sütunu verideki yeri verir.
# Sayfanın üstünde satır, sütun, eksik hücre ve tekrar eden satır sayısı yer alır.
# Eksiği %10 ve üstü olan değişkenin eksik sayısı kırmızıdır.
veri_karti <- function(veri, dosya = NULL, baslik = "Veri kimlik kartı") {
  n <- nrow(veri)
  tur_adi <- function(x) {
    if (is.factor(x)) "kategorik" else if (is.character(x)) "metin" else
      if (inherits(x, c("Date", "POSIXt"))) "tarih" else if (is.logical(x)) "mantıksal" else
        if (is.numeric(x) && all(x == round(x), na.rm = TRUE)) "tam sayı" else
          if (is.numeric(x)) "sayısal" else class(x)[1]
  }
  kart <- do.call(rbind, lapply(names(veri), function(ad) {
    x <- veri[[ad]]
    dolu <- sum(!is.na(x))
    eksik <- n - dolu
    farkli <- length(unique(stats::na.omit(x)))
    aralik <- ortalama_ss <- ortanca <- ""
    if (is.numeric(x) && dolu > 0) {
      o <- vb_ozet(x)
      aralik <- o$aralik
      ortalama_ss <- o$ortalama_ss
      ortanca <- o$ortanca_ceyrek
    } else if (inherits(x, c("Date", "POSIXt")) && dolu > 0) {
      aralik <- paste0(format(min(x, na.rm = TRUE)), " – ", format(max(x, na.rm = TRUE)))
    } else if (farkli == dolu && dolu > 15) {
      aralik <- paste0("her değer farklı (kimlik): ",
                       paste(utils::head(vb_duzeyler(x), 3), collapse = ", "), ", …")
    } else if (dolu > 0) {
      tab <- table(factor(x, levels = vb_duzeyler(x)))
      parca <- paste(names(tab), vb_yuzde(100 * as.vector(tab) / n))
      if (length(parca) > 8) parca <- c(parca[1:7], paste0("… (", length(parca) - 7, " kategori daha)"))
      aralik <- paste(parca, collapse = "; ")
    }
    data.frame(Sıra = match(ad, names(veri)), Değişken = ad, Etiket = vb_etiket(veri, ad),
               Tür = tur_adi(x), Dolu = dolu,
               Eksik = paste0(eksik, " (", vb_yuzde(100 * eksik / n), ")"),
               `Farklı değer` = farkli, `Aralık / kategoriler (%)` = aralik,
               `Ortalama ± SS` = ortalama_ss, `Ortanca (Q1–Q3)` = ortanca, check.names = FALSE)
  }))
  # Aynı türdeki değişkenler bir arada; tür içinde verideki sıra korunur
  tur_sirasi <- c("metin", "tarih", "kategorik", "mantıksal", "tam sayı", "sayısal")
  kart <- kart[order(match(kart$Tür, tur_sirasi, nomatch = 99), kart$Sıra), ]
  rownames(kart) <- NULL

  turler <- table(factor(kart$Tür, levels = unique(kart$Tür)))
  ozet_satiri <- paste0(
    n, " satır · ", ncol(veri), " değişken (",
    paste(paste(turler, names(turler)), collapse = ", "), ") · ",
    "eksik hücre ", sum(is.na(veri)), " (", vb_yuzde(100 * mean(is.na(veri))), ") · ",
    "eksiksiz satır ", sum(stats::complete.cases(veri)), " · ",
    "birebir tekrar eden satır ", sum(duplicated(veri)))

  # Tablo ggplot2 ile çizilir; konum ve genişlikler inç cinsindendir. Uzun metin hücre
  # içinde alt satıra kayar. Sayısal olmayan değişkenlerde kategoriler 8.–10. sütunlara yayılır.
  genislik <- c(0.26, 0.88, 1.3, 0.66, 0.36, 0.56, 0.42, 0.75, 0.85, 1.05, 0.86)
  sol <- cumsum(c(0, genislik))[seq_along(genislik)]
  toplam_en <- sum(genislik)
  ortali <- c(TRUE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
  harf <- 0.046    # 7 punto yazıda bir harfin yaklaşık genişliği
  satir_ara <- 0.125
  sayisal_degil <- kart$`Ortalama ± SS` == ""
  cok_eksik <- 1 - kart$Dolu / n >= 0.10

  hucre <- sapply(kart, as.character)
  if (!is.matrix(hucre)) hucre <- matrix(hucre, nrow = 1)
  for (j in seq_len(ncol(hucre))) {
    en <- if (j == 8) ifelse(sayisal_degil, sum(genislik[8:10]), genislik[8]) else rep(genislik[j], nrow(hucre))
    if (j == 4) en <- en - 0.14
    hucre[, j] <- mapply(function(m, e) paste(strwrap(m, width = max(4, floor((e - 0.08) / harf))), collapse = "\n"),
                         hucre[, j], en)
  }
  satir_sayisi <- apply(hucre, 1, function(r) max(lengths(strsplit(r, "\n")), 1))
  yukseklik <- pmax(0.25, 0.06 + satir_ara * satir_sayisi)
  baslik_boyu <- 0.34
  ust <- -baslik_boyu - c(0, cumsum(yukseklik))[seq_along(yukseklik)]
  toplam_boy <- baslik_boyu + sum(yukseklik)

  yazilar <- do.call(rbind, lapply(seq_len(ncol(hucre)), function(j) {
    data.frame(x = if (ortali[j]) sol[j] + genislik[j] / 2 else sol[j] + if (j == 4) 0.16 else 0.04,
               y = ust - 0.035, etiket = hucre[, j], hjust = if (ortali[j]) 0.5 else 0,
               renk = if (j == 6) ifelse(cok_eksik, "#b2182b", "grey10") else "grey10",
               yuz = if (j == 6) ifelse(cok_eksik, "bold", "plain") else "plain")
  }))
  tur_renk <- c(kategorik = "#2166ac", metin = "#8c8c8c", tarih = "#7b3294", sayısal = "#1b7837",
                `tam sayı` = "#66bd63", mantıksal = "#b35806")
  renk_bul <- function(tur) unname(ifelse(is.na(tur_renk[tur]), "#8c8c8c", tur_renk[tur]))
  noktalar <- data.frame(x = sol[4] + 0.08, y = ust - 0.035 - 0.045, renk = renk_bul(kart$Tür))
  basliklar <- data.frame(x = ifelse(ortali, sol + genislik / 2, sol + 0.04), y = -baslik_boyu + 0.05,
                          etiket = c("Sıra", "Değişken", "Etiket", "Tür", "Dolu", "Eksik", "Farklı\ndeğer",
                                     "Aralık /\nkategoriler (%)", "Ortalama ± SS", "Ortanca (Q1–Q3)", "Dağılım"),
                          hjust = ifelse(ortali, 0.5, 0))
  zebra <- data.frame(ust = ust, alt = ust - yukseklik)[seq_len(nrow(kart)) %% 2 == 0, ]
  # Tür grupları arasında ince çizgi
  ayrac <- (ust - yukseklik)[which(kart$Tür[-1] != kart$Tür[-nrow(kart)])]

  # Son sütun: küçük grafikler. Sayısal ve tarih değişkende histogram, kategorik
  # değişkende kategori başına çubuk (eksik gri). Yükseklikler en uzun çubuğa göredir.
  kucuk <- do.call(rbind, lapply(seq_len(nrow(kart)), function(i) {
    x <- veri[[kart$Değişken[i]]]
    x0 <- sol[11] + 0.06
    en <- genislik[11] - 0.12
    y0 <- ust[i] - yukseklik[i] + 0.045
    boy <- yukseklik[i] - 0.09
    if (is.numeric(x) || inherits(x, c("Date", "POSIXt"))) {
      x <- as.numeric(stats::na.omit(x))
      if (length(x) == 0) return(NULL)
      sayim <- if (all(x == round(x)) && diff(range(x)) <= 40) {
        as.vector(table(factor(x, levels = seq(min(x), max(x)))))
      } else if (diff(range(x)) == 0) {
        length(x)
      } else {
        graphics::hist(x, breaks = seq(min(x), max(x), length.out = 25), plot = FALSE)$counts
      }
      renk <- renk_bul(kart$Tür[i])
      aralik_payi <- 0.12
    } else {
      duzey <- vb_duzeyler(x)
      if (length(duzey) > 15) return(NULL)
      eksik <- sum(is.na(x))
      sayim <- c(as.vector(table(factor(x, levels = duzey))), if (eksik > 0) eksik)
      renk <- c(vb_renkler(length(duzey)), if (eksik > 0) "grey75")
      aralik_payi <- 0.3
    }
    k <- length(sayim)
    w <- min(en / k, 0.14)
    data.frame(xmin = x0 + (seq_len(k) - 1) * w, xmax = x0 + seq_len(k) * w - w * aralik_payi,
               ymin = y0, ymax = y0 + boy * sayim / max(sayim), renk = renk)
  }))
  yazi_boyu <- 7 / ggplot2::.pt

  g_tablo <- ggplot2::ggplot() +
    ggplot2::geom_rect(data = zebra, ggplot2::aes(xmin = 0, xmax = toplam_en, ymin = alt, ymax = ust),
                       fill = "#f2f5f8") +
    ggplot2::annotate("segment", x = 0, xend = toplam_en, y = c(0, -baslik_boyu, -toplam_boy),
                      yend = c(0, -baslik_boyu, -toplam_boy), linewidth = c(0.7, 0.4, 0.7), colour = "grey20") +
    ggplot2::geom_text(data = basliklar, ggplot2::aes(x = x, y = y, label = etiket, hjust = hjust),
                       vjust = 0, size = yazi_boyu, fontface = "bold", lineheight = 0.9) +
    ggplot2::geom_text(data = yazilar, ggplot2::aes(x = x, y = y, label = etiket, hjust = hjust,
                                                    colour = renk, fontface = yuz),
                       vjust = 1, size = yazi_boyu, lineheight = 0.95) +
    ggplot2::geom_point(data = noktalar, ggplot2::aes(x = x, y = y, colour = renk), size = 1.4) +
    ggplot2::geom_rect(data = kucuk, ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                                                  fill = renk)) +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_x_continuous(limits = c(0, toplam_en), expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(-toplam_boy - 0.01, 0.01), expand = c(0, 0)) +
    ggplot2::theme_void()
  if (length(ayrac) > 0)
    g_tablo <- g_tablo + ggplot2::annotate("segment", x = 0, xend = toplam_en, y = ayrac, yend = ayrac,
                                           linewidth = 0.3, colour = "grey55")

  sayfa <- patchwork::wrap_plots(g_tablo, patchwork::plot_spacer(), ncol = 1,
                                 heights = grid::unit(c(toplam_boy, 1), c("in", "null"))) +
    patchwork::plot_annotation(
      title = baslik, subtitle = ozet_satiri,
      caption = if (any(cok_eksik)) "Kırmızı: eksiği %10 ve üstü olan değişken.",
      theme = vb_sayfa_temasi)
  vb_kaydet(sayfa, dosya)
  kart
}


# ---- 2. Kategorik değişkenler sayfası ---------------------------------

# Üst bölüm: her kategorik değişken tek bir %100 çubuktur; kategorilerin payı yan yana
# görünür. Alt bölüm: her değişken ayrı bir grafiktir; çubuğun üstünde yüzde ve kişi
# sayısı yazar. Bütün çubuklar aynı genişlikte ve aynı %0–100 ölçeğindedir; çok
# kategorili değişkenler (15'e kadar) satırda daha geniş yer alır. Yüzdeler bütün
# satırlara göredir; eksik değerler gri çubukla gösterilir.
kategorik_sayfasi <- function(veri, dosya = NULL, baslik = "Kategorik değişkenler",
                              en_cok_kategori = 15, satir_genisligi = 15) {
  degiskenler <- vb_kategorikler(veri, en_cok_kategori)
  disarida <- setdiff(names(veri)[vapply(veri, function(x) is.factor(x) || is.character(x), logical(1))],
                      degiskenler)
  if (length(disarida) > 0)
    message("Sayfaya alınmadı (", en_cok_kategori, "'ten fazla kategori): ", paste(disarida, collapse = ", "))
  if (length(degiskenler) == 0) {
    message("Veride kategorik değişken yok.")
    return(invisible(NULL))
  }

  n <- nrow(veri)
  sayimlar <- lapply(degiskenler, function(ad) {
    x <- veri[[ad]]
    duzey <- vb_duzeyler(x)
    eksik <- sum(is.na(x))
    d <- data.frame(degisken = ad, etiket = vb_etiket(veri, ad),
                    kategori = c(duzey, if (eksik > 0) "Eksik"),
                    n = c(as.vector(table(factor(x, levels = duzey))), if (eksik > 0) eksik),
                    renk = c(vb_renkler(length(duzey)), if (eksik > 0) "grey75"))
    d$yuzde <- 100 * d$n / n
    d$sira <- seq_len(nrow(d))
    d
  })
  hepsi <- do.call(rbind, sayimlar)
  # Koyu zeminde beyaz, açık zeminde koyu yazı
  parlaklik <- colSums(grDevices::col2rgb(hepsi$renk) * c(0.299, 0.587, 0.114)) / 255
  hepsi$yazi <- ifelse(parlaklik < 0.55, "white", "grey10")

  # Üst bölüm: kategori payları. Dar dilimlere yazı sığmadığı için yazılmaz.
  hepsi$satir <- factor(hepsi$etiket, levels = rev(unique(hepsi$etiket)))
  hepsi$pay_yazisi <- ifelse(hepsi$yuzde >= pmax(5, 0.9 * nchar(hepsi$kategori)),
                             paste0(hepsi$kategori, "\n", vb_yuzde(hepsi$yuzde, 0)), "")
  g_pay <- ggplot2::ggplot(hepsi, ggplot2::aes(x = yuzde, y = satir, group = sira)) +
    ggplot2::geom_col(ggplot2::aes(fill = renk), width = 0.75, colour = "white", linewidth = 0.6,
                      position = ggplot2::position_stack(reverse = TRUE)) +
    ggplot2::geom_text(ggplot2::aes(label = pay_yazisi, colour = yazi),
                       position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE),
                       size = 2.1, lineheight = 0.85) +
    ggplot2::scale_fill_identity() +
    ggplot2::scale_colour_identity() +
    ggplot2::scale_x_continuous(labels = function(b) paste0("%", b), expand = c(0, 0)) +
    ggplot2::labs(title = "Kategori payları", x = NULL, y = NULL) +
    vb_tema() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 7.5, colour = "grey10"))

  # Alt bölüm: her değişken için dikey çubuklar
  tek_grafik <- function(d) {
    d$kategori_f <- factor(d$kategori, levels = d$kategori)
    ggplot2::ggplot(d, ggplot2::aes(x = kategori_f, y = yuzde)) +
      ggplot2::geom_col(ggplot2::aes(fill = renk), width = 0.72) +
      ggplot2::geom_text(ggplot2::aes(y = yuzde + 8.5, label = vb_yuzde(yuzde)), vjust = 0,
                         size = 2.3, fontface = "bold") +
      ggplot2::geom_text(ggplot2::aes(y = yuzde + 2, label = paste0("n=", n)), vjust = 0,
                         size = 1.9, colour = "grey35") +
      ggplot2::scale_fill_identity() +
      ggplot2::scale_x_discrete(labels = scales::label_wrap(11)) +
      ggplot2::scale_y_continuous(limits = c(0, 118), breaks = c(0, 25, 50, 75, 100),
                                  labels = function(b) paste0("%", b), expand = c(0, 0)) +
      ggplot2::labs(title = paste(strwrap(d$etiket[1], width = 9 * (nrow(d) + 1)), collapse = "\n"),
                    x = NULL, y = NULL) +
      vb_tema(7) +
      ggplot2::theme(panel.grid.major.x = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size = 6.3, colour = "grey15", lineheight = 0.9),
                     plot.title = ggplot2::element_text(size = 7.5),
                     plot.background = ggplot2::element_rect(colour = "grey75", linewidth = 0.3, fill = NA),
                     plot.margin = ggplot2::margin(5, 6, 4, 6))
  }
  # Grafikler satırlara dizilir: her grafiğin genişliği kategori sayısı + 1 birimdir; uzun
  # kategori adı (ör. Yükseköğretim) olan grafik biraz daha geniş tutulur
  paneller <- lapply(sayimlar, function(d) {
    en_uzun <- max(nchar(unlist(strsplit(d$kategori, " "))))
    list(grafik = tek_grafik(d), genislik = nrow(d) * max(1, en_uzun / 10) + 1)
  })
  satirlar <- list()
  mevcut <- list()
  dolu <- 0
  for (p in paneller) {
    if (length(mevcut) > 0 && dolu + p$genislik > satir_genisligi) {
      satirlar[[length(satirlar) + 1]] <- mevcut
      mevcut <- list()
      dolu <- 0
    }
    mevcut[[length(mevcut) + 1]] <- p
    dolu <- dolu + p$genislik
  }
  satirlar[[length(satirlar) + 1]] <- mevcut
  satir_grafikleri <- lapply(satirlar, function(s) {
    grafikler <- lapply(s, `[[`, "grafik")
    # Yüzde ölçeği yalnız satırın ilk grafiğinde yazılır
    grafikler[[1]] <- grafikler[[1]] +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 6, colour = "grey50"))
    genislikler <- vapply(s, function(p) p$genislik, numeric(1))
    # Kutular birbirine değmesin: grafikler arasına dar boşluk
    ara <- 0.2
    dizi <- list()
    dizi_en <- numeric(0)
    for (k in seq_along(grafikler)) {
      if (k > 1) {
        dizi <- c(dizi, list(patchwork::plot_spacer()))
        dizi_en <- c(dizi_en, ara)
      }
      dizi <- c(dizi, list(grafikler[[k]]))
      dizi_en <- c(dizi_en, genislikler[k])
    }
    kalan <- satir_genisligi + ara * 3 - sum(dizi_en)
    if (kalan > 0) {
      dizi <- c(dizi, list(patchwork::plot_spacer()))
      dizi_en <- c(dizi_en, kalan)
    }
    # wrap_elements: satır, üstteki pay grafiğinin eksenlerine hizalanmaz
    patchwork::wrap_elements(full = patchwork::wrap_plots(dizi, nrow = 1, widths = dizi_en))
  })
  ara_baslik <- ggplot2::ggplot() + ggplot2::labs(title = "Kategorilerin yüzdesi ve kişi sayısı") +
    ggplot2::theme_void(base_size = 8) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 9))

  # Yükseklikler inç cinsinden; satır az ise grafikler en çok 2,6 inç olur
  ust_yukseklik <- 0.3 * length(degiskenler) + 0.5
  r <- length(satir_grafikleri)
  satir_boyu <- min(2.6, (10.2 - ust_yukseklik - 0.2 - 0.12 * (r - 1)) / r)
  bosluk <- max(0.01, 10.2 - ust_yukseklik - 0.2 - satir_boyu * r - 0.12 * (r - 1))
  # Satırlar arasına dar boşluk
  dikey <- list(g_pay, ara_baslik)
  dikey_boy <- c(ust_yukseklik, 0.2)
  for (k in seq_len(r)) {
    if (k > 1) {
      dikey <- c(dikey, list(patchwork::plot_spacer()))
      dikey_boy <- c(dikey_boy, 0.12)
    }
    dikey <- c(dikey, list(satir_grafikleri[[k]]))
    dikey_boy <- c(dikey_boy, satir_boyu)
  }
  sayfa <- patchwork::wrap_plots(c(dikey, list(patchwork::plot_spacer())),
                                 ncol = 1, heights = c(dikey_boy, bosluk)) +
    patchwork::plot_annotation(
      title = baslik,
      subtitle = paste0("Yüzdeler bütün satırlara (n=", n, ") göredir. Eksik değerler gri çubukla gösterilir."),
      theme = vb_sayfa_temasi)
  vb_kaydet(sayfa, dosya)
  hepsi[, c("degisken", "kategori", "n", "yuzde")]
}


# ---- 3. Sayısal değişkenler ve normallik sayfası ------------------------

# normallik_sayfasi(): Bir veri setindeki bütün sayısal değişkenlerin tanımlayıcıları ve
# normalliği tek bir A4 sayfasında özetlenir. Fonksiyon başka bir veriyle de aynı
# biçimde kullanılabilir:
#   source("veriye_bakis.R")
#   normallik_sayfasi(veri_adi, dosya = "ciktilar/normallik.pdf")
#
# Sayfada her değişken bir satırdır ve beş bölümden oluşur:
#   1. Histogram, yoğunluk eğrisi (KDE, düz kırmızı çizgi) ve aynı ortalama ile standart
#      sapmaya (SS) sahip normal dağılım eğrisi (kesikli çizgi)
#   2. QQ grafiği: noktalar çizgi üzerindeyse dağılım normale yakındır
#   3. Keman (violin) ve kutu (box) grafiği
#   4. Tanımlayıcılar: n, ortalama ± SS, ortanca (Q1–Q3), en küçük–en büyük, çarpıklık ve
#      basıklık (z değerleriyle)
#   5. Testler: Shapiro-Wilk, Kolmogorov-Smirnov, Anderson-Darling ve Lilliefors
# Gereken paketler: ggplot2, patchwork, nortest.
#
# Dört test p≥0,05 ise "normal", dört test p<0,05 ise "normal değil", testler çelişirse
# "grafiğe göre değerlendirin" yazar. Büyük örneklemde testler küçük sapmaları da anlamlı bulabilir;
# son kararı grafikler ve uzman görüşüyle birlikte vermeniz önerilir!


normallik_sayfasi <- function(veri, dosya = NULL,
                              baslik = "Sayısal değişkenler ve normallik değerlendirmesi") {

  sayisal <- vb_sayisallar(veri)

  # Türkçe sayı yazımı: ondalık ayracı virgül, çok küçük p değeri p<0,001
  sayi <- function(x, basamak = 2) formatC(x, format = "f", digits = basamak, decimal.mark = ",")
  p_yaz <- function(p) ifelse(p < 0.001, "<0,001", sayi(p, 3))

  sonuc <- data.frame()
  satirlar <- list()

  for (v in sayisal) {
    x <- veri[[v]][!is.na(veri[[v]])]
    etiket <- attr(veri[[v]], "label")
    if (is.null(etiket)) etiket <- v

    # Testler. Kolmogorov-Smirnov testinde ortalama ve SS veriden alınır; eşit değerlerde
    # R'ın verdiği uyarı gösterilmez.
    sw <- shapiro.test(x)$p.value
    ks <- suppressWarnings(ks.test(x, "pnorm", mean(x), sd(x))$p.value)
    ad <- nortest::ad.test(x)$p.value
    lf <- nortest::lillie.test(x)$p.value
    # Çarpıklık ve basıklık SPSS ile aynı yöntemle hesaplanır (normal dağılımda ikisi de 0).
    # z = değer / standart hata.
    nx <- length(x)
    m2 <- mean((x - mean(x))^2)
    carpiklik <- mean((x - mean(x))^3) / m2^1.5 * sqrt(nx * (nx - 1)) / (nx - 2)
    basiklik <- ((nx + 1) * (mean((x - mean(x))^4) / m2^2 - 3) + 6) * (nx - 1) / ((nx - 2) * (nx - 3))
    se_carpiklik <- sqrt(6 * nx * (nx - 1) / ((nx - 2) * (nx + 1) * (nx + 3)))
    se_basiklik <- 2 * se_carpiklik * sqrt((nx^2 - 1) / ((nx - 3) * (nx + 5)))
    z_carpiklik <- carpiklik / se_carpiklik
    z_basiklik <- basiklik / se_basiklik
    p_hepsi <- c(sw, ks, ad, lf)
    karar <- if (all(p_hepsi >= 0.05)) "normal" else if (all(p_hepsi < 0.05)) "normal değil" else "grafiğe göre değerlendirin"

    # Tanımlayıcılar; çeyrekler Tablo 1 (gtsummary) ile aynı yöntemle hesaplanır
    ceyrek <- quantile(x, c(0.25, 0.5, 0.75), type = 2, names = FALSE)
    ozet <- vb_ozet(x)

    sonuc <- rbind(sonuc, data.frame(degisken = v, n = length(x),
                                     ortalama = round(mean(x), 2), ss = round(sd(x), 2),
                                     ortanca = ceyrek[2], q1 = ceyrek[1], q3 = ceyrek[3],
                                     en_kucuk = min(x), en_buyuk = max(x),
                                     carpiklik = round(carpiklik, 2), carpiklik_z = round(z_carpiklik, 2),
                                     basiklik = round(basiklik, 2), basiklik_z = round(z_basiklik, 2),
                                     shapiro = signif(sw, 2), ks = signif(ks, 2),
                                     anderson = signif(ad, 2), lilliefors = signif(lf, 2),
                                     karar = karar))

    d <- data.frame(x = x)
    tema <- ggplot2::theme_minimal(base_size = 7) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                     plot.title = ggplot2::element_text(face = "bold", size = 8))
    virgul <- function(b) format(b, decimal.mark = ",", trim = TRUE)

    g_hist <- ggplot2::ggplot(d, ggplot2::aes(x)) +
      ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), bins = 20,
                              fill = "#f4a582", colour = "white", linewidth = 0.2) +
      ggplot2::geom_density(colour = "#b2182b", linewidth = 0.5) +
      ggplot2::stat_function(fun = dnorm, args = list(mean = mean(x), sd = sd(x)),
                             linetype = "dashed", linewidth = 0.4) +
      ggplot2::scale_x_continuous(labels = virgul) +
      ggplot2::labs(title = etiket, x = NULL, y = NULL) +
      tema + ggplot2::theme(axis.text.y = ggplot2::element_blank())

    g_qq <- ggplot2::ggplot(d, ggplot2::aes(sample = x)) +
      ggplot2::stat_qq(size = 0.4, alpha = 0.6, colour = "#2166ac") +
      ggplot2::stat_qq_line(colour = "#b2182b", linewidth = 0.4) +
      ggplot2::scale_y_continuous(labels = virgul) +
      ggplot2::labs(title = "QQ", x = NULL, y = NULL) + tema

    g_keman <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = "")) +
      ggplot2::geom_violin(fill = "#f4a582", colour = NA, alpha = 0.7) +
      ggplot2::geom_boxplot(width = 0.18, outlier.size = 0.5, fill = "white") +
      ggplot2::scale_x_continuous(labels = virgul) +
      ggplot2::labs(title = "Keman ve kutu", x = NULL, y = NULL) + tema

    yazi_alani <- function(baslik_, metin_) {
      ggplot2::ggplot() +
        ggplot2::annotate("text", x = 0, y = 0.97, label = metin_, hjust = 0, vjust = 1,
                          size = 2.2, lineheight = 1.05) +
        ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
        ggplot2::labs(title = baslik_) +
        ggplot2::theme_void(base_size = 7) +
        ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 8))
    }

    g_tanim <- yazi_alani("Tanımlayıcılar", paste(
      paste0("n=", length(x)),
      paste0("Ortalama ± SS: ", ozet$ortalama_ss),
      paste0("Ortanca (Q1–Q3): ", ozet$ortanca_ceyrek),
      paste0("En küçük–en büyük: ", ozet$aralik),
      paste0("Çarpıklık: ", sayi(carpiklik), " (z=", sayi(z_carpiklik), ")"),
      paste0("Basıklık: ", sayi(basiklik), " (z=", sayi(z_basiklik), ")"), sep = "\n"))

    p_satir <- function(ad_, p) paste0(ad_, "  p", ifelse(p < 0.001, "", "="), p_yaz(p))
    metin <- paste(p_satir("Shapiro-Wilk", sw), p_satir("Kolmogorov-Smirnov", ks),
                   p_satir("Anderson-Darling", ad), p_satir("Lilliefors", lf), sep = "\n")
    renk <- c("normal" = "#1a7f37", "normal değil" = "#b2182b", "grafiğe göre değerlendirin" = "#b35806")[[karar]]
    g_metin <- yazi_alani("Normallik testleri", metin) +
      ggplot2::annotate("text", x = 0, y = 0.08, label = paste("Testler:", karar), hjust = 0,
                        size = 2.4, fontface = "bold", colour = renk)

    satirlar[[v]] <- patchwork::wrap_plots(g_hist, g_qq, g_keman, g_tanim, g_metin, nrow = 1,
                                           widths = c(1.2, 0.8, 0.8, 1.1, 1.15))
  }

  sayfa <- patchwork::wrap_plots(satirlar, ncol = 1) +
    patchwork::plot_annotation(
      title = baslik,
      subtitle = "Histogram: kırmızı çizgi gerçek verinin yoğunluğu, kesikli çizgi ise aynı ortalama ve SS'li normal dağılan verinin nasıl görüneceğidir.",
      caption = paste0("Testlerde p≥0,05: normal dağılım lehine yorumlanır. Son kararı grafikler ve uzman görüşüyle birlikte vermeniz önerilir.\n",
                       "Çarpıklık ve basıklık normal dağılımda 0'dır; z = değer / standart hata."),
      theme = ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 12),
                             plot.subtitle = ggplot2::element_text(size = 8),
                             plot.caption = ggplot2::element_text(size = 7, hjust = 0)))

  if (!is.null(dosya)) {
    # cairo_pdf aygıtı Türkçe harfleri (ı, ş, ğ) PDF'te doğru basar
    ggplot2::ggsave(dosya, sayfa, width = 21, height = 29.7, units = "cm", device = cairo_pdf)
    message("Kaydedildi: ", dosya)
  }
  print(sayfa)
  sonuc
}


# ---- 4. Sonuca göre profil sayfası --------------------------------------

# grup: sonuç değişkeni (ör. dusme_12ay). Tırnaklı ya da tırnaksız yazılabilir.
# Üst bölüm: her kategorik değişkenin her kategorisinde sonucun yüzdesi ve %95 güven
# aralığı (Wilson skor yöntemi); kesikli çizgi bütün örneklemdeki yüzdedir. Her değişkenin
# satırında ki-kare testinin p değeri yer alır; beklenen sayısı 5'ten küçük hücre varsa
# Fisher kesin testi kullanılır ve p değeri (*) ile işaretlenir. İkiden çok kategorili ve
# p<0,05 olan değişkende kategoriler ikişer karşılaştırılır (Bonferroni düzeltmesi); anlamlı
# çiftler sağda köşeli çizgiyle birleştirilir. Sonuç ikiden çok kategorili ise yüzdeler
# yığılmış çubukla verilir.
# Kategorik bölümün notu blokların hemen altındadır. Sayısal değişkenler 5. sayfadadır
# (grup_sayisal_sayfasi()).
# Sonuç sürekli ise (10'dan çok farklı değerli sayısal değişken) her kategoride sonucun
# ortalaması ve %95 GA verilir; testler iki kategoride t testi ve Mann-Whitney U, ikiden çok
# kategoride ANOVA ve Kruskal-Wallis. parametrik = FALSE ise ikili karşılaştırma ve kalın
# yazılan ana test parametrik olmayan testtir.
# Sonuç bir listedir: kategorik (yüzde ya da ortalama ve GA), ikili (ikili karşılaştırmalar).
grup_sayfasi <- function(veri, grup, dosya = NULL, baslik = NULL, en_cok_kategori = 15,
                         parametrik = TRUE) {
  grup <- vb_sutun_adi(substitute(grup), veri, parent.frame())
  veri <- veri[!is.na(veri[[grup]]), , drop = FALSE]
  y <- veri[[grup]]
  # 10'dan çok farklı değer alan sayısal sonuç sürekli kabul edilir; öteki sonuçlar kategoridir
  surekli <- is.numeric(y) && length(unique(y)) > 10
  if (!surekli && !is.factor(y)) y <- factor(y)
  grup_etiket <- vb_etiket(veri, grup)
  if (is.null(baslik)) baslik <- paste0("Sonuca göre profil: ", grup_etiket)

  kategorik <- setdiff(vb_kategorikler(veri, en_cok_kategori), grup)

  # Kategorik sonuç: ki-kare testi; beklenen sayısı 5'ten küçük hücre varsa Fisher kesin
  # testi (gtsummary add_p() ile aynı kural)
  test_et <- function(x, yy = y) {
    tab <- table(x, yy)
    tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
    if (nrow(tab) < 2 || ncol(tab) < 2) return(list(p = NA_real_, test = ""))
    beklenen <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$expected)
    if (all(beklenen >= 5))
      return(list(p = stats::chisq.test(tab, correct = FALSE)$p.value, test = "ki-kare"))
    p <- tryCatch(stats::fisher.test(tab, workspace = 2e7)$p.value,
                  error = function(e) stats::fisher.test(tab, simulate.p.value = TRUE, B = 1e4)$p.value)
    list(p = p, test = "Fisher")
  }

  # Sürekli sonuç: iki kategoride t testi (Welch) ve Mann-Whitney U; ikiden çok kategoride
  # ANOVA ve Kruskal-Wallis
  surekli_test <- function(x, yy = y) {
    secim <- !is.na(x)
    g <- factor(x[secim], levels = intersect(vb_duzeyler(x), unique(x[secim])))
    v <- yy[secim]
    if (nlevels(g) < 2) return(list(p1 = NA_real_, ad1 = "", p2 = NA_real_, ad2 = ""))
    if (nlevels(g) == 2)
      return(list(p1 = stats::t.test(v ~ g)$p.value, ad1 = "t testi",
                  p2 = suppressWarnings(stats::wilcox.test(v ~ g, exact = FALSE))$p.value, ad2 = "M-W U"))
    list(p1 = stats::oneway.test(v ~ g, var.equal = TRUE)$p.value, ad1 = "ANOVA",
         p2 = stats::kruskal.test(v ~ g)$p.value, ad2 = "K-W")
  }

  # İkili karşılaştırma: kategoriler ikişer alınıp karşılaştırılır; p değeri çift sayısıyla
  # çarpılır (Bonferroni düzeltmesi). Sürekli sonuçta parametrik = TRUE ise t testi, FALSE
  # ise Mann-Whitney U kullanılır.
  ikili_karsilastir <- function(x) {
    ciftler <- utils::combn(vb_duzeyler(x), 2)
    do.call(rbind, lapply(seq_len(ncol(ciftler)), function(k) {
      secim <- !is.na(x) & x %in% ciftler[, k]
      xx <- factor(x[secim], levels = ciftler[, k])
      if (surekli) {
        tst <- surekli_test(xx, y[secim])
        p <- if (parametrik) tst$p1 else tst$p2
        ad_ <- if (parametrik) "t testi" else "M-W U"
      } else {
        tst <- test_et(xx, y[secim])
        p <- tst$p
        ad_ <- tst$test
      }
      data.frame(a = ciftler[1, k], b = ciftler[2, k], p = min(1, p * ncol(ciftler)), test = ad_)
    }))
  }

  # Not satırları sayfa genişliğinde kaydırılır
  not_yaz <- function(satirlar) paste(unlist(lapply(satirlar, strwrap, width = 190)), collapse = "\n")
  ikili <- NULL

  # Üst bölüm: iki kategorili sonuçta kategorilere göre sonuç yüzdesi, sürekli sonuçta ortalama
  if (surekli || nlevels(y) == 2) {
    if (surekli) {
      genel <- mean(y)
      bs <- min(2, max(1, vb_basamak(y)))
    } else {
      olay <- levels(y)[2]
      genel <- 100 * mean(y == olay)
    }
    satirlar <- list()
    ikililer <- list()
    for (j in seq_along(kategorik)) {
      ad <- kategorik[j]
      x <- veri[[ad]]
      if (surekli) {
        tst <- surekli_test(x)
        p_ana <- if (parametrik) tst$p1 else tst$p2
        satirlar[[length(satirlar) + 1]] <- data.frame(
          tur = "baslik", grup_no = j, degisken = ad, kategori = vb_etiket(veri, ad), n = NA, olay = NA,
          deger = NA, alt = NA, ust = NA, p = p_ana, p1 = tst$p1, p2 = tst$p2, test = "",
          sag = paste0(tst$ad1, ": ", vb_p(tst$p1)), sag2 = paste0(tst$ad2, ": ", vb_p(tst$p2)))
      } else {
        tst <- test_et(x)
        p_ana <- tst$p
        satirlar[[length(satirlar) + 1]] <- data.frame(
          tur = "baslik", grup_no = j, degisken = ad, kategori = vb_etiket(veri, ad), n = NA, olay = NA,
          deger = NA, alt = NA, ust = NA, p = tst$p, p1 = tst$p, p2 = NA, test = tst$test,
          sag = paste0(vb_p(tst$p), if (tst$test == "Fisher") "*"), sag2 = "")
      }
      for (k in vb_duzeyler(x)) {
        secim <- !is.na(x) & x == k
        nk <- sum(secim)
        if (surekli) {
          # Ortalama ve %95 GA (t dağılımı)
          ok <- NA
          yz <- if (nk > 0) mean(y[secim]) else NA
          ga <- if (nk > 1) stats::t.test(y[secim])$conf.int else c(NA, NA)
          sag <- if (nk > 0) paste0(vb_sayi(yz, bs), " (", vb_sayi(ga[1], bs), "–", vb_sayi(ga[2], bs), ")") else ""
        } else {
          ok <- sum(y[secim] == olay)
          # Wilson skor yöntemi: prop.test() süreklilik düzeltmesi olmadan
          ga <- if (nk > 0) 100 * suppressWarnings(stats::prop.test(ok, nk, correct = FALSE))$conf.int else c(NA, NA)
          yz <- if (nk > 0) 100 * ok / nk else NA
          sag <- if (nk > 0) paste0(vb_yuzde(yz), " (", vb_sayi(ga[1]), "–", vb_sayi(ga[2]), ")") else ""
        }
        satirlar[[length(satirlar) + 1]] <- data.frame(
          tur = "kategori", grup_no = j, degisken = ad, kategori = k, n = nk, olay = ok,
          deger = yz, alt = ga[1], ust = ga[2], p = p_ana, p1 = NA, p2 = NA, test = "", sag = sag, sag2 = "")
      }
      if (length(vb_duzeyler(x)) > 2 && !is.na(p_ana) && p_ana < 0.05)
        ikililer[[length(ikililer) + 1]] <- cbind(degisken = ad, etiket = vb_etiket(veri, ad),
                                                  ikili_karsilastir(x))
    }
    profil <- do.call(rbind, satirlar)
    m <- nrow(profil)
    profil$konum <- rev(seq_len(m))
    baslik_y <- m + 1
    # Değişkenler sırayla açık zeminli bantlara alınır
    bantlar <- do.call(rbind, lapply(split(profil$konum, profil$grup_no),
                                     function(k) data.frame(alt = min(k) - 0.5, ust = max(k) + 0.5)))
    bantlar <- bantlar[seq(1, nrow(bantlar), by = 2), ]
    bant <- ggplot2::geom_rect(data = bantlar, ggplot2::aes(xmin = -Inf, xmax = Inf, ymin = alt, ymax = ust),
                               fill = "#e9f0f7", alpha = 0.8, inherit.aes = FALSE)
    y_olcek <- ggplot2::scale_y_continuous(limits = c(0.5, m + 1.5), expand = c(0, 0))
    bos_tema <- ggplot2::theme_void(base_size = 8) +
      ggplot2::theme(plot.margin = ggplot2::margin(0, 3, 0, 3),
                     plot.title = ggplot2::element_text(face = "bold", size = 9))

    # Anlamlı çiftler için köşeli çizgiler: kısa aralıklı çift içte, uzun aralıklı dışta
    koseler <- NULL
    if (length(ikililer) > 0) {
      ikili <- do.call(rbind, ikililer)
      anlamli <- ikili[ikili$p < 0.05, ]
      if (nrow(anlamli) > 0) {
        konum_bul <- function(d, k) profil$konum[profil$tur == "kategori" & profil$degisken == d & profil$kategori == k]
        anlamli$y1 <- mapply(konum_bul, anlamli$degisken, anlamli$a)
        anlamli$y2 <- mapply(konum_bul, anlamli$degisken, anlamli$b)
        anlamli$sira <- stats::ave(abs(anlamli$y1 - anlamli$y2), anlamli$degisken,
                                   FUN = function(a) rank(a, ties.method = "first"))
        anlamli$x <- 0.6 + 0.055 * (anlamli$sira - 1)
        koseler <- rbind(
          data.frame(x = anlamli$x, xend = anlamli$x, y = anlamli$y1, yend = anlamli$y2),
          data.frame(x = anlamli$x - 0.025, xend = anlamli$x, y = anlamli$y1, yend = anlamli$y1),
          data.frame(x = anlamli$x - 0.025, xend = anlamli$x, y = anlamli$y2, yend = anlamli$y2))
      }
    }

    g_sol <- ggplot2::ggplot(profil, ggplot2::aes(y = konum)) + bant +
      ggplot2::geom_text(ggplot2::aes(x = ifelse(tur == "baslik", 0.01, 0.07), label = kategori,
                                      fontface = ifelse(tur == "baslik", "bold", "plain")),
                         hjust = 0, size = 2.6) +
      ggplot2::geom_text(ggplot2::aes(x = 0.98, label = ifelse(is.na(n), "", n)), hjust = 1,
                         size = 2.5, colour = "grey30") +
      ggplot2::annotate("text", x = c(0.01, 0.98), y = baslik_y, label = c("Değişken / kategori", "n"),
                        hjust = c(0, 1), fontface = "bold", size = 2.4) +
      ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + y_olcek +
      ggplot2::labs(title = "Kategorik değişkenler") + bos_tema

    kat <- profil[profil$tur == "kategori" & !is.na(profil$deger), ]
    if (surekli) {
      virgul <- function(b) format(b, decimal.mark = ",", trim = TRUE)
      x_olcek <- ggplot2::scale_x_continuous(labels = virgul, expand = ggplot2::expansion(mult = 0.06))
      genel_yazi <- paste0("bütün örneklem ", vb_sayi(genel, bs))
      x_adi <- paste0(grup_etiket, ": ortalama ve %95 GA")
      sag_baslik <- "Ortalama (%95 GA) · p"
    } else {
      x_olcek <- ggplot2::scale_x_continuous(limits = c(0, min(100, ceiling(max(kat$ust) / 10) * 10)),
                                             labels = function(b) paste0("%", b),
                                             expand = ggplot2::expansion(mult = c(0, 0.02)))
      genel_yazi <- paste0("bütün örneklem ", vb_yuzde(genel))
      x_adi <- paste0(grup_etiket, ": \"", olay, "\" yüzdesi ve %95 GA")
      sag_baslik <- "% (%95 GA) · p"
    }
    g_orta <- ggplot2::ggplot(kat, ggplot2::aes(y = konum)) + bant +
      ggplot2::geom_vline(xintercept = genel, linetype = "dashed", colour = "grey40", linewidth = 0.35) +
      ggplot2::geom_linerange(ggplot2::aes(xmin = alt, xmax = ust), colour = "#6baed6", linewidth = 0.9) +
      ggplot2::geom_point(ggplot2::aes(x = deger), colour = "#1f4e79", size = 2) +
      ggplot2::annotate("text", x = genel, y = baslik_y, label = genel_yazi, size = 2.2, colour = "grey30") +
      x_olcek + y_olcek +
      ggplot2::labs(x = x_adi, y = NULL) +
      ggplot2::theme_minimal(base_size = 7) +
      ggplot2::theme(axis.text.y = ggplot2::element_blank(), panel.grid.major.y = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(0, 4, 0, 4))

    # Sağ sütun: kategori satırında yüzde ya da ortalama ve GA, değişken satırında p değeri
    # (sürekli sonuçta iki test yan yana)
    basliklar <- profil[profil$tur == "baslik", ]
    g_sag <- ggplot2::ggplot(profil, ggplot2::aes(y = konum)) + bant +
      ggplot2::geom_text(ggplot2::aes(x = 0.03, label = sag,
                                      fontface = ifelse(tur == "baslik" & !is.na(p1) & p1 < 0.05, "bold", "plain"),
                                      colour = ifelse(tur == "baslik", "grey5", "grey25")),
                         hjust = 0, size = 2.5) +
      ggplot2::scale_colour_identity() +
      ggplot2::annotate("text", x = 0.03, y = baslik_y, label = sag_baslik, hjust = 0,
                        fontface = "bold", size = 2.4) +
      ggplot2::scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) + y_olcek + bos_tema
    if (surekli)
      g_sag <- g_sag + ggplot2::geom_text(data = basliklar,
                                          ggplot2::aes(x = 0.5, label = sag2,
                                                       fontface = ifelse(!is.na(p2) & p2 < 0.05, "bold", "plain")),
                                          hjust = 0, size = 2.5, colour = "grey5")
    if (!is.null(koseler))
      g_sag <- g_sag + ggplot2::geom_segment(data = koseler, ggplot2::aes(x = x, xend = xend, y = y, yend = yend),
                                             colour = "grey25", linewidth = 0.35, inherit.aes = FALSE)

    ust_grafik <- patchwork::wrap_plots(g_sol, g_orta, g_sag, nrow = 1, widths = c(2.2, 3.4, 1.7))
    ust_yukseklik <- (m + 1.5) * 0.165 + 0.5
    if (surekli) {
      not_satirlari <- c(
        paste0("Nokta: kategoride ", grup_etiket, " ortalaması. Yatay çizgi: %95 GA (t dağılımı). ",
               "Kesikli çizgi: bütün örneklemin ortalaması (", vb_sayi(genel, bs), ")."),
        paste0("p: iki kategoride t testi (Welch) ve Mann-Whitney U (M-W U), ikiden çok kategoride ANOVA ve ",
               "Kruskal-Wallis (K-W). p<0,05 kalın yazılır."))
      ikili_yontem <- paste0(if (parametrik) "ANOVA" else "Kruskal-Wallis", " p<0,05 olan değişkenlerde; ",
                             if (parametrik) "t testi (Welch)" else "Mann-Whitney U", ", Bonferroni düzeltmeli p")
    } else {
      not_satirlari <- c(
        paste0("Nokta: kategoride \"", olay, "\" yüzdesi. Yatay çizgi: %95 GA; Wilson skor yöntemiyle (prop.test()) ",
               "hesaplanmıştır. Kesikli çizgi: bütün örneklem (", vb_yuzde(genel), ")."),
        paste0("p: ki-kare testi; beklenen sayısı 5'ten küçük hücre varsa Fisher kesin testi (*) ile işaretlenmiştir. ",
               "p<0,05 kalın yazılır."))
      ikili_yontem <- "p<0,05 olan değişkenlerde; Bonferroni düzeltmeli p"
    }
    if (!is.null(ikili)) {
      ozet <- vapply(split(ikili, factor(ikili$degisken, levels = unique(ikili$degisken))), function(d) {
        a <- d[d$p < 0.05, ]
        paste0(d$etiket[1], ": ", if (nrow(a) == 0) "anlamlı çift yok" else
          paste(paste0(a$a, " ve ", a$b, " ", vb_p(a$p), ifelse(a$test == "Fisher", "*", "")), collapse = "; "))
      }, character(1))
      not_satirlari <- c(not_satirlari, paste0(
        "İkili karşılaştırma (ikiden çok kategorili ve ", ikili_yontem, "): ",
        "köşeli çizgi p<0,05 olan kategori çiftlerini birleştirir. ", paste(ozet, collapse = ". "), "."))
    }
  } else {
    profil <- do.call(rbind, lapply(kategorik, function(ad) {
      tst <- test_et(veri[[ad]])
      tab <- table(factor(veri[[ad]], levels = vb_duzeyler(veri[[ad]])), y)
      d <- as.data.frame(tab, stringsAsFactors = FALSE)
      names(d) <- c("kategori", "grup", "n")
      d$degisken <- ad
      d$p <- tst$p
      d$test <- tst$test
      d$panel <- paste0(vb_etiket(veri, ad), "   ", vb_p(tst$p), if (tst$test == "Fisher") "*")
      d
    }))
    profil$panel <- factor(profil$panel, levels = unique(profil$panel))
    ust_grafik <- ggplot2::ggplot(profil, ggplot2::aes(x = n, y = kategori, fill = grup)) +
      ggplot2::geom_col(position = ggplot2::position_fill(reverse = TRUE), width = 0.7) +
      ggplot2::facet_wrap(~ panel, scales = "free_y", ncol = 2) +
      ggplot2::scale_x_continuous(labels = function(b) paste0("%", 100 * b)) +
      ggplot2::scale_fill_manual(values = vb_renkler(nlevels(y))) +
      ggplot2::labs(x = "Yüzde", y = NULL, fill = grup_etiket, title = "Kategorik değişkenler") +
      vb_tema() + ggplot2::theme(legend.position = "top")
    ust_yukseklik <- ceiling(length(kategorik) / 2) * 1.1 + 0.5
    not_satirlari <- "p: ki-kare testi; beklenen sayısı 5'ten küçük hücre varsa Fisher kesin testi (*) ile işaretlenmiştir."
  }
  kategorik_notu <- not_yaz(not_satirlari)
  not_grafigi <- patchwork::wrap_elements(full = grid::textGrob(
    kategorik_notu, x = grid::unit(0, "npc"), y = grid::unit(1, "npc") - grid::unit(6, "pt"), hjust = 0, vjust = 1,
    gp = grid::gpar(fontsize = 6.5, lineheight = 1.1)))
  not_yuksekligi <- 0.12 * length(strsplit(kategorik_notu, "\n")[[1]]) + 0.15

  # Sayfa yalnız kategorik bölümü taşır; satır yüksekliği sayfayı dolduracak biçimde seçilir
  orman <- surekli || nlevels(y) == 2
  if (orman) {
    satir_boyu <- min(0.3, (9.2 - not_yuksekligi) / (m + 1.5))
    ust_yukseklik <- (m + 1.5) * satir_boyu + 0.5
  }
  bosluk <- max(0.01, 10 - ust_yukseklik - not_yuksekligi)
  sayfa <- patchwork::wrap_plots(ust_grafik, not_grafigi, patchwork::plot_spacer(), ncol = 1,
                                 heights = c(ust_yukseklik, not_yuksekligi, bosluk)) +
    patchwork::plot_annotation(title = baslik, theme = vb_sayfa_temasi)
  vb_kaydet(sayfa, dosya)

  kategori_satirlari <- if (orman) profil[profil$tur == "kategori", ] else profil
  if (surekli) {
    kategori_satirlari <- kategori_satirlari[, c("degisken", "kategori", "n", "deger", "alt", "ust", "p")]
    names(kategori_satirlari)[4] <- "ortalama"
  } else if (orman) {
    kategori_satirlari <- kategori_satirlari[, c("degisken", "kategori", "n", "olay", "deger", "alt", "ust", "p")]
    names(kategori_satirlari)[5] <- "yuzde"
  }
  list(kategorik = kategori_satirlari, ikili = ikili)
}


# ---- 5. Sonuca göre sayısal değişkenler sayfası --------------------------

# grup: sonuç değişkeni (ör. dusme_12ay). Her sayısal değişken çerçeveli bir grafiktir:
# gruplara göre keman ve kutu grafiği; üstünde her grubun ortalama ± SS, ortanca (Q1–Q3)
# ve en küçük–en büyük değeri, onların altında tek satırda testler. İki grupta t testi (Welch) ve Mann-Whitney
# U, ikiden çok grupta ANOVA ve Kruskal-Wallis. p<0,05 kalın yazılır.
# Sonuç sürekli ise (10'dan çok farklı değerli sayısal değişken) her kart bir saçılım grafiğidir:
# doğrusal regresyon doğrusu ve LOESS eğrisi; üstte n, Pearson r (%95 GA) ve Spearman ρ.
# Sonuç bir listedir: testler ve (kategorik sonuçta) tanimlayicilar.
grup_sayisal_sayfasi <- function(veri, grup, dosya = NULL, baslik = NULL, sutun = 3) {
  grup <- vb_sutun_adi(substitute(grup), veri, parent.frame())
  veri <- veri[!is.na(veri[[grup]]), , drop = FALSE]
  y <- veri[[grup]]
  # 10'dan çok farklı değer alan sayısal sonuç sürekli kabul edilir: saçılım grafiği ve korelasyon
  surekli <- is.numeric(y) && length(unique(y)) > 10
  if (!surekli && !is.factor(y)) y <- factor(y)
  grup_etiket <- vb_etiket(veri, grup)
  if (is.null(baslik))
    baslik <- paste0("Sayısal değişkenler: ", grup_etiket, if (surekli) " ile ilişki" else " gruplarında dağılım")
  sayisal <- setdiff(vb_sayisallar(veri), grup)
  if (length(sayisal) == 0) {
    message("Veride sayısal değişken yok.")
    return(invisible(NULL))
  }

  k <- if (surekli) 1 else nlevels(y)
  iki_grup <- k == 2
  test_adlari <- if (iki_grup) c("t testi", "M-W U") else c("ANOVA", "Kruskal-Wallis")
  renkler <- grDevices::colorRampPalette(c("#6baed6", "#f4a582"))(max(k, 2))
  virgul <- function(b) format(b, decimal.mark = ",", trim = TRUE)

  # Kart yüksekliği (inç) ve grafiğin üstündeki yazı bloğu: üç tanımlayıcı satırı ve test satırı
  satir_sayisi <- ceiling(length(sayisal) / sutun)
  kart_boyu <- min(3.1, (9.6 - 0.12 * (satir_sayisi - 1)) / satir_sayisi)
  panel_boyu <- kart_boyu * 1.05 - 0.55
  satir_in <- 0.092
  blok_in <- 5.3 * satir_in + 0.08
  ust_pay <- 1.04 * blok_in / (panel_boyu - blok_in)   # yazı bloğu, veri aralığının katı olarak
  satir_pay <- satir_in * (1.04 + ust_pay) / panel_boyu
  orta <- (k + 1) / 2

  testler <- list()
  tanimlayicilar <- list()
  kartlar <- list()
  for (ad in sayisal) {
    # Sürekli sonuç: saçılım grafiği, doğrusal regresyon doğrusu ve LOESS eğrisi; üstte n,
    # Pearson r (%95 GA) ve Spearman ρ, altında iki korelasyonun p değeri
    if (surekli) {
      d <- data.frame(x = veri[[ad]], y = y)
      d <- d[stats::complete.cases(d), ]
      pr <- stats::cor.test(d$x, d$y)
      sp <- suppressWarnings(stats::cor.test(d$x, d$y, method = "spearman", exact = FALSE))
      testler[[ad]] <- data.frame(degisken = ad, n = nrow(d), r = unname(pr$estimate), r_alt = pr$conf.int[1],
                                  r_ust = pr$conf.int[2], p_pearson = pr$p.value,
                                  rho = unname(sp$estimate), p_spearman = sp$p.value)
      alt <- min(d$y)
      ust <- max(d$y)
      r <- if (ust > alt) ust - alt else 1
      kirilim <- pretty(c(alt, ust), n = 4)
      kirilim <- kirilim[kirilim >= alt & kirilim <= ust]
      x0 <- min(d$x)
      x1 <- max(d$x)
      xr <- if (x1 > x0) x1 - x0 else 1
      sol_kenar <- x0 - 0.04 * xr
      y_satir <- ust + ust_pay * r - (seq_len(3) - 0.5) * satir_pay * r
      degerler <- data.frame(x = sol_kenar + 0.02 * xr, y = y_satir,
                             etiket = c(nrow(d), paste0(vb_sayi(pr$estimate, 2), " (", vb_sayi(pr$conf.int[1], 2),
                                                        "–", vb_sayi(pr$conf.int[2], 2), ")"),
                                        vb_sayi(sp$estimate, 2)))
      adlar <- data.frame(x = sol_kenar, y = y_satir, etiket = c("n:", "Pearson r (%95 GA):", "Spearman \u03c1:"))
      p_yazi <- data.frame(x = (x0 + x1) / 2 + c(-0.03, 0.03) * xr, y = ust + ust_pay * r - 4.1 * satir_pay * r,
                           hjust = c(1, 0),
                           etiket = paste0(c("Pearson", "Spearman"), ": ", vb_p(c(pr$p.value, sp$p.value))),
                           yuz = ifelse(c(pr$p.value, sp$p.value) < 0.05, "bold", "plain"))
      # Az değerli tam sayılarda (ör. kronik hastalık sayısı) noktalar yatayda hafifçe dağıtılır
      titreme <- if (all(d$x == round(d$x)) && length(unique(d$x)) <= 15) min(0.15, 0.02 * xr + 0.1) else 0
      kartlar[[ad]] <- ggplot2::ggplot(d, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point(position = ggplot2::position_jitter(width = titreme, height = 0, seed = 1),
                            size = 0.5, alpha = 0.35, colour = "#2166ac") +
        ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = "grey30",
                             linetype = "dashed", linewidth = 0.4) +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, colour = "#b2182b", fill = "#b2182b",
                             alpha = 0.15, linewidth = 0.6) +
        ggplot2::geom_text(data = degerler, ggplot2::aes(x = x, y = y, label = etiket), hjust = 0, size = 2) +
        ggplot2::geom_text(data = adlar, ggplot2::aes(x = x, y = y, label = etiket), hjust = 1, size = 2,
                           colour = "grey40") +
        ggplot2::geom_text(data = p_yazi, ggplot2::aes(x = x, y = y, label = etiket, hjust = hjust, fontface = yuz),
                           size = 2.15) +
        ggplot2::scale_x_continuous(labels = virgul) +
        ggplot2::scale_y_continuous(breaks = kirilim, labels = virgul) +
        ggplot2::coord_cartesian(xlim = c(sol_kenar, x1 + 0.04 * xr), ylim = c(alt - 0.04 * r, ust + ust_pay * r),
                                 expand = FALSE, clip = "off") +
        ggplot2::labs(title = vb_etiket(veri, ad), x = NULL, y = NULL) +
        vb_tema(7) +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 6.5, colour = "grey30"),
                       plot.title = ggplot2::element_text(size = 7.5), plot.title.position = "plot",
                       plot.background = ggplot2::element_rect(colour = "grey75", linewidth = 0.3, fill = NA),
                       plot.margin = ggplot2::margin(6, 8, 4, 46))
      next
    }

    d <- data.frame(deger = veri[[ad]], grup = y)
    d <- d[!is.na(d$deger), ]
    p1 <- if (iki_grup) stats::t.test(deger ~ grup, data = d)$p.value else
      stats::oneway.test(deger ~ grup, data = d, var.equal = TRUE)$p.value
    p2 <- if (iki_grup) suppressWarnings(stats::wilcox.test(deger ~ grup, data = d, exact = FALSE))$p.value else
      stats::kruskal.test(deger ~ grup, data = d)$p.value
    testler[[ad]] <- data.frame(degisken = ad, test1 = gsub("\n", "", test_adlari[1]), p1 = p1,
                                test2 = gsub("\n", "", test_adlari[2]), p2 = p2)

    # Grupların tanımlayıcıları; aynı değişkende bütün gruplar aynı ondalıkla yazılır
    b <- vb_basamak(d$deger)
    ozet <- do.call(rbind, lapply(levels(y), function(g) {
      xg <- d$deger[d$grup == g]
      if (length(xg) == 0) return(data.frame(grup = g, ortalama_ss = "", ortanca = "", aralik = ""))
      o <- vb_ozet(xg, b)
      data.frame(grup = g, ortalama_ss = o$ortalama_ss, ortanca = o$ortanca_ceyrek, aralik = o$aralik)
    }))
    ozet$grup <- factor(ozet$grup, levels = levels(y))
    tanimlayicilar[[ad]] <- cbind(degisken = ad, ozet)

    # Y ekseni: verinin üstünde tanımlayıcılar için yer bırakılır; çizgiler yalnız veri aralığında
    alt <- min(d$deger)
    ust <- max(d$deger)
    r <- if (ust > alt) ust - alt else 1
    kirilim <- pretty(c(alt, ust), n = 4)
    kirilim <- kirilim[kirilim >= alt & kirilim <= ust]
    # Konumlar veri tablosuna yazılır (aes() içindeki değişkenler çizim anında okunur).
    # Tanımlayıcılar grupların üstünde, adları solda; testler altında tek satırda, ortada.
    y_satir <- ust + ust_pay * r - (seq_len(3) - 0.5) * satir_pay * r
    degerler <- data.frame(grup = rep(ozet$grup, 3), y = rep(y_satir, each = k),
                           etiket = c(ozet$ortalama_ss, ozet$ortanca, ozet$aralik))
    adlar <- data.frame(x = 0.35, y = y_satir,
                        etiket = c("Ortalama ± SS:", "Ortanca (Q1–Q3):", "En küçük–en büyük:"))
    y_test <- ust + ust_pay * r - 4.1 * satir_pay * r
    p_yazi <- data.frame(
      x = orta + c(-0.06, 0.06), y = y_test, hjust = c(1, 0),
      etiket = paste0(gsub("\n", "", test_adlari), ": ", vb_p(c(p1, p2))),
      yuz = ifelse(c(p1, p2) < 0.05, "bold", "plain"))

    kartlar[[ad]] <- ggplot2::ggplot(d, ggplot2::aes(x = grup, y = deger, fill = grup)) +
      ggplot2::geom_violin(colour = NA, alpha = 0.6) +
      ggplot2::geom_boxplot(width = 0.16, outlier.size = 0.4, fill = "white", linewidth = 0.3) +
      ggplot2::geom_text(data = degerler, ggplot2::aes(x = grup, y = y, label = etiket),
                         size = 2, inherit.aes = FALSE) +
      ggplot2::geom_text(data = adlar, ggplot2::aes(x = x, y = y, label = etiket),
                         hjust = 1, size = 2, colour = "grey40", inherit.aes = FALSE) +
      ggplot2::geom_text(data = p_yazi, ggplot2::aes(x = x, y = y, label = etiket, hjust = hjust, fontface = yuz),
                         size = 2.15, inherit.aes = FALSE) +
      ggplot2::scale_y_continuous(breaks = kirilim, labels = virgul, limits = c(alt - 0.04 * r, ust + ust_pay * r),
                                  expand = c(0, 0)) +
      ggplot2::scale_fill_manual(values = renkler) +
      ggplot2::coord_cartesian(clip = "off") +
      ggplot2::labs(title = vb_etiket(veri, ad), x = NULL, y = NULL) +
      vb_tema(7) +
      ggplot2::theme(legend.position = "none", panel.grid.major.x = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(size = 7, colour = "grey10"),
                     plot.title = ggplot2::element_text(size = 7.5), plot.title.position = "plot",
                     plot.background = ggplot2::element_rect(colour = "grey75", linewidth = 0.3, fill = NA),
                     plot.margin = ggplot2::margin(6, 8, 4, 38))
  }

  # Çerçeveler birbirine değmesin: grafikler arasına dar boşluk
  satirlar <- lapply(seq_len(satir_sayisi), function(i) {
    parca <- kartlar[((i - 1) * sutun + 1):min(i * sutun, length(kartlar))]
    while (length(parca) < sutun) parca <- c(parca, list(patchwork::plot_spacer()))
    dizi <- list()
    for (j in seq_len(sutun)) {
      if (j > 1) dizi <- c(dizi, list(patchwork::plot_spacer()))
      dizi <- c(dizi, list(parca[[j]]))
    }
    patchwork::wrap_plots(dizi, nrow = 1, widths = c(1, rep(c(0.05, 1), sutun - 1)))
  })
  dikey <- list()
  dikey_boy <- numeric(0)
  for (i in seq_len(satir_sayisi)) {
    if (i > 1) {
      dikey <- c(dikey, list(patchwork::plot_spacer()))
      dikey_boy <- c(dikey_boy, 0.12)
    }
    dikey <- c(dikey, list(satirlar[[i]]))
    dikey_boy <- c(dikey_boy, kart_boyu)
  }
  bosluk <- max(0.01, 9.6 - sum(dikey_boy))
  test_notu <- if (surekli) {
    paste0("Kırmızı çizgi: doğrusal regresyon doğrusu ve %95 GA; kesikli çizgi: LOESS eğrisi (doğrusal olmayan ilişkiyi gösterir). ",
           "Pearson korelasyonu doğrusal ilişki ve normal dağılım varsayar, Spearman sıra korelasyonu varsaymaz. p<0,05 kalın yazılır.")
  } else if (iki_grup) {
    "t testi (Welch) normal dağılım varsayar, Mann-Whitney U testi (M-W U) varsaymaz; raporlanacak test normallik değerlendirmesine göre seçilir. p<0,05 kalın yazılır."
  } else {
    "ANOVA normal dağılım varsayar, Kruskal-Wallis testi varsaymaz; raporlanacak test normallik değerlendirmesine göre seçilir. p<0,05 kalın yazılır."
  }
  # Uzun not sayfa genişliğinde kaydırılır (yoksa sayfa yana taşar)
  test_notu <- paste(strwrap(test_notu, width = 190), collapse = "\n")
  # wrap_elements: sayfa başlığı ve notu ilk grafiğin paneline değil, sayfanın soluna hizalanır
  sayfa <- patchwork::wrap_elements(full = patchwork::wrap_plots(c(dikey, list(patchwork::plot_spacer())), ncol = 1,
                                                                 heights = c(dikey_boy, bosluk))) +
    patchwork::plot_annotation(
      title = baslik,
      subtitle = if (surekli) paste0("Dikey eksen: ", grup_etiket, ". Her grafiğin üstünde kişi sayısı, korelasyon katsayıları ve p değerleri.") else
        "Her grafiğin üstünde grupların tanımlayıcıları ve grupları karşılaştıran testlerin p değeri.",
      caption = test_notu, theme = vb_sayfa_temasi)
  vb_kaydet(sayfa, dosya)
  if (surekli) return(list(testler = do.call(rbind, testler)))
  list(testler = do.call(rbind, testler), tanimlayicilar = do.call(rbind, tanimlayicilar))
}


# ---- Bütün sayfalar birlikte: veriye_bakis() -----------------------------

# Kullanım kılavuzu: veriye_bakis() parantez içi boş çalıştırılınca ekrana yazılır.
# (?veriye_bakis yalnız paketlerdeki fonksiyonlar için çalışır; bu dosya bir paket değildir.)
vb_yardim <- function() {
  cat(
"veriye_bakis(): bir veri setine ilk bakış için beş sayfalık PDF raporu (A4)

Kullanım:
  veriye_bakis(veri, grup = NULL, dosya = \"ciktilar/veriye_bakis/veriye_bakis.pdf\",
               birlestir = TRUE, parametrik = TRUE)

Seçenekler (eşittirden sonraki değer, yazılmadığında kullanılan değerdir):
  veri        Veri çerçevesi, ör. dusme
  grup        Sonuç değişkeni, tırnaklı ya da tırnaksız: grup = dusme_12ay
              Kategorik sonuç: kategorilerde sonucun yüzdesi; ki-kare / Fisher
              Sürekli sonuç (10'dan çok farklı değerli sayı): ortalama, t testi / ANOVA, korelasyon
              Verilmezse 4. ve 5. sayfa üretilmez.
  dosya       Raporun adı ve yeri, ör. dosya = \"ciktilar/dusme_raporu.pdf\"
              Klasör yoksa oluşturulur; klasör yazılmazsa proje klasörüne kaydedilir.
              Yalnız klasör de verilebilir: dosya = \"ciktilar/rapor/\" (ad: veriye_bakis)
  birlestir   TRUE: beş sayfa bu tek dosyada
              FALSE: beş ayrı dosya aynı klasörde, adları dosya adıyla başlar
              (dusme_raporu_1_veri_karti.pdf ... dusme_raporu_5_grup_sayisal.pdf)
  parametrik  Sürekli sonuçta ikili karşılaştırma: TRUE t testi, FALSE Mann-Whitney U

Sayfalar tek tek de üretilebilir. dosya verilmezse sayfa Plots panelinde görünür;
baslik ile sayfa başlığı değiştirilir:
  veri_karti(veri, dosya = NULL, baslik = \"Veri kimlik kartı\")
  kategorik_sayfasi(veri, dosya = NULL, en_cok_kategori = 15)
  normallik_sayfasi(veri, dosya = NULL)
  grup_sayfasi(veri, grup, dosya = NULL, parametrik = TRUE)
  grup_sayisal_sayfasi(veri, grup, dosya = NULL, sutun = 3)

Sonuç: sayfaların özet tablolarını içeren liste; ör. bakis$normallik, bakis$grup_sayisal$testler

Örnekler:
  bakis <- veriye_bakis(dusme, grup = dusme_12ay)
  bakis <- veriye_bakis(dusme, grup = dusme_12ay, dosya = \"ciktilar/rapor/dusme_raporu.pdf\")
  bakis <- veriye_bakis(dusme, grup = dusme_12ay, dosya = \"ciktilar/rapor/dusme\", birlestir = FALSE)
  bakis <- veriye_bakis(dusme, grup = yurume_hizi, parametrik = TRUE)
  normallik_sayfasi(dusme, dosya = \"ciktilar/normallik.pdf\")
")
  invisible(NULL)
}


# dosya: raporun adı ve yeri, ör. "ciktilar/dusme_raporu.pdf". Klasör yoksa oluşturulur;
#   sonda .pdf yoksa eklenir. Klasör yazılmazsa dosya proje klasörüne kaydedilir. Yalnız
#   klasör verilirse ("ciktilar/rapor/") dosya adı veriye_bakis olur.
# birlestir = TRUE: beş sayfa bu tek dosyada
# birlestir = FALSE: beş ayrı dosya aynı klasörde; adlar dosya adıyla başlar
#   (dusme_raporu_1_veri_karti.pdf ... dusme_raporu_5_grup_sayisal.pdf)
# grup verilmezse sonuca göre sayfalar (4 ve 5) üretilmez.
# Sonuç, sayfaların özet tablolarını içeren bir listedir.
veriye_bakis <- function(veri, grup = NULL, dosya = "ciktilar/veriye_bakis/veriye_bakis.pdf",
                         birlestir = TRUE, parametrik = TRUE) {
  if (missing(veri)) return(vb_yardim())
  grup_adi <- vb_sutun_adi(substitute(grup), veri, parent.frame())

  # Dosya adı ve yeri. Yalnız klasör verilirse ("ciktilar/rapor/") ad veriye_bakis olur.
  if (grepl("/$", dosya) || dir.exists(dosya)) dosya <- file.path(sub("/$", "", dosya), "veriye_bakis.pdf")
  if (!grepl("\\.pdf$", dosya, ignore.case = TRUE)) dosya <- paste0(dosya, ".pdf")
  klasor <- dirname(dosya)
  dir.create(klasor, recursive = TRUE, showWarnings = FALSE)
  ad <- sub("\\.pdf$", "", basename(dosya), ignore.case = TRUE)
  sayfa_dosyasi <- function(ek) if (birlestir) NULL else file.path(klasor, paste0(ad, "_", ek, ".pdf"))

  if (birlestir) {
    # Çok sayfalı PDF aygıtı: her sayfa fonksiyonu kendi sayfasını bu dosyaya basar
    grDevices::cairo_pdf(dosya, width = 21 / 2.54, height = 29.7 / 2.54, onefile = TRUE)
    on.exit(grDevices::dev.off())
  }
  sonuc <- list(
    veri_karti = veri_karti(veri, dosya = sayfa_dosyasi("1_veri_karti")),
    kategorik  = kategorik_sayfasi(veri, dosya = sayfa_dosyasi("2_kategorik")),
    normallik  = normallik_sayfasi(veri, dosya = sayfa_dosyasi("3_normallik")),
    grup       = if (!is.null(grup_adi)) grup_sayfasi(veri, grup_adi, dosya = sayfa_dosyasi("4_grup_kategorik"),
                                                           parametrik = parametrik),
    grup_sayisal = if (!is.null(grup_adi)) grup_sayisal_sayfasi(veri, grup_adi, dosya = sayfa_dosyasi("5_grup_sayisal"))
  )
  if (birlestir) message("Kaydedildi: ", dosya, " (", if (is.null(grup_adi)) 3 else 5, " sayfa)")
  invisible(sonuc)
}
