# Oturum 3 "Tarayıcıda deneyin" sayfasındaki "Sıra sizde" alıştırmaları (yazım kuralları: _alistirma.R)
# dusme sayfa açılınca Bölüm 1–2'deki gibi hazırlanır (_oturum3_dene_basi.qmd).

alistirma_listesi <- list(
  list(bolum = "2", id = "referans", baslik = "Referans kategori",
       metin = "Yalnız yaşama değişkeninde referans \"Hayır\" olsun. Kategorilerin sırasını veren argümanı ve ilk kategoriyi yazın.",
       kod = "x <- c(\"Evet\", \"Hayır\", \"Hayır\")\nlevels(factor(x, {{levels}} = c(\"{{Hayır}}\", \"Evet\")))",
       ipucu = "factor() fonksiyonunda kategorilerin sırası levels ile verilir; ilk yazılan kategori referanstır. Sonuç \"Hayır\" \"Evet\" olmalı."),
  list(bolum = "2", id = "kes", baslik = "Yaş grupları",
       metin = "Üç yaşı 65-74, 75-84 ve 85+ gruplarına ayırın. Son aralığın üst sınırı yok; R'da sonsuzu gösteren sözcüğü yazın.",
       kod = "cut(c(66, 80, 91), breaks = c(64, 74, 84, {{Inf}}), labels = c(\"65-74\", \"75-84\", \"85+\"))",
       ipucu = "Sonsuz R'da Inf ile yazılır (büyük I). Sonuç 65-74, 75-84, 85+ olmalı."),
  list(bolum = "4", id = "spearman", baslik = "Spearman korelasyonu",
       metin = "Yaş ile yürüme hızı arasındaki Spearman korelasyon katsayısını hesaplayın.",
       kod = "cor(dusme$yas, dusme$yurume_hizi, method = \"{{spearman}}\")",
       ipucu = "method argümanına yöntemin adı küçük harfle yazılır. Sonuç yaklaşık -0,59 olmalı."),
  list(bolum = "5", id = "by", baslik = "Gruplara göre tablo",
       metin = "Yaş ve cinsiyeti 12 ayda düşenler ve düşmeyenler için ayrı sütunlarda gösterin.",
       kod = "tbl_summary(dusme, include = c(yas, cinsiyet), {{by}} = dusme_12ay)",
       ipucu = "Tabloyu bir değişkene göre sütunlara bölen argüman Bölüm 5d'de. Tabloda Hayır (n=454) ve Evet (n=146) sütunları görünmeli."),
  list(bolum = "6", id = "nokta", baslik = "Saçılım grafiği",
       metin = "Yaş ile yürüme hızını saçılım grafiğinde gösterin: her katılımcı bir nokta olsun.",
       kod = "ggplot(dusme, aes(x = yas, y = yurume_hizi)) +\n  {{geom_point}}(alpha = 0.3)",
       ipucu = "Nokta katmanı geom_ ile başlar (6b, Adım 2). Yaş arttıkça noktalar aşağı doğru inmeli."),
  list(bolum = "8", id = "lm", baslik = "Doğrusal regresyon",
       metin = "Yürüme hızını yaşla açıklayan doğrusal regresyon modelini kurun ve katsayılara bakın.",
       kod = "model <- {{lm}}(yurume_hizi ~ yas, data = dusme)\ncoef(model)",
       ipucu = "Doğrusal regresyon fonksiyonunun adı iki harftir (linear model). Yaşın katsayısı eksi işaretli olmalı."),
  list(bolum = "9", id = "or", baslik = "Odds oranı",
       metin = "Önceki düşmenin 12 ayda düşme ile ilişkisini lojistik regresyonla inceleyin ve katsayıları odds oranına çevirin.",
       kod = "model <- glm(dusme_12ay ~ onceki_dusme, family = {{binomial}}, data = dusme)\n{{exp}}(coef(model))",
       ipucu = "İki kategorili sonuç için family = binomial yazılır. Katsayı log(OR)'dur; OR için üstel fonksiyon exp() kullanılır. onceki_dusmeEvet satırı yaklaşık 2,58 olmalı.")
)
