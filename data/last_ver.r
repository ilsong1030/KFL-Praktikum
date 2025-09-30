## =========================================================
##  Packages (tibble/dplyr 미사용)
## =========================================================
req <- c("ggplot2","ggpubr","rstatix","scales","cowplot")
new <- req[!sapply(req, requireNamespace, quietly = TRUE)]
if (length(new)) install.packages(new, dependencies = TRUE)
lapply(req, library, character.only = TRUE)

## =========================================================
##  Data (data.frame로 유지)
## =========================================================
df <- read.csv("Ergebnisse.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

## Faktor 순서 고정
df$Tagesabschnitt <- factor(df$Tagesabschnitt, levels = c("Früh","Mittag","Nachmittag"))

uhr_levels <- c('8:10','8:20','8:30','8:40','8:50','9:00','9:10','9:20','9:30','9:40','9:50','10:00',
                '11:40','11:50','11:55','12:00','12:05','12:10','12:15','12:20','12:25','12:30','12:35',
                '12:45','14:55','15:05','15:10','15:15','15:20','15:25','15:30','15:35','15:40','15:45',
                '15:50','15:55','16:00','16:05','16:10','16:15','16:20','16:25','16:30','16:35','16:40',
                '16:45','16:50','17:00')
df$Uhrzeit <- factor(df$Uhrzeit, levels = uhr_levels)

## Uhrzeit 팔레트
pal_uhr <- scales::hue_pal(h = c(15, 300), c = 100, l = 65)(length(uhr_levels))
names(pal_uhr) <- uhr_levels

## =========================================================
##  Helper
## =========================================================
ycol_for <- function(species, kind = c("number","scan")){
  kind <- match.arg(kind)
  paste0(species, "_", kind)
}

## 좌패널(세부: Uhrzeit 색 박스 + 점)
left_panel <- function(data, species, kind = c("number","scan"), ylab_text = NULL, y_max = NULL){
  kind <- match.arg(kind)
  ycol <- ycol_for(species, kind); stopifnot(ycol %in% names(data))
  nm <- if (species == "All_Individuen") "Alle" else species
  if (is.null(ylab_text)) ylab_text <- paste0(nm, "_Anzahl ", if (kind=="number") "(continuous)" else "(scan)")
  if (is.null(y_max)) y_max <- suppressWarnings(max(data[[ycol]], na.rm = TRUE))
  if (!is.finite(y_max)) y_max <- 1
  
  ggplot(data, aes(x = Tagesabschnitt, y = .data[[ycol]], color = Uhrzeit)) +
    geom_boxplot(position = position_dodge2(width = 0.85, preserve = "single"),
                 fill = NA, width = 0.55, outlier.shape = NA, size = 0.6, na.rm = TRUE) +
    geom_point(position = position_jitterdodge(jitter.width = 0.12, dodge.width = 0.85),
               alpha = 0.6, size = 1.8, stroke = 0, na.rm = TRUE) +
    scale_color_manual(values = pal_uhr, drop = FALSE,
                       guide = guide_legend(ncol = 6, title = "Uhrzeit")) +
    coord_cartesian(ylim = c(0, y_max * 1.05)) +
    labs(x = "", y = ylab_text) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "top",
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 8),
      legend.key.size = grid::unit(6, "pt"),
      legend.box.spacing = grid::unit(4, "pt"),
      axis.title.y = element_text(margin = margin(r = 8))
    )
}

## 우패널(요약: 3수준 박스 + Kruskal p 라벨)
right_panel <- function(data, species, kind = c("number","scan"), ylab_text = NULL, y_max = NULL){
  kind <- match.arg(kind)
  ycol <- ycol_for(species, kind); stopifnot(ycol %in% names(data))
  nm <- if (species == "All_Individuen") "Alle" else species
  if (is.null(ylab_text)) ylab_text <- paste0(nm, "_Anzahl ", if (kind=="number") "(continuous)" else "(scan)")
  if (is.null(y_max)) y_max <- suppressWarnings(max(data[[ycol]], na.rm = TRUE))
  if (!is.finite(y_max)) y_max <- 1
  p_y <- y_max * 0.95
  
  ggpubr::ggboxplot(data, x = "Tagesabschnitt", y = ycol, fill = "Tagesabschnitt",
                    order = c("Früh","Mittag","Nachmittag"),
                    outlier.shape = 16, outlier.size = 1.8) +
    coord_cartesian(ylim = c(0, y_max * 1.05)) +
    labs(x = "", y = ylab_text) +
    ggpubr::stat_compare_means(method = "kruskal.test", label.y = p_y) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "none",
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(), # 거의 흰 배경처럼
      axis.title.y = element_text(margin = margin(r = 8))
    )
}

## 범례를 별도 최상단 행으로(좌측 폭에만 맞춤)
legend_row_for <- function(plot_with_legend) {
  lg <- cowplot::get_legend(plot_with_legend)
  ggpubr::as_ggplot(lg)
}

## 한 종 레이아웃(범례행 + 2행2열 / 좌:우=1.6:1.0, 상:하=1:1)
one_species_figure <- function(data, species, unify_ylim = TRUE){
  # 공통 y축(행 단위) 최대값 계산
  y_num <- ycol_for(species, "number")
  y_scn <- ycol_for(species, "scan")
  y_max_num <- suppressWarnings(max(data[[y_num]], na.rm = TRUE)); if (!is.finite(y_max_num)) y_max_num <- 1
  y_max_scn <- suppressWarnings(max(data[[y_scn]], na.rm = TRUE)); if (!is.finite(y_max_scn)) y_max_scn <- 1
  if (!unify_ylim) { y_max_num <- NULL; y_max_scn <- NULL }
  
  # 좌상 패널을 먼저 만들어 범례 추출
  pL_num_tmp <- left_panel (data, species, "number", y_max = y_max_num)
  legend_top <- legend_row_for(pL_num_tmp)
  
  # 좌우 패널(continuous)
  pL_num  <- pL_num_tmp + theme(legend.position = "none")  # 범례 제거본
  pR_num  <- right_panel(data, species, "number", y_max = y_max_num)
  
  # 좌우 패널(scan)
  pL_scan <- left_panel (data, species, "scan",   y_max = y_max_scn) + theme(legend.position = "none")
  pR_scan <- right_panel(data, species, "scan",   y_max = y_max_scn)
  
  # 범례행(좌쪽에만 배치) + 스페이서
  legend_row <- ggpubr::ggarrange(legend_top, ggpubr::ggparagraph(text = "", size = 0.5),
                                  ncol = 2, widths = c(1.6, 1.0))
  
  # 본문 2행
  row_top <- ggpubr::ggarrange(pL_num,  pR_num,  ncol = 2, widths = c(1.6, 1.0))
  row_bot <- ggpubr::ggarrange(pL_scan, pR_scan, ncol = 2, widths = c(1.6, 1.0))
  
  # 최종 조립(범례행 + 본문 2행)
  ggpubr::ggarrange(legend_row, row_top, row_bot,
                    nrow = 3, heights = c(0.28, 1, 1), align = "v")
}

## =========================================================
##  실행: 4개 종을 순서대로 출력
## =========================================================
species_vec <- c("All_Individuen", "Blaumeise", "Kohlmeise", "Sumpfmeise")
for (sp in species_vec) {
  cols_needed <- c(ycol_for(sp,"number"), ycol_for(sp,"scan"))
  if (all(cols_needed %in% names(df))) {
    print(one_species_figure(df, sp, unify_ylim = TRUE))
  } else {
    warning("Missing columns for ", sp, ": ", paste(cols_needed, collapse = ", "))
  }
}
