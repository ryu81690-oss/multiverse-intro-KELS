# =====================================================================
# 02_figures.R : 본문 그림 1~3
# 01_multiverse_analysis.R 실행 후 사용한다(output/analysis_objects.RData를 읽음).
# 그림 안의 수치(중앙값, 유의 비율)는 결과에서 계산해 넣는다. 컬러판과 흑백판을 저장한다.
# =====================================================================

library(ggplot2)
library(patchwork)
library(dplyr)
library(tidyr)

# 실행: 어느 폴더에서 실행하든 된다. 아래 한 줄을 그대로 실행하면 된다.
#   source("C:/Users/ryu81/Desktop/cowork/Multiverse analysis/A_교육심리_멀티버스소개_KELS/저장소_업로드용/02_figures.R", encoding = "UTF-8")
# 이 스크립트에 필요한 입력은 01이 저장한 output/analysis_objects.RData 하나뿐이다(원자료 불필요).
# 그림은 그 RData가 있는 폴더(output)에 저장된다.

find_rdata <- function() {
  cands <- c(file.path("output", "analysis_objects.RData"),        # 작업 폴더 = 저장소_업로드용
             "analysis_objects.RData",                             # 작업 폴더 = output
             "C:/Users/ryu81/Desktop/cowork/Multiverse analysis/A_교육심리_멀티버스소개_KELS/저장소_업로드용/output/analysis_objects.RData")
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (nzchar(p)) cands <- c(file.path(dirname(p), "output", "analysis_objects.RData"), cands)
  }
  hit <- cands[file.exists(cands)]
  if (length(hit) == 0)
    stop("analysis_objects.RData를 찾을 수 없습니다. 01_multiverse_analysis.R을 먼저 실행하거나 ",
         "위 cands에 파일 경로를 추가하세요.")
  normalizePath(hit[1])
}
rdata   <- find_rdata()
load(rdata)                                   # results, one_switch, long
results <- results |> mutate(measure = sub("강한 동일성", "강한동일성", measure))  # 옛 표기 통일
fig_dir <- dirname(rdata)
cat("입력:", rdata, "\n그림 저장 폴더:", fig_dir, "\n")

# 표 2의 용어에 맞춰 그림 3의 묶음 이름을 바꾼다(01의 결과 객체가 옛 이름으로 저장된 경우 대비).
one_switch <- one_switch |>
  mutate(group = dplyr::recode(group,
                               "추정 대상을 바꾸는 선택" = "질문을 바꾸는 선택",
                               "[비교] 임의적 선택"      = "[비교] 같은 질문 안의 선택"))

y_lab <- "학년당 변화(SD/년)"
axis_order <- c("점수화", "표본", "이상치", "표준화", "형태", "모형")

grid_long <- function(d, keep = "x") {
  d |>
    select(all_of(keep), 점수화 = measure, 표본 = sample, 이상치 = outlier,
           표준화 = scaling, 형태 = shape, 모형 = model) |>
    pivot_longer(-all_of(keep), names_to = "axis", values_to = "choice") |>
    mutate(axis = factor(axis, axis_order))
}

grid_theme <- theme_minimal(base_size = 10) +
  theme(strip.placement = "outside",
        strip.text.y.left = element_text(angle = 0, face = "bold", size = 9),
        panel.grid = element_blank(),
        panel.spacing.y = unit(4, "pt"))

# ---- 그림 1. 질문 1(세 교과 평균)의 명세곡선과 각 설정의 분석 선택 ----
draw_fig1 <- function(sig_col, zero_col, grid_col) {
  d <- results |> filter(subject == "평균") |> arrange(slope) |> mutate(x = row_number())
  lab1 <- sprintf("유의한 하락 %d/%d개(%.0f%%)", sum(d$sig_decline), nrow(d),
                  100 * mean(d$sig_decline))
  lab2 <- sprintf("중앙값 %.3f", median(d$slope))

  top <- ggplot(d, aes(x, slope)) +
    geom_hline(yintercept = 0, color = zero_col, linewidth = .7, linetype = "42") +
    geom_linerange(aes(ymin = slope - 1.96 * se, ymax = slope + 1.96 * se),
                   color = sig_col, alpha = .6, linewidth = .55) +
    geom_point(size = .8, color = "grey20") +
    scale_x_continuous(limits = c(0.5, 192.5), expand = c(0, 0)) +
    coord_cartesian(ylim = c(min(d$slope - 1.96 * d$se) * 1.05, 0.004)) +
    labs(x = NULL, y = y_lab) +
    annotate("text", 8, -0.010, label = lab1, size = 3.4, hjust = 0, color = "grey35") +
    annotate("text", 8, -0.016, label = lab2, size = 3.4, hjust = 0, color = "grey35") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          axis.text.x = element_blank())

  bottom <- ggplot(grid_long(d), aes(x, choice)) +
    geom_tile(width = 1, height = .72, fill = grid_col) +
    facet_grid(axis ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_x_continuous(limits = c(0.5, 192.5), expand = c(0, 0)) +
    labs(x = "설정 (기울기 순 정렬, 192개). 칠해진 칸이 각 설정이 택한 선택", y = NULL) +
    grid_theme

  top / bottom + plot_layout(heights = c(1, 1.15))
}

ggsave(file.path(fig_dir, "그림1_컬러.png"), draw_fig1("#E69F00", "#B22222", "grey35"),
       width = 9.2, height = 8.2, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "그림1_흑백.png"), draw_fig1("black", "black", "grey30"),
       width = 9.2, height = 8.2, dpi = 300, bg = "white")

# ---- 그림 2. 질문 2(국어)의 명세곡선과 각 설정의 분석 선택 ----
# 점수화 방식별로 색을 달리한다. 네 색이 서로 구별되도록 회색·하늘색·주황·진파랑을 쓴다.
draw_fig2 <- function(pal4, zero_col) {
  lv <- c("3문항 평균", "요인점수(강한동일성)", "중요성 제외", "요인점수(부분동일성)")
  d <- results |> filter(subject == "국어") |> arrange(slope) |>
    mutate(x = row_number(), grp = factor(measure, lv))
  med <- d |> group_by(grp) |> summarise(m = median(slope), xm = median(x), .groups = "drop")
  pal <- setNames(pal4, lv)

  top <- ggplot(d, aes(x, slope, color = grp)) +
    geom_hline(yintercept = 0, color = zero_col, linewidth = .7, linetype = "42") +
    geom_linerange(aes(ymin = slope - 1.96 * se, ymax = slope + 1.96 * se),
                   alpha = .3, linewidth = .5) +
    geom_point(aes(shape = sig_decline, fill = grp), size = 1.8, stroke = .6) +
    scale_shape_manual(values = c("TRUE" = 21, "FALSE" = 1), guide = "none") +
    scale_color_manual(values = pal, name = NULL) +
    scale_fill_manual(values = pal, guide = "none") +
    geom_label(data = med, aes(xm, m, label = sprintf("중앙값 %.3f", m)),
               inherit.aes = FALSE, vjust = -0.9, size = 3.4, color = "grey10",
               fill = "white", label.size = 0, label.padding = unit(1.5, "pt")) +
    scale_x_continuous(limits = c(0.5, 192.5), expand = c(0, 0)) +
    labs(x = NULL, y = y_lab, caption = "빈 원 = 신뢰구간이 0을 포함한 설정") +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          axis.text.x = element_blank(),
          legend.position = c(.99, .05), legend.justification = c(1, 0))

  bottom <- ggplot(grid_long(d, keep = c("x", "grp")), aes(x, choice, fill = grp)) +
    geom_tile(width = 1, height = .72) +
    facet_grid(axis ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_manual(values = pal, guide = "none") +
    scale_x_continuous(limits = c(0.5, 192.5), expand = c(0, 0)) +
    labs(x = "설정 (기울기 순 정렬, 192개). 칠해진 칸이 각 설정이 택한 선택", y = NULL) +
    grid_theme

  top / bottom + plot_layout(heights = c(1, 1.15))
}

ggsave(file.path(fig_dir, "그림2_컬러.png"),
       draw_fig2(c("grey55", "#56B4E9", "#E69F00", "#0072B2"), "#B22222"),
       width = 9.2, height = 8.2, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "그림2_흑백.png"),
       draw_fig2(c("grey78", "grey55", "grey30", "black"), "black"),
       width = 9.2, height = 8.2, dpi = 300, bg = "white")

# ---- 그림 3. 기준 설정에서 선택을 하나씩 바꾸었을 때의 기울기 ----
# 왼쪽 묶음 이름은 두세 줄로 접어 폭을 줄이고, x축 눈금은 0을 포함해 소수 셋째 자리까지 고정 표기한다.
grp_lv  <- c("기준", "질문을 바꾸는 선택", "[비교] 같은 질문 안의 선택")
grp_lab <- c("기준", "질문을\n바꾸는\n선택", "[비교]\n같은 질문\n안의 선택")
sw <- one_switch |>
  mutate(group = factor(group, grp_lv, labels = grp_lab),
         label = factor(label, rev(label)))

draw_fig3 <- function(col_est, col_arb) {
  pal   <- setNames(c("grey20", col_est, col_arb), grp_lab)
  base  <- sw$slope[sw$group == "기준"]
  ggplot(sw, aes(slope, label, color = group, shape = group)) +
    geom_vline(xintercept = 0, color = "grey25", linewidth = .6, linetype = "42") +
    geom_vline(xintercept = base, color = "grey55", linetype = 3) +
    geom_linerange(aes(xmin = slope - 1.96 * se, xmax = slope + 1.96 * se), linewidth = 1) +
    geom_point(size = 2.8) +
    geom_text(aes(label = ifelse(abs(slope) < 0.0005, "0.000", sprintf("%+.3f", slope))),
              vjust = -1.0, size = 3.2, show.legend = FALSE) +
    scale_x_continuous(breaks = c(-0.100, -0.075, -0.050, -0.025, 0, 0.025),
                       labels = function(x) sprintf("%.3f", x),
                       limits = c(-0.122, 0.040), expand = c(0, 0)) +
    facet_grid(group ~ ., scales = "free_y", space = "free_y", switch = "y") +
    scale_color_manual(values = pal, guide = "none") +
    scale_shape_manual(values = c(15, 16, 17), guide = "none") +
    scale_y_discrete(expand = expansion(add = .9)) +
    labs(x = "학년당 변화(SD/년)", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(strip.placement = "outside",
          strip.text.y.left = element_text(angle = 0, face = "bold", size = 10,
                                           hjust = 0.5, lineheight = 1.05),
          strip.background = element_blank(),
          panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
          panel.spacing.y = unit(10, "pt"),
          axis.text.y = element_text(size = 10, color = "grey15"))
}

ggsave(file.path(fig_dir, "그림3_컬러.png"), draw_fig3("#E69F00", "#2E6FB7"),
       width = 8.2, height = 5.6, dpi = 300, bg = "white")
ggsave(file.path(fig_dir, "그림3_흑백.png"), draw_fig3("grey25", "grey55"),
       width = 8.2, height = 5.6, dpi = 300, bg = "white")
