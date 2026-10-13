# =====================================================================
# 교육심리 연구의 신뢰성 향상을 위한 멀티버스 분석:
# 분석 선택에 따른 결론의 강건성 점검
#
# 01_multiverse_analysis.R : 자료 구성, 점수화, 멀티버스 추정, 본문 수치 산출
#
# 실행 전 준비
#   1) 한국교육종단연구 2013(KELS 2013) 원자료를 한국교육개발원에서 신청하여 받는다.
#   2) 아래 네 파일을 "02 연구데이터" 폴더에 둔다(파일명은 제공받은 이름 그대로).
#        1. L2Y2S_학생.sav            (2차, 초6: 학습자특성 문항이 학생 파일에 포함)
#        2. L2Y3LT_학습자특성.SAV     (3차, 중1)
#        2. L2Y5LT_학습자특성.sav     (5차, 중3)
#        2. L2Y7LT_학습자특성.sav     (7차, 고2)
#   3) 작업 폴더를 이 스크립트가 있는 폴더로 맞춘 뒤 전체를 실행한다.
#
# 산출물(output 폴더)
#   multiverse_results.csv      : 4개 추정 대상(평균·국어·수학·영어) x 192개 설정의 기울기
#   one_switch_results.csv      : 기준 설정에서 선택을 하나씩 바꾼 결과(그림 3)
#   cfa_fit.csv                 : 교과별 종단 측정동일성 모형 적합도
#   manuscript_numbers.txt      : 본문에 보고하는 수치 전체
#   sessionInfo.txt             : R 및 패키지 버전
#
# 필요 패키지: haven, dplyr, tidyr, sandwich, lme4, lavaan
# =====================================================================

rm(list = ls())   # 이전 실행에서 남은 객체(items 등)와 섞이지 않도록 비우고 시작

library(haven)
library(dplyr)
library(tidyr)
library(sandwich)
library(lme4)
library(lavaan)

# 작업 폴더를 이 스크립트가 있는 폴더(저장소_업로드용)로 맞춘다.
#   어느 폴더에서 실행하든 원자료("02 연구데이터")와 결과("output")를 이 폴더 기준으로 찾는다.
script_dir <- tryCatch({
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    dirname(rstudioapi::getSourceEditorContext()$path) else ""
}, error = function(e) "")
if (!nzchar(script_dir) || !dir.exists(file.path(script_dir, "02 연구데이터")))
  script_dir <- "C:/Users/ryu81/Desktop/cowork/Multiverse analysis/A_교육심리_멀티버스소개_KELS/저장소_업로드용"
if (dir.exists(script_dir)) setwd(script_dir)
cat("작업 폴더:", getwd(), "\n")

data_dir <- "02 연구데이터"
out_dir  <- "output"
dir.create(out_dir, showWarnings = FALSE)

# ---------------------------------------------------------------------
# 1. 원자료 불러오기
# ---------------------------------------------------------------------
# 내재적 동기: 문1, 교과별 3문항(4점 척도)
#   1~3 국어, 4~6 수학, 7~9 영어. 각 교과 안에서 1번째 몰입, 2번째 중요성, 3번째 재미.
#   ※ 2차(초6)의 재미 문항은 "○○를 공부하는 것이 재미있으므로, ○○를 포기하지 않겠다"로
#     3·5·7차("○○를 공부하는 것이 재미있다")와 문구가 다르다.
# 자기효능감: 교과별 4문항. 차수에 따라 교과 순서가 다르다.
#   2·3·5차: 1~4 수학, 5~8 영어, 9~12 국어
#   7차    : 1~4 국어, 5~8 수학, 9~12 영어
# 결측 코드: -1(무응답) ~ -5(중복응답). SPSS에 사용자 결측으로 지정되어 있으나,
#   안전을 위해 1~4 범위 밖의 값은 모두 NA로 바꾼다.

wave_info <- data.frame(
  wave      = c(2, 3, 5, 7),
  grade     = c(6, 7, 9, 11),
  file      = c("1. L2Y2S_학생.sav", "2. L2Y3LT_학습자특성.SAV",
                "2. L2Y5LT_학습자특성.sav", "2. L2Y7LT_학습자특성.sav"),
  eff_block = c(5, 4, 4, 4),
  stringsAsFactors = FALSE
)

eff_order <- function(grade) {
  if (grade == 11) list(국어 = 1:4, 수학 = 5:8, 영어 = 9:12)
  else             list(수학 = 1:4, 영어 = 5:8, 국어 = 9:12)
}

subj_code <- c(국어 = "kor", 수학 = "mat", 영어 = "eng")
item_type <- c("imm", "imp", "fun")   # 몰입, 중요성, 재미
clean <- function(x) { x <- as.numeric(x); x[x < 1 | x > 4] <- NA; x }

miss_file <- wave_info$file[!file.exists(file.path(data_dir, wave_info$file))]
if (length(miss_file) > 0)
  stop("원자료 파일을 찾을 수 없습니다: ", paste(miss_file, collapse = ", "),
       "\n작업 폴더(getwd()): ", getwd())

items <- NULL
for (i in seq_len(nrow(wave_info))) {
  w <- wave_info$wave[i]; g <- wave_info$grade[i]
  d <- read_sav(file.path(data_dir, wave_info$file[i]))
  need <- c(sprintf("L2Y%dLT01%02d", w, 1:9),
            sprintf("L2Y%dLT%02d%02d", w, wave_info$eff_block[i], 1:12))
  if (!all(need %in% names(d)))
    stop(wave_info$file[i], "에 없는 변수: ", paste(setdiff(need, names(d)), collapse = ", "))
  dat <- data.frame(SID = as.numeric(d$L2SID), grade = g)
  for (s in names(subj_code)) {
    k0 <- match(s, names(subj_code)) - 1
    for (j in 1:3) {
      v <- sprintf("L2Y%dLT01%02d", w, 3 * k0 + j)
      dat[[paste(subj_code[s], item_type[j], sep = "_")]] <- clean(d[[v]])
    }
  }
  eo <- eff_order(g)
  for (s in names(eo)) {
    ev <- sprintf("L2Y%dLT%02d%02d", w, wave_info$eff_block[i], eo[[s]])
    dat[[paste0("eff_", subj_code[s])]] <-
      rowMeans(sapply(ev, function(v) clean(d[[v]])), na.rm = FALSE)
  }
  items <- rbind(items, dat)
}
items$eff_avg <- rowMeans(items[c("eff_kor", "eff_mat", "eff_eng")], na.rm = FALSE)

mot_cols <- unname(unlist(lapply(subj_code, function(s) paste(s, item_type, sep = "_"))))
stopifnot(all(mot_cols %in% names(items)))
print(table(items$grade))   # 학년별 행 수(각 7,324)

# ---------------------------------------------------------------------
# 2. 점수화: 교과 4(평균·국어·수학·영어) x 점수화 4
#   (1) 3문항 평균
#   (2) 중요성 제외: 몰입·재미 2문항 평균
#   (3) 요인점수(강한동일성): 종단 CFA에서 적재치와 절편을 네 시점에 동일하게 제약
#   (4) 요인점수(부분동일성): (3)에서 절편 동일성 제약을 가장 크게 위배하는 문항
#       (그 문항만 풀었을 때 카이제곱 차이가 가장 큰 문항)의 절편 제약을 풀어 준 모형
#   '평균'은 (1)(2)는 9문항·6문항 평균, (3)(4)는 세 교과 요인점수를
#   각각 초6 기준으로 표준화한 뒤 평균한다.
#   점수는 해당 시점에 그 교과 3문항에 모두 응답한 경우에만 부여한다(네 방식 공통).
# ---------------------------------------------------------------------

grades <- c(6, 7, 9, 11)

wide <- items |>
  select(SID, grade, all_of(mot_cols)) |>
  pivot_wider(id_cols = SID, names_from = grade, values_from = all_of(mot_cols),
              names_sep = "_")

# 종단 CFA 모형 문법 만들기
cfa_syntax <- function(s, level = c("configural", "metric", "scalar"), free_int = NULL) {
  level <- match.arg(level)
  it <- paste(s, item_type, sep = "_")
  lines <- c()
  for (g in grades) {
    lam <- if (level == "configural") c("1*", "", "") else c("1*", "l2*", "l3*")
    lines <- c(lines, sprintf("F%d =~ %s%s_%d + %s%s_%d + %s%s_%d", g,
                              lam[1], it[1], g, lam[2], it[2], g, lam[3], it[3], g))
  }
  if (level == "scalar") {
    for (g in grades) for (k in seq_along(it)) {
      lab <- if (it[k] %in% free_int) "" else paste0("i", k, "*")
      lines <- c(lines, sprintf("%s_%d ~ %s1", it[k], g, lab))
    }
    lines <- c(lines, "F6 ~ 0*1", sprintf("F%d ~ 1", grades[-1]))
  }
  for (k in seq_along(it)) {           # 같은 문항의 시점 간 잔차 공분산
    v <- sprintf("%s_%d", it[k], grades)
    pr <- combn(v, 2)
    lines <- c(lines, sprintf("%s ~~ %s", pr[1, ], pr[2, ]))
  }
  paste(lines, collapse = "\n")
}

fit_cfa <- function(model) {
  cfa(model, data = wide, estimator = "MLR", missing = "fiml", meanstructure = TRUE)
}

# 절편 동일성 제약을 가장 크게 위배하는 문항 찾기
#   문항마다 그 문항의 절편 제약만 풀어 준 모형을 적합하고, 강한동일성 모형과의
#   척도화 카이제곱 차이 검정(Satorra-Bentler)에서 개선이 가장 큰 문항을 고른다.
worst_intercept_item <- function(sc, fit_strong) {
  it <- paste(sc, item_type, sep = "_")
  tab <- data.frame(item = it, chisq_diff = NA, df_diff = NA, p = NA)
  for (k in seq_along(it)) {
    fk  <- fit_cfa(cfa_syntax(sc, "scalar", free_int = it[k]))
    lrt <- lavTestLRT(fk, fit_strong)
    tab$chisq_diff[k] <- lrt[2, "Chisq diff"]
    tab$df_diff[k]    <- lrt[2, "Df diff"]
    tab$p[k]          <- lrt[2, "Pr(>Chisq)"]
  }
  tab <- tab[order(-tab$chisq_diff), ]
  list(item = tab$item[1], table = tab)
}

fit_row <- function(fit, subject, model) {
  fm <- round(as.numeric(fitMeasures(fit, c("chisq.scaled", "df.scaled", "cfi.robust",
                                            "rmsea.robust", "srmr"))), 3)
  data.frame(subject = subject, model = model, chisq_scaled = fm[1], df = fm[2],
             cfi_robust = fm[3], rmsea_robust = fm[4], srmr = fm[5])
}

fs_long  <- list()
cfa_fit  <- NULL
free_log <- NULL
for (s in names(subj_code)) {
  sc <- subj_code[s]
  m_conf <- fit_cfa(cfa_syntax(sc, "configural"))
  m_met  <- fit_cfa(cfa_syntax(sc, "metric"))
  m_str  <- fit_cfa(cfa_syntax(sc, "scalar"))
  worst  <- worst_intercept_item(sc, m_str)
  m_par  <- fit_cfa(cfa_syntax(sc, "scalar", free_int = worst$item))

  cfa_fit <- rbind(cfa_fit,
                   fit_row(m_conf, s, "형태 동일성"),
                   fit_row(m_met,  s, "적재치 동일성"),
                   fit_row(m_str,  s, "강한동일성(절편)"),
                   fit_row(m_par,  s, paste0("부분동일성(", worst$item, " 절편 자유화)")))
  free_log <- rbind(free_log, data.frame(subject = s, worst$table))

  for (nm in c("강한", "부분")) {
    fit <- if (nm == "강한") m_str else m_par
    fs  <- lavPredict(fit, method = "regression")
    idx <- lavInspect(fit, "case.idx")
    sid <- wide$SID[idx]
    for (g in grades) {
      fs_long[[length(fs_long) + 1]] <- data.frame(
        SID = sid, grade = g, subject = s,
        measure = ifelse(nm == "강한", "요인점수(강한동일성)", "요인점수(부분동일성)"),
        y = fs[, paste0("F", g)])
    }
  }
}
fs_long <- bind_rows(fs_long)
write.csv(cfa_fit,  file.path(out_dir, "cfa_fit.csv"),  row.names = FALSE, fileEncoding = "UTF-8")
write.csv(free_log, file.path(out_dir, "cfa_partial_test.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# 응답 완료 여부(교과별·시점별): 3문항 모두 응답해야 점수 부여
complete_flag <- items |>
  transmute(SID, grade,
            국어 = complete.cases(across(starts_with("kor_"))),
            수학 = complete.cases(across(starts_with("mat_"))),
            영어 = complete.cases(across(starts_with("eng_")))) |>
  mutate(평균 = 국어 & 수학 & 영어) |>
  pivot_longer(c(국어, 수학, 영어, 평균), names_to = "subject", values_to = "complete")

# 평균·문항 제외 점수
mean_scores <- NULL
for (s in names(subj_code)) {
  sc <- subj_code[s]
  mean_scores <- rbind(mean_scores,
    data.frame(SID = items$SID, grade = items$grade, subject = s, measure = "3문항 평균",
               y = rowMeans(items[paste(sc, item_type, sep = "_")], na.rm = FALSE)),
    data.frame(SID = items$SID, grade = items$grade, subject = s, measure = "중요성 제외",
               y = rowMeans(items[paste(sc, c("imm", "fun"), sep = "_")], na.rm = FALSE)))
}
mean_scores <- rbind(mean_scores,
  data.frame(SID = items$SID, grade = items$grade, subject = "평균", measure = "3문항 평균",
             y = rowMeans(items[mot_cols], na.rm = FALSE)),
  data.frame(SID = items$SID, grade = items$grade, subject = "평균", measure = "중요성 제외",
             y = rowMeans(items[grep("_(imm|fun)$", mot_cols, value = TRUE)], na.rm = FALSE)))

# 세 교과 요인점수를 초6 기준으로 표준화하여 평균
fs_avg <- fs_long |>
  inner_join(filter(complete_flag, subject != "평균"), by = c("SID", "grade", "subject")) |>
  filter(complete) |>
  group_by(subject, measure) |>
  mutate(y = (y - mean(y[grade == 6])) / sd(y[grade == 6])) |>
  group_by(SID, grade, measure) |>
  summarise(n_sub = n(), y = mean(y), .groups = "drop") |>
  filter(n_sub == 3) |>
  mutate(subject = "평균") |>
  select(SID, grade, subject, measure, y)

eff_long <- items |>
  transmute(SID, grade, 국어 = eff_kor, 수학 = eff_mat, 영어 = eff_eng, 평균 = eff_avg) |>
  pivot_longer(c(국어, 수학, 영어, 평균), names_to = "subject", values_to = "eff")

long <- bind_rows(mean_scores, fs_long, fs_avg) |>
  left_join(complete_flag, by = c("SID", "grade", "subject")) |>
  filter(complete %in% TRUE, !is.na(y)) |>
  select(-complete) |>
  left_join(eff_long, by = c("SID", "grade", "subject")) |>
  group_by(subject, measure) |>
  mutate(is_out = abs((y - mean(y)) / sd(y)) > 3) |>     # 점수 유형별 ±3SD
  ungroup()

# ---------------------------------------------------------------------
# 3. 멀티버스 격자: 추정 대상(교과) 하나당 4 x 3 x 2 x 2 x 2 x 2 = 192개 설정
# ---------------------------------------------------------------------
measures <- c("3문항 평균", "중요성 제외", "요인점수(강한동일성)", "요인점수(부분동일성)")
grid <- expand.grid(
  subject = c("평균", "국어", "수학", "영어"),
  measure = measures,
  sample  = c("전체", "2시점 이상", "완전 응답"),
  outlier = c("미제거", "±3SD 제거"),
  scaling = c("기준 시점 SD", "통합 SD"),
  shape   = c("선형", "구간"),
  model   = c("통합 회귀", "다층성장"),
  stringsAsFactors = FALSE)

time_points <- list(전체 = c(6, 7, 9, 11), 중등 = c(7, 9, 11))

# ---------------------------------------------------------------------
# 4. 설정 하나 추정
#   기울기 단위: 학년당 표준편차 변화(SD/년)
#   구간: 중3(9)을 매듭으로 전·후반 기울기를 추정하고 그 평균(b_t + 0.5 b_tb)을 요약 기울기로 쓴다.
#   기준 시점 SD: 분석에 포함된 첫 시점(전체 4개 차수에서는 초6)의 평균과 SD로 표준화
#   시점별 표준화, 공변량 통제, 시간 단위(조사 차수)는 그림 3 비교(질문을 바꾸는 선택)에서만 쓴다.
#   시간 단위: 학년(6, 7, 9, 11) 또는 조사 차수(1, 2, 3, 4). 차수로 매기면 기울기 단위가 '차수당 변화'가 된다.
# ---------------------------------------------------------------------
run_spec <- function(subject, measure, sample, outlier, scaling, shape, model,
                     tp = "전체", cov = FALSE, tunit = "학년") {
  d <- long[long$subject == subject & long$measure == measure &
              long$grade %in% time_points[[tp]], ]
  if (outlier == "±3SD 제거") d <- d[!d$is_out, ]
  nw <- table(d$SID)
  if (sample == "2시점 이상") d <- d[d$SID %in% names(nw)[nw >= 2], ]
  if (sample == "완전 응답")  d <- d[d$SID %in% names(nw)[nw == length(time_points[[tp]])], ]
  if (cov) d <- d[!is.na(d$eff), ]

  std <- function(v) {
    if (scaling == "기준 시점 SD") {
      b <- v[d$grade == min(d$grade)]; (v - mean(b)) / sd(b)
    } else if (scaling == "통합 SD") {
      (v - mean(v)) / sd(v)
    } else {                                   # "시점별"
      ave(v, d$grade, FUN = function(x) (x - mean(x)) / sd(x))
    }
  }
  d$z <- std(d$y)
  tv <- if (tunit == "차수") match(d$grade, c(6, 7, 9, 11)) else d$grade
  d$t <- tv - mean(tv)
  form <- "z ~ t"
  if (shape == "구간") { d$tb <- pmax(d$grade - 9, 0); form <- "z ~ t + tb" }
  if (cov) { d$ez <- std(d$eff); form <- paste(form, "+ ez") }

  if (model == "다층성장") {
    fit <- lmer(as.formula(paste(form, "+ (t | SID)")), data = d, REML = FALSE,
                control = lmerControl(calc.derivs = FALSE))
    b <- fixef(fit); V <- as.matrix(vcov(fit))
  } else {
    fit <- lm(as.formula(form), data = d)
    b <- coef(fit); V <- vcovCL(fit, cluster = d$SID)   # 개인 군집 강건 표준오차
  }
  w <- setNames(rep(0, length(b)), names(b)); w["t"] <- 1
  if (shape == "구간") w["tb"] <- 0.5
  slope <- sum(w * b); se <- sqrt(drop(t(w) %*% V %*% w))
  data.frame(slope = slope, se = se, sig_decline = slope + 1.96 * se < 0,
             n_students = length(unique(d$SID)))
}

# ---------------------------------------------------------------------
# 5. 멀티버스 실행 (768회 추정, 다층성장모형 때문에 시간이 걸린다)
# ---------------------------------------------------------------------
results <- NULL
for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  r <- run_spec(g$subject, g$measure, g$sample, g$outlier, g$scaling, g$shape, g$model)
  results <- rbind(results, cbind(g, r))
  if (i %% 50 == 0) message(i, " / ", nrow(grid))
}
write.csv(results, file.path(out_dir, "multiverse_results.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------
# 6. 기준 설정에서 선택을 하나씩 바꾼 비교(그림 3)
#   기준: 세 교과 평균, 3문항 평균, 2시점 이상, 이상치 미제거, 기준 시점 SD, 선형, 통합 회귀
# ---------------------------------------------------------------------
base <- list(subject = "평균", measure = "3문항 평균", sample = "2시점 이상",
             outlier = "미제거", scaling = "기준 시점 SD", shape = "선형", model = "통합 회귀")
one <- function(label, group, ...) {
  a <- modifyList(base, list(...))
  cbind(label = label, group = group, do.call(run_spec, a))
}
one_switch <- rbind(
  one("기준: 세 교과 평균",                "기준"),
  one("교과를 수학으로",                   "질문을 바꾸는 선택", subject = "수학"),
  one("교과를 영어로",                     "질문을 바꾸는 선택", subject = "영어"),
  one("교과를 국어로",                     "질문을 바꾸는 선택", subject = "국어"),
  one("시점 선택을 중등 3개 차수로",       "질문을 바꾸는 선택", tp = "중등"),
  one("시점별 표준화",                     "질문을 바꾸는 선택", scaling = "시점별"),
  one("시간 단위를 조사 차수로",           "질문을 바꾸는 선택", tunit = "차수"),
  one("자기효능감 통제",                   "질문을 바꾸는 선택", cov = TRUE),
  one("점수화만 변경(중요성 제외)",        "[비교] 같은 질문 안의 선택", measure = "중요성 제외"),
  one("형태만 변경(구간)",                 "[비교] 같은 질문 안의 선택", shape = "구간"))
write.csv(one_switch, file.path(out_dir, "one_switch_results.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")

# ---------------------------------------------------------------------
# 7. 본문 수치 산출
# ---------------------------------------------------------------------
sink(file.path(out_dir, "manuscript_numbers.txt"))

cat("== 응답자 수(동기 문항 1개 이상 응답) ==\n")
resp <- items |> mutate(any = rowSums(!is.na(across(all_of(mot_cols)))) > 0)
print(resp |> group_by(grade) |> summarise(n = sum(any)))
cat("네 시점 모두 응답:", sum(table(resp$SID[resp$any]) == 4), "\n\n")

cat("== 표본 정의별 학생 수(4개 시점, 이상치 미제거) ==\n")
print(results |> filter(outlier == "미제거") |>
        group_by(subject, sample) |>
        summarise(최소 = min(n_students), 최대 = max(n_students), .groups = "drop"))

cat("\n== 교과별 이상치(±3SD) 사례 수 ==\n")
print(long |> group_by(subject, measure) |> summarise(이상치 = sum(is_out), .groups = "drop"))

cat("\n== 추정 대상별 192개 설정 요약 ==\n")
summ <- results |> group_by(subject) |>
  summarise(중앙값 = median(slope), Q1 = quantile(slope, .25), Q3 = quantile(slope, .75),
            최소 = min(slope), 최대 = max(slope),
            유의 = sum(sig_decline), 비유의 = sum(!sig_decline)) |>
  mutate(across(where(is.double), \(x) round(x, 3)))
print(summ)

cat("\n== 질문 1: 점수화별 중앙값 ==\n")
print(results |> filter(subject == "평균") |> group_by(measure) |>
        summarise(중앙값 = round(median(slope), 3)))

cat("\n== 질문 1: 점수화 외 다섯 선택을 바꿀 때 기울기 변화(짝지은 설정 간 평균 절대 차이) ==\n")
q1 <- filter(results, subject == "평균")
choice_cols <- c("measure", "sample", "outlier", "scaling", "shape", "model")
for (f in choice_cols) {
  others <- setdiff(choice_cols, f)
  dd <- q1 |> group_by(across(all_of(others))) |>
    summarise(range = max(slope) - min(slope), .groups = "drop")
  cat(sprintf("  %-8s 평균 %.4f, 최대 %.4f\n", f, mean(dd$range), max(dd$range)))
}

cat("\n== 질문 1: 분산분해(각 선택이 설명하는 기울기 변동 %) ==\n")
an <- anova(lm(slope ~ measure + sample + outlier + scaling + shape + model, data = q1))
print(round(100 * an[["Sum Sq"]] / sum(an[["Sum Sq"]]), 1) |> setNames(rownames(an)))

cat("\n== 질문 2(국어): 중요성 문항 처리에 따른 두 군집 ==\n")
kor <- filter(results, subject == "국어") |>
  mutate(군집 = ifelse(measure %in% c("3문항 평균", "요인점수(강한동일성)"),
                     "중요성 반영", "중요성 제외·자유화"))
print(kor |> group_by(군집) |>
        summarise(중앙값 = round(median(slope), 3), 비유의 = sum(!sig_decline)))
print(kor |> group_by(measure) |>
        summarise(중앙값 = round(median(slope), 3), 비유의 = sum(!sig_decline)))

cat("\n== 질문 2: 국어 문항별 초6 대비 고2 표준화 평균 변화 ==\n")
for (it in paste("kor", item_type, sep = "_")) {
  b <- items[[it]][items$grade == 6]
  z <- (items[[it]] - mean(b, na.rm = TRUE)) / sd(b, na.rm = TRUE)
  cat(sprintf("  %s: %+.3f\n", it, mean(z[items$grade == 11], na.rm = TRUE)))
}

cat("\n== 측정동일성 모형 적합도 ==\n")
print(cfa_fit)
cat("\n== 문항별 절편 제약 해제 시 카이제곱 차이 검정 ==\n")
print(free_log)

cat("\n== 그림 3: 기준 설정에서 하나씩 바꾼 결과 ==\n")
print(one_switch |> mutate(across(c(slope, se), \(x) round(x, 3))))

sink()

writeLines(capture.output(sessionInfo()), file.path(out_dir, "sessionInfo.txt"))
save(results, one_switch, long, file = file.path(out_dir, "analysis_objects.RData"))
