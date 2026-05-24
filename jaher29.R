# ------------------------------------------------------------
# スクリプトにおける変数の名づけ等は以下のとおり．
# ------------------------------------------------------------
# ◆結果変数：revenue2=revenue-grant
# 事業活動収支差額	revenue	2012-2023
# 私学助成	grant	2012-2023
# ◆処置変数
# 私学助成　grant 2012-2023
# ◆交絡変数　
# 学生数　stu 2012-2023
# 教員数　teach 2012-2023
# 法人傘下の学校数　school 2012-2023
# ◆交絡因子の候補（欠測値補完に利用）
# 職員数　staff 2012-2023
# 附属病院ダミー　med 2012-2023
# 平均偏差値　stat 2012-2023
# 3大都市圏ダミー  cityd 2012-2023
# 設置年　found 2012-2023
# ◆ 効果修飾：fillr=ent/capa
# 入学者総数　ent 2012-2023
# 入学定員数　capa 2012-2023
# ◆IDなど
# 東洋経済ID	ID
# 大学名	name

# ------------------------------------------------------------
# 準備
# ------------------------------------------------------------
# Rのバージョン確認
R.version

# ワークスペースのクリア: オブジェクト一括削除
rm(list = ls())

# データの取り込み
df <- read.csv(file.choose(), stringsAsFactors = FALSE, fileEncoding = "Shift-JIS")
head(df, 5)

# ------------------------------------------------------------
# パッケージの読み込み
# ------------------------------------------------------------
if (!require("dplyr")) install.packages("dplyr") 
library(dplyr) 
if (!require("tidyr")) install.packages("tidyr") 
library(tidyr) 
if (!require("mice")) install.packages("mice") 
library(mice) 
if (!require("WeightIt")) install.packages("WeightIt") 
library(WeightIt) 
if (!require("gbm")) install.packages("gbm") 
library(gbm) 
if (!require("ggplot2")) install.packages("ggplot2") 
library(ggplot2) 
if (!require("EValue")) install.packages("EValue")
library(EValue)

# ------------------------------------------------------------
# データの確認
# ------------------------------------------------------------
# データのサイズ確認
dim(df)

# 列名の確認
names(df)

# 大学数の確認
nrow(df)

# データ構造の確認
str(df)

# ------------------------------------------------------------
# 変数の作成：入学定員充足率（ワイドのまま）
# ------------------------------------------------------------
# 各年度の入学定員充足率を計算
for(year in 2012:2023) {
  df[[paste0("fillr", year)]] <- (df[[paste0("ent", year)]] / df[[paste0("capa", year)]]) * 100
}

# 結果の確認（最初の3大学の入学充足率を表示）
head(df[, c("ID", "name2023", 
            "fillr2012", "fillr2013", "fillr2014", 
            "fillr2015", "fillr2016", "fillr2017", 
            "fillr2018", "fillr2019", "fillr2020", 
            "fillr2021", "fillr2022", "fillr2023")], 3)

# ------------------------------------------------------------
# 変数の作成：定員割れカテゴリ（ワイドのまま）
# ------------------------------------------------------------
# 各年度の定員割れカテゴリ変数を作成
for (year in 2012:2023) {
  fillr <- df[[paste0("fillr", year)]]
  
  df[[paste0("capacity_ordinal_60_", year)]] <- cut(
    fillr,
    breaks = c(-Inf, 60, 80, 100, Inf),
    labels = c(4, 3, 2, 1),  # 1=100%以上, 2=80-100%, 3=60-80%, 4=60%未満
    right = FALSE
  )
}

# ------------------------------------------------------------
# 変数の作成：結果変数revenue2 = revenue - grant（ワイドのまま）
# ------------------------------------------------------------
years <- 2012:2023

for (year in years) {
  rev_col   <- paste0("revenue", year)
  grant_col <- paste0("grant", year)
  out_col   <- paste0("revenue2", year)
  
  # 列の存在確認
  if (!rev_col %in% names(df))   stop("列がない: ", rev_col)
  if (!grant_col %in% names(df)) stop("列がない: ", grant_col)
  
  df[[out_col]] <- as.numeric(df[[rev_col]]) - as.numeric(df[[grant_col]])
}

# 確認（先頭3校・2012と2020と2023）
check_cols <- c(
  "ID",
  paste0("revenue", c(2012, 2020, 2023)),
  paste0("grant", c(2012, 2020, 2023)),
  paste0("revenue2", c(2012, 2020, 2023))
)
check_cols <- intersect(check_cols, names(df))
head(df[, check_cols], 3)

# 欠測の確認（どちらかNAなら revenue2 も NA）
for (year in years) {
  r <- df[[paste0("revenue", year)]]
  g <- df[[paste0("grant", year)]]
  y <- df[[paste0("revenue2", year)]]
  cat(year, ": NA(revenue2) =", sum(is.na(y)),
      " (revenue NA:", sum(is.na(r)), ", grant NA:", sum(is.na(g)), ")\n", sep = "")
}

# ------------------------------------------------------------
# grant（私学助成）の分布確認（ワイドのまま）
# ------------------------------------------------------------
years <- 2012:2023
grant_cols <- paste0("grant", years)
K <- 5  # 5分位

# 行列（大学 × 年）
G <- as.matrix(df[, grant_cols])
mode(G) <- "numeric"

# 表1：年別要約統計
tab1 <- data.frame(
  year     = years,
  n        = colSums(!is.na(G)),
  n_na     = colSums(is.na(G)),
  mean     = round(colMeans(G, na.rm = TRUE), 1),
  median   = round(apply(G, 2, median, na.rm = TRUE), 1),
  sd       = round(apply(G, 2, sd, na.rm = TRUE), 1),
  min      = round(apply(G, 2, min, na.rm = TRUE), 1),
  p25      = round(apply(G, 2, quantile, 0.25, na.rm = TRUE), 1),
  p75      = round(apply(G, 2, quantile, 0.75, na.rm = TRUE), 1),
  max      = round(apply(G, 2, max, na.rm = TRUE), 1),
  p95      = round(apply(G, 2, quantile, 0.95, na.rm = TRUE), 1)
)

print(tab1, row.names = FALSE)

# 保存
write.csv(tab1, "table1_grant_summary_by_year.csv", row.names = FALSE)

# 表2：年別・5分位の境界値（20%, 40%, 60%, 80%点）
# cut_12 = 1群|2群 の境界 など
probs_cut <- (1:(K - 1)) / K  # 0.2, 0.4, 0.6, 0.8

tab2 <- data.frame(
  year = years,
  n    = colSums(!is.na(G)),
  cut_12 = round(apply(G, 2, quantile, probs = 0.2, na.rm = TRUE), 1),
  cut_23 = round(apply(G, 2, quantile, probs = 0.4, na.rm = TRUE), 1),
  cut_34 = round(apply(G, 2, quantile, probs = 0.6, na.rm = TRUE), 1),
  cut_45 = round(apply(G, 2, quantile, probs = 0.8, na.rm = TRUE), 1)
)

print(tab2, row.names = FALSE)
write.csv(tab2, "table2_grant_quintile_cutpoints_by_year.csv", row.names = FALSE)


# 表3：年 × 5分位 の人数（ntile の確認）
count_list <- vector("list", length(years))
for (i in seq_along(years)) {
  x <- G[, i]
  bin <- ntile(x, K)  # NA は NA のまま
  count_list[[i]] <- as.data.frame(table(bin, useNA = "ifany"))
  count_list[[i]]$year <- years[i]
  names(count_list[[i]]) <- c("quintile", "n", "year")
}
tab3_long <- do.call(rbind, count_list)
tab3_long <- tab3_long[, c("year", "quintile", "n")]

# 見やすい形（年が行・分位が列）に広げる
tab3 <- reshape(tab3_long, direction = "wide",
                idvar = "year", timevar = "quintile", v.names = "n")
names(tab3)[-1] <- paste0("Q", 1:K)

print(tab3, row.names = FALSE)
write.csv(tab3, "table3_grant_quintile_counts_by_year.csv", row.names = FALSE)

# ------------------------------------------------------------
# 変数群の記述統計（ワイド）
#   - 年次変数：2012–2023 をプール（大学×年）
#   - 時不変：found（大学×1）
#   - ダミー：med, cityd（プール＋割合）
# ------------------------------------------------------------
years <- 2012:2023

# 年次変数（接頭辞 + 年）
var_prefixes_year <- c(
  "revenue", "revenue2", "grant",     # 結果・処置
  "stu", "teach", "school",           # 交絡
  "staff", "stat",                     # 補完候補（連続）
  "ent", "capa", "fillr"               # 効果修飾
)

# 年次ダミー（別表で prop_1 も出す）
dummy_prefixes_year <- c("med", "cityd")

# 時不変（列名はデータに合わせて変更可）
found_col <- "found"

# --- 年次変数：1接頭辞ぶん ---
summarize_prefix_year <- function(df, prefix, years) {
  cols <- paste0(prefix, years)
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) {
    warning("列がない: ", prefix, " (", paste0(prefix, years[1]), " など)")
    return(NULL)
  }
  
  x <- as.numeric(as.matrix(df[, cols, drop = FALSE]))
  
  data.frame(
    type     = "year_pooled",
    variable = prefix,
    n        = sum(!is.na(x)),
    n_na     = sum(is.na(x)),
    mean     = round(mean(x, na.rm = TRUE), 2),
    sd       = round(sd(x, na.rm = TRUE), 2),
    median   = round(median(x, na.rm = TRUE), 2),
    min      = round(min(x, na.rm = TRUE), 2),
    p25      = round(quantile(x, 0.25, na.rm = TRUE), 2),
    p75      = round(quantile(x, 0.75, na.rm = TRUE), 2),
    max      = round(max(x, na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
}

# --- 時不変：found（大学単位） ---
summarize_found <- function(df, col_name = "found") {
  if (!col_name %in% names(df)) {
    warning("列がない: ", col_name)
    return(NULL)
  }
  x <- as.numeric(df[[col_name]])
  
  data.frame(
    type     = "time_invariant",
    variable = col_name,
    n        = sum(!is.na(x)),
    n_na     = sum(is.na(x)),
    mean     = round(mean(x, na.rm = TRUE), 2),
    sd       = round(sd(x, na.rm = TRUE), 2),
    median   = round(median(x, na.rm = TRUE), 2),
    min      = round(min(x, na.rm = TRUE), 2),
    p25      = round(quantile(x, 0.25, na.rm = TRUE), 2),
    p75      = round(quantile(x, 0.75, na.rm = TRUE), 2),
    max      = round(max(x, na.rm = TRUE), 2),
    stringsAsFactors = FALSE
  )
}

# --- 年次ダミー：連続統計＋0/1割合 ---
summarize_dummy_year <- function(df, prefix, years) {
  cols <- paste0(prefix, years)
  cols <- intersect(cols, names(df))
  if (length(cols) == 0) {
    warning("列がない", prefix)
    return(NULL)
  }
  
  x <- as.numeric(as.matrix(df[, cols, drop = FALSE]))
  
  data.frame(
    type     = "year_pooled_dummy",
    variable = prefix,
    n        = sum(!is.na(x)),
    n_na     = sum(is.na(x)),
    mean     = round(mean(x, na.rm = TRUE), 4),
    sd       = round(sd(x, na.rm = TRUE), 4),
    median   = round(median(x, na.rm = TRUE), 4),
    min      = round(min(x, na.rm = TRUE), 4),
    p25      = round(quantile(x, 0.25, na.rm = TRUE), 4),
    p75      = round(quantile(x, 0.75, na.rm = TRUE), 4),
    max      = round(max(x, na.rm = TRUE), 4),
    prop_0   = round(mean(x == 0, na.rm = TRUE), 4),
    prop_1   = round(mean(x == 1, na.rm = TRUE), 4),
    stringsAsFactors = FALSE
  )
}

# ========== 実行 ==========
desc_year <- do.call(
  rbind,
  lapply(var_prefixes_year, summarize_prefix_year, df = df, years = years)
)

desc_found <- summarize_found(df, found_col)

desc_dummy <- do.call(
  rbind,
  lapply(dummy_prefixes_year, summarize_dummy_year, df = df, years = years)
)

# 1つにまとめる（found がある場合）
desc_all <- desc_year
if (!is.null(desc_found)) desc_all <- rbind(desc_all, desc_found)
rownames(desc_all) <- NULL

print(desc_all, row.names = FALSE)
write.csv(desc_all, "desc_all_variables.csv", row.names = FALSE)

print(desc_dummy, row.names = FALSE)
write.csv(desc_dummy, "desc_dummy_med_cityd.csv", row.names = FALSE)

# ------------------------------------------------------------
# ワイド → ロング（大学 × 年）
# ------------------------------------------------------------
years <- 2012:2023

# ID 列
id_cols <- intersect(c("ID", "name", "name2023", "found"), names(df))

# ロング化する変数の接頭辞（revenue2 を revenue より先に！）
prefixes <- c(
  "revenue2", "revenue", "grant",
  "stu", "teach", "school",
  "staff", "med", "stat", "cityd",
  "ent", "capa", "fillr"
)

# 対象列名（存在するものだけ）
year_cols <- as.vector(outer(prefixes, years, paste0))
year_cols <- intersect(year_cols, names(df))

# ロング化
long <- df %>%
  select(all_of(c(id_cols, year_cols))) %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = c(".value", "year"),
    names_pattern = paste0("^(", paste(prefixes, collapse = "|"), ")(\\d{4})$")
  ) %>%
  mutate(year = as.integer(year)) %>%
  arrange(ID, year)

# 確認
dim(long)          # 行数 ≈ 大学数 × 12
head(long, 6)
str(long)

# 1大学だけチェック（元データと一致するか）
df %>% filter(ID == long$ID[1]) %>% select(ID, revenue2012, grant2012)
long %>% filter(ID == long$ID[1], year == 2012) %>% select(ID, year, revenue, grant)

# ------------------------------------------------------------
# 処置変数・結果変数の変換※連続量の処置変数は結局使わなかった
# ------------------------------------------------------------
# 対称対数（revenue2：マイナス・0あり）
# sign(y) * log(|y| + 1)  … IHS と同型
slog <- function(x, c = 1) {
  sign(x) * log(abs(x) + c)
}

long <- long %>%
  mutate(
    revenue2_slog = slog(revenue2),   # 結果（主に MSM で使用）
    grant_log1p   = log(grant + 1)    # grant は 0 以上 → log1p で可（連続処置の感度用）
  )

# 確認
summary(long$revenue2)
summary(long$revenue2_slog)
summary(long$grant)
summary(long$grant_log1p)

# 極端値・0 の件数
long %>%
  summarise(
    n_rev2_zero     = sum(revenue2 == 0, na.rm = TRUE),
    n_rev2_neg      = sum(revenue2 < 0, na.rm = TRUE),
    n_grant_zero    = sum(grant == 0, na.rm = TRUE),
    n_slog_na       = sum(is.na(revenue2_slog)),
    .groups = "drop"
  )

# 元データとの一致（変換は revenue2 非欠測行のみチェック）
long %>%
  filter(!is.na(revenue2)) %>%
  summarise(
    max_abs_diff = max(abs(revenue2_slog - slog(revenue2)), na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 欠測値分析
# 　- 欠測は観測された共変量（年度，規模，ダミーなど）に依存すると仮定（MAR）
# ------------------------------------------------------------
# 欠測率（変数別）
vars_miss <- c("revenue", "revenue2", "grant", "stu", "teach", "school",
               "staff", "med", "stat", "cityd", "ent", "capa", "fillr")
sapply(vars_miss, function(v) mean(is.na(long[[v]])))

# 年別（grant）
long %>% group_by(year) %>%
  summarise(n = n(), pct_miss_grant = mean(is.na(grant)),
            pct_miss_staff = mean(is.na(staff)), .groups = "drop")

# 大学別：欠測の年数
long %>% group_by(ID) %>%
  summarise(n_miss_rev = sum(is.na(revenue)),
            n_miss_staff = sum(is.na(staff)), .groups = "drop") %>%
  summary()

# ------------------------------------------------------------
# 多重代入（MICE）— 欠測値補完
# Austin et al. (2020): PS/MSM には stu, teach, school のみ
# 補助: staff, med, stat, cityd, found, year（調整はしない）
# ------------------------------------------------------------
# --- 1) 分析対象行（結果・処置は観測がある行だけ） ---
dat <- long %>%
  filter(!is.na(revenue), !is.na(grant)) %>%
  mutate(year = factor(year))

# --- 2) 補完用データセット ---
# 補完する変数
imp_targets <- c("stu", "teach", "school", "staff", "ent", "capa")

# mice に渡す列（ID は補完に使わない）
mice_cols <- c(
  imp_targets,
  "med", "stat", "cityd", "found", "year",
  "revenue", "grant"   # 補完しないが予測子として入れる
)

dat_mice <- dat %>%
  select(all_of(mice_cols))

# --- 3) 補完方法の指定 ---
ini <- mice(dat_mice, maxit = 0, printFlag = FALSE)

meth <- ini$method
meth["revenue"] <- ""
meth["grant"]   <- ""
meth["found"]   <- ""      # 欠測なし
meth["year"]    <- ""      # 完全観測
meth["med"]     <- "logreg"
meth["cityd"]   <- "logreg"
# 連続変数は既定の pmm のまま（stu, teach, school, staff, ent, capa, stat）

pred <- ini$predictorMatrix
# 補完しない変数は「ほかを予測する側」だけつかう（行は 0 = 予測子にしない）
pred[, c("revenue", "grant", "found", "year")] <- 0
pred[c("revenue", "grant", "found", "year"), ] <- 0

# --- 4) MICE 実行 ---
set.seed(123)
mids <- mice(
  dat_mice,
  m            = 50,
  method       = meth,
  predictorMatrix = pred,
  maxit        = 100,
  printFlag    = TRUE
)

# 収束の確認
plot(mids, c("stu", "teach", "school"))

# --- 5) 補完後データ（1つ目 + 全 m） ---
# fillr は ent/capa から再計算（capa=0 に注意）
make_completed <- function(comp) {
  comp %>%
    mutate(
      fillr = ifelse(!is.na(capa) & capa > 0, ent / capa * 100, NA_real_)
    )
}

imp1 <- make_completed(complete(mids, 1))

# 元の列と結合（revenue2, name2023, ID など）
stopifnot(nrow(dat) == nrow(imp1))  # 行数が一致するか確認

imp1_id <- imp1 %>%
  mutate(ID = dat$ID)

long_imp1 <- dat %>%
  select(ID, name2023, year, revenue, revenue2, grant) %>%
  left_join(
    imp1_id %>% select(ID, year, stu, teach, school, staff, ent, capa, fillr,
                       med, stat, cityd, found),
    by = c("ID", "year")
  )

# ------------------------------------------------------------
# 代入データセット一覧 long_imp_list の作成
# （mids から．MICE に ID を入れていないので行順で結合）
# ------------------------------------------------------------
# 対称対数（結果変数用）
slog <- function(x, c = 1) sign(x) * log(abs(x) + c)

# MICE 後の comp に ent/capa から fillr を再計算
make_completed <- function(comp) {
  comp %>%
    mutate(
      fillr = ifelse(!is.na(capa) & capa > 0, ent / capa * 100, NA_real_)
    )
}

# MICE で補完した列（dat の同名列を上書きする）
imp_cols <- c(
  "stu", "teach", "school",           # 交絡（PS に入れる）
  "staff", "ent", "capa", "fillr",    # 補助・修飾
  "med", "stat", "cityd"              # 補助（PS には入れない）
)

long_imp_list <- lapply(1:mids$m, function(i) {
  # i 番目の代入完了データ（ID 列なし）
  comp <- make_completed(complete(mids, i))

  # MICE 入力 dat と行数・行順が同じ前提
  if (nrow(dat) != nrow(comp)) {
    stop("dat と complete の行数が一致しない．")
  }

  out <- dat
  out[imp_cols] <- comp[imp_cols]   # 補完済みで上書き

  out %>%
    mutate(
      # year が factor のときは整数に戻す
      year = if (is.factor(year)) as.integer(as.character(year)) else as.integer(year),
      # 分析用の変換（MICE の前後で同じ式）
      revenue2_slog = slog(revenue2),   # 結果（MSM）
      grant_log1p   = log(grant + 1)    # 処置（連続・仮説1）
    )
})

# 確認
length(long_imp_list)                                    # m=50 なら 50
sum(is.na(long_imp_list[[1]]$stu))                       # 0 なら補完成功
head(long_imp_list[[1]] %>% select(ID, year, revenue2_slog, grant_log1p, stu, teach, school), 3)

# =============================================================================
# 仮説1・仮説2：IPTW ＋ 周辺構造モデル（ラグ仕様 ＋ 年別5分位 QBA）
# -----------------------------------------------------------------------------
# 処置：前年の私学助成 grant_{t-1} を，各年の5分位に区分（grant_bin_l1）
# 結果：当年の revenue2_slog（revenue2 = revenue - grant の対称対数）
# 共変量（PS）：前年の stu, teach, school（媒介・事後化のリスクを避ける）
# 修飾（仮説2のみ）：前年の fillr（連続・中心化 fillr_c）
# 推定：m=50 の代入データごとに weightit → トリミング → glm → pool()
# 前提：long_imp_list（MICE 済み），library(dplyr), WeightIt, mice
# =============================================================================
# =============================================================================
# 仮説1：私学助成の平均因果効果はない
# =============================================================================
run_h1_iptw <- function(dat_i, trim_q = c(0.01, 0.99)) {
  
  # --- 分析に必要な変数が揃った行 ---
  d <- dat_i %>%
    filter(
      !is.na(revenue2_slog),   # 結果（当年）
      !is.na(grant),             # 処置の元（生の助成額）
      !is.na(stu), !is.na(teach), !is.na(school)
    ) %>%
    arrange(ID, year) %>%
    group_by(ID) %>%
    mutate(
      # 処置・共変量は 1 期ラグ（t-1）
      # 同年の L を PS に入れると中間変数になりうるため
      grant_l1  = lag(grant, 1),
      stu_l1    = lag(stu, 1),
      teach_l1  = lag(teach, 1),
      school_l1 = lag(school, 1)
    ) %>%
    ungroup() %>%
    # 2012 年は前年が無いので除外
    filter(!is.na(grant_l1)) %>%
    group_by(year) %>%
    mutate(
      # 年別5分位（quantile binning；Naimi et al. 2014）
      # 1 = その年の下位20%（助成が少ない）… 5 = 上位20%
      grant_bin_l1 = ntile(grant_l1, K)
    ) %>%
    ungroup() %>%
    mutate(grant_bin_l1 = factor(grant_bin_l1))
  
  # --- 傾向スコア → 重み（多項処置） ---
  # 連続処置の用量–反応仮定（Robins et al. 2000）を避けるためビン化
  wobj <- weightit(
    grant_bin_l1 ~ stu_l1 + teach_l1 + school_l1,
    data = d,
    method = "ps",
    estimand = "ATE"
  )
  
  sw <- wobj$weights
  # 極端な重みを下位1%・上位1%でトリミング
  lo <- quantile(sw, trim_q[1], na.rm = TRUE)
  hi <- quantile(sw, trim_q[2], na.rm = TRUE)
  sw_trim <- pmin(pmax(sw, lo), hi)
  
  # --- 周辺構造モデル（重み付き GLM） ---
  # 当年の revenue2_slog を、前年助成の5分位で説明
  fit <- glm(
    revenue2_slog ~ grant_bin_l1,
    data = d,
    weights = sw_trim
  )
  
  list(
    fit = fit,
    n = nrow(d),
    ess = sum(sw_trim)^2 / sum(sw_trim^2),
    trim_lo = lo,
    trim_hi = hi
  )
}


# --- m 本の代入データで繰り返し（進捗表示） ---
h1_results <- vector("list", length(long_imp_list))
for (i in seq_along(long_imp_list)) {
  cat("H1 imp", i, "/", length(long_imp_list), " ", format(Sys.time()), "\n")
  h1_results[[i]] <- run_h1_iptw(long_imp_list[[i]])
}

# 重み・サンプルサイズの要約（任意）
h1_diag <- data.frame(
  imp = seq_along(h1_results),
  n   = sapply(h1_results, `[[`, "n"),
  ess = sapply(h1_results, `[[`, "ess")
)
print(h1_diag)

# --- Rubin のルールでプール ---
h1_fits <- lapply(h1_results, `[[`, "fit")
pooled_h1 <- pool(h1_fits)
print(summary(pooled_h1, conf.int = TRUE))

# --- 仮説1の判断 ---
# grant_bin_l1 は因子のため係数が Q2–Q5 の4本（Q1が基準）

# =============================================================================
# 仮説2：定員割れ水準（fillr）による効果修飾
# =============================================================================
run_h2_iptw <- function(dat_i, trim_q = c(0.01, 0.99)) {
  
  d <- dat_i %>%
    filter(
      !is.na(revenue2_slog),
      !is.na(grant),
      !is.na(stu), !is.na(teach), !is.na(school),
      !is.na(fillr)              # 修飾変数（入学充足率）
    ) %>%
    arrange(ID, year) %>%
    group_by(ID) %>%
    mutate(
      grant_l1  = lag(grant, 1),
      stu_l1    = lag(stu, 1),
      teach_l1  = lag(teach, 1),
      school_l1 = lag(school, 1),
      fillr_l1  = lag(fillr, 1)   # 修飾も前年（処置と同時点）
    ) %>%
    ungroup() %>%
    filter(!is.na(grant_l1), !is.na(fillr_l1)) %>%
    group_by(year) %>%
    mutate(grant_bin_l1 = ntile(grant_l1, K)) %>%
    ungroup() %>%
    mutate(
      grant_bin_l1 = factor(grant_bin_l1),
      # 中心化：主効果＝平均入学充足率での私学助成効果の解釈用
      fillr_c = fillr_l1 - mean(fillr_l1, na.rm = TRUE)
    )
  
  # PS は仮説1と同じ
  wobj <- weightit(
    grant_bin_l1 ~ stu_l1 + teach_l1 + school_l1,
    data = d,
    method = "ps",
    estimand = "ATE"
  )
  
  sw <- wobj$weights
  lo <- quantile(sw, trim_q[1], na.rm = TRUE)
  hi <- quantile(sw, trim_q[2], na.rm = TRUE)
  sw_trim <- pmin(pmax(sw, lo), hi)
  
  # 効果修飾：前年助成（5分位）× 前年充足率（連続）
  fit <- glm(
    revenue2_slog ~ grant_bin_l1 * fillr_c,
    data = d,
    weights = sw_trim
  )
  
  list(
    fit = fit,
    n = nrow(d),
    ess = sum(sw_trim)^2 / sum(sw_trim^2),
    fillr_mean = mean(d$fillr_l1)
  )
}


# --- m 本実行 ---
h2_results <- vector("list", length(long_imp_list))
for (i in seq_along(long_imp_list)) {
  cat("H2 imp", i, "/", length(long_imp_list), " ", format(Sys.time()), "\n")
  h2_results[[i]] <- run_h2_iptw(long_imp_list[[i]])
}

h2_diag <- data.frame(
  imp = seq_along(h2_results),
  n   = sapply(h2_results, `[[`, "n"),
  ess = sapply(h2_results, `[[`, "ess")
)
print(h2_diag)

# --- プール ---
h2_fits <- lapply(h2_results, `[[`, "fit")
pooled_h2 <- pool(h2_fits)
print(summary(pooled_h2, conf.int = TRUE))

# =============================================================================
# 仮説2-b 効果修飾：定員割れ4カテゴリ × 私学助成5分位（IPTW + MI プール）
# コントラスト：処置ダミー（基準＝100%以上、助成Q1）
# 前提：df, long_imp_list, K (=5), revenue2_slog, grant など
# =============================================================================
# --- 1) カテゴリをロングに結合 ---
cap_cols <- paste0("capacity_ordinal_60_", 2012:2023)

cap_long <- df %>%
  select(ID, all_of(cap_cols)) %>%
  pivot_longer(
    cols = all_of(cap_cols),
    names_to = "year",
    values_to = "capacity_ord_raw",
    names_prefix = "capacity_ordinal_60_"
  ) %>%
  mutate(
    year = as.integer(year),
    capacity_ord_num = as.integer(as.character(capacity_ord_raw)),
    capacity_ord = factor(
      capacity_ord_num,
      levels = 1:4,
      labels = c("100%以上", "80-100%", "60-80%", "60%未満"),
      ordered = FALSE
    )
  ) %>%
  select(ID, year, capacity_ord_num, capacity_ord)

long_imp_list_cap <- lapply(long_imp_list, function(d) {
  dplyr::left_join(d, cap_long, by = c("ID", "year"))
})

# --- 2) 解析用データの前処理（1代入分） ---
prep_h2_cap_data <- function(dat_i, trim_q = c(0.01, 0.99)) {
  
  d <- dat_i %>%
    filter(
      !is.na(revenue2_slog), !is.na(grant),
      !is.na(stu), !is.na(teach), !is.na(school),
      !is.na(capacity_ord_num)
    ) %>%
    arrange(ID, year) %>%
    group_by(ID) %>%
    mutate(
      grant_l1          = lag(grant, 1),
      stu_l1            = lag(stu, 1),
      teach_l1          = lag(teach, 1),
      school_l1         = lag(school, 1),
      capacity_l1_num   = lag(capacity_ord_num, 1),
      capacity_l1       = factor(
        capacity_l1_num,
        levels = 1:4,
        labels = c("100%以上", "80-100%", "60-80%", "60%未満"),
        ordered = FALSE
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(grant_l1), !is.na(capacity_l1)) %>%
    group_by(year) %>%
    mutate(grant_bin_l1 = factor(ntile(grant_l1, K))) %>%
    ungroup()
  
  contrasts(d$capacity_l1) <- contr.treatment(4, base = 1)
  
  wobj <- weightit(
    grant_bin_l1 ~ stu_l1 + teach_l1 + school_l1,
    data = d,
    method = "ps",
    estimand = "ATE"
  )
  
  sw <- wobj$weights
  lo <- quantile(sw, trim_q[1], na.rm = TRUE)
  hi <- quantile(sw, trim_q[2], na.rm = TRUE)
  sw_trim <- pmin(pmax(sw, lo), hi)
  
  list(data = d, weights = sw_trim, wobj = wobj)
}

# --- 3) 効果修飾あり ---
run_h2_cap_iptw <- function(dat_i, trim_q = c(0.01, 0.99)) {
  
  prep <- prep_h2_cap_data(dat_i, trim_q)
  d <- prep$data
  sw_trim <- prep$weights
  
  fit <- glm(
    revenue2_slog ~ grant_bin_l1 * capacity_l1,
    data = d,
    weights = sw_trim
  )
  
  list(
    fit = fit,
    n = nrow(d),
    ess = sum(sw_trim)^2 / sum(sw_trim^2)
  )
}

# --- 4) m 本実行 ---
h2_cap_results <- vector("list", length(long_imp_list_cap))
for (i in seq_along(long_imp_list_cap)) {
  cat("H2-cap imp", i, "/", length(long_imp_list_cap), " ", format(Sys.time()), "\n")
  h2_cap_results[[i]] <- run_h2_cap_iptw(long_imp_list_cap[[i]])
}

# --- 5) プール ---
h2_cap_fits <- lapply(h2_cap_results, `[[`, "fit")
pooled_h2_cap <- pool(h2_cap_fits)
sum_h2_cap <- summary(pooled_h2_cap, conf.int = TRUE)
print(sum_h2_cap)

sum_h2_cap[grepl(":", sum_h2_cap$term), ]

sum_h2_cap[
  grepl("^grant_bin_l1", sum_h2_cap$term) & !grepl(":", sum_h2_cap$term),
]

sum_h2_cap[
  grepl("^capacity_l1", sum_h2_cap$term) & !grepl(":", sum_h2_cap$term),
]

# =============================================================================
# プール結果 → CSV 出力（仮説1 / 2 / 2-b）
# 前提：pooled_h1, pooled_h2, pooled_h2_cap
# =============================================================================
# --- 共通：summary → data.frame ---
pool_to_table <- function(pooled, model_name = "model") {
  s <- summary(pooled, conf.int = TRUE)
  term_vec <- if ("term" %in% names(s)) s$term else rownames(s)
  data.frame(
    model     = model_name,
    term      = term_vec,
    estimate  = s$estimate,
    std.error = s$std.error,
    statistic = s$statistic,
    df        = s$df,
    p.value   = s$p.value,
    ci_low    = s$`2.5 %`,
    ci_high   = s$`97.5 %`,
    conf.low  = s$conf.low,
    conf.high = s$conf.high,
    row.names = NULL
  )
}

# 有意性スター
add_stars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*",
                       ifelse(p < 0.1, ".", ""))))
}

# =============================================================================
# 仮説1
# =============================================================================
tab_h1 <- pool_to_table(pooled_h1, "H1_MSM") %>%
  mutate(
    sig = add_stars(p.value),
    est_ci = sprintf("%.3f (%.3f, %.3f)", estimate, conf.low, conf.high)
  )

write.csv(tab_h1, "table_H1_pooled.csv", row.names = FALSE, fileEncoding = "UTF-8")

# =============================================================================
# 仮説2（pooled_h2 がある場合）
# =============================================================================
if (exists("pooled_h2")) {
  tab_h2 <- pool_to_table(pooled_h2, "H2_MSM") %>%
    mutate(sig = add_stars(p.value), est_ci = sprintf("%.3f (%.3f, %.3f)", estimate, conf.low, conf.high))
  
  write.csv(tab_h2, "table_H2_pooled.csv", row.names = FALSE, fileEncoding = "UTF-8")
  
  tab_h2_grant <- tab_h2 %>%
    filter(grepl("^grant_bin_l1", term), !grepl(":", term)) %>%
    mutate(quintile = paste0("Q", sub("grant_bin_l1", "", term))) %>%
    select(quintile, estimate, std.error, conf.low, conf.high, p.value, sig)
  
  tab_h2_int <- tab_h2 %>%
    filter(grepl(":", term)) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value, sig)
  
  write.csv(tab_h2_grant, "table_H2_grant_main.csv", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(tab_h2_int,   "table_H2_interactions.csv", row.names = FALSE, fileEncoding = "UTF-8")
}

# =============================================================================
# 仮説2-b（pooled_h2_cap がある場合）
# =============================================================================
if (exists("pooled_h2_cap")) {
  tab_h2b <- pool_to_table(pooled_h2_cap, "H2b_MSM") %>%
    mutate(sig = add_stars(p.value), est_ci = sprintf("%.3f (%.3f, %.3f)", estimate, conf.low, conf.high))
  
  write.csv(tab_h2b, "table_H2b_pooled.csv", row.names = FALSE, fileEncoding = "UTF-8")
  
  tab_h2b_grant <- tab_h2b %>%
    filter(grepl("^grant_bin_l1", term), !grepl(":", term)) %>%
    mutate(quintile = paste0("Q", sub("grant_bin_l1", "", term))) %>%
    select(quintile, estimate, std.error, conf.low, conf.high, p.value, sig)
  
  tab_h2b_cap <- tab_h2b %>%
    filter(grepl("^capacity_l1", term), !grepl(":", term)) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value, sig)
  
  tab_h2b_int <- tab_h2b %>%
    filter(grepl(":", term)) %>%
    select(term, estimate, std.error, conf.low, conf.high, p.value, sig)
  
  write.csv(tab_h2b_grant, "table_H2b_grant_main.csv", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(tab_h2b_cap,   "table_H2b_capacity_main.csv", row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(tab_h2b_int,   "table_H2b_interactions.csv", row.names = FALSE, fileEncoding = "UTF-8")
}

# =============================================================================
# 未観測交絡の感度：E-value（仮説1 / 2 / 2-b）
# 前提：pooled_h1, pooled_h2, pooled_h2_cap, long_imp_list, K (=5)
# =============================================================================
# --- 結果変数の SD（E-value 用；代表として代入1） ---
sd_y <- sd(long_imp_list[[1]]$revenue2_slog, na.rm = TRUE)
if (is.na(sd_y) || sd_y <= 0) stop("sd_y が計算できない．long_imp_list を確認しよう．")

# --- プール結果から係数を1本取得 ---
get_coef <- function(pooled, term_name) {
  s <- summary(pooled, conf.int = TRUE)
  if ("term" %in% names(s)) {
    row <- s[s$term == term_name, , drop = FALSE]
  } else {
    row <- s[term_name, , drop = FALSE]
  }
  if (nrow(row) == 0) {
    stop(
      "係数がみつからない: ", term_name,
      "\n利用可能: ", paste(s$term %||% rownames(s), collapse = ", ")
    )
  }
  list(
    est = row$estimate,
    lo  = row$conf.low,
    hi  = row$conf.high,
    p   = row$p.value
  )
}

# --- 1係数の E-value（自分の EValue 版：行=E-values, 列=point/lower/upper） ---
evalue_one <- function(pooled, term_name, label) {
  
  b <- get_coef(pooled, term_name)
  
  ev <- evalues.MD(
    est = b$est,
    lo  = b$lo,
    hi  = b$hi,
    sd  = sd_y
  )
  
  data.frame(
    hypothesis = label,
    term       = term_name,
    estimate   = round(b$est, 3),
    conf.low   = round(b$lo, 3),
    conf.high  = round(b$hi, 3),
    p.value    = signif(b$p, 3),
    E_point    = round(as.numeric(ev["E-values", "point"]), 2),
    E_CI_lower = round(as.numeric(ev["E-values", "lower"]), 2),
    E_CI_upper = round(as.numeric(ev["E-values", "upper"]), 2),
    sd_outcome = round(sd_y, 2),
    stringsAsFactors = FALSE
  )
}

# =============================================================================
# 仮説1：Q2–Q5 vs Q1
# =============================================================================
ev_h1_q5 <- evalue_one(
  pooled_h1,
  paste0("grant_bin_l1", K),
  "H1: Q5 vs Q1"
)

ev_h1_all <- do.call(
  rbind,
  lapply(2:K, function(k) {
    evalue_one(
      pooled_h1,
      paste0("grant_bin_l1", k),
      paste0("H1: Q", k, " vs Q1")
    )
  })
)

print(ev_h1_q5)
print(ev_h1_all)

# =============================================================================
# 仮説2
# =============================================================================
# 有意な効果修飾がないため削除

# =============================================================================
# 仮説2-b：有意だった効果修飾のみ
# =============================================================================
ev_h2b_sig <- do.call(
  rbind,
  lapply(
    list(
      c("grant_bin_l14:capacity_l12", "H2b: Q4 x capacity 80-100%"),
      c("grant_bin_l13:capacity_l13", "H2b: Q3 x capacity 60-80%"),
      c("grant_bin_l14:capacity_l13", "H2b: Q4 x capacity 60-80%"),
      c("grant_bin_l15:capacity_l13", "H2b: Q5 x capacity 60-80%"),
      c("grant_bin_l14:capacity_l14", "H2b: Q4 x capacity 60%未満"),
      c("grant_bin_l15:capacity_l14", "H2b: Q5 x capacity 60%未満")
    ),
    function(x) evalue_one(pooled_h2_cap, x[1], x[2])
  )
)
print(ev_h2b_sig)
ev_h2b <- rbind(ev_h2b_sig)

# =============================================================================
# CSV 出力
# =============================================================================
evalue_h1   <- ev_h1_all
evalue_h2b  <- ev_h2b_sig
evalue_main <- rbind(evalue_h1, evalue_h2b)   # ev_h2 は NULL なら rbind から外す

write.csv(evalue_main, "table_evalue_sensitivity_main.csv",
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(evalue_h1, "table_evalue_sensitivity_H1.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

=========================================================================
# 読み方（メモ）
# - E_point：点推定を無効化する未観測交絡の強さ（大きいほどロバスト）
# - E_CI_lower / E_CI_upper：NA のことがある（推定の符号・CI による）
# - 5分位処置はコントラストごとに E-value（全体で1個ではない）
# - 係数は revenue2_slog 上の MD としての補足的分析
# - capacity_l13 = 60-80%, capacity_l14 = 60%未満（基準=100%以上）
# =======================================================================

# =============================================================================
# 仮説1・確認分析：結果 = revenue（助成控除前）の対数変換
# 処置・PS・MSM は本番 H1 と同じ（grant_bin_l1, stu_l1 等）
# =============================================================================
# 対称対数（本番と同じ定義）
slog <- function(x, c = 1) sign(x) * log(abs(x) + c)

# --- long_imp_list に revenue_log が無ければ追加（1回だけで可）---
if (!"revenue_log" %in% names(long_imp_list[[1]])) {
  long_imp_list <- lapply(long_imp_list, function(d) {
    d %>% mutate(revenue_log = slog(revenue))
  })
}

# --- 1 代入分：本番 run_h1_iptw と同じ流れ，Y だけ差し替え ---
run_h1_iptw_revenue <- function(dat_i, trim_q = c(0.01, 0.99)) {
  
  d <- dat_i %>%
    filter(
      !is.na(revenue_log),      # 結果（当年・生の収支）
      !is.na(grant),
      !is.na(stu), !is.na(teach), !is.na(school)
    ) %>%
    arrange(ID, year) %>%
    group_by(ID) %>%
    mutate(
      grant_l1  = lag(grant, 1),
      stu_l1    = lag(stu, 1),
      teach_l1  = lag(teach, 1),
      school_l1 = lag(school, 1)
    ) %>%
    ungroup() %>%
    filter(!is.na(grant_l1)) %>%
    group_by(year) %>%
    mutate(grant_bin_l1 = ntile(grant_l1, K)) %>%
    ungroup() %>%
    mutate(grant_bin_l1 = factor(grant_bin_l1))
  
  wobj <- weightit(
    grant_bin_l1 ~ stu_l1 + teach_l1 + school_l1,
    data = d,
    method = "ps",
    estimand = "ATE"
  )
  
  sw <- wobj$weights
  lo <- quantile(sw, trim_q[1], na.rm = TRUE)
  hi <- quantile(sw, trim_q[2], na.rm = TRUE)
  sw_trim <- pmin(pmax(sw, lo), hi)
  
  fit <- glm(
    revenue_log ~ grant_bin_l1,
    data = d,
    weights = sw_trim
  )
  
  list(
    fit = fit,
    n = nrow(d),
    ess = sum(sw_trim)^2 / sum(sw_trim^2),
    trim_lo = lo,
    trim_hi = hi
  )
}

# --- m 本ループ ---
h1_rev_results <- vector("list", length(long_imp_list))
for (i in seq_along(long_imp_list)) {
  cat("H1-revenue imp", i, "/", length(long_imp_list), " ", format(Sys.time()), "\n")
  h1_rev_results[[i]] <- run_h1_iptw_revenue(long_imp_list[[i]])
}

h1_rev_diag <- data.frame(
  imp = seq_along(h1_rev_results),
  n   = sapply(h1_rev_results, `[[`, "n"),
  ess = sapply(h1_rev_results, `[[`, "ess")
)
print(h1_rev_diag)

# --- プール ---
h1_rev_fits  <- lapply(h1_rev_results, `[[`, "fit")
pooled_h1_rev <- pool(h1_rev_fits)
print(summary(pooled_h1_rev, conf.int = TRUE))

# --- 表・CSV（本番 pool_to_table と同じ要領）---
pool_to_table <- function(pooled, model_name = "model") {
  s <- summary(pooled, conf.int = TRUE)
  term_col <- if ("term" %in% names(s)) s$term else rownames(s)
  data.frame(
    model     = model_name,
    term      = term_col,
    estimate  = s$estimate,
    std.error = s$std.error,
    statistic = s$statistic,
    df        = s$df,
    p.value   = s$p.value,
    ci_low    = s$`2.5 %`,
    ci_high   = s$`97.5 %`,
    conf.low  = s$conf.low,
    conf.high = s$conf.high,
    row.names = NULL
  )
}

add_stars <- function(p) {
  ifelse(p < 0.001, "***",
         ifelse(p < 0.01, "**",
                ifelse(p < 0.05, "*", "")))
}

tab_h1_rev <- pool_to_table(pooled_h1_rev, "H1_MSM_revenue") %>%
  mutate(
    sig    = add_stars(p.value),
    est_ci = sprintf("%.3f (%.3f, %.3f)", estimate, conf.low, conf.high)
  )

write.csv(tab_h1_rev, "table_H1_pooled_revenue.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

tab_h1_rev_grant <- tab_h1_rev %>%
  filter(grepl("^grant_bin_l1", term)) %>%
  mutate(quintile = paste0("Q", sub("grant_bin_l1", "", term))) %>%
  select(quintile, estimate, std.error, conf.low, conf.high, p.value, sig, est_ci)

write.csv(tab_h1_rev_grant, "table_H1_grant_main_revenue.csv",
          row.names = FALSE, fileEncoding = "UTF-8")

# --- 仮説1のざっくり判断（Q2–Q5 がすべて非有意か）---
tab_h1_rev_grant %>%
  mutate(reject_H1 = p.value < 0.05)