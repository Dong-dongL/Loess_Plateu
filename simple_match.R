# =============================================================================
#  simple_match.R —— 极简版：按原始学名精确匹配，导出每个种的省级分布
#
#  规则：学名只统一大小写和首尾空格，其余原样比较；不做任何归并、不丢任何行。
#  名单怎么进就怎么出，数量自洽。
#
#  用法：
#    Rscript simple_match.R 名单.xlsx 分布表.xlsx
#    （不传参数则用下面的默认路径）
#  输出：省级分布_匹配结果.xlsx（4 个 sheet）
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readxl); library(readr)
  library(stringr); library(writexl)
})

LIST_FILE <- "demo/名录.xlsx"   # <- 名单（Excel/CSV）
DIST_FILE <- "demo/分布表.xlsx" # <- 分布数据表（含 Province 列）
OUT_FILE  <- "省级分布_匹配结果.xlsx"

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

read_tbl <- function(path) {
  if (!file.exists(path)) stop("找不到文件：", path)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    df <- read_excel(path, .name_repair = "minimal")
  } else {
    raw <- readBin(path, "raw", min(file.size(path), 2e5))
    enc <- if (!length(raw) || all(raw < 0x80) || isTRUE(validUTF8(raw))) "UTF-8" else "GBK"
    df <- suppressWarnings(read_delim(path, delim = if (ext == "tsv") "\t" else ",",
                                      show_col_types = FALSE,
                                      locale = locale(encoding = enc),
                                      name_repair = "minimal"))
  }
  names(df) <- trimws(names(df))
  df
}

find_col <- function(nms, pats) {
  for (p in pats) {
    hit <- nms[str_detect(nms, regex(p, ignore_case = TRUE))]
    if (length(hit)) return(hit[1])
  }
  NULL
}

SP_PAT   <- c("^学名$", "拉丁", "学名", "^species$", "species", "^taxon",
              "^scientific", "^latin")
PROV_PAT <- c("^province$", "province", "^省$", "省份", "region", "state")

# ---- 主流程 ----------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1) LIST_FILE <- args[1]
if (length(args) >= 2) DIST_FILE <- args[2]

L <- read_tbl(LIST_FILE)
D <- read_tbl(DIST_FILE)

lc <- find_col(names(L), SP_PAT)
dc <- find_col(names(D), SP_PAT)
pc <- find_col(names(D), PROV_PAT)
if (is.null(lc)) stop("名单没找到学名列（现有：", paste(names(L), collapse = " | "), "）")
if (is.null(dc)) stop("分布表没找到学名列（现有：", paste(names(D), collapse = " | "), "）")
if (is.null(pc)) stop("分布表没找到省份列（现有：", paste(names(D), collapse = " | "), "）")

L <- L %>% mutate(key = str_to_lower(str_squish(as.character(.data[[lc]]))))
D <- D %>% mutate(key = str_to_lower(str_squish(as.character(.data[[dc]]))),
                  province = as.character(.data[[pc]]))

# 分布表按学名汇总（只汇总名单里用到的学名）
dist_sum <- D %>%
  filter(!is.na(key), !is.na(province), nzchar(province)) %>%
  semi_join(L %>% filter(!is.na(key)), by = "key") %>%
  group_by(key) %>%
  summarise(`记录数`   = n(),
            `省份数`   = n_distinct(province),
            `省份列表` = paste(sort(unique(province)), collapse = "、"),
            .groups = "drop")

# 1) 匹配结果：名单原有所有列 + 省级分布（匹配不上的省份数为 0）
res <- L %>%
  left_join(dist_sum, by = "key") %>%
  mutate(`记录数`   = ifelse(is.na(`记录数`), 0L, `记录数`),
         `省份数`   = ifelse(is.na(`省份数`), 0L, `省份数`),
         `省份列表` = ifelse(is.na(`省份列表`), "", `省份列表`)) %>%
  select(-key)

# 2) 分布长表：名单中每个种的 学名-省份-记录数
long <- D %>%
  filter(!is.na(key), !is.na(province), nzchar(province)) %>%
  semi_join(L %>% filter(!is.na(key)), by = "key") %>%
  count(key, province, name = "记录数") %>%
  select(`学名` = key, `省份` = province, `记录数`)

# 3) 名单中没匹配上的种
no_match <- res %>% filter(`省份数` == 0)

# 4) 分布表里没被名单用到的种
unused <- D %>%
  filter(!is.na(key), nzchar(key), !key %in% (L$key %||% character(0))) %>%
  count(key, name = "记录数") %>%
  select(`学名` = key, `记录数`) %>%
  arrange(desc(`记录数`))

# ---- 统计（原样数字，不做任何归并）------------------------------------------
n_list <- n_distinct(L$key[!is.na(L$key)])
n_dist <- n_distinct(D$key[!is.na(D$key)])
n_hit  <- n_distinct(dist_sum$key)
cat(sprintf("名单不重复学名：%d\n分布表不重复学名：%d\n匹配上：%d｜名单未匹配：%d｜分布表未用到：%d\n",
            n_list, n_dist, n_hit, n_list - n_hit, n_dist - n_hit))

tryCatch(
  write_xlsx(list("01_匹配结果" = res, "02_分布长表" = long,
                  "03_名单未匹配" = no_match, "04_分布表未用物种" = unused), OUT_FILE),
  error = function(e) {
    alt <- sprintf("%s_%s.xlsx", tools::file_path_sans_ext(OUT_FILE),
                   format(Sys.time(), "%Y%m%d_%H%M"))
    message("提示：", OUT_FILE, " 被占用，改存为 ", alt)
    write_xlsx(list("01_匹配结果" = res, "02_分布长表" = long,
                    "03_名单未匹配" = no_match, "04_分布表未用物种" = unused), alt)
  })
cat("已导出：", normalizePath(OUT_FILE, mustWork = FALSE), "\n")
