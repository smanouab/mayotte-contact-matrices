#!/usr/bin/env Rscript
# Reproduce the MayCov V12 analyses from the three original workbooks.
args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 3L || length(args) > 6L) {
  stop("Usage: Rscript reproduce_maycov.R SURVEY.xlsx CENSUS.xlsx CASES.xlsx [OUTPUT] [B] [SIMULATIONS]")
}
inputs <- normalizePath(args[1:3], mustWork = TRUE)
output <- if (length(args) >= 4L) args[4] else "results"
params <- list(B = if (length(args) >= 5L) as.integer(args[5]) else 1000L,
               R_sim = if (length(args) >= 6L) as.integer(args[6]) else 300L,
               seed = 2021L, use_cache = FALSE)
stopifnot(params$B >= 2L, params$R_sim >= 2L)
packages <- c("readxl", "dplyr", "tidyr", "ggplot2", "survey", "patchwork", "numDeriv", "scales")
missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop("Missing R packages: ", paste(missing, collapse = ", "))
dir.create(output, recursive = TRUE, showWarnings = FALSE)
setwd(output)


# --- packages ---
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(tidyr); library(ggplot2)
  library(survey); library(patchwork); library(numDeriv); library(parallel)
})
options(survey.lonely.psu = "adjust", dplyr.summarise.inform = FALSE,
        stringsAsFactors = FALSE)
for (dd in c("figures", "tables", "cache")) dir.create(dd, showWarnings = FALSE)
RNGkind("L'Ecuyer-CMRG"); set.seed(params$seed)
n_cores <- 1L  # portable and reproducible across operating systems

## colours (colour-blind-safe categorical slots, one-hue sequential ramp)
pal  <- c(blue = "#2a78d6", orange = "#eb6834", aqua = "#1baf7a", violet = "#6250d6")
ink  <- "#0b0b0b"; ink2 <- "#52514e"; grid_col <- "#e6e5e1"
seq_blue <- c("#f4f8fe", "#cde2fb", "#86b6ef", "#3987e5", "#184f95", "#0d366b")
theme_paper <- theme_minimal(base_size = 9) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = grid_col, linewidth = 0.3),
        text = element_text(colour = ink), axis.text = element_text(colour = ink2),
        plot.title = element_text(size = 9, face = "bold"),
        plot.title.position = "plot", legend.position = "bottom",
        legend.title = element_blank(), legend.key.height = unit(3, "mm"),
        legend.margin = margin(0, 0, 0, 0), strip.text = element_text(face = "bold"))
save_fig <- function(p, name, w = 7.2, h = 3.4) {
  ggsave(file.path("figures", paste0(name, ".pdf")), p, width = w, height = h,
         device = if (capabilities("cairo")) grDevices::cairo_pdf else grDevices::pdf)
  ggsave(file.path("figures", paste0(name, ".png")), p, width = w, height = h, dpi = 300)
  invisible(p)
}

## helpers: number formatting, LaTeX macros for in-text numbers, LaTeX tables
fmt  <- function(x, d = 1) formatC(ifelse(round(x, d) == 0, 0, x), format = "f", digits = d, big.mark = ",")
fint <- function(x) formatC(round(x), format = "d", big.mark = ",")
ci_txt <- function(lo, hi, d = 1) paste0(fmt(lo, d), "--", fmt(hi, d))
NUM <- list()
num <- function(name, value) {
  stopifnot(grepl("^[A-Za-z]+$", name)); NUM[[name]] <<- as.character(value); invisible(value)
}
tex_table <- function(df, file, align = NULL, addspace = integer(0)) {
  esc <- function(x) gsub("(?<!\\\\)%", "\\\\%", gsub("(?<!\\\\)&", "\\\\&", as.character(x), perl = TRUE), perl = TRUE)
  if (is.null(align)) align <- paste0("l", strrep("r", ncol(df) - 1))
  write.csv(df, file.path("tables", sub("\\.tex$", ".csv", file)),
            row.names = FALSE, fileEncoding = "UTF-8")
  body <- apply(df, 1, function(r) paste(esc(ifelse(is.na(r), "", r)), collapse = " & "))
  lines <- c(paste0("\\begin{tabular}{", align, "}"), "\\toprule",
             paste0(paste(esc(names(df)), collapse = " & "), " \\\\"), "\\midrule")
  for (i in seq_along(body)) {
    if (i %in% addspace) lines <- c(lines, "\\addlinespace")
    lines <- c(lines, paste0(body[i], " \\\\"))
  }
  writeLines(c(lines, "\\bottomrule", "\\end{tabular}"), file.path("tables", file))
}
wilson <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2); p <- x / n
  ctr <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  hw <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  cbind(p = p, lo = ctr - hw, hi = ctr + hw)
}


# --- data ---
raw <- as.data.frame(readxl::read_excel(inputs[1]))

cvar <- c("Contact_moins18", "Contact_19_24", "Contact_25_49", "Contact_50_64", "Contact_65plus")
hvar <- c("nb_pers_menage_moins18", "nb_pers_menage_19_24", "nb_pers_menage_25_49",
          "nb_pers_menage_50_64", "nb_pers_menage_65plus")
grp  <- c("0-17", "18-24", "25-49", "50-64", "65+")

d <- data.frame(id = raw$NID, hh = sub("-.*", "", raw$NID), w = raw$Poids, age = raw$age)
d$adultq <- raw$Contact_moins18 != 9997                 # adult questionnaire (contact module asked)
d$female <- as.integer(raw$Sexe %in% c("Femme", "Fille"))
d$agp  <- cut(d$age, c(0, 18, 25, 50, 65, Inf), right = FALSE, labels = grp)
d$age9 <- factor(raw$cate_age, levels = c("[6-9]", "[10-14]", "[15-24]", "[25-34]", "[35-44]",
                                          "[45-54]", "[55-64]", "[65-74]", "75+"),
                 labels = c("6-9", "10-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))
d$age6 <- cut(d$age, c(0, 10, 15, 25, 45, 65, Inf), right = FALSE,
              labels = c("6-9", "10-14", "15-24", "25-44", "45-64", "65+"))
d$nat3 <- factor(ifelse(raw$Nationalite == "Fran\u00e7aise", "French",
                 ifelse(raw$Nationalite %in% c("Comorienne", "Anjouanaise", "Moh\u00e9lienne"),
                        "Comorian", "Other or unknown")),
                 levels = c("French", "Comorian", "Other or unknown"))
d$housing <- factor(ifelse(raw$Bati == "En dur", "Permanent",
                    ifelse(raw$Bati %in% c("En t\u00f4le", "Bois, v\u00e9g\u00e9tale"), "Sheet metal or wood", NA)),
                    levels = c("Permanent", "Sheet metal or wood"))
d$water <- factor(ifelse(raw$acces_eau == "A l'int\u00e9rieur", "Inside dwelling",
                  ifelse(raw$acces_eau %in% c("A l'ext\u00e9rieur", "Hors du foyer"), "Outside dwelling", NA)),
                  levels = c("Inside dwelling", "Outside dwelling"))
d$employ <- factor(ifelse(raw$emploi == "Oui", "Employed",
                   ifelse(raw$emploi == "Non", "Not employed",
                   ifelse(raw$emploi == "Etudiant", "Student", NA))),
                   levels = c("Not employed", "Employed", "Student"))
d$employ[!d$adultq] <- NA

## contacts outside the household over the two previous days, by age group of the contact
C <- as.matrix(raw[, cvar]); C[C %in% c(9997, 9999)] <- NA
d[, cvar] <- C
d$cc_complete <- d$adultq & rowSums(is.na(C)) == 0
d$tot  <- rowSums(C)
d$heap <- ifelse(d$cc_complete, apply(C, 1, function(v) any(v >= 100)), NA)

## household composition (other members, by age group) and household size
H <- as.matrix(raw[, hvar]); H[H >= 997 | H > 30] <- NA
d[, hvar] <- H
d$hhsize <- ifelse(raw$taille_menage >= 997 | raw$taille_menage > 30, NA, raw$taille_menage)

## serology (Eurofins Biomnis): total Ig and IgG against SARS-CoV-2, dengue IgG
ser <- function(x) ifelse(x == 996, NA, x)
d$igt <- ser(raw$IgTotal_cov19); d$igg <- ser(raw$IgG_cov19); d$deng <- ser(raw$IgG_deng)
d$vacc <- ifelse(grepl("^Oui", raw$vaccin), 1L, ifelse(raw$vaccin %in% c("Non", "Anti vaccin"), 0L, NA))

## interview date, survey week and number of weekend days in the two-day recall window
d$date <- as.Date(raw$DDE); d$date[d$date > as.Date("2021-12-31")] <- NA
d$week <- as.Date(cut(d$date, "week"))                    # Monday of the interview week
wd <- as.POSIXlt(d$date)$wday                             # 0 = Sunday
d$wkend <- factor(ifelse(wd == 1, 2, ifelse(wd %in% c(0, 2), 1, 0)), levels = 0:2,
                  labels = c("None", "One", "Two"))
d$period <- cut(as.numeric(d$date), as.numeric(as.Date(c("2021-08-01", "2021-09-20", "2021-10-11", "2022-01-01"))),
                right = FALSE, labels = c("1-19 Sept", "20 Sept-10 Oct", "From 11 Oct"))

## household-level covariates: taken from the (first) adult respondent of the household,
## which also gives them to children interviewed with the child questionnaire
ref <- d[d$adultq, ] %>% group_by(hh) %>% slice(1) %>% ungroup() %>%
  transmute(hh, ref_id = id, housing_h = housing, water_h = water, hhsize_h = hhsize, ref_agp = agp,
            r1 = .data[[hvar[1]]], r2 = .data[[hvar[2]]], r3 = .data[[hvar[3]]],
            r4 = .data[[hvar[4]]], r5 = .data[[hvar[5]]])
d <- left_join(d, ref, by = "hh")
d$hhcat_h <- cut(d$hhsize_h, c(0, 2, 4, 6, Inf), labels = c("1-2", "3-4", "5-6", "7 or more"))

## household composition seen from each respondent (other members by age group):
## own answer for adult-questionnaire respondents; for children, the composition reported by
## the linked adult, plus that adult, minus the child itself
Hl <- as.matrix(d[, hvar])
kid <- !d$adultq & !is.na(d$ref_id)
Rk <- as.matrix(d[kid, c("r1", "r2", "r3", "r4", "r5")])
Rk[cbind(seq_len(nrow(Rk)), as.integer(d$ref_agp[kid]))] <-
  Rk[cbind(seq_len(nrow(Rk)), as.integer(d$ref_agp[kid]))] + 1
Rk[, 1] <- pmax(Rk[, 1] - 1, 0)
Hl[kid, ] <- Rk
hl <- paste0("h", 1:5); d[, hl] <- Hl
d$h_complete <- rowSums(is.na(Hl)) == 0

## population by contact age group (INSEE census 2017, five-year bands; the 15-19 band is
## split 3/5 - 2/5 between 0-17 and 18-24)


# --- data ---
ins <- read_excel(inputs[2],
                                sheet = 1, col_names = FALSE, .name_repair = "minimal")
pop5 <- as.numeric(unlist(ins[which(ins[[1]] == "Ensemble"), 3:21], use.names = FALSE))
N <- c(sum(pop5[1:3]) + 0.6 * pop5[4], 0.4 * pop5[4] + pop5[5], sum(pop5[6:10]),
       sum(pop5[11:13]), sum(pop5[14:19]))
names(N) <- grp
stopifnot(abs(sum(N) - 256518) < 5)        # census total (age bands are rounded estimates)
round(N); round(100 * N / sum(N), 1)

A <- d[d$cc_complete, ]                                   # adult respondents with complete contact answers
core <- A[!A$heap, ]

num("nTotal", fint(nrow(d))); num("nHouseholds", fint(n_distinct(d$hh)))
num("nAdultQ", fint(sum(d$adultq))); num("nChildQ", fint(sum(!d$adultq)))
num("nMultiHH", fint(sum(table(d$hh) >= 2)))
num("nContactComplete", fint(nrow(A)))
num("nContactIncomplete", fint(sum(d$adultq) - nrow(A)))
num("dateMin", trimws(format(min(d$date, na.rm = TRUE), "%e %B %Y"))); num("dateMax", trimws(format(max(d$date, na.rm = TRUE), "%e %B %Y")))
num("nAfterOct", sum(d$date > as.Date("2021-10-31"), na.rm = TRUE))
num("pctBeforeMidOct", fmt(100 * mean(d$date < as.Date("2021-10-16"), na.rm = TRUE), 1))
num("nDateOdd", sum(is.na(d$date)))
num("popTotal", fint(sum(N))); num("popUnderEighteenPct", fmt(100 * N[1] / sum(N), 1))
num("popSixtyFivePct", fmt(100 * N[5] / sum(N), 1))
num("wSum", fint(sum(d$w)))
num("nSero", fint(sum(!is.na(d$igt)))); num("pctSero", fmt(100 * mean(!is.na(d$igt)), 1))
num("nKidsLinked", fint(sum(kid))); num("nKidsAll", fint(sum(!d$adultq)))


# --- table1 ---
des_all <- svydesign(ids = ~hh, weights = ~w, data = d)
t1_block <- function(label, f, sub = rep(TRUE, nrow(d))) {
  f <- if (is.factor(f)) droplevels(f) else factor(f, levels = unique(f)); out <- NULL
  for (l in levels(f)) {
    k <- sub & !is.na(f)
    out <- rbind(out, data.frame(Characteristic = paste0("\\quad ", l),
      n = fint(sum(f[k] == l)), pct = fmt(100 * mean(f[k] == l), 1),
      wpct = fmt(100 * sum(d$w[k & f == l]) / sum(d$w[k]), 1)))
  }
  rbind(data.frame(Characteristic = label, n = "", pct = "", wpct = ""), out)
}
t1 <- rbind(
  t1_block("Questionnaire", factor(ifelse(d$adultq, "Adult", "Child"), levels = c("Adult", "Child"))),
  t1_block("Sex", factor(ifelse(d$female == 1, "Female", "Male"), levels = c("Female", "Male"))),
  t1_block("Age (years)", factor(d$age9, labels = gsub("-", "--", levels(d$age9)))),
  t1_block("Nationality", d$nat3),
  t1_block("Housing (adult respondents)", d$housing, d$adultq),
  t1_block("Water access (adult respondents)", d$water, d$adultq),
  t1_block("Employment (adult respondents)", d$employ, d$adultq),
  t1_block("Household size (adult respondents)",
           cut(d$hhsize, c(0, 2, 4, 6, Inf), labels = c("1--2", "3--4", "5--6", "7 or more")), d$adultq),
  t1_block("Blood sample analysed", factor(ifelse(!is.na(d$igt), "Yes", "No"), levels = c("Yes", "No"))))
names(t1) <- c("Characteristic", "$n$", "\\%", "Weighted \\%")
tex_table(t1, "table_01.tex", align = "lrrr",
          addspace = which(t1[[2]] == "")[-1])
t1
num("hhsizeMedian", fmt(median(d$hhsize[d$adultq], na.rm = TRUE), 0))
num("hhsizeMean", fmt(weighted.mean(d$hhsize[d$adultq], d$w[d$adultq], na.rm = TRUE), 1))
num("pctFemaleAdults", fmt(100 * mean(d$female[d$adultq]), 1))


# --- heaping ---
ans <- as.vector(as.matrix(A[, cvar]))                    # 5 answers per respondent
ans_w <- rep(A$w, 5)
straight <- apply(A[, cvar], 1, function(v) length(unique(v)) == 1 && v[1] >= 100)
n_ge100 <- apply(A[, cvar], 1, function(v) sum(v >= 100))
heap_week <- A %>% filter(!is.na(week)) %>% group_by(week) %>%
  summarise(n = n(), heaped = sum(heap), core_mean = weighted.mean(tot[!heap], w[!heap]),
            core_zero = mean(tot[!heap] == 0)) %>% filter(n >= 30)
heap_week <- cbind(heap_week, wilson(heap_week$heaped, heap_week$n))
heap_week

num("nAnswers", fint(length(ans)))
num("pctAnswersZero", fmt(100 * mean(ans == 0), 1))
num("pctTotalZero", fmt(100 * mean(A$tot == 0), 1))
num("nAnswersHundred", fint(sum(ans == 100)))
num("pctAnswersHundred", fmt(100 * mean(ans == 100), 1))
num("nAnswersGeHundred", fint(sum(ans >= 100)))
num("pctAnswersGeHundred", fmt(100 * mean(ans >= 100), 1))
num("pctGeHundredMultiple", fmt(100 * mean(ans[ans >= 100] %% 100 == 0), 1))
num("nAnswersFiftyOneNinetyNine", fint(sum(ans > 50 & ans < 100)))
num("pctAnswersFiftyOneNinetyNine", fmt(100 * mean(ans > 50 & ans < 100), 2))
num("nHeaped", fint(sum(A$heap))); num("pctHeaped", fmt(100 * mean(A$heap), 1))
num("nCore", fint(nrow(core)))
num("nAllFiveHeaped", fint(sum(n_ge100 == 5)))
num("pctAllFiveAmongHeaped", fmt(100 * sum(n_ge100 == 5) / sum(A$heap), 1))
num("nStraight", fint(sum(straight)))
num("heapWeekMin", fmt(100 * min(heap_week$p), 1)); num("heapWeekMax", fmt(100 * max(heap_week$p), 1))
num("rawMeanTotal", fmt(weighted.mean(A$tot, A$w), 1)); num("rawMedianTotal", fmt(median(A$tot), 0))
num("rawVarTotal", fint(var(A$tot))); num("rawMaxTotal", fint(max(A$tot)))
num("coreMeanTotal", fmt(weighted.mean(core$tot, core$w), 1))
num("coreMedianTotal", fmt(median(core$tot), 0))
num("coreVarTotal", fmt(var(core$tot), 0)); num("coreMaxTotal", fint(max(core$tot)))
num("corePctZero", fmt(100 * mean(core$tot == 0), 1))
num("coreMaxAnswer", fint(max(as.matrix(core[, cvar]))))



# --- fig1 ---
brk <- c(-1, 0, 1, 2, 4, 9, 19, 49, 99, 100, 5000)
lab <- c("0", "1", "2", "3-4", "5-9", "10-19", "20-49", "50-99", "100", ">100")
f1a <- data.frame(bin = cut(ans, brk, labels = lab)) %>% count(bin) %>%
  mutate(share = 100 * n / sum(n),
         kind = ifelse(bin %in% c("100", ">100"), "Heaped answers (100 or more)", "Core range (0-99)"))
p1a <- ggplot(f1a, aes(bin, share, fill = kind)) + geom_col(width = 0.8) +
  geom_text(aes(label = fmt(share, 1)), vjust = -0.4, size = 2.5, colour = ink2) +
  scale_fill_manual(values = c("Core range (0-99)" = unname(pal["blue"]),
                               "Heaped answers (100 or more)" = unname(pal["orange"]))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(title = "a  Reported number of contacts per age group", x = "Answer (persons, two days)",
       y = "Share of answers (%)") + theme_paper
p1b <- ggplot(heap_week, aes(week, 100 * p)) +
  geom_linerange(aes(ymin = 100 * lo, ymax = 100 * hi), colour = pal["orange"], linewidth = 0.6) +
  geom_point(colour = pal["orange"], size = 2) +
  geom_text(aes(y = 100 * hi, label = paste0("n=", n)), vjust = -0.6, size = 2.3, colour = ink2) +
  scale_x_date(date_breaks = "1 week", date_labels = "%d %b") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
  labs(title = "b  Respondents with at least one answer of 100 or more",
       x = "Interview week (Monday), 2021", y = "Share of respondents (%)") +
  theme_paper + theme(axis.text.x = element_text(angle = 30, hjust = 1))
fig1 <- p1a + p1b + plot_layout(widths = c(1.15, 1)) & theme(legend.position = "bottom")
save_fig(fig1, "figure_01_heaping", h = 3.3)
fig1


# --- estimators ---
log_bell_numbers <- function(nmax, kmax = max(400, 2 * nmax)) {   # Dobinski's formula, log scale
  k <- 1:kmax; lk <- log(k); lf <- lgamma(k + 1); out <- numeric(nmax + 1)
  for (n in 1:nmax) { v <- n * lk - lf; m <- max(v); out[n + 1] <- -1 + m + log(sum(exp(v - m))) }
  out
}
lambertW0 <- function(x) {                                       # principal branch, Halley iterations
  w <- ifelse(x < 1, x, log(x) - log(log(x) + 1e-12) * (x > exp(1)))
  w[!is.finite(w) | w < 0] <- 0.5
  for (i in 1:60) { ew <- exp(w); f <- w * ew - x; w <- w - f / (ew * (w + 1) - (w + 2) * f / (2 * w + 2)) }
  w
}
GRID_MAX <- 3000
logB <- log_bell_numbers(GRID_MAX)
stopifnot(abs(exp(logB[11]) - 115975) < 1e-3, abs(lambertW0(1) - 0.5671433) < 1e-6)

dzi <- function(y, family, pi0, mu, theta = NULL) {
  g <- switch(family,
    pois = dpois(y, mu),
    nb   = dnbinom(y, size = theta, mu = mu),
    bell = { th <- lambertW0(mu); exp(y * log(th) + 1 - exp(th) + logB[y + 1] - lgamma(y + 1)) })
  f <- (1 - pi0) * g; f[y == 0] <- f[y == 0] + pi0; f
}
unpack <- function(par, family, zi) {
  i <- 1; pi0 <- 0
  if (zi) { pi0 <- plogis(par[i]); i <- i + 1 }
  mu <- exp(par[i]); theta <- if (family == "nb") exp(par[i + 1]) else NULL
  list(pi0 = pi0, mu = mu, theta = theta)
}
objective <- function(par, yu, wu, family, zi, alpha, type, grid) {
  p <- unpack(par, family, zi)
  fy <- dzi(yu, family, p$pi0, p$mu, p$theta)
  if (alpha == 0) return(-sum(wu * log(pmax(fy, 1e-300))))
  fg <- dzi(grid, family, p$pi0, p$mu, p$theta)
  if (type == "dpd") return(sum(fg^(1 + alpha)) - (1 + 1 / alpha) * sum(wu * fy^alpha))
  -log(max(sum(wu * fy^alpha), 1e-300)) / alpha + log(sum(fg^(1 + alpha))) / (1 + alpha)
}
fit_count <- function(y, w = rep(1, length(y)), family = "nb", zi = TRUE, alpha = 0,
                      type = "dpd", grid_max = GRID_MAX, nstart = 4) {
  ok <- !is.na(y) & !is.na(w); y <- y[ok]; w <- w[ok]
  tab <- tapply(w, y, sum); yu <- as.numeric(names(tab)); wu <- as.numeric(tab) / sum(w)
  yc <- y[y < 100]; m0 <- max(mean(yc), 0.2); p0 <- min(max(mean(y == 0) - 0.15, 0.05), 0.9)
  st <- list(c(qlogis(p0), log(m0 / (1 - p0)), log(0.7)), c(qlogis(0.05), log(m0), log(0.3)),
             c(qlogis(0.5), log(2 * m0), log(1.5)), c(qlogis(p0), log(max(median(c(1, y[y > 0])), 1)), 0))
  sel <- c(zi, TRUE, family == "nb")
  lower <- c(-12, -4, -4)[sel]; upper <- c(8, 7.5, 4)[sel]
  best <- NULL
  for (s in st[seq_len(nstart)]) {
    o <- try(optim(s[sel], objective, yu = yu, wu = wu, family = family, zi = zi, alpha = alpha,
                   type = type, grid = 0:grid_max, method = "L-BFGS-B",
                   lower = lower, upper = upper, control = list(maxit = 500)), silent = TRUE)
    if (!inherits(o, "try-error") && (is.null(best) || o$value < best$value)) best <- o
  }
  p <- unpack(best$par, family, zi)
  c(pi0 = p$pi0, mu = p$mu, theta = if (is.null(p$theta)) NA else p$theta,
    mean = (1 - p$pi0) * p$mu, objective = best$value, n = length(y), npar = sum(sel))
}


# --- model_comparison ---
ycore <- core$tot
fam_tab <- NULL
for (fam in list(c("pois", FALSE, "Poisson"), c("pois", TRUE, "Zero-inflated Poisson"),
                 c("bell", FALSE, "Bell"), c("bell", TRUE, "Zero-inflated Bell"),
                 c("nb", FALSE, "Negative binomial"), c("nb", TRUE, "Zero-inflated negative binomial"))) {
  r <- fit_count(ycore, family = fam[1], zi = as.logical(fam[2]))
  ll <- -r["objective"] * length(ycore)
  f0 <- dzi(0:GRID_MAX, fam[1], r["pi0"], r["mu"], if (fam[1] == "nb") r["theta"] else NULL)
  fam_tab <- rbind(fam_tab, data.frame(Model = fam[3], k = r["npar"], logLik = ll,
    AIC = -2 * ll + 2 * r["npar"], p0 = 100 * f0[1], p50 = 100 * sum(f0[52:(GRID_MAX + 1)]),
    var = sum((0:GRID_MAX)^2 * f0) - sum((0:GRID_MAX) * f0)^2))
}
rownames(fam_tab) <- NULL
fam_tab
ts1 <- with(fam_tab, data.frame(Model, k = k, logLik = sub("^-", "$-$", fmt(logLik, 1)), AIC = fmt(AIC, 1),
            p0 = fmt(p0, 1), p50 = fmt(p50, 1), var = fmt(var, 0)))
ts1 <- rbind(ts1, data.frame(Model = "Observed", k = "", logLik = "", AIC = "",
            p0 = fmt(100 * mean(ycore == 0), 1), p50 = fmt(100 * mean(ycore > 50), 1), var = fmt(var(ycore), 0)))
names(ts1) <- c("Model", "$k$", "Log-lik.", "AIC", "$P(Y=0)$, \\%", "$P(Y>50)$, \\%", "Variance")
tex_table(ts1, "table_09.tex", align = "lrrrrrr", addspace = 7)
num("aicNB", fmt(fam_tab$AIC[5], 0)); num("aicZINB", fmt(fam_tab$AIC[6], 0))
num("aicZIBell", fmt(fam_tab$AIC[4], 0)); num("aicZIP", fmt(fam_tab$AIC[2], 0)); num("aicPois", fmt(fam_tab$AIC[1], 0))
num("varZIBell", fmt(fam_tab$var[4], 0)); num("varZINB", fmt(fam_tab$var[6], 0))
zinb_core <- fit_count(ycore, family = "nb", zi = TRUE)
num("zinbCorePi", fmt(zinb_core["pi0"], 2)); num("zinbCoreTheta", fmt(zinb_core["theta"], 2))
num("zinbCoreMu", fmt(zinb_core["mu"], 1))


# --- threshold_path ---
thr <- c(10, 15, 20, 25, 30, 40, 50, 60, 75, 90, 100, 101, 150, 200, 300, 500, 1000)
thr_path <- do.call(rbind, lapply(thr, function(cc)
  data.frame(c = cc, trimmed = weighted.mean(ans[ans < cc], ans_w[ans < cc]), n = sum(ans < cc))))
thr_path


# --- alpha_path ---
alphas <- c(0, 0.05, 0.1, 0.15, 0.2, 0.3, 0.4, 0.5, 0.6, 0.75, 1)
cache_f <- "cache/alpha_path.rds"
if (params$use_cache && file.exists(cache_f)) alpha_path <- readRDS(cache_f) else {
  alpha_path <- do.call(rbind, lapply(alphas, function(a) {
    r1 <- fit_count(ans, ans_w, alpha = a, type = "dpd", grid_max = 6000)
    r2 <- fit_count(ans, ans_w, alpha = a, type = "gamma", grid_max = 6000)
    data.frame(alpha = a, MDPDE = unname(r1["mean"]), Gamma = unname(r2["mean"]),
               MDPDE_pi = unname(r1["pi0"]), Gamma_pi = unname(r2["pi0"]))
  }))
  saveRDS(alpha_path, cache_f)
}
alpha_path
core_ans <- as.vector(as.matrix(core[, cvar])); core_ans_w <- rep(core$w, 5)
core_mean_ans <- weighted.mean(core_ans, core_ans_w)      # heap-mixture estimate (core respondents)



# --- fig2 ---
p2a <- ggplot(thr_path, aes(c, trimmed)) +
  geom_vline(xintercept = 100, linetype = "dashed", colour = ink2, linewidth = 0.3) +
  geom_line(linewidth = 0.7, colour = pal["orange"]) + geom_point(size = 1.8, colour = pal["orange"]) +
  annotate("text", x = 92, y = 20, label = "value 100\nadmitted", hjust = 1, size = 2.5, colour = ink2) +
  scale_x_log10(breaks = c(10, 20, 50, 100, 200, 500, 1000)) + scale_y_log10(breaks = c(1, 2, 3, 5, 10, 20, 30)) +
  labs(title = "a  Trimmed mean by truncation threshold", x = "Threshold c (answers of c or more excluded)",
       y = "Mean answer (persons, two days)") + theme_paper
f2b <- pivot_longer(alpha_path[, c("alpha", "MDPDE", "Gamma")], -alpha) %>%
  mutate(name = ifelse(name == "MDPDE", "Density power divergence (MDPDE)", "Gamma-divergence"))
p2b <- ggplot(f2b, aes(alpha, value, colour = name, shape = name)) +
  geom_hline(yintercept = core_mean_ans, linetype = "dashed", colour = ink2, linewidth = 0.3) +
  annotate("text", x = 0, y = core_mean_ans, label = "core respondents (heap-mixture)", hjust = 0,
           vjust = -0.6, size = 2.5, colour = ink2) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.8) +
  scale_y_log10(breaks = c(1, 2, 3, 5, 10, 20, 30)) +
  scale_colour_manual(values = c("Density power divergence (MDPDE)" = unname(pal["aqua"]),
                                 "Gamma-divergence" = unname(pal["violet"]))) +
  labs(title = "b  Divergence-based ZINB mean by tuning constant",
       x = expression(paste("Tuning constant ", alpha, " (0 = maximum likelihood)")),
       y = "Mean answer (persons, two days)") + theme_paper
fig2 <- p2a + p2b
save_fig(fig2, "figure_02_sensitivity", h = 3.2)
fig2

tm <- function(cc) thr_path$trimmed[thr_path$c == cc]
num("rawMeanAnswer", fmt(weighted.mean(ans, ans_w), 1))
num("trimTwenty", fmt(tm(20), 2)); num("trimFifty", fmt(tm(50), 2)); num("trimHundred", fmt(tm(100), 2))
num("trimHundredOne", fmt(tm(101), 2)); num("trimThousand", fmt(tm(1000), 1))
num("jumpHundred", fmt(tm(101) / tm(100), 1))
num("coreMeanAnswer", fmt(core_mean_ans, 2))
ap <- function(a, v) alpha_path[[v]][abs(alpha_path$alpha - a) < 1e-9]
num("mdpdeTenth", fmt(ap(0.1, "MDPDE"), 1)); num("mdpdeHalf", fmt(ap(0.5, "MDPDE"), 1)); num("mdpdeOne", fmt(ap(1, "MDPDE"), 1))
num("gammaTenth", fmt(ap(0.1, "Gamma"), 1)); num("gammaHalf", fmt(ap(0.5, "Gamma"), 1)); num("gammaOne", fmt(ap(1, "Gamma"), 1))


# --- simulation ---
sim_data <- function(n, eps, dgp) {
  if (dgp == "A") y <- rnbinom(n, size = 0.5, mu = 7)
  else y <- rpois(n, exp(rnorm(n, log(7) - 1.3^2 / 2, 1.3)))
  y[runif(n) < 0.3] <- 0
  h <- runif(n) < eps
  y[h] <- sample(seq(100, 900, 100), sum(h), replace = TRUE, prob = c(0.7, rep(0.3 / 8, 8)))
  y
}
sim_est <- function(y) c(
  naive  = mean(y),
  trim50 = mean(y[y < 50]),
  core   = mean(y[y < 100]),
  mdpde  = unname(fit_count(y, alpha = 0.5, type = "dpd", grid_max = 1500, nstart = 2)["mean"]),
  mdpde1 = unname(fit_count(y, alpha = 1, type = "dpd", grid_max = 1500, nstart = 2)["mean"]),
  gamma  = unname(fit_count(y, alpha = 0.5, type = "gamma", grid_max = 1500, nstart = 2)["mean"]),
  gamma1 = unname(fit_count(y, alpha = 1, type = "gamma", grid_max = 1500, nstart = 2)["mean"]))
sim_grid <- expand.grid(eps = c(0, 0.05, 0.10, 0.15), dgp = c("A", "B"), stringsAsFactors = FALSE)
cache_f <- sprintf("cache/simulation_R%d.rds", params$R_sim)
if (params$use_cache && file.exists(cache_f)) sim_res <- readRDS(cache_f) else {
  set.seed(params$seed)
  sim_res <- lapply(seq_len(nrow(sim_grid)), function(s)
    do.call(rbind, mclapply(seq_len(params$R_sim), function(r)
      sim_est(sim_data(500, sim_grid$eps[s], sim_grid$dgp[s])), mc.cores = n_cores)))
  saveRDS(sim_res, cache_f)
}
true_mean <- 0.7 * 7
sim_tab <- do.call(rbind, lapply(seq_len(nrow(sim_grid)), function(s) {
  e <- sim_res[[s]]
  data.frame(dgp = sim_grid$dgp[s], eps = sim_grid$eps[s], estimator = colnames(e),
             bias = 100 * (colMeans(e) - true_mean) / true_mean,
             rmse = sqrt(colMeans((e - true_mean)^2)))
}))


# --- Simulation summary ---
sim_wide <- sim_tab %>%
  mutate(
    cell = sub(
      "^-", "$-$",
      paste0(fmt(bias, 0), " (", fmt(rmse, 2), ")")
    )
  ) %>%
  dplyr::select(dgp, eps, estimator, cell) %>%
  tidyr::pivot_wider(
    names_from = estimator,
    values_from = cell
  )

sim_wide

ts2 <- data.frame(
  Core = ifelse(
    sim_wide$dgp == "A",
    "A: ZINB",
    "B: ZI Poisson--lognormal"
  ),
  eps = fmt(sim_wide$eps, 2),
  sim_wide[, c(
    "naive", "trim50", "core", "mdpde",
    "mdpde1", "gamma", "gamma1"
  )]
)

ts2$Core[duplicated(ts2$Core)] <- ""

names(ts2) <- c(
  "Core distribution", "$\\varepsilon$",
  "Sample mean", "Trimmed", "Heap-mixture",
  "DPD, 0.5", "DPD, 1",
  "$\\gamma$, 0.5", "$\\gamma$, 1"
)

tex_table(
  ts2, "table_10.tex",
  align = "llrrrrrrr",
  addspace = 5
)

sb <- function(dg, ep, es, what = "bias") {
  sim_tab[[what]][
    sim_tab$dgp == dg &
    abs(sim_tab$eps - ep) < 1e-9 &
    sim_tab$estimator == es
  ]
}

num("simNaiveBiasTen", fmt(sb("A", 0.10, "naive"), 0))

num("simCoreBiasTenA", fmt(sb("A", 0.10, "core"), 0))
num("simCoreBiasTenB", fmt(sb("B", 0.10, "core"), 0))

num("simTrimBiasTenA", fmt(sb("A", 0.10, "trim50"), 0))

num("simMdpdeBiasZeroA", fmt(sb("A", 0, "mdpde"), 0))
num("simMdpdeBiasTenA", fmt(sb("A", 0.10, "mdpde"), 0))
num("simMdpdeBiasFifteenA", fmt(sb("A", 0.15, "mdpde"), 0))

num("simGammaBiasTenA", fmt(sb("A", 0.10, "gamma"), 0))
num("simGammaBiasFifteenA", fmt(sb("A", 0.15, "gamma"), 0))

num("simMdpdeBiasTenB", fmt(sb("B", 0.10, "mdpde"), 0))
num("simGammaBiasTenB", fmt(sb("B", 0.10, "gamma"), 0))

num("simGammaBiasZeroB", fmt(sb("B", 0, "gamma"), 0))
num("simMdpdeBiasZeroB", fmt(sb("B", 0, "mdpde"), 0))

num("simGammaOneBiasTenA", fmt(sb("A", 0.10, "gamma1"), 0))
num("simGammaOneBiasFifteenA", fmt(sb("A", 0.15, "gamma1"), 0))

num("simGammaOneBiasZeroB", fmt(sb("B", 0, "gamma1"), 0))
num("simGammaOneBiasTenB", fmt(sb("B", 0.10, "gamma1"), 0))

num("simMdpdeOneBiasTenA", fmt(sb("A", 0.10, "mdpde1"), 0))
num("simMdpdeOneBiasFifteenA", fmt(sb("A", 0.15, "mdpde1"), 0))

num("simGammaOneRmseZeroA", fmt(sb("A", 0, "gamma1", "rmse"), 2))
num("simCoreRmseZeroA", fmt(sb("A", 0, "core", "rmse"), 2))

num("simReps", params$R_sim)


# --- matrices ---
wmean_by <- function(df, vars) {
  out <- matrix(NA_real_, 5, length(vars), dimnames = list(grp, grp))
  X <- as.matrix(df[, vars, drop = FALSE])
  for (g in grp) { k <- which(df$agp == g); if (length(k)) out[g, ] <- colSums(df$w[k] * X[k, , drop = FALSE]) / sum(df$w[k]) }
  out
}
d$maxans <- suppressWarnings(apply(as.matrix(d[, cvar]), 1, max))   # largest of the five answers
A$maxans <- apply(as.matrix(A[, cvar]), 1, max)
stopifnot(all((A$maxans >= 100) == A$heap))
thr_set <- c(5, 10, 20, 30, 50, 99, Inf)                              # respondents kept if largest answer <= c
contact_stats <- function(dd) {
  Ad <- dd[dd$cc_complete, ]
  Acap <- Ad; Acap[, cvar] <- pmin(as.matrix(Ad[, cvar]), 100)
  list(M_core = wmean_by(Ad[!Ad$heap, ], cvar), M_s50 = wmean_by(Ad[Ad$maxans <= 50, ], cvar),
       M_cap = wmean_by(Acap, cvar), M_naive = wmean_by(Ad, cvar), H = wmean_by(dd[dd$h_complete, ], hl),
       M_thr = lapply(thr_set, function(cc) wmean_by(Ad[Ad$maxans <= cc, ], cvar)))
}
adult_pairs <- t(combn(2:5, 2))
recip_ratio <- function(M) (M[adult_pairs] * N[adult_pairs[, 1]]) / (M[adult_pairs[, 2:1]] * N[adult_pairs[, 2]])
elasticity <- function(M) {
  x <- log(N[adult_pairs[, 1]] / N[adult_pairs[, 2]]); y <- log(recip_ratio(M))
  b <- sum(x * y) / sum(x^2)
  c(beta = b, r2 = 1 - sum((y - b * x)^2) / sum(y^2))
}
reconcile <- function(M, H, method = c("anchoring", "arithmetic"), omega = 0,
                      beta = unname(elasticity(M)["beta"]), kappa = 1, per_day = 0.5) {
  method <- match.arg(method)
  Tm <- M * N                                              # reported totals N_i M_ij (two days)
  if (method == "anchoring") { s <- N^beta; Tm <- Tm / outer(s^omega, s^(-(1 - omega))) }
  for (j in 2:5) Tm[1, j] <- Tm[j, 1]                      # under-18 row: adults' reports only
  Tm[1, 1] <- kappa * Tm[1, 1]                             # child-child: 15-17 year-olds' reports x kappa
  Ts <- (Tm + t(Tm)) / 2
  if (method == "anchoring") Ts <- Ts * sum((M * N)[2:5, ]) / sum(Ts[2:5, ])   # keep adults' reported total
  Hs <- (H * N + t(H * N)) / 2
  (Hs + per_day * Ts) / N                                  # daily contacts per capita
}
mat_summary <- function(Cs) {
  P <- Cs / rowSums(Cs)
  c(rho = max(Re(eigen(Cs, only.values = TRUE)$values)), Q = (sum(diag(P)) - 1) / 4,
    mean_contacts = sum(rowSums(Cs) * N) / sum(N))
}
recip_gap <- function(M) mean(abs(log(recip_ratio(M))))       # 0 under reciprocity
derived <- function(st) {
  Cref <- reconcile(st$M_core, st$H); C50 <- reconcile(st$M_s50, st$H)
  Cc <- reconcile(st$M_core, st$H, "arithmetic"); Cp <- reconcile(st$M_cap, st$H, "arithmetic")
  Cn <- reconcile(st$M_naive, st$H, "arithmetic"); Hs <- ((st$H * N + t(st$H * N)) / 2) / N
  rec <- setNames(recip_ratio(st$M_core), paste0(grp[adult_pairs[, 1]], "|", grp[adult_pairs[, 2]]))
  c(tot_core = rowSums(st$M_core), tot_cap = rowSums(st$M_cap), tot_naive = rowSums(st$M_naive),
    tot_H = rowSums(st$H), ref = mat_summary(Cref), daily_ref = rowSums(Cref),
    rho_ratio_naive = unname(mat_summary(Cn)["rho"] / mat_summary(Cc)["rho"]),
    rho_ratio_cap = unname(mat_summary(Cp)["rho"] / mat_summary(Cc)["rho"]),
    hh_share = sum(rowSums(Hs) * N) / sum(rowSums(Cref) * N),
    rec = rec, el = elasticity(st$M_core), el_naive = elasticity(st$M_naive),
    imp = st$M_core[2:5, 1] * N[2:5] / N[1], teen = st$M_core[1, 2:5],
    tot_s50 = rowSums(st$M_s50), el50 = elasticity(st$M_s50),
    rec50 = setNames(recip_ratio(st$M_s50), names(rec)),
    recn = setNames(recip_ratio(st$M_naive), names(rec)),
    gap = c(core = recip_gap(st$M_core), s50 = recip_gap(st$M_s50), naive = recip_gap(st$M_naive)),
    el_diff = unname(elasticity(st$M_s50)["beta"] - elasticity(st$M_core)["beta"]),
    gap_diff = recip_gap(st$M_s50) - recip_gap(st$M_core),
    el_path = setNames(sapply(st$M_thr, function(M) elasticity(M)["beta"]), paste0("c", thr_set)),
    ref50 = mat_summary(C50), daily_ref50 = rowSums(C50),
    hh_share50 = sum(rowSums(Hs) * N) / sum(rowSums(C50) * N),
    rho_ratio_s50 = unname(mat_summary(C50)["rho"] / mat_summary(Cref)["rho"]))
}
st0 <- contact_stats(d); der0 <- derived(st0)
lapply(st0[c("M_core", "M_s50", "M_cap", "M_naive", "H")], round, 2)

## household-cluster bootstrap (percentile intervals)
hh_ids <- unique(d$hh); idx_by_hh <- split(seq_len(nrow(d)), d$hh)
cache_f <- sprintf("cache/bootstrap_v2_B%d.rds", params$B)
if (params$use_cache && file.exists(cache_f)) boot <- readRDS(cache_f) else {
  set.seed(params$seed + 1)
  draws <- replicate(params$B, sample.int(length(hh_ids), replace = TRUE))
  boot <- do.call(rbind, mclapply(seq_len(params$B), function(b) {
    dd <- d[unlist(idx_by_hh[hh_ids[draws[, b]]], use.names = FALSE), ]
    st <- contact_stats(dd)
    c(M_core = as.vector(st$M_core), H = as.vector(st$H), M_s50 = as.vector(st$M_s50), derived(st))
  }, mc.cores = n_cores))
  saveRDS(boot, cache_f)
}
bq <- function(pattern, exact = FALSE) {
  cols <- if (exact) which(colnames(boot) == pattern) else grep(pattern, colnames(boot))
  t(apply(boot[, cols, drop = FALSE], 2, quantile, c(0.025, 0.975), na.rm = TRUE))
}
ci_M <- bq("^M_core"); ci_H <- bq("^H[0-9]"); ci_tot <- bq("^tot_core")
ci_M50 <- bq("^M_s50"); ci_tot50 <- bq("^tot_s50")


# --- table2 ---
cell <- function(est, ci, d = 1) paste0(fmt(est, d), " (", ci_txt(ci[, 1], ci[, 2], d), ")")
M2 <- matrix(cell(as.vector(st0$M_core), ci_M), 5, 5)
n_core <- table(core$agp); n_all <- table(A$agp)
t2 <- data.frame(Respondent = c("0--17$^{a}$", "18--24", "25--49", "50--64", "65 or older"),
                 n = fint(as.vector(n_core)), M2,
                 Total = cell(rowSums(st0$M_core), ci_tot))
names(t2) <- c("Age of respondent", "$n$", "0--17", "18--24", "25--49", "50--64", "65 or older", "All ages")
tex_table(t2, "table_02.tex", align = "lrrrrrrr")
t2
keep50 <- A[A$maxans <= 50, ]; n_s50 <- table(keep50$agp)
ts4 <- data.frame(Respondent = c("0--17$^{a}$", "18--24", "25--49", "50--64", "65 or older"),
                  n = fint(as.vector(n_s50)), matrix(cell(as.vector(st0$M_s50), ci_M50), 5, 5),
                  Total = cell(rowSums(st0$M_s50), ci_tot50))
names(ts4) <- names(t2)
tex_table(ts4, "table_03.tex", align = "lrrrrrrr")
ts4


# --- table3 ---
ci_cap <- bq("^tot_cap"); ci_naive <- bq("^tot_naive"); ci_h <- bq("^tot_H")
heap_pct <- tapply(A$heap, A$agp, mean) * 100
out50_pct <- tapply(A$maxans > 50, A$agp, mean) * 100
t3 <- data.frame(Respondent = c("0--17", "18--24", "25--49", "50--64", "65 or older"),
                 n = fint(as.vector(n_all)), heaped = fmt(heap_pct, 1), out50 = fmt(out50_pct, 1),
                 core = cell(der0[paste0("tot_core.", grp)], ci_tot),
                 s50 = cell(der0[paste0("tot_s50.", grp)], ci_tot50),
                 cap = cell(der0[paste0("tot_cap.", grp)], ci_cap),
                 naive = cell(der0[paste0("tot_naive.", grp)], ci_naive, 0),
                 hh = cell(der0[paste0("tot_H.", grp)], ci_h))
names(t3) <- c("Age", "$n$", "$\\ge$100, \\%", "$>$50, \\%", "E100", "E50", "Capped at 100", "As reported", "Household")
tex_table(t3, "table_04.tex", align = "lrrrrrrrr")
t3

b1 <- function(nm) quantile(boot[, nm], c(0.025, 0.975), na.rm = TRUE)
bci <- function(nm, d = 1, mult = 1) ci_txt(mult * b1(nm)[1], mult * b1(nm)[2], d)
num("bootB", fint(params$B))
num("ratioRawCore", fmt(der0["tot_naive.25-49"] * 0 + weighted.mean(A$tot, A$w) / weighted.mean(core$tot, core$w), 0))
num("nRatioMidOld", fmt(N["25-49"] / N["65+"], 0)); num("nRatioKidsOld", fmt(N["0-17"] / N["65+"], 0))
num("rhoRatioNaive", fmt(der0["rho_ratio_naive"], 1)); num("rhoRatioNaiveCI", bci("rho_ratio_naive"))
num("rhoRatioCap", fmt(der0["rho_ratio_cap"], 1)); num("rhoRatioCapCI", bci("rho_ratio_cap"))
num("hhShare", fmt(100 * der0["hh_share"], 0)); num("hhShareCI", bci("hh_share", 0, 100))
num("meanDailyRef", fmt(der0["ref.mean_contacts"], 1)); num("meanDailyRefCI", bci("ref.mean_contacts"))
num("qRef", fmt(der0["ref.Q"], 2)); num("qRefCI", bci("ref.Q", 2))
num("dailyRefKids", fmt(der0["daily_ref.0-17"], 1)); num("dailyRefKidsCI", bci("daily_ref.0-17"))
num("dailyRefYoung", fmt(der0["daily_ref.18-24"], 1)); num("dailyRefMid", fmt(der0["daily_ref.25-49"], 1))
num("dailyRefFifty", fmt(der0["daily_ref.50-64"], 1))
num("dailyRefOld", fmt(der0["daily_ref.65+"], 1)); num("dailyRefOldCI", bci("daily_ref.65+"))
num("totCoreYoung", fmt(der0["tot_core.18-24"], 1)); num("totCoreMid", fmt(der0["tot_core.25-49"], 1))
num("totCoreOld", fmt(der0["tot_core.65+"], 1)); num("totCoreOldCI", ci_txt(ci_tot[5, 1], ci_tot[5, 2], 1))
num("totCoreMidCI", ci_txt(ci_tot[3, 1], ci_tot[3, 2], 1))
num("totNaiveYoung", fmt(der0["tot_naive.18-24"], 0)); num("totNaiveMid", fmt(der0["tot_naive.25-49"], 0))
num("totNaiveOld", fmt(der0["tot_naive.65+"], 0))
num("totCapOld", fmt(der0["tot_cap.65+"], 1)); num("totCapYoung", fmt(der0["tot_cap.18-24"], 1))
num("ratioOldMidCore", fmt(der0["tot_core.65+"] / der0["tot_core.25-49"], 2))
num("ratioOldMidNaive", fmt(der0["tot_naive.65+"] / der0["tot_naive.25-49"], 2))
num("nKeptFifty", fint(nrow(keep50))); num("nExclFifty", fint(sum(A$maxans > 50)))
num("pctExclFifty", fmt(100 * mean(A$maxans > 50), 1)); num("nBetweenFiftyHundred", fint(sum(A$maxans > 50 & A$maxans < 100)))
num("nExactlyFifty", fint(sum(A$maxans == 50)))
num("fiftyMeanTotal", fmt(weighted.mean(keep50$tot, keep50$w), 1))
num("fiftyMeanTotalDrop", fmt(100 * (1 - weighted.mean(keep50$tot, keep50$w) / weighted.mean(core$tot, core$w)), 0))
num("totFiftyYoung", fmt(der0["tot_s50.18-24"], 1)); num("totFiftyMid", fmt(der0["tot_s50.25-49"], 1))
num("totFiftyMidCI", ci_txt(ci_tot50[3, 1], ci_tot50[3, 2], 1))
num("totFiftyFifty", fmt(der0["tot_s50.50-64"], 1)); num("totCoreFifty", fmt(der0["tot_core.50-64"], 1))
num("totFiftyOld", fmt(der0["tot_s50.65+"], 1)); num("totFiftyOldCI", ci_txt(ci_tot50[5, 1], ci_tot50[5, 2], 1))
num("ratioOldMidFifty", fmt(der0["tot_s50.65+"] / der0["tot_s50.25-49"], 2))
num("meanDailyFifty", fmt(der0["ref50.mean_contacts"], 1)); num("meanDailyFiftyCI", bci("ref50.mean_contacts"))
num("hhShareFifty", fmt(100 * der0["hh_share50"], 0)); num("hhShareFiftyCI", bci("hh_share50", 0, 100))
num("qFifty", fmt(der0["ref50.Q"], 2))
num("dailyFiftyKids", fmt(der0["daily_ref50.0-17"], 1)); num("dailyFiftyYoung", fmt(der0["daily_ref50.18-24"], 1))
num("dailyFiftyMid", fmt(der0["daily_ref50.25-49"], 1)); num("dailyFiftyFifty", fmt(der0["daily_ref50.50-64"], 1))
num("dailyFiftyOld", fmt(der0["daily_ref50.65+"], 1)); num("dailyFiftyOldCI", bci("daily_ref50.65+"))
num("rhoRatioFifty", fmt(der0["rho_ratio_s50"], 2)); num("rhoRatioFiftyCI", bci("rho_ratio_s50", 2))
num("rhoDropFiftyPct", fmt(100 * (1 - der0["rho_ratio_s50"]), 0))
num("hhOtherMean", fmt(weighted.mean(rowSums(d[d$h_complete, hl]), d$w[d$h_complete]), 1))
num("hhKidsOfMid", fmt(st0$H["25-49", "0-17"], 1)); num("hhOld", fmt(der0["tot_H.65+"], 1)); num("hhKids", fmt(der0["tot_H.0-17"], 1))


# --- reciprocity ---
rec0 <- der0[grep("^rec\\.", names(der0))]; rec_ci <- bq("^rec\\.")
rec_tab <- data.frame(pair = sub("rec\\.", "", names(rec0)),
                      Nratio = N[adult_pairs[, 1]] / N[adult_pairs[, 2]], ratio = rec0,
                      lo = rec_ci[, 1], hi = rec_ci[, 2])
rec_tab$fitted <- rec_tab$Nratio^der0["el.beta"]
rec_tab
rec50 <- der0[grep("^rec50\\.", names(der0))]; rec50_ci <- bq("^rec50\\.")
recn <- der0[grep("^recn\\.", names(der0))]; recn_ci <- bq("^recn\\.")
rci <- function(est, ci) paste0(fmt(est, 2), " (", ci_txt(ci[, 1], ci[, 2], 2), ")")
ts3 <- data.frame(Pair = gsub("\\|", " and ", gsub("-", "--", gsub("65\\+", "65 or older", rec_tab$pair))),
                  Nr = fmt(rec_tab$Nratio, 2),
                  R = rci(rec_tab$ratio, rec_ci), fit = fmt(rec_tab$fitted, 2),
                  R50 = rci(rec50, rec50_ci), fit50 = fmt(rec_tab$Nratio^der0["el50.beta"], 2),
                  Rn = rci(recn, recn_ci))
names(ts3) <- c("Age groups $i$ and $j$", "$N_i/N_j$", "E100: $R_{ij}$ (95\\% CI)", "$(N_i/N_j)^{\\hat\\eta}$",
                "E50: $R_{ij}$ (95\\% CI)", "$(N_i/N_j)^{\\hat\\eta}$", "As reported: $R_{ij}$ (95\\% CI)")
tex_table(ts3, "table_11.tex", align = "lrrrrrr")

## does a stricter exclusion bring the answers closer to reciprocity?
el_path <- data.frame(c = thr_set, n = sapply(thr_set, function(cc) sum(A$maxans <= cc)),
                      eta = der0[paste0("el_path.c", thr_set)], bq("^el_path"))
names(el_path)[4:5] <- c("lo", "hi"); rownames(el_path) <- NULL
el_path
cover1 <- function(ci) sum(ci[, 1] <= 1 & ci[, 2] >= 1)
num("betaElFifty", fmt(der0["el50.beta"], 2)); num("betaElFiftyCI", bci("el50.beta", 2)); num("betaElFiftyRsq", fmt(der0["el50.r2"], 2))
num("betaElDiff", sub("^-", "$-$", fmt(der0["el_diff"], 2)))
num("betaElDiffCI", paste0(sub("^-", "$-$", fmt(b1("el_diff")[1], 2)), " to ", sub("^-", "$-$", fmt(b1("el_diff")[2], 2))))
num("betaElNaiveCI", bci("el_naive.beta", 2))
num("recMinFifty", fmt(min(rec50), 2)); num("recMaxFifty", fmt(max(rec50), 1)); num("recNcoverFifty", cover1(rec50_ci))
num("recMinNaive", fmt(min(recn), 2)); num("recMaxNaive", fmt(max(recn), 1)); num("recNcoverNaive", cover1(recn_ci))
num("gapCore", fmt(der0["gap.core"], 2)); num("gapCoreCI", bci("gap.core", 2))
num("gapFifty", fmt(der0["gap.s50"], 2)); num("gapFiftyCI", bci("gap.s50", 2))
num("gapNaive", fmt(der0["gap.naive"], 2)); num("gapNaiveCI", bci("gap.naive", 2))
num("gapFoldCore", fmt(exp(der0["gap.core"]), 1)); num("gapFoldFifty", fmt(exp(der0["gap.s50"]), 1))
num("gapDiffCI", paste0(sub("^-", "$-$", fmt(b1("gap_diff")[1], 2)), " to ", sub("^-", "$-$", fmt(b1("gap_diff")[2], 2))))
ep <- function(cc, v = "eta") el_path[[v]][el_path$c == cc]
num("etaPathMin", fmt(min(el_path$eta), 2)); num("etaPathMax", fmt(max(el_path$eta), 2))
num("etaPathFive", fmt(ep(5), 2)); num("etaPathFiveCI", ci_txt(ep(5, "lo"), ep(5, "hi"), 2)); num("nPathFive", fint(ep(5, "n")))
num("etaPathTwenty", fmt(ep(20), 2)); num("etaPathTwentyCI", ci_txt(ep(20, "lo"), ep(20, "hi"), 2)); num("nPathTwenty", fint(ep(20, "n")))
num("etaPathThirty", fmt(ep(30), 2)); num("etaPathThirtyCI", ci_txt(ep(30, "lo"), ep(30, "hi"), 2))
num("etaPathLoMin", fmt(min(el_path$lo), 2))
num("mMidOldFifty", fmt(st0$M_s50["25-49", "65+"], 1)); num("mOldMidFifty", fmt(st0$M_s50["65+", "25-49"], 1))
num("recMidOldFifty", fmt(rec50["rec50.25-49|65+"], 1))
num("recMin", fmt(min(rec0), 2)); num("recMax", fmt(max(rec0), 1))
num("recNcover", sum(rec_ci[, 1] <= 1 & rec_ci[, 2] >= 1)); num("recNpairs", length(rec0))
num("betaEl", fmt(der0["el.beta"], 2)); num("betaElCI", bci("el.beta", 2)); num("betaElRsq", fmt(der0["el.r2"], 2))
num("betaElNaive", fmt(der0["el_naive.beta"], 2))
num("recMidOld", fmt(rec0["rec.25-49|65+"], 1))
num("totMidToOld", fint(st0$M_core["25-49", "65+"] * N["25-49"])); num("totOldToMid", fint(st0$M_core["65+", "25-49"] * N["65+"]))
num("mMidOld", fmt(st0$M_core["25-49", "65+"], 1)); num("mOldMid", fmt(st0$M_core["65+", "25-49"], 1))
imp0 <- der0[grep("^imp", names(der0))]; teen0 <- der0[grep("^teen", names(der0))]
rbind(reciprocity_implied = imp0, reported_by_15_17 = teen0)
teenR <- (st0$M_core[1, 2:5] * N[1]) / (st0$M_core[2:5, 1] * N[2:5])
rbind(teen_ratio = teenR, anchoring_prediction = (N[1] / N[2:5])^der0["el.beta"])
num("teenChildChild", fmt(st0$M_core[1, 1], 1)); num("teenChildChildCI", ci_txt(ci_M[1, 1], ci_M[1, 2], 1))
num("nTeenCore", fint(n_core[1]))
num("teenRmin", fmt(min(teenR), 1)); num("teenRmax", fmt(max(teenR), 1))
num("teenPredMin", fmt(min((N[1] / N[2:5])^der0["el.beta"]), 1)); num("teenPredMax", fmt(max((N[1] / N[2:5])^der0["el.beta"]), 1))


# Figure 3 and machine-readable adult reciprocity diagnostics.
adult_diagnostics <- function(M, pattern, rule) {
  ratio <- recip_ratio(M)
  interval <- bq(pattern)
  x <- log(N[adult_pairs[, 1]] / N[adult_pairs[, 2]])
  eta <- unname(elasticity(M)["beta"])
  data.frame(rule, pair = paste(grp[adult_pairs[, 1]], grp[adult_pairs[, 2]], sep = " / "),
             x, ratio, low = interval[, 1], high = interval[, 2],
             eta, fitted_ratio = exp(eta*x), residual = log(ratio)-eta*x,
             highlight = adult_pairs[, 1] == 2 & adult_pairs[, 2] == 4)
}
adult_diag <- rbind(adult_diagnostics(st0$M_core, "^rec\\.", "E100"),
                    adult_diagnostics(st0$M_s50, "^rec50\\.", "E50"))
write.csv(adult_diag, "tables/figure_03_reciprocity.csv", row.names = FALSE)
rec_plot <- ggplot(adult_diag, aes(x, log(ratio))) +
  geom_hline(yintercept = 0, colour = "grey45", linetype = "dashed") +
  geom_abline(aes(slope = eta, intercept = 0), data = unique(adult_diag[c("rule", "eta")]),
              colour = "#29848b") +
  geom_errorbar(aes(ymin = log(low), ymax = log(high), colour = highlight), width = 0) +
  geom_point(aes(colour = highlight), size = 2) +
  geom_text(aes(label = pair, colour = highlight), nudge_y = .15, size = 2.6) +
  scale_colour_manual(values = c("#23425b", "#a5422b"), guide = "none") +
  scale_x_continuous(expand = expansion(mult = .14)) +
  facet_wrap(~rule) +
  labs(x = expression(log(N[i]/N[j])), y = expression(log(R[ij]))) + theme_paper
save_fig(rec_plot, "figure_03_reciprocity", 8, 3.6)


# --- fig3 ---
Cs0 <- reconcile(st0$M_core, st0$H)                     # reference: E100, category-anchoring correction
Cs50 <- reconcile(st0$M_s50, st0$H)                     # E50, category-anchoring correction
Cs_arith <- reconcile(st0$M_core, st0$H, "arithmetic")
heat <- function(M, title, xl, yl, d = 1, lim = max(M)) {
  df <- as.data.frame(as.table(M)); names(df) <- c("row", "col", "v")
  lv <- c("0-17", "18-24", "25-49", "50-64", "65+")
  df$row <- factor(df$row, levels = lv); df$col <- factor(df$col, levels = lv)
  ggplot(df, aes(col, row, fill = v)) + geom_tile(colour = "white", linewidth = 0.8) +
    geom_text(aes(label = fmt(v, d), colour = v > 0.55 * lim), size = 2.6) +
    scale_fill_gradientn(colours = seq_blue, limits = c(0, lim)) +
    scale_colour_manual(values = c(`TRUE` = "white", `FALSE` = ink), guide = "none") +
    labs(title = title, x = xl, y = yl) + coord_fixed() + theme_paper +
    theme(legend.position = "none", panel.grid = element_blank())
}
limM <- max(st0$M_core, st0$M_s50); limC <- max(Cs0, Cs50)
p3a <- heat(st0$M_core, "a  Outside household, E100", "Age of contact", "Age of respondent", lim = limM)
p3b <- heat(st0$M_s50, "b  Outside household, E50", "Age of contact", "Age of respondent", lim = limM)
p3c <- heat(st0$H, "c  Household members", "Age of household member", "Age of respondent")
p3d <- heat(Cs0, "d  Daily matrix, E100", "Age of contact", "Age group", lim = limC)
p3e <- heat(Cs50, "e  Daily matrix, E50", "Age of contact", "Age group", lim = limC)
ep_df <- el_path; ep_df$lab <- ifelse(is.finite(ep_df$c), as.character(ep_df$c), "any")
ep_df$lab[ep_df$c == 50] <- "50\nE50"; ep_df$lab[ep_df$c == 99] <- "99\nE100"
ep_df$lab <- factor(ep_df$lab, levels = ep_df$lab)
ep_df$key <- ifelse(ep_df$c == 99, "E100", ifelse(ep_df$c == 50, "E50", "other"))
p3f <- ggplot(ep_df, aes(lab, eta)) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed", colour = ink2, linewidth = 0.3) +
  geom_linerange(aes(ymin = lo, ymax = hi), colour = pal["blue"], linewidth = 0.5) +
  geom_point(aes(shape = key), colour = pal["blue"], fill = "white", size = 2) +
  annotate("text", x = 0.6, y = 0.05, label = "reciprocity", hjust = 0, size = 2.4, colour = ink2) +
  annotate("text", x = 0.6, y = 1.06, label = "answers ignore class size", hjust = 0, size = 2.4, colour = ink2) +
  scale_shape_manual(values = c(E100 = 16, E50 = 15, other = 21), guide = "none") +
  scale_y_continuous(limits = c(-0.05, 1.2), breaks = seq(0, 1, 0.25)) +
  labs(title = "f  Reciprocity elasticity", x = "Largest answer kept",
       y = expression(paste("Elasticity ", eta))) + theme_paper
fig3 <- (p3a + p3b + p3c) / (p3d + p3e + p3f)
save_fig(fig3, "figure_04_matrices", h = 5.7)
fig3
round(Cs0, 2); round(Cs50, 2)
write.csv(round(Cs0, 4), "tables/contact_matrix_daily_reconciled.csv")
write.csv(round(Cs50, 4), "tables/contact_matrix_daily_reconciled_E50.csv")
write.csv(round(Cs_arith, 4), "tables/contact_matrix_daily_arithmetic_symmetrisation.csv")
write.csv(round(st0$M_core, 4), "tables/contact_matrix_outside_core_2days.csv")
write.csv(round(st0$M_s50, 4), "tables/contact_matrix_outside_E50_2days.csv")
write.csv(round(st0$H, 4), "tables/contact_matrix_household.csv")


# --- regression ---
fit_nb2 <- function(formula, data, wvar = "w", cluster = "hh") {
  mf <- model.frame(formula, data, na.action = na.pass)
  keep <- complete.cases(mf) & !is.na(data[[wvar]])
  X <- model.matrix(formula, mf[keep, , drop = FALSE]); y <- model.response(mf)[keep]
  w <- data[[wvar]][keep]; w <- w / mean(w); cl <- data[[cluster]][keep]; p <- ncol(X)
  ll_i <- function(par) dnbinom(y, size = exp(par[p + 1]), mu = exp(as.vector(X %*% par[1:p])), log = TRUE)
  nll <- function(par) -sum(w * ll_i(par))
  gr <- function(par) { mu <- exp(as.vector(X %*% par[1:p])); th <- exp(par[p + 1])
    sb <- (y - mu) / (1 + mu / th)
    st <- th * (digamma(y + th) - digamma(th) + log(th) + 1 - log(th + mu) - (y + th) / (th + mu))
    -c(colSums(w * sb * X), sum(w * st)) }
  st <- c(coef(glm.fit(X, y, weights = w, family = quasipoisson())), 0)
  o <- optim(st, nll, gr, method = "BFGS", control = list(maxit = 2000, reltol = 1e-12))
  Ainv <- solve(numDeriv::jacobian(gr, o$par))
  S <- numDeriv::jacobian(ll_i, o$par, method = "simple") * w
  Sc <- rowsum(S, cl); G <- nrow(Sc)
  V <- Ainv %*% (crossprod(Sc) * G / (G - 1)) %*% Ainv
  list(coef = setNames(o$par[1:p], colnames(X)), se = sqrt(diag(V))[1:p], theta = exp(o$par[p + 1]),
       n = length(y), clusters = G, convergence = o$convergence)
}
A$agp_r <- relevel(A$agp, ref = "25-49"); A$sex <- factor(ifelse(A$female == 1, "Female", "Male"), levels = c("Male", "Female"))
A$hhcat <- cut(A$hhsize, c(0, 2, 4, 6, Inf), labels = c("1-2", "3-4", "5-6", "7 or more"))
rhs <- ~ agp_r + sex + employ + hhcat + housing + water + nat3 + wkend + period
Areg <- A[complete.cases(A[, all.vars(rhs)]), ]
des_reg <- svydesign(ids = ~hh, weights = ~w, data = Areg)
m_heap <- svyglm(update(rhs, heap ~ .), des_reg, family = quasibinomial())
m_nb <- fit_nb2(update(rhs, tot ~ .), Areg[!Areg$heap, ])
stopifnot(m_nb$convergence == 0)
or_tab <- data.frame(term = names(coef(m_heap)), est = exp(coef(m_heap)),
                     lo = exp(coef(m_heap) - 1.96 * SE(m_heap)), hi = exp(coef(m_heap) + 1.96 * SE(m_heap)))
rr_tab <- data.frame(term = names(m_nb$coef), est = exp(m_nb$coef),
                     lo = exp(m_nb$coef - 1.96 * m_nb$se), hi = exp(m_nb$coef + 1.96 * m_nb$se))
reg <- merge(or_tab, rr_tab, by = "term", suffixes = c(".or", ".rr"), sort = FALSE)
reg

labels4 <- c(
  "agp_r0-17" = "\\quad 15--17", "agp_r18-24" = "\\quad 18--24", "agp_r50-64" = "\\quad 50--64",
  "agp_r65+" = "\\quad 65 or older", "sexFemale" = "\\quad Female",
  "employEmployed" = "\\quad Employed", "employStudent" = "\\quad Student",
  "hhcat3-4" = "\\quad 3--4", "hhcat5-6" = "\\quad 5--6", "hhcat7 or more" = "\\quad 7 or more",
  "housingSheet metal or wood" = "\\quad Sheet metal or wood", "waterOutside dwelling" = "\\quad Outside dwelling",
  "nat3Comorian" = "\\quad Comorian", "nat3Other or unknown" = "\\quad Other or unknown",
  "wkendOne" = "\\quad One", "wkendTwo" = "\\quad Two",
  "period20 Sept-10 Oct" = "\\quad 20 Sept--10 Oct", "periodFrom 11 Oct" = "\\quad From 11 Oct")
heads4 <- c("agp_r0-17" = "Age, years (ref. 25--49)", "sexFemale" = "Sex (ref. male)",
            "employEmployed" = "Employment (ref. not employed)", "hhcat3-4" = "Household size (ref. 1--2)",
            "housingSheet metal or wood" = "Housing (ref. permanent)", "waterOutside dwelling" = "Water access (ref. inside dwelling)",
            "nat3Comorian" = "Nationality (ref. French)", "wkendOne" = "Weekend days in recall window (ref. none)",
            "period20 Sept-10 Oct" = "Interview period (ref. 1--19 Sept)")
t4 <- NULL
for (tm in names(labels4)) {
  if (tm %in% names(heads4)) t4 <- rbind(t4, data.frame(v = heads4[[tm]], or = "", rr = ""))
  r <- reg[reg$term == tm, ]
  t4 <- rbind(t4, data.frame(v = labels4[[tm]],
              or = paste0(fmt(r$est.or, 2), " (", ci_txt(r$lo.or, r$hi.or, 2), ")"),
              rr = paste0(fmt(r$est.rr, 2), " (", ci_txt(r$lo.rr, r$hi.rr, 2), ")")))
}
names(t4) <- c("Characteristic", "Heaped answer, OR (95\\% CI)", "Core contacts, RR (95\\% CI)")
tex_table(t4, "table_06.tex", align = "lrr", addspace = which(t4[[2]] == "")[-1])
t4
g <- function(tm, what) reg[reg$term == tm, what]
tri <- function(tm, s) paste0(fmt(g(tm, paste0("est.", s)), 2), " (95\\% CI ", ci_txt(g(tm, paste0("lo.", s)), g(tm, paste0("hi.", s)), 2), ")")
num("nReg", fint(nrow(Areg))); num("nRegCore", fint(m_nb$n)); num("nbTheta", fmt(m_nb$theta, 2))
num("orHeapPeriodMid", tri("period20 Sept-10 Oct", "or")); num("orHeapPeriodLate", tri("periodFrom 11 Oct", "or"))
num("orHeapFemale", tri("sexFemale", "or")); num("orHeapOld", tri("agp_r65+", "or"))
num("orHeapEmployed", tri("employEmployed", "or")); num("orHeapWater", tri("waterOutside dwelling", "or"))
num("rrOld", tri("agp_r65+", "rr")); num("rrYoung", tri("agp_r18-24", "rr")); num("rrFemale", tri("sexFemale", "rr"))
num("rrEmployed", tri("employEmployed", "rr")); num("rrStudent", tri("employStudent", "rr"))
num("rrHHlarge", tri("hhcat7 or more", "rr")); num("rrPeriodMid", tri("period20 Sept-10 Oct", "rr"))
num("rrWater", tri("waterOutside dwelling", "rr")); num("rrHousing", tri("housingSheet metal or wood", "rr"))
num("rrWeekendTwo", tri("wkendTwo", "rr")); num("rrComorian", tri("nat3Comorian", "rr"))
num("rrFiftySixtyFour", tri("agp_r50-64", "rr"))

## sensitivity: core matrix with inverse-probability weights for not being in the heap class
Areg$p_heap <- as.vector(predict(m_heap, newdata = Areg, type = "response"))
ipw <- Areg[!Areg$heap, ]; ipw$w <- ipw$w / (1 - ipw$p_heap)
M_ipw <- wmean_by(ipw, cvar); M_cc <- wmean_by(Areg[!Areg$heap, ], cvar)
round(rowSums(M_ipw) / rowSums(M_cc), 3)
num("ipwMaxChange", fmt(100 * max(abs(rowSums(M_ipw)[2:5] / rowSums(M_cc)[2:5] - 1)), 1))


# --- dpd_theory ---
zinb_parts <- function(pi0, mu, th, ymax = GRID_MAX) {
  ## ZINB probabilities and scores with respect to (logit pi, log mu, log theta)
  y <- 0:ymax; g <- dnbinom(y, size = th, mu = mu); f <- (1 - pi0) * g; f[1] <- f[1] + pi0
  smu <- th * (y - mu) / (mu * (mu + th))
  sth <- th * (digamma(y + th) - digamma(th) + log(th) + 1 - log(th + mu) - (y + th) / (th + mu))
  w <- (1 - pi0) * g / f
  U <- cbind(a = ifelse(y == 0, pi0 * (1 - pi0) * (1 - g[1]) / f[1], -pi0), b = w * smu * mu, t = w * sth)
  U[!is.finite(U)] <- 0
  list(y = y, f = f, U = U, grad_mean = c(-pi0 * (1 - pi0) * mu, (1 - pi0) * mu, 0))
}
dpd_mats <- function(pi0, mu, th, alpha) {
  z <- zinb_parts(pi0, mu, th)
  J <- crossprod(z$U * z$f^(1 + alpha), z$U); xi <- colSums(z$U * z$f^(1 + alpha))
  K <- crossprod(z$U * z$f^(1 + 2 * alpha), z$U) - tcrossprod(xi)
  IF <- t(solve(J, t(z$U * z$f^alpha) - xi))
  list(J = J, K = K, xi = xi, IF_mean = as.vector(IF %*% z$grad_mean),
       IF_limit = -as.numeric(z$grad_mean %*% solve(J, xi)),
       avar_mean = as.numeric(t(z$grad_mean) %*% solve(J, K) %*% solve(J, z$grad_mean)))
}
## the analytical score agrees with numerical differentiation of the log-probability
chk <- zinb_parts(0.3, 7, 0.5, 60)
lf <- function(par, y) log(dzi(y, "nb", plogis(par[1]), exp(par[2]), exp(par[3])))
stopifnot(max(abs(sapply(c(0, 1, 5, 40), function(yy)
  numDeriv::grad(lf, c(qlogis(0.3), log(7), log(0.5)), y = yy) - chk$U[yy + 1, ]))) < 1e-6)


# --- dpd_variance ---
sim0 <- sim_res[[which(sim_grid$dgp == "A" & sim_grid$eps == 0)]]
var_chk <- data.frame(alpha = c(0, 0.5, 1),
  asymptotic_sd = sapply(c(1e-8, 0.5, 1), function(a) sqrt(dpd_mats(0.3, 7, 0.5, a)$avar_mean / 500)),
  simulated_sd = c(sd(sim0[, "naive"]), sd(sim0[, "mdpde"]), sd(sim0[, "mdpde1"])))
var_chk$efficiency <- (var_chk$asymptotic_sd[1] / var_chk$asymptotic_sd)^2
var_chk
num("asySdHalf", fmt(var_chk$asymptotic_sd[2], 3)); num("simSdHalf", fmt(var_chk$simulated_sd[2], 3))
num("asySdOne", fmt(var_chk$asymptotic_sd[3], 3)); num("simSdOne", fmt(var_chk$simulated_sd[3], 3))
num("asySdML", fmt(var_chk$asymptotic_sd[1], 3)); num("simSdML", fmt(var_chk$simulated_sd[1], 3))
num("effHalf", fmt(100 * var_chk$efficiency[2], 0)); num("effOne", fmt(100 * var_chk$efficiency[3], 0))


# --- dpd_influence ---
if_alphas <- c(0, 0.25, 0.5, 1)
if_list <- lapply(if_alphas, function(a) dpd_mats(0.3, 7, 0.5, max(a, 1e-8)))
if_df <- do.call(rbind, lapply(seq_along(if_alphas), function(i)
  data.frame(alpha = if_alphas[i], y0 = 0:300, IF = if_list[[i]]$IF_mean[1:301])))
rej_scale <- function(alpha, mu, th) 1 / (alpha * log(1 + th / mu))
if_sum <- data.frame(alpha = if_alphas, at100 = sapply(if_list, function(m) m$IF_mean[101]),
  max = sapply(if_list, function(m) max(m$IF_mean[1:2001])), argmax = sapply(if_list, function(m) which.max(m$IF_mean[1:2001]) - 1),
  limit = sapply(if_list, function(m) m$IF_limit), scale = c(Inf, rej_scale(if_alphas[-1], 7, 0.5)))
if_sum
zinb_ans <- fit_count(core_ans, core_ans_w, family = "nb", zi = TRUE)        # age-specific answers of core respondents
num("rejScaleHalfSim", fmt(rej_scale(0.5, 7, 0.5), 0)); num("rejScaleOneSim", fmt(rej_scale(1, 7, 0.5), 1))
num("rejScaleHalfAns", fmt(rej_scale(0.5, zinb_ans["mu"], zinb_ans["theta"]), 0))
num("rejScaleHalfTot", fmt(rej_scale(0.5, zinb_core["mu"], zinb_core["theta"]), 0))
num("zinbAnsMu", fmt(zinb_ans["mu"], 1)); num("zinbAnsTheta", fmt(zinb_ans["theta"], 2))
num("ifArgmaxHalf", if_sum$argmax[3]); num("ifArgmaxOne", if_sum$argmax[4])
num("ifAtHundredHalf", fmt(if_sum$at100[3], 1)); num("ifAtHundredML", fmt(if_sum$at100[1], 1))
num("ifLimitHalf", fmt(if_sum$limit[3], 1)); num("ifLimitOne", fmt(if_sum$limit[4], 1))
num("ifFirstOrderHalf", fmt(100 * 0.1 * if_sum$at100[3] / 4.9, 0))


# --- dpd_pseudo_true ---
pop_fit <- function(g, alpha, type, grid = 0:GRID_MAX) {
  ## minimiser of the population objective for a probability vector g on the grid
  sup <- which(g > 0); best <- NULL
  for (s in list(c(qlogis(0.3), log(7), log(0.5)), c(qlogis(0.05), log(5), log(0.3)),
                 c(qlogis(0.5), log(10), log(1.5)), c(qlogis(0.2), log(20), log(0.2)))) {
    o <- try(optim(s, objective, yu = grid[sup], wu = g[sup], family = "nb", zi = TRUE, alpha = alpha, type = type,
                   grid = grid, method = "L-BFGS-B", lower = c(-12, -4, -4), upper = c(8, 7.5, 4),
                   control = list(maxit = 2000, factr = 10)), silent = TRUE)
    if (!inherits(o, "try-error") && (is.null(best) || o$value < best$value)) best <- o
  }
  p <- unpack(best$par, "nb", TRUE); (1 - p$pi0) * p$mu
}
est4 <- list(mdpde = c(0.5, "dpd"), mdpde1 = c(1, "dpd"), gamma = c(0.5, "gamma"), gamma1 = c(1, "gamma"))
cache_f <- "cache/dpd_pseudo_true.rds"
if (params$use_cache && file.exists(cache_f)) ptv <- readRDS(cache_f) else {
  f0 <- dzi(0:GRID_MAX, "nb", 0.3, 7, 0.5)
  heap_pmf <- numeric(GRID_MAX + 1); heap_pmf[seq(100, 900, 100) + 1] <- c(0.7, rep(0.3 / 8, 8))
  eps_grid <- c(0, 0.05, 0.10, 0.15); c_grid <- c(10, 15, 20, 30, 40, 50, 65, 80, 100, 130, 160, 200, 300, 500, 1000, 2500)
  jobs <- rbind(data.frame(kind = "heap", x = eps_grid), data.frame(kind = "point", x = c_grid), data.frame(kind = "lost", x = 0.1))
  res <- mclapply(seq_len(nrow(jobs)), function(i) {
    g <- if (jobs$kind[i] == "heap") (1 - jobs$x[i]) * f0 + jobs$x[i] * heap_pmf
         else if (jobs$kind[i] == "point") { v <- 0.9 * f0; v[jobs$x[i] + 1] <- v[jobs$x[i] + 1] + 0.1; v }
         else 0.9 * f0                                  # contaminating mass sent to infinity
    sapply(est4, function(e) pop_fit(g, as.numeric(e[1]), e[2]))
  }, mc.cores = n_cores)
  ptv <- cbind(jobs, do.call(rbind, res)); saveRDS(ptv, cache_f)
}
ptv
rb <- function(x) 100 * (x / true_mean - 1)
pt_heap <- ptv[ptv$kind == "heap", ]
sim_mean <- t(sapply(pt_heap$x, function(e) colMeans(sim_res[[which(sim_grid$dgp == "A" & abs(sim_grid$eps - e) < 1e-9)]])[names(est4)]))
sgn <- function(x, d = 0) sub("^-", "$-$", fmt(x, d))
ts5 <- data.frame(eps = fmt(pt_heap$x, 2))
for (e in names(est4)) { ts5[[paste0(e, "_th")]] <- sgn(rb(pt_heap[[e]])); ts5[[paste0(e, "_sim")]] <- sgn(rb(sim_mean[, e])) }
names(ts5) <- c("$\\varepsilon$", "DPD 0.5: limit", "simulated", "DPD 1: limit", "simulated",
                "$\\gamma$ 0.5: limit", "simulated", "$\\gamma$ 1: limit", "simulated")
tex_table(ts5, "table_12.tex", align = "lrrrrrrrr")
ts5
pth <- function(e, est) rb(pt_heap[[est]][abs(pt_heap$x - e) < 1e-9])
num("ptMdpdeTen", fmt(pth(0.10, "mdpde"), 0)); num("ptMdpdeFive", fmt(pth(0.05, "mdpde"), 0)); num("ptMdpdeOneTen", fmt(pth(0.10, "mdpde1"), 0))
num("ptGammaTen", fmt(pth(0.10, "gamma"), 0)); num("ptGammaOneTen", fmt(pth(0.10, "gamma1"), 0))
num("simMdpdeBiasFiveA", fmt(sb("A", 0.05, "mdpde"), 0))
num("ptMaxAbsDiff", fmt(max(abs(rb(as.matrix(pt_heap[pt_heap$x <= 0.1, names(est4)])) - rb(sim_mean[pt_heap$x <= 0.1, ]))), 0))
pt_lost <- ptv[ptv$kind == "lost", ]
num("lostMdpdeHalf", fmt(rb(pt_lost$mdpde), 0)); num("lostMdpdeOne", fmt(rb(pt_lost$mdpde1), 0))
num("lostGammaHalf", fmt(abs(rb(pt_lost$gamma)), 1)); num("lostGammaOne", fmt(abs(rb(pt_lost$gamma1)), 1))
pt_point <- ptv[ptv$kind == "point", ]
num("pointMdpdeHalfHundred", fmt(rb(pt_point$mdpde[pt_point$x == 100]), 0)); num("pointGammaOneHundred", fmt(rb(pt_point$gamma1[pt_point$x == 100]), 0))
num("pointGammaHalfHundred", fmt(rb(pt_point$gamma[pt_point$x == 100]), 0))
num("pointGammaHalfTwoHundred", fmt(rb(pt_point$gamma[pt_point$x == 200]), 0))


# --- figS1 ---
lab_if <- c("Maximum likelihood", "Density power, 0.25", "Density power, 0.5", "Density power, 1")
if_df$lab <- factor(lab_if[match(if_df$alpha, if_alphas)], levels = lab_if)
pS1a <- ggplot(if_df, aes(y0, IF, colour = lab, linetype = lab)) +
  geom_hline(yintercept = 0, colour = grid_col, linewidth = 0.4) +
  geom_vline(xintercept = 100, linetype = "dashed", colour = ink2, linewidth = 0.3) +
  geom_line(linewidth = 0.7) + coord_cartesian(ylim = c(-15, 70)) +
  scale_colour_manual(values = setNames(c(ink2, rep(unname(pal["aqua"]), 3)), lab_if)) +
  scale_linetype_manual(values = setNames(c("longdash", "dotted", "solid", "dashed"), lab_if)) +
  annotate("text", x = 104, y = 67, label = "heap at 100", hjust = 0, size = 2.5, colour = ink2) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(title = "a  Influence of one answer on the estimated mean", x = "Value of the contaminating answer",
       y = "Influence function of the mean") + theme_paper + theme(legend.key.width = unit(8, "mm"))
pp <- pivot_longer(pt_point[, c("x", names(est4))], -x)
pp$bias <- rb(pp$value)
lab_est <- c(mdpde = "Density power, 0.5", mdpde1 = "Density power, 1", gamma = "Gamma-divergence, 0.5", gamma1 = "Gamma-divergence, 1")
pp$lab <- factor(lab_est[pp$name], levels = lab_est)
pS1b <- ggplot(pp, aes(x, bias, colour = lab, shape = lab, linetype = lab)) +
  geom_hline(yintercept = 0, colour = grid_col, linewidth = 0.4) +
  geom_vline(xintercept = 100, linetype = "dashed", colour = ink2, linewidth = 0.3) +
  geom_line(linewidth = 0.7) + geom_point(size = 1.6) +
  scale_x_log10(breaks = c(10, 30, 100, 300, 1000)) +
  scale_colour_manual(values = setNames(unname(pal[c("aqua", "aqua", "violet", "violet")]), lab_est)) +
  scale_linetype_manual(values = setNames(c("solid", "dashed", "solid", "dashed"), lab_est)) +
  scale_shape_manual(values = setNames(c(16, 1, 15, 0), lab_est)) + guides(colour = guide_legend(nrow = 2)) +
  labs(title = "b  Limit of the estimators, 10% of answers at one value", x = "Value of the contaminating answers (log scale)",
       y = "Relative bias of the mean (%)") + theme_paper + theme(legend.key.width = unit(8, "mm"))
figS1 <- pS1a + pS1b
save_fig(figS1, "figure_07_dpd_theory", h = 3.6)
figS1


# --- dpd_regression ---
nb_sums <- function(lmu, th, alpha) {
  t(sapply(lmu, function(l) {
    mu <- exp(l); y <- 0:max(200, qnbinom(1 - 1e-13, size = th, mu = mu))
    ga <- dnbinom(y, size = th, mu = mu)^(1 + alpha)
    sb <- th * (y - mu) / (mu + th)
    st <- th * (digamma(y + th) - digamma(th) + log(th) + 1 - log(th + mu) - (y + th) / (th + mu))
    c(sum(ga), sum(ga * sb), sum(ga * st))
  }))
}
fit_nb2_dpd <- function(formula, data, alpha, start, wvar = "w", cluster = "hh", nodes = 60) {
  mf <- model.frame(formula, data, na.action = na.pass)
  keep <- complete.cases(mf) & !is.na(data[[wvar]])
  X <- model.matrix(formula, mf[keep, , drop = FALSE]); y <- model.response(mf)[keep]
  w <- data[[wvar]][keep]; w <- w / mean(w); cl <- data[[cluster]][keep]; p <- ncol(X)
  pieces <- function(par) {
    eta <- as.vector(X %*% par[1:p]); th <- exp(par[p + 1]); mu <- exp(eta)
    nd <- seq(min(eta) - 1e-6, max(eta) + 1e-6, length.out = nodes); sm <- nb_sums(nd, th, alpha)
    ip <- function(k) spline(nd, sm[, k], xout = eta, method = "fmm")$y
    ga <- dnbinom(y, size = th, mu = mu)^alpha
    sb <- th * (y - mu) / (mu + th)
    st <- th * (digamma(y + th) - digamma(th) + log(th) + 1 - log(th + mu) - (y + th) / (th + mu))
    list(obj = ip(1) - (1 + 1 / alpha) * ga, psi = cbind((ip(2) - ga * sb) * X, ip(3) - ga * st))
  }
  fn <- function(par) sum(w * pieces(par)$obj) / length(y)
  gr <- function(par) (1 + alpha) * colSums(w * pieces(par)$psi) / length(y)
  o <- optim(start, fn, gr, method = "BFGS", control = list(maxit = 1000, reltol = 1e-12))
  Ji <- solve(numDeriv::jacobian(function(par) colSums(w * pieces(par)$psi), o$par))
  Sc <- rowsum(w * pieces(o$par)$psi, cl); G <- nrow(Sc)
  V <- Ji %*% (crossprod(Sc) * G / (G - 1)) %*% t(Ji)
  list(coef = setNames(o$par[1:p], colnames(X)), se = sqrt(diag(V))[1:p], theta = exp(o$par[p + 1]),
       n = length(y), convergence = o$convergence)
}
f_tot <- update(rhs, tot ~ .)
m_all <- fit_nb2(f_tot, Areg)                                     # maximum likelihood, heaped respondents included
cache_f <- "cache/dpd_regression.rds"
if (params$use_cache && file.exists(cache_f)) dpd_reg <- readRDS(cache_f) else {
  st_reg <- c(m_nb$coef, log(m_nb$theta))
  dpd_reg <- mclapply(list(Areg, Areg[!Areg$heap, ]), function(dd) fit_nb2_dpd(f_tot, dd, alpha = 0.5, start = st_reg),
                      mc.cores = n_cores)
  saveRDS(dpd_reg, cache_f)
}
stopifnot(dpd_reg[[1]]$convergence == 0, dpd_reg[[2]]$convergence == 0, m_all$convergence == 0)
rrci <- function(m, tm) { i <- match(tm, names(m$coef))
  paste0(fmt(exp(m$coef[i]), 2), " (", ci_txt(exp(m$coef[i] - 1.96 * m$se[i]), exp(m$coef[i] + 1.96 * m$se[i]), 2), ")") }
ts6 <- NULL
for (tm in names(labels4)) {
  if (tm %in% names(heads4)) ts6 <- rbind(ts6, data.frame(v = heads4[[tm]], a = "", b = "", c = "", d = ""))
  ts6 <- rbind(ts6, data.frame(v = labels4[[tm]], a = rrci(m_nb, tm), b = rrci(dpd_reg[[2]], tm), c = rrci(m_all, tm), d = rrci(dpd_reg[[1]], tm)))
}
ts6 <- rbind(ts6, data.frame(v = "Shape $\\hat\\vartheta$", a = fmt(m_nb$theta, 2), b = fmt(dpd_reg[[2]]$theta, 2),
                             c = fmt(m_all$theta, 2), d = fmt(dpd_reg[[1]]$theta, 2)))
names(ts6) <- c("Characteristic", "ML, core", "MDPDE, core", "ML, all", "MDPDE, all")
tex_table(ts6, "table_13.tex", align = "lrrrr", addspace = c(which(ts6[[2]] == "")[-1], nrow(ts6)))
ts6
slopes <- names(labels4)
dist_core <- function(m) mean(abs(m$coef[slopes] - m_nb$coef[slopes]))
c(ML_all = dist_core(m_all), MDPDE_all = dist_core(dpd_reg[[1]]), MDPDE_core = dist_core(dpd_reg[[2]]))
num("regStabDPD", fmt(mean(abs(dpd_reg[[1]]$coef[slopes] - dpd_reg[[2]]$coef[slopes])), 2))
num("regDistMLall", fmt(dist_core(m_all), 2)); num("regDistDPDall", fmt(dist_core(dpd_reg[[1]]), 2)); num("regDistDPDcore", fmt(dist_core(dpd_reg[[2]]), 2))
rr1 <- function(m, tm) fmt(exp(m$coef[tm]), 2)
num("rrPeriodMidMLall", rr1(m_all, "period20 Sept-10 Oct")); num("rrPeriodMidDPDall", rr1(dpd_reg[[1]], "period20 Sept-10 Oct"))
num("rrPeriodMidML", rr1(m_nb, "period20 Sept-10 Oct")); num("rrPeriodMidDPDcore", rr1(dpd_reg[[2]], "period20 Sept-10 Oct"))
num("rrOldML", rr1(m_nb, "agp_r65+")); num("rrOldDPDcore", rr1(dpd_reg[[2]], "agp_r65+")); num("rrOldDPDall", rr1(dpd_reg[[1]], "agp_r65+"))
num("rrHHlargeML", rr1(m_nb, "hhcat7 or more")); num("rrHHlargeDPDcore", rr1(dpd_reg[[2]], "hhcat7 or more"))
num("nbThetaAll", fmt(m_all$theta, 2)); num("nbThetaDPDall", fmt(dpd_reg[[1]]$theta, 2)); num("nRegAll", fint(m_all$n))


# --- serology_weights ---
d$resp <- as.integer(!is.na(d$igt))
d$qtype <- factor(ifelse(d$adultq, "Adult", "Child"))
m_resp <- glm(resp ~ qtype * female + age9 + nat3, family = binomial(), data = d)
d$p_resp <- fitted(m_resp)
d$w_s <- ifelse(d$resp == 1, d$w / d$p_resp, NA)
d$w_s <- d$w_s * sum(d$w) / sum(d$w_s, na.rm = TRUE)
round(tapply(d$resp, d$qtype, mean), 3); round(tapply(d$resp, d$nat3, mean), 3)
S <- d[d$resp == 1, ]
S$any <- as.integer(S$igt == 1 | S$igg == 1)
des_s  <- svydesign(ids = ~hh, weights = ~w_s, data = S)
des_s0 <- svydesign(ids = ~hh, weights = ~w, data = S)        # calibrated weights only
des_s1 <- svydesign(ids = ~1, weights = ~w, data = S)         # as in the earlier scripts (no clustering)
prev <- function(f, des) { r <- svyciprop(f, des, method = "logit"); c(est = 100 * as.numeric(r), 100 * attr(r, "ci")) }
prev_tab <- rbind(`Total Ig (IPW + cluster)` = prev(~igt, des_s), `Total Ig (calibrated weights, cluster)` = prev(~igt, des_s0),
                  `Total Ig (calibrated weights, no cluster)` = prev(~igt, des_s1), `IgG` = prev(~igg, des_s),
                  `Total Ig or IgG` = prev(~any, des_s), `Dengue IgG` = prev(~deng, des_s))
round(prev_tab, 1)
deff_igt <- as.numeric(deff(svymean(~igt, des_s, deff = TRUE)))
pt <- function(r) paste0(fmt(prev_tab[r, 1], 1), "\\% (95\\% CI ", ci_txt(prev_tab[r, 2], prev_tab[r, 3], 1), ")")
num("prevIgT", pt(1)); num("prevIgTpoint", fmt(prev_tab[1, 1], 1)); num("prevIgTcal", pt(2)); num("prevIgTnocluster", pt(3))
num("prevIgG", pt(4)); num("prevAny", pt(5)); num("prevDengue", pt(6))
num("prevIgTunweighted", fmt(100 * mean(S$igt), 1)); num("deffIgT", fmt(deff_igt, 2))
num("respAdult", fmt(100 * mean(d$resp[d$adultq]), 1)); num("respChild", fmt(100 * mean(d$resp[!d$adultq]), 1))
num("respFrench", fmt(100 * mean(d$resp[d$nat3 == "French"]), 1)); num("respComorian", fmt(100 * mean(d$resp[d$nat3 == "Comorian"]), 1))
num("nDiscordA", fint(sum(S$igt == 0 & S$igg == 1))); num("nDiscordB", fint(sum(S$igt == 1 & S$igg == 0)))
unv <- S$adultq & !is.na(S$vacc) & S$vacc == 0; vac <- S$adultq & !is.na(S$vacc) & S$vacc == 1
num("iggVaccinated", fmt(100 * weighted.mean(S$igg[vac], S$w_s[vac]), 1)); num("iggUnvaccinated", fmt(100 * weighted.mean(S$igg[unv], S$w_s[unv]), 1))
num("igtVaccinated", fmt(100 * weighted.mean(S$igt[vac], S$w_s[vac]), 1)); num("igtUnvaccinated", fmt(100 * weighted.mean(S$igt[unv], S$w_s[unv]), 1))
num("pctVaccAdults", fmt(100 * weighted.mean(d$vacc[d$adultq], d$w[d$adultq], na.rm = TRUE), 1))

## Rogan-Gladen correction over a grid of assay characteristics (not known for MayCov)
rg <- expand.grid(se = c(0.90, 0.95, 1), sp = c(0.97, 0.99, 1))
rg$adj <- pmin(pmax((prev_tab[1, 1] / 100 + rg$sp - 1) / (rg$se + rg$sp - 1), 0), 1) * 100
rg
num("rgMin", fmt(min(rg$adj), 1)); num("rgMax", fmt(max(rg$adj), 1))


# --- age_profiles ---
by_age <- function(v, lab) {
  r <- svyby(as.formula(paste0("~", v)), ~age9, des_s, svyciprop, vartype = "ci", method = "logit")
  data.frame(age = r$age9, est = 100 * r[[v]], lo = 100 * r$ci_l, hi = 100 * r$ci_u, marker = lab)
}
ap_df <- rbind(by_age("igt", "SARS-CoV-2 total Ig"), by_age("igg", "SARS-CoV-2 IgG"), by_age("deng", "Dengue IgG"))
ap_df$marker <- factor(ap_df$marker, levels = c("SARS-CoV-2 total Ig", "SARS-CoV-2 IgG", "Dengue IgG"))
ap_df
ag <- function(a, m = "SARS-CoV-2 total Ig") { r <- ap_df[ap_df$age == a & ap_df$marker == m, ]; paste0(fmt(r$est, 1), "\\% (", ci_txt(r$lo, r$hi, 1), ")") }
num("prevAgeSixNine", ag("6-9")); num("prevAgeTenFourteen", ag("10-14")); num("prevAgeFifteenTwentyFour", ag("15-24"))
num("prevAgeSixtyFive", ag("65-74")); num("prevAgeSeventyFive", ag("75+"))
num("dengAgeSixNine", ag("6-9", "Dengue IgG")); num("dengAgeFiftyFive", ag("55-64", "Dengue IgG"))


# --- household_pairs ---
pairs <- inner_join(S[S$adultq, c("hh", "igt", "agp", "nat3", "housing_h", "hhcat_h")] %>% group_by(hh) %>% slice(1) %>% ungroup(),
                    S[!S$adultq, c("hh", "igt", "age6", "female")] %>% group_by(hh) %>% slice(1) %>% ungroup(),
                    by = "hh", suffix = c(".adult", ".child"))
tb <- table(adult = pairs$igt.adult, child = pairs$igt.child); tb
or_pair <- tb[1, 1] * tb[2, 2] / (tb[1, 2] * tb[2, 1]); se_lor <- sqrt(sum(1 / tb))
m_pair <- glm(igt.child ~ igt.adult + age6 + female + nat3 + housing_h + hhcat_h, family = binomial(), data = pairs)
ao <- exp(c(coef(m_pair)["igt.adult"], confint.default(m_pair)["igt.adult", ]))
num("nPairs", fint(nrow(pairs)))
num("orPair", paste0(fmt(or_pair, 2), " (95\\% CI ", ci_txt(exp(log(or_pair) - 1.96 * se_lor), exp(log(or_pair) + 1.96 * se_lor), 2), ")"))
num("orPairAdj", paste0(fmt(ao[1], 2), " (95\\% CI ", ci_txt(ao[2], ao[3], 2), ")"))
num("childPosIfAdultPos", fmt(100 * tb[2, 2] / sum(tb[2, ]), 1)); num("childPosIfAdultNeg", fmt(100 * tb[1, 2] / sum(tb[1, ]), 1))


# --- prevalence_ratios ---
S$sex <- factor(ifelse(S$female == 1, "Female", "Male"), levels = c("Male", "Female"))
S$age6r <- relevel(S$age6, ref = "25-44")
S$ccat <- factor(ifelse(!S$cc_complete, NA, ifelse(S$heap, "Heaped answers",
            as.character(cut(S$tot, c(-1, 0, 9, 29, Inf), labels = c("None", "1-9", "10-29", "30 or more"))))),
            levels = c("None", "1-9", "10-29", "30 or more", "Heaped answers"))
des_s <- svydesign(ids = ~hh, weights = ~w_s, data = S)
f1 <- ~ age6r + sex + nat3 + housing_h + water_h + hhcat_h
pr_fit <- function(outcome, rhs, des) {
  m <- svyglm(update(rhs, as.formula(paste(outcome, "~ ."))), des, family = quasipoisson(link = "log"))
  data.frame(term = names(coef(m)), est = exp(coef(m)), lo = exp(coef(m) - 1.96 * SE(m)),
             hi = exp(coef(m) + 1.96 * SE(m)), p = summary(m)$coefficients[, 4], n = nrow(m$model))[-1, ]
}
pr_cov <- pr_fit("igt", f1, des_s); pr_den <- pr_fit("deng", f1, des_s)
des_a <- subset(des_s, adultq & cc_complete)
f2 <- update(f1, ~ . + employ + ccat)
pr_cov_a <- pr_fit("igt", f2, des_a); pr_den_a <- pr_fit("deng", f2, des_a)
pr_cov; pr_cov_a[grep("ccat|employ", pr_cov_a$term), ]

labels5 <- c("age6r6-9" = "\\quad 6--9", "age6r10-14" = "\\quad 10--14", "age6r15-24" = "\\quad 15--24",
  "age6r45-64" = "\\quad 45--64", "age6r65+" = "\\quad 65 or older", "sexFemale" = "\\quad Female",
  "nat3Comorian" = "\\quad Comorian", "nat3Other or unknown" = "\\quad Other or unknown",
  "housing_hSheet metal or wood" = "\\quad Sheet metal or wood", "water_hOutside dwelling" = "\\quad Outside dwelling",
  "hhcat_h3-4" = "\\quad 3--4", "hhcat_h5-6" = "\\quad 5--6", "hhcat_h7 or more" = "\\quad 7 or more",
  "employEmployed" = "\\quad Employed", "employStudent" = "\\quad Student",
  "ccat1-9" = "\\quad 1--9", "ccat10-29" = "\\quad 10--29", "ccat30 or more" = "\\quad 30 or more",
  "ccatHeaped answers" = "\\quad Heaped answers ($\\geq 100$)")
heads5 <- c("age6r6-9" = "Age, years (ref. 25--44)", "sexFemale" = "Sex (ref. male)", "nat3Comorian" = "Nationality (ref. French)",
  "housing_hSheet metal or wood" = "Housing (ref. permanent)", "water_hOutside dwelling" = "Water access (ref. inside dwelling)",
  "hhcat_h3-4" = "Household size (ref. 1--2)", "employEmployed" = "Employment (ref. not employed)$^{a}$",
  "ccat1-9" = "Contacts outside household (ref. none)$^{a}$")
prc <- function(tab, tm) { r <- tab[tab$term == tm, ]; paste0(fmt(r$est, 2), " (", ci_txt(r$lo, r$hi, 2), ")") }
t5 <- NULL
for (tm in names(labels5)) {
  if (tm %in% names(heads5)) t5 <- rbind(t5, data.frame(v = heads5[[tm]], a = "", b = ""))
  adult_only <- grepl("ccat|employ", tm)
  t5 <- rbind(t5, data.frame(v = labels5[[tm]], a = prc(if (adult_only) pr_cov_a else pr_cov, tm),
                             b = prc(if (adult_only) pr_den_a else pr_den, tm)))
}
names(t5) <- c("Characteristic", "SARS-CoV-2 total Ig, PR", "Dengue IgG, PR")
tex_table(t5, "table_07.tex", align = "lrr", addspace = which(t5[[2]] == "")[-1])
t5
trp <- function(tab, tm) { r <- tab[tab$term == tm, ]; paste0(fmt(r$est, 2), " (95\\% CI ", ci_txt(r$lo, r$hi, 2), ")") }
num("nPRall", fint(pr_cov$n[1])); num("nPRadults", fint(pr_cov_a$n[1]))
num("prOld", trp(pr_cov, "age6r65+")); num("prComorian", trp(pr_cov, "nat3Comorian"))
num("prHHlarge", trp(pr_cov, "hhcat_h7 or more")); num("prWater", trp(pr_cov, "water_hOutside dwelling"))
num("prHousing", trp(pr_cov, "housing_hSheet metal or wood")); num("prFemale", trp(pr_cov, "sexFemale"))
num("prYoungKids", trp(pr_cov, "age6r6-9")); num("prTeens", trp(pr_cov, "age6r15-24"))
num("prDengHHlarge", trp(pr_den, "hhcat_h7 or more")); num("prDengComorian", trp(pr_den, "nat3Comorian"))
num("prDengOld", trp(pr_den, "age6r65+")); num("prDengWater", trp(pr_den, "water_hOutside dwelling"))
num("prDengYoungKids", trp(pr_den, "age6r6-9")); num("prDengHousing", trp(pr_den, "housing_hSheet metal or wood"))
num("prContactsHigh", trp(pr_cov_a, "ccat30 or more")); num("prContactsMid", trp(pr_cov_a, "ccat10-29"))
num("prContactsHeaped", trp(pr_cov_a, "ccatHeaped answers")); num("prDengContactsHigh", trp(pr_den_a, "ccat30 or more"))
num("prEmployed", trp(pr_cov_a, "employEmployed"))



# --- fig4 ---
p4a <- ggplot(ap_df, aes(age, est, colour = marker, shape = marker, group = marker)) +
  geom_line(linewidth = 0.6, position = position_dodge(width = 0.35)) +
  geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 0.4, position = position_dodge(width = 0.35)) +
  geom_point(size = 1.8, position = position_dodge(width = 0.35)) +
  scale_colour_manual(values = unname(pal[c("blue", "orange", "aqua")])) +
  scale_y_continuous(limits = c(30, 100)) + guides(colour = guide_legend(nrow = 2)) +
  labs(title = "a  Seroprevalence by age", x = "Age (years)", y = "Seropositive (%)") + theme_paper +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))
keep4 <- names(labels5)[!grepl("ccat|employ", names(labels5))]
f4b <- rbind(cbind(pr_cov[pr_cov$term %in% keep4, ], outcome = "SARS-CoV-2 total Ig"),
             cbind(pr_den[pr_den$term %in% keep4, ], outcome = "Dengue IgG"))
nice <- c("Age 6-9", "Age 10-14", "Age 15-24", "Age 45-64", "Age 65+", "Female", "Comorian nationality",
          "Other nationality", "Sheet-metal or wood housing", "Water outside dwelling", "Household of 3-4",
          "Household of 5-6", "Household of 7 or more")
f4b$label <- factor(nice[match(f4b$term, keep4)], levels = rev(nice))
f4b$outcome <- factor(f4b$outcome, levels = c("SARS-CoV-2 total Ig", "Dengue IgG"))
p4b <- ggplot(f4b, aes(est, label, colour = outcome, shape = outcome)) +
  geom_vline(xintercept = 1, colour = ink2, linewidth = 0.3) +
  geom_linerange(aes(xmin = lo, xmax = hi), linewidth = 0.45, position = position_dodge(width = 0.55)) +
  geom_point(size = 1.7, position = position_dodge(width = 0.55)) +
  scale_colour_manual(values = c("SARS-CoV-2 total Ig" = unname(pal["blue"]), "Dengue IgG" = unname(pal["aqua"]))) +
  scale_shape_manual(values = c("SARS-CoV-2 total Ig" = 16, "Dengue IgG" = 15)) +
  scale_x_log10(breaks = c(0.7, 0.8, 0.9, 1, 1.1, 1.2)) + guides(colour = guide_legend(nrow = 2)) +
  labs(title = "b  Adjusted prevalence ratios", x = "Prevalence ratio (95% CI), log scale", y = NULL) + theme_paper
fig4 <- p4a + p4b + plot_layout(widths = c(1, 1.05))
save_fig(fig4, "figure_08_serology", h = 3.6)
fig4


# --- final_size ---
obs <- svyby(~igt, ~agp, des_s, svymean)
obs_df <- data.frame(agp = obs$agp, p = obs$igt, se = obs$se)
obs_df$neff <- obs_df$p * (1 - obs_df$p) / obs_df$se^2
deviance_fs <- function(z, p, n) { z <- pmin(pmax(z, 1e-9), 1 - 1e-9); 2 * sum(n * (p * log(p / z) + (1 - p) * log((1 - p) / (1 - z)))) }


# --- seir_data ---
cs <- as.data.frame(suppressMessages(read_excel(inputs[3])))
cs$Date <- as.Date(cs$Date)
stopifnot(all(diff(cs$Date) == 1))
n_days <- nrow(cs); wk_id <- floor((seq_len(n_days) - 1) / 7)
y_w <- as.vector(tapply(cs$Nouveaux_cas, wk_id, sum)); wk_start <- cs$Date[1] + 7 * (seq_along(y_w) - 1)
age_cols <- list(`0-17` = c("0-4ans_cas", "5-9ans_cas", "10-18ans_cas"), `18-24` = "19-24ans_cas",
                 `25-49` = "25-49ans_cas", `50-64` = "50-64ans_cas", `65+` = "65+ans_cas")
cases_age <- sapply(age_cols, function(v) sum(cs[, v]))          # age bands of the series: 0-18, 19-24, 25-49, 50-64, 65+
c(days = n_days, weeks = length(y_w), cases = sum(y_w), cases_with_age = sum(cases_age))
second_wave <- cs$Date >= as.Date("2020-12-28")



# --- seir_model ---
lat_days <- 3; inf_days <- 4.5
knots <- seq(0, n_days, length.out = 15); nK <- length(knots); pen_sd <- 0.5
seir_run <- function(lb, e0, Cn, Np, lam = 1 / lat_days, gam = 1 / inf_days, dt = 1) {
  ## fourth-order Runge-Kutta; Z counts people who have become infectious
  n <- length(Np); nst <- round(n_days / dt); tt <- (0:nst) * dt
  bt <- exp(approx(knots, lb, xout = c(tt, tt[-1] - dt / 2), rule = 2)$y)
  b_full <- bt[seq_len(nst + 1)]; b_half <- bt[-seq_len(nst + 1)]
  S <- Np - e0 * Np / sum(Np); E <- e0 * Np / sum(Np); I <- numeric(n); Z <- numeric(n)
  outS <- matrix(0, nst + 1, n); outZ <- outS; outS[1, ] <- S
  f <- function(S, E, I, b) { foi <- b * S * as.vector(Cn %*% (I / Np)); list(-foi, foi - lam * E, lam * E - gam * I, lam * E) }
  for (k in seq_len(nst)) {
    k1 <- f(S, E, I, b_full[k])
    k2 <- f(S + dt / 2 * k1[[1]], E + dt / 2 * k1[[2]], I + dt / 2 * k1[[3]], b_half[k])
    k3 <- f(S + dt / 2 * k2[[1]], E + dt / 2 * k2[[2]], I + dt / 2 * k2[[3]], b_half[k])
    k4 <- f(S + dt * k3[[1]], E + dt * k3[[2]], I + dt * k3[[3]], b_full[k + 1])
    S <- S + dt / 6 * (k1[[1]] + 2 * k2[[1]] + 2 * k3[[1]] + k4[[1]]); E <- E + dt / 6 * (k1[[2]] + 2 * k2[[2]] + 2 * k3[[2]] + k4[[2]])
    I <- I + dt / 6 * (k1[[3]] + 2 * k2[[3]] + 2 * k3[[3]] + k4[[3]]); Z <- Z + dt / 6 * (k1[[4]] + 2 * k2[[4]] + 2 * k3[[4]] + k4[[4]])
    outS[k + 1, ] <- S; outZ[k + 1, ] <- Z
  }
  list(t = tt, S = outS, Z = outZ, beta = b_full)
}
weekly_mean <- function(run, a) a * diff(rowSums(run$Z)[seq(0, n_days, by = 7) + 1])
seir_nll <- function(par, Cn, Np, a, ...) {
  m <- pmax(weekly_mean(seir_run(par[1:nK], exp(par[nK + 1]), Cn, Np, ...), a), 1e-8)
  -sum(dnbinom(y_w, size = exp(par[nK + 2]), mu = m, log = TRUE)) + sum(diff(par[1:nK])^2) / (2 * pen_sd^2)
}
fit_seir <- function(Cn, Np, a, ...) {
  gam <- list(...)$gam; if (is.null(gam)) gam <- 1 / inf_days
  o <- optim(c(rep(log(1.2 * gam), nK), log(20 / a), log(10)), seir_nll, Cn = Cn, Np = Np, a = a, ...,
             method = "BFGS", control = list(maxit = 500, reltol = 1e-10))
  for (r in 1:3) {
    o2 <- optim(o$par, seir_nll, Cn = Cn, Np = Np, a = a, ..., method = "BFGS", control = list(maxit = 500, reltol = 1e-12))
    done <- abs(o$value - o2$value) < 1e-6; o <- o2; if (done) break
  }
  o
}
unit_sr <- function(Cm) Cm / max(Re(eigen(Cm, only.values = TRUE)$values))
seir_mats <- list(ref = unit_sr(Cs0), e50 = unit_sr(Cs50),
                  naive = unit_sr(reconcile(st0$M_naive, st0$H, "arithmetic")), hom = matrix(1))
seir_pops <- list(ref = N, e50 = N, naive = N, hom = sum(N))
inf_sero <- sum(obs_df$p * N)                                    # infections implied by age-specific seroprevalence
a_anchor <- sum(y_w) / inf_sero
c(infections_implied_by_serology = inf_sero, ascertainment_anchor = a_anchor)



# --- seir_fits ---
seir_jobs <- rbind(expand.grid(mat = "ref", a = c(1, 0.5, 0.25, 0.15, a_anchor), gen = "base", stringsAsFactors = FALSE),
                   expand.grid(mat = c("e50", "naive", "hom"), a = c(1, a_anchor), gen = "base", stringsAsFactors = FALSE),
                   data.frame(mat = "ref", a = a_anchor, gen = "short"))
cache_f <- "cache/seir_fits.rds"
if (params$use_cache && file.exists(cache_f)) seir_fits <- readRDS(cache_f) else {
  seir_fits <- mclapply(seq_len(nrow(seir_jobs)), function(i) {
    if (seir_jobs$gen[i] == "base") fit_seir(seir_mats[[seir_jobs$mat[i]]], seir_pops[[seir_jobs$mat[i]]], seir_jobs$a[i])
    else fit_seir(seir_mats[[seir_jobs$mat[i]]], seir_pops[[seir_jobs$mat[i]]], seir_jobs$a[i], lam = 1 / 2.5, gam = 1 / 3)
  }, mc.cores = n_cores)
  saveRDS(seir_fits, cache_f)
}
stopifnot(all(sapply(seir_fits, function(o) o$convergence) == 0))
seir_out <- function(i) {
  o <- seir_fits[[i]]; Cn <- seir_mats[[seir_jobs$mat[i]]]; Np <- seir_pops[[seir_jobs$mat[i]]]; a <- seir_jobs$a[i]
  lam <- if (seir_jobs$gen[i] == "base") 1 / lat_days else 1 / 2.5; gam <- if (seir_jobs$gen[i] == "base") 1 / inf_days else 1 / 3
  run <- seir_run(o$par[1:nK], exp(o$par[nK + 1]), Cn, Np, lam = lam, gam = gam)
  m <- weekly_mean(run, a); n <- length(Np)
  reff <- sapply(seq_len(n_days + 1), function(k) run$beta[k] / gam *
                   (if (n == 1) run$S[k, 1] / Np else max(Re(eigen(Cn * (run$S[k, ] / Np), only.values = TRUE)$values))))
  list(fitted = m, loglik = sum(dnbinom(y_w, size = exp(o$par[nK + 2]), mu = m, log = TRUE)), k = exp(o$par[nK + 2]),
       inf = run$Z[n_days + 1, ], ar = run$Z[n_days + 1, ] / Np, r0 = run$beta / gam, reff = reff,
       date = cs$Date[1] + 0:n_days)
}
seir_res <- lapply(seq_len(nrow(seir_jobs)), seir_out)
seir_tab <- cbind(seir_jobs, loglik = sapply(seir_res, function(r) r$loglik),
                  attack = sapply(seq_along(seir_res), function(i) sum(seir_res[[i]]$inf) / sum(N)),
                  sero_dev = sapply(seir_res, function(r) deviance_fs(rep_len(r$ar, 5), obs_df$p, obs_df$neff)),
                  r0_peak2 = sapply(seir_res, function(r) max(r$r0[r$date >= as.Date("2020-12-28")])),
                  r0_end = sapply(seir_res, function(r) r$r0[n_days + 1]),
                  reff_peak2 = sapply(seir_res, function(r) max(r$reff[r$date >= as.Date("2020-12-28")])),
                  reff_end = sapply(seir_res, function(r) r$reff[n_days + 1]))
seir_tab
jid <- function(mat, a, gen = "base") which(seir_jobs$mat == mat & abs(seir_jobs$a - a) < 1e-9 & seir_jobs$gen == gen)
r_anchor <- seir_res[[jid("ref", a_anchor)]]; r_full <- seir_res[[jid("ref", 1)]]; r_hom <- seir_res[[jid("hom", a_anchor)]]


# --- table7 ---
inf_bands <- r_anchor$inf; inf_bands[1] <- inf_bands[1] + inf_bands[2] / 7; inf_bands[2] <- inf_bands[2] * 6 / 7
asc_age <- cases_age / inf_bands
t7 <- data.frame(Age = c("0--17", "18--24", "25--49", "50--64", "65 or older", "All ages"),
  N = fint(c(N, sum(N))), cases = fint(c(cases_age, sum(cases_age))),
  share = fmt(100 * c(cases_age / sum(cases_age), 1), 1),
  sero = c(fmt(100 * obs_df$p, 1), fmt(100 * inf_sero / sum(N), 1)),
  ar = fmt(100 * c(r_anchor$ar, sum(r_anchor$inf) / sum(N)), 1),
  ar50 = fmt(100 * c(seir_res[[jid("e50", a_anchor)]]$ar, sum(seir_res[[jid("e50", a_anchor)]]$inf) / sum(N)), 1),
  arn = fmt(100 * c(seir_res[[jid("naive", a_anchor)]]$ar, sum(seir_res[[jid("naive", a_anchor)]]$inf) / sum(N)), 1),
  asc = fmt(100 * c(asc_age, sum(cases_age) / sum(inf_bands)), 1))
names(t7) <- c("Age, years", "Population", "Cases", "\\% of cases", "Seroprevalence, \\%", "Infected$^{a}$, E100",
               "Infected$^{a}$, E50", "Infected$^{a}$, as reported", "Cases per 100 infections$^{b}$")
tex_table(t7, "table_08.tex", align = "lrrrrrrrr", addspace = 6)
t7
sm <- c("ref", "e50", "naive", "hom")
sg <- function(x, d = 2) sub("^-", "$-$", fmt(x, d))
ts7 <- data.frame(
  Quantity = c("Log-likelihood, $a=1$", "Log-likelihood, anchored", "Serological deviance, anchored",
               "Peak $\\mathcal R_0$, second wave, $a=1$", "Peak $\\mathcal R_0$, second wave, anchored",
               "$\\mathcal R_0$ on 21 March 2021, $a=1$", "$\\mathcal R_0$ on 21 March 2021, anchored"),
  rbind(sapply(sm, function(m) sg(seir_tab$loglik[jid(m, 1)])), sapply(sm, function(m) sg(seir_tab$loglik[jid(m, a_anchor)])),
        sapply(sm, function(m) fmt(seir_tab$sero_dev[jid(m, a_anchor)], 1)),
        sapply(sm, function(m) fmt(seir_tab$r0_peak2[jid(m, 1)], 2)), sapply(sm, function(m) fmt(seir_tab$r0_peak2[jid(m, a_anchor)], 2)),
        sapply(sm, function(m) fmt(seir_tab$r0_end[jid(m, 1)], 2)), sapply(sm, function(m) fmt(seir_tab$r0_end[jid(m, a_anchor)], 2))),
  row.names = NULL, check.names = FALSE)
names(ts7) <- c("", "Reconciled, E100", "Reconciled, E50", "As reported", "Homogeneous")
tex_table(ts7, "table_14.tex", align = "lrrrr", addspace = c(3, 4))
ts7

num("seirDays", fint(n_days)); num("seirWeeks", length(y_w)); num("seirCases", fint(sum(y_w)))
num("seirCasesSecondPct", fmt(100 * sum(cs$Nouveaux_cas[second_wave]) / sum(y_w), 0))
num("seirPeakWeekCases", fint(max(y_w))); num("seirPeakWeek", trimws(format(wk_start[which.max(y_w)], "%e %B %Y")))
num("seirAnchorPct", fmt(100 * a_anchor, 1)); num("seirInfSero", fint(round(inf_sero, -2)))
num("seirOnePer", fmt(1 / a_anchor, 0))
ll_all <- seir_tab$loglik[seir_tab$gen == "base"]
num("seirLLmin", sg(min(ll_all), 1)); num("seirLLmax", sg(max(ll_all), 1)); num("seirLLrange", fmt(diff(range(ll_all)), 2))
num("seirNfits", sum(seir_tab$gen == "base"))
num("seirLLrefFull", fmt(seir_tab$loglik[jid("ref", 1)], 1)); num("seirLLhomFull", fmt(seir_tab$loglik[jid("hom", 1)], 1))
num("seirDisp", fmt(r_anchor$k, 0))
sr <- function(mat, a, v, d = 2) fmt(seir_tab[[v]][jid(mat, a)], d)
num("seirRpeakFull", sr("ref", 1, "r0_peak2")); num("seirRpeakAnchor", sr("ref", a_anchor, "r0_peak2"))
num("seirRendFull", sr("ref", 1, "r0_end")); num("seirRendAnchor", sr("ref", a_anchor, "r0_end"))
num("seirRpeakQuarter", sr("ref", 0.25, "r0_peak2")); num("seirRendQuarter", sr("ref", 0.25, "r0_end"))
num("seirReffPeakFull", sr("ref", 1, "reff_peak2")); num("seirReffPeakAnchor", sr("ref", a_anchor, "reff_peak2"))
num("seirReffEndFull", sr("ref", 1, "reff_end")); num("seirReffEndAnchor", sr("ref", a_anchor, "reff_end"))
num("seirRpeakHomAnchor", sr("hom", a_anchor, "r0_peak2")); num("seirRpeakNaiveAnchor", sr("naive", a_anchor, "r0_peak2"))
num("seirRendNaiveAnchor", sr("naive", a_anchor, "r0_end"))
num("seirDevRef", sr("ref", a_anchor, "sero_dev", 1)); num("seirDevFifty", sr("e50", a_anchor, "sero_dev", 1))
num("seirDevNaive", sr("naive", a_anchor, "sero_dev", 0)); num("seirDevHom", sr("hom", a_anchor, "sero_dev", 1))
num("seirARkids", fmt(100 * r_anchor$ar[1], 1)); num("seirARyoung", fmt(100 * r_anchor$ar[2], 1)); num("seirARmid", fmt(100 * r_anchor$ar[3], 1))
num("seirARfifty", fmt(100 * r_anchor$ar[4], 1)); num("seirARold", fmt(100 * r_anchor$ar[5], 1))
num("seirARoldNaive", fmt(100 * seir_res[[jid("naive", a_anchor)]]$ar[5], 0)); num("seirARkidsNaive", fmt(100 * seir_res[[jid("naive", a_anchor)]]$ar[1], 0))
num("seirARoldFifty", fmt(100 * seir_res[[jid("e50", a_anchor)]]$ar[5], 1))
num("seirARfull", fmt(100 * sum(r_full$inf) / sum(N), 1)); num("seirARhom", fmt(100 * r_hom$ar, 1))
num("seirSusOld", fmt(100 * (1 - r_anchor$ar[5]), 0)); num("seirSusKids", fmt(100 * (1 - r_anchor$ar[1]), 0))
num("seirAscKids", fmt(100 * asc_age[1], 1)); num("seirAscYoung", fmt(100 * asc_age[2], 1)); num("seirAscMid", fmt(100 * asc_age[3], 1))
num("seirAscFifty", fmt(100 * asc_age[4], 1)); num("seirAscOld", fmt(100 * asc_age[5], 1))
num("seirAscRatioOldKids", fmt(asc_age[5] / asc_age[1], 0))
num("seirCasesKidsPct", fmt(100 * cases_age[1] / sum(cases_age), 0)); num("seirCasesMidPct", fmt(100 * cases_age[3] / sum(cases_age), 0))
r_short <- seir_res[[jid("ref", a_anchor, "short")]]
num("seirARoldShort", fmt(100 * r_short$ar[5], 1)); num("seirRpeakShort", fmt(seir_tab$r0_peak2[jid("ref", a_anchor, "short")], 2))
num("seirDevShort", fmt(seir_tab$sero_dev[jid("ref", a_anchor, "short")], 1))



# --- fig6 ---
d6a <- data.frame(week = wk_start, obs = y_w, fit = r_anchor$fitted)
p6a <- ggplot(d6a, aes(week)) +
  geom_point(aes(y = pmax(obs, 1), shape = "Confirmed cases"), colour = ink, size = 1.2) +
  geom_line(aes(y = fit, colour = "Fitted SEIR model"), linewidth = 0.7) +
  scale_y_log10(breaks = c(1, 10, 100, 1000)) + scale_x_date(date_breaks = "2 months", date_labels = "%b %y") +
  scale_colour_manual(values = c("Fitted SEIR model" = unname(pal["blue"]))) + scale_shape_manual(values = c("Confirmed cases" = 16)) +
  labs(title = "a  Weekly confirmed cases", x = NULL, y = "Cases per week (log scale)") + theme_paper
keep_t <- r_anchor$date >= cs$Date[1] + 28
d6b <- rbind(data.frame(date = r_full$date, v = r_full$reff, s = "Effective, complete ascertainment"),
             data.frame(date = r_anchor$date, v = r_anchor$reff, s = "Effective, serology-anchored"),
             data.frame(date = r_full$date, v = r_full$r0, s = "Fully susceptible, complete ascertainment"),
             data.frame(date = r_anchor$date, v = r_anchor$r0, s = "Fully susceptible, serology-anchored"))[rep(keep_t, 4), ]
lv6 <- c("Effective, complete ascertainment", "Effective, serology-anchored",
         "Fully susceptible, complete ascertainment", "Fully susceptible, serology-anchored")
d6b$s <- factor(d6b$s, levels = lv6)
p6b <- ggplot(d6b, aes(date, v, colour = s, linetype = s)) +
  geom_hline(yintercept = 1, colour = ink2, linewidth = 0.3) +
  geom_vline(xintercept = as.Date("2021-02-05"), linetype = "dotted", colour = ink2, linewidth = 0.4) +
  annotate("text", x = as.Date("2021-01-28"), y = 0.52, label = "lockdown\n5 Feb 2021", hjust = 1, size = 2.3, colour = ink2, lineheight = 0.9) +
  geom_line(linewidth = 0.7) +
  scale_colour_manual(values = setNames(unname(pal[c("orange", "blue", "orange", "blue")]), lv6)) +
  scale_linetype_manual(values = setNames(c("solid", "solid", "dashed", "dashed"), lv6)) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %y") + coord_cartesian(ylim = c(0.4, 3)) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(title = "b  Reproduction number", x = NULL, y = "Reproduction number") + theme_paper +
  theme(legend.key.width = unit(8, "mm"))
lab6 <- c(ref = "Reconciled matrix, E100", e50 = "Reconciled matrix, E50", naive = "As reported, arithmetic", hom = "Homogeneous SEIR")
d6c <- do.call(rbind, lapply(names(lab6), function(m) data.frame(agp = factor(grp, levels = grp),
          pred = 100 * rep_len(seir_res[[jid(m, a_anchor)]]$ar, 5), s = lab6[[m]])))
d6c$s <- factor(d6c$s, levels = lab6)
p6c <- ggplot() +
  geom_linerange(data = obs_plot, aes(agp, ymin = lo, ymax = hi), colour = ink, linewidth = 2.2, alpha = 0.18) +
  geom_point(data = obs_plot, aes(agp, p), colour = ink, shape = 95, size = 7) +
  geom_line(data = d6c, aes(agp, pred, colour = s, group = s, linetype = s), linewidth = 0.6) +
  geom_point(data = d6c, aes(agp, pred, colour = s, shape = s), size = 1.9) +
  scale_colour_manual(values = setNames(c(unname(pal[c("blue", "violet", "orange")]), ink2), lab6)) +
  scale_linetype_manual(values = setNames(c(1, 1, 1, 2), lab6)) + scale_shape_manual(values = setNames(c(16, 18, 17, 4), lab6)) +
  coord_cartesian(ylim = c(35, 100)) + guides(colour = guide_legend(ncol = 2)) +
  labs(title = "c  Cumulative infections by age",
       x = "Age group (years; 0-17 observed in 6-17)", y = "Infected / total Ig positive (%)") + theme_paper
d6d <- data.frame(agp = factor(grp, levels = grp), asc = 100 * asc_age)
p6d <- ggplot(d6d, aes(agp, asc)) + geom_col(fill = pal["blue"], width = 0.6) +
  geom_text(aes(label = fmt(asc, 1)), vjust = -0.5, size = 2.6, colour = ink) +
  scale_y_continuous(limits = c(0, max(d6d$asc) * 1.15), expand = expansion(mult = c(0, 0.02))) +
  labs(title = "d  Confirmed cases per 100 infections", x = "Age group (years)", y = "Cases per 100 infections") + theme_paper
fig6 <- (p6a + p6b) / (p6c + p6d)
save_fig(fig6, "figure_09_seir", h = 6.4)
fig6


# --- export ---
macro <- sprintf("\\newcommand{\\%s}{%s}", names(NUM), unlist(NUM))
writeLines(c("% Generated by reproduce_maycov.R -- do not edit by hand", macro), "tables/numbers.tex")
length(NUM)
sessionInfo()

# Sensitivity to the counts of excluded respondents, conditional on complete items.
write_result <- function(value, name) {
  write.csv(value, file.path("tables", paste0(name, ".csv")), row.names = FALSE,
            fileEncoding = "UTF-8")
}
rules <- c("As reported", "Capped at 100", "E100", "E50")
retained <- list(rep(TRUE, nrow(A)), rep(TRUE, nrow(A)), !A$heap, A$maxans <= 50)
report_sensitivity <- function(rule) {
  keep <- retained[[match(rule, rules)]]
  q <- vapply(grp, function(g) {
    in_group <- A$agp == g
    sum(A$w[in_group & !keep]) / sum(A$w[in_group])
  }, numeric(1))
  scenarios <- list(A2 = rep(1, 5), all_half = rep(.5, 5),
                    all_one_half = rep(1.5, 5), all_double = rep(2, 5),
                    older_half = c(1, 1, 1, 1, .5), older_double = c(1, 1, 1, 1, 2))
  M <- wmean_by(A[keep, ], cvar)
  do.call(rbind, lapply(names(scenarios), function(name) {
    modified <- M * (1 + q * (scenarios[[name]] - 1))
    data.frame(rule, scenario = name, eta = unname(elasticity(modified)["beta"]),
               mean_abs_log_ratio = recip_gap(modified),
               as.list(setNames(rowSums(modified), paste0("mean_", grp))),
               as.list(setNames(recip_ratio(modified),
                                 paste(grp[adult_pairs[, 1]], grp[adult_pairs[, 2]], sep = "_vs_"))))
  }))
}
write_result(do.call(rbind, lapply(rules[3:4], report_sensitivity)), "sensitivity_excluded")

# Figures 5 and 6: age profiles and paired household resampling.
omega_values <- seq(0, 1, by = .05)
kappa_values <- seq(.5, 2, by = .25)
candidates <- rbind(
  do.call(rbind, lapply(seq_along(rules), function(i) {
    data.frame(rule = rules[i], method = "anchoring",
               expand.grid(omega = omega_values, kappa = if (i >= 3) kappa_values else 1))
  })),
  do.call(rbind, lapply(seq_along(rules), function(i) {
    data.frame(rule = rules[i], method = "arithmetic", omega = NA,
               kappa = if (i >= 3) kappa_values else 1)
  })),
  data.frame(rule = c("Household only", "Homogeneous"), method = "comparator",
             omega = NA, kappa = 1)
)
anchored <- function(rule, kappa = 1) {
  which(candidates$rule == rule & candidates$method == "anchoring" &
          candidates$kappa == kappa)
}
arithmetic <- which(candidates$method == "arithmetic" & candidates$kappa == 1)
household_only <- which(candidates$rule == "Household only")
homogeneous <- which(candidates$rule == "Homogeneous")
hh_index <- match(d$hh, hh_ids)
serology_observed <- !is.na(d$igt)
# Match the treatment coding of the revision scripts for the penalized bootstrap.
nationality <- factor(ifelse(d$nat3 == "Other or unknown", "Other", as.character(d$nat3)),
                      levels = c("Comorian", "French", "Other"))
X_response <- model.matrix(~ adultq * female + factor(raw$cate_age) + nationality,
                           data = d)
stopifnot(nrow(X_response) == nrow(d), all(d$igt[serology_observed] %in% 0:1))
response_fit <- function(multiplicity) {
  # There are few distinct design rows. Aggregate the likelihood by pattern.
  successes <- as.numeric(rowsum(matrix(multiplicity * serology_observed, ncol = 1), patterns, reorder = FALSE))
  trials <- as.numeric(rowsum(matrix(multiplicity, ncol = 1), patterns, reorder = FALSE))
  penalty <- c(0, rep(.001, ncol(X_unique)-1))
  objective <- function(b) {
    eta <- drop(X_unique %*% b)
    sum(trials * (pmax(eta, 0) + log1p(exp(-abs(eta)))) - successes * eta) +
      sum(penalty*b^2)/2
  }
  derivative <- function(b) {
    drop(crossprod(X_unique, trials * plogis(X_unique %*% b) - successes)) + penalty*b
  }
  fit <- optim(rep(0, ncol(X_unique)), objective, derivative, method = "BFGS",
               control = list(maxit = 1000, reltol = 1e-12))
  if (fit$convergence != 0) stop("Serology response model failed to converge")
  plogis(drop(X_response %*% fit$par))
}
patterns <- match(do.call(paste, as.data.frame(X_response)),
                  unique(do.call(paste, as.data.frame(X_response))))
X_unique <- X_response[!duplicated(patterns), , drop = FALSE]

# Fit all matrices together, using the same golden-section search as the revision.
fit_grid <- function(matrices, p, ne) {
  rows <- t(vapply(matrices, rowSums, numeric(5)))
  kernels <- lapply(1:5, function(i) t(vapply(matrices, function(m) m[i, ], numeric(5))))
  solve_size <- function(logq) {
    q <- exp(logq)
    z <- matrix(1, nrow(rows), 5)
    for (iter in seq_len(600L)) {
      next_z <- -expm1(-q * vapply(kernels, function(kernel) rowSums(kernel*z), numeric(nrow(z))))
      if (max(abs(next_z-z)) < 1e-9) break
      z <- next_z
    }
    pmin(pmax(next_z, 1e-9), 1-1e-9)
  }
  loss <- function(z) {
    # Bernoulli cross-entropy minus the saturated model; handles p = 0 or 1.
    cross_entropy <- -drop(log(z) %*% (ne*p) + log1p(-z) %*% (ne*(1-p)))
    saturated <- sum(ne * (ifelse(p == 0, 0, p*log(p)) +
                            ifelse(p == 1, 0, (1-p)*log1p(-p))))
    pmax(0, 2*(cross_entropy+saturated))
  }
  rho <- vapply(matrices, function(m) max(Re(eigen(m, only.values = TRUE)$values)), numeric(1))
  lower <- log((1+1e-8)/rho)
  upper <- log(8/apply(rows, 1, min))
  ratio <- (sqrt(5)-1)/2
  for (iter in seq_len(30L)) {
    left <- upper-ratio*(upper-lower)
    right <- lower+ratio*(upper-lower)
    better_left <- loss(solve_size(left)) < loss(solve_size(right))
    lower[!better_left] <- left[!better_left]
    upper[better_left] <- right[better_left]
  }
  logq <- (lower+upper)/2
  z <- solve_size(logq)
  list(score = loss(z), fitted = z, q = exp(logq))
}

compare_matrices <- function(counts, per_day = .5, point = FALSE) {
  multiplicity <- counts[hh_index]
  dd <- d
  dd$w <- d$w * multiplicity
  s <- contact_stats(dd)
  means <- s[c("M_naive", "M_cap", "M_core", "M_s50")]
  beta <- vapply(means, function(m) unname(elasticity(m)["beta"]), numeric(1))
  matrices <- lapply(seq_len(nrow(candidates)), function(i) {
    row <- candidates[i, ]
    if (i == household_only) return((s$H*N+t(s$H*N))/(2*N))
    if (i == homogeneous) return(matrix(N/sum(N), 5, 5, byrow = TRUE))
    j <- match(row$rule, rules)
    reconcile(means[[j]], s$H, method = row$method, omega = row$omega,
              beta = beta[j], kappa = row$kappa, per_day = per_day)
  })
  stopifnot(all(vapply(matrices, function(m) {
    totals <- m*N
    all(is.finite(totals)) && max(abs(totals-t(totals))) < 1e-7
  }, logical(1))))
  probability <- if (point) d$p_resp else response_fit(multiplicity)
  wh <- d$w/probability
  summary <- vapply(grp, function(g) {
    idx <- which(serology_observed & d$agp == g)
    w <- wh[idx] * multiplicity[idx]
    p <- weighted.mean(d$igt[idx], w)
    cluster <- rowsum(matrix(wh[idx]*(d$igt[idx]-p), ncol = 1), hh_index[idx], reorder = FALSE)
    weights <- counts[as.integer(rownames(cluster))]
    variance <- sum(weights*(cluster/sum(w))^2)*length(counts)/(length(counts)-1)
    c(p = p, neff = p*(1-p)/variance)
  }, numeric(2))
  stopifnot(all(is.finite(summary)), all(summary[2, ] > 0))
  result <- fit_grid(matrices, summary[1, ], summary[2, ])
  result$p <- summary[1, ]
  result$neff <- summary[2, ]
  result
}

message("Calculating the serology grid and paired household bootstrap.")
point_counts <- rep(1L, length(hh_ids))
grid_point <- compare_matrices(point_counts, point = TRUE)
perday <- compare_matrices(point_counts, per_day = 1, point = TRUE)
grid_boot <- lapply(seq_len(params$B), function(b) {
  if (b %% 50L == 0L) message("Serology bootstrap ", b, " / ", params$B)
  compare_matrices(tabulate(draws[, b], nbins = length(hh_ids)))
})
scores <- do.call(rbind, lapply(grid_boot, "[[", "score"))
interval <- function(x) unname(quantile(x, c(.025, .975)))
limits <- t(apply(scores, 2, interval))
grid_results <- transform(candidates, deviance = grid_point$score, q = grid_point$q,
                          low = limits[, 1], high = limits[, 2],
                          deviance_per_day_1 = perday$score)
write_result(data.frame(age = grp, prevalence = grid_point$p, neff = grid_point$neff), "serology_observed")
colnames(scores) <- paste0("candidate_", seq_len(ncol(scores)))
grid_results$candidate <- colnames(scores)
write_result(grid_results, "serology_grid")
write_result(as.data.frame(scores), "serology_bootstrap_scores")
minima <- do.call(rbind, lapply(rules[3:4], function(rule) {
  do.call(rbind, lapply(kappa_values, function(kappa) {
    idx <- anchored(rule, kappa)
    i <- idx[which.min(grid_point$score[idx])]
    winner <- max.col(-scores[, idx, drop = FALSE], ties.method = "first")
    data.frame(rule, kappa, omega = candidates$omega[i], deviance = grid_point$score[i],
               bootstrap_fraction_omega_le_025 = mean(omega_values[winner] <= .25))
  }))
}))
write_result(minima, "serology_grid_minima")

ref <- vapply(rules, function(rule) anchored(rule)[1], integer(1))
table5_num <- do.call(rbind, lapply(seq_along(rules), function(i) {
  rule <- rules[i]
  M <- st0[[c("M_naive", "M_cap", "M_core", "M_s50")[i]]]
  values <- A$tot
  if (i == 2) values <- rowSums(pmin(as.matrix(A[, cvar]), 100))
  keep <- retained[[i]]
  diff <- interval(scores[, arithmetic[i]] - scores[, ref[i]])
  eta_ci <- if (i != 2) bq(c("el_naive.beta", "", "el.beta", "el50.beta")[i], exact = TRUE)
  gap_ci <- if (i != 2) bq(c("gap.naive", "", "gap.core", "gap.s50")[i], exact = TRUE)
  ratio_ci <- if (i != 2) bq(c("^recn\\.", "^$", "^rec\\.", "^rec50\\.")[i])
  total_draws <- vapply(seq_len(params$B), function(b) {
    w <- A$w * tabulate(draws[, b], nbins = length(hh_ids))[match(A$hh, hh_ids)]
    weighted.mean(values[keep], w[keep])
  }, numeric(1))
  total_ci <- interval(total_draws)
  data.frame(rule, n = sum(keep), mean_2day = weighted.mean(values[keep], A$w[keep]),
             mean_low = total_ci[1], mean_high = total_ci[2],
             eta = unname(elasticity(M)[1]), eta_low = if (length(eta_ci)) eta_ci[1] else NA,
             eta_high = if (length(eta_ci)) eta_ci[2] else NA,
             r2 = unname(elasticity(M)[2]), ratio_min = min(recip_ratio(M)),
             ratio_max = max(recip_ratio(M)),
             intervals_covering_one = if (length(ratio_ci)) sum(ratio_ci[, 1] <= 1 & ratio_ci[, 2] >= 1) else NA,
             mean_absolute_log_ratio = recip_gap(M),
             gap_low = if (length(gap_ci)) gap_ci[1] else NA,
             gap_high = if (length(gap_ci)) gap_ci[2] else NA,
             daily_contacts = unname(mat_summary(reconcile(M, st0$H))["mean_contacts"]),
             arithmetic = grid_point$score[arithmetic[i]], anchoring = grid_point$score[ref[i]],
             arithmetic_minus_anchoring = grid_point$score[arithmetic[i]]-grid_point$score[ref[i]],
             difference_low = diff[1], difference_high = diff[2],
             minimum_common_grid = if (i >= 3) min(grid_point$score[anchored(rule)]) else NA_real_)
}))
write_result(table5_num, "table_05")
# Compare against rounded point estimates printed in V12.
reference_check <- data.frame(
  quantity = c("n_complete", "n_E100", "n_E50", "mean_raw", "mean_E100", "mean_E50",
               "eta_E100", "eta_E50", "household_deviance",
               "min_E100_kappa_1", "min_E50_kappa_1", "min_E100_kappa_2", "min_E50_kappa_2"),
  manuscript = c(3396, 3002, 2969, 133.7, 14.8, 13.3, .76, .66, 8.26, .8, 1.7, 18, 19.9),
  computed = c(table5_num$n[c(1, 3, 4)], table5_num$mean_2day[c(1, 3, 4)],
               table5_num$eta[3:4], grid_point$score[household_only],
               minima$deviance[minima$kappa == 1], minima$deviance[minima$kappa == 2]),
  tolerance = c(rep(0, 3), rep(.051, 3), .0051, .0051, .03, rep(.06, 4))
)
reference_check$within_tolerance <- with(reference_check, abs(computed-manuscript) <= tolerance)
write_result(reference_check, "verification_V12")
if (any(!reference_check$within_tolerance)) {
  warning("Some point estimates differ from V12. See tables/verification_V12.csv.")
}
contrast <- function(a, b, label) {
  ci <- interval(scores[, a]-scores[, b])
  data.frame(contrast = label, estimate = grid_point$score[a]-grid_point$score[b],
             low = ci[1], high = ci[2])
}
write_result(rbind(contrast(ref[3], ref[4], "E100 minus E50 at omega 0, kappa 1"),
                   contrast(household_only, ref[3], "Household only minus E100"),
                   contrast(household_only, ref[4], "Household only minus E50")),
             "serology_paired_contrasts")

plot_grid <- subset(grid_results, rule %in% rules[3:4] & method == "anchoring")
fig_grid <- ggplot(plot_grid, aes(omega, kappa, fill = log1p(deviance))) +
  geom_tile() + geom_path(data = minima, aes(omega, kappa, group = rule),
                         inherit.aes = FALSE, colour = "white") +
  geom_point(data = minima, aes(omega, kappa), inherit.aes = FALSE, colour = "white", size = 1) +
  facet_wrap(~rule) + scale_fill_viridis_c(name = "log(1 + deviance)") +
  scale_y_continuous(breaks = kappa_values) +
  labs(x = expression(omega), y = expression(kappa),
       caption = sprintf("White line: minimum over omega at each kappa. Household-only deviance: %.2f.",
                         grid_point$score[household_only])) +
  theme_paper + theme(legend.position = "right", legend.title = element_text())
save_fig(fig_grid, "figure_05_grid", 9, 4.1)

age_labels <- c("Observed (anti-N)", "As reported, arithmetic", "E100, arithmetic",
                "E100, omega = 0", "E50, omega = 0", "Household only")
selected <- c(NA, arithmetic[1], arithmetic[3], ref[3], ref[4], household_only)
profiles <- do.call(rbind, lapply(seq_along(selected), function(i) {
  j <- selected[i]
  extract <- function(result) if (is.na(j)) result$p else result$fitted[j, ]
  band <- t(apply(do.call(rbind, lapply(grid_boot, extract)), 2, interval))
  data.frame(scenario = factor(age_labels[i], levels = age_labels), age = grp,
             x = seq_along(grp) + .075*(i-3.5), estimate = extract(grid_point),
             low = band[, 1], high = band[, 2])
}))
write_result(profiles, "figure_06_age_profiles")
curves <- subset(grid_results, method == "anchoring" & kappa == 1)
curves$rule <- factor(curves$rule, levels = rules)
write_result(curves, "figure_06_deviance_profiles")
age_plot <- ggplot(profiles, aes(x, 100*estimate, colour = scenario)) +
  geom_line() + geom_point(size = 1.5) +
  geom_errorbar(aes(ymin = 100*low, ymax = 100*high), width = 0, alpha = .6) +
  scale_x_continuous(breaks = seq_along(grp), labels = grp) +
  scale_colour_manual(values = c("#151515", "#9d3c42", "#df9231", "#14649a", "#147e69", "#73508c")) +
  labs(title = "A  Age profiles", x = "Age group (years)", y = "Prevalence or fitted final size (%)") +
  theme_paper
symlog <- scales::trans_new("deviance", function(x) ifelse(x <= 1, x, 1 + log10(pmax(1, x))),
                            function(x) ifelse(x <= 1, x, 10^(x-1)))
curve_plot <- ggplot(curves, aes(omega, deviance, colour = rule, fill = rule)) +
  geom_ribbon(aes(ymin = low, ymax = high), colour = NA, alpha = .13, show.legend = FALSE) +
  geom_line() + scale_y_continuous(trans = symlog, breaks = c(1, 10, 100, 1000)) +
  scale_colour_manual(values = c("#9d3c42", "#df9231", "#14649a", "#147e69")) +
  scale_fill_manual(values = c("#9d3c42", "#df9231", "#14649a", "#147e69")) +
  labs(title = "B  Sensitivity to reconciliation", x = expression(omega), y = "Discrepancy (binomial deviance)") +
  theme_paper
save_fig(age_plot + curve_plot, "figure_06_serology", 10, 4.5)
writeLines(capture.output(sessionInfo()), "session_R.txt")
