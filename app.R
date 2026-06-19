# ================================================================
# AR INBRE Research Output Tracker  |  P20GM103429
# ================================================================
# Tabs: Dashboard | Publications | Presentations | Grant Pipeline
#       Core & Outreach | Network & Compliance | RPPR Export
# ================================================================

library(shiny); library(bslib); library(tidyverse); library(DT)
library(plotly); library(httr); library(jsonlite)
library(lubridate); library(readr); library(writexl)

# ── CONSTANTS ─────────────────────────────────────────────────
GRANT_NUMBER  <- "P20GM103429"
EUTILS_BASE   <- "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/"
BUDGET_INSTS  <- c("Arkansas State","UA Little Rock","U of Central Arkansas",
                    "Arkansas Tech","Ouachita Baptist","Harding University",
                    "UA Fort Smith","UA Pine Bluff","Lyon College",
                    "Hendrix College","U of the Ozarks","Southern Arkansas",
                    "UA Monticello","Philander Smith","John Brown University",
                    "Cossatot Community","UA Fayetteville")

FUNDING_CYCLES <- c("2025-2026 (May 2025 – Apr 2026)",
                     "2024-2025 (May 2024 – Apr 2025)",
                     "2023-2024 (May 2023 – Apr 2024)",
                     "2022-2023 (May 2022 – Apr 2023)",
                     "2021-2022 (May 2021 – Apr 2022)",
                     "Other / Unknown")

GRANT_TYPES <- c("Faculty Research Voucher ($7,500)",
                  "Student Research Voucher ($750)",
                  "Faculty Curriculum Voucher ($750)",
                  "Pilot Project / DRPP ($50K, 12 mo)",
                  "Research Project / DRPP ($125K/yr)",
                  "Summer Research Program",
                  "Summer Manuscript Support",
                  "Other INBRE Support")

PI_ROLES <- c("Research Project Leader (RPL)",
              "Pilot Project Leader (PPL)",
              "Supplement Project Leader (SPL)",
              "Core Staff","Other Network Participant")

DELIVERY_METHODS <- c("In-person","Virtual","Hybrid")
OUTREACH_TYPES   <- c("Workshop","Webinar","Symposium","Seminar",
                       "Conference","Retreat","Training Program","Other")
CORE_NAMES <- c("Proteomics Core","Genomics Core","Bioinformatics Core",
                 "Flow Cytometry","Digital & Electron Microscopy",
                 "DNA Damage & Toxicology","Statewide Mass Spectrometry",
                 "NMR Spectroscopy","X-ray Crystallography",
                 "Experimental Pathology","Skeletal Phenotyping",
                 "Brain Imaging Research","Tissue Procurement","Other Core")

# ── HELPERS ───────────────────────────────────────────────────
`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b
  else if (length(a) == 1 && is.na(a)) b else a
}

normalize_whitespace <- function(x) {
  x %>% as.character() %>% stringr::str_replace_all("\\s+", " ") %>% stringr::str_trim()
}

normalize_name <- function(x) {
  normalize_whitespace(x) %>% stringr::str_replace_all(",", "") %>% stringr::str_to_lower()
}

normalize_title <- function(x) {
  normalize_whitespace(x) %>% stringr::str_to_lower() %>%
    stringr::str_replace_all("[^a-z0-9 ]", "") %>%
    stringr::str_replace_all("\\s+", " ") %>% stringr::str_trim()
}

safe_blank_to_na <- function(x) {
  x <- as.character(x)
  x <- stringr::str_replace_all(x, "\\u00A0", " ")
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "N/A", "NULL")] <- NA_character_
  x
}

extract_year_safe <- function(date_str) {
  x <- safe_blank_to_na(date_str)
  vapply(x, function(one) {
    if (is.na(one)) return("Missing Date")
    if (stringr::str_detect(one, "^\\d{4}-\\d{1,2}-\\d{1,2}")) return(substr(one, 1, 4))
    if (stringr::str_detect(one, "^\\d{1,2}/\\d{1,2}/\\d{2,4}")) {
      parts <- unlist(strsplit(one, "/"))
      yr_part <- unlist(strsplit(parts[3], " "))[1]
      yr_num <- suppressWarnings(as.numeric(yr_part))
      if (is.na(yr_num)) return("Missing Date")
      if (yr_num < 100) yr_num <- ifelse(yr_num >= 80, 1900 + yr_num, 2000 + yr_num)
      return(as.character(yr_num))
    }
    "Missing Date"
  }, character(1))
}

assign_budget_year_safe <- function(date_str) {
  x <- safe_blank_to_na(date_str)
  vapply(x, function(one) {
    if (is.na(one)) return("Unknown")
    d <- suppressWarnings(lubridate::ymd(one, quiet = TRUE))
    if (is.na(d)) d <- suppressWarnings(lubridate::mdy(one, quiet = TRUE))
    if (is.na(d)) return("Unknown")
    yr <- lubridate::year(d)
    mo <- lubridate::month(d)
    if (mo >= 5) paste0(yr, "-", yr + 1) else paste0(yr - 1, "-", yr)
  }, character(1))
}

read_first_existing_csv <- function(paths, ...) {
  hit <- paths[file.exists(paths)][1]
  if (is.na(hit) || is.null(hit)) return(tibble())
  readr::read_csv(hit, show_col_types = FALSE, ...)
}

triangulate_apps_reports <- function(apps, reps) {
  if (nrow(apps) == 0 || nrow(reps) == 0) return(tibble())

  apps_clean <- apps %>%
    mutate(
      application_record_id = as.character(record_id),
      app_name = normalize_whitespace(name),
      app_title = normalize_whitespace(project_title),
      app_name_clean = normalize_name(name),
      app_title_clean = normalize_title(project_title),
      app_year = extract_year_safe(due_date),
      app_budget_year = assign_budget_year_safe(due_date),
      institution_clean = normalize_whitespace(home_institution),
      voucher_label = dplyr::case_when(
        as.character(voucher_type) == "1" ~ "Student Research Voucher ($750)",
        as.character(voucher_type) == "2" ~ "Faculty Research Voucher ($7,500)",
        as.character(voucher_type) == "3" ~ "Faculty Curriculum Voucher ($750)",
        TRUE ~ "Other"
      )
    )

  reps_clean <- reps %>%
    mutate(
      report_record_id = as.character(record_id),
      rep_name = normalize_whitespace(name),
      rep_title = normalize_whitespace(project_title),
      rep_name_clean = normalize_name(name),
      rep_title_clean = normalize_title(project_title),
      rep_year = extract_year_safe(award_date),
      rep_budget_year = assign_budget_year_safe(award_date),
      has_presentation = dplyr::if_else(tolower(coalesce(as.character(choose_presentations), "")) %in% c("1","yes","checked","true"), 1L, 0L),
      has_manuscript   = dplyr::if_else(tolower(coalesce(as.character(choose_manuscripts), "")) %in% c("1","yes","checked","true"), 1L, 0L),
      has_grant        = dplyr::if_else(tolower(coalesce(as.character(choose_grant), "")) %in% c("1","yes","checked","true"), 1L, 0L),
      presentations_citing_inbre = suppressWarnings(as.numeric(presentations_citing_inbre)),
      manuscripts_citing_inbre = suppressWarnings(as.numeric(manuscripts_citing_inbre)),
      grants_citing_inbre = suppressWarnings(as.numeric(grants_citing_inbre))
    )

  tidyr::crossing(
    apps_clean %>% select(application_record_id, app_name, app_title, app_name_clean, app_title_clean, app_year, app_budget_year, institution_clean, voucher_label, amount_requested, date_samples),
    reps_clean %>% select(report_record_id, rep_name, rep_title, rep_name_clean, rep_title_clean, rep_year, rep_budget_year, has_presentation, has_manuscript, has_grant, presentations_citing_inbre, manuscripts_citing_inbre, grants_citing_inbre, data_outcomes, impact, presentation_info, manuscript, grants)
  ) %>%
    mutate(
      score_name = if_else(app_name_clean == rep_name_clean & app_name_clean != "", 4L, 0L),
      score_title = if_else(app_title_clean == rep_title_clean & app_title_clean != "", 5L, 0L),
      score_year = if_else(app_budget_year == rep_budget_year & app_budget_year != "Unknown", 2L, 0L),
      total_score = score_name + score_title + score_year,
      confidence_tier = case_when(
        total_score >= 9 ~ "High Confidence",
        total_score >= 6 ~ "Review Required",
        TRUE ~ "Pending / Unmatched"
      )
    ) %>%
    group_by(report_record_id) %>%
    slice_max(order_by = total_score, n = 1, with_ties = FALSE) %>%
    ungroup()
}

# Assign INBRE budget year (May 1 – Apr 30) from a date
assign_budget_year <- function(date_str) {
  d <- suppressWarnings(as.Date(date_str))
  if (is.na(d)) return("Unknown")
  yr <- as.integer(format(d, "%Y"))
  mo <- as.integer(format(d, "%m"))
  if (mo >= 5) paste0(yr, "-", yr + 1) else paste0(yr - 1, "-", yr)
}

# Compute next key deadlines from today
next_deadline <- function(month, day) {
  today <- Sys.Date()
  yr    <- as.integer(format(today, "%Y"))
  d     <- as.Date(paste(yr, month, day, sep = "-"))
  if (d <= today) d <- as.Date(paste(yr + 1, month, day, sep = "-"))
  d
}
days_until <- function(d) as.integer(d - Sys.Date())

# Urgency colour from days remaining
urgency_class <- function(days) {
  if (is.na(days) || days < 0) return("text-secondary")
  if (days <= 30)  return("text-danger")
  if (days <= 90)  return("text-warning")
  return("text-success")
}

# PMC compliance
pmc_compliance <- function(pmcid, pub_year) {
  has <- !is.na(pmcid) && nchar(trimws(as.character(pmcid))) > 0
  if (has) return("compliant")
  yr <- suppressWarnings(as.integer(substr(as.character(pub_year), 1, 4)))
  if (!is.na(yr) && (as.integer(format(Sys.Date(), "%Y")) - yr) >= 1)
    return("non_compliant")
  "pending"
}
compliance_badge <- function(s) switch(s,
  "compliant"     = "✅ Compliant",
  "pending"       = "🔵 In Process",
  "non_compliant" = "🔴 Action Required",
  "⚫ Flagged")

# PubMed pull
rppr_citation <- function(r) {
  paste(Filter(nchar, c(
    trimws(r$authors %||% ""),
    trimws(r$title   %||% ""),
    paste0(trimws(r$journal %||% ""), ". ", r$pub_year %||% "",
           if (nchar(r$volume %||% "") > 0)
             paste0(";", r$volume, if (nchar(r$issue %||% "") > 0)
               paste0("(", r$issue, ")"), ":", r$pages %||% "") else ""),
    if (nchar(r$pmid  %||% "") > 0) paste0("PMID: ",  r$pmid),
    if (nchar(r$pmcid %||% "") > 0) paste0("PMCID: ", r$pmcid)
  )), collapse = ". ")
}

fetch_pubmed <- function(grant_number = GRANT_NUMBER, extra = "") {
  primary  <- paste0('"', grant_number, '"[gr]')
  text_str <- '"AR INBRE"[tiab] OR "ARINBRE"[tiab] OR "Arkansas INBRE"[tiab] AND arkansas[affil]'
  term <- if (nchar(trimws(extra)) > 0)
    paste0(primary, " OR (", extra, " AND arkansas[affil])")
  else paste0(primary, " OR (", text_str, ")")

  r1 <- tryCatch(httr::GET(paste0(EUTILS_BASE, "esearch.fcgi"),
    query = list(db = "pubmed", term = term, retmax = 500, retmode = "json")),
    error = function(e) NULL)
  if (is.null(r1) || httr::status_code(r1) != 200) return(NULL)

  d1    <- jsonlite::fromJSON(httr::content(r1,"text",encoding="UTF-8"), simplifyVector=TRUE)
  pmids <- d1$esearchresult$idlist
  if (length(pmids) == 0) return(tibble())
  Sys.sleep(0.4)

  batches <- split(pmids, ceiling(seq_along(pmids)/200))
  purrr::map_dfr(batches, function(batch) {
    Sys.sleep(0.35)
    r2 <- tryCatch(httr::GET(paste0(EUTILS_BASE,"esummary.fcgi"),
      query = list(db="pubmed",id=paste(batch,collapse=","),retmode="json",version="2.0")),
      error = function(e) NULL)
    if (is.null(r2) || httr::status_code(r2) != 200) return(tibble())
    d2  <- jsonlite::fromJSON(httr::content(r2,"text",encoding="UTF-8"), simplifyVector=FALSE)
    res <- d2$result
    purrr::map_dfr(batch, function(pmid) {
      doc <- res[[pmid]]; if (is.null(doc)) return(NULL)
      pmcid <- ""
      if (!is.null(doc$articleids))
        for (a in doc$articleids)
          if (!is.null(a$idtype) && a$idtype == "pmc") { pmcid <- a$value %||% ""; break }
      nms <- if (!is.null(doc$authors) && length(doc$authors) > 0)
        sapply(doc$authors, function(a) a$name %||% "") else character(0)
      nms <- nms[nchar(nms) > 0]
      auth <- if (length(nms) > 3) paste(c(nms[1:3],"et al."), collapse=", ")
              else paste(nms, collapse=", ")
      py   <- substr(doc$pubdate %||% "", 1, 4)
      s    <- pmc_compliance(pmcid, py)
      tibble(pmid=pmid, title=doc$title %||% "", authors=auth,
             journal=doc$source %||% "", pub_year=py,
             pub_date=doc$pubdate %||% "", volume=doc$volume %||% "",
             issue=doc$issue %||% "", pages=doc$pages %||% "",
             pmcid=pmcid, doi="", source="PubMed Auto",
             grant_type="Auto-detected", cycle="",
             compliance=s, compliance_badge=compliance_badge(s),
             flagged=FALSE, notes="")
    })
  })
}

# Empty schemas
empty_pubs   <- function() tibble(pmid=character(),title=character(),
  authors=character(),journal=character(),pub_year=character(),
  pub_date=character(),volume=character(),issue=character(),pages=character(),
  pmcid=character(),doi=character(),source=character(),grant_type=character(),
  cycle=character(),compliance=character(),compliance_badge=character(),
  flagged=logical(),notes=character())

empty_pres   <- function() tibble(pi_name=character(),title=character(),
  event=character(),event_date=character(),location=character(),
  type=character(),cited_inbre=logical(),grant_type=character(),
  cycle=character(),notes=character())

empty_drpp   <- function() tibble(pi_name=character(),institution=character(),
  project_title=character(),pi_role=character(),grant_type=character(),
  applied_date=character(),status=character(),award_source=character(),
  award_amount=numeric(),start_date=character(),end_date=character(),
  cycle=character(),dms_plan=logical(),notes=character())

empty_roster <- function() tibble(pi_name=character(),email=character(),
  institution=character(),pi_role=character(),era_commons=character(),
  orcid=character(),mentor=character(),active=logical(),notes=character())

empty_core   <- function() tibble(budget_year=character(),core_name=character(),
  user_type=character(),pi_name=character(),project=character(),
  tech_service=character(),n_users=integer(),n_student_users=integer(),
  contributed_grant=logical(),contributed_course=logical(),
  contributed_pub=logical(),notes=character())

empty_outreach <- function() tibble(budget_year=character(),activity_type=character(),
  provider=character(),delivery=character(),date=character(),
  n_faculty=integer(),n_students=integer(),notes=character())

empty_eac    <- function() tibble(meeting_date=character(),attendees=character(),
  recommendations=character(),actions_taken=character(),notes=character())

empty_dms    <- function() tibble(pi_name=character(),project_title=character(),
  grant_type=character(),cycle=character(),dms_plan_exists=logical(),
  dms_activated=logical(),repository=character(),notes=character())

empty_rrid   <- function() tibble(core_name=character(),rrid=character(),
  description=character(),website=character(),notes=character())

# ── UI ────────────────────────────────────────────────────────
ui <- page_navbar(
  title = "AR INBRE Research Tracker",
  theme = bs_theme(version=5, bootswatch="flatly"),
  fillable = FALSE, bg = "#2c6fad",

  # ── TAB 1: DASHBOARD ────────────────────────────────────────
  nav_panel("📊 Dashboard",
    # Deadline countdown row
    card(
      card_header("⏰ Key Deadlines"),
      uiOutput("deadline_row")
    ),
    br(),
    layout_columns(fill=FALSE,
      value_box("Total Publications", textOutput("vb_pubs"),
                showcase=icon("book"), theme="primary"),
      value_box("✅ PMC Compliant",   textOutput("vb_green"),
                showcase=icon("circle-check"), theme="success"),
      value_box("🔴 Action Required", textOutput("vb_red"),
                showcase=icon("circle-exclamation"), theme="danger"),
      value_box("Vouchers (All Years)",textOutput("vb_hist"),
                showcase=icon("award"), theme="secondary")
    ),
    layout_columns(
      card(card_header("Voucher Spending by Year (Historical)"),
           plotlyOutput("hist_trend", height="320px")),
      card(card_header("Voucher Type Mix by Year"),
           plotlyOutput("hist_mix",   height="320px"))
    ),
    layout_columns(
      card(card_header("Vouchers by Institution (All Years)"),
           plotlyOutput("hist_inst",  height="380px")),
      card(card_header("Research Outputs by Budget Year"),
           plotlyOutput("dash_pubs",  height="380px"))
    )
  ),

  # ── TAB 2: PUBLICATIONS ─────────────────────────────────────
  nav_panel("📄 Publications",
    layout_sidebar(
      sidebar = sidebar(width=260,
        h6("PubMed Auto-Pull"),
        tags$p(paste0("Grant: ", GRANT_NUMBER), class="text-muted small"),
        textInput("extra_pi","Additional PI names (optional)",
                  placeholder='"Smith J"[au] OR "Jones A"[au]'),
        actionButton("btn_pull","🔄 Pull from PubMed",class="btn-primary w-100 mb-1"),
        uiOutput("pull_status"),
        hr(),
        h6("Manual Entry"),
        actionButton("btn_add_pub","➕ Add Publication",class="btn-outline-success w-100 mb-2"),
        fileInput("ul_pubs","Or upload CSV",accept=".csv"),
        hr(),
        h6("Filter"),
        selectInput("f_status","Compliance",
                    c("All","✅ Compliant","🔵 In Process","🔴 Action Required")),
        selectInput("f_year","Year","All"),
        hr(),
        downloadButton("dl_pubs_csv","Download CSV",class="btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton("dl_rppr_b1_txt","RPPR B.1 (.txt)",class="btn-sm btn-outline-primary w-100"),
        hr(),
        h6("Publication Alert Emails"),
        actionButton("btn_gen_alerts","📧 Draft Monthly Alerts",class="btn-sm btn-outline-warning w-100"),
        downloadButton("dl_alert_emails","Download Drafts (.txt)",class="btn-sm btn-outline-secondary w-100 mt-1")
      ),
      card(card_header("Publications — P20GM103429"),
           DTOutput("pubs_table", height="600px"))
    )
  ),

  # ── TAB 3: PRESENTATIONS ────────────────────────────────────
  nav_panel("🎤 Presentations",
    layout_sidebar(
      sidebar = sidebar(width=240,
        actionButton("btn_add_pres","➕ Add Entry",class="btn-primary w-100 mb-2"),
        hr(),
        fileInput("ul_pres","Upload CSV",accept=".csv"),
        downloadButton("dl_pres_tmpl","Download Template",class="btn-sm btn-outline-secondary w-100 mb-1"),
        downloadButton("dl_pres","Download Log",class="btn-sm btn-outline-secondary w-100")
      ),
      card(card_header("Poster & Presentation Log"),
           DTOutput("pres_table",height="600px"))
    )
  ),

  # ── TAB 4: GRANT PIPELINE ───────────────────────────────────
  nav_panel("🏆 Grant Pipeline",
    navset_tab(
      nav_panel("Historical Vouchers",
        card(card_header("All 11 Years — 309 Vouchers (2015-2026)"),
             DTOutput("hist_table",height="560px")),
        layout_columns(fill=FALSE,
          downloadButton("dl_hist","Download All Historical (.csv)",
                         class="btn-sm btn-outline-secondary"),
          downloadButton("dl_t1a","Export Table 1A (.csv)",
                         class="btn-sm btn-outline-primary"),
          downloadButton("dl_t1b","Export Table 1B (.csv)",
                         class="btn-sm btn-outline-primary")
        )
      ),
      nav_panel("Current Cycle + DRPP",
        layout_sidebar(
          sidebar = sidebar(width=240,
            tags$p("Vouchers auto-load from PID 1239.",class="text-muted small"),
            actionButton("btn_add_drpp","➕ Add DRPP Grant",class="btn-primary w-100 mb-2"),
            fileInput("ul_drpp","Upload CSV",accept=".csv"),
            hr(),
            selectInput("f_pipe_status","Status",c("All","Applied","Under Review","Awarded","Declined","Completed")),
            selectInput("f_pipe_type","Type",c("All","Voucher","Pilot Project","Research Project","Summer","Other")),
            hr(),
            downloadButton("dl_pipeline","Download CSV",class="btn-sm btn-outline-secondary w-100")
          ),
          card(card_header("Current Cycle Pipeline"),
               DTOutput("pipeline_table",height="560px"))
        )
      ),
      nav_panel("Table 1A / 1B Preview",
        layout_columns(
          card(card_header("Table 1A — INBRE Funding Outcomes by Source"),
               DTOutput("t1a_preview")),
          card(card_header("Table 1B — Outcomes by PI Role"),
               DTOutput("t1b_preview"))
        )
      )
    )
  ),

  # ── TAB 5: CORE & OUTREACH ──────────────────────────────────
  nav_panel("🔬 Core & Outreach",
    navset_tab(
      nav_panel("Core Use (Table 4)",
        layout_sidebar(
          sidebar = sidebar(width=240,
            selectInput("t4_core","Core Facility",c("All",CORE_NAMES)),
            selectInput("t4_year","Budget Year",c("All")),
            hr(),
            actionButton("btn_add_core","➕ Add Core Use Entry",class="btn-primary w-100 mb-2"),
            fileInput("ul_core","Upload CSV",accept=".csv"),
            hr(),
            downloadButton("dl_core","Download Table 4 (.csv)",class="btn-sm btn-outline-secondary w-100"),
            downloadButton("dl_core_xlsx","Export Table 4 (.xlsx)",class="btn-sm btn-outline-primary w-100 mt-1")
          ),
          card(
            card_header("Core Demand and Matched Reporting"),
            plotlyOutput("core_impact_chart", height="320px"),
            br(),
            DTOutput("core_table",height="560px")
          )
        )
      ),
      nav_panel("Education & Outreach (Table 3)",
        layout_sidebar(
          sidebar = sidebar(width=240,
            actionButton("btn_add_out","➕ Add Activity",class="btn-primary w-100 mb-2"),
            fileInput("ul_out","Upload CSV",accept=".csv"),
            hr(),
            downloadButton("dl_out","Download Table 3 (.csv)",class="btn-sm btn-outline-secondary w-100"),
            downloadButton("dl_out_xlsx","Export Table 3 (.xlsx)",class="btn-sm btn-outline-primary w-100 mt-1")
          ),
          card(card_header("Education & Outreach Activity Log — NIGMS Table 3"),
               DTOutput("out_table",height="560px"))
        )
      )
    )
  ),

  # ── TAB 6: NETWORK & COMPLIANCE ─────────────────────────────
  nav_panel("👥 Network & Compliance",
    navset_tab(
      nav_panel("Personnel Roster",
        layout_sidebar(
          sidebar = sidebar(width=240,
            actionButton("btn_add_pi","➕ Add Person",class="btn-primary w-100 mb-2"),
            actionButton("btn_seed_pi","🌱 Seed from Historical",class="btn-outline-secondary w-100 mb-2"),
            hr(),
            downloadButton("dl_roster","Download Roster (.csv)",class="btn-sm btn-outline-secondary w-100")
          ),
          card(card_header("PI & Network Personnel Roster"),
               DTOutput("roster_table",height="560px"))
        )
      ),
      nav_panel("EAC Meeting Log",
        layout_sidebar(
          sidebar = sidebar(width=240,
            actionButton("btn_add_eac","➕ Add Meeting",class="btn-primary w-100 mb-2"),
            hr(),
            downloadButton("dl_eac","Download Log (.csv)",class="btn-sm btn-outline-secondary w-100")
          ),
          card(card_header("External Advisory Committee Meeting Log"),
               DTOutput("eac_table",height="560px"))
        )
      ),
      nav_panel("DMS Plan Tracker",
        layout_sidebar(
          sidebar = sidebar(width=240,
            tags$p("Required in RPPRs since Oct 2024 (NOT-OD-24-175).",class="text-muted small"),
            actionButton("btn_add_dms","➕ Add DMS Record",class="btn-primary w-100 mb-2"),
            hr(),
            downloadButton("dl_dms","Download (.csv)",class="btn-sm btn-outline-secondary w-100")
          ),
          card(card_header("Data Management & Sharing Plan Tracker"),
               DTOutput("dms_table",height="560px"))
        )
      ),
      nav_panel("RRID Registry",
        layout_sidebar(
          sidebar = sidebar(width=240,
            tags$p("Assign RRIDs to cores to track usage in publications.",class="text-muted small"),
            actionButton("btn_add_rrid","➕ Add Core RRID",class="btn-primary w-100 mb-2"),
            hr(),
            downloadButton("dl_rrid","Download Registry (.csv)",class="btn-sm btn-outline-secondary w-100")
          ),
          card(card_header("Core Facility RRID Registry"),
               DTOutput("rrid_table",height="300px")),
          br(),
          card(
            card_header("Acknowledgment Text Checker"),
            tags$p("Paste a paper's acknowledgment section to verify INBRE citation.", class="text-muted small"),
            textAreaInput("ack_text","Acknowledgment text",rows=4,
                          placeholder="Paste the full acknowledgment section here..."),
            actionButton("btn_check_ack","🔍 Check Citation",class="btn-primary"),
            br(),br(),
            uiOutput("ack_result")
          )
        )
      )
    )
  ),

  # ── TAB 7: RPPR EXPORT ──────────────────────────────────────
  nav_panel("📋 RPPR Export",
    navset_tab(
      nav_panel("Section B.1 — Publications",
        card(card_header("Publication List (RPPR-formatted)"),
             uiOutput("b1_summary"),
             verbatimTextOutput("b1_preview"),
             downloadButton("dl_b1","Download B.1 (.txt)",class="btn-primary mt-2"))
      ),
      nav_panel("Section B.6 — Summary",
        card(card_header("Outcome Summary"),
             verbatimTextOutput("b6_preview"),
             downloadButton("dl_b6","Download B.6 (.txt)",class="btn-outline-primary mt-2"))
      ),
      nav_panel("NIGMS Tables Export",
        layout_columns(
          card(card_header("Table 1A: Funding Outcomes"),
               DTOutput("exp_t1a")),
          card(card_header("Table 1B: Outcomes by Role"),
               DTOutput("exp_t1b"))
        ),
        layout_columns(
          card(card_header("Table 3: Education & Outreach"),
               DTOutput("exp_t3")),
          card(card_header("Table 4: Core Use"),
               DTOutput("exp_t4"))
        ),
        br(),
        layout_columns(fill=FALSE,
          downloadButton("dl_all_xlsx","⬇ Download All NIGMS Tables (.xlsx)",
                         class="btn-success"),
          downloadButton("dl_all_csv_zip","⬇ Download All Tables (.csv files)",
                         class="btn-outline-success")
        )
      ),
      nav_panel("Non-Compliance Outreach",
        card(card_header("🔴 Draft Emails for Non-Compliant PIs"),
             uiOutput("noncompliance_panel"),
             downloadButton("dl_emails","Download All Draft Emails (.txt)",
                            class="btn-outline-danger mt-2"))
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Data loading ──────────────────────────────────────────
  hist_vouchers <- reactive({
    df <- read_first_existing_csv(c("raw_data/historical_vouchers.csv", "historical_vouchers.csv"),
                                  col_types = cols(.default = "c"))
    if (nrow(df) == 0) return(tibble())
    df %>%
      mutate(amount = as.numeric(amount),
             uams_amount = as.numeric(uams_amount),
             uaf_amount  = as.numeric(uaf_amount),
             uams_paid   = as.numeric(uams_paid)) %>%
      replace_na(list(amount=0, uams_amount=0, uaf_amount=0, uams_paid=0))
  })

  apps_data <- reactive({
    tryCatch({
      df <- read_first_existing_csv(c("raw_data/applications.csv", "applications.csv"))
      if (nrow(df) == 0) return(tibble())
      df %>%
        filter(!is.na(name), normalize_whitespace(name) != "",
               toupper(trimws(coalesce(home_institution,""))) != "TEST")
    }, error = function(e) tibble())
  })

  core_outcomes_summary_data <- reactive({
    f <- "cleaned/core_outcomes_summary.csv"
    if (!file.exists(f)) return(tibble())
    tryCatch(
      readr::read_csv(f, show_col_types = FALSE),
      error = function(e) tibble()
    )
  })

  core_requests_with_outcomes_data <- reactive({
    f <- "cleaned/core_requests_with_outcomes.csv"
    if (!file.exists(f)) return(tibble())
    tryCatch(
      readr::read_csv(f, show_col_types = FALSE),
      error = function(e) tibble()
    )
  })

  reports_data <- reactive({
    tryCatch({
      df <- read_first_existing_csv(c("raw_data/reports.csv", "reports.csv"))
      if (nrow(df) == 0) return(tibble())
      df %>% filter(!is.na(name), normalize_whitespace(name) != "")
    }, error = function(e) tibble())
  })

  linked_reports <- reactive({
    triangulate_apps_reports(apps_data(), reports_data())
  })

  # ── Reactive stores ───────────────────────────────────────
  pubmed_rv     <- reactiveVal(empty_pubs())
  pub_status    <- reactiveVal("Not yet pulled.")
  manual_pubs   <- reactiveVal(empty_pubs())
  pres_rv       <- reactiveVal(empty_pres())
  drpp_rv       <- reactiveVal(empty_drpp())
  roster_rv     <- reactiveVal(empty_roster())
  core_rv       <- reactiveVal(empty_core())
  outreach_rv   <- reactiveVal(empty_outreach())
  eac_rv        <- reactiveVal(empty_eac())
  dms_rv        <- reactiveVal(empty_dms())
  rrid_rv       <- reactiveVal(empty_rrid())

  # ── Deadline countdown ────────────────────────────────────
  output$deadline_row <- renderUI({
    deadlines <- list(
      list(label="Next RPPR Due",   date=next_deadline(3,1),  icon="ti ti-file-check"),
      list(label="Next Voucher Window", date=min(Filter(function(d) d > Sys.Date(),
        c(next_deadline(1,1), next_deadline(5,1), next_deadline(9,1)))), icon="ti ti-award"),
      list(label="DRPP Apps Due",   date=next_deadline(11,3), icon="ti ti-flask"),
      list(label="Budget Year Ends",date=next_deadline(4,30), icon="ti ti-calendar")
    )
    cols <- lapply(deadlines, function(dl) {
      days <- days_until(dl$date)
      cls  <- urgency_class(days)
      div(class="text-center p-3",
        tags$i(class=paste(dl$icon,"mb-1"), style="font-size:24px;"),
        tags$p(dl$label, class="mb-1 small text-muted"),
        tags$p(format(dl$date, "%b %d, %Y"), class="mb-0 fw-bold"),
        tags$p(paste(days, "days"), class=paste("fw-bold", cls))
      )
    })
    do.call(layout_columns, c(list(fill=FALSE), cols))
  })

  # ── Historical charts ─────────────────────────────────────
  output$hist_trend <- renderPlotly({
    df <- hist_vouchers()
    if (nrow(df) == 0) return(plotly_empty())
    d <- df %>% group_by(budget_year) %>%
      summarise(total=sum(amount,na.rm=TRUE), n=n(), .groups="drop")
    p <- ggplot(d, aes(x=budget_year, y=total, group=1)) +
      geom_col(fill="#4472c4", width=0.65) +
      geom_text(aes(label=paste0("n=",n)), vjust=-0.4, size=3) +
      scale_y_continuous(labels=scales::dollar_format()) +
      labs(x="Budget Year", y="Total Awarded") +
      theme_minimal(base_size=11) +
      theme(axis.text.x=element_text(angle=35, hjust=1))
    ggplotly(p) %>% plotly::config(displayModeBar=FALSE)
  })

  output$hist_mix <- renderPlotly({
    df <- hist_vouchers()
    if (nrow(df) == 0) return(plotly_empty())
    d <- df %>% count(budget_year, mechanism)
    p <- ggplot(d, aes(x=budget_year, y=n, fill=mechanism)) +
      geom_bar(stat="identity", position="stack", width=0.65) +
      scale_fill_brewer(palette="Set2") +
      labs(x="Budget Year", y="Vouchers", fill="") +
      theme_minimal(base_size=11) +
      theme(axis.text.x=element_text(angle=35, hjust=1), legend.position="bottom")
    ggplotly(p) %>% plotly::config(displayModeBar=FALSE)
  })

  output$hist_inst <- renderPlotly({
    df <- hist_vouchers()
    if (nrow(df) == 0) return(plotly_empty())
    d <- df %>% filter(!is.na(institution)) %>% count(institution) %>%
      arrange(n)
    p <- ggplot(d, aes(x=reorder(institution,n), y=n, fill=n)) +
      geom_bar(stat="identity", width=0.7) +
      coord_flip() + scale_fill_viridis_c(option="plasma") +
      labs(x="",y="Total Vouchers") +
      theme_minimal(base_size=11) + theme(legend.position="none")
    ggplotly(p) %>% plotly::config(displayModeBar=FALSE)
  })

  output$dash_pubs <- renderPlotly({
    df <- all_pubs()
    if (nrow(df) == 0) return(plotly_empty())

    d <- df %>%
      filter(nchar(pub_year) == 4) %>%
      count(pub_year, compliance) %>%
      mutate(
        compliance = factor(
          compliance,
          levels = c("compliant", "pending", "non_compliant"),
          labels = c("✅ Compliant", "🔵 In Process", "🔴 Action Required")
        )
      )

    p <- ggplot(d, aes(x = pub_year, y = n, fill = compliance)) +
      geom_bar(stat = "identity", position = "stack") +
      scale_fill_manual(values = c(
        "✅ Compliant" = "#70ad47",
        "🔵 In Process" = "#4472c4",
        "🔴 Action Required" = "#c00000"
      )) +
      labs(x = "Year", y = "Count", fill = "") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")

    ggplotly(p) %>% plotly::config(displayModeBar = FALSE)
  })

  # ── Value boxes ───────────────────────────────────────────
  output$vb_pubs  <- renderText({
    df <- linked_reports()
    if (nrow(df) == 0) return("0")
    as.character(sum(df$has_presentation, na.rm = TRUE) + sum(df$has_manuscript, na.rm = TRUE) + sum(df$has_grant, na.rm = TRUE))
  })
  output$vb_green <- renderText({
    df <- linked_reports()
    if (nrow(df) == 0) return("0")
    as.character(sum(df$confidence_tier == "High Confidence", na.rm = TRUE))
  })
  output$vb_red   <- renderText({
    df <- linked_reports()
    if (nrow(df) == 0) return("0")
    as.character(sum(df$confidence_tier != "High Confidence", na.rm = TRUE))
  })
  output$vb_hist  <- renderText(nrow(hist_vouchers()))

  # ── Publications ──────────────────────────────────────────
  observeEvent(input$btn_pull, {
    pub_status("⏳ Querying PubMed…")
    withProgress(message="Contacting NCBI…", {
      result <- tryCatch(fetch_pubmed(GRANT_NUMBER, input$extra_pi),
                         error=function(e){ pub_status(paste0("❌ ",e$message)); NULL })
    })
    if (!is.null(result) && nrow(result)>0) {
      pubmed_rv(result); pub_status(paste0("✅ ",nrow(result)," records (",Sys.Date(),")"))
    } else if (!is.null(result)) pub_status("⚠️ No results found.")
  })
  output$pull_status <- renderUI(tags$p(pub_status(), class="text-muted small mt-1"))

  observeEvent(input$ul_pubs, {
    req(input$ul_pubs)
    df <- tryCatch(read.csv(input$ul_pubs$datapath, stringsAsFactors=FALSE), error=function(e)NULL)
    if (!is.null(df)) {
      df <- df %>% mutate(source=coalesce(source,"CSV Upload"),flagged=coalesce(as.logical(flagged),FALSE),
        compliance=mapply(pmc_compliance,pmcid,pub_year),compliance_badge=sapply(compliance,compliance_badge))
      manual_pubs(bind_rows(manual_pubs(),df))
    }
  })

  observeEvent(input$btn_add_pub, {
    showModal(modalDialog(title="Add Publication",size="l",easyClose=TRUE,
      textInput("mp_authors","Authors *",placeholder="Smith J, Jones A, et al."),
      textInput("mp_title","Title *"),
      fluidRow(column(8,textInput("mp_journal","Journal")),column(4,textInput("mp_year","Year *",placeholder="2025"))),
      fluidRow(column(4,textInput("mp_vol","Volume")),column(4,textInput("mp_iss","Issue")),column(4,textInput("mp_pg","Pages"))),
      fluidRow(column(6,textInput("mp_pmid","PMID")),column(6,textInput("mp_pmcid","PMCID"))),
      textInput("mp_doi","DOI"),
      fluidRow(column(6,selectInput("mp_gt","Grant Type",GRANT_TYPES)),
               column(6,selectInput("mp_cy","Cycle",FUNDING_CYCLES))),
      textAreaInput("mp_notes","Notes",rows=2),
      footer=tagList(modalButton("Cancel"),actionButton("btn_save_pub","Save",class="btn-success"))))
  })

  observeEvent(input$btn_save_pub, {
    req(nchar(trimws(input$mp_title))>0, nchar(trimws(input$mp_year))>0)
    s <- pmc_compliance(input$mp_pmcid, input$mp_year)
    manual_pubs(bind_rows(manual_pubs(), tibble(
      pmid=trimws(input$mp_pmid), title=trimws(input$mp_title),
      authors=trimws(input$mp_authors), journal=trimws(input$mp_journal),
      pub_year=trimws(input$mp_year), pub_date=trimws(input$mp_year),
      volume=trimws(input$mp_vol), issue=trimws(input$mp_iss),
      pages=trimws(input$mp_pg), pmcid=trimws(input$mp_pmcid),
      doi=trimws(input$mp_doi), source="Manual Entry",
      grant_type=input$mp_gt, cycle=input$mp_cy,
      compliance=s, compliance_badge=compliance_badge(s),
      flagged=FALSE, notes=trimws(input$mp_notes))))
    removeModal(); showNotification("Saved.",type="message")
  })

  all_pubs <- reactive({
    combined <- bind_rows(pubmed_rv(), manual_pubs())
    if (nrow(combined)==0) return(combined)
    combined %>%
      arrange(desc(source=="Manual Entry")) %>%
      distinct(title, .keep_all=TRUE) %>%
      mutate(compliance=mapply(pmc_compliance,pmcid,pub_year),
             compliance_badge=sapply(compliance,compliance_badge))
  })

  observe({
    yrs <- sort(unique(all_pubs()$pub_year),decreasing=TRUE)
    updateSelectInput(session,"f_year",choices=c("All",yrs[yrs!=""]))
  })

  output$pubs_table <- renderDT({
    df <- all_pubs()
    if (input$f_status!="All") df <- df %>% filter(compliance_badge==input$f_status)
    if (input$f_year!="All")   df <- df %>% filter(pub_year==input$f_year)
    if (nrow(df)==0) return(datatable(tibble(Status="No publications yet.")))
    display <- df %>% select(compliance_badge,pub_year,authors,title,journal,pmid,pmcid,source) %>%
      rename(Status=compliance_badge,Year=pub_year,Authors=authors,Title=title,
             Journal=journal,PMID=pmid,PMCID=pmcid,Source=source)
    datatable(display,options=list(pageLength=15,scrollX=TRUE,dom="Blfrtip",scrollY="480px"),
              filter="top",rownames=FALSE) %>%
      formatStyle("Status",target="row",backgroundColor=styleEqual(
        c("✅ Compliant","🔵 In Process","🔴 Action Required"),
        c("#e2f0d9","#dae8fc","#fce4d6")))
  })

  # Publication alert emails
  alert_text <- reactive({
    df <- all_pubs() %>% filter(compliance=="non_compliant",!flagged)
    if (nrow(df)==0) return("No non-compliant publications identified.")
    by_auth <- df %>% group_by(authors) %>%
      summarise(n=n(), titles=paste(title,collapse="\n   - "), .groups="drop")
    drafts <- sapply(seq_len(nrow(by_auth)), function(i) {
      r <- by_auth[i,]
      paste0("TO: ", r$authors,"\nSUBJECT: Monthly Compliance Check — AR INBRE Publications\n\n",
             "Dear ", r$authors,",\n\nThis is our monthly compliance review for AR INBRE (P20GM103429).\n",
             "We found ",r$n," publication(s) not yet in PubMed Central:\n\n   - ",r$titles,
             "\n\nPlease upload your author-accepted manuscript at:\n",
             "https://www.ncbi.nlm.nih.gov/pmc/about/submission-methods/\n\n",
             "No charge for this. Reply with your PMCID(s) once submitted.\n\n",
             "Thank you,\nAR INBRE Program | UAMS\n",strrep("-",60))
    })
    paste(drafts,collapse="\n\n")
  })

  output$dl_pubs_csv    <- downloadHandler(filename=function() paste0("publications_",Sys.Date(),".csv"),
    content=function(f) write_csv(all_pubs(),f))
  b1_text <- reactive({
    df <- all_pubs() %>% filter(!flagged) %>% arrange(desc(pub_year))
    if (nrow(df)==0) return("No publications recorded.")
    paste(sapply(seq_len(nrow(df)), function(i) paste0(i,". ",rppr_citation(df[i,]))), collapse="\n\n")
  })
  output$dl_rppr_b1_txt <- downloadHandler(filename=function() paste0("rppr_b1_",Sys.Date(),".txt"),
    content=function(f) writeLines(b1_text(),f))
  output$dl_alert_emails <- downloadHandler(filename=function() paste0("alert_emails_",Sys.Date(),".txt"),
    content=function(f) writeLines(alert_text(),f))

  # ── Presentations ─────────────────────────────────────────
  observeEvent(input$ul_pres, { req(input$ul_pres)
    df <- tryCatch(read.csv(input$ul_pres$datapath,stringsAsFactors=FALSE),error=function(e)NULL)
    if (!is.null(df)) { pres_rv(bind_rows(pres_rv(),df)); showNotification(paste("Loaded",nrow(df)),type="message") }
  })
  observeEvent(input$btn_add_pres, {
    showModal(modalDialog(title="Add Poster / Presentation",size="l",easyClose=TRUE,
      fluidRow(column(6,textInput("pr_name","PI / Student Name *")),
               column(6,selectInput("pr_type","Type",c("Poster","Oral Presentation","Invited Talk","Workshop")))),
      textInput("pr_title","Presentation Title *"),
      fluidRow(column(8,textInput("pr_event","Event / Conference *")),
               column(4,dateInput("pr_date","Date",value=Sys.Date()))),
      textInput("pr_loc","Location",placeholder="Little Rock, AR"),
      checkboxInput("pr_cited","Acknowledged AR INBRE / P20GM103429?",value=TRUE),
      fluidRow(column(6,selectInput("pr_gt","Funded By",GRANT_TYPES)),
               column(6,selectInput("pr_cy","Cycle",FUNDING_CYCLES))),
      textInput("pr_notes","Notes"),
      footer=tagList(modalButton("Cancel"),actionButton("btn_save_pres","Save",class="btn-success"))))
  })
  observeEvent(input$btn_save_pres, {
    req(nchar(trimws(input$pr_name))>0, nchar(trimws(input$pr_title))>0)
    pres_rv(bind_rows(pres_rv(),tibble(pi_name=trimws(input$pr_name),
      title=trimws(input$pr_title),event=trimws(input$pr_event),
      event_date=as.character(input$pr_date),location=trimws(input$pr_loc),
      type=input$pr_type,cited_inbre=input$pr_cited,
      grant_type=input$pr_gt,cycle=input$pr_cy,notes=trimws(input$pr_notes))))
    removeModal(); showNotification("Saved.",type="message")
  })
  output$pres_table <- renderDT({
    df <- pres_rv()
    if (nrow(df)==0) return(datatable(tibble(Status="No entries yet.")))
    datatable(df,options=list(pageLength=15,scrollX=TRUE,scrollY="480px"),filter="top",rownames=FALSE)
  })
  output$dl_pres      <- downloadHandler(filename=function() paste0("presentations_",Sys.Date(),".csv"),content=function(f) write_csv(pres_rv(),f))
  output$dl_pres_tmpl <- downloadHandler(filename="presentations_template.csv",content=function(f)
    write_csv(tibble(pi_name="Jane Smith",title="Research Poster",event="AR INBRE Annual Conference",
      event_date="2026-09-15",location="Little Rock, AR",type="Poster",
      cited_inbre=TRUE,grant_type="Faculty Research Voucher ($7,500)",
      cycle="2025-2026 (May 2025 – Apr 2026)",notes=""),f))

  # ── Grant Pipeline ────────────────────────────────────────
  output$hist_table <- renderDT({
    df <- hist_vouchers()
    if (nrow(df)==0) return(datatable(tibble(Status="Loading...")))
    datatable(df, options=list(pageLength=25,scrollX=TRUE,dom="Blfrtip",scrollY="520px"),
              filter="top",rownames=FALSE) %>%
      formatCurrency("amount",currency="$",digits=0)
  })

  # Table 1A from historical data
  t1a_data <- reactive({
    df <- hist_vouchers()
    if (nrow(df)==0) return(tibble())
    df %>% group_by(budget_year) %>%
      summarise(
        apps_submitted = n(),
        awards_funded  = sum(amount>0, na.rm=TRUE),
        total_nih_fed  = sum(amount[uams_amount>0 | uaf_amount>0], na.rm=TRUE),
        total_uams     = sum(uams_amount, na.rm=TRUE),
        total_uaf      = sum(uaf_amount, na.rm=TRUE),
        total_costs    = sum(amount, na.rm=TRUE),
        .groups="drop"
      ) %>%
      rename(`Budget Year`=budget_year, `Apps Submitted`=apps_submitted,
             `Awards Funded`=awards_funded,
             `NIH Fed (UAMS)`=total_uams, `NIH Fed (UAF)`=total_uaf,
             `Total Costs`=total_costs)
  })

  # Table 1B by mechanism
  t1b_data <- reactive({
    df <- hist_vouchers()
    if (nrow(df)==0) return(tibble())
    df %>% group_by(budget_year, mechanism) %>%
      summarise(n_apps=n(), n_funded=sum(amount>0,na.rm=TRUE),
                total=sum(amount,na.rm=TRUE), .groups="drop") %>%
      rename(`Budget Year`=budget_year, `Role/Type`=mechanism,
             `# Apps`=n_apps, `# Funded`=n_funded, `Total ($)`=total)
  })

  output$t1a_preview <- renderDT({ datatable(t1a_data(),options=list(scrollX=TRUE),rownames=FALSE) })
  output$t1b_preview <- renderDT({ datatable(t1b_data(),options=list(scrollX=TRUE),rownames=FALSE) })
  output$exp_t1a <- renderDT({ datatable(t1a_data(),options=list(scrollX=TRUE),rownames=FALSE) })
  output$exp_t1b <- renderDT({ datatable(t1b_data(),options=list(scrollX=TRUE),rownames=FALSE) })

  output$dl_hist   <- downloadHandler(filename=function() paste0("historical_vouchers_",Sys.Date(),".csv"),content=function(f) write_csv(hist_vouchers(),f))
  output$dl_t1a    <- downloadHandler(filename=function() paste0("table_1a_",Sys.Date(),".csv"),content=function(f) write_csv(t1a_data(),f))
  output$dl_t1b    <- downloadHandler(filename=function() paste0("table_1b_",Sys.Date(),".csv"),content=function(f) write_csv(t1b_data(),f))

  # DRPP pipeline (current cycle)
  observeEvent(input$ul_drpp, { req(input$ul_drpp)
    df <- tryCatch(read.csv(input$ul_drpp$datapath,stringsAsFactors=FALSE),error=function(e)NULL)
    if (!is.null(df)) drpp_rv(bind_rows(drpp_rv(),df))
  })
  observeEvent(input$btn_add_drpp, {
    showModal(modalDialog(title="Add DRPP / Other Grant",size="l",easyClose=TRUE,
      fluidRow(column(6,textInput("dg_name","PI Name *")),column(6,textInput("dg_inst","Institution"))),
      textInput("dg_title","Project Title *"),
      fluidRow(column(4,selectInput("dg_role","PI Role",PI_ROLES)),
               column(4,selectInput("dg_type","Grant Type",c("Pilot Project ($50K)","Research Project ($125K/yr)","Summer Research","Summer Manuscript","Other"))),
               column(4,selectInput("dg_status","Status",c("Applied","Under Review","Awarded","Declined","Completed")))),
      fluidRow(column(4,textInput("dg_source","Award Source",placeholder="NIH, NSF, etc.")),
               column(4,numericInput("dg_amt","Award Amount ($)",0,min=0)),
               column(4,selectInput("dg_cy","Cycle",FUNDING_CYCLES))),
      fluidRow(column(6,dateInput("dg_start","Start Date",value=Sys.Date())),
               column(6,dateInput("dg_end","End Date",value=Sys.Date()+365))),
      checkboxInput("dg_dms","DMS Plan exists?",value=FALSE),
      textAreaInput("dg_notes","Notes",rows=2),
      footer=tagList(modalButton("Cancel"),actionButton("btn_save_drpp","Save",class="btn-success"))))
  })
  observeEvent(input$btn_save_drpp, {
    req(nchar(trimws(input$dg_name))>0, nchar(trimws(input$dg_title))>0)
    drpp_rv(bind_rows(drpp_rv(),tibble(pi_name=trimws(input$dg_name),institution=trimws(input$dg_inst),
      project_title=trimws(input$dg_title),pi_role=input$dg_role,grant_type=input$dg_type,
      applied_date=as.character(Sys.Date()),status=input$dg_status,
      award_source=trimws(input$dg_source),award_amount=input$dg_amt,
      start_date=as.character(input$dg_start),end_date=as.character(input$dg_end),
      cycle=input$dg_cy,dms_plan=input$dg_dms,notes=trimws(input$dg_notes))))
    removeModal(); showNotification("Saved.",type="message")
  })

  pipeline_filtered <- reactive({
    apps <- apps_data()
    v_rows <- if (nrow(apps)>0) apps %>%
      transmute(pi_name=name,institution=home_institution,
        project_title=project_title,pi_role="Voucher Recipient",
        grant_type=case_when(as.character(voucher_type)=="1"~"Student Voucher ($750)",
          as.character(voucher_type)=="2"~"Faculty Research Voucher ($7,500)",
          as.character(voucher_type)=="3"~"Faculty Curriculum Voucher ($750)",
          TRUE~"Other"),
        applied_date=coalesce(as.character(date_samples), as.character(due_date)),status="Awarded",award_source="AR INBRE",
        award_amount=coalesce(as.numeric(amount_requested),0),
        start_date=coalesce(as.character(date_samples), as.character(due_date)),end_date=NA_character_,
        cycle=NA_character_,dms_plan=FALSE,notes=NA_character_) else tibble()
    combined <- bind_rows(v_rows, drpp_rv())
    if (nrow(combined)==0) return(combined)
    if (input$f_pipe_status!="All") combined <- combined %>% filter(status==input$f_pipe_status)
    if (input$f_pipe_type!="All") combined <- combined %>%
      filter(str_detect(tolower(grant_type),tolower(input$f_pipe_type)))
    combined
  })

  output$pipeline_table <- renderDT({
    df <- pipeline_filtered()
    if (nrow(df)==0) return(datatable(tibble(Status="No grants. Vouchers load from raw_data/applications.csv")))
    datatable(df,options=list(pageLength=15,scrollX=TRUE,dom="Blfrtip",scrollY="480px"),filter="top",rownames=FALSE) %>%
      formatCurrency("award_amount",currency="$",digits=0) %>%
      formatStyle("status",backgroundColor=styleEqual(
        c("Awarded","Completed","Applied","Under Review","Declined"),
        c("#e2f0d9","#c6efce","#fff2cc","#dae8fc","#fce4d6")))
  })
  output$dl_pipeline <- downloadHandler(filename=function() paste0("pipeline_",Sys.Date(),".csv"),content=function(f) write_csv(pipeline_filtered(),f))

  # ── Core Use (Table 4) ────────────────────────────────────
  observeEvent(input$ul_core, { req(input$ul_core)
    df <- tryCatch(read.csv(input$ul_core$datapath,stringsAsFactors=FALSE),error=function(e)NULL)
    if (!is.null(df)) core_rv(bind_rows(core_rv(),df))
  })
  observeEvent(input$btn_add_core, {
    showModal(modalDialog(title="Add Core Use Entry",size="l",easyClose=TRUE,
      fluidRow(column(6,selectInput("cu_core","Core Facility *",CORE_NAMES)),
               column(6,selectInput("cu_year","Budget Year",c("2025-2026","2024-2025","2023-2024","2022-2023","Other")))),
      fluidRow(column(6,selectInput("cu_type","User Type",c("RP Lab","PP Lab","SP Lab","Other Network User","External User"))),
               column(6,textInput("cu_pi","PI / Project Name *"))),
      textInput("cu_tech","Technology / Service / Instruments Used"),
      fluidRow(column(4,numericInput("cu_n_users","# Individual Users",1,min=0)),
               column(4,numericInput("cu_n_stud","# Student Users",0,min=0))),
      fluidRow(column(4,checkboxInput("cu_g","Contributed to Grant App (G)?",FALSE)),
               column(4,checkboxInput("cu_c","Contributed to Course (C)?",FALSE)),
               column(4,checkboxInput("cu_p","Contributed to Publication (P)?",FALSE))),
      textInput("cu_notes","Notes"),
      footer=tagList(modalButton("Cancel"),actionButton("btn_save_core","Save",class="btn-success"))))
  })
  observeEvent(input$btn_save_core, {
    req(nchar(trimws(input$cu_pi))>0)
    core_rv(bind_rows(core_rv(),tibble(budget_year=input$cu_year,core_name=input$cu_core,
      user_type=input$cu_type,pi_name=trimws(input$cu_pi),project=trimws(input$cu_pi),
      tech_service=trimws(input$cu_tech),n_users=as.integer(input$cu_n_users),
      n_student_users=as.integer(input$cu_n_stud),contributed_grant=input$cu_g,
      contributed_course=input$cu_c,contributed_pub=input$cu_p,notes=trimws(input$cu_notes))))
    removeModal(); showNotification("Saved.",type="message")
  })
  core_detail_filtered <- reactive({
    df <- core_requests_with_outcomes_data()
    if (nrow(df) == 0) return(df)

    if (!is.null(input$t4_core) && input$t4_core != "All") {
      df <- df %>% filter(core_name == input$t4_core)
    }
    if (!is.null(input$t4_year) && input$t4_year != "All") {
      df <- df %>% filter(budget_year == input$t4_year)
    }
    df
  })

  observe({
    df <- core_requests_with_outcomes_data()
    yrs <- if (nrow(df) == 0) character(0) else sort(unique(df$budget_year), decreasing = TRUE)
    yrs <- yrs[!is.na(yrs) & yrs != ""]
    updateSelectInput(session, "t4_year", choices = c("All", yrs))
  })

  output$core_impact_chart <- renderPlotly({
    df <- core_outcomes_summary_data()
    if (nrow(df) == 0) return(plotly_empty())

    plot_df <- df %>%
      mutate(core_name = forcats::fct_reorder(core_name, n_projects))

    p <- ggplot(plot_df, aes(x = core_name, y = n_projects, fill = n_with_reports)) +
      geom_col(width = 0.75) +
      geom_text(aes(label = n_projects), hjust = -0.2, size = 3.2) +
      coord_flip() +
      labs(
        x = "",
        y = "Projects requesting core",
        fill = "Matched reports"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        axis.text.y = element_text(size = 10, face = "bold"),
        legend.position = "right"
      )

    ggplotly(p) %>% plotly::config(displayModeBar = FALSE)
  })

  output$core_table <- renderDT({
    df <- core_detail_filtered()
    if (nrow(df) == 0) {
      return(datatable(tibble(Status = "No cleaned core outcome data found. Run scripts_clean_core_requests.R and scripts_join_core_outcomes.R first.")))
    }

    display_df <- df %>%
      select(
        budget_year,
        institution,
        awardee_name,
        voucher_type,
        project_title,
        core_name,
        match_status,
        has_report,
        has_presentation,
        has_manuscript,
        has_grant
      ) %>%
      rename(
        `Budget Year` = budget_year,
        `Institution` = institution,
        `Awardee` = awardee_name,
        `Voucher Type` = voucher_type,
        `Project Title` = project_title,
        `Core Name` = core_name,
        `Match Status` = match_status,
        `Has Report` = has_report,
        `Presentation` = has_presentation,
        `Manuscript` = has_manuscript,
        `Grant` = has_grant
      )

    datatable(
      display_df,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        scrollY = "480px",
        scrollCollapse = TRUE
      ),
      filter = "top",
      rownames = FALSE
    ) %>%
      formatStyle(
        "Match Status",
        target = "row",
        backgroundColor = styleEqual(
          c("high_confidence", "review", "pending_or_unmatched"),
          c("#e2f0d9", "#fff2cc", "#fce4d6")
        )
      )
  })

  output$dl_core <- downloadHandler(
    filename = function() paste0("table4_core_", Sys.Date(), ".csv"),
    content = function(f) write_csv(core_detail_filtered(), f)
  )

  output$dl_core_xlsx <- downloadHandler(
    filename = function() paste0("table4_core_", Sys.Date(), ".xlsx"),
    content = function(f) write_xlsx(list("Table 4 Core Use" = core_detail_filtered()), f)
  )

  # ── Education & Outreach (Table 3) ────────────────────────
  observeEvent(input$ul_out, { req(input$ul_out)
    df <- tryCatch(read.csv(input$ul_out$datapath,stringsAsFactors=FALSE),error=function(e)NULL)
    if (!is.null(df)) outreach_rv(bind_rows(outreach_rv(),df))
  })
  observeEvent(input$btn_add_out, {
    showModal(modalDialog(title="Add Education / Outreach Activity",size="l",easyClose=TRUE,
      fluidRow(column(6,selectInput("oa_type","Activity Type *",OUTREACH_TYPES)),
               column(6,selectInput("oa_year","Budget Year",c("2025-2026","2024-2025","2023-2024","Other")))),
      fluidRow(column(8,textInput("oa_provider","Organizing Core or Institution *")),
               column(4,dateInput("oa_date","Date",value=Sys.Date()))),
      selectInput("oa_delivery","Delivery Method",DELIVERY_METHODS),
      fluidRow(column(6,numericInput("oa_nfac","# Faculty/Staff Participants",0,min=0)),
               column(6,numericInput("oa_nstu","# Student Participants",0,min=0))),
      textAreaInput("oa_notes","Notes",rows=2),
      footer=tagList(modalButton("Cancel"),actionButton("btn_save_out","Save",class="btn-success"))))
  })
  observeEvent(input$btn_save_out, {
    req(nchar(trimws(input$oa_provider))>0)
    outreach_rv(bind_rows(outreach_rv(),tibble(budget_year=input$oa_year,
      activity_type=input$oa_type,provider=trimws(input$oa_provider),
      delivery=input$oa_delivery,date=as.character(input$oa_date),
      n_faculty=as.integer(input$oa_nfac),n_students=as.integer(input$oa_nstu),
      notes=trimws(input$oa_notes))))
    removeModal(); showNotification("Saved.",type="message")
  })
  output$out_table <- renderDT({
    df <- outreach_rv()
    if (nrow(df)==0) return(datatable(tibble(Status="No activities yet. Click ➕ to log.")))
    datatable(df,options=list(pageLength=15,scrollX=TRUE,scrollY="480px"),filter="top",rownames=FALSE)
  })
  output$dl_out      <- downloadHandler(filename=function() paste0("table3_outreach_",Sys.Date(),".csv"),content=function(f) write_csv(outreach_rv(),f))
  output$dl_out_xlsx <- downloadHandler(filename=function() paste0("table3_outreach_",Sys.Date(),".xlsx"),content=function(f) write_xlsx(list("Table 3 Outreach"=outreach_rv()),f))

  # ── Personnel Roster ──────────────────────────────────────
  observeEvent(input$btn_seed_pi, {
    df <- hist_vouchers()
    if (nrow(df)==0) { showNotification("No historical data loaded.",type="warning"); return() }
    seeds <- df %>% filter(!is.na(applicant),applicant!="") %>%
      group_by(applicant) %>%
      summarise(email=first(email[email!=""]), institution=first(institution),
                .groups="drop") %>%
      transmute(pi_name=applicant, email=coalesce(email,""),
                institution=coalesce(institution,""),pi_role="Voucher Recipient",
                era_commons="",orcid="",mentor="",active=TRUE,notes="")
    existing <- roster_rv()$pi_name
    new_seeds <- seeds %>% filter(!pi_name %in% existing)
    roster_rv(bind_rows(roster_rv(), new_seeds))
    showNotification(paste("Added",nrow(new_seeds),"PIs from historical data."),type="message")
  })
  observeEvent(input$btn_add_pi, {
    showModal(modalDialog(title="Add Personnel",size="l",easyClose=TRUE,
      fluidRow(column(6,textInput("ri_name","Name *")),column(6,textInput("ri_email","Email"))),
      fluidRow(column(6,textInput("ri_inst","Institution")),
               column(6,selectInput("ri_role","Role",PI_ROLES))),
      fluidRow(column(6,textInput("ri_era","eRA Commons Username")),column(6,textInput("ri_orcid","ORCID"))),
      textInput("ri_mentor","Mentor (if applicable)"),
      checkboxInput("ri_active","Currently Active?",value=TRUE),
      textInput("ri_notes","Notes"),
      footer=tagList(modalButton("Cancel"),actionButton("btn_save_pi","Save",class="btn-success"))))
  })
  observeEvent(input$btn_save_pi, {
    req(nchar(trimws(input$ri_name))>0)
    roster_rv(bind_rows(roster_rv(),tibble(pi_name=trimws(input$ri_name),
      email=trimws(input$ri_email),institution=trimws(input$ri_inst),
      pi_role=input$ri_role,era_commons=trimws(input$ri_era),
      orcid=trimws(input$ri_orcid),mentor=trimws(input$ri_mentor),
      active=input$ri_active,notes=trimws(input$ri_notes))))
    removeModal(); showNotification("Saved.",type="message")
  })
  output$roster_table <- renderDT({
    df <- roster_rv()
    if (nrow(df)==0) return(datatable(tibble(Status="Empty. Click 'Seed from Historical' to populate from Excel data.")))
    datatable(df,options=list(pageLength=25,scrollX=TRUE,scrollY="480px"),filter="top",rownames=FALSE)
  })
  output$dl_roster <- downloadHandler(filename=function() paste0("roster_",Sys.Date(),".csv"),content=function(f) write_csv(roster_rv(),f))

  # ── EAC Meeting Log ───────────────────────────────────────
  observeEvent(input$btn_add_eac, {
    showModal(modalDialog(title="Add EAC Meeting",size="l",easyClose=TRUE,
      fluidRow(column(6,dateInput("eac_date","Meeting Date",value=Sys.Date())),
               column(6,textInput("eac_att","Attendees (comma-separated)"))),
      textAreaInput("eac_rec","Recommendations Made",rows=3),
      textAreaInput("eac_act","Actions Taken",rows=3),
      textAreaInput("eac_notes","Additional Notes",rows=2),
      footer=tagList(modalButton("Cancel"),actionButton("btn_save_eac","Save",class="btn-success"))))
  })
  observeEvent(input$btn_save_eac, {
    eac_rv(bind_rows(eac_rv(),tibble(meeting_date=as.character(input$eac_date),
      attendees=trimws(input$eac_att),recommendations=trimws(input$eac_rec),
      actions_taken=trimws(input$eac_act),notes=trimws(input$eac_notes))))
    removeModal(); showNotification("Saved.",type="message")
  })
  output$eac_table <- renderDT({
    df <- eac_rv()
    if (nrow(df)==0) return(datatable(tibble(Status="No meetings logged yet.")))
    datatable(df,options=list(pageLength=10,scrollX=TRUE,scrollY="480px"),filter="top",rownames=FALSE)
  })
  output$dl_eac <- downloadHandler(filename=function() paste0("eac_log_",Sys.Date(),".csv"),content=function(f) write_csv(eac_rv(),f))

  # ── DMS Plan Tracker ──────────────────────────────────────
  observeEvent(input$btn_add_dms, {
    showModal(modalDialog(title="Add DMS Plan Record",size="l",easyClose=TRUE,
      fluidRow(column(6,textInput("dm_pi","PI Name *")),column(6,selectInput("dm_cy","Cycle",FUNDING_CYCLES))),
      textInput("dm_title","Project Title *"),
      selectInput("dm_gt","Grant Type",GRANT_TYPES),
      fluidRow(column(6,checkboxInput("dm_exists","DMS Plan exists?",FALSE)),
               column(6,checkboxInput("dm_active","DMS Plan activated/implemented?",FALSE))),
      textInput("dm_repo","Repository / Platform",placeholder="OSF, Zenodo, Figshare, etc."),
      textAreaInput("dm_notes","Notes",rows=2),
      footer=tagList(modalButton("Cancel"),actionButton("btn_save_dms","Save",class="btn-success"))))
  })
  observeEvent(input$btn_save_dms, {
    req(nchar(trimws(input$dm_pi))>0)
    dms_rv(bind_rows(dms_rv(),tibble(pi_name=trimws(input$dm_pi),
      project_title=trimws(input$dm_title),grant_type=input$dm_gt,
      cycle=input$dm_cy,dms_plan_exists=input$dm_exists,
      dms_activated=input$dm_active,repository=trimws(input$dm_repo),
      notes=trimws(input$dm_notes))))
    removeModal(); showNotification("Saved.",type="message")
  })
  output$dms_table <- renderDT({
    df <- dms_rv()
    if (nrow(df)==0) return(datatable(tibble(Status="No DMS records yet. Required for RPPRs since Oct 2024.")))
    datatable(df,options=list(pageLength=15,scrollX=TRUE,scrollY="480px"),filter="top",rownames=FALSE) %>%
      formatStyle("dms_plan_exists",backgroundColor=styleEqual(c(TRUE,FALSE),c("#e2f0d9","#fce4d6")))
  })
  output$dl_dms <- downloadHandler(filename=function() paste0("dms_plans_",Sys.Date(),".csv"),content=function(f) write_csv(dms_rv(),f))

  # ── RRID Registry ────────────────────────────────────────
  observeEvent(input$btn_add_rrid, {
    showModal(modalDialog(title="Add Core RRID",size="l",easyClose=TRUE,
      selectInput("rr_core","Core Facility *",CORE_NAMES),
      textInput("rr_rrid","RRID *",placeholder="RRID:SCR_000000"),
      textInput("rr_desc","Description"),
      textInput("rr_web","Website / Core Marketplace URL"),
      textInput("rr_notes","Notes"),
      footer=tagList(modalButton("Cancel"),actionButton("btn_save_rrid","Save",class="btn-success"))))
  })
  observeEvent(input$btn_save_rrid, {
    req(nchar(trimws(input$rr_rrid))>0)
    rrid_rv(bind_rows(rrid_rv(),tibble(core_name=input$rr_core,rrid=trimws(input$rr_rrid),
      description=trimws(input$rr_desc),website=trimws(input$rr_web),notes=trimws(input$rr_notes))))
    removeModal(); showNotification("Saved.",type="message")
  })
  output$rrid_table <- renderDT({
    df <- rrid_rv()
    if (nrow(df)==0) return(datatable(tibble(Status="No RRIDs registered yet. Add each core's RRID from Core Marketplace.")))
    datatable(df,options=list(pageLength=10,scrollX=TRUE),rownames=FALSE)
  })
  output$dl_rrid <- downloadHandler(filename=function() paste0("rrid_registry_",Sys.Date(),".csv"),content=function(f) write_csv(rrid_rv(),f))

  # ── Acknowledgment Checker ────────────────────────────────
  ack_result <- reactiveVal(NULL)
  observeEvent(input$btn_check_ack, {
    txt <- trimws(input$ack_text)
    if (nchar(txt)==0) { ack_result(list(ok=FALSE, msg="Paste acknowledgment text first.")); return() }
    has_num   <- grepl("P20GM103429", txt, ignore.case=TRUE)
    has_name  <- grepl("AR INBRE|ARINBRE|Arkansas INBRE|arkansas inbre", txt, ignore.case=TRUE)
    has_nigms <- grepl("NIGMS|National Institute of General Medical Sciences", txt, ignore.case=TRUE)
    ack_result(list(ok=has_num && has_name, has_num=has_num, has_name=has_name, has_nigms=has_nigms))
  })
  output$ack_result <- renderUI({
    r <- ack_result()
    if (is.null(r)) return(NULL)
    if (!r$ok) {
      issues <- c(
        if (!r$has_num)  "❌ Grant number P20GM103429 not found",
        if (!r$has_name) "❌ 'AR INBRE' or 'Arkansas INBRE' not found",
        if (!r$has_nigms)"⚠️  NIGMS / National Institute of General Medical Sciences not mentioned"
      )
      div(class="alert alert-danger mt-2",
          tags$b("Acknowledgment may be non-compliant:"),
          tags$ul(lapply(issues, tags$li)),
          tags$small("Recommended text: 'This work was supported by the Arkansas INBRE program, supported by a grant from the National Institute of General Medical Sciences (NIGMS), P20GM103429.'"))
    } else {
      div(class="alert alert-success mt-2",
          tags$b("✅ Acknowledgment looks compliant!"),
          tags$p(class="mb-0 small",
            paste("Contains grant number:", if(r$has_num) "✓" else "✗"),
            "|",
            paste("INBRE name:", if(r$has_name) "✓" else "✗"),
            "|",
            paste("NIGMS:", if(r$has_nigms) "✓" else "—")))
    }
  })

  # ── RPPR Export ───────────────────────────────────────────
  output$b1_summary <- renderUI({
    df <- all_pubs() %>% filter(!flagged)
    comp <- sum(df$compliance=="compliant")
    pend <- sum(df$compliance=="pending")
    ncom <- sum(df$compliance=="non_compliant")
    tags$p(tags$b("Total: "),nrow(df)," | ✅ ",comp," | 🔵 ",pend," | 🔴 ",ncom,class="text-muted")
  })
  output$b1_preview <- renderText(b1_text())
  output$dl_b1 <- downloadHandler(filename=function() paste0("rppr_b1_",Sys.Date(),".txt"),content=function(f) writeLines(b1_text(),f))

  b6_text <- reactive({
    pubs <- all_pubs() %>% filter(!flagged)
    pres <- pres_rv(); pipe <- pipeline_filtered()
    paste0(
      "AR INBRE (P20GM103429) — RPPR Section B.6 Summary\nGenerated: ",Sys.Date(),"\n",
      strrep("─",50),"\n\n",
      "PUBLICATIONS\n",
      "  Total: ",nrow(pubs),"\n",
      "  PMC Compliant: ",sum(pubs$compliance=="compliant")," of ",nrow(pubs),"\n\n",
      "PRESENTATIONS & POSTERS\n",
      "  Total: ",nrow(pres),"\n",
      "  Posters: ",sum(pres$type=="Poster",na.rm=TRUE),
      " | Oral: ",sum(pres$type=="Oral Presentation",na.rm=TRUE),"\n\n",
      "CORE USE (Table 4)\n",
      "  Records logged: ",nrow(core_rv()),"\n",
      "  Contributed to grant apps: ",sum(core_rv()$contributed_grant,na.rm=TRUE),"\n",
      "  Contributed to publications: ",sum(core_rv()$contributed_pub,na.rm=TRUE),"\n\n",
      "EDUCATION & OUTREACH (Table 3)\n",
      "  Activities logged: ",nrow(outreach_rv()),"\n",
      "  Total participants: ",sum(outreach_rv()$n_faculty+outreach_rv()$n_students,na.rm=TRUE),"\n\n",
      "GRANT PIPELINE\n",
      "  Grants tracked: ",nrow(pipe),"\n",
      "  Total funding: $",format(sum(pipe$award_amount,na.rm=TRUE),big.mark=",",scientific=FALSE),"\n\n",
      "DMS PLANS\n",
      "  Projects tracked: ",nrow(dms_rv()),"\n",
      "  Plans in place: ",sum(dms_rv()$dms_plan_exists,na.rm=TRUE),"\n\n",
      strrep("─",50),"\n",
      "Add narrative text for NIH submission below this line.\n")
  })
  output$b6_preview <- renderText(b6_text())
  output$dl_b6 <- downloadHandler(filename=function() paste0("rppr_b6_",Sys.Date(),".txt"),content=function(f) writeLines(b6_text(),f))

  output$exp_t3 <- renderDT({ df <- outreach_rv()
    if (nrow(df)==0) return(datatable(tibble(Status="No outreach activities logged yet.")))
    datatable(df,options=list(scrollX=TRUE),rownames=FALSE) })
  output$exp_t4 <- renderDT({ df <- core_rv()
    if (nrow(df)==0) return(datatable(tibble(Status="No core use entries yet.")))
    datatable(df,options=list(scrollX=TRUE),rownames=FALSE) })

  output$dl_all_xlsx <- downloadHandler(
    filename=function() paste0("arinbre_nigms_tables_",Sys.Date(),".xlsx"),
    content=function(f) write_xlsx(list(
      "Table 1A"=t1a_data(), "Table 1B"=t1b_data(),
      "Table 3 Outreach"=outreach_rv(), "Table 4 Core Use"=core_rv(),
      "Publications"=all_pubs(), "Presentations"=pres_rv(),
      "DMS Plans"=dms_rv()), f)
  )
  output$dl_all_csv_zip <- downloadHandler(
    filename=function() paste0("arinbre_tables_",Sys.Date(),".csv"),
    content=function(f) {
      combined <- bind_rows(
        t1a_data() %>% mutate(.table="Table1A"),
        t1b_data() %>% mutate(.table="Table1B"))
      write_csv(combined, f)
    }
  )

  output$noncompliance_panel <- renderUI({
    df <- all_pubs() %>% filter(compliance=="non_compliant",!flagged)
    if (nrow(df)==0) return(tags$p("✅ No non-compliant publications.",class="text-success"))
    by_auth <- df %>% group_by(authors) %>%
      summarise(n=n(),titles=paste(title,collapse="; "),.groups="drop")
    lapply(seq_len(nrow(by_auth)), function(i) {
      r <- by_auth[i,]
      card(class="mb-2 border-danger",
           card_header(paste0("📧 ",r$authors," — ",r$n," paper(s)"),class="bg-danger text-white"),
           tags$p(tags$b("Titles: "),r$titles,class="small px-2 pt-2"))
    })
  })
  output$dl_emails <- downloadHandler(
    filename=function() paste0("noncompliance_emails_",Sys.Date(),".txt"),
    content=function(f) {
      df <- all_pubs() %>% filter(compliance=="non_compliant",!flagged)
      if (nrow(df)==0) { writeLines("No non-compliant publications.",f); return() }
      by_auth <- df %>% group_by(authors) %>%
        summarise(n=n(),titles=paste(title,collapse="\n   - "),.groups="drop")
      drafts <- sapply(seq_len(nrow(by_auth)), function(i) {
        r <- by_auth[i,]
        paste0("TO: ",r$authors,"\nSUBJECT: Action Required — PMC Compliance\n\n",
               "Dear ",r$authors,",\n\nCompliance review for AR INBRE (P20GM103429) found ",
               r$n," publication(s) not deposited in PMC:\n\n   - ",r$titles,
               "\n\nUpload at: https://www.ncbi.nlm.nih.gov/pmc/about/submission-methods/\n\n",
               "Reply with PMCID(s) once submitted.\n\nAR INBRE Program | UAMS\n",strrep("-",60))
      })
      writeLines(paste(drafts,collapse="\n\n"),f)
    }
  )
}

shinyApp(ui=ui, server=server)
