library(shiny)
library(pdftools)
library(httr)
library(future)
library(promises)

plan(multisession)

# ══════════════════════════════════════════════════════════════════════════════
#  辅助函数
# ══════════════════════════════════════════════════════════════════════════════

fix_path <- function(p) gsub("\\\\", "/", trimws(p))

read_csv_auto <- function(path) {
  raw <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
  sep <- if (lengths(regmatches(raw, gregexpr("\t", raw))) >= 2) "\t" else ","
  read.csv(path, sep = sep, stringsAsFactors = FALSE,
           check.names = FALSE, quote = '"', encoding = "UTF-8")
}

parse_urls <- function(cell) {
  if (is.na(cell) || trimws(cell) == "") return(list(protocol = NA, sap = NA))
  entries <- trimws(strsplit(cell, "\\|")[[1]])
  prot <- NA_character_; sap <- NA_character_
  for (entry in entries) {
    m <- regmatches(entry, regexpr("https?://\\S+\\.pdf", entry))
    if (!length(m)) next
    url <- m[[1]]
    if      (grepl("Prot_SAP|Protocol|Prot_",             entry, ignore.case = TRUE) && is.na(prot)) prot <- url
    else if (grepl("Statistical Analysis Plan|SAP_", entry, ignore.case = TRUE) && is.na(sap))  sap  <- url
  }
  list(protocol = prot, sap = sap)
}

get_targets <- function(cell, doc_type, trial_id, pdf_dir) {
  urls <- parse_urls(cell)
  targets <- list()
  if (doc_type == "protocol" && !is.na(urls$protocol))
    targets[[1]] <- list(url = urls$protocol, path = file.path(pdf_dir, paste0(trial_id, ".pdf")))
  else if (doc_type == "sap" && !is.na(urls$sap))
    targets[[1]] <- list(url = urls$sap, path = file.path(pdf_dir, paste0(trial_id, ".pdf")))
  else if (doc_type == "both") {
    if (!is.na(urls$protocol)) targets[[1]] <- list(url = urls$protocol, path = file.path(pdf_dir, paste0(trial_id, "_protocol.pdf")))
    if (!is.na(urls$sap))      targets[[2]] <- list(url = urls$sap,      path = file.path(pdf_dir, paste0(trial_id, "_sap.pdf")))
  }
  targets
}

run_download <- function(df, nct_col, doc_col, doc_type, pdf_dir, log_file, stat_file, stop_flag) {
  write_log  <- function(msg) cat(paste0(msg, "\n"), file = log_file, append = TRUE)
  write_stat <- function(ok, skip, fail, done) {
    writeLines(as.character(c(ok, skip, fail, done)), stat_file)
  }
  hdrs <- c("User-Agent" = "Mozilla/5.0", "Accept" = "application/pdf,*/*")
  n    <- nrow(df)
  ok <- skip <- fail <- 0
  write_log(sprintf("▶ Download started | Type: %s | Total: %d", toupper(doc_type), n))
  for (i in seq_len(n)) {
    if (file.exists(stop_flag)) {
      write_log("■ Download stopped by user")
      write_stat(ok, skip, fail, "stopped")
      return(invisible(NULL))
    }
    trial_id <- trimws(df[[nct_col]][i])
    targets  <- get_targets(df[[doc_col]][i], doc_type, trial_id, pdf_dir)
    if (!length(targets)) {
      write_log(sprintf("⚠ [%s] No %s document found", trial_id, doc_type))
      fail <- fail + 1
      write_stat(ok, skip, fail, i)
      next
    }
    for (tgt in targets) {
      if (file.exists(tgt$path)) {
        skip <- skip + 1
        write_stat(ok, skip, fail, i)
        next
      }
      result <- tryCatch({
        resp <- GET(tgt$url, add_headers(.headers = hdrs), timeout(30))
        if (http_error(resp)) stop(http_status(resp)$message)
        writeBin(content(resp, "raw"), tgt$path)
        "ok"
      }, error = function(e) e$message)
      if (result == "ok") {
        ok <- ok + 1
        write_log(sprintf("✓ [%s] %s", trial_id, basename(tgt$path)))
      } else {
        fail <- fail + 1
        write_log(sprintf("✗ [%s] %s", trial_id, result))
      }
      write_stat(ok, skip, fail, i)
    }
  }
  write_log(sprintf("✅ All done — Downloaded: %d  Skipped: %d  Failed: %d", ok, skip, fail))
  write_stat(ok, skip, fail, "done")
  invisible(NULL)
}


# ══════════════════════════════════════════════════════════════════════════════
#  UI
# ══════════════════════════════════════════════════════════════════════════════

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { font-family: 'Segoe UI', Arial, sans-serif; background: #f0f4f8; margin: 0; }

      /* ── 顶部标题栏 ── */
      .app-header {
        background: linear-gradient(135deg, #1a3a5c 0%, #2980b9 100%);
        color: white; padding: 18px 36px 15px;
      }
      .app-header h2  { color: white; font-weight: 800; margin: 0 0 3px; font-size: 24px;
                        letter-spacing: 0.5px; }
      .app-header .app-sub { color: rgba(255,255,255,0.65); font-size: 12.5px; margin: 0; }
      .app-logo { display: flex; align-items: center; gap: 14px; }
      .title-row { display: flex; align-items: baseline; gap: 10px; }
      .ver-badge {
        color: rgba(255,255,255,0.6); font-size: 11px; font-weight: 700;
        letter-spacing: 1px; background: rgba(255,255,255,0.15);
        padding: 2px 8px; border-radius: 10px;
      }
      .cn-title { color: rgba(255,255,255,0.45); font-size: 14px; font-weight: 400; }

      /* ── 标签页 ── */
      .nav-tabs { border-bottom: 2px solid #2980b9; background: white; padding: 0 28px; }
      .nav-tabs > li > a {
        color: #5d6d7e; border: none !important;
        border-bottom: 3px solid transparent !important;
        padding: 12px 22px; font-weight: 500; font-size: 14px;
        margin-bottom: -2px; border-radius: 0 !important;
      }
      .nav-tabs > li.active > a, .nav-tabs > li > a:hover {
        color: #2980b9 !important; border-bottom: 3px solid #2980b9 !important;
        background: transparent !important;
      }
      .tab-content { background: white; }

      /* ── 主卡片 ── */
      .main-card {
        background: white; padding: 28px 36px 36px;
        box-shadow: 0 4px 18px rgba(0,0,0,0.07);
      }

      .divider { border: none; border-top: 1px solid #eaecef; margin: 20px 0; }

      .sec-label {
        font-size: 11.5px; font-weight: 700; color: #34495e;
        text-transform: uppercase; letter-spacing: 0.7px; margin-bottom: 8px;
      }

      .form-control { border-radius: 6px; border: 1px solid #dde1e7; font-size: 14px; }
      .form-control:focus { border-color: #2980b9; box-shadow: 0 0 0 3px rgba(41,128,185,0.13); }

      /* ── 文档类型按钮 ── */
      .doc-btn-group { display: flex; gap: 8px; }
      .doc-btn {
        flex: 1; padding: 9px 4px; border-radius: 7px; border: 2px solid #dde1e7;
        background: white; font-size: 13px; font-weight: 500; color: #5d6d7e;
        cursor: pointer; transition: all 0.15s; text-align: center;
      }
      .doc-btn:hover  { border-color: #2980b9; color: #2980b9; background: #f0f7ff; }
      .doc-btn.active { border-color: #2980b9; background: #2980b9; color: white; font-weight: 600; }

      /* ── 操作按钮 ── */
      .btn-dl {
        background: #2980b9; color: white; border: none; border-radius: 7px;
        padding: 11px 0; font-size: 15px; font-weight: 700; width: 100%;
        cursor: pointer; transition: background 0.2s;
      }
      .btn-dl:hover { background: #1f6391; }
      .btn-stop {
        background: #e74c3c; color: white; border: none; border-radius: 7px;
        padding: 11px 0; font-size: 15px; font-weight: 700; width: 100%;
        cursor: pointer; transition: background 0.2s;
      }
      .btn-stop:hover { background: #c0392b; }
      .btn-search {
        background: #27ae60; color: white; border: none; border-radius: 7px;
        padding: 9px 0; font-size: 14px; font-weight: 700; width: 100%;
        cursor: pointer; transition: background 0.2s; margin-top: 6px;
      }
      .btn-search:hover { background: #1e8449; }

      /* ── 统计卡片 ── */
      .stat-row { display: flex; gap: 10px; margin-bottom: 14px; }
      .stat-box {
        flex: 1; background: #f0f7ff; border-left: 4px solid #2980b9;
        border-radius: 7px; padding: 12px 14px; text-align: center;
      }
      .stat-box.green  { background: #f0fff4; border-color: #27ae60; }
      .stat-box.orange { background: #fff8f0; border-color: #e67e22; }
      .stat-box.red    { background: #fff5f5; border-color: #e74c3c; }
      .stat-num   { font-size: 26px; font-weight: 800; color: #2c3e50; line-height: 1; }
      .stat-label { font-size: 11px; color: #7f8c8d; margin-top: 4px;
                    text-transform: uppercase; letter-spacing: 0.5px; }

      /* ── 进度条 ── */
      .prog-track { background: #ecf0f1; border-radius: 6px; height: 8px;
                    overflow: hidden; margin: 6px 0 3px; }
      .prog-fill  { height: 100%; border-radius: 6px;
                    background: linear-gradient(90deg, #2980b9, #5dade2);
                    transition: width 0.4s ease; width: 0%; }
      .prog-fill.green { background: linear-gradient(90deg, #27ae60, #58d68d); }
      .prog-pct   { font-size: 11px; color: #95a5a6; text-align: right; }

      /* ── 状态徽章 ── */
      .status-pill {
        display: inline-flex; align-items: center; gap: 6px;
        padding: 4px 12px; border-radius: 20px; font-size: 12px; font-weight: 600;
      }
      .pill-idle    { background: #eaecef; color: #7f8c8d; }
      .pill-running { background: #d6eaf8; color: #2980b9; }
      .pill-done    { background: #d5f5e3; color: #1e8449; }
      .pill-stopped { background: #fadbd8; color: #c0392b; }
      .blink { animation: blink 1s infinite; }
      @keyframes blink { 0%,100%{opacity:1} 50%{opacity:0.3} }

      /* ── 日志框 ── */
      .log-box {
        background: #1a2332; color: #7ecf88; font-family: 'Courier New', monospace;
        font-size: 12px; padding: 14px 16px; border-radius: 8px;
        height: 220px; overflow-y: auto; white-space: pre-wrap; line-height: 1.65;
        border: 1px solid #243447;
      }
      .log-warn  { color: #f0ad4e; }
      .log-error { color: #e74c3c; }
      .log-head  { color: #5dade2; font-weight: bold; }

      /* ── 提示条 ── */
      .hint-ok {
        font-size: 12px; color: #27ae60; margin-top: 5px;
        padding: 4px 10px; background: #f0fff4;
        border-radius: 4px; border-left: 3px solid #27ae60;
      }

      /* ── 关键词标签 ── */
      .kw-tag {
        display: inline-block; background: #ebf5fb; color: #2980b9;
        border-radius: 12px; padding: 3px 10px; font-size: 12px;
        margin: 3px 2px; font-weight: 500;
      }

      /* ── 搜索区 ── */
      .search-section {
        background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 8px;
        padding: 18px 20px; margin-top: 4px;
      }

      /* ── 脚注 ── */
      .app-footer {
        background: #1a3a5c; color: rgba(255,255,255,0.45);
        font-size: 11.5px; text-align: center;
        padding: 12px 36px; margin-top: 0;
        display: flex; justify-content: space-between; align-items: center;
      }
      .app-footer a { color: rgba(255,255,255,0.55); text-decoration: none; }
      .app-footer a:hover { color: white; }
      .footer-copy { font-weight: 600; color: rgba(255,255,255,0.6); }

      /* ── Guide ── */
      .guide-card { background: #f8fafc; border-radius: 9px; padding: 22px 26px;
                    border: 1px solid #e2e8f0; margin-bottom: 18px; }
      .guide-card h4 { color: #1a3a5c; font-size: 15px; font-weight: 700;
                       margin: 0 0 14px; padding-bottom: 9px;
                       border-bottom: 2px solid #2980b9; display: inline-block; }
      .guide-step { display: flex; align-items: flex-start; margin-bottom: 13px; gap: 13px; }
      .guide-step-num { background: #2980b9; color: white; border-radius: 50%;
                        width: 24px; height: 24px; min-width: 24px; display: flex;
                        align-items: center; justify-content: center;
                        font-size: 12px; font-weight: 700; }
      .guide-step-title { font-weight: 600; color: #2c3e50; font-size: 13.5px; margin-bottom: 3px; }
      .guide-step-desc  { color: #5d6d7e; font-size: 13px; line-height: 1.6; }
      .guide-note { background: #fffbea; border-left: 4px solid #f39c12; border-radius: 5px;
                    padding: 10px 14px; font-size: 13px; color: #7d6608; line-height: 1.6; }
      .guide-code { background: #1a2332; color: #7ecf88; border-radius: 5px;
                    padding: 8px 12px; font-family: monospace; font-size: 12px;
                    margin: 6px 0; line-height: 1.6; }
    ")),
    
    tags$script(HTML("
      Shiny.addCustomMessageHandler('setProgDl', function(p) {
        var el = document.getElementById('dl_prog_fill');
        if (el) el.style.width = p + '%';
      });
      Shiny.addCustomMessageHandler('setProgSr', function(p) {
        var el = document.getElementById('sr_prog_fill');
        if (el) el.style.width = p + '%';
      });
      Shiny.addCustomMessageHandler('scrollLog', function(id) {
        var el = document.getElementById(id);
        if (el) el.scrollTop = el.scrollHeight;
      });
      function setDocType(v) {
        ['protocol','sap','both'].forEach(function(t) {
          document.getElementById('doc_'+t).classList.remove('active');
        });
        document.getElementById('doc_'+v).classList.add('active');
        var el = document.getElementById('doc_type');
        el.value = v; el.dispatchEvent(new Event('change'));
        Shiny.setInputValue('doc_type', v);
      }
    "))
  ),
  
  # ── 顶部标题栏 ────────────────────────────────────────────────────────────
  div(class = "app-header",
      div(class = "app-logo",
          tags$span(style = "font-size:34px; line-height:1;", "🔬"),
          div(
            div(class = "title-row",
                h2("TrialLens"),
                tags$span(class = "ver-badge", "v1.0"),
                tags$span(class = "cn-title", "临镜")
            ),
            p(class = "app-sub",
              "Batch download & keyword-screen Protocol / SAP from ClinicalTrials.gov",
              tags$span(style = "margin:0 6px; opacity:0.4;", "|"),
              "批量下载并关键词筛选临床试验文件")
          )
      )
  ),
  
  # ── 标签页 ────────────────────────────────────────────────────────────────
  tabsetPanel(id = "tabs", type = "tabs",
              
              # ══════════════════════════════════════════════════════
              #  Tab 1 — Tool
              # ══════════════════════════════════════════════════════
              tabPanel("🛠 Tool / 工具",
                       div(class = "main-card",
                           fluidRow(
                             
                             # ── 左列：输入
                             column(5,
                                    div(class = "sec-label", "① Trial List CSV  /  试验列表"),
                                    fileInput("csv_file", label = NULL, accept = ".csv",
                                              buttonLabel = "Browse / 选择",
                                              placeholder = "ClinicalTrials.gov export"),
                                    uiOutput("csv_hint"),
                                    
                                    div(style = "margin-top:16px;", class = "sec-label",
                                        "② PDF Output Folder  /  输出目录"),
                                    textInput("pdf_dir", label = NULL,
                                              placeholder = "e.g.  F:\\test\\output",
                                              width = "100%"),
                                    
                                    div(style = "margin-top:16px;", class = "sec-label",
                                        "③ Document Type  /  文档类型"),
                                    div(class = "doc-btn-group",
                                        tags$button(id = "doc_protocol", class = "doc-btn active",
                                                    onclick = "setDocType('protocol')", "📄 Protocol"),
                                        tags$button(id = "doc_sap",      class = "doc-btn",
                                                    onclick = "setDocType('sap')",      "📊 SAP"),
                                        tags$button(id = "doc_both",     class = "doc-btn",
                                                    onclick = "setDocType('both')",     "📋 Both / 两者")
                                    ),
                                    tags$input(id = "doc_type", type = "hidden", value = "protocol"),
                                    
                                    hr(class = "divider"),
                                    
                                    div(class = "sec-label", "Download  /  下载"),
                                    uiOutput("dl_btn_ui"),
                                    
                                    hr(class = "divider"),
                                    
                                    div(class = "sec-label", "④ Keyword Search  /  关键词筛选"),
                                    div(class = "search-section",
                                        p(style = "font-size:12px;color:#7f8c8d;margin:0 0 10px;",
                                          "Scans PDFs already in the output folder above  /  扫描上方目录中已下载的 PDF"),
                                        textInput("keywords", label = NULL,
                                                  placeholder = "RPSFT, rank preserving  (comma / 逗号分隔)",
                                                  width = "100%"),
                                        uiOutput("kw_tags"),
                                        tags$button("🔍  Search / 开始筛选", class = "btn-search",
                                                    onclick = "Shiny.setInputValue('search_btn', Math.random())")
                                    )
                             ),
                             
                             # ── 右列：仪表盘 + 日志
                             column(7,
                                    div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
                                        div(class = "sec-label", style = "margin:0;", "Download Progress  /  下载进度"),
                                        uiOutput("dl_status_pill")
                                    ),
                                    div(class = "stat-row",
                                        div(class = "stat-box",
                                            div(class = "stat-num", textOutput("dl_total", inline = TRUE)),
                                            div(class = "stat-label", "Total / 总计")
                                        ),
                                        div(class = "stat-box green",
                                            div(class = "stat-num", textOutput("dl_ok", inline = TRUE)),
                                            div(class = "stat-label", "Downloaded / 已下载")
                                        ),
                                        div(class = "stat-box orange",
                                            div(class = "stat-num", textOutput("dl_skip", inline = TRUE)),
                                            div(class = "stat-label", "Skipped / 跳过")
                                        ),
                                        div(class = "stat-box red",
                                            div(class = "stat-num", textOutput("dl_fail", inline = TRUE)),
                                            div(class = "stat-label", "Failed / 失败")
                                        )
                                    ),
                                    div(class = "prog-track", div(id = "dl_prog_fill", class = "prog-fill")),
                                    div(class = "prog-pct", textOutput("dl_pct", inline = TRUE)),
                                    div(class = "log-box", id = "dl_log_box", htmlOutput("dl_log")),
                                    
                                    hr(class = "divider"),
                                    
                                    div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:12px;",
                                        div(class = "sec-label", style = "margin:0;", "Search Results  /  筛选结果"),
                                        uiOutput("sr_status_pill")
                                    ),
                                    div(class = "stat-row",
                                        div(class = "stat-box",
                                            div(class = "stat-num", textOutput("sr_total",   inline = TRUE)),
                                            div(class = "stat-label", "PDFs Found / 总数")
                                        ),
                                        div(class = "stat-box green",
                                            div(class = "stat-num", textOutput("sr_matched", inline = TRUE)),
                                            div(class = "stat-label", "Matched / 命中")
                                        ),
                                        div(class = "stat-box orange",
                                            div(class = "stat-num", textOutput("sr_no",      inline = TRUE)),
                                            div(class = "stat-label", "No Match / 未命中")
                                        ),
                                        div(class = "stat-box red",
                                            div(class = "stat-num", textOutput("sr_err",     inline = TRUE)),
                                            div(class = "stat-label", "Errors / 失败")
                                        )
                                    ),
                                    div(class = "prog-track", div(id = "sr_prog_fill", class = "prog-fill green")),
                                    div(class = "prog-pct", textOutput("sr_pct", inline = TRUE)),
                                    div(class = "log-box", id = "sr_log_box", htmlOutput("sr_log"))
                             )
                           )
                       )
              ),
              
              # ══════════════════════════════════════════════════════
              #  Tab 2 — Guide EN
              # ══════════════════════════════════════════════════════
              tabPanel("📖 Guide (EN)",
                       div(class = "main-card",
                           div(class = "guide-card",
                               h4("🔬 What does TrialLens do?"),
                               p(style = "color:#5d6d7e;font-size:13.5px;line-height:1.8;",
                                 tags$strong("Download:"), " Given a CSV from ClinicalTrials.gov, batch-downloads Protocol and/or SAP PDFs
            named by NCT number. You can stop at any time and resume — already-downloaded files are skipped.", tags$br(),
                                 tags$strong("Search:"), " Scans all PDFs in the same output folder for keywords.
            Hits go to ", tags$code("matched/"), ", others to ", tags$code("not_matched/"), ".")
                           ),
                           div(class = "guide-card",
                               h4("📋 How to use"),
                               div(class="guide-step", div(class="guide-step-num","1"),
                                   div(class="guide-step-body",
                                       div(class="guide-step-title","Export trial list from ClinicalTrials.gov"),
                                       div(class="guide-step-desc",
                                           "Go to ", tags$a("clinicaltrials.gov", href="https://clinicaltrials.gov", target="_blank"),
                                           " → search → ", tags$strong("Download → CSV"),
                                           ". File must have ", tags$code("NCT Number"), " and ", tags$code("Study Documents"), " columns.")
                                   )
                               ),
                               div(class="guide-step", div(class="guide-step-num","2"),
                                   div(class="guide-step-body",
                                       div(class="guide-step-title","① Upload CSV, ② set output folder, ③ choose document type"),
                                       div(class="guide-step-desc",
                                           tags$strong("Protocol"), " / ", tags$strong("SAP"), " / ", tags$strong("Both"),
                                           " — choose which document to download per trial.")
                                   )
                               ),
                               div(class="guide-step", div(class="guide-step-num","3"),
                                   div(class="guide-step-body",
                                       div(class="guide-step-title","Click ▶ Start Download — stop any time with ■ Stop"),
                                       div(class="guide-step-desc",
                                           "Dashboard updates live. Stop is safe — already-downloaded files are preserved.
                Re-running skips files already on disk.")
                                   )
                               ),
                               div(class="guide-step", div(class="guide-step-num","4"),
                                   div(class="guide-step-body",
                                       div(class="guide-step-title","④ Enter keywords and click 🔍 Search"),
                                       div(class="guide-step-desc",
                                           "Searches the same output folder. Comma-separate multiple keywords.
                A PDF is a hit if ", tags$em("any"), " keyword matches (case-insensitive).")
                                   )
                               )
                           ),
                           div(class = "guide-card",
                               h4("📁 Output structure"),
                               div(class="guide-code",
                                   "F:/output/", tags$br(),
                                   "├── NCT01714739.pdf    ← downloaded", tags$br(),
                                   "├── matched/           ← keyword hits", tags$br(),
                                   "└── not_matched/       ← no match"
                               )
                           ),
                           div(class = "guide-card",
                               h4("⚠️ Notes"),
                               div(class="guide-note",
                                   tags$strong("Paths:"), " Both ", tags$code("F:\\test"), " and ", tags$code("F:/test"),
                                   " work — backslashes convert automatically.", tags$br(), tags$br(),
                                   tags$strong("Case sensitivity:"), " Keyword matching is case-insensitive.",tags$br(), tags$br(),
                                   tags$strong("Stop & resume:"), " Safe to stop mid-download. Re-run skips existing files.", tags$br(), tags$br(),
                                   tags$strong("Packages needed:"),
                                   div(class="guide-code", 'install.packages(c("shiny","pdftools","httr","future","promises"))')
                               )
                           )
                       )
              ),
              
              # ══════════════════════════════════════════════════════
              #  Tab 3 — 使用指南
              # ══════════════════════════════════════════════════════
              tabPanel("📖 使用指南",
                       div(class = "main-card",
                           div(class = "guide-card",
                               h4("🔬 临镜 · TrialLens 简介"),
                               p(style = "color:#5d6d7e;font-size:13.5px;line-height:1.8;",
                                 tags$strong("下载："), "根据 ClinicalTrials.gov 导出的 CSV，批量下载 Protocol / SAP PDF，以 NCT 编号命名。
            可随时停止，已下载文件不丢失，重新运行时自动跳过。", tags$br(),
                                 tags$strong("筛选："), "扫描同一输出目录中的 PDF，命中关键词的文件移入 ",
                                 tags$code("matched/"), "，未命中的移入 ", tags$code("not_matched/"), "。")
                           ),
                           div(class = "guide-card",
                               h4("📋 操作步骤"),
                               div(class="guide-step", div(class="guide-step-num","1"),
                                   div(class="guide-step-body",
                                       div(class="guide-step-title","从 ClinicalTrials.gov 导出试验列表"),
                                       div(class="guide-step-desc",
                                           "打开 ", tags$a("clinicaltrials.gov", href="https://clinicaltrials.gov", target="_blank"),
                                           "，检索后点击 ", tags$strong("Download → CSV"),
                                           "，确保文件包含 ", tags$code("NCT Number"), " 和 ", tags$code("Study Documents"), " 列。")
                                   )
                               ),
                               div(class="guide-step", div(class="guide-step-num","2"),
                                   div(class="guide-step-body",
                                       div(class="guide-step-title","① 上传 CSV，② 填写输出目录，③ 选择文档类型"),
                                       div(class="guide-step-desc",
                                           tags$strong("Protocol（研究方案）"), " / ",
                                           tags$strong("SAP（统计分析计划）"), " / ",
                                           tags$strong("Both（两者）"), " 三选一。")
                                   )
                               ),
                               div(class="guide-step", div(class="guide-step-num","3"),
                                   div(class="guide-step-body",
                                       div(class="guide-step-title","点击 ▶ 开始下载，随时可点 ■ 停止"),
                                       div(class="guide-step-desc",
                                           "右侧仪表盘实时刷新进度。停止后已下载文件完整保留，再次运行会从断点续传。")
                                   )
                               ),
                               div(class="guide-step", div(class="guide-step-num","4"),
                                   div(class="guide-step-body",
                                       div(class="guide-step-title","④ 输入关键词，点击 🔍 开始筛选"),
                                       div(class="guide-step-desc",
                                           "扫描同一输出目录中的所有 PDF。多个关键词用英文逗号分隔，",
                                           "任意一个命中即判定为匹配。", tags$strong("关键词匹配不区分大小写。"))
                                   )
                               )
                           ),
                           div(class = "guide-card",
                               h4("📁 输出文件夹结构"),
                               div(class="guide-code",
                                   "F:/output/", tags$br(),
                                   "├── NCT01714739.pdf    ← 下载的文件", tags$br(),
                                   "├── matched/           ← 命中关键词", tags$br(),
                                   "└── not_matched/       ← 未命中"
                               )
                           ),
                           div(class = "guide-card",
                               h4("⚠️ 注意事项"),
                               div(class="guide-note",
                                   tags$strong("路径格式："), tags$code("F:\\test"), " 和 ", tags$code("F:/test"),
                                   " 均可，自动转换。", tags$br(), tags$br(),
                                   tags$strong("大小写："), "关键词匹配不区分大小写，RPSFT / rpsft / Rpsft 效果相同。", tags$br(), tags$br(),
                                   tags$strong("随时停止："), "安全中断，再次运行从断点续传。", tags$br(), tags$br(),
                                   tags$strong("所需 R 包："),
                                   div(class="guide-code", 'install.packages(c("shiny","pdftools","httr","future","promises"))')
                               )
                           )
                       )
              )
              
  ),  # end tabsetPanel
  
  # ── 脚注 ──────────────────────────────────────────────────────────────────
  div(class = "app-footer",
      div(
        tags$span(class = "footer-copy", "TrialLens  临镜  v1.0"),
        tags$span(style = "margin:0 10px; opacity:0.3;", "·"),
        tags$span("© ", format(Sys.Date(), "%Y"), " Dr Zhang. All rights reserved.")
      ),
      div(
        tags$span("Built with "),
        tags$a("R Shiny", href = "https://shiny.posit.co", target = "_blank"),
        tags$span("  |  For research use only  |  仅供学术研究使用")
      )
  )
)


# ══════════════════════════════════════════════════════════════════════════════
#  Server
# ══════════════════════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  tmp       <- tempdir()
  log_file  <- file.path(tmp, "dl_log.txt")
  stat_file <- file.path(tmp, "dl_stat.txt")
  stop_flag <- file.path(tmp, "dl_stop")
  
  dl_state <- reactiveValues(status = "idle", total = 0)
  sr_state <- reactiveValues(status = "idle", log = "",
                             total = 0, matched = 0, no = 0, err = 0)
  
  # ── CSV ───────────────────────────────────────────────────────────────────
  csv_data <- reactive({
    req(input$csv_file)
    read_csv_auto(input$csv_file$datapath)
  })
  
  output$csv_hint <- renderUI({
    df <- tryCatch(csv_data(), error = function(e) NULL)
    if (is.null(df)) return(NULL)
    div(class = "hint-ok", sprintf("✓  %d trials loaded / 已读取 %d 条", nrow(df), nrow(df)))
  })
  
  # ── 关键词标签 ────────────────────────────────────────────────────────────
  output$kw_tags <- renderUI({
    req(input$keywords)
    kws <- trimws(strsplit(input$keywords, ",")[[1]])
    kws <- kws[nchar(kws) > 0]
    if (!length(kws)) return(NULL)
    div(style = "margin-top:7px;",
        tagList(lapply(kws, function(k) span(class = "kw-tag", k))))
  })
  
  # ── 下载按钮 ─────────────────────────────────────────────────────────────
  output$dl_btn_ui <- renderUI({
    if (dl_state$status == "running") {
      tags$button("■  Stop Download / 停止下载", class = "btn-stop",
                  onclick = "Shiny.setInputValue('stop_btn', Math.random())")
    } else {
      tags$button("▶  Start Download / 开始下载", class = "btn-dl",
                  onclick = "Shiny.setInputValue('start_btn', Math.random())")
    }
  })
  
  # ── 状态徽章 ─────────────────────────────────────────────────────────────
  output$dl_status_pill <- renderUI({
    switch(dl_state$status,
           idle    = div(class = "status-pill pill-idle",    "● Idle"),
           running = div(class = "status-pill pill-running", span(class="blink","●"), " Downloading…"),
           done    = div(class = "status-pill pill-done",    "✓ Done"),
           stopped = div(class = "status-pill pill-stopped", "■ Stopped")
    )
  })
  output$sr_status_pill <- renderUI({
    switch(sr_state$status,
           idle    = div(class = "status-pill pill-idle",    "● Idle"),
           running = div(class = "status-pill pill-running", span(class="blink","●"), " Searching…"),
           done    = div(class = "status-pill pill-done",    "✓ Done")
    )
  })
  
  # ── 定时轮询 ─────────────────────────────────────────────────────────────
  poll_timer    <- reactiveTimer(800)
  dl_log_content <- reactiveVal("")
  dl_ok_val     <- reactiveVal(0)
  dl_skip_val   <- reactiveVal(0)
  dl_fail_val   <- reactiveVal(0)
  dl_done_val   <- reactiveVal(0)
  
  observe({
    poll_timer()
    if (dl_state$status != "running") return()
    if (file.exists(log_file)) {
      lines <- readLines(log_file, warn = FALSE, encoding = "UTF-8")
      html  <- paste(sapply(lines, function(l) {
        cls <- if (grepl("^✗|failed|error", l, ignore.case=TRUE)) "log-error"
        else if (grepl("^⚠|warn|stop", l, ignore.case=TRUE)) "log-warn"
        else if (grepl("^▶|^✅", l)) "log-head"
        else ""
        if (cls != "") sprintf('<span class="%s">%s</span>', cls, l) else l
      }), collapse="\n")
      dl_log_content(html)
      session$sendCustomMessage("scrollLog", "dl_log_box")
    }
    if (file.exists(stat_file)) {
      vals <- tryCatch(readLines(stat_file, warn=FALSE), error=function(e) NULL)
      if (length(vals) == 4) {
        dl_ok_val(as.integer(vals[1]))
        dl_skip_val(as.integer(vals[2]))
        dl_fail_val(as.integer(vals[3]))
        done_raw <- vals[4]
        if      (done_raw == "done")    { dl_state$status <- "done" }
        else if (done_raw == "stopped") { dl_state$status <- "stopped" }
        else {
          done <- as.integer(done_raw)
          dl_done_val(done)
          pct <- if (dl_state$total > 0) round(done / dl_state$total * 100) else 0
          session$sendCustomMessage("setProgDl", pct)
        }
      }
    }
  })
  
  observeEvent(dl_state$status, {
    if (dl_state$status %in% c("done", "stopped") && file.exists(log_file)) {
      lines <- readLines(log_file, warn = FALSE, encoding = "UTF-8")
      html  <- paste(sapply(lines, function(l) {
        cls <- if (grepl("^✗", l)) "log-error"
        else if (grepl("^⚠|■", l)) "log-warn"
        else if (grepl("^▶|^✅", l)) "log-head"
        else ""
        if (cls != "") sprintf('<span class="%s">%s</span>', cls, l) else l
      }), collapse="\n")
      dl_log_content(html)
      session$sendCustomMessage("setProgDl",
                                if (dl_state$total > 0)
                                  round((dl_ok_val()+dl_skip_val()+dl_fail_val()) / dl_state$total * 100)
                                else 100)
      session$sendCustomMessage("scrollLog", "dl_log_box")
    }
  })
  
  output$dl_log   <- renderUI({ HTML(dl_log_content()) })
  output$dl_total <- renderText(dl_state$total)
  output$dl_ok    <- renderText(dl_ok_val())
  output$dl_skip  <- renderText(dl_skip_val())
  output$dl_fail  <- renderText(dl_fail_val())
  output$dl_pct   <- renderText({
    if (dl_state$total == 0) return("—")
    done <- dl_ok_val() + dl_skip_val() + dl_fail_val()
    sprintf("%d / %d  (%.0f%%)", done, dl_state$total, done/dl_state$total*100)
  })
  
  # ── 开始下载 ─────────────────────────────────────────────────────────────
  observeEvent(input$start_btn, {
    if (dl_state$status == "running") return()
    pdf_dir  <- fix_path(input$pdf_dir)
    doc_type <- isolate(input$doc_type)
    if (is.null(doc_type) || doc_type == "") doc_type <- "protocol"
    
    errs <- character(0)
    if (is.null(input$csv_file)) errs <- c(errs, "Please upload a CSV file")
    if (pdf_dir == "")           errs <- c(errs, "Please enter the output folder path")
    if (length(errs)) {
      dl_log_content(paste(sapply(errs, function(e)
        sprintf('<span class="log-error">❌ %s</span>', e)), collapse="\n"))
      return()
    }
    
    df <- tryCatch(csv_data(), error = function(e) NULL)
    if (is.null(df)) {
      dl_log_content('<span class="log-error">❌ Failed to read CSV</span>'); return()
    }
    nct_col <- grep("nct.number",      names(df), ignore.case=TRUE, value=TRUE)[1]
    doc_col <- grep("study.documents", names(df), ignore.case=TRUE, value=TRUE)[1]
    if (is.na(nct_col) || is.na(doc_col)) {
      dl_log_content('<span class="log-error">❌ CSV missing NCT Number or Study Documents column</span>')
      return()
    }
    
    if (file.exists(log_file))  file.remove(log_file)
    if (file.exists(stat_file)) file.remove(stat_file)
    if (file.exists(stop_flag)) file.remove(stop_flag)
    dir.create(pdf_dir, recursive=TRUE, showWarnings=FALSE)
    
    dl_state$status <- "running"
    dl_state$total  <- nrow(df)
    dl_ok_val(0); dl_skip_val(0); dl_fail_val(0); dl_done_val(0)
    dl_log_content("")
    session$sendCustomMessage("setProgDl", 0)
    
    future_promise({
      run_download(df, nct_col, doc_col, doc_type, pdf_dir, log_file, stat_file, stop_flag)
    },
    packages = c("httr"),
    globals  = list(
      run_download = run_download,
      get_targets  = get_targets,
      parse_urls   = parse_urls,
      df           = df,
      nct_col      = nct_col,
      doc_col      = doc_col,
      doc_type     = doc_type,
      pdf_dir      = pdf_dir,
      log_file     = log_file,
      stat_file    = stat_file,
      stop_flag    = stop_flag
    )) %...>% (function(x) NULL) %...!% (function(e) {
      cat(paste0("Download error: ", e$message, "\n"), file = log_file, append = TRUE)
      writeLines(as.character(c("0","0","0","stopped")), stat_file)
    })
    
    NULL
  })
  
  # ── 停止下载 ─────────────────────────────────────────────────────────────
  observeEvent(input$stop_btn, {
    if (dl_state$status == "running") file.create(stop_flag)
  })
  
  # ── 搜索 ─────────────────────────────────────────────────────────────────
  observeEvent(input$search_btn, {
    pdf_dir <- fix_path(input$pdf_dir)
    kws     <- trimws(strsplit(input$keywords, ",")[[1]])
    kws     <- kws[nchar(kws) > 0]
    
    add_sr_log <- function(msg, type = "ok") {
      cls  <- switch(type, ok="", warn="log-warn", error="log-error", head="log-head", "")
      line <- if (cls != "") sprintf('<span class="%s">%s</span>', cls, msg) else msg
      sr_state$log <- paste0(sr_state$log, line, "\n")
      session$sendCustomMessage("scrollLog", "sr_log_box")
    }
    
    if (pdf_dir == "") { add_sr_log("❌ Please enter the output folder path", "error"); return() }
    if (!length(kws))  { add_sr_log("❌ Please enter at least one keyword",   "error"); return() }
    
    pdf_files <- list.files(pdf_dir, pattern="\\.pdf$", full.names=TRUE)
    if (!length(pdf_files)) { add_sr_log("❌ No PDF files found in folder", "error"); return() }
    
    sr_state$status  <- "running"
    sr_state$log     <- ""
    sr_state$total   <- length(pdf_files)
    sr_state$matched <- 0; sr_state$no <- 0; sr_state$err <- 0
    session$sendCustomMessage("setProgSr", 0)
    
    add_sr_log(sprintf("🔍 Searching %d PDFs — Keywords: %s",
                       length(pdf_files), paste(kws, collapse=", ")), "head")
    
    matched_dir     <- file.path(pdf_dir, "matched")
    not_matched_dir <- file.path(pdf_dir, "not_matched")
    dir.create(matched_dir,     recursive=TRUE, showWarnings=FALSE)
    dir.create(not_matched_dir, recursive=TRUE, showWarnings=FALSE)
    
    kw_lower <- tolower(kws)
    
    for (pdf_file in pdf_files) {
      fname   <- basename(pdf_file)
      content <- tryCatch(
        tolower(paste(pdf_text(pdf_file), collapse=" ")),
        error = function(e) NA_character_
      )
      if (is.na(content)) {
        add_sr_log(sprintf("✗ Cannot read: %s", fname), "error")
        sr_state$err <- sr_state$err + 1
      } else {
        hit  <- any(sapply(kw_lower, grepl, x=content, fixed=TRUE))
        dest <- file.path(if (hit) matched_dir else not_matched_dir, fname)
        tryCatch({
          if (!file.rename(pdf_file, dest)) {
            file.copy(pdf_file, dest, overwrite=TRUE); file.remove(pdf_file)
          }
        }, error = function(e) NULL)
        if (hit) {
          sr_state$matched <- sr_state$matched + 1
          add_sr_log(sprintf("✓ HIT → %s", fname))
        } else {
          sr_state$no <- sr_state$no + 1
        }
      }
      done <- sr_state$matched + sr_state$no + sr_state$err
      session$sendCustomMessage("setProgSr", round(done / sr_state$total * 100))
    }
    
    add_sr_log(sprintf("✅ Done — Matched: %d  No match: %d  Errors: %d",
                       sr_state$matched, sr_state$no, sr_state$err), "head")
    sr_state$status <- "done"
  })
  
  output$sr_log     <- renderUI({ HTML(sr_state$log) })
  output$sr_total   <- renderText(sr_state$total)
  output$sr_matched <- renderText(sr_state$matched)
  output$sr_no      <- renderText(sr_state$no)
  output$sr_err     <- renderText(sr_state$err)
  output$sr_pct     <- renderText({
    if (sr_state$total == 0) return("—")
    done <- sr_state$matched + sr_state$no + sr_state$err
    sprintf("%d / %d  (%.0f%%)", done, sr_state$total, done/sr_state$total*100)
  })
}

shinyApp(ui, server)