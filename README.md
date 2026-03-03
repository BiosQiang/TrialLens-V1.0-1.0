# 🔬 TrialLens 临镜 `v1.0`

> **Batch-download and keyword-screen Protocol / SAP documents from ClinicalTrials.gov**  
> 批量下载并关键词筛选 ClinicalTrials.gov 临床试验 Protocol / SAP 文件

---

## What is TrialLens?

TrialLens is a local R Shiny application designed for researchers and statisticians who need to efficiently retrieve and screen clinical trial documents from [ClinicalTrials.gov](https://clinicaltrials.gov).

Given a trial list exported from ClinicalTrials.gov as a CSV file, TrialLens can:

- **Batch-download** Protocol and/or Statistical Analysis Plan (SAP) PDFs for hundreds or thousands of trials
- **Stop and resume** at any time — already-downloaded files are automatically skipped on re-run
- **Keyword-screen** downloaded PDFs and sort them into per-keyword subfolders under `matched/`
- Display **real-time progress** with a live dashboard (counts, progress bar, live log)

---

## Screenshots

> *(Add screenshots here)*

---

## Requirements

- R ≥ 4.1
- The following R packages:

```r
install.packages(c("shiny", "pdftools", "httr", "future", "promises"))
```

---

## Installation & Usage

TrialLens runs **locally on your own machine** — no server or internet hosting required. File paths refer to your local filesystem.

### 1. Clone the repository

```bash
git clone https://github.com/YOUR_USERNAME/TrialLens.git
cd TrialLens
```

### 2. Install dependencies

```r
install.packages(c("shiny", "pdftools", "httr", "future", "promises"))
```

### 3. Run the app

```r
shiny::runApp("app.R")
```

---

## How to use

### Step 1 — Export your trial list

Go to [clinicaltrials.gov](https://clinicaltrials.gov), run your search, then click **Download → CSV**. The exported file must contain the columns `NCT Number` and `Study Documents`.

### Step 2 — Upload CSV and configure

In the **Tool** tab:

| Field | Description |
|---|---|
| ① Trial List CSV | Upload the CSV exported from ClinicalTrials.gov |
| ② PDF Output Folder | Local folder path where PDFs will be saved (e.g. `F:\research\pdfs`) |
| ③ Document Type | Choose **Protocol**, **SAP**, or **Both** |

### Step 3 — Download

Click **▶ Start Download**. The dashboard updates in real time showing downloaded / skipped / failed counts and a progress bar.

Click **■ Stop** at any point to interrupt — progress is fully preserved. Re-running will skip files already on disk.

### Step 4 — Keyword search

Enter one or more keywords (comma-separated) in the **④ Keyword Search** section and click **🔍 Search**.

- Matching is **case-insensitive** — `RPSFT`, `rpsft`, and `Rpsft` are treated identically
- A PDF is a hit if **any** keyword is found anywhere in the document
- Each matched PDF is moved into `matched/<keyword>/`, where `<keyword>` is the **first** keyword it matches (priority follows the order you entered)
- Non-matched PDFs are moved to `not_matched/`

---

## Output folder structure

```
PDF Output Folder/
├── NCT01714739.pdf          ← downloaded PDFs (named by NCT number)
├── NCT01234567.pdf
├── matched/
│   ├── RPSFT/               ← PDFs where "RPSFT" was the first keyword matched
│   │   ├── NCT01714739.pdf
│   │   └── NCT02234567.pdf
│   └── rank preserving/     ← PDFs where "rank preserving" was the first keyword matched
│       └── NCT03456789.pdf
└── not_matched/             ← PDFs with no keyword match
    └── NCT09999999.pdf
```

When `Both` is selected, files are named `NCTxxxxxxxx_protocol.pdf` and `NCTxxxxxxxx_sap.pdf`.

---

## Notes

- **Path format**: Both `F:\test\output` and `F:/test/output` are accepted — backslashes are converted automatically.
- **Keyword priority**: If a PDF matches more than one keyword, it is filed under the first matching keyword in the order you entered. Arrange your keywords accordingly.
- **Encrypted PDFs**: Some PDFs on ClinicalTrials.gov are password-protected and cannot be read for keyword matching. These are reported as errors in the log and do not affect other files.
- **Local use only**: TrialLens reads and writes to your local filesystem. It is designed to be run locally via `shiny::runApp()`, not deployed to a remote server.

---

## License

This project is released for **academic and research use only**.  
© 2025 Dr Zhang. All rights reserved. Unauthorized reproduction or redistribution is prohibited.

---

## Citation

If you use TrialLens in your research, please consider citing it as:

```
Zhang. (2025). TrialLens 临镜 (v1.0): A Shiny application for batch downloading 
and keyword screening of clinical trial documents from ClinicalTrials.gov. 
GitHub. https://github.com/YOUR_USERNAME/TrialLens
```
