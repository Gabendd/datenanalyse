# Plan: Fix LaTeX Compilation Error for PDF Export

## Problem Summary
Line 333 in your script attempts to export a huxtable regression table to PDF using `quick_pdf()`, but LaTeX compilation fails. The error stack trace shows the LaTeX compiler cannot successfully build the `.tex` file.

## Root Cause Analysis

**Located in:** `Scripts/Datenanalyse script.R:333`
```r
huxtable::quick_pdf(
  swiss_regression_table,
  file = "tables/swiss_regression_results.pdf",
  open = FALSE
)
```

**Most likely causes:**
1. **German special characters** – Column names like "Angep. R-Quadrat" (with the period) or other umlauts may need escaping in LaTeX
2. **Missing LaTeX packages** – TinyTeX lacks required packages for complex table formatting
3. **Unescaped special characters** – Underscores, ampersands, or other LaTeX-special characters in labels
4. **TinyTeX installation incomplete** – Missing core packages or corrupted state

## Investigation & Solution Plan

### Step 1: Diagnose the actual LaTeX error
- Examine the raw generated `.tex` file before it compiles
- Look for unescaped special characters (especially in German labels)
- Check LaTeX preamble for required packages

### Step 2: Implement fixes (in priority order)

**Option A (Recommended first):** Export to HTML instead 
- Change `quick_pdf()` to `quick_xlsx()` or `quick_html()`
- HTML/Excel exports don't require LaTeX and avoid this entire issue
- Already feasible given existing code structure

**Option B:** Fix German characters in labels
- Review labels in the `make_regression_table()` function (lines 308–313)
- Escape special characters if needed: `\\_`, `\\&`, etc.
- Ensure UTF-8 encoding throughout

**Option C:** Repair TinyTeX
- Run `tinytex::reinstall_tinytex()` to reset the installation
- Ensure all core packages are installed

**Option D:** Manually export as CSV/Excel
- Skip `quick_pdf()` entirely
- Use `huxtable::write_csv()` or `huxtable::write_xlsx()`

## Recommended Next Steps

1. **Try HTML export first** – simplest, no LaTeX dependency
2. **If PDF required**, identify and escape problematic characters
3. **Fallback:** Reinstall TinyTeX if character encoding isn't the issue

## Files to Modify
- `Scripts/Datenanalyse script.R` 
  - Line 333: Change export format or add character escaping
  - Lines 299–322: Potentially simplify/escape table labels

## Expected Outcome
- Successfully export regression table in a standard format (PDF, HTML, or Excel)
- German labels display correctly without compilation errors
- No impact on upstream analysis

---

**Ready to proceed?** Approve this plan and we'll implement the fix.
