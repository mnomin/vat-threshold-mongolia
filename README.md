# Digital Enforcement and Tax Frictions — VAT Threshold in Mongolia

Master's thesis (GraSPP, University of Tokyo) and working paper prepared for
submission to *Asia & the Pacific Policy Studies*.

## Structure
- `paper/`   — LaTeX manuscript (`main.tex`), bibliography, figures
- `code/`    — R estimation code (`bunching_enforcement_gap.R`)
- `data/`    — `raw/` (confidential MTA data, not tracked) and `results/` (aggregated outputs)
- `tables/`  — auto-generated LaTeX table fragments

## Data availability
The binned administrative data are provided by the Mongolian Tax Authority and are
not redistributed here. Aggregated estimation outputs are in `data/results/`.

## Build
Compile `paper/main.tex` with pdflatex + biber (biblatex-chicago).
