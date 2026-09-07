# Putting your thesis on GitHub — a step-by-step guide

Written for a first-time git user, on a Mac. Follow it top to bottom. Each phase ends with a **Checkpoint** so you know it worked before moving on.

You only do **Phases 1–5 once**. After that, the day-to-day work is just **Phase 6**, which is three short commands.

---

## First, the mental model (30 seconds)

Three ideas, that's all:

- **git** is a program on your Mac that saves snapshots of your folder over time. Each snapshot is a **commit**.
- **GitHub** is a website that stores a copy of your folder online, so it's backed up and you can reach it from anywhere.
- You **commit** snapshots on your Mac, then **push** them up to GitHub. If you ever work on another computer, you **pull** them down.

That's the whole thing. Everything below is just doing those steps in order.

> **Prefer clicking to typing?** There's a no-terminal alternative using the GitHub Desktop app at the very end (Appendix B). But since you asked for commands, the main guide uses Terminal.

---

## Phase 1 — One-time setup

### Step 1.1 — Create a GitHub account
Go to **https://github.com**, sign up with your `mnominerdene@gmail.com` address, and pick a username (something like `nomin-erdene`). Verify the email they send you.

### Step 1.2 — Open Terminal
Press `Cmd + Space`, type **Terminal**, press Enter. A window with a text prompt opens. This is where you'll paste commands. To run a command: paste it, press Enter.

### Step 1.3 — Install git
Paste this and press Enter:

```bash
xcode-select --install
```

A dialog box pops up — click **Install** and wait (a few minutes). If it says *"already installed,"* that's fine, move on.

**Checkpoint:** run `git --version`. You should see something like `git version 2.39.5`.

### Step 1.4 — Tell git who you are
This just labels your commits with your name. Paste both lines:

```bash
git config --global user.name "Nomin-Erdene"
git config --global user.email "mnominerdene@gmail.com"
```

### Step 1.5 — Install the GitHub helper (`gh`) and sign in
This tool makes signing in painless (no passwords or tokens to fiddle with).

First install **Homebrew**, the standard Mac app installer. Paste this one line and follow its prompts (it will ask for your Mac password — typing shows nothing, that's normal):

```bash
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

When it finishes, it prints a **"Next steps"** box with two `echo` lines to run. **Copy and run those two lines** (they let your Mac find Homebrew). Then install `gh`:

```bash
brew install gh
```

Now sign in to GitHub:

```bash
gh auth login
```

Answer the prompts with the arrow keys + Enter:
- **GitHub.com**
- **HTTPS**
- **Login with a web browser** → it shows a one-time code, press Enter, your browser opens → paste the code → **Authorize**.

**Checkpoint:** run `gh auth status`. It should say *"Logged in to github.com as <your username>."*

You never have to do Phase 1 again on this Mac.

---

## Phase 2 — Organize your thesis folder

A clean layout now saves confusion later. Here's the structure we're aiming for:

```
vat-threshold-mongolia/
├── README.md                     ← what the project is (we make this in Phase 3)
├── .gitignore                    ← list of files git should ignore (Phase 3)
├── paper/
│   ├── main.tex                  ← your manuscript (renamed from "Final_draft (1).tex")
│   ├── references.bib
│   └── Figures/                  ← all the figure PDFs
├── code/
│   └── bunching_enforcement_gap.R
├── data/
│   ├── raw/                      ← MAIN_DATA.xlsx + the NSO CSVs (kept OFF GitHub)
│   └── results/                  ← results_*.csv (aggregated outputs — safe to keep)
└── tables/                       ← the auto-generated *.tex table fragments
```

**Do this in Finder** (easiest), or with the commands below.

1. Make a new folder somewhere sensible, e.g. `Documents/GraSPP/vat-threshold-mongolia`.
2. Inside it, create the folders: `paper`, `paper/Figures`, `code`, `data`, `data/raw`, `data/results`, `tables`.
3. Move your files in:
   - Your thesis `.tex` → `paper/`, and **rename it `main.tex`** (spaces and `(1)` in filenames cause trouble on the command line).
   - `references.bib` → `paper/`
   - all figure PDFs (`fig1_...pdf`, `fig2_...pdf`, etc.) → `paper/Figures/`
   - `bunching_enforcement_gap.R` → `code/`
   - `MAIN_DATA.xlsx` and the two NSO CSVs → `data/raw/`
   - `results_*.csv` → `data/results/`
   - the `*tab_*.tex` table files → `tables/`

> **Why `main.tex`?** Your `\includegraphics` paths already point to `Figures/...`, so keeping a `Figures/` folder right next to the `.tex` means it still compiles unchanged. Just remember to update the file name if your compiler is set to look for the old one.

If you'd rather use commands, `cd` into your new folder first (drag the folder onto the Terminal window to paste its path after typing `cd `), then:

```bash
mkdir -p paper/Figures code data/raw data/results tables
```

...and move files with Finder, which is less error-prone than typing `mv` for each one.

**Checkpoint:** your folder matches the tree above, and `MAIN_DATA.xlsx` is inside `data/raw/`.

---

## Phase 3 — Add the two housekeeping files

These two files are what keep the repo clean and safe.

### Step 3.1 — `.gitignore` (the safety net)
This tells git which files to **never** track — your confidential data and all the messy files LaTeX/R generate. In the **top** folder (`vat-threshold-mongolia/`), create a plain text file named exactly `.gitignore` with this content:

```gitignore
# --- Confidential data: never upload ---
data/raw/
*.xlsx

# --- LaTeX build files (regenerated every compile) ---
*.aux
*.bbl
*.bcf
*.blg
*.fdb_latexmk
*.fls
*.lof
*.log
*.lot
*.out
*.run.xml
*.synctex.gz
*.toc
*.glo
*.gls
*.glg
*.ist
*.acn
*.acr
*.alg

# --- R ---
.Rhistory
.RData
.Rproj.user/

# --- macOS ---
.DS_Store
```

The two lines under *Confidential data* are the important ones: `data/raw/` and `*.xlsx` mean `MAIN_DATA.xlsx` can never be pushed to GitHub, even by accident.

> Easiest way to make a dotfile: in Terminal, `cd` into the top folder, then run `open -e .gitignore`, paste the text, save, close.

### Step 3.2 — `README.md` (the front page)
Create `README.md` in the top folder. This shows up on your repo's home page. A starting point:

```markdown
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
not redistributed in this repository. Aggregated estimation outputs are in `data/results/`.

## Build
Compile `paper/main.tex` with pdflatex + biber (biblatex-chicago).
```

**Checkpoint:** the top folder now contains `.gitignore` and `README.md` alongside your `paper/`, `code/`, `data/`, `tables/` folders.

---

## Phase 4 — Turn the folder into a repository and save your first snapshot

In Terminal, make sure you're inside the top folder (`cd` into it, or drag it onto the window after typing `cd `). Then run these one at a time:

```bash
git init
```
Turns the folder into a git repository.

```bash
git add .
```
Stages every file (except the ones `.gitignore` excludes) for the snapshot.

```bash
git status
```
**Read this output carefully.** You should see your files listed in green. **Confirm `MAIN_DATA.xlsx` and `data/raw/` are NOT listed.** If they appear, stop — the `.gitignore` isn't right; fix it before continuing.

```bash
git commit -m "Initial commit: thesis draft, code, tables, figures"
```
Saves the snapshot. The text in quotes is your note-to-self describing it.

**Checkpoint:** `git log --oneline` shows one line — your first commit.

---

## Phase 5 — Create the GitHub repo and push it online (private)

One command creates the online repo, links it, and uploads everything:

```bash
gh repo create vat-threshold-mongolia --private --source=. --remote=origin --push
```

What the flags mean:
- `--private` → only you can see it (right choice given the data)
- `--source=.` → use this folder
- `--remote=origin` → nickname the online copy "origin" (the standard name)
- `--push` → upload the commit you just made

**Checkpoint:** run `gh repo view --web` — your browser opens your repository on GitHub, showing your README. It's now backed up online. 🎉

---

## Phase 6 — The everyday loop (this is all you repeat)

Every time you finish a chunk of work — say you edit a section or fix a table — do these three commands from the top folder:

```bash
git add -A
git commit -m "Describe what you changed"
git push
```

- `git add -A` → gather all your changes
- `git commit -m "..."` → save a snapshot with a short description
- `git push` → send it to GitHub

Good commit messages are short and specific. For your current work:
- `git commit -m "Add exogeneity subsection for APPS revision"`
- `git commit -m "Add revenue section and illustrative-revenue appendix"`
- `git commit -m "Fix inflation figure: inflation-indexed threshold ~37M not 16.3M"`

That's the whole workflow. Commit whenever you finish something you'd be annoyed to lose.

---

## A few habits worth keeping

- **Commit often, in meaningful chunks.** One commit per idea ("added X", "fixed Y") beats one giant commit at the end of the day. It makes your history readable and lets you undo one change without losing others.
- **Write the commit message in plain words** — future you is the reader.
- **Never force anything you don't understand.** If a command mentions `--force` or `reset --hard`, pause and ask.
- **See your history:** `git log --oneline`.
- **The data stays out.** As long as `.gitignore` has `data/raw/` and `*.xlsx`, your confidential file is safe. Double-check with `git status` before pushing if you're ever unsure.

---

## Quick reference card

| I want to... | Command |
|---|---|
| Save + upload my latest work | `git add -A` then `git commit -m "note"` then `git push` |
| See what's changed | `git status` |
| See my history | `git log --oneline` |
| Open the repo on GitHub | `gh repo view --web` |
| Get changes from another computer | `git pull` |

---

## Appendix A — If something goes wrong

- **"command not found: git"** → Phase 1.3 didn't finish. Re-run `xcode-select --install`.
- **"command not found: brew"** → run the two `echo` lines Homebrew printed at the end of its install, then close and reopen Terminal.
- **`git push` asks for a username/password** → you skipped `gh auth login` (Phase 1.5). Run it, then push again.
- **You accidentally committed the data file** → don't push. Run `git rm --cached data/raw/MAIN_DATA.xlsx`, confirm `.gitignore` covers it, then commit again. If you already pushed it, tell me and I'll walk you through removing it from history.

## Appendix B — The no-terminal alternative (GitHub Desktop)

If the terminal ever feels like too much:

1. Download **GitHub Desktop** from https://desktop.github.com and sign in with your GitHub account.
2. **File → Add Local Repository**, choose your `vat-threshold-mongolia` folder (do Phases 2–3 first so it's organized and has a `.gitignore`).
3. It shows your changed files. Type a summary at the bottom left, click **Commit to main**.
4. Click **Publish repository** — tick **Keep this code private** — then **Publish**.
5. From then on: make changes → type a summary → **Commit to main** → **Push origin**. Same three ideas, with buttons instead of commands.
