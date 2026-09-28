# TODO on the main machine: make the repo runnable on a fresh clone

Written 2026-09-28 from the laptop (repos in `C:\GitHubRepos`). Delete this
file, and the pointer in `CLAUDE.md`, once everything below is done.

## Why

`R/01_assemble.R` builds `data/` from four sibling repos. The `E:/...` paths
have been changed to sibling-relative paths.

Code in these repos finds other repos as **sibling folders**
(`dirname(here::here())`), not through `E:/...` paths. On the main machine
they sit side by side in `E:\`, and on the laptop in `C:\GitHubRepos`.
`bh-school-system` can be named either `bh-school-system` or
`bh_school_system`.

The simulator and decks already work on the laptop, because the assembled
`data/` and `app/data/` are committed. Re-running `01_assemble.R` on a fresh
machine needs git-ignored data from:

- `BH_Schools_2/data`: see that repo's `MACHINE_SYNC_TODO.md`
- `BH_Schools_Consultation/data`: see that repo's `MACHINE_SYNC_TODO.md`
- `school_attainment_tool`: see that repo's `MACHINE_SYNC_TODO.md`
- `BH_Pupil_Destinations/public/output`: committed, so it's fine

## Steps

### 1. Check `R/01_assemble.R` still finds everything on this machine

On the main machine this repo's folder is `E:\bh_school_system`, and the
others are `E:\<RepoName>`, so the new paths should resolve.

### 2. Do the sibling TODOs above

### Check it

Pull on the laptop and run `R/01_assemble.R`.
