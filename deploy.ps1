<#
.SYNOPSIS
  Deploy this repo's Shiny app (app/) to the self-hosted Shiny Server on the NAS.

.DESCRIPTION
  The apps at https://apps.adamdennett.co.uk/<app>/ are served by one
  rocker/shiny Docker container on the NAS. It serves every folder in the NAS's
  web/apps/ directory (mounted read-only), and a Cloudflare tunnel publishes it.
  The NAS share holding web/ is mapped on this PC over Tailscale (N:\web by
  default; set $env:SHINY_NAS_ROOT to override). Pushing to GitHub does NOT
  update the app; this script does.

  Steps:
    1. Checks app/app.R exists and the NAS share is reachable.
    2. Checks every package the app's R code loads is installed in the
       container image (listed in web/Dockerfile, or base/recommended/
       preinstalled). With -AddPackages it appends missing ones to the
       Dockerfile; the image then has to be rebuilt on the NAS before deploying.
    3. Mirrors app/ to web/apps/<app>/ with robocopy (deletes files removed
       here; skips rsconnect/, tests/, manifest.json and R session files).
    4. Writes restart.txt (timestamp + git commit) so Shiny Server restarts
       the app on its next visit.
    5. Requests the live URL and fails if the app does not start.

.EXAMPLE
  .\deploy.ps1            # deploy
  .\deploy.ps1 -DryRun    # show what would be copied, change nothing
  .\deploy.ps1 -AddPackages   # add missing packages to the NAS Dockerfile
#>
param(
  [string]$AppName = 'bh_school_system',
  [string]$NasRoot = '',
  [string]$BaseUrl = 'https://apps.adamdennett.co.uk',
  [switch]$DryRun,
  [switch]$AddPackages,
  [switch]$SkipPackageCheck,
  [switch]$SkipUrlCheck
)

$ErrorActionPreference = 'Stop'

if (-not $NasRoot) {
  if ($env:SHINY_NAS_ROOT) { $NasRoot = $env:SHINY_NAS_ROOT } else { $NasRoot = 'N:\web' }
}

$src        = Join-Path $PSScriptRoot 'app'
$dest       = Join-Path (Join-Path $NasRoot 'apps') $AppName
$dockerfile = Join-Path $NasRoot 'Dockerfile'
$appUrl     = "$BaseUrl/$AppName/"

function Write-Step($msg) { Write-Host "==> $msg" -ForegroundColor Cyan }

# 1. Preconditions ------------------------------------------------------------
Write-Step "Deploying $src -> $dest"
if (-not (Test-Path (Join-Path $src 'app.R'))) {
  throw "No app/app.R under $PSScriptRoot. Run this from the repo that holds the app."
}
if (-not (Test-Path $dockerfile)) {
  throw "Cannot reach $dockerfile. Is Tailscale connected and the NAS share mapped (expected $NasRoot)?"
}

# 2. Package check ------------------------------------------------------------
if (-not $SkipPackageCheck) {
  Write-Step 'Checking R packages against the NAS image'

  # Packages the app's code loads: library(x), require(x), requireNamespace("x"), x::
  $used = New-Object System.Collections.Generic.HashSet[string]
  Get-ChildItem $src -Recurse -Filter *.R | ForEach-Object {
    foreach ($line in (Get-Content $_.FullName)) {
      $code = ($line -replace '#.*$', '')   # drop comments (naive, fine for these apps)
      foreach ($m in [regex]::Matches($code, '\b(?:library|require|requireNamespace)\(\s*["'']?([A-Za-z][A-Za-z0-9.]*)')) {
        [void]$used.Add($m.Groups[1].Value)
      }
      foreach ($m in [regex]::Matches($code, '\b([A-Za-z][A-Za-z0-9.]*):::?')) {
        [void]$used.Add($m.Groups[1].Value)
      }
    }
  }

  # Packages the image has: everything after install2.r in the Dockerfile,
  # plus base, recommended and those rocker/shiny ships with.
  $dfText = Get-Content $dockerfile -Raw
  $listed = @()
  foreach ($m in [regex]::Matches($dfText, 'install2\.r((?:[^\r\n]*\\\r?\n)*[^\r\n]*)')) {
    $listed += ($m.Groups[1].Value -split '[\s\\]+') | Where-Object { $_ -and $_ -notmatch '^-' }
  }
  $builtin = @(
    'base','compiler','datasets','graphics','grDevices','grid','methods','parallel',
    'splines','stats','stats4','tcltk','tools','utils',
    'boot','class','cluster','codetools','foreign','KernSmooth','lattice','MASS',
    'Matrix','mgcv','nlme','nnet','rpart','spatial','survival',
    'shiny','rmarkdown','htmltools','httpuv','jsonlite','promises','later',
    'remotes','littler','docopt'
  )
  $have = @{}
  foreach ($p in ($listed + $builtin)) { $have[$p] = $true }

  $missing = @($used | Where-Object { -not $have.ContainsKey($_) } | Sort-Object)
  if ($missing.Count -gt 0) {
    if ($AddPackages -and -not $DryRun) {
      $re = New-Object System.Text.RegularExpressions.Regex '(install2\.r(?:[^\r\n]*\\\r?\n)*[^\r\n]*?)[ \t]*(\r?\n)'
      $new = $re.Replace($dfText, { param($m) $m.Groups[1].Value + ' ' + ($missing -join ' ') + $m.Groups[2].Value }, 1)
      [System.IO.File]::WriteAllText($dockerfile, $new)
      Write-Host "Added to ${dockerfile}: $($missing -join ', ')" -ForegroundColor Yellow
    }
    $msg = "The app uses packages not in the NAS image: $($missing -join ', ').`n" +
           "Add them to the install2.r list in $dockerfile (or rerun with -AddPackages),`n" +
           "rebuild the 'shiny' service on the NAS (in web/: docker compose up -d --build),`n" +
           "then run this script again. Use -SkipPackageCheck if a name is a false positive."
    if ($DryRun) { Write-Warning $msg } else { throw $msg }
  } else {
    Write-Host "All $($used.Count) packages are in the image."
  }
}

# 3. Copy ---------------------------------------------------------------------
Write-Step $(if ($DryRun) { 'Dry run: files that would change' } else { 'Copying app files' })
if (-not (Test-Path $dest) -and -not $DryRun) { New-Item -ItemType Directory -Path $dest | Out-Null }
$rcArgs = @($src, $dest, '/MIR', '/XD', 'rsconnect', 'tests', '.Rproj.user',
            '/XF', 'manifest.json', '.Rhistory', '.RData', 'restart.txt',
            '/R:2', '/W:2', '/NP', '/NDL', '/NJH')
if ($DryRun) { $rcArgs += '/L' }
& robocopy @rcArgs
if ($LASTEXITCODE -ge 8) { throw "robocopy failed with exit code $LASTEXITCODE" }
if ($DryRun) { Write-Step 'Dry run finished; nothing changed.'; exit 0 }

# 4. Restart ------------------------------------------------------------------
$commit = ''
try {
  $commit = (& git -C $PSScriptRoot rev-parse --short HEAD 2>$null)
  if (& git -C $PSScriptRoot status --porcelain -- app 2>$null) { $commit = "$commit (+ uncommitted changes in app/)" }
} catch {}
$stamp = "deployed $(Get-Date -Format o) from commit $commit"
[System.IO.File]::WriteAllText((Join-Path $dest 'restart.txt'), $stamp + "`n")
Write-Step "Wrote restart.txt: $stamp"

# 5. Check it starts ----------------------------------------------------------
if (-not $SkipUrlCheck) {
  Write-Step "Checking $appUrl"
  Start-Sleep -Seconds 2
  try {
    $r = Invoke-WebRequest -Uri $appUrl -UseBasicParsing -TimeoutSec 180
    if ($r.Content -match 'An error has occurred|application failed to start') {
      throw 'Shiny Server says the app failed to start.'
    }
    Write-Host "OK: $appUrl returned $($r.StatusCode)." -ForegroundColor Green
  } catch {
    throw ("The app did not load at ${appUrl}: $($_.Exception.Message)`n" +
           "Usually a missing R package or data file. Run the app locally with " +
           "shiny::runApp('app') and check the app log in the container on the NAS`n" +
           "(docker exec shiny ls -t /var/log/shiny-server, then cat the newest $AppName log).")
  }
}
