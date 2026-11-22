# bh-school-system

**Outstanding:** read `MACHINE_SYNC_TODO.md` first. There are setup fixes to
make on the main machine (where the git-ignored data lives) so the repo runs
from a fresh clone. Mention it to the user at the start of a session until
it's done.

- Never use absolute drive paths like `E:/...`. Use `here::here()` for this
  repo and `dirname(here::here())` for sibling repos.

## Deploying the Shiny app (`app/`)

- The live app is <https://apps.adamdennett.co.uk/bh_school_system/>, served by the
  self-hosted Shiny Server: one `rocker/shiny` Docker container on the NAS
  that serves `web/apps/<name>/`, with a Cloudflare tunnel in front. Pushing
  to GitHub does not update it. Posit Connect Cloud and shinyapps.io are no
  longer used: `manifest.json` and `rsconnect/` are kept only as a fallback,
  so never deploy there.
- To deploy, run `powershell -ExecutionPolicy Bypass -File .\deploy.ps1 -DryRun`
  from the repo root, then the same without `-DryRun`. The real run updates
  the live app, so do it when the user asks to deploy, publish or update the
  app. After committing changes to `app/`, offer it. The script checks
  packages, mirrors `app/` (including `app/data/`) to the NAS, writes
  `restart.txt` and checks the live URL. Report its result.
- The NAS share is mapped over Tailscale as `N:` (root `N:\web`, override with
  `SHINY_NAS_ROOT`). If the script can't reach it, ask the user to connect
  Tailscale or map the drive. Don't work around it.
- If the script says a package is missing from the image, rerun with
  `-AddPackages`. That appends it to `N:\web\Dockerfile`. Then ask the user to
  rebuild the `shiny` service on the NAS (`docker compose up -d --build` in
  `web/`, or in the NAS's Docker manager); Claude can't run Docker on the NAS.
  Deploy again afterwards. Watch for this whenever app code starts using a
  new package.
- If the app fails to start after a deploy, run it locally
  (`shiny::runApp("app")`) to find the error. The server log is in the
  container (`docker exec shiny ls -t /var/log/shiny-server`); ask the user
  for it if needed.
- `deploy.ps1` is identical in school_value_added, school_attainment_tool and
  bh_school_system apart from the app name. Change all three together.
