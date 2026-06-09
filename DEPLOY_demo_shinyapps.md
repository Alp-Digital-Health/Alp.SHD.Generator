# Deploying the public demo to shinyapps.io

This guide gets `app.R` (the demo build) live on the free tier.

## 1. One edit before you deploy
Open `app.R` and set the download link near the top:

```r
DOWNLOAD_URL <- "https://github.com/YOUR-USERNAME/SyntheticHealthDataGenerator/releases/latest"
```

Point it at wherever the local/R-Portable version lives (e.g. your GitHub
Release). This link appears in the warning modal, the banner, the sidebar
button, and the About page.

## 2. Folder layout
Put the file in its own folder, named exactly `app.R`:

```
synthetic-demo/
└── app.R
```

Nothing else is required. The built-in synthetic data is generated inside the
script, so there are no data files to upload (and no real data can reach the
server).

## 3. One-time setup (in R / RStudio)
```r
install.packages("rsconnect")
```
Create a free account at https://www.shinyapps.io, then on the site go to
**Account → Tokens → Show**, copy the `setAccountInfo(...)` line, and run it
once in R. This links your machine to your account.

## 4. Deploy
```r
rsconnect::deployApp("path/to/synthetic-demo", appName = "synthetic-health-demo")
```
First deploy takes several minutes while shinyapps.io compiles the dependencies
(`synthpop`, `bnlearn`, `randomForest`, etc.). When it finishes you get a public
URL like `https://your-account.shinyapps.io/synthetic-health-demo/`.

## 5. Conserve free active hours
The free tier allows 5 apps and 25 active hours/month. To stretch them, in the
shinyapps.io dashboard open the app's **Settings → General**:

- Set **Idle timeout** to a low value (e.g. 5 minutes) so the app releases hours
  quickly after a visitor leaves. (Default is 15 minutes.)
- Keep **Instance size** at the default (free tier is 1 GB RAM, which is fine
  for the built-in demo data).
- Leave **Max worker processes / connections** at defaults for a demo.

If the app ever exceeds 25 active hours in a month, visitors see a polite
"unavailable, try later" notice until the next month — the app is not deleted.

## 6. After it's live
- Link the demo URL from your GitHub README and documentation page, labelled
  clearly as a **demo on sample data**.
- The app already warns users on load and pushes them to download for their own
  data — but reinforce that framing wherever you share the link.

## Notes
- The demo caps synthetic records at 10,000 (`DEMO_MAX_SYNTH` in `app.R`) to keep
  the shared free instance stable. Raise or lower it there if you wish.
- This build has **no file upload by design** — it is the privacy guarantee.
  Keep uploads only in the downloadable local version.
- To update the live app later, just edit `app.R` and re-run the same
  `deployApp(...)` call.
