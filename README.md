# digiHerb
Anders' digitale herbarium

## Repo strucure

`docs`: all files needed to depoy the dashboard on GitHub pages, including `docs/img`
`_site`: a gitignored folder with the output from locally rendering the dashboard. Content is deleted for every new render.
`img`: a gitignored folder with all the images. 

## Workflow for updating the dashboard
- Download the photos from pCloud
- Update `img` locally by first deleting all files and pasting inn the new. This folder is ignored by git.
- Compress the images by opening a terminal from the root folder and running `magick mogrify -resize 1200x -quality 80 *.jpg`. Deploying with original resolution couses memory issues on the runners.
- Delete and replace the pictures in `docs/img`. Git will not treat the same files as new unless they have been modified, but due to re-compressing, it probably will treat them as new files.
- Re-run `familienavn.R` to update * slektOGfam.RData`
- Render ìndex.qmd` (output to `_site`)
- Copy `img` over to `_site`
- Open `_site/index.html` in editor (RStudio) and then preview. Check that everythink is OK.
- Move files from `_site` to replace those in `docs`
- Push to github. Github deploys from `docs`.
