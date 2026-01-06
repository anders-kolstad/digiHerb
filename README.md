# digiHerb
Anders' digitale herbarium

## Repo strucure

`docs`: all files needed to depoy the dashboard on GitHub pages, including `docs/img`
`_site`: a gitignored folder with the output from locally rendering the dashboard. Content is deleted for every new render.
`img`: a gitignored folder with all the images. 

## Workflow for updating the dashboard
- Clear the img/ folder on root, if not done already.
- Download the photos from pCloud (from the `1 - prep/klar`-folder)  and into `img`. A good idea is to keep new or edited pictures in a seperate folder (e.g. `1 - prep`) and then just download that one. 
- Compress the images by opening a terminal from img/ and running `magick mogrify -resize 1200x -quality 80 *.jpg`. Deploying with original resolution couses memory issues on the runners.
- Move the the new and newly compressed pictures to `docs/img`. Clear img/.
- Re-run `familienavn.R` to update * slektOGfam.RData`.
- In this proces, also update names in dics/img/ and on pCloud if needed.
- Copy `docs/img` folder over too img/ so that you can render the qmd correctly.
- Render `index.qmd` (output to `_site/`)
- Copy `img` over to `_site`
- Open `_site/index.html` in editor (RStudio) and then preview. Check that everythink is OK.
- Move files from `_site` to replace those in `docs` (except img/ which is already there). Don't delete the file `.nojekyll`.
- Push to github. Github deploys from `docs`.
- Clear `_site` and `img` to save local disc space.
