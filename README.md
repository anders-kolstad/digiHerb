# digiHerb
Anders' digitale herbarium

## Repo strucure

`docs`: all files needed to depoy the dashboard on GitHub pages, including `docs/img`
`_site`: a gitignored folder with the output from locally rendering the dashboard. Content is deleted for every new render.
`img`: a gitignored folder with all the images. 

## Workflow for updating the dashboard
- Download the photos from pCloud and into `img` (clear that folder first). A good idea is to keep new or edited pictures in a seperate folder (e.g. `1 - prep`) and then just download that one. 
- Compress the images by opening a terminal from the root folder and running `magick mogrify -resize 1200x -quality 80 *.jpg`. Deploying with original resolution couses memory issues on the runners.
- Add the new pictures to `docs/img`.
- Copy the complie `docs/img` folder over too root so that you can render the qmd correctly.
- Re-run `familienavn.R` to update * slektOGfam.RData`
- Render ìndex.qmd` (output to `_site`)
- Copy `img` over to `_site`
- Open `_site/index.html` in editor (RStudio) and then preview. Check that everythink is OK.
- Move files from `_site` to replace those in `docs`
- Push to github. Github deploys from `docs`.
- Clear `_site` and `img` to save local disc space.
