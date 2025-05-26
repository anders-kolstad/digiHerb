# digiHerb
Anders' digitale herbarium

## Repo strucure

`docs`: all files needed to depoy the dashboard on GitHub pages
`_site`: a gitignored folder with the output from locally rendering the dashboard. Content is deleted for every new render.
`docs/img`: a folder with all the images. 

## Workflow for updating the dashboard
- Download the photos from pCloud
- Update `docs/img` on main. Remember to check if you modified some images, but kept the same file name, that you actually repace it. Best practice is to just replace everything. I don't think that will trigger git to treat all identical files as new, but not sure.
- Re-run `familienavn.R` to update * slektOGfam.RData`
- Render ìndex.qmd`
- Move files from `_site` to replace those in `docs`
- Test html and push to github. Github deploys from `docs`.
