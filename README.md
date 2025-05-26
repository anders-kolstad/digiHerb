# digiHerb
Anders' digitale herbarium

## Repo strucure

`docs`: all files needed to depoy the dashboard on GitHub pages
`_site`: a gitignored folder with the output from locally rendering the dashboard. Content is deleted for every new render.
`docs/img`: a folder with all the images. 

## Workflow for updating the dashboard
- Download the photos from pCloud
- Update `docs/img` on main by first deleting all files and pasting inn the new, with no git commits in between. Git will not treat the same files as new unless they have been modified.
- Re-run `familienavn.R` to update * slektOGfam.RData`
- Render ìndex.qmd`
- Move files from `_site` to replace those in `docs`
- Test html and push to github. Github deploys from `docs`.
