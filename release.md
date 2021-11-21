# Release procedure

This document describes releasing a new version of NGS.

* Build and run tests
	* `rm -rf build`
	* `make build && make tests`
* `make update-vim-syntax`
* `git commit -am 'Update vim/syntax/ngs.vim'`
* `git push`
* Review the changes between `dev` and `master`
* Update `CHANGELOG.md`. Place the release date in the section on the top.
* `git commit -am 'Preparing for release'`
* `git push`
* Wait and see that GitHub Actions build is OK - https://github.com/ngs-lang/ngs/actions
* `git checkout master`
* `git pull`
* Review the changes made in `master` branch if any. There should not be.
* `git merge --no-commit dev`
* Update `version.h`
	* Remove the `-alpha` or whatever pre-release mark.
	* `git add version.h`
* Update `snap/snapcraft.yaml`
	*  Remove the `-alpha` or whatever pre-release mark from the `version` field
	* `git add snap/snapcraft.yaml`
* Review the changes, special attention to `readme.md` as they differ a bit.
* `git commit`
* Build and run tests
	* `rm -rf build`
	* `make build && make tests`
* Documentation
	* Build: `(cd doc && ./make.ngs out)`
	* Take a look at the generated documentaion
* `git push`
* Wait and see that GitHub Actions build is OK - https://github.com/ngs-lang/ngs/actions
* Update doc folder so that `latest` link points to the last version: `(cd doc/out && rm latest && ln -s $(ngs -p VERSION) latest)`
* Sync documentation (to be scripted): `aws s3 sync --acl public-read doc/out/ s3://01.ngs-lang.org/doc/`
* `(cd site && ./update.ngs)` (must be after syncing documentation because it invalidates caches)
* Tag the release: `git tag v$(ngs -p VERSION)`
* Push the tag: `git push origin v$(ngs -p VERSION)`
* Create release on GitHub: https://github.com/ngs-lang/ngs/releases/new
	* select the newly created version for both tag and title
	* For description, take most important things from `CHANGELOG.md` and generalize them
	* At the end of description add: `` Detailed list of changes is in `CHANGELOG.md` ``
* Release Snap
	* access https://snapcraft.io/ngs/builds, if the build was not already started (triggered automatically on git push) click the `Trigger new build` button
	* wait for the build for all arquitectures to finish, then go to https://snapcraft.io/ngs/releases, the build should appear released in the `latest/edge` channel, click in the cog and promote it to the desired channel, or just drag it into the wanted channel. Then click `Save` button on top of the page
* Checkout `dev`
* Increase version in `version.h`
* Increase version in `snap/snapcraft.yaml`
* Make a new entry at the top of `CHANGELOG.md`
* Build (to get the new version from `version.h`)
	* `rm -rf build`
	* `make build`
* `git commit -am 'Organizing after release'`
