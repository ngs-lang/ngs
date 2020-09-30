# Release procedure

This document describes releasing a new version of NGS.

* Build and run tests
* `make update-vim-syntax`
* `git commit -am 'Update vim/syntax/ngs.vim'`
* `git push`
* Review the changes between `dev` and `master`
* Update `CHANGELOG.md`. Place the release date in the section on the top.
* Commit
* Push
* Wait and see that Github Actions build is OK
* Checkout `master` branch
* `git pull`
* Review the changes made in `master` branch if any. There should not be.
* `git merge --no-commit dev`
* Update `version.h`
	* Remove the `-alpha` or whatever pre-release mark.
	* `git add version.h`
* Update `snap/snapcraft.yaml`
	* Check if `version` is correct
	* Change `grade` to `stable`
* Review the changes, special attention to `readme.md` as they differ a bit.
* `git commit`
* Build and run tests
* Build documentation
	* `(cd doc && ./make.ngs out)`
* `git push`
* Wait and see that Github Actions build is OK
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
	* run `snapcraft` in the root folder (output will be a file like `ngs_VERSION_amd64.snap`)
	* run `snapcraft login` and login with the snapcraft credentials
	* release snap in the stable channel `snapcraft upload --release=stable ngs_VERSION_amd64.snap`
	* check if `https://snapcraft.io/ngs` has the release
	* delete `ngs_VERSION_amd64.snap` from root folder
* Checkout `dev`
* Increase version in `version.h`
* Increase version in `snap/snapcraft.yaml` and make sure `grade` is `devel`  (for releasing developer versions, use `snapcraft upload --release=beta ngs_VERSION_amd64.snap`)
* Make a new entry at the top of `CHANGELOG.md`
* Build (to get the new version from `version.h`)
