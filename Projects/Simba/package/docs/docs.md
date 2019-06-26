# Simba Packages

Simba packages is a system based around [Github releases](https://help.github.com/en/articles/creating-releases) to easily share Simba projects.

![img](form.png?raw=true "Package Form")

It's pretty simple. It downloads the zipball of a release and extracts. The install options can filled in by default if the release contains a `.simbapackage` file in the root of the repository.

Example:
```
  // The package name to install under. Can be empty to install directly into the directory
  name=SRL
  // The directory to install into from simba's executable location.
  directory=Includes
  // flat extraction.
  flat=false
  // Files or directories not to install delimited with commas.
  ignore=.gitignore,.simbapackage
```
## Updating

The package system does not automatically update installed packages to the newest version. This is by design since on Windows you can't overwrite a loaded file (think plugins). It also helps for safety and simplicity.

However, updates are checked for hourly and will change the appearance and hint of the package button when updates are available.

![img](updates.png?raw=true "Package Update")


## Installing the master branch
If the repository has no releases, or you want to install the master branch (most likely the development branch) this is also possible. It's added last in the version list. Simba will not notify you of updates if the master branch is installed.

![img](master.png?raw=true "Master Branch")

## Miscellaneous

* Each component has a mouse-over hint for detailed infomation.
* Right click on the package list for quick access to a packages Github page or to submit a Github issue.
* Github API has a limit of 60 requests per hour for unauthorised users so all requests are cached for an hour. "Check for updates" can be clicked which will force update the cache. 