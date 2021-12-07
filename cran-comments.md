
## Update to version 0.3.3

I have reproduced the rcnst issue locally and fixed the problematic tests.
I also looked through the main package's code to see if the problem could arise there as well,
but didn't find any issues.

## Test environments
* Local GNU/Linux, R release
* Local Windows, R release
* win-builder (devel and release)
* GitHub Actions (Windows, OSX, Linux)

## R CMD check results
* There were no ERRORs, WARNINGs or NOTEs
