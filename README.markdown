# Dot Race

*Dot Race* is an in-browser, multi player turn based role playing game. It is
my submission to the [Winter of Code] competition

[Winter of Code]: http://openshift.devconf.cz/


## Technical details

The server side is implemented in *Haskell* using the [Yesod] web framework.
Interestingly, the client side is also written in *Haskell*, and compiled to
JavaScript using the *Fay* compiler.

[Yesod]: http://www.yesodweb.com/
[Fay]: https://github.com/faylang/fay/wiki


## OpenShift

Deploying this application to *OpenShift* is rather tricky. There is a [Haskell
Cloud] cartridge. In theory, this should be enough. In practice, it is not.

[Haskell Cloud]: http://code.accursoft.com/haskell-cloud/

The main issue is that the application with all its dependencies does not fit
into 1 GB limit. It therefore needs additional space.

I also could not manage to build *Fay* on *OpenShift*, so instead opted for a
local build and bundled the compiled version into the source code on
*openshift-build* branch, which is therefore much lighter on dependencies.

While the application is perfectly capable of running on a small gear, building
it there is a problem as the 512 MB limit on memory makes it impossible to link
certain libraries. I built the application on a medium gear and then copied to
a small one. Once it is copied, it is not possible to push updates to it as the
linking would require more memory than is available.


## Misc

There is a caveat with using WebSockets on *OpenShift*: you alway have to
initiate connection to port 8000.


## Known issues

 * the algorithm for detecting if race is finished is a bit shaky
 * player is not warned that leaving the game will abort the race and they will
   not be able to join again
