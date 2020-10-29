
.onLoad <- function(libname, pkgname) {
   if (requireNamespace("debugme", quietly = TRUE)) {
     debugme::debugme()
   }

   run_on_load()
}
