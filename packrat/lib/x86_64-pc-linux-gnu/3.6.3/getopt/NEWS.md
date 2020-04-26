getopt 1.20.3
=============
* Now by default ``getopt`` won't override a user specified ``opt`` argument if ``argv`` is in the global environment.
  Will continue to use ``argv`` as a default for ``opt`` if it is in the global environment and the user does not specify an ``opt`` argument (for ``littler`` compatibility).

getopt 1.20.2
=============
* Now allows one to pass an empty string to a character option.
  Thanks Matthew Flickinger for bug report.

getopt 1.20.1
=============
* Now explicitly imports the ``na.omit`` method from the ``stats`` package.
  Thanks Derrick Oswald for bug report.
* Improved parsing for negative numbers preceded by a space instead of a '=' sign.  
  Thanks Roman Zenka for improved regular expression.
* Slightly more informative error message if `storage.mode` coercion results in an `NA`.
  Thanks Roman Zenka for suggestion.

getopt 1.20.0
=============
* Type of "numeric" in spec automatically cast to "double".  
  Previously users might have had an error passing negative numbers if they
  accidentally specified "numeric" instead of "double".
* Project website moved to https://github.com/trevorld/getopt
* Exports new function ``sort_list``.

getopt 1.19.1
=============
* If a passed in option matches multiple options in the getopt specification but matches one exactly
  then `getopt` now uses that value instead of throwing a "long flag is ambiguous" error.

getopt 1.19.0
=============
* Exports new function `get_Rscript_filename` that returns name of calling script,
  `getopt` now uses this function value as default for `command` argument
* Documentation improved and now highlights differences 
  between `getopt` and `optparse` packages for new undecided users
