# systemfonts 0.2.0

* Add `string_widths_dev()` and `string_metrics_dev()` to request the current 
  graphic device for string widths and metrics.
* Add system for registering non-system fonts for look-up.
* systemfonts will now detect user-installed fonts on Windows 
  (possible after the 1806 update)
* Font lookup is now cached for faster performance. The caching will get flushed
  when new fonts are added to the registry, or manually with `reset_font_cache()`
* Systemfonts now provide querying of font information with `font_info()` and 
  `glyph_info()`
* Basic string shaping is now provided with `shape_string()`
* Line width calculation is now available with `string_width()` (ignores 
  presence of newlines, use `shape_string()` for more complicated strings)
* Added `str_split_emoji()` for splitting of strings into substrings of emoji 
  and non-emoji glyphs
* Provide a header file for easy use from within C in other packages
* Fix memory management issues on Mac
* Fix handling of erroneous font files on windows

# systemfonts 0.1.1

* Fix compilation on systems with a very old fontconfig version (Solaris)

# systemfonts 0.1.0

* First version with `match_font()` and `system_fonts()` capabilities. More to
  come.
* Added a `NEWS.md` file to track changes to the package.
