#ifndef SYSTEMFONTS_H
#define SYSTEMFONTS_H

#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <stdbool.h>

#include <stdint.h>
typedef uint32_t u_int32_t;

// Get the file and index of a font given by its name, along with italic and
// bold status. Writes filepath to `path` and returns the index
extern int (*locate_font)(const char *family, int italic, int bold, char *path, 
                          int max_path_length);
// Get ascent, descent, and width of a glyph, given by its unicode number, 
// fontfile and index, along with its size and the resolution. Returns 0 if
// successful
extern int (*glyph_metrics)(u_int32_t code, const char* fontfile, int index, 
                            double size, double res, double* ascent, 
                            double* descent, double* width);
// Calculate the width of a string based on a fontfile, index, size, and 
// resolution. Writes it to width, and returns 0 if successful
extern int (*string_width)(const char* string, const char* fontfile, int index, 
                           double size, double res, int include_bearing, 
                           double* width);
// Calculate glyph positions for a string based on a fontfile, index, size, and
// resolution, and writes it to the x and y arrays. Returns 0 if successful.
extern int (*string_shape)(const char* string, const char* fontfile, int index, 
                           double size, double res, double* x, double* y, 
                           unsigned int max_length);

// Call this function to setup systemfonts in your own init call
void systemfonts_init_api();

#endif
