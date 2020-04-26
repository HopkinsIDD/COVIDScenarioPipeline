#include "systemfonts.h"
#include <stdlib.h>

int (*locate_font)(const char *family, int italic, int bold, char *path, 
                   int max_path_length) = NULL;
int (*glyph_metrics)(u_int32_t code, const char* fontfile, int index, 
                     double size, double res, double* ascent, 
                     double* descent, double* width) = NULL;
int (*string_width)(const char* string, const char* fontfile, int index, 
                    double size, double res, int include_bearing, 
                    double* width) = NULL;
int (*string_shape)(const char* string, const char* fontfile, int index, 
                    double size, double res, double* x, double* y, 
                    unsigned int max_length) = NULL;

void vctrs_init_api() {
  locate_font = (int (*)(const char *, int, int, char *, int)) R_GetCCallable("systemfonts", "locate_font");
  glyph_metrics = (int (*)(u_int32_t, const char*, int, double, double, double*, double*, double*)) R_GetCCallable("systemfonts", "glyph_metrics");
  string_width = (int (*)(const char*, const char*, int, double, double, int, double*)) R_GetCCallable("systemfonts", "string_width");
  string_shape = (int (*)(const char*, const char*, int, double, double, double*, double*, unsigned int)) R_GetCCallable("systemfonts", "string_shape");
}
