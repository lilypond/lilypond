/* @configure_input@ */
#ifndef CONFIG_HH
#define CONFIG_HH

/* autoheader really wants this */
#define PACKAGE

/* undef to get lots of debugging stuff (see .dstream) */
#define NPRINT

/* undef to do checking */
#undef NDEBUG

/* define to inline string funcs */
#undef STRINGS_UTILS_INLINED

/* default lilypond init and input dir */
#define  DIR_DATADIR "/home/fred/lelie/current"

/* the toplevel version string */
#define TOPLEVEL_VERSION "0"

/* the version string of the flower lib */
#define FLOWER_VERSION "0"

/* define if you have memmem */
#define HAVE_MEMMEM 0

/* define if you have snprintf */
#define HAVE_SNPRINTF 0

#endif

