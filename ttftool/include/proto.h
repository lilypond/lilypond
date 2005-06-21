/* Copyright (c) 1997-1998 by Juliusz Chroboczek */

struct TableDirectoryEntry *readDirectory (FILE *fd, struct OffsetTable *ot);
char **readNamingTable (FILE *fd);
void readHeadTable (FILE *fd, struct HeadTable *ht);
int readPostTable (FILE *fd, int root_nglyphs,
		   struct PostTable *pt, USHORT *nglyphs,
		   struct GlyphName **gnt);
int readMaxpTable (FILE *fd);
void *readLocaTable (FILE *fd, int nglyphs, int format);
struct Box *readGlyfTable (FILE *fd, int nglyphs, int format, void *loca);
longHorMetric *readHmtxTable (FILE *fd, int nummetrics);
struct HheaTable *readHheaTable (FILE *fd);
int readKernTable (FILE *fd, int **nke, struct KernEntry0 ***ke);

void printPSFont (void * out, struct HeadTable *ht,
		  char **strings, int nglyphs, int postType,
		  struct PostTable *pt,
		  USHORT png,
		  struct GlyphName *gnt, FILE *fd);

void printPSHeader (void * out, struct HeadTable *ht,
		    char **strings, struct PostTable *pt);
void printPSData (void * out, FILE *fd);
void printPSTrailer (void * out, int nglyphs,
		     int postType, USHORT pnt, struct GlyphName *gnt);

void printAFM (FILE * afm, struct HeadTable *ht,
	       char **strings, int nglyphs, int postType,
	       struct PostTable *pt, struct GlyphName *gnt,
	       struct Box *glyf, struct HheaTable *hhea, longHorMetric * hmtx,
	       int nkern, int *nke, struct KernEntry0 **ke);
void printAFMHeader (FILE * afm, struct HeadTable *ht,
		     char **strings, struct PostTable *pt);
void printAFMMetrics (FILE * afm, struct HeadTable *ht,
		      int nglyphs, int postType, struct GlyphName *gnt,
		      struct Box *bbox,
		      struct HheaTable *hhea, longHorMetric * hmtx);
void printOneAFMMetric (FILE * afm,
			int index, int code, char *name,
			struct HeadTable *ht,
			struct Box *bbox,
			struct HheaTable *hhea, longHorMetric * hmtx);
void printAFMKerning (FILE * afm, struct HeadTable *ht,
		      int postType, struct GlyphName *gnt,
		      int nkern, int *nke, struct KernEntry0 **ke);

extern char *macGlyphEncoding[];
extern char *adobeStandardEncoding[];

void *mymalloc (size_t size);
void *mycalloc (size_t nelem, size_t elsize);
void *myrealloc (void *ptr, size_t size);
void ttf_error (char *string);
void syserror (char *string);
ssize_t surely_read (FILE *fildes, void *buf, size_t nbyte);
char *unistrncpy (char *dst, char *str, size_t length);
void fputpss (char *s, void * stream);
void surely_lseek (FILE *fildes, off_t offset, int whence);
unsigned hash (char *string);
struct hashtable *make_hashtable (int size);
int puthash (struct hashtable *t, char *key, int value);
int gethash (struct hashtable *t, char *key);

#ifdef TEST_TTFTOOL
#define lily_cookie_fclose fclose
#define lily_cookie_fprintf fprintf
#define lily_cookie_putc fputc
#else
#include "file-cookie.hh"
#endif
