/* Copyright (c) 1997-1998 by Juliusz Chroboczek */

struct TableDirectoryEntry *readDirectory (int fd, struct OffsetTable *ot);
char **readNamingTable (int fd);
void readHeadTable (int fd, struct HeadTable *ht);
int readPostTable (int fd, int nglyphs,
		   struct PostTable *pt, struct GlyphName **gnt);
int readMaxpTable (int fd);
void *readLocaTable (int fd, int nglyphs, int format);
struct Box *readGlyfTable (int fd, int nglyphs, int format, void *loca);
longHorMetric *readHmtxTable (int fd, int nummetrics);
struct HheaTable *readHheaTable (int fd);
int readKernTable (int fd, int **nke, struct KernEntry0 ***ke);

void printPSFont (void * out, struct HeadTable *ht,
		  char **strings, int nglyphs, int postType,
		  struct PostTable *pt, struct GlyphName *gnt, int fd);

void printPSHeader (void * out, struct HeadTable *ht,
		    char **strings, struct PostTable *pt);
void printPSData (void * out, int fd);
void printPSTrailer (void * out, int nglyphs,
		     int postType, struct GlyphName *gnt);

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
ssize_t surely_read (int fildes, void *buf, size_t nbyte);
char *unistrncpy (char *dst, char *str, size_t length);
void fputpss (char *s, void * stream);
off_t surely_lseek (int fildes, off_t offset, int whence);
unsigned hash (char *string);
struct hashtable *make_hashtable (int size);
int puthash (struct hashtable *t, char *key, int value);
int gethash (struct hashtable *t, char *key);
