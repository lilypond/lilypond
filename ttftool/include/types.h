/* Copyright (c) 1997-1998 by Juliusz Chroboczek */


typedef unsigned char BYTE;
typedef signed char CHAR;
typedef unsigned short USHORT;
typedef short SHORT;
typedef unsigned long ULONG;
typedef long LONG;

/* Endianness conversions */
#ifdef SMALLENDIAN
#define H(x) ((SHORT)((((SHORT)(x))&0xFF)<<8)+(((USHORT)(x))>>8))
#define UH(x) ((USHORT)((((USHORT)(x))&0xFF)<<8)+(((USHORT)(x))>>8))
#define L(x) ((LONG)(((LONG)UH((x)&0xFFFF))<<16)+UH(((ULONG)(x))>>16))
#define UL(x) ((ULONG)(((ULONG)UH((x)&0xFFFF))<<16)+UH(((ULONG)(x))>>16))
#else
#define H(x) ((SHORT)x)
#define UH(x) ((USHORT)x)
#define L(x) ((LONG)x)
#define UL(x) ((ULONG)x)
#endif

#define FIX_H(x) x=H(x)
#define FIX_UH(x) x=UH(x)
#define FIX_L(x) x=L(x)
#define FIX_UL(x) x=UL(x)

/* We are assuming that the compiler will not pad the following
 * structures; note that their members are intrinsically properly
 * aligned.  This will probably break on some machines. */

typedef struct
{
  SHORT mantissa;
  USHORT fraction;
} Fixed;

#define FIX_Fixed(x) {(x).mantissa=H((x).mantissa); (x).fraction=UH((x).fraction);}

typedef struct
{
  BYTE data[8];
} longDateTime;

typedef USHORT uFWord;
typedef SHORT FWord;
typedef USHORT F2Dot14;

#define MAKE_ULONG(a,b,c,d) ((ULONG)(((ULONG)(a)<<24)|((b)<<16)|((c)<<8)|(d)))

/*------------------------------------------------------------*/

struct OffsetTable
{
  Fixed version;
  USHORT numTables;
  USHORT searchRange;
  USHORT entrySelector;
  USHORT rangeShift;
};
#define FIX_OffsetTable(x) \
  {FIX_Fixed((x).version);\
   FIX_UH((x).numTables);\
   FIX_UH((x).searchRange);\
   FIX_UH((x).entrySelector);}

struct TableDirectoryEntry
{
  ULONG tag;
  ULONG checkSum;
  ULONG offset;
  ULONG length;
};
#define FIX_TableDirectoryEntry(x) \
  {FIX_UL((x).tag); FIX_UL((x).checkSum);\
   FIX_UL((x).offset); FIX_UL((x).length);}

/*------------------------------------------------------------*/

struct HeadTable
{
  Fixed version;
  Fixed fontRevision;
  ULONG checkSumAdjustment;
  ULONG magicNumber;
  USHORT flags;
  USHORT unitsPerEm;
  longDateTime created;
  longDateTime modified;
  FWord xMin;
  FWord yMin;
  FWord xMax;
  FWord yMax;
  USHORT macStyle;
  USHORT lowestRecPPEM;
  SHORT fontDirectionHint;
  SHORT indexToLocFormat;
  SHORT glyphDataFormat;
};
#define FIX_HeadTable(x) \
  {FIX_Fixed((x).version); FIX_Fixed((x).fontRevision);\
   FIX_UL((x).checkSumAdjustment); FIX_UL((x).magicNumber);\
   FIX_UH((x).flags); FIX_UH((x).unitsPerEm);\
   FIX_UH((x).xMin); FIX_UH((x).yMin); FIX_UH((x).xMax); FIX_UH((x).yMax);\
   FIX_UH((x).macStyle); FIX_UH((x).lowestRecPPEM);\
   FIX_H((x).fontDirectionHint); FIX_H((x).indexToLocFormat);\
   FIX_H((x).glyphDataFormat);}


/*------------------------------------------------------------*/

struct NameRecord
{
  USHORT platformID;
  USHORT encodingID;
  USHORT languageID;
  USHORT nameID;
  USHORT length;
  USHORT offset;
};
#define FIX_NameRecord(x) \
  {FIX_UH((x).platformID); FIX_UH((x).encodingID); FIX_UH((x).languageID);\
   FIX_UH((x).nameID); FIX_UH((x).length); FIX_UH((x).offset);}



/*------------------------------------------------------------*/

struct PostTable
{
  Fixed formatType;
  Fixed italicAngle;
  FWord underlinePosition;
  FWord underlineThickness;
  ULONG isFixedPitch;
  ULONG minMemType42;
  ULONG maxMemType42;
  ULONG minMemType1;
  ULONG maxMemType1;
};
#define FIX_PostTable(x) \
  {FIX_Fixed((x).formatType); FIX_Fixed((x).italicAngle);\
   FIX_H((x).underlinePosition); FIX_H((x).underlineThickness);\
   FIX_UL((x).isFixedPitch);\
   FIX_UL((x).minMemType42); FIX_UL((x).maxMemType42);\
   FIX_UL((x).minMemType1); FIX_UL((x).maxMemType1); }

struct GlyphName
{
  int type;
  union
  {
    int index;
    char *name;
  } name;
};

/*-----------------------------------------------------------------*/
struct HheaTable
{
  Fixed version;
  FWord Ascender;
  FWord Descender;
  FWord LineGap;
  uFWord advanceWidthMax;
  FWord minLeftSideBearing;
  FWord minRightSideBearing;
  FWord xMaxExtent;
  SHORT caretSlopeRise;
  SHORT caretSlopeRun;
  SHORT reserved[5];
  SHORT metricDataFormat;
  USHORT numberOfHMetrics;
};
#define FIX_HheaTable(x) \
  {FIX_Fixed((x).version); FIX_H((x).Ascender); FIX_H((x).Descender); FIX_H((x).LineGap);\
   FIX_UH((x).advanceWidthMax);\
   FIX_H((x).minLeftSideBearing); FIX_H((x).minRightSideBearing);\
   FIX_H((x).xMaxExtent); FIX_H((x).caretSlopeRise); FIX_H((x).caretSlopeRun);\
   FIX_H((x).metricDataFormat); FIX_UH((x).numberOfHMetrics);}

struct Box
{
  FWord xMin;
  FWord yMin;
  FWord xMax;
  FWord yMax;
};
#define FIX_Box(x) {FIX_H((x).xMin); FIX_H((x).yMin); FIX_H((x).xMax); FIX_H((x).yMax);}

typedef struct
{
  uFWord advanceWidth;
  FWord lsb;
} longHorMetric;
#define FIX_longHorMetric(x) {FIX_UH((x).advanceWidth); FIX_H((x).lsb);}

/*------------------------------------------------------------*/
struct KernTable
{
  USHORT version;
  USHORT nTables;
};
#define FIX_KernTable(x) {FIX_UH((x).version); FIX_UH((x).nTables);}

struct KernSubTableHeader
{
  USHORT version;
  USHORT length;
  USHORT coverage;
};
#define FIX_KernSubTableHeader(x) \
  {FIX_UH((x).version); FIX_UH((x).length); FIX_UH((x).coverage);}


#define kernHorizontal 0x0001
#define kernMinimum 0x0002
#define kernCrossStream 0x0004
#define kernOverride 0x0008
#define kernFormat(coverage) ((coverage)>>8)

struct KernSubTable0
{
  USHORT nPairs;
  USHORT searchRange;
  USHORT entrySelector;
  USHORT rangeShift;
};

#define FIX_KernSubTable0(x) \
  {FIX_UH((x).nPairs); FIX_UH((x).searchRange);\
   FIX_UH((x).entrySelector); FIX_UH((x).rangeShift);}

struct KernEntry0
{
  USHORT left;
  USHORT right;
  FWord value;
};
#define FIX_KernEntry0(x) \
  {FIX_UH((x).left); FIX_UH((x).right); FIX_H((x).value);}


/*------------------------------------------------------------*/
/* Hashtables */

struct hashtable_entry
{
  char *key;
  int value;
};

struct hashtable_bucket
{
  int size;
  int nentries;
  struct hashtable_entry *entries;
};

struct hashtable
{
  int size;
  struct hashtable_bucket **buckets;
};
