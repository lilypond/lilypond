/*
  ttf.cc --  implement ttf -> pfa routine.

  source file of the GNU LilyPond music typesetter

  (c) 2005--2007 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "freetype.hh"

#include <freetype/tttables.h>

#include "memory-stream.hh"
#include "warn.hh"
#include "lily-guile.hh"
#include "main.hh"
#include "open-type-font.hh"


Index_to_charcode_map
make_index_to_charcode_map (FT_Face face)
{
  Index_to_charcode_map m;
  FT_ULong charcode;
  FT_UInt gindex;

  FT_CharMap current_cmap = face->charmap;
  FT_Select_Charmap (face, FT_ENCODING_UNICODE);

  int j = 0;
  for (charcode = FT_Get_First_Char (face, &gindex); gindex != 0;
       charcode = FT_Get_Next_Char (face, charcode, &gindex))
    {
      m[gindex] = charcode;
      j++;
    }
  FT_Set_Charmap (face, current_cmap);

  
  return m;
}

/*
  Based on ttfps by Juliusz Chroboczek
*/
static void
print_header (void *out, FT_Face face)
{
  lily_cookie_fprintf (out, "%%!PS-TrueTypeFont\n");

  TT_Postscript *pt
    = (TT_Postscript *) FT_Get_Sfnt_Table (face, ft_sfnt_post);

  if (pt->maxMemType42)
    lily_cookie_fprintf (out, "%%%%VMUsage: %d %d\n", 0, 0);

  lily_cookie_fprintf (out, "%d dict begin\n", 11);
  lily_cookie_fprintf (out, "/FontName /%s def\n",
		       FT_Get_Postscript_Name (face));

  lily_cookie_fprintf (out, "/Encoding StandardEncoding def\n");
  lily_cookie_fprintf (out, "/PaintType 0 def\n");
  lily_cookie_fprintf (out, "/FontMatrix [1 0 0 1 0 0] def\n");

  TT_Header *ht
    = (TT_Header *)FT_Get_Sfnt_Table (face, ft_sfnt_head);

  lily_cookie_fprintf (out, "/FontBBox [%ld %ld %ld %ld] def\n",
		       ht->xMin *1000L / ht->Units_Per_EM,
		       ht->yMin *1000L / ht->Units_Per_EM,
		       ht->xMax *1000L / ht->Units_Per_EM,
		       ht->yMax *1000L / ht->Units_Per_EM);

  lily_cookie_fprintf (out, "/FontType 42 def\n");
  lily_cookie_fprintf (out, "/FontInfo 8 dict dup begin\n");
  lily_cookie_fprintf (out, "/version (%d.%d) def\n",
		       int (ht->Font_Revision >> 16),
		       int (ht->Font_Revision &((1 << 16) -1)));

#if 0
  if (strings[0])
    {
      lily_cookie_fprintf (out, "/Notice (");
      fputpss (strings[0], out);
      lily_cookie_fprintf (out, ") def\n");
    }
  if (strings[4])
    {
      lily_cookie_fprintf (out, "/FullName (");
      fputpss (strings[4], out);
      lily_cookie_fprintf (out, ") def\n");
    }
  if (strings[1])
    {
      lily_cookie_fprintf (out, "/FamilyName (");
      fputpss (strings[1], out);
      lily_cookie_fprintf (out, ") def\n");
    }
#endif

  lily_cookie_fprintf (out, "/isFixedPitch %s def\n",
		       pt->isFixedPitch ? "true" : "false");
  lily_cookie_fprintf (out, "/UnderlinePosition %ld def\n",
		       pt->underlinePosition *1000L / ht->Units_Per_EM);
  lily_cookie_fprintf (out, "/UnderlineThickness %ld def\n",
		       pt->underlineThickness *1000L / ht->Units_Per_EM);
  lily_cookie_fprintf (out, "end readonly def\n");
}

#define CHUNKSIZE 65534

static void
print_body (void *out, string name)
{
  FILE *fd = fopen (name.c_str (), "rb");

  static char xdigits[] = "0123456789ABCDEF";

  unsigned char *buffer;
  int i, j;

  buffer = new unsigned char[CHUNKSIZE];
  lily_cookie_fprintf (out, "/sfnts [");
  for (;;)
    {
      i = fread (buffer, 1, CHUNKSIZE, fd);
      if (i == 0)
	break;
      lily_cookie_fprintf (out, "\n<");
      for (j = 0; j < i; j++)
	{
	  if (j != 0 && j % 36 == 0)
	    lily_cookie_putc ('\n', out);
	  /* lily_cookie_fprintf (out,"%02X",(int)buffer[j]) is too slow */
	  lily_cookie_putc (xdigits[ (buffer[j] & 0xF0) >> 4], out);
	  lily_cookie_putc (xdigits[buffer[j] & 0x0F], out);
	}
      lily_cookie_fprintf (out, "00>");	/* Adobe bug? */
      if (i < CHUNKSIZE)
	break;
    }
  lily_cookie_fprintf (out, "\n] def\n");
  delete[] buffer;
  fclose (fd);
}

#if 0
static
void t42_write_sting (void *out, unsigned char const * buffer, size_t s)
{
  lily_cookie_fprintf (out, "\n<");
  int l = 0;
  static char xdigits[] = "0123456789ABCDEF";
  for (size_t j = 0; j < s; j++)
    {
      if (j != 0 && j % 36 == 0)
	lily_cookie_putc ('\n', out);

      if (l ++ >= CHUNKSIZE)
	lily_cookie_fprintf (out, "00>\n<");

      /* lily_cookie_fprintf (out,"%02X",(int)buffer[j]) is too slow */
      lily_cookie_putc (xdigits[ (buffer[j] & 0xF0) >> 4], out);
      lily_cookie_putc (xdigits[buffer[j] & 0x0F], out);
    }
  lily_cookie_fprintf (out, "00>");	/* Adobe bug? */
}


static void
new_print_body (void *out,  FT_Face face)
{
  FT_UInt idx = 0;

  FT_ULong tag, length;
  
  lily_cookie_fprintf (out, "/sfnts [");
  while (FT_Sfnt_Table_Info(face, idx, &tag, &length)!=
	 FT_Err_Table_Missing)
    {
      unsigned char *buf = new unsigned char[length];
      FT_Error error = FT_Load_Sfnt_Table(face, tag, 0, buf, NULL); 

      t42_write_sting (out, buf, length);

      delete[] buf;
      idx ++;
    }
  lily_cookie_fprintf (out, "\n] def\n");
}
#endif

static void
print_trailer (void *out,
	       FT_Face face)
{
  const int GLYPH_NAME_LEN = 256;
  char glyph_name[GLYPH_NAME_LEN];

  TT_MaxProfile *mp
    = (TT_MaxProfile *)FT_Get_Sfnt_Table (face, ft_sfnt_maxp);

  lily_cookie_fprintf (out, "/CharStrings %d dict dup begin\n", mp->numGlyphs);

  Index_to_charcode_map ic_map (make_index_to_charcode_map (face));

  int output_count = 0;
  for (int i = 0; i < mp->numGlyphs; i++)
    {
      glyph_name[0] = 0;
      if (face->face_flags & FT_FACE_FLAG_GLYPH_NAMES)
	{
	  FT_Error error = FT_Get_Glyph_Name (face, i, glyph_name,
					      GLYPH_NAME_LEN);
	  if (error)
	    {
	      programming_error ("FT_Get_Glyph_Name (): error.");
	      glyph_name[0] = 0;
	    }
	}

      if (!glyph_name[0] && ic_map.find (i) != ic_map.end ())
	{
	  FT_ULong ucode = ic_map[i];
	  get_unicode_name (glyph_name, ucode);
	}

      if (glyph_name == string (".notdef"))
	glyph_name[0] = '\0';

      
      if (!glyph_name[0])
	{
	  get_glyph_index_name (glyph_name, i);
	}
      
      if (glyph_name[0])
	{
	  lily_cookie_fprintf (out, "/%s %d def ", glyph_name, i);
	  output_count ++;
	}
      else
	{
	  programming_error (to_string ("no name for glyph %d", i));
	}
			     
      if (! (output_count % 5))
	lily_cookie_fprintf (out, "\n");
    }

  lily_cookie_fprintf (out, "end readonly def\n");
  lily_cookie_fprintf (out, "FontName currentdict end definefont pop\n");
}

static void
create_type42_font (void *out, string name)
{
  FT_Face face = open_ft_face (name);

  print_header (out, face);
  // new_print_body (out, face);
  print_body (out, name);
  print_trailer (out, face);

  FT_Done_Face (face);
}


LY_DEFINE (ly_ttf_ps_name, "ly:ttf-ps-name",
	   1, 0, 0, (SCM ttf_file_name),
	   "Extract the PostScript name from a TrueType font.")
{
  LY_ASSERT_TYPE (scm_is_string, ttf_file_name, 1);
  string file_name = ly_scm2string (ttf_file_name);
  if (be_verbose_global)
    progress_indication ("[" + file_name);

  FT_Face face = open_ft_face (file_name);
  char const *ps_name_str0 = FT_Get_Postscript_Name (face);
  SCM ps_name = scm_from_locale_string (ps_name_str0 ? ps_name_str0 : "");
  
  FT_Done_Face (face);
  
  if (be_verbose_global)
    progress_indication ("]");
  
  return ps_name;
}



LY_DEFINE (ly_ttf_2_pfa, "ly:ttf->pfa",
	   1, 0, 0, (SCM ttf_file_name),
	   "Convert the contents of a TTF file to Type42 PFA, returning it as "
	   " a string.")
{
  LY_ASSERT_TYPE (scm_is_string, ttf_file_name, 1);

  string file_name = ly_scm2string (ttf_file_name);
  if (be_verbose_global)
    progress_indication ("[" + file_name);

  Memory_out_stream stream;

  create_type42_font (&stream, file_name);
  SCM asscm = scm_from_locale_stringn (stream.get_string (),
				       stream.get_length ());

  if (be_verbose_global)
    progress_indication ("]");

  return asscm;
}
