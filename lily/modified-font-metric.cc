/*
  scaled-font-metric.cc -- declare Modified_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999--2004 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include <ctype.h>

#include "warn.hh"
#include "scaled-font-metric.hh"
#include "string.hh"
#include "stencil.hh"

Modified_font_metric::Modified_font_metric (String coding, Font_metric* m, Real magn)
{
  coding_vector_ = SCM_EOL;
  coding_mapping_ = SCM_EOL;
  coding_table_ = SCM_EOL;
  coding_description_ = SCM_EOL;
  
  coding_scheme_ = coding;
  magnification_ = magn;
  
  SCM desc = m->description_;

  Real total_mag = magn * ly_scm2double (ly_cdr (desc));
  assert (total_mag);
  
  description_ = scm_cons (ly_car (desc), scm_make_real (total_mag));
  orig_ = m;

  if (coding_scheme_ != "TeX"
      && coding_scheme_ != "ASCII"
      && coding_scheme_ !=  orig_->coding_scheme ())
    {
      coding_vector_ = scm_call_1 (ly_scheme_function ("get-coding-vector"),
				   scm_makfrom0str (coding_scheme_.to_str0 ()));

      if (!ly_c_vector_p (coding_vector_))
	{
	  programming_error ("get-coding-vector  should return vector");
	  coding_vector_ = scm_c_make_vector (256, ly_symbol2scm (".notdef"));
	}

      
      coding_table_ = scm_call_1 (ly_scheme_function ("get-coding-table"),
				  scm_makfrom0str (orig_->coding_scheme ().to_str0 ()));
	  
      coding_mapping_  = scm_call_2 (ly_scheme_function ("make-encoding-mapping"),
					 coding_vector_,
					 coding_table_);

      coding_description_= SCM_EOL;

      coding_description_ = scm_acons (ly_symbol2scm ("input-name"),
				       scm_makfrom0str (coding_scheme_.to_str0 ()),
				       coding_description_);

      coding_description_ = scm_acons (ly_symbol2scm ("input-vector"),
				       coding_vector_, coding_description_);
      coding_description_ = scm_acons (ly_symbol2scm ("output-name"),
				       scm_makfrom0str (orig_->coding_scheme ().to_str0 ()),
				       coding_description_);
      coding_description_ = scm_acons (ly_symbol2scm ("output-table"),
				       coding_table_,
				       coding_description_);
      
      coding_description_ = scm_acons (ly_symbol2scm ("char-mapping"),
				       coding_mapping_,
				       coding_description_);
    } 
}



LY_DEFINE (ly_font_encoding_alist, "ly:font-encoding-alist",
	   1, 0, 0,
	   (SCM font),
	   "Given the Modified_font_metric @var{font}, return an "
	   "alist. Keys are input-name, input-vector, "
	   "output-name, output-table, mapping.")
{
  Modified_font_metric *fm
    = dynamic_cast<Modified_font_metric*> (unsmob_metrics (font));
  
  SCM_ASSERT_TYPE (fm, font, SCM_ARG1, __FUNCTION__, "Modified_font_metric");
  return fm->coding_description_;
}

SCM
Modified_font_metric::make_scaled_font_metric (SCM coding, Font_metric *m, Real s)
{
  /*
    UGOHR.
   */
  if (ly_c_symbol_p (coding))
    coding = scm_symbol_to_string (coding);
  
  String scheme = ly_scm2string (coding);
  
  Modified_font_metric *sfm = new Modified_font_metric (scheme, m, s);
  
  return sfm->self_scm ();
}

Real
Modified_font_metric::design_size () const
{
  return orig_->design_size ();
}


Box 
Modified_font_metric::get_indexed_char (int i) const
{
  Box b = orig_->get_indexed_char (i);
  b.scale (magnification_);
  return b;  
}

Box 
Modified_font_metric::get_ascii_char (int i) const
{
  Box b = orig_->get_ascii_char (i);
  b.scale (magnification_);
  return b;  
}

int
Modified_font_metric::count () const
{
  return orig_->count ();
}

Offset
Modified_font_metric::get_indexed_wxwy (int k) const
{
  Offset o = orig_->get_indexed_wxwy (k);
  return o * magnification_;
}

int
Modified_font_metric::name_to_index (String s) const
{
  return orig_->name_to_index (s);
}

int
Modified_font_metric::index_to_ascii (int k) const
{
  return orig_->index_to_ascii (k);
}

String
Modified_font_metric::coding_scheme () const
{
  return coding_scheme_;
}

void
Modified_font_metric::derived_mark () const
{
  scm_gc_mark (coding_vector_);
  scm_gc_mark (coding_description_);
  scm_gc_mark (coding_table_);
  scm_gc_mark (coding_mapping_);
}

Box
Modified_font_metric::tex_kludge (String text) const
{
  Interval ydims;
  Real w=0.0;

  /*
    TODO: put this klutchness behind ly:option switch.
  */  
  for (int i = 0; i < text.length (); i++) 
    {
      switch (text[i]) 
	{
	case '\\':
	  // accent marks use width of base letter
         if (i +1 < text.length ())
	   {
	     if (text[i+1]=='\'' || text[i+1]=='`' || text[i+1]=='"' ||
		 text[i+1]=='^')
	       {
		 i++;
		 break;
	       }
	     // for string width \\ is a \ and \_ is a _.
	     if (text[i+1]=='\\' || text[i+1]=='_')        
	       {
		 break;
	       }
	   }
	  
	  for (i++; (i < text.length ()) && !isspace (text[i]) 
		 && text[i]!='{' && text[i]!='}'; i++)
	    ;
	  
	  // ugh.
	  i--; // Compensate for the increment in the outer loop!
	  break;
	case '{':  // Skip '{' and '}'
	case '}':
	  break;
	
	default: 
	  Box b = get_ascii_char ((unsigned char)text[i]);
	  
	  // Ugh, use the width of 'x' for unknown characters
	  if (b[X_AXIS].length () == 0) 
	    b = get_ascii_char ((unsigned char)'x');
	  
	  w += b[X_AXIS].length ();
	  ydims.unite (b[Y_AXIS]);
	  break;
	}
    }
  
  if (ydims.is_empty ())
    ydims = Interval (0, 0);
  
  return Box (Interval (0, w), ydims);
}

Box
Modified_font_metric::text_dimension (String text) 
{
  Box b; 
  if (coding_scheme_ == "TeX")
    {
      b = tex_kludge (text);
    }
  else if (coding_scheme_ == "ASCII"
	   || coding_scheme_ ==  orig_->coding_scheme ())
    {
      Interval ydims;

      Real w=0.0;

      for (int i = 0; i < text.length (); i++) 
	{
	  Box b = get_ascii_char ((unsigned char)text[i]);
    
	  w += b[X_AXIS].length ();
	  ydims.unite (b[Y_AXIS]); 
	}
      if (ydims.is_empty ())
	ydims = Interval (0, 0);

      b = Box(Interval(0,w), ydims);
    }
  else
    {
      Interval ydims;
      Real w = 0.0;

      for (int i = 0; i < text.length (); i++) 
	{
	  SCM sym = scm_vector_ref (coding_vector_,
				    scm_from_int((unsigned char) text[i]));

	  Box char_box;

	  if (!ly_c_symbol_p (sym))
	    continue;

	  char const * chars =  SCM_SYMBOL_CHARS(sym);
	  
	  int idx = orig_->name_to_index (chars);
	  if (idx >= 0)
	    {
	      char_box = orig_->get_indexed_char (idx);
	    }
	  
	  char_box.scale (magnification_);
	  if (!char_box[X_AXIS].is_empty ())
	    w += char_box[X_AXIS][RIGHT]; // length ?

	  ydims.unite (char_box[Y_AXIS]);
	}

      if (ydims.is_empty ())
	ydims = Interval (0, 0);

	  
      b = Box (Interval (0, w), ydims);
    }
  
  return b;
}

Font_metric*
Modified_font_metric::original_font () const
{
  return orig_;
}
