/*   
  tfm.cc --  implement Tex_font_metric
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  

  some code shamelessly copied from GNU fontutils-0.6/tfm/tfm_input.c
 */

#include "tfm.hh"
#include "binary-source-file.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "warn.hh"

#define format_str String_convert::form_str

#define FIX_UNITY (1 << 20)

static const Real
fix_to_real (Fix f)
{
  Real r = f / FIX_UNITY + ((Real) (f % FIX_UNITY) / (Real) FIX_UNITY);
  return r;
}

#if 0 //not used
static const Fix
real_to_fix (Real r)
{
  Fix f = (Fix) (floor (r) * FIX_UNITY + (r - floor (r)) * FIX_UNITY);
  return f;
}
#endif


Box
Tex_font_char_metric::dimensions () const
{
  Real d = -depth_;
  return Box (Interval (0, width_),Interval ( d <? height_, d >? height_));
}

Tex_font_char_metric::Tex_font_char_metric ()
{
  exists_b_ = false;
  code_ = 0;;
  width_ = height_ = depth_ = italic_correction_ = 0;
  width_fix_ = height_fix_ = depth_fix_ = italic_correction_fix_ = 0;
}

#define APPEND_CHAR_METRIC_ELT(k)  outstr += to_str (#k) + " "  + to_str (k ## _)  + "; "

String
Tex_font_char_metric::str () const
{
  String outstr ;

  APPEND_CHAR_METRIC_ELT (exists_b);
  APPEND_CHAR_METRIC_ELT (code);
  APPEND_CHAR_METRIC_ELT (width);
  APPEND_CHAR_METRIC_ELT (height);
  APPEND_CHAR_METRIC_ELT (depth);
  APPEND_CHAR_METRIC_ELT (italic_correction);
  
  return outstr + "\n";
}

Tex_font_metric::Tex_font_metric ()
{
}

static Tex_font_char_metric dummy_static_char_metric;

Tex_font_char_metric const &
Tex_font_metric::find_ascii (int ascii, bool warn) const
{
  if (ascii_to_metric_idx_[ascii] >= 0)
    return char_metrics_[ascii_to_metric_idx_ [ascii]];
  else if (warn)

    {
      warning (_f ("can't find ascii character `%d'", ascii));

    }
  return dummy_static_char_metric;  
}

Character_metric*
Tex_font_metric::get_char (int a, bool w) const
{
  return &find_ascii (a, w);
}


String
Tex_font_metric::str () const
{
  String outstr;
  for (int i=0; i < char_metrics_.size (); i++)
    outstr += char_metrics_[i].str ();
  
  return outstr;
}

void
Tex_font_metric::clear (int n)
{
  for (int i=0; i < n; i++)
    ascii_to_metric_idx_.push (-1);
}

/* Most quantities are fixed-point fractions.  */

Real
Tex_font_metric::get_U32_fix_f (Binary_source_file* input)
{
  return fix_to_real (input->get_U32 ());
}


/* Dimensions are a `Fix' scaled by the design size.  */

Real
Tex_font_metric::get_U32_fix_scaled_f (Binary_source_file* input)
{
  return get_U32_fix_f (input) * info_.design_size;
}

String
Tex_font_metric::get_bcpl_str (Binary_source_file* input)
{
  U8 length_u8 = input->get_U8 ();
  String str = input->get_str (length_u8);
  return str;
}

/* Here we read the information at the beginning of the file.  We store
   the result into the static variables `global_info' and
   `tfm_header'.  */
void
Tex_font_metric::read_header (Binary_source_file* input)
{
  U16 file_length = input->get_U16 ();
  (void) file_length;
  U16 header_length = input->get_U16 ();

  info_.first_charcode = input->get_U16 ();
  info_.last_charcode = input->get_U16 ();
  U16 width_word_count = input->get_U16 ();
  U16 height_word_count = input->get_U16 ();
  U16 depth_word_count = input->get_U16 ();
  U16 italic_correction_word_count = input->get_U16 ();
  U16 lig_kern_word_count = input->get_U16 ();
  U16 kern_word_count = input->get_U16 ();
  (void)kern_word_count;
  U16 extensible_word_count = input->get_U16 ();
  (void)extensible_word_count;
  
  header_.param_word_count = input->get_U16 ();
  info_.parameter_count = header_.param_word_count;

  header_.char_info_pos = (6 + header_length) * 4;
  header_.width_pos = header_.char_info_pos
                         + (info_.last_charcode
                            - info_.first_charcode + 1) * 4;
  header_.height_pos = header_.width_pos + width_word_count * 4;
  header_.depth_pos = header_.height_pos + height_word_count * 4;
  header_.italic_correction_pos = header_.depth_pos
                                     + depth_word_count * 4;
  header_.lig_kern_pos = header_.italic_correction_pos
    + italic_correction_word_count * 4;
  header_.kern_pos = header_.lig_kern_pos + lig_kern_word_count * 4;
  /* We don't care about the extensible table.  */

  if (header_length < 2)
    error (_f ("TFM header of `%s' has only %u word(s)",
	       input->name_str ().ch_C (), header_length));

  info_.checksum = input->get_U32 ();
  info_.design_size = get_U32_fix_f (input);

  /* Although the coding scheme might be interesting to the caller, the
     font family and face byte probably aren't.  So we don't read them.  */
  info_.coding_scheme = header_length > 2
    ? get_bcpl_str (input) : "unspecified";

  DOUT << format_str ("TFM checksum = %u, design_size = %fpt, coding scheme = `%s'.\n",
		      info_.checksum,
		      info_.design_size,
		      info_.coding_scheme.ch_C ());
}

void
Tex_font_metric::read_file (String name)
{
  Binary_source_file input (name);

  clear (TFM_SIZE);
  read_header (&input);
  read_params (&input);
  read_char_metrics (&input);
}

/* Although TFM files are only usable by TeX if they have at least seven
   parameters, that is not a requirement of the file format itself, so
   we don't impose it.  And they can have many more than seven, of
   course.  We do impose a limit of TFM_MAX_FONT_PARAMETERS.  We assume
   that `tfm_header' has already been filled in.  */

void
Tex_font_metric::read_params (Binary_source_file* input)
{
  /* If we have no font parameters at all, we're done.  */
  if (header_.param_word_count == 0)
    return;

  //brrr
  /* Move to the beginning of the parameter table in the file.  */
  input->seek_ch_C (-4 * header_.param_word_count);

  /* It's unlikely but possible that this TFM file has more fontdimens
     than we can deal with.  */
  if (header_.param_word_count > TFM_MAX_FONTDIMENS)
    {
      warning (_f ("%s: TFM file has %u parameters, which is more than the
%u I can handle",
		   input->name_str ().ch_C (),
		   header_.param_word_count,
		   TFM_MAX_FONTDIMENS));
      header_.param_word_count = TFM_MAX_FONTDIMENS;
    }

  /* The first parameter is different than all the rest, because it
     isn't scaled by the design size.  */
  info_.parameters[(TFM_SLANT_PARAMETER) - 1] = get_U32_fix_f (input);

  for (Char_code i = 2; i <= header_.param_word_count; i++)
    info_.parameters[i - 1] = get_U32_fix_scaled_f (input);

#ifdef PRINT
  for (Char_code i = 1; i <= header_.param_word_count; i++)
    DOUT << format_str ("TFM parameter %d: %.3f", i, info_.parameters[i - 1]);
#endif
}

/* Read every character in the TFM file, storing the result in the
   static `tfm_char_table'.  We return a copy of that variable.  */

void
Tex_font_metric::read_char_metrics (Binary_source_file* input)
{
  for (int i = info_.first_charcode; i <= info_.last_charcode; i++)
    {
      Tex_font_char_metric tfm_char = read_char_metric (input, i);
      if (tfm_char.exists_b_)
	ascii_to_metric_idx_[tfm_char.code_] = char_metrics_.size ();
      char_metrics_.push (tfm_char);
    }
}

/* Read the character CODE.  If the character doesn't exist, return
   NULL.  If it does, save the information in `tfm_char_table', as well
   as returning it.  */

Tex_font_char_metric
Tex_font_metric::read_char_metric (Binary_source_file* input, Char_code code)
{
  Tex_font_char_metric tfm_char;

  /* If the character is outside the declared bounds in the file, don't
     try to read it. */
  if (code < info_.first_charcode || code > info_.last_charcode)
    return tfm_char;
  
  //brr
  /* Move to the appropriate place in the `char_info' array.  */
  input->seek_ch_C (header_.char_info_pos + (code - info_.first_charcode) * 4);

  /* Read the character.  */
  tfm_char = read_char (input);

  if (tfm_char.exists_b_)
    tfm_char.code_ = code;

  return tfm_char;
}


/* We assume we are positioned at the beginning of a `char_info' word.
   We read that word to get the indexes into the dimension tables; then
   we go read the tables to get the values (if the character exists).  */

Tex_font_char_metric
Tex_font_metric::read_char (Binary_source_file* input)
{
  /* Read the char_info word.  */
  U8 width_index = input->get_U8 ();

  U8 packed;
  packed = input->get_U8 ();
  U8 height_index = (packed & 0xf0) >> 4;
  U8 depth_index = packed & 0x0f;

  packed = input->get_U8 ();
  U8 italic_correction_index = (packed & 0xfc) >> 6;
  U8 tag = packed & 0x3;

  U8 remainder = input->get_U8 ();

  Tex_font_char_metric tfm_char;

#define GET_CHAR_DIMEN(d) \
   if (d##_index != 0) \
     { \
       input->seek_ch_C (header_.##d##_pos + d##_index*4); \
       tfm_char.d##_fix_ = input->get_U32 (); \
       tfm_char.d##_ = fix_to_real (tfm_char.d##_fix_) \
                      * info_.design_size; \
     }

  GET_CHAR_DIMEN (width);
  GET_CHAR_DIMEN (height);
  GET_CHAR_DIMEN (depth);
  GET_CHAR_DIMEN (italic_correction);

  /* The other condition for a character existing is that it be between
     the first and last character codes given in the header.  We've
     already assumed that's true (or we couldn't be positioned at a
     `char_info_word').  */
  tfm_char.exists_b_ = width_index != 0;

#ifdef PRINT
  DOUT << format_str ("   width = %f, height = %f, ",
		      tfm_char.width_, tfm_char.height_);
  DOUT << format_str ("depth = %f, ic = %f.\n",
		      tfm_char.depth, tfm_char.italic_correction); 
#endif

  if (tag == 1)
    {
      input->seek_ch_C (header_.lig_kern_pos + remainder * 4);
      read_lig_kern_program (input, &tfm_char.ligature_arr_, &tfm_char.kern_arr_);
    }

  /* We don't handle the other tags.  */
  return tfm_char;
}

/* Read a ligature/kern program at the current position, storing the
   result into *LIGATURE and *KERN.  We don't distinguish all the kinds
   of ligatures that Metafont can output.  */

#define STOP_FLAG 128
#define KERN_FLAG 128

void
Tex_font_metric::read_lig_kern_program (Binary_source_file* input, Array <Tfm_ligature>* ligature_arr_p, Array <Tfm_kern>* kern_arr_p)
{
  bool end_b;

  do
    {
      end_b = input->get_U8 () >= STOP_FLAG;

      U8 next_char = input->get_U8 ();
      bool kern_step_b = input->get_U8 () >= KERN_FLAG;
      U8 remainder = input->get_U8 ();

#ifdef PRINT
      DOUT << format_str ("   if next = %u (%c), ", next_char, next_char);
#endif

      if (kern_step_b)
	{
	  Tfm_kern kern_element;
	  kern_element.character = next_char;

	  char const* old_pos = input->pos_ch_C ();
	  input->seek_ch_C (header_.kern_pos + remainder * 4);
	  kern_element.kern = get_U32_fix_scaled_f (input);
	  input->set_pos (old_pos);

	  kern_arr_p->push (kern_element);

#ifdef PRINT
	  DOUT << format_str ("kern %f.\n", kern_element.kern);
#endif
	}
      else
	{
	  Tfm_ligature ligature_element;
	  ligature_element.character = next_char;
	  ligature_element.ligature = remainder;
	  ligature_arr_p->push (ligature_element);

#ifdef PRINT
	  DOUT format_str ("ligature %d (hex %x).\n",
			   ligature_element.ligature,
			   ligature_element.ligature);
#endif
	}
  } while (!end_b);
}

