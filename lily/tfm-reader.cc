/*   
  tfm-reader.cc --  implement Tex_font_metric_reader
  
  source file of the GNU LilyPond music typesetter
  
  (c) 1999 Jan Nieuwenhuizen <janneke@gnu.org>
  

  some code shamelessly copied from GNU fontutils-0.6/tfm/tfm_input.c
 */

#include "tfm-reader.hh"
#include "string-convert.hh"
#include "debug.hh"
#include "warn.hh"

#define format_str String_convert::form_str
#define FIX_UNITY (1 << 20)
static const Real fix_to_real (Fix f);


Tex_font_metric_reader::Tex_font_metric_reader (String name)
  : input_ (name)
{
  tfm_.clear (TFM_SIZE);
}

Tex_font_metric
Tex_font_metric_reader::read_tfm ()
{
  read_header ();
  read_params ();
  read_char_metrics ();
  return tfm_;
}


static const Real
fix_to_real (Fix f)
{
  Real r = f / FIX_UNITY + ((Real) (f % FIX_UNITY) / (Real) FIX_UNITY);
  return r;
}

/* Most quantities are fixed-point fractions.  */

Real
Tex_font_metric_reader::get_U32_fix_f ()
{
  return fix_to_real (input_.get_U32 ());
}

/* Dimensions are a `Fix' scaled by the design size.  */

Real
Tex_font_metric_reader::get_U32_fix_scaled_f ()
{
  return get_U32_fix_f () * tfm_.info_.design_size;
}

String
Tex_font_metric_reader::get_bcpl_str ()
{
  U8 length_u8 = input_.get_U8 ();
  String str = input_.get_str (length_u8);
  return str;
}

/* Here we read the information at the beginning of the file.  We store
   the result into the static variables `global_info' and
   `tfm_header'.  */
void
Tex_font_metric_reader::read_header ()
{
  U16 file_length = input_.get_U16 ();
  (void) file_length;
  U16 header_length = input_.get_U16 ();

  tfm_.info_.first_charcode = input_.get_U16 ();
  tfm_.info_.last_charcode = input_.get_U16 ();
  U16 width_word_count = input_.get_U16 ();
  U16 height_word_count = input_.get_U16 ();
  U16 depth_word_count = input_.get_U16 ();
  U16 italic_correction_word_count = input_.get_U16 ();
  U16 lig_kern_word_count = input_.get_U16 ();
  U16 kern_word_count = input_.get_U16 ();
  (void)kern_word_count;
  U16 extensible_word_count = input_.get_U16 ();
  (void)extensible_word_count;
  
  tfm_.header_.param_word_count = input_.get_U16 ();
  tfm_.info_.parameter_count = tfm_.header_.param_word_count;

  tfm_.header_.char_info_pos = (6 + header_length) * 4;
  tfm_.header_.width_pos = tfm_.header_.char_info_pos
                         + (tfm_.info_.last_charcode
                            - tfm_.info_.first_charcode + 1) * 4;
  tfm_.header_.height_pos = tfm_.header_.width_pos + width_word_count * 4;
  tfm_.header_.depth_pos = tfm_.header_.height_pos + height_word_count * 4;
  tfm_.header_.italic_correction_pos = tfm_.header_.depth_pos
                                     + depth_word_count * 4;
  tfm_.header_.lig_kern_pos = tfm_.header_.italic_correction_pos
    + italic_correction_word_count * 4;
  tfm_.header_.kern_pos = tfm_.header_.lig_kern_pos + lig_kern_word_count * 4;
  /* We don't care about the extensible table.  */

  if (header_length < 2)
    error (_f ("TFM header of `%s' has only %u word(s)",
	       input_.name_str ().ch_C (), header_length));

  tfm_.info_.checksum = input_.get_U32 ();
  tfm_.info_.design_size = get_U32_fix_f ();

  /* Although the coding scheme might be interesting to the caller, the
     font family and face byte probably aren't.  So we don't read them.  */
  tfm_.info_.coding_scheme = header_length > 2
    ? get_bcpl_str () : "unspecified";

  DEBUG_OUT << format_str ("TFM checksum = %u, design_size = %fpt, coding scheme = `%s'.\n",
		      tfm_.info_.checksum,
		      tfm_.info_.design_size,
		      tfm_.info_.coding_scheme.ch_C ());
}

/* Although TFM files are only usable by TeX if they have at least seven
   parameters, that is not a requirement of the file format itself, so
   we don't impose it.  And they can have many more than seven, of
   course.  We do impose a limit of TFM_MAX_FONT_PARAMETERS.  We assume
   that `tfm_header' has already been filled in.  */

void
Tex_font_metric_reader::read_params ()
{
  /* If we have no font parameters at all, we're done.  */
  if (tfm_.header_.param_word_count == 0)
    return;

  //brrr
  /* Move to the beginning of the parameter table in the file.  */
  input_.seek_ch_C (-4 * tfm_.header_.param_word_count);

  /* It's unlikely but possible that this TFM file has more fontdimens
     than we can deal with.  */
  if (tfm_.header_.param_word_count > TFM_MAX_FONTDIMENS)
    {
      warning (_f ("%s: TFM file has %u parameters, which is more than the %u I can handle",
		   input_.name_str ().ch_C (),
		   tfm_.header_.param_word_count,
		   TFM_MAX_FONTDIMENS));
      tfm_.header_.param_word_count = TFM_MAX_FONTDIMENS;
    }

  /* The first parameter is different than all the rest, because it
     isn't scaled by the design size.  */
  tfm_.info_.parameters[(TFM_SLANT_PARAMETER) - 1] = get_U32_fix_f ();

  for (Char_code i = 2; i <= tfm_.header_.param_word_count; i++)
    tfm_.info_.parameters[i - 1] = get_U32_fix_scaled_f ();

#ifdef PRINT
  for (Char_code i = 1; i <= tfm_.header_.param_word_count; i++)
    DEBUG_OUT << format_str ("TFM parameter %d: %.3f", i, tfm_.info_.parameters[i - 1]);
#endif
}

/* Read every character in the TFM file, storing the result in the
   static `tfm_char_table'.  We return a copy of that variable.  */

void
Tex_font_metric_reader::read_char_metrics ()
{
  for (int i = tfm_.info_.first_charcode; i <= tfm_.info_.last_charcode; i++)
    {
      Tex_font_char_metric tfm_char = read_char_metric (i);
      if (tfm_char.exists_b_)
	tfm_.ascii_to_metric_idx_[tfm_char.code_] = tfm_.char_metrics_.size ();
      tfm_.char_metrics_.push (tfm_char);
    }
}

/* Read the character CODE.  If the character doesn't exist, return
   NULL.  If it does, save the information in `tfm_char_table', as well
   as returning it.  */

Tex_font_char_metric
Tex_font_metric_reader::read_char_metric (Char_code code)
{
  Tex_font_char_metric tfm_char;

  /* If the character is outside the declared bounds in the file, don't
     try to read it. */
  if (code < tfm_.info_.first_charcode || code > tfm_.info_.last_charcode)
    return tfm_char;
  
  //brr
  /* Move to the appropriate place in the `char_info' array.  */
  input_.seek_ch_C (tfm_.header_.char_info_pos + (code - tfm_.info_.first_charcode) * 4);

  /* Read the character.  */
  tfm_char = read_char ();

  if (tfm_char.exists_b_)
    tfm_char.code_ = code;

  return tfm_char;
}


/* We assume we are positioned at the beginning of a `char_info' word.
   We read that word to get the indexes into the dimension tables; then
   we go read the tables to get the values (if the character exists).  */

Tex_font_char_metric
Tex_font_metric_reader::read_char ()
{
  /* Read the char_info word.  */
  U8 width_index = input_.get_U8 ();

  U8 packed;
  packed = input_.get_U8 ();
  U8 height_index = (packed & 0xf0) >> 4;
  U8 depth_index = packed & 0x0f;

  packed = input_.get_U8 ();
  U8 italic_correction_index = (packed & 0xfc) >> 6;
  U8 tag = packed & 0x3;

  U8 remainder = input_.get_U8 ();

  Tex_font_char_metric tfm_char;

#define GET_CHAR_DIMEN(d) \
   if (d##_index != 0) \
     { \
       input_.seek_ch_C (tfm_.header_.##d##_pos + d##_index*4); \
       tfm_char.d##_fix_ = input_.get_U32 (); \
       tfm_char.d##_ = fix_to_real (tfm_char.d##_fix_) \
                      * tfm_.info_.design_size; \
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
  DEBUG_OUT << format_str ("   width = %f, height = %f, ",
		      tfm_char.width_, tfm_char.height_);
  DEBUG_OUT << format_str ("depth = %f, ic = %f.\n",
		      tfm_char.depth, tfm_char.italic_correction); 
#endif

  if (tag == 1)
    {
      input_.seek_ch_C (tfm_.header_.lig_kern_pos + remainder * 4);
      read_lig_kern_program (&tfm_char.ligature_arr_, &tfm_char.kern_arr_);
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
Tex_font_metric_reader::read_lig_kern_program (Array <Tfm_ligature>* ligature_arr_p, Array <Tfm_kern>* kern_arr_p)
{
  bool end_b;

  do
    {
      end_b = input_.get_U8 () >= STOP_FLAG;

      U8 next_char = input_.get_U8 ();
      bool kern_step_b = input_.get_U8 () >= KERN_FLAG;
      U8 remainder = input_.get_U8 ();

#ifdef PRINT
      DEBUG_OUT << format_str ("   if next = %u (%c), ", next_char, next_char);
#endif

      if (kern_step_b)
	{
	  Tfm_kern kern_element;
	  kern_element.character = next_char;

	  char const* old_pos = input_.pos_ch_C ();
	  input_.seek_ch_C (tfm_.header_.kern_pos + remainder * 4);
	  kern_element.kern = get_U32_fix_scaled_f ();
	  input_.set_pos (old_pos);

	  kern_arr_p->push (kern_element);

#ifdef PRINT
	  DEBUG_OUT << format_str ("kern %f.\n", kern_element.kern);
#endif
	}
      else
	{
	  Tfm_ligature ligature_element;
	  ligature_element.character = next_char;
	  ligature_element.ligature = remainder;
	  ligature_arr_p->push (ligature_element);

#ifdef PRINT
	  DEBUG_OUT format_str ("ligature %d (hex %x).\n",
			   ligature_element.ligature,
			   ligature_element.ligature);
#endif
	}
  } while (!end_b);
}

