/*
  text-interface.cc -- implement Text_interface

  source file of the GNU LilyPond music typesetter

  (c) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "text-interface.hh"


#include "main.hh"
#include "config.hh"
#include "pango-font.hh"
#include "warn.hh"
#include "grob.hh"
#include "font-interface.hh"
#include "output-def.hh"
#include "modified-font-metric.hh"

static void
replace_whitespace (string *str)
{
  vsize i = 0;
  vsize n = str->size ();

  while (i < n)
    {
      vsize char_len = 1;
      char cur = (*str)[i];
      
      // U+10000 - U+10FFFF
      if ((cur & 0x11110000) == 0x11110000)
	char_len = 4;
      // U+0800 - U+FFFF
      else if ((cur & 0x11100000) == 0x11100000)
	char_len = 3;
      // U+0080 - U+07FF
      else if ((cur & 0x11000000) == 0x11000000)
	char_len = 2;
      else if (cur & 0x10000000)
	programming_error ("invalid utf-8 string");
      else
	// avoid the locale-dependent isspace
	if (cur == '\n' || cur == '\t' || cur == '\v')
	  (*str)[i] = ' ';

      i += char_len;
    }
}

MAKE_SCHEME_CALLBACK (Text_interface, interpret_string, 3);
SCM
Text_interface::interpret_string (SCM layout_smob,
				  SCM props,
				  SCM markup)
{
  LY_ASSERT_SMOB (Output_def, layout_smob, 1);
  LY_ASSERT_TYPE (scm_is_string, markup, 3);

  string str = ly_scm2string (markup);
  Output_def *layout = unsmob_output_def (layout_smob);
  Font_metric *fm = select_encoded_font (layout, props);

  replace_whitespace (&str);

  /*
    We want to use "glyph-string" in the SVG backend for all
    music fonts (Emmentaler and Aybabtu) that pass through the
    text interface.  Here the font encoding is checked to see if
    it matches one of the music font encodings.  --pmccarty
  */
  SCM encoding = ly_chain_assoc_get (ly_symbol2scm ("font-encoding"),
				     props,
				     SCM_BOOL_F);
  SCM music_encodings = ly_lily_module_constant ("all-music-font-encodings");

  if (scm_memq (encoding, music_encodings) != SCM_BOOL_F)
    return fm->word_stencil (str, true).smobbed_copy ();
  else
    return fm->word_stencil (str, false).smobbed_copy ();
}

MAKE_SCHEME_CALLBACK_WITH_OPTARGS (Text_interface, interpret_markup, 3, 0,
				   "Convert a text markup into a stencil."
"  Takes three arguments, @var{layout}, @var{props}, and @var{markup}.\n"
"\n"
"@var{layout} is a @code{\\layout} block; it may be obtained from a grob with"
" @code{ly:grob-layout}.  @var{props} is an alist chain, i.e. a list of"
"  alists.  This is typically obtained with"
" @code{(ly:grob-alist-chain grob (ly:output-def-lookup layout 'text-font-defaults))}."
"  @var{markup} is the markup text to be processed.");
SCM
Text_interface::interpret_markup (SCM layout_smob, SCM props, SCM markup)
{
  if (scm_is_string (markup))
    return interpret_string (layout_smob, props, markup);
  else if (scm_is_pair (markup))
    {
      SCM func = scm_car (markup);
      SCM args = scm_cdr (markup);
      if (!is_markup (markup))
	programming_error ("markup head has no markup signature");

      return scm_apply_2 (func, layout_smob, props, args);
    }
  else
    {
      programming_error ("Object is not a markup. ");
      scm_puts ("This object should be a markup: ", scm_current_error_port ());
      scm_display (markup, scm_current_error_port ());
      scm_puts ("\n", scm_current_error_port ());

      Box b;
      b[X_AXIS].set_empty ();
      b[Y_AXIS].set_empty ();

      Stencil s (b, SCM_EOL);
      return s.smobbed_copy ();
    }
}

MAKE_SCHEME_CALLBACK (Text_interface, print, 1);
SCM
Text_interface::print (SCM grob)
{
  Grob *me = unsmob_grob (grob);

  SCM t = me->get_property ("text");
  SCM chain = Font_interface::text_font_alist_chain (me);
  return interpret_markup (me->layout ()->self_scm (), chain, t);
}

/* Ugh. Duplicated from Scheme.  */
bool
Text_interface::is_markup (SCM x)
{
  return (scm_is_string (x)
	  || (scm_is_pair (x)
	      && SCM_BOOL_F
	      != scm_object_property (scm_car (x),
				      ly_symbol2scm ("markup-signature"))));
}

bool
Text_interface::is_markup_list (SCM x)
{
  SCM music_list_p = ly_lily_module_constant ("markup-list?");
  return scm_is_true (scm_call_1 (music_list_p, x));
}


ADD_INTERFACE (Text_interface,
	       "A Scheme markup text, see @ruser{Formatting text} and"
	       " @rextend{New markup command definition}.\n"
	       "\n"
	       "There are two important commands:"
	       " @code{ly:text-interface::print}, which is a"
	       " grob callback, and"
	       " @code{ly:text-interface::interpret-markup}.",

	       /* properties */
	       "baseline-skip "
	       "text "
	       "word-space "
	       "text-direction "
	       );

