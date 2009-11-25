/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

  LilyPond is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  LilyPond is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with LilyPond.  If not, see <http://www.gnu.org/licenses/>.
*/

#include "text-interface.hh"

#include "config.hh"
#include "font-interface.hh"
#include "grob.hh"
#include "main.hh"
#include "misc.hh"
#include "modified-font-metric.hh"
#include "output-def.hh"
#include "pango-font.hh"
#include "warn.hh"

static void
replace_whitespace (string *str)
{
  vsize i = 0;
  vsize n = str->size ();

  while (i < n)
    {
      char cur = (*str)[i];

      // avoid the locale-dependent isspace
      if (cur == '\n' || cur == '\t' || cur == '\v')
	(*str)[i] = ' ';

      vsize char_len = utf8_char_len (cur);

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

