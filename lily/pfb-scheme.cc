/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2007--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "international.hh"
#include "program-option.hh"
#include "source-file.hh"
#include "open-type-font.hh"
#include "warn.hh"

using std::string;
using std::vector;

LY_DEFINE (ly_type1_2_pfa, "ly:type1->pfa", 1, 0, 0, (SCM type1_file_name),
           R"(
Convert the contents of a Type@tie{}1 font in PFB format to PFA format.  If the
file is already in PFA format, pass it through.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, type1_file_name, 1);

  string file_name = ly_scm2string (type1_file_name);

  debug_output ("[" + file_name); // start message on a new line

  string type1_string = gulp_file (file_name, 0);
  SCM pfa_scm;

  if (static_cast<Byte> (type1_string[0]) == 0x80)
    {
      /* The file is in PFB format. Convert it to PFA format. */
      string pfa = pfb2pfa (type1_string);
      pfa_scm = scm_from_latin1_stringn (&pfa[0], pfa.size ());
    }
  else
    {
      /* The file is in PFA format. Pass it through. */
      pfa_scm
        = scm_from_latin1_stringn (&type1_string[0], type1_string.size ());
    }

  debug_output ("]", false);

  return pfa_scm;
}

LY_DEFINE (ly_otf_2_cff, "ly:otf->cff", 1, 1, 0, (SCM otf_file_name, SCM idx),
           R"(
Convert the contents of an OTF file to a CFF file, returning it as a string.
The optional @var{idx} argument is useful for OpenType/CFF collections (OTC)
only; it specifies the font index within the OTC.  The default value of
@var{idx} is@tie{}0.
           )")
{
  LY_ASSERT_TYPE (scm_is_string, otf_file_name, 1);

  int i = 0;
  if (!SCM_UNBNDP (idx))
    {
      LY_ASSERT_TYPE (scm_is_integer, idx, 2);
      i = from_scm<int> (idx);
      if (i < 0)
        {
          warning (_ ("font index must be non-negative, using index 0"));
          i = 0;
        }
    }

  string file_name = ly_scm2string (otf_file_name);
  debug_output ("[" + file_name); // start message on a new line

  FT_Face face;
  /* check whether font index is valid */
  if (i > 0)
    {
      face = open_ft_face (file_name, -1);
      if (i >= face->num_faces)
        {
          warning (_f ("font index %d too large for font `%s', using index 0",
                       i, file_name.c_str ()));
          i = 0;
        }
      FT_Done_Face (face);
    }

  face = open_ft_face (file_name, i);
  string table = get_otf_table (face, "CFF ");

  SCM asscm = scm_from_latin1_stringn (table.data (), table.length ());
  FT_Done_Face (face);

  debug_output ("]", false);

  return asscm;
}
