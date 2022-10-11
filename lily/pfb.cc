/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2004--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <iomanip>
#include <sstream>

using std::string;
using std::stringstream;
using std::vector;

string
pfb2pfa (const string &pfb)
{
  string out;

  string::const_iterator p = pfb.begin ();
  while (p < pfb.end ())
    {
      if (static_cast<Byte> (*p++) != 128)
        {
          error (_ ("Segment header of the Type 1 (PFB) font is broken."));
          break;
        }

      Byte type = static_cast<Byte> (*p++);
      if (type == 3)
        break;

      size_t seglen = static_cast<Byte> (*p++);
      seglen |= (static_cast<Byte> (*p++) << 8);
      seglen |= (static_cast<Byte> (*p++) << 16);
      seglen |= (static_cast<Byte> (*p++) << 24);
      if ((p + seglen) > pfb.end ())
        {
          error (_ ("Segment length of the Type 1 (PFB) font is too long."));
          break;
        }

      if (type == 1)
        {
          copy (p, p + seglen, back_inserter (out));
          p += seglen;
        }
      else if (type == 2)
        {
          stringstream ss;

          ss << std::hex << std::setfill ('0');

          for (size_t i = seglen; i > 0; --i)
            {
              ss << std::setw (2)
                 << static_cast<int> (static_cast<Byte> (*p++));
              if (!(i % 32))
                ss << '\n';
            }

          string str = ss.str ();
          copy (str.begin (), str.end (), back_inserter (out));
        }
      else
        {
          error (_ ("Segment type of the Type 1 (PFB) font is unknown."));
          break;
        }
    }

  return out;
}
