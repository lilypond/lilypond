/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef OVERLAY_STRING_PORT_HH
#define OVERLAY_STRING_PORT_HH

#include "lily-guile.hh"

/*
   Overlays a string port on existing data, without copying

   GUILE v2 has a decent API for defining ports. We setup the encoding
   as UTF-8 so UTF-8 within the embedded Scheme gets properly stored.

   The 1.8 API is terrible and incompatible. We do mostly a cut &
   paste from strport.c, which installs the data directly into the
   port structure as buffered data.
*/
class Overlay_string_port
{
  typedef scm_t_off guile_off_t;
  typedef scm_t_port_type *guile_port_t;
  static guile_port_t type_;

  const char *data_;
  ssize_t pos_;
  ssize_t len_;

  size_t read (SCM dest, size_t dest_off, size_t n)
  {
    if (pos_ >= len_)
      return 0;

    size_t left = static_cast<size_t> (len_ - pos_);
    if (n > left)
      n = left;

    memcpy (SCM_BYTEVECTOR_CONTENTS (dest) + dest_off, data_ + pos_, n);
    pos_ += n;
    return n;
  }

  static size_t read_scm (SCM port, SCM dest, size_t dest_off, size_t n)
  {
    Overlay_string_port *p = (Overlay_string_port *) SCM_STREAM (port);
    return p->read (dest, dest_off, n);
  }

  guile_off_t seek (guile_off_t off, int whence)
  {
    ssize_t base = 0;
    switch (whence)
      {
      case SEEK_CUR:
        base = pos_;
        break;
      case SEEK_SET:
        base = 0;
        break;
      case SEEK_END:
        base = len_;
        break;
      default:
        abort ();
      }

    ssize_t newpos = base + off;
    if (newpos >= 0 && newpos <= len_)
      pos_ = newpos;
    else
      scm_out_of_range ("Overlay_string_port::seek", scm_from_ssize_t (off));
    return pos_;
  }

  static guile_off_t seek_scm (SCM port, guile_off_t offset, int whence)
  {
    Overlay_string_port *p = (Overlay_string_port *) SCM_STREAM (port);
    return p->seek (offset, whence);
  }

public:
  SCM as_port ()
  {
    SCM encoding = ly_symbol2scm ("UTF-8");
    return scm_c_make_port_with_encoding (type_, SCM_RDNG, encoding,
                                          ly_symbol2scm ("error"),
                                          reinterpret_cast<scm_t_bits> (this));
  }

public:
  Overlay_string_port (const char *data, ssize_t len)
    : data_ (data),
      pos_ (0),
      len_ (len)
  {
  }

  static void init ()
  {
    // TODO: GUILE should take const char * for the name.
    char *name = const_cast<char *> ("Overlay_string_port");
    type_ = scm_make_port_type (name, &read_scm, NULL);
    scm_set_port_seek (type_, &seek_scm);
  }
};

#endif /* OVERLAY_STRING_PORT_HH */
