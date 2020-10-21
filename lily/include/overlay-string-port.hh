/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2020 Han-Wen Nienhuys <hanwen@xs4all.nl>

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
#if GUILEV2
  typedef scm_t_off guile_off_t;
  typedef scm_t_port_type *guile_port_t;
#else
  typedef scm_t_bits guile_port_t;
  typedef off_t guile_off_t;
#endif
  static guile_port_t type_;

  const char *data_;
  size_t pos_;
  ssize_t len_;

#if GUILEV2
  size_t read (SCM dest, size_t dest_off, size_t n)
  {
    if (pos_ >= len_)
      return 0;

    if (n > len_ - pos_)
      n = len_ - pos_;

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
                                          (scm_t_bits) this);
  }
#else
  static int fill_buffer_scm (SCM port)
  {
    scm_t_port *pt = SCM_PTAB_ENTRY (port);
    if (pt->read_pos >= pt->read_end)
      return EOF;
    else
      return scm_return_first_int (*pt->read_pos, port);
  }

  static guile_off_t seek_scm (SCM port, guile_off_t /* offset */, int whence)
  {
    assert (whence == SEEK_CUR);
    scm_t_port *pt = SCM_PTAB_ENTRY (port);
    if (pt->read_buf == pt->putback_buf)
      {
        return pt->saved_read_pos - pt->saved_read_buf
               - (pt->read_end - pt->read_pos);
      }
    return pt->read_pos - pt->read_buf;
  }

public:
  SCM as_port ()
  {
    // Avoid compiler-warning because of unused private field.
    (void) pos_;

    // strports.c in GUILE 1.8 has ominous comments about locking to
    // protect the global port table. We assume LilyPond doesn't use
    // threads; we have no access to the lock anyway.
    SCM port = scm_new_port_table_entry (type_);
    SCM_SET_CELL_TYPE (port, type_ | SCM_RDNG | SCM_OPN);
    SCM_SETSTREAM (port, this);

    scm_t_port *pt = SCM_PTAB_ENTRY (port);

    pt->read_buf = (unsigned char *) data_;
    pt->read_pos = (const unsigned char *) data_;
    pt->read_buf_size = len_;
    pt->read_end = pt->read_buf + len_;
    return port;
  }
#endif

public:
  Overlay_string_port (const char *data, ssize_t len) : data_ (data),
    pos_ (0), len_ (len)
  {
  }

  static void init ()
  {
    // TODO: GUILE should take const char * for the name.
    char *name = (char *) "Overlay_string_port";
#if GUILEV2
    type_ = scm_make_port_type (name, &read_scm, NULL);
#else
    type_ = scm_make_port_type (name, &fill_buffer_scm, NULL);
#endif
    scm_set_port_seek (type_, &seek_scm);
  }
};
