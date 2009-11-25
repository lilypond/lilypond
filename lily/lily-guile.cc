/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1998--2009 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include <cstdio>
#include <cstdlib>
#include <cstring> /* strdup, strchr */
#include <cctype>

using namespace std;

#include "dimensions.hh"
#include "direction.hh"
#include "file-path.hh"
#include "international.hh"
#include "libc-extension.hh"
#include "main.hh"
#include "misc.hh"
#include "offset.hh"
#include "pitch.hh"
#include "string-convert.hh"
#include "source-file.hh"
#include "version.hh"
#include "warn.hh"


/*
  symbols/strings.
 */
string
ly_scm_write_string (SCM s)
{
  SCM port = scm_mkstrport (SCM_INUM0,
			    scm_make_string (SCM_INUM0, SCM_UNDEFINED),
			    SCM_OPN | SCM_WRTNG,
			    "ly_write2string");
  //  SCM write = scm_eval_3 (ly_symbol2scm ("write"), s, SCM_EOL);
  SCM write = scm_primitive_eval (ly_symbol2scm ("write"));

  // scm_apply (write, port, SCM_EOL);
  scm_call_2 (write, s, port);
  return ly_scm2string (scm_strport_to_string (port));
}

SCM
ly_quote_scm (SCM s)
{
  return scm_list_n (ly_symbol2scm ("quote"), s, SCM_UNDEFINED);
}

string
ly_symbol2string (SCM s)
{
  /*
    Ugh. this is not very efficient.
  */
  SCM str = scm_symbol_to_string (s);
  return ly_scm2string (str);
}

string
gulp_file_to_string (string fn, bool must_exist, int size)
{
  string s = global_path.find (fn);
  if (s == "")
    {
      if (must_exist)
	{
	  string e = _f ("cannot find file: `%s'", fn);
	  e += " ";
	  e += _f ("(load path: `%s')", global_path.to_string ());
	  error (e);
	  /* unreachable */
	}
      return s;
    }

  if (be_verbose_global)
    progress_indication ("[" + s);

  vector<char> chars = gulp_file (s, size);
  string result (&chars[0], chars.size ());

  if (be_verbose_global)
    progress_indication ("]\n");

  return result;
}

extern "C" {
  // maybe gdb 5.0 becomes quicker if it doesn't do fancy C++ typing?
  void
  ly_display_scm (SCM s)
  {
    scm_display (s, scm_current_output_port ());
    scm_newline (scm_current_output_port ());
  }
};

/*
  STRINGS
 */
string
ly_scm2string (SCM str)
{
  assert (scm_is_string (str));
  string result;
  size_t len = scm_c_string_length (str);
  if (len) {
    result.resize(len);
    scm_to_locale_stringbuf(str, &result.at(0), len);
  }
  return result;
}

SCM
ly_string2scm (string const &str)
{
  return scm_from_locale_stringn (str.c_str (),
				  str.length ());
}


char *
ly_scm2newstr (SCM str, size_t *lenp)
{
  char* p = scm_to_locale_stringn(str, lenp);
  return p;
}

/*
  PAIRS
*/
SCM 
index_get_cell (SCM s, Direction d)
{
  assert (d);
  return (d == LEFT) ? scm_car (s) : scm_cdr (s);
}

SCM
index_set_cell (SCM s, Direction d, SCM v)
{
  if (d == LEFT)
    scm_set_car_x (s, v);
  else if (d == RIGHT)
    scm_set_cdr_x (s, v);
  return s;
}

bool
is_number_pair (SCM p)
{
  return scm_is_pair (p)
    && scm_is_number (scm_car (p)) && scm_is_number (scm_cdr (p));
}


unsigned int
ly_scm_hash (SCM s)
{
  return scm_ihashv (s, ~1u);
}

bool
is_axis (SCM s)
{
  if (scm_is_number (s))
    {
      int i = scm_to_int (s);
      return i == 0 || i == 1;
    }
  return false;
}

bool
to_boolean (SCM s)
{
  return scm_is_bool (s) && ly_scm2bool (s);
}

/*
  DIRECTIONS
 */
Direction
to_dir (SCM s)
{
  return scm_is_integer (s) ? (Direction) scm_to_int (s) : CENTER;
}

Direction
robust_scm2dir (SCM d, Direction def)
{
  if (is_direction (d))
    def = to_dir (d);
  return def;
}

bool
is_direction (SCM s)
{
  if (scm_is_number (s))
    {
      int i = scm_to_int (s);
      return i >= -1 && i <= 1;
    }
  return false;
}

/*
  INTERVALS
 */
Interval
ly_scm2interval (SCM p)
{
  return Interval (scm_to_double (scm_car (p)), scm_to_double (scm_cdr (p)));
}

Drul_array<Real>
ly_scm2realdrul (SCM p)
{
  return Drul_array<Real> (scm_to_double (scm_car (p)),
			   scm_to_double (scm_cdr (p)));
}

SCM
ly_interval2scm (Drul_array<Real> i)
{
  return scm_cons (scm_from_double (i[LEFT]), scm_from_double (i[RIGHT]));
}


Interval
robust_scm2interval (SCM k, Drul_array<Real> v)
{
  Interval i;
  i[LEFT] = v[LEFT];
  i[RIGHT] = v[RIGHT];
  if (is_number_pair (k))
    i = ly_scm2interval (k);
  return i;
}

Drul_array<Real>
robust_scm2drul (SCM k, Drul_array<Real> v)
{
  if (is_number_pair (k))
    v = ly_scm2interval (k);
  return v;
}

Drul_array<bool>
robust_scm2booldrul (SCM k, Drul_array<bool> def)
{
  if (scm_is_pair (k))
    {
      def[LEFT] = to_boolean (scm_car (k));
      def[RIGHT] = to_boolean (scm_cdr (k));
    }
  return def;
}

/*
  OFFSET
*/
SCM
ly_offset2scm (Offset o)
{
  return scm_cons (scm_from_double (o[X_AXIS]), scm_from_double (o[Y_AXIS]));
}

Offset
ly_scm2offset (SCM s)
{
  return Offset (scm_to_double (scm_car (s)),
		 scm_to_double (scm_cdr (s)));
}

Offset
robust_scm2offset (SCM k, Offset o)
{
  if (is_number_pair (k))
    o = ly_scm2offset (k);
  return o;
}
SCM
ly_offsets2scm (vector<Offset> os)
{
  SCM l = SCM_EOL;
  SCM *tail = &l;
  for (vsize i = 0; i < os.size (); i++)
    {
      *tail = scm_cons (ly_offset2scm (os[i]), SCM_EOL);
      tail = SCM_CDRLOC (*tail);
    }
  return l;
}

vector<Offset>
ly_scm2offsets (SCM s)
{
  vector<Offset> os;
  for (; scm_is_pair (s); s = scm_cdr (s))
    os.push_back (ly_scm2offset (scm_car (s)));
  return os;
}




/*
  ALIST
*/

bool
alist_equal_p (SCM a, SCM b)
{
  for (SCM s = a;
       scm_is_pair (s); s = scm_cdr (s))
    {
      SCM key = scm_caar (s);
      SCM val = scm_cdar (s);
      SCM l = scm_assoc (key, b);

      if (l == SCM_BOOL_F
	  || !ly_is_equal (scm_cdr (l), val))

	return false;
    }
  return true;
}

SCM
ly_alist_vals (SCM alist)
{
  SCM x = SCM_EOL;
  for (SCM p = alist; scm_is_pair (p); p = scm_cdr (p))
    x = scm_cons (scm_cdar (p), x);
  return x;
}

/*
  LISTS
 */

/* Return I-th element, or last elt L. If I < 0, then we take the first
   element.

   PRE: length (L) > 0  */
SCM
robust_list_ref (int i, SCM l)
{
  while (i-- > 0 && scm_is_pair (scm_cdr (l)))
    l = scm_cdr (l);
  return scm_car (l);
}


SCM
ly_deep_copy (SCM src)
{
  if (scm_is_pair (src))
    return scm_cons (ly_deep_copy (scm_car (src)), ly_deep_copy (scm_cdr (src)));
  else if (scm_is_vector (src))
    {
      int len = scm_c_vector_length (src);
      SCM nv = scm_c_make_vector (len, SCM_UNDEFINED);
      for (int i = 0;i < len; i++)
	{
	  SCM si = scm_from_int (i);
	  scm_vector_set_x (nv, si, ly_deep_copy (scm_vector_ref (src, si)));
	}
    }
  return src;
}

string
print_scm_val (SCM val)
{
  string realval = ly_scm_write_string (val);
  if (realval.length () > 200)
    realval = realval.substr (0, 100)
      + "\n :\n :\n"
      + realval.substr (realval.length () - 100);
  return realval;
}

bool
type_check_assignment (SCM sym, SCM val, SCM type_symbol)
{
  bool ok = true;

  /*
    Always succeeds.


    TODO: should remove #f from allowed vals?
  */
  if (val == SCM_EOL || val == SCM_BOOL_F)
    return ok;

  if (!scm_is_symbol (sym))
#if 0
    return false;
#else
  /*
    This is used for autoBeamSettings.

    TODO: deprecate the use of \override and \revert for
    autoBeamSettings?

    or use a symbol autoBeamSettingS?
  */
  return true;
#endif

  SCM type = scm_object_property (sym, type_symbol);

  if (type != SCM_EOL && !ly_is_procedure (type))
    {
      warning (_f ("cannot find property type-check for `%s' (%s).",
		   ly_symbol2string (sym).c_str (),
		   ly_symbol2string (type_symbol).c_str ())
	       + "  " + _ ("perhaps a typing error?"));

      /* Be strict when being anal :) */
      if (do_internal_type_checking_global)
	scm_throw (ly_symbol2scm ("ly-file-failed"), scm_list_3 (ly_symbol2scm ("typecheck"),
								 sym, val));

      warning (_ ("doing assignment anyway"));
    }
  else
    {
      if (val != SCM_EOL
	  && ly_is_procedure (type)
	  && scm_call_1 (type, val) == SCM_BOOL_F)
	{
	  ok = false;
	  SCM typefunc = ly_lily_module_constant ("type-name");
	  SCM type_name = scm_call_1 (typefunc, type);

	  warning (_f ("type check for `%s' failed; value `%s' must be of type `%s'",
		       ly_symbol2string (sym).c_str (),
		       print_scm_val (val),
		       ly_scm2string (type_name).c_str ()));
	  progress_indication ("\n");
	}
    }
  return ok;
}

/* some SCM abbrevs

zijn deze nou handig?
zijn ze er al in scheme, maar heten ze anders? */

/* Remove doubles from (sorted) list */
SCM
ly_unique (SCM list)
{
  SCM unique = SCM_EOL;
  for (SCM i = list; scm_is_pair (i); i = scm_cdr (i))
    {
      if (!scm_is_pair (scm_cdr (i))
	  || !ly_is_equal (scm_car (i), scm_cadr (i)))
	unique = scm_cons (scm_car (i), unique);
    }
  return scm_reverse_x (unique, SCM_EOL);
}


/* Split list at member s, removing s.
   Return (BEFORE . AFTER)  */
SCM
ly_split_list (SCM s, SCM list)
{
  SCM before = SCM_EOL;
  SCM after = list;
  for (; scm_is_pair (after);)
    {
      SCM i = scm_car (after);
      after = scm_cdr (after);
      if (ly_is_equal (i, s))
	break;
      before = scm_cons (i, before);
    }
  return scm_cons (scm_reverse_x (before, SCM_EOL), after);
}

void
taint (SCM *)
{
  /*
    nop.
  */
}

/*
  display stuff without using stack
*/
SCM
display_list (SCM s)
{
  SCM p = scm_current_output_port ();

  scm_puts ("(", p);
  for (; scm_is_pair (s); s = scm_cdr (s))
    {
      scm_display (scm_car (s), p);
      scm_puts (" ", p);
    }
  scm_puts (")", p);
  return SCM_UNSPECIFIED;
}

Slice
int_list_to_slice (SCM l)
{
  Slice s;
  s.set_empty ();
  for (; scm_is_pair (l); l = scm_cdr (l))
    if (scm_is_number (scm_car (l)))
      s.add_point (scm_to_int (scm_car (l)));
  return s;
}

Real
robust_scm2double (SCM k, double x)
{
  if (scm_is_number (k))
    x = scm_to_double (k);
  return x;
}


string
robust_scm2string (SCM k, string s)
{
  if (scm_is_string (k))
    s = ly_scm2string (k);
  return s;
}

int
robust_scm2int (SCM k, int o)
{
  if (scm_integer_p (k) == SCM_BOOL_T)
    o = scm_to_int (k);
  return o;
}


SCM
ly_rational2scm (Rational r)
{
  return scm_divide (scm_from_int64 (r.numerator ()),
		     scm_from_int64 (r.denominator ()));
}


Rational
ly_scm2rational (SCM r)
{
  return Rational (scm_to_int64 (scm_numerator (r)),
		   scm_to_int64 (scm_denominator (r)));
}

Rational
robust_scm2rational (SCM n, Rational rat)
{
  if (ly_is_fraction (n))
    return ly_scm2rational (n);
  else
    return rat;
}

SCM
alist_to_hashq (SCM alist)
{
  int i = scm_ilength (alist);
  if (i < 0)
    return scm_c_make_hash_table (0);

  SCM tab = scm_c_make_hash_table (i);
  for (SCM s = alist; scm_is_pair (s); s = scm_cdr (s))
    {
      SCM pt = scm_cdar (s);
      scm_hashq_set_x (tab, scm_caar (s), pt);
    }
  return tab;
}

SCM
ly_hash2alist (SCM tab)
{
  SCM func = ly_lily_module_constant ("hash-table->alist");
  return scm_call_1 (func, tab);
}


/*
  C++ interfacing.
 */

string
mangle_cxx_identifier (string cxx_id)
{
  if (cxx_id.substr (0, 3) == "ly_")
    cxx_id = cxx_id.replace (0, 3, "ly:");
  else
    {
      cxx_id = String_convert::to_lower (cxx_id);
      cxx_id = "ly:" + cxx_id;
    }
  if (cxx_id.substr (cxx_id.length () - 2) == "_p")
    cxx_id = cxx_id.replace (cxx_id.length () - 2, 2, "?");
  else if (cxx_id.substr (cxx_id.length () - 2) == "_x")
    cxx_id = cxx_id.replace (cxx_id.length () - 2, 2, "!");

  replace_all (&cxx_id, "_less?", "<?");
  replace_all (&cxx_id, "_2_", "->");
  replace_all (&cxx_id, "__", "::");
  replace_all (&cxx_id, '_', '-');


  return cxx_id;
}



SCM
ly_string_array_to_scm (vector<string> a)
{
  SCM s = SCM_EOL;
  for (vsize i = a.size (); i ; i--)
    s = scm_cons (ly_symbol2scm (a[i - 1].c_str ()), s);
  return s;
}

/* SYMBOLS is a whitespace separated list.  */
SCM
parse_symbol_list (char const *symbols)
{
  while (isspace (*symbols))
    *symbols++;
  string s = symbols;
  replace_all (&s, '\n', ' ');
  replace_all (&s, '\t', ' ');
  replace_all (&s, "  ", " ");
  return ly_string_array_to_scm (string_split (s, ' '));
}

/* GDB debugging. */
struct ly_t_double_cell
{
  SCM a;
  SCM b;
  SCM c;
  SCM d;
};

/* inserts at front, removing duplicates */
SCM ly_assoc_prepend_x (SCM alist, SCM key, SCM val)
{
  return scm_acons (key, val, scm_assoc_remove_x (alist, key));
}

