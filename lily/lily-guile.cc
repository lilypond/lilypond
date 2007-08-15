/*
  lily-guile.cc -- implement assorted SCM interface functions

  source file of the GNU LilyPond music typesetter

  (c) 1998--2007 Jan Nieuwenhuizen <janneke@gnu.org>
  Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#include "lily-guile.hh"

#include <cstdio>
#include <cstdlib>
#include <cstring> /* strdup, strchr */
#include <cctype>

using namespace std;

#include "config.hh"

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

// #define TEST_GC

SCM
ly_to_symbol (SCM scm)
{
  return scm_string_to_symbol (ly_to_string (scm));
}

SCM
ly_to_string (SCM scm)
{
  return scm_call_3 (ly_lily_module_constant ("format"), SCM_BOOL_F,

		     scm_makfrom0str ("~S"), scm);
}

SCM
ly_last (SCM list)
{
  return scm_car (scm_last_pair (list));
}

SCM
ly_write2scm (SCM s)
{
  SCM port = scm_mkstrport (SCM_INUM0,
			    scm_make_string (SCM_INUM0, SCM_UNDEFINED),
			    SCM_OPN | SCM_WRTNG,
			    "ly_write2string");
  //  SCM write = scm_eval_3 (ly_symbol2scm ("write"), s, SCM_EOL);
  SCM write = scm_primitive_eval (ly_symbol2scm ("write"));

  // scm_apply (write, port, SCM_EOL);
  scm_call_2 (write, s, port);
  return scm_strport_to_string (port);
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
	  string e = _f ("can't find file: `%s'", fn);
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
    progress_indication ("]");

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

string
ly_scm2string (SCM str)
{
  assert (scm_is_string (str));
  return string (scm_i_string_chars (str),
		 (int) scm_i_string_length (str));
}

SCM
ly_string2scm (string const &str)
{
  return scm_from_locale_stringn (str.c_str(),
				  str.length ());
}

char *
ly_scm2newstr (SCM str, size_t *lenp)
{
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "string");

  size_t len = scm_i_string_length (str);
  if (char *new_str = (char *) malloc ((len + 1) * sizeof (char)))
    {
      memcpy (new_str, scm_i_string_chars (str), len);
      new_str[len] = '\0';

      if (lenp)
	*lenp = len;

      return new_str;
    }
  return 0;
}

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

typedef void (*Void_fptr) ();
vector<Void_fptr> *scm_init_funcs_;

void add_scm_init_func (void (*f) ())
{
  if (!scm_init_funcs_)
    scm_init_funcs_ = new vector<Void_fptr>;

  scm_init_funcs_->push_back (f);
}

void
ly_init_ly_module (void *)
{
  for (vsize i = scm_init_funcs_->size (); i--;)
    (scm_init_funcs_->at (i)) ();

  if (be_verbose_global)
    {
      progress_indication ("[");
      scm_display (scm_c_eval_string ("(%search-load-path \"lily.scm\")"),
		   scm_current_error_port ());
      progress_indication ("]\n");
    }

  scm_primitive_load_path (scm_makfrom0str ("lily.scm"));
}

SCM global_lily_module;

void
ly_c_init_guile ()
{
  global_lily_module = scm_c_define_module ("lily", ly_init_ly_module, 0);
  scm_c_use_module ("lily");
}

unsigned int
ly_scm_hash (SCM s)
{
  return scm_ihashv (s, ~1u);
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

Direction
to_dir (SCM s)
{
  return scm_is_integer (s) ? (Direction) scm_to_int (s) : CENTER;
}

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

bool
to_boolean (SCM s)
{
  return scm_is_bool (s) && ly_scm2bool (s);
}

/* Appendable list L: the cdr contains the list, the car the last cons
   in the list.  */
SCM
appendable_list ()
{
  SCM s = scm_cons (SCM_EOL, SCM_EOL);
  scm_set_car_x (s, s);

  return s;
}

void
appendable_list_append (SCM l, SCM elt)
{
  SCM newcons = scm_cons (elt, SCM_EOL);

  scm_set_cdr_x (scm_car (l), newcons);
  scm_set_car_x (l, newcons);
}

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

/* looks the key up in the cdrs of the alist-keys
   - ignoring the car and ignoring non-pair keys.
   Returns first match found, i.e.

   alist = ((1 . 10)
   ((1 . 2) . 11)
   ((2 . 1) . 12)
   ((3 . 0) . 13)
   ((4 . 1) . 14) )

   I would like (ly_assoc_cdr 1) to return 12 - because it's the first
   element with the cdr of the key = 1.  In other words (alloc_cdr key)
   corresponds to call

   (alloc (anything . key))
*/
SCM
ly_assoc_cdr (SCM key, SCM alist)
{
  if (scm_is_pair (alist))
    {
      SCM trykey = scm_caar (alist);
      if (scm_is_pair (trykey)
	  && to_boolean (scm_equal_p (key, scm_cdr (trykey))))
	return scm_car (alist);
      return ly_assoc_cdr (key, scm_cdr (alist));
    }
  return SCM_BOOL_F;
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
  replace_all (s, '\n', ' ');
  replace_all (s, '\t', ' ');
  return ly_string_array_to_scm (string_split (s, ' '));
}

SCM
ly_truncate_list (int k, SCM lst)
{
  if (k == 0)
    lst = SCM_EOL;
  else
    {
      SCM s = lst;
      k--;
      for (; scm_is_pair (s) && k--; s = scm_cdr (s))
	;

      if (scm_is_pair (s))
	scm_set_cdr_x (s, SCM_EOL);
    }
  return lst;
}

string
print_scm_val (SCM val)
{
  string realval = ly_scm2string (ly_write2scm (val));
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
      warning (_f ("can't find property type-check for `%s' (%s).",
		   ly_symbol2string (sym).c_str (),
		   ly_symbol2string (type_symbol).c_str ())
	       + "  " + _ ("perhaps a typing error?"));

      /* Be strict when being anal :) */
      if (do_internal_type_checking_global)
	abort ();

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

static int
scm_default_compare (void const *a, void const *b)
{
  SCM pa = *(SCM *) a;
  SCM pb = *(SCM *) b;
  if (pa == pb)
    return 0;
  return pa < pb ? -1 : 1;
}

/*  Modify LST in place: qsort it.

FIXME: unused, junk? */
SCM
ly_list_qsort_uniq_x (SCM lst)
{
  int len = scm_ilength (lst);
  SCM *arr = new SCM[len];
  int k = 0;
  for (SCM s = lst; SCM_NNULLP (s); s = scm_cdr (s))
    arr[k++] = scm_car (s);

  assert (k == len);
  qsort (arr, len, sizeof (SCM), &scm_default_compare);

  SCM *tail = &lst;
  for (int i = 0; i < len; i++)
    if (!i || arr[i] != arr[i - 1])
      {
	SCM_SETCAR (*tail, arr[i]);
	tail = SCM_CDRLOC (*tail);
      }

  *tail = SCM_EOL;
  delete[] arr;

  return lst;
}

/* tail add */
SCM
ly_snoc (SCM s, SCM list)
{
  return ly_append2 (list, scm_list_n (s, SCM_UNDEFINED));
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

Real
robust_scm2double (SCM k, double x)
{
  if (scm_is_number (k))
    x = scm_to_double (k);
  return x;
}

Direction
robust_scm2dir (SCM d, Direction def)
{
  if (is_direction (d))
    def = to_dir (d);
  return def;
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

Offset
robust_scm2offset (SCM k, Offset o)
{
  if (is_number_pair (k))
    o = ly_scm2offset (k);
  return o;
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

SCM
ly_hash2alist (SCM tab)
{
  SCM func = ly_lily_module_constant ("hash-table->alist");
  return scm_call_1 (func, tab);
}

int
procedure_arity (SCM proc)
{
  assert (ly_is_procedure (proc));
  SCM arity = scm_procedure_property (proc,
				      ly_symbol2scm ("arity"));

  SCM fixed = scm_car (arity);
  return scm_to_int (fixed);
}

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
    cxx_id = cxx_id.replace (cxx_id.length () - 2, 1, "?");
  else if (cxx_id.substr (cxx_id.length () - 2) == "_x")
    cxx_id = cxx_id.replace (cxx_id.length () - 2, 1, "!");

  cxx_id = replace_all (cxx_id, '_', '-');
  return cxx_id;
}

