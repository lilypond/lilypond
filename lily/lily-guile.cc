/*
  lily-guile.cc -- implement assorted guile functions

  source file of the GNU LilyPond music typesetter

  (c) 1998--2004 Jan Nieuwenhuizen <janneke@gnu.org>
                 Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/

#include "lily-guile.hh"

#include <stdio.h>
#include <stdlib.h>
#include <math.h>   /* isinf */
#include <string.h> /* strdup, strchr */
#include <ctype.h>

/* source-file.hh includes cmath

   1.  No it doesn't
   2.  If it did, move this to sources.hh
   3.  Do not change the code when ! __APPLE__
   
   which undefines isinf and isnan

#ifdef __APPLE__
inline int my_isinf (Real r) { return isinf (r); }
inline int my_isnan (Real r) { return isnan (r); }
#define isinf my_isinf
#define isnan my_isnan
#endif
*/


#include "config.hh"

#include "lily-proto.hh"

#include "dimensions.hh"
#include "direction.hh"
#include "file-path.hh"
#include "international.hh"
#include "interval.hh"
#include "libc-extension.hh"
#include "main.hh"
#include "offset.hh"
#include "pitch.hh"
#include "source-file.hh"
#include "version.hh"
#include "warn.hh"

// #define TEST_GC

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

String
ly_symbol2string (SCM s)
{
  /*
    Ugh. this is not very efficient.
   */
  SCM str = scm_symbol_to_string (s);
  return ly_scm2string (str);
}

String
gulp_file_to_string (String fn)
{
  String s = global_path.find (fn);
  if (s == "")
    {
      String e = _f ("can't find file: `%s'", fn);
      e += " ";
      e += _f ("(load path: `%s')", global_path.to_string ());
      error (e);
    }
  else if (verbose_global_b)
    progress_indication ("[" + s);

  int n;
  char * str = gulp_file (s, &n);
  String result (str);
  delete[] str;
  
  if (verbose_global_b)
    progress_indication ("]");

  return result;
}

LY_DEFINE (ly_gulp_file, "ly:gulp-file",
	   1, 0, 0, (SCM name),
	   "Read the file @var{name}, and return its contents in a string.  "
	   "The file is looked up using the search path.")
{
  SCM_ASSERT_TYPE (scm_is_string (name), name, SCM_ARG1, __FUNCTION__, "string");
  return scm_makfrom0str (gulp_file_to_string (ly_scm2string (name)).to_str0 ());
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

String
ly_scm2string (SCM str)
{
  return String ((Byte*)scm_i_string_chars (str),
		 (int) scm_i_string_length (str));
}

char *
ly_scm2newstr (SCM str, size_t *lenp)
{
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "string");

  size_t len = SCM_STRING_LENGTH (str);
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
  
LY_DEFINE (ly_warn, "ly:warn",
	   1, 0, 1, (SCM str, SCM rest),
	   "Scheme callable function to issue the warning @code{msg}. "
	   "The message is formatted with @code{format} and @code{rest}.")
{
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "string");
  progress_indication ("\n");

  str = scm_simple_format (SCM_BOOL_F, str, rest);
  warning ("lily-guile: " + ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_programming_error, "ly:programming-error",
	   1, 0, 1, (SCM str, SCM rest),
	   "Scheme callable function to issue the warning @code{msg}. "
	   "The message is formatted with @code{format} and @code{rest}.")
{
  SCM_ASSERT_TYPE (scm_is_string (str), str, SCM_ARG1, __FUNCTION__, "string");
  progress_indication ("\n");

  str = scm_simple_format (SCM_BOOL_F, str, rest);
  programming_error (ly_scm2string (str));
  return SCM_UNSPECIFIED;
}

LY_DEFINE (ly_dir_p, "ly:dir?",
	   1, 0, 0, (SCM s),
	  "type predicate. A direction is @code{-1}, @code{0} or "
	   "@code{1}, where @code{-1} represents "
	  "left or down and @code{1} represents right or up.")
{
  if (scm_is_number (s))
    {
      int i = scm_to_int (s);
      return (i>= -1 && i <= 1)  ? SCM_BOOL_T : SCM_BOOL_F; 
    }
  return SCM_BOOL_F;
}

bool
is_number_pair (SCM p)
{
  return scm_is_pair (p)
    && scm_is_number (scm_car (p)) && scm_is_number (scm_cdr (p));
}

typedef void (*Void_fptr) ();
Array<Void_fptr> *scm_init_funcs_;

void add_scm_init_func (void (*f) ())
{
  if (!scm_init_funcs_)
    scm_init_funcs_ = new Array<Void_fptr>;

  scm_init_funcs_->push (f);
}

void
ly_init_ly_module (void *)
{
  for (int i=scm_init_funcs_->size () ; i--;)
    (scm_init_funcs_->elem (i)) ();

  if (verbose_global_b)
    progress_indication ("\n");
  
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
      return i>= -1 && i <= 1; 
    }
  return false;
}

LY_DEFINE(ly_assoc_get, "ly:assoc-get",
	  2, 1, 0,
	  (SCM key, SCM alist, SCM default_value),
	  "Return value if KEY in ALIST, else DEFAULT-VALUE (or #f if not specified).")
{
  SCM handle = scm_assoc (key, alist);

  if (default_value == SCM_UNDEFINED)
    default_value = SCM_BOOL_F;
  
  if (scm_is_pair (handle))
    return scm_cdr (handle);
  else
    return default_value;
}

bool
is_axis (SCM s)
{
  if (scm_is_number (s))
    {
      int i = scm_to_int (s);
      return i== 0 || i == 1;
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
  return scm_cons (scm_make_real (i[LEFT]), scm_make_real (i[RIGHT]));
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
  return scm_cons (scm_make_real (o[X_AXIS]), scm_make_real (o[Y_AXIS]));
}

Offset
ly_scm2offset (SCM s)
{
  return Offset (scm_to_double (scm_car (s)),
		 scm_to_double (scm_cdr (s)));
}

LY_DEFINE (ly_number2string, "ly:number->string",
	   1, 0, 0, (SCM s),
	   "Convert @var{num} to a string without generating many decimals.")
{
  SCM_ASSERT_TYPE (scm_is_number (s), s, SCM_ARG1, __FUNCTION__, "number");

  char str[400];			// ugh.

  if (scm_exact_p (s) == SCM_BOOL_F)
    {
      Real r (scm_to_double (s));

      if (isinf (r) || isnan (r))
	{
	  programming_error ("Infinity or NaN encountered while converting Real number; setting to zero.");
	  r = 0.0;
	}

      sprintf (str, "%08.4f", r);
    }
  else
    sprintf (str, "%d", scm_to_int (s));

  return scm_makfrom0str (str);
}



LY_DEFINE (ly_version,  "ly:version", 0, 0, 0, (),
	  "Return the current lilypond version as a list, e.g. @code{(1 3 127 uu1)}. ")
{
  char const* vs = "\'(" MAJOR_VERSION " " MINOR_VERSION " "  PATCH_LEVEL " " MY_PATCH_LEVEL ")" ;
  
  return scm_c_eval_string ((char*)vs);
}

LY_DEFINE (ly_unit,  "ly:unit", 0, 0, 0, (),
	  "Return the unit used for lengths as a string.")
{
  return scm_makfrom0str (INTERNAL_UNIT);
}



LY_DEFINE (ly_dimension_p,  "ly:dimension?", 1, 0, 0, (SCM d),
	  "Return @var{d} is a number. Used to distinguish length "
	  "variables from normal numbers.")
{
  return scm_number_p (d);
}

SCM
ly_deep_copy (SCM src)
{
  if (scm_is_pair (src))
    return scm_cons (ly_deep_copy (scm_car (src)), ly_deep_copy (scm_cdr (src)));
  else if (ly_c_vector_p (src))
    {
      int len = SCM_VECTOR_LENGTH (src);
      SCM nv = scm_c_make_vector (len, SCM_UNDEFINED);
      for (int i  =0 ; i < len ; i++)
	{
	  SCM si = scm_int2num (i);
	  scm_vector_set_x (nv, si, ly_deep_copy (scm_vector_ref (src, si))); 
	}
    }
  return src;
}




SCM
ly_assoc_chain (SCM key, SCM achain)
{
  if (scm_is_pair (achain))
    {
      SCM handle = scm_assoc (key, scm_car (achain));
      if (scm_is_pair (handle))
	return handle;
      else
	return ly_assoc_chain (key, scm_cdr (achain));
    }
  else
    return SCM_BOOL_F;
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
      if (scm_is_pair (trykey) && to_boolean (scm_equal_p (key, scm_cdr (trykey))))
	return scm_car (alist);
      else
	return ly_assoc_cdr (key, scm_cdr (alist));
    }
  return SCM_BOOL_F;
}

/* LST has the form "sym1 sym2 sym3\nsym4\nsym5"
   i.e. \n and ' ' can be used interchangeably as separators.  */
SCM
parse_symbol_list (char const *lst)
{
  char *s = strdup (lst);
  char *orig = s;
  SCM create_list = SCM_EOL;

  char * e = s + strlen (s) - 1;
  while (e >= s && isspace (*e))
    *e-- = 0;

  for (char * p = s; *p; p++)
    if (*p == '\n')
      *p = ' ';
  
  if (!s[0])
    s = 0;
  
  while (s)
    {
      char *next = strchr (s, ' ');
      if (next)
	*next++ = 0;

      create_list = scm_cons (ly_symbol2scm (s), create_list);
      s = next;
    }

  free (orig);
  return create_list;
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

String
print_scm_val (SCM val)
{
  String realval = ly_scm2string (ly_write2scm (val));
  if (realval.length () > 200)
    realval = realval.left_string (100)
      + "\n :\n :\n"
      + realval.right_string (100);
  return realval;	 
}

bool
type_check_assignment (SCM sym, SCM val,  SCM type_symbol) 
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

  if (type != SCM_EOL && !ly_c_procedure_p (type))
      {
	warning (_f ("Can't find property type-check for `%s' (%s).",
		     ly_symbol2string (sym).to_str0 (),
		     ly_symbol2string (type_symbol).to_str0 ())
		 + "  " + _ ("Perhaps you made a typing error?"));

	/* Be strict when being anal :) */
	if (internal_type_checking_global_b)
	  abort ();
	
	warning (_ ("Doing assignment anyway."));
      }
  else
    {
      if (val != SCM_EOL
	  && ly_c_procedure_p (type)
	  && scm_call_1 (type, val) == SCM_BOOL_F)
	{
	  SCM errport = scm_current_error_port ();
	  ok = false;
	  SCM typefunc = ly_scheme_function ("type-name");
	  SCM type_name = scm_call_1 (typefunc, type);

	 
	  scm_puts (_f ("Type check for `%s' failed; value `%s' must be of type `%s'",
			ly_symbol2string (sym).to_str0 (),
			print_scm_val (val),
			ly_scm2string (type_name).to_str0 ()).to_str0 (),
		    errport);
	  scm_puts ("\n", errport);		      
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
	  || !ly_c_equal_p (scm_car (i), scm_cadr (i)))
	unique = scm_cons (scm_car (i), unique);
    }
  return scm_reverse_x (unique, SCM_EOL);
}


static int
scm_default_compare (void const *a, void const *b)
{
  SCM pa = *(SCM*) a;
  SCM pb = *(SCM*) b;
  if (pa == pb)
    return 0;
  return pa < pb ? -1 : 1;
}

/*  Modify LST in place: qsort it.  */
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
      if (ly_c_equal_p (i, s))
	break;
      before = scm_cons (i, before);
    }
  return scm_cons ( scm_reverse_x (before, SCM_EOL),  after);
  
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
  for (; scm_is_pair (s); s =scm_cdr (s))
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

Interval
robust_scm2interval (SCM k, Drul_array<Real> v)
{
  Interval i;
  i[LEFT]= v[LEFT];
  i[RIGHT]= v[RIGHT];
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

Offset
robust_scm2offset (SCM k, Offset o)
{
  if (is_number_pair (k))
    o = ly_scm2offset (k);
  return o;
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

#if 1
/*
  Debugging mem leaks:
 */
LY_DEFINE (ly_protects, "ly:protects",
	   0, 0, 0, (),
	  "Return hash of protected objects.")
{
  return scm_protects;
}
#endif


#if HAVE_PANGO_FC_FONT_MAP_ADD_DECODER_FIND_FUNC

#include "pangofc-afm-decoder.hh"

LY_DEFINE (ly_pango_add_afm_decoder, "ly:pango-add-afm-decoder",
	   1, 0, 0, (SCM font_family),
	   "Add pango afm decoder for FONT-FAMILY.")
{
  SCM_ASSERT_TYPE (scm_is_string (font_family), font_family, SCM_ARG1,
		   __FUNCTION__, "font_family");
  pango_fc_afm_add_decoder (ly_scm2newstr (font_family, 0));
  return SCM_UNSPECIFIED;
}

#endif

LY_DEFINE (ly_gettext, "ly:gettext",
	   1, 0, 0, (SCM string),
	   "Gettext wrapper.")
{
  SCM_ASSERT_TYPE (scm_is_string (string), string, SCM_ARG1,
		   __FUNCTION__, "string");
  return scm_makfrom0str (_ (scm_i_string_chars (string)).to_str0 ());
}

