/*
  lily-guile.cc -- implement assorted guile functions

  source file of the GNU LilyPond music typesetter

  (c) 1998--2002 Jan Nieuwenhuizen <janneke@gnu.org>

  Han-Wen Nienhuys <hanwen@cs.uu.nl>
*/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>   /* isinf */
#include <string.h> /* strdup, strchr */
#include "libc-extension.hh"
#include "lily-guile.hh"
#include "main.hh"
#include "simple-file-storage.hh"
#include "file-path.hh"
#include "debug.hh"
#include "direction.hh"
#include "offset.hh"
#include "interval.hh"
#include "pitch.hh"

SCM
ly_last (SCM list)
{
  return ly_car (scm_last_pair (list));
}

SCM
ly_str02scm (char const*c)
{
  // this all really sucks, guile should take char const* arguments!
  return gh_str02scm ((char*)c);
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
  gh_call2 (write, s, port);
  return scm_strport_to_string (port);
}


/*
  Pass string to scm parser, evaluate one expression.
  Return result value and #chars read.

  Thanks to Gary Houston <ghouston@freewire.co.uk>

  Need guile-1.3.4 (>1.3 anyway) for ftell on str ports -- jcn
*/
SCM
ly_parse_scm (char const* s, int* n)
{
  SCM str = ly_str02scm (s);
  SCM port = scm_mkstrport (SCM_INUM0, str, SCM_OPN | SCM_RDNG,
                            "ly_eval_scm_0str");
  SCM from = scm_ftell (port);

  SCM form;
  SCM answer = SCM_UNSPECIFIED;

  /* Read expression from port */
  if (!SCM_EOF_OBJECT_P (form = scm_read (port)))
    answer = scm_primitive_eval (form);
 
  /*
   After parsing

 (begin (foo 1 2))

   all seems fine, but after parsing

 (foo 1 2)

   read_buf has been advanced to read_pos - 1,
   so that scm_ftell returns 1, instead of #parsed chars
   */
  
  /*
    urg: reset read_buf for scm_ftell
    shouldn't scm_read () do this for us?
  */
  scm_fill_input (port);
  SCM to = scm_ftell (port);
  *n = gh_scm2int (to) - gh_scm2int (from);

  /* Don't close the port here; if we re-enter this function via a
     continuation, then the next time we enter it, we'll get an error.
     It's a string port anyway, so there's no advantage to closing it
     early.

     scm_close_port (port);
  */

  return answer;
}

SCM
ly_quote_scm (SCM s)
{
  return scm_list_n (ly_symbol2scm ("quote"), s, SCM_UNDEFINED);
}



String
ly_symbol2string (SCM s)
{
  assert (gh_symbol_p (s));
  return String ((Byte*)SCM_STRING_CHARS (s), (int) SCM_STRING_LENGTH (s));
}


String
gulp_file_to_string (String fn)
{
  String s = global_path.find (fn);
  if (s == "")
    {
      String e = _f ("can't find file: `%s'", fn);
      e += " ";
      e += _f ("(load path: `%s')", global_path.str ());
      error (e);
    }
  else if (verbose_global_b)
    progress_indication ("[" + s);


  Simple_file_storage f (s);
  String result (f.ch_C ());
  if (verbose_global_b)
    progress_indication ("]");
  return result;
}

SCM
ly_gulp_file (SCM fn)
{
  return ly_str02scm (gulp_file_to_string (ly_scm2string (fn)).ch_C ());
}


/**
   Read a file, and shove it down GUILE.  GUILE also has file read
   functions, but you can't fiddle with the path of those.
 */
void
read_lily_scm_file (String fn)
{
  gh_eval_str ((char *) gulp_file_to_string (fn).ch_C ());
}

extern "C" {
  // maybe gdb 5.0 becomes quicker if it doesn't do fancy C++ typing?
void
ly_display_scm (SCM s)
{
  gh_display (s);
  gh_newline ();
}
};

String
ly_scm2string (SCM s)
{
  assert (gh_string_p (s));

  size_t len; 
  char *p = gh_scm2newstr (s , &len);
  
  String r (p);

  free (p);
  return r;
}

SCM
index_cell (SCM s, Direction d)
{
  assert (d);
  return (d == LEFT) ? ly_car (s) : ly_cdr (s);
}

SCM
index_set_cell (SCM s, Direction d, SCM v)
{
  if (d == LEFT)
    gh_set_car_x (s, v);
  else if (d == RIGHT)
    gh_set_cdr_x (s, v);
  return s;
}
  
SCM
ly_warning (SCM str)
{
  assert (gh_string_p (str));
  warning ("lily-guile: " + ly_scm2string (str));
  return SCM_BOOL_T;
}

SCM
ly_isdir_p (SCM s)
{
  if (gh_number_p (s))
    {
      int i = gh_scm2int (s);
      return (i>= -1 && i <= 1)  ? SCM_BOOL_T : SCM_BOOL_F; 
    }
  return SCM_BOOL_F;
}

bool
ly_number_pair_p (SCM p)
{
  return gh_pair_p (p) && gh_number_p (ly_car (p)) && gh_number_p (ly_cdr (p));
}

bool
ly_axis_p (SCM a)
{
  return gh_number_p (a) && (gh_scm2int (a) == 0 || gh_scm2int (a) == 1); 
}

typedef void (*Void_fptr) ();
Array<Void_fptr> *scm_init_funcs_;

void add_scm_init_func (void (*f) ())
{
  if (!scm_init_funcs_)
    scm_init_funcs_ = new Array<Void_fptr>;

  scm_init_funcs_->push (f);
}

extern  void init_cxx_function_smobs ();

void
prepend_load_path (String p )
{
  char s[1024];
  sprintf (s, 
	   "(set! %%load-path (cons \"%s\" %%load-path))", p.ch_C());

  scm_c_eval_string (s);
}

void
init_lily_guile (String p )
{
  prepend_load_path (p);

  // todo: junk this. We should make real modules iso. just loading files.
  prepend_load_path (p + "/scm/");

  SCM last_mod = scm_current_module ();
  scm_set_current_module (scm_c_resolve_module ("guile"));
  
  init_cxx_function_smobs ();
  for (int i=scm_init_funcs_->size () ; i--;)
    (scm_init_funcs_->elem (i)) ();

  if (verbose_global_b)
    progress_indication ("\n");
  read_lily_scm_file ("lily.scm");

  scm_set_current_module (last_mod);
}

unsigned int ly_scm_hash (SCM s)
{
  return scm_ihashv (s, ~1u);
}



bool
isdir_b (SCM s)
{
  if (gh_number_p (s))
    {
      int i = gh_scm2int (s);
      return i>= -1 && i <= 1; 
    }
  return false;
}


bool
isaxis_b (SCM s)
{
  if (gh_number_p (s))
    {
      int i = gh_scm2int (s);
      return i== 0 || i == 1;
    }
  return false;
}


Direction
to_dir (SCM s)
{
  return (Direction) gh_scm2int (s);
}

Interval
ly_scm2interval (SCM p)
{
  return  Interval (gh_scm2double (ly_car (p)),
		    gh_scm2double (ly_cdr (p)));
}

SCM
ly_interval2scm (Interval i)
{
  return gh_cons (gh_double2scm (i[LEFT]),
		  gh_double2scm (i[RIGHT]));
}




bool
to_boolean (SCM s)
{
  return gh_boolean_p (s) && gh_scm2bool (s);
}

/*
  Appendable list L: the cdr contains the list, the car the last cons
  in the list.
 */
SCM
appendable_list ()
{
  SCM s = gh_cons (SCM_EOL, SCM_EOL);
  gh_set_car_x (s, s);
  
  return s;
}

void
appendable_list_append (SCM l, SCM elt)
{
  SCM newcons = gh_cons (elt, SCM_EOL);
  
  gh_set_cdr_x (ly_car (l), newcons);      
  gh_set_car_x (l, newcons);
}


SCM
ly_offset2scm (Offset o)
{
  return gh_cons (gh_double2scm (o[X_AXIS]), gh_double2scm (o[Y_AXIS]));
}

Offset
ly_scm2offset (SCM s)
{
  return Offset (gh_scm2double (ly_car (s)),
		 gh_scm2double (ly_cdr (s)));
}

SCM
ly_type (SCM exp)
{
  char const  * cp = "unknown";
  if (gh_number_p (exp))
    {
      cp = "number";
    }
  else if (gh_string_p (exp))
    {
      cp = "string";
    }
  else if (gh_procedure_p (exp))
    {
      cp = "procedure";
    }
  else if (gh_boolean_p (exp))
    {
      cp = "boolean";
    }
  else if (gh_pair_p (exp))
    {
      cp = "list";
    }

  return ly_str02scm (cp);
}

/*
  convert without too many decimals, and leave  a space at the end.
 */
   
   
SCM
ly_number2string (SCM s)
{
  assert (gh_number_p (s));

  char str[400];			// ugh.

  if (scm_exact_p (s) == SCM_BOOL_F)
    {
      Real r (gh_scm2double (s));

      if (isinf (r) || isnan (r))
	{
	  programming_error ("Infinity or NaN encountered while converting Real number; setting to zero.");
	  r = 0.0;
	}

      sprintf (str, "%8.4f ", r);
    }
  else
    {
      sprintf (str, "%d ", gh_scm2int (s));
    }

  return ly_str02scm (str);
}

/*
  Undef this to see if GUILE GC is causing too many swaps.
 */

// #define TEST_GC

#ifdef TEST_GC
#include <libguile/gc.h>

static void *
greet_sweep (void *dummy1, void *dummy2, void *dummy3)
{
   fprintf (stderr, "entering sweep\n");
}

static void *
wave_sweep_goodbye (void *dummy1, void *dummy2, void *dummy3)
{
   fprintf (stderr, "leaving sweep\n");
}
#endif


#include "version.hh"
SCM
ly_version ()
{
  char const* vs =  "\' (" MAJOR_VERSION " " MINOR_VERSION " "  PATCH_LEVEL " " MY_PATCH_LEVEL ")" ;
  
  return gh_eval_str ((char*)vs);
}

static void
init_functions ()
{
  scm_c_define_gsubr ("ly-warn", 1, 0, 0,
		      (Scheme_function_unknown)ly_warning);
  scm_c_define_gsubr ("ly-version", 0, 0, 0,
		      (Scheme_function_unknown)ly_version);  
  scm_c_define_gsubr ("ly-gulp-file", 1,0, 0,
		      (Scheme_function_unknown)ly_gulp_file);
  scm_c_define_gsubr ("dir?", 1,0, 0, (Scheme_function_unknown)ly_isdir_p);
  scm_c_define_gsubr ("ly-number->string", 1, 0,0,
		      (Scheme_function_unknown) ly_number2string);


#ifdef TEST_GC 
  scm_c_hook_add (&scm_before_mark_c_hook, greet_sweep, 0, 0);
  scm_c_hook_add (&scm_before_sweep_c_hook, wave_sweep_goodbye, 0, 0);
#endif
}

ADD_SCM_INIT_FUNC (funcs, init_functions);

SCM
ly_deep_copy (SCM l)
{
  if (gh_pair_p (l))
    {
      return gh_cons (ly_deep_copy (ly_car (l)), ly_deep_copy (ly_cdr (l)));
    }
  else
    return l;
}




SCM
ly_assoc_chain (SCM key, SCM achain)
{
  if (gh_pair_p (achain))
    {
      SCM handle = scm_assoc (key, ly_car (achain));
      if (gh_pair_p (handle))
	return handle;
      else
	return ly_assoc_chain (key, ly_cdr (achain));
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
  if (gh_pair_p (alist)) {
    SCM trykey = ly_caar(alist);
    if(gh_pair_p(trykey) && to_boolean(scm_equal_p(key,ly_cdr(trykey))))
      return ly_car(alist);
    else
      return ly_assoc_cdr (key, ly_cdr (alist));
  }
  else
    return SCM_BOOL_F;
}

/*
  LIST has the form "sym1 sym2 sym3" 
 */
SCM
parse_symbol_list (const char * list)
{
  char * s = strdup (list);
  char *orig = s;
  SCM create_list = SCM_EOL;
  if (!s[0] )
    s = 0;

  while (s)
    {
      char *next = strchr (s, ' ');
      if (next)
	*next++ = 0;

      create_list = gh_cons (ly_symbol2scm (s), create_list);
      s = next;
    }

  free (orig);
  return create_list;
}


SCM
ly_truncate_list (int k, SCM l )
{
  if (k == 0)
    {
      l = SCM_EOL;
    }
  else
    {
      SCM s = l;
      k--;
      for (; gh_pair_p (s) && k--; s = ly_cdr (s))
	;

      if (gh_pair_p (s))
	{
	  gh_set_cdr_x (s, SCM_EOL);
	}
    }
  return l;
}

SCM my_gh_symbol2scm (const char* x)
{
  return gh_symbol2scm ((char*)x);
}

String
print_scm_val (SCM val)
{
  String realval = ly_scm2string (ly_write2scm (val));
  if (realval.length_i () > 200)
    realval = realval.left_str (100) + "\n :\n :\n" + realval.right_str (100);
  
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

  
  SCM type_p = SCM_EOL;

  if (gh_symbol_p (sym))
    type_p = scm_object_property (sym, type_symbol);

  if (type_p != SCM_EOL && !gh_procedure_p (type_p))
      {
	warning (_f ("Can't find property type-check for `%s' (%s).  Perhaps you made a typing error? Doing assignment anyway.",
		     ly_symbol2string (sym).ch_C (),
		     ly_symbol2string (type_symbol).ch_C ()

		     ));
      }
  else
    {
      if (val != SCM_EOL
	  && gh_procedure_p (type_p)
	  && gh_call1 (type_p, val) == SCM_BOOL_F)
	{
	  SCM errport = scm_current_error_port ();
	  ok = false;
	  SCM typefunc = scm_primitive_eval (ly_symbol2scm ("type-name"));
	  SCM type_name = gh_call1 (typefunc, type_p);

	 
	  scm_puts (_f ("Type check for `%s' failed; value `%s' must be of type `%s'",
			ly_symbol2string (sym).ch_C (),
			print_scm_val (val),
			ly_scm2string (type_name).ch_C ()).ch_C (),
		    errport);
	  scm_puts ("\n", errport);		      
	}
    }
  return ok;
}
