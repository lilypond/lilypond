/*   
  input-smob.cc --  implement Input smob
  
  source file of the GNU LilyPond music typesetter
  
  (c) 2000--2002 Han-Wen Nienhuys <hanwen@cs.uu.nl>
  
 */

#include "input.hh"
#include "input-smob.hh"
#include "string.hh"
#include "ly-smobs.icc"

static long input_tag;

static
SCM mark_smob (SCM)
{
  return SCM_EOL;
}

static int
print_smob (SCM s, SCM port, scm_print_state *)
{
  String str = "#<location " +  unsmob_input (s)->location_string () + ">";
  scm_puts (str.to_str0 (), port);
  return 1;
}

static size_t
free_smob (SCM s)
{
  delete unsmob_input (s);
  return 0;
}

/*
  We don't use IMPLEMENT_TYPE_P, since the smobification part is
  implemented separately from the class.
 */
LY_DEFINE(ly_input, "ly-input-location?", 1, 0, 0,
	  (SCM x),
	  "Return whether @var{x} is an input location")
{
  return unsmob_input (x) ? SCM_BOOL_T : SCM_BOOL_F ;
}

LY_DEFINE(ly_input_message,  "ly-input-message", 2, 0, 0, (SCM sip, SCM msg),
	  "Print @var{msg} as a GNU compliant error message, pointing to the
location in @var{sip}.")
{
  Input *ip  = unsmob_input(sip);
  
  SCM_ASSERT_TYPE(ip, sip, SCM_ARG1, __FUNCTION__, "input location");
  SCM_ASSERT_TYPE(gh_string_p (msg), msg, SCM_ARG2, __FUNCTION__, "string");

  String m = ly_scm2string (msg);

  ip->message (m);
  return SCM_UNDEFINED;
}


static void
start_input_smobs ()
{
  input_tag = scm_make_smob_type ("input", 0);
  scm_set_smob_mark (input_tag, mark_smob);
  scm_set_smob_free (input_tag, free_smob);
  scm_set_smob_print (input_tag, print_smob);
  scm_set_smob_equalp (input_tag, 0);

  
}

SCM
make_input (Input ip)
{
  Input * nip =  new Input (ip);
  SCM z;
  
  SCM_NEWSMOB (z, input_tag, nip);
  return z;
}

Input *						
unsmob_input (SCM s)
{
  if (SCM_IMP (s))
    return 0;
  if (SCM_CAR (s) == (SCM)input_tag) // ugh.
    return (Input*) SCM_CDR (s);
  else						
    return 0;					
}


ADD_SCM_INIT_FUNC (input, start_input_smobs);


Input dummy_input_global;

