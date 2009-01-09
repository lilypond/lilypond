/*
  stem.cc -- implement Stem

  source file of the GNU LilyPond music typesetter

  (c) 1996--2009 Han-Wen Nienhuys <hanwen@xs4all.nl>
  Jan Nieuwenhuizen <janneke@gnu.org>

  TODO: This is way too hairy

  TODO: fix naming.

  Stem-end, chord-start, etc. is all confusing naming.
*/

#include "stem.hh"
#include "spanner.hh"

#include <cmath>		// rint
using namespace std;

#include "beam.hh"
#include "directional-element-interface.hh"
#include "dot-column.hh"
#include "font-interface.hh"
#include "international.hh"
#include "lookup.hh"
#include "misc.hh"
#include "note-head.hh"
#include "output-def.hh"
#include "paper-column.hh"
#include "pointer-group-interface.hh"
#include "rest.hh"
#include "rhythmic-head.hh"
#include "side-position-interface.hh"
#include "staff-symbol-referencer.hh"
#include "stem-tremolo.hh"
#include "warn.hh"

void
Stem::set_beaming (Grob *me, int beam_count, Direction d)
{
  SCM pair = me->get_property ("beaming");

  if (!scm_is_pair (pair))
    {
      pair = scm_cons (SCM_EOL, SCM_EOL);
      me->set_property ("beaming", pair);
    }

  SCM lst = index_get_cell (pair, d);
  if (beam_count)
    for (int i = 0; i < beam_count; i++)
      lst = scm_cons (scm_from_int (i), lst);
  else
    lst = SCM_BOOL_F;
  
  index_set_cell (pair, d, lst);
}

int
Stem::get_beaming (Grob *me, Direction d)
{
  SCM pair = me->get_property ("beaming");
  if (!scm_is_pair (pair))
    return 0;

  SCM lst = index_get_cell (pair, d);

  int len = scm_ilength (lst);
  return max (len, 0);
}

Interval
Stem::head_positions (Grob *me)
{
  if (head_count (me))
    {
      Drul_array<Grob *> e (extremal_heads (me));
      return Interval (Staff_symbol_referencer::get_position (e[DOWN]),
		       Staff_symbol_referencer::get_position (e[UP]));
    }
  return Interval ();
}

Real
Stem::chord_start_y (Grob *me)
{
  Interval hp = head_positions (me);
  if (!hp.is_empty ())
    return hp[get_grob_direction (me)] * Staff_symbol_referencer::staff_space (me)
      * 0.5;
  return 0;
}



void
Stem::set_stemend (Grob *me, Real se)
{
  // todo: margins
  Direction d = get_grob_direction (me);

  if (d && d * head_positions (me)[get_grob_direction (me)] >= se * d)
    me->warning (_ ("weird stem size, check for narrow beams"));

  me->set_property ("stem-end-position", scm_from_double (se));
}

/* Note head that determines hshift for upstems
   WARNING: triggers direction  */
Grob *
Stem::support_head (Grob *me)
{
  extract_grob_set (me, "note-heads", heads);
  if (heads.size () == 1)
    return heads[0];

  return first_head (me);
}

int
Stem::head_count (Grob *me)
{
  return Pointer_group_interface::count (me, ly_symbol2scm ("note-heads"));
}

/* The note head which forms one end of the stem.
   WARNING: triggers direction  */
Grob *
Stem::first_head (Grob *me)
{
  Direction d = get_grob_direction (me);
  if (d)
    return extremal_heads (me)[-d];
  return 0;
}

/* The note head opposite to the first head.  */
Grob *
Stem::last_head (Grob *me)
{
  Direction d = get_grob_direction (me);
  if (d)
    return extremal_heads (me)[d];
  return 0;
}

/*
  START is part where stem reaches `last' head.

  This function returns a drul with (bottom-head, top-head).
*/
Drul_array<Grob *>
Stem::extremal_heads (Grob *me)
{
  const int inf = INT_MAX;
  Drul_array<int> extpos;
  extpos[DOWN] = inf;
  extpos[UP] = -inf;

  Drul_array<Grob *> exthead (0, 0);
  extract_grob_set (me, "note-heads", heads);

  for (vsize i = heads.size (); i--;)
    {
      Grob *n = heads[i];
      int p = Staff_symbol_referencer::get_rounded_position (n);

      Direction d = LEFT;
      do
	{
	  if (d * p > d * extpos[d])
	    {
	      exthead[d] = n;
	      extpos[d] = p;
	    }
	}
      while (flip (&d) != DOWN);
    }
  return exthead;
}

/* The positions, in ascending order.  */
vector<int>
Stem::note_head_positions (Grob *me)
{
  vector<int> ps;
  extract_grob_set (me, "note-heads", heads);

  for (vsize i = heads.size (); i--;)
    {
      Grob *n = heads[i];
      int p = Staff_symbol_referencer::get_rounded_position (n);

      ps.push_back (p);
    }

  vector_sort (ps, less<int> ());
  return ps;
}

void
Stem::add_head (Grob *me, Grob *n)
{
  n->set_object ("stem", me->self_scm ());

  if (Note_head::has_interface (n))
    Pointer_group_interface::add_grob (me, ly_symbol2scm ("note-heads"), n);
  else if (Rest::has_interface (n))
    Pointer_group_interface::add_grob (me, ly_symbol2scm ("rests"), n);
}

bool
Stem::is_invisible (Grob *me)
{
  return !is_normal_stem (me)
    && (robust_scm2double (me->get_property ("stemlet-length"),
			   0.0) == 0.0);
}


bool
Stem::is_normal_stem (Grob *me)
{
  return head_count (me) && scm_to_int (me->get_property ("duration-log")) >= 1;
}


MAKE_SCHEME_CALLBACK (Stem, pure_height, 3)
SCM
Stem::pure_height (SCM smob,
		   SCM /* start */,
		   SCM /* end */)
{
  Grob *me = unsmob_grob (smob);
  Interval iv;

  if (!is_normal_stem (me))
    return ly_interval2scm (iv);

  Real ss = Staff_symbol_referencer::staff_space (me);
  Real rad = Staff_symbol_referencer::staff_radius (me);

  if (!to_boolean (me->get_property ("cross-staff")))
    {
      Real len = scm_to_double (calc_length (smob)) * ss / 2;
      Direction dir = get_grob_direction (me);

      Interval hp = head_positions (me);
      if (dir == UP)
	iv = Interval (0, len);
      else
	iv = Interval (-len, 0);

      if (!hp.is_empty ())
	iv.translate (hp[dir] * ss / 2);

      /* extend the stem (away from the head) to cover the staff */
      if (dir == UP)
	iv[UP] = max (iv[UP], rad * ss);
      else
	iv[DOWN] = min (iv[DOWN], -rad * ss);
    }
  else
    iv = Interval (-rad * ss, rad * ss);

  return ly_interval2scm (iv);
}

MAKE_SCHEME_CALLBACK (Stem, calc_stem_end_position, 1)
SCM
Stem::calc_stem_end_position (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  if (!head_count (me))
    return scm_from_double (0.0);

  if (Grob *beam = get_beam (me))
    {
      (void) beam->get_property ("quantized-positions");
      return me->get_property ("stem-end-position");
    }
  
  vector<Real> a;

  /* WARNING: IN HALF SPACES */
  Real length = robust_scm2double (me->get_property ("length"), 7);

  Direction dir = get_grob_direction (me);
  Interval hp = head_positions (me);
  Real stem_end = dir ? hp[dir] + dir * length : 0;

  /* TODO: change name  to extend-stems to staff/center/'()  */
  bool no_extend = to_boolean (me->get_property ("no-stem-extend"));
  if (!no_extend && dir * stem_end < 0)
    stem_end = 0.0;

  return scm_from_double (stem_end);
}

/* Length is in half-spaces (or: positions) here. */
MAKE_SCHEME_CALLBACK (Stem, calc_length, 1)
SCM
Stem::calc_length (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  
  SCM details = me->get_property ("details");
  int durlog = duration_log (me);

  Real ss = Staff_symbol_referencer::staff_space (me);
  Real length = 7;
  SCM s = ly_assoc_get (ly_symbol2scm ("lengths"), details, SCM_EOL);
  if (scm_is_pair (s))
    length = 2 * scm_to_double (robust_list_ref (durlog - 2, s));

  Direction dir = get_grob_direction (me);

  /* Stems in unnatural (forced) direction should be shortened,
     according to [Roush & Gourlay] */
  Interval hp = head_positions (me);
  if (dir && dir * hp[dir] >= 0)
    {
      SCM sshorten = ly_assoc_get (ly_symbol2scm ("stem-shorten"), details, SCM_EOL);
      SCM scm_shorten = scm_is_pair (sshorten)
	? robust_list_ref (max (duration_log (me) - 2, 0), sshorten) : SCM_EOL;
      Real shorten = 2* robust_scm2double (scm_shorten, 0);

      /* On boundary: shorten only half */
      if (abs (head_positions (me)[dir]) <= 1)
	shorten *= 0.5;

      length -= shorten;
    }

  length *= robust_scm2double (me->get_property ("length-fraction"), 1.0);

  /* Tremolo stuff.  */
  Grob *t_flag = unsmob_grob (me->get_object ("tremolo-flag"));
  if (t_flag && !unsmob_grob (me->get_object ("beam")))
    {
      /* Crude hack: add extra space if tremolo flag is there.

      We can't do this for the beam, since we get into a loop
      (Stem_tremolo::raw_stencil () looks at the beam.) --hwn  */

      Real minlen = 1.0
	+ 2 * Stem_tremolo::vertical_length (t_flag) / ss;

      /* We don't want to add the whole extent of the flag because the trem
         and the flag can overlap partly. beam_translation gives a good
         approximation */
      if (durlog >= 3)
        {
          Real beam_trans = Stem_tremolo::get_beam_translation (t_flag);
          /* the obvious choice is (durlog - 2) here, but we need a bit more space. */
          minlen += 2 * (durlog - 1.5) * beam_trans;

          /* up-stems need even a little more space to avoid collisions. This
             needs to be in sync with the tremolo positioning code in
             Stem_tremolo::print */
          if (dir == UP)
            minlen += beam_trans;
        }
      length = max (length, minlen + 1.0);
    }
  
  return scm_from_double (length);
}
/* The log of the duration (Number of hooks on the flag minus two)  */
int
Stem::duration_log (Grob *me)
{
  SCM s = me->get_property ("duration-log");
  return (scm_is_number (s)) ? scm_to_int (s) : 2;
}

MAKE_SCHEME_CALLBACK (Stem, calc_positioning_done, 1);
SCM
Stem::calc_positioning_done (SCM smob)
{
  Grob *me = unsmob_grob (smob);  
  if (!head_count (me))
    return SCM_BOOL_T;

  me->set_property ("positioning-done", SCM_BOOL_T);
  
  extract_grob_set (me, "note-heads", ro_heads);
  vector<Grob*> heads (ro_heads);
  vector_sort (heads, position_less);
  Direction dir = get_grob_direction (me);

  if (dir < 0)
    reverse (heads);

  Real thick = thickness (me);

  Grob *hed = support_head (me);
  if (!dir)
    {
      programming_error ("Stem dir must be up or down.");
      dir = UP;
      set_grob_direction (me, dir);
    }

  bool is_harmonic_centered = false;
  for (vsize i = 0; i < heads.size (); i++)
    is_harmonic_centered = is_harmonic_centered 
      || heads[i]->get_property ("style") == ly_symbol2scm ("harmonic");
  is_harmonic_centered = is_harmonic_centered && is_invisible (me);

  Real w = hed->extent (hed, X_AXIS)[dir];
  for (vsize i = 0; i < heads.size (); i++)
    {
      Real amount = w - heads[i]->extent (heads[i], X_AXIS)[dir];

      if (is_harmonic_centered)
	amount =
	  hed->extent (hed, X_AXIS).linear_combination (CENTER)
	  - heads[i]->extent (heads[i], X_AXIS).linear_combination (CENTER);
      
      heads[i]->translate_axis (amount, X_AXIS);
    }
  bool parity = true;
  Real lastpos = Real (Staff_symbol_referencer::get_position (heads[0]));
  for (vsize i = 1; i < heads.size (); i++)
    {
      Real p = Staff_symbol_referencer::get_position (heads[i]);
      Real dy = fabs (lastpos- p);

      /*
	dy should always be 0.5, 0.0, 1.0, but provide safety margin
	for rounding errors.
      */
      if (dy < 1.1)
	{
	  if (parity)
	    {
	      Real ell = heads[i]->extent (heads[i], X_AXIS).length ();

	      Direction d = get_grob_direction (me);
	      /*
		Reversed head should be shifted ell-thickness, but this
		looks too crowded, so we only shift ell-0.5*thickness.

		This leads to assymetry: Normal heads overlap the
		stem 100% whereas reversed heads only overlaps the
		stem 50%
	      */

	      Real reverse_overlap = 0.5;
	      heads[i]->translate_axis ((ell - thick * reverse_overlap) * d,
					X_AXIS);

	      if (is_invisible (me))
		heads[i]->translate_axis (-thick * (2 - reverse_overlap) * d,
					  X_AXIS);

	      /* TODO:

	      For some cases we should kern some more: when the
	      distance between the next or prev note is too large, we'd
	      get large white gaps, eg.

	      |
              X|
	      |X  <- kern this.
	      |
	      X

	      */
	    }
	  parity = !parity;
	}
      else
	parity = true;

      lastpos = int (p);
    }

  return SCM_BOOL_T;
}

MAKE_SCHEME_CALLBACK (Stem, calc_direction, 1);
SCM
Stem::calc_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Direction dir = CENTER;
  if (Grob *beam = unsmob_grob (me->get_object ("beam")))
    {
      SCM ignore_me = beam->get_property ("direction");
      (void) ignore_me;
      dir = get_grob_direction (me);
    }
  else
    {
      SCM dd = me->get_property ("default-direction");
      dir = to_dir (dd);
      if (!dir)
	return me->get_property ("neutral-direction");
    }
  
  return scm_from_int (dir);
}

MAKE_SCHEME_CALLBACK (Stem, calc_default_direction, 1);
SCM
Stem::calc_default_direction (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  Direction dir = CENTER;
  int staff_center = 0;
  Interval hp = head_positions (me);
  if (!hp.is_empty ())
    {
      int udistance = (int) (UP * hp[UP] - staff_center);
      int ddistance = (int) (DOWN * hp[DOWN] - staff_center);
      
      dir = Direction (sign (ddistance - udistance));
    }
  
  return scm_from_int (dir);
}


MAKE_SCHEME_CALLBACK (Stem, height, 1);
SCM
Stem::height (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  if (!is_normal_stem (me))
    return ly_interval2scm (Interval ());
  
  Direction dir = get_grob_direction (me);
  
  Grob *beam = get_beam (me);
  if (beam)
    {
      /* trigger set-stem-lengths. */
      beam->get_property ("quantized-positions");
    }

  /*
    Can't get_stencil (), since that would cache stencils too early.
    This causes problems with beams.
   */
  Stencil *stencil = unsmob_stencil (print (smob));
  Interval iv = stencil ? stencil->extent (Y_AXIS) : Interval ();
  if (beam)
    {
      if (dir == CENTER)
	{
	  programming_error ("no stem direction");
	  dir = UP;
	}
      iv[dir] += dir * Beam::get_thickness (beam) * 0.5;
    }

  return ly_interval2scm (iv);
}

Real
Stem::stem_end_position (Grob *me)
{
  return robust_scm2double (me->get_property ("stem-end-position"), 0);
}

MAKE_SCHEME_CALLBACK (Stem, calc_flag, 1);
SCM
Stem::calc_flag (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  int log = duration_log (me);
  /*
    TODO: maybe property stroke-style should take different values,
    e.g. "" (i.e. no stroke), "single" and "double" (currently, it's
    '() or "grace").  */
  string flag_style;

   SCM flag_style_scm = me->get_property ("flag-style");
  if (scm_is_symbol (flag_style_scm))
    flag_style = ly_symbol2string (flag_style_scm);

  if (flag_style == "no-flag")
    return Stencil ().smobbed_copy ();

  bool adjust = true;

  string staffline_offs;
  if (flag_style == "mensural")
    /* Mensural notation: For notes on staff lines, use different
       flags than for notes between staff lines.  The idea is that
       flags are always vertically aligned with the staff lines,
       regardless if the note head is on a staff line or between two
       staff lines.  In other words, the inner end of a flag always
       touches a staff line.
    */
    {
      if (adjust)
        {
          int p = (int) (rint (stem_end_position (me)));
          staffline_offs
            = Staff_symbol_referencer::on_line (me, p) ? "0" : "1";
        }
      else
        staffline_offs = "2";
     }
  else
    staffline_offs = "";

  char dir = (get_grob_direction (me) == UP) ? 'u' : 'd';
  string font_char = flag_style
    + to_string (dir) + staffline_offs + to_string (log);
  Font_metric *fm = Font_interface::get_default_font (me);
  Stencil flag = fm->find_by_name ("flags." + font_char);
  if (flag.is_empty ())
    me->warning (_f ("flag `%s' not found", font_char));

  SCM stroke_style_scm = me->get_property ("stroke-style");
  if (scm_is_string (stroke_style_scm))
    {
      string stroke_style = ly_scm2string (stroke_style_scm);
      if (!stroke_style.empty ())
        {
          string font_char = flag_style + to_string (dir) + stroke_style;
          Stencil stroke = fm->find_by_name ("flags." + font_char);
          if (stroke.is_empty ())
            {
              font_char = to_string (dir) + stroke_style;
              stroke = fm->find_by_name ("flags." + font_char);
            }
          if (stroke.is_empty ())
            me->warning (_f ("flag stroke `%s' not found", font_char));
          else
            flag.add_stencil (stroke);
        }
     }

  return flag.smobbed_copy ();
}


Stencil
Stem::flag (Grob *me)
{
  int log = duration_log (me);
  if (log < 3
      || unsmob_grob (me->get_object ("beam")))
    return Stencil ();

  if (!is_normal_stem (me))
    return Stencil ();

  // This get_property call already evaluates the scheme function with
  // the grob passed as argument! Thus, we only have to check if a valid
  // stencil is returned.
  SCM flag_style_scm = me->get_property ("flag");
  if (Stencil *flag = unsmob_stencil (flag_style_scm)) {
    return *flag;
  } else {
    return Stencil ();
  }
}

MAKE_SCHEME_CALLBACK (Stem, width, 1);
SCM
Stem::width (SCM e)
{
  Grob *me = unsmob_grob (e);

  Interval r;

  if (is_invisible (me))
    r.set_empty ();
  else if (unsmob_grob (me->get_object ("beam"))
	   || abs (duration_log (me)) <= 2)
    {
      r = Interval (-1, 1);
      r *= thickness (me) / 2;
    }
  else
    {
      r = Interval (-1, 1) * thickness (me) * 0.5;
      r.unite (flag (me).extent (X_AXIS));
    }
  return ly_interval2scm (r);
}

Real
Stem::thickness (Grob *me)
{
  return scm_to_double (me->get_property ("thickness"))
    * Staff_symbol_referencer::line_thickness (me);
}

MAKE_SCHEME_CALLBACK (Stem, print, 1);
SCM
Stem::print (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Grob *beam = get_beam (me);
    
  Stencil mol;
  Direction d = get_grob_direction (me);

  Real stemlet_length = robust_scm2double (me->get_property ("stemlet-length"),
					   0.0);
  bool stemlet = stemlet_length > 0.0;

  /* TODO: make the stem start a direction ?
     This is required to avoid stems passing in tablature chords.  */
  Grob *lh
    = to_boolean (me->get_property ("avoid-note-head"))
    ? last_head (me)
    : first_head (me);

  if (!lh && !stemlet)
    return SCM_EOL;

  if (!lh && stemlet && !beam)
    return SCM_EOL;

  if (lh && robust_scm2int (lh->get_property ("duration-log"), 0) < 1) 
    return SCM_EOL;

  if (is_invisible (me))
    return SCM_EOL;

  Real y2 = robust_scm2double (me->get_property ("stem-end-position"), 0.0);
  Real y1 = y2;
  Real half_space = Staff_symbol_referencer::staff_space (me) * 0.5;

  if (lh)
    y2 = Staff_symbol_referencer::get_position (lh);
  else if (stemlet)
    {
      Real beam_translation = Beam::get_beam_translation (beam);
      Real beam_thickness = Beam::get_thickness (beam);
      int beam_count = beam_multiplicity (me).length () + 1;

      y2 -= d
	* (0.5 * beam_thickness
	   + beam_translation * max (0, (beam_count - 1))
	   + stemlet_length) / half_space;
    }

  Interval stem_y (min (y1, y2), max (y2, y1));

  if (Grob *head = support_head (me))
    {
      /*
	must not take ledgers into account.
      */
      Interval head_height = head->extent (head, Y_AXIS);
      Real y_attach = Note_head::stem_attachment_coordinate (head, Y_AXIS);

      y_attach = head_height.linear_combination (y_attach);
      stem_y[Direction (-d)] += d * y_attach / half_space;
    }

  // URG
  Real stem_width = thickness (me);
  Real blot
    = me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));

  Box b = Box (Interval (-stem_width / 2, stem_width / 2),
	       Interval (stem_y[DOWN] * half_space, stem_y[UP] * half_space));

  Stencil ss = Lookup::round_filled_box (b, blot);
  mol.add_stencil (ss);

  mol.add_stencil (get_translated_flag (me));

  return mol.smobbed_copy ();
}

Stencil
Stem::get_translated_flag (Grob *me)
{
  Stencil fl = flag (me);
  if (!fl.is_empty ())
    {
      Direction d = get_grob_direction (me);
      Real blot
	= me->layout ()->get_dimension (ly_symbol2scm ("blot-diameter"));
      Real stem_width = thickness (me);
      Real half_space = Staff_symbol_referencer::staff_space (me) * 0.5;
      Real y2 = robust_scm2double (me->get_property ("stem-end-position"), 0.0);
      fl.translate_axis (y2 * half_space - d * blot / 2, Y_AXIS);
      fl.translate_axis (stem_width / 2, X_AXIS);
    }
  return fl;
}


/*
  move the stem to right of the notehead if it is up.
*/
MAKE_SCHEME_CALLBACK (Stem, offset_callback, 1);
SCM
Stem::offset_callback (SCM smob)
{
  Grob *me = unsmob_grob (smob);

  extract_grob_set (me, "rests", rests);
  if (rests.size ())
    {
      Grob *rest = rests.back ();
      Real r = rest->extent (rest, X_AXIS).center ();
      return scm_from_double (r);
    }

  
  if (Grob *f = first_head (me))
    {
      Interval head_wid = f->extent (f, X_AXIS);
      Real attach = 0.0;

      if (is_invisible (me))
	attach = 0.0;
      else
	attach = Note_head::stem_attachment_coordinate (f, X_AXIS);

      Direction d = get_grob_direction (me);
      Real real_attach = head_wid.linear_combination (d * attach);
      Real r = real_attach;

      /* If not centered: correct for stem thickness.  */
      if (attach)
	{
	  Real rule_thick = thickness (me);
	  r += -d * rule_thick * 0.5;
	}
      return scm_from_double (r);
    }

  programming_error ("Weird stem.");
  return scm_from_double (0.0);
}

Spanner *
Stem::get_beam (Grob *me)
{
  SCM b = me->get_object ("beam");
  return dynamic_cast<Spanner *> (unsmob_grob (b));
}

Stem_info
Stem::get_stem_info (Grob *me)
{
  Stem_info si;
  si.dir_ = get_grob_direction (me);
  
  SCM scm_info = me->get_property ("stem-info");
  si.ideal_y_ = scm_to_double (scm_car (scm_info));
  si.shortest_y_ = scm_to_double (scm_cadr (scm_info));
  return si;
}

MAKE_SCHEME_CALLBACK (Stem, calc_stem_info, 1);
SCM
Stem::calc_stem_info (SCM smob)
{
  Grob *me = unsmob_grob (smob);
  Direction my_dir = get_grob_direction (me);

  if (!my_dir)
    {
      programming_error ("no stem dir set");
      my_dir = UP;
    }

  Real staff_space = Staff_symbol_referencer::staff_space (me);
  Grob *beam = get_beam (me);

  if (beam)
    {
      (void) beam->get_property ("beaming");
    }
  
  Real beam_translation = Beam::get_beam_translation (beam);
  Real beam_thickness = Beam::get_thickness (beam);
  int beam_count = Beam::get_direction_beam_count (beam, my_dir);
  Real length_fraction
    = robust_scm2double (me->get_property ("length-fraction"), 1.0);

  /* Simple standard stem length */
  SCM details = me->get_property ("details");
  SCM lengths = ly_assoc_get (ly_symbol2scm ("beamed-lengths"), details, SCM_EOL);
  
  Real ideal_length
    = scm_to_double (robust_list_ref (beam_count - 1, lengths))
    * staff_space
    * length_fraction
    
    /* stem only extends to center of beam
     */
    - 0.5 * beam_thickness;

  /* Condition: sane minimum free stem length (chord to beams) */
  lengths = ly_assoc_get (ly_symbol2scm ("beamed-minimum-free-lengths"), details, SCM_EOL);

  Real ideal_minimum_free
    = scm_to_double (robust_list_ref (beam_count - 1, lengths))
    * staff_space
    * length_fraction;

  Real height_of_my_trem = 0.0;
  Grob *trem = unsmob_grob (me->get_object ("tremolo-flag"));
  if (trem)
    {
      height_of_my_trem
	= Stem_tremolo::vertical_length (trem)
        /* hack a bit of space around the trem. */
        + beam_translation;
    }

  
  /* UGH
     It seems that also for ideal minimum length, we must use
     the maximum beam count (for this direction):

     \score{ \notes\relative c''{ [a8 a32] }}

     must be horizontal. */
  Real height_of_my_beams = beam_thickness
    + (beam_count - 1) * beam_translation;

  Real ideal_minimum_length = ideal_minimum_free
    + height_of_my_beams
    + height_of_my_trem
    /* stem only extends to center of beam */
    - 0.5 * beam_thickness;

  ideal_length = max (ideal_length, ideal_minimum_length);

  /* Convert to Y position, calculate for dir == UP */
  Real note_start
    =     /* staff positions */
    head_positions (me)[my_dir] * 0.5
    * my_dir * staff_space;
  Real ideal_y = note_start + ideal_length;

  /* Conditions for Y position */

  /* Lowest beam of (UP) beam must never be lower than second staffline

  Reference?

  Although this (additional) rule is probably correct,
  I expect that highest beam (UP) should also never be lower
  than middle staffline, just as normal stems.

  Reference?

  Obviously not for grace beams.

  Also, not for knees.  Seems to be a good thing. */
  bool no_extend = to_boolean (me->get_property ("no-stem-extend"));
  bool is_knee = to_boolean (beam->get_property ("knee"));
  if (!no_extend && !is_knee)
    {
      /* Highest beam of (UP) beam must never be lower than middle
	 staffline */
      ideal_y = max (ideal_y, 0.0);
      /* Lowest beam of (UP) beam must never be lower than second staffline */
      ideal_y = max (ideal_y, (-staff_space
			       - beam_thickness + height_of_my_beams));
    }

  ideal_y -= robust_scm2double (beam->get_property ("shorten"), 0);

  SCM bemfl = ly_assoc_get (ly_symbol2scm ("beamed-extreme-minimum-free-lengths"),
			    details, SCM_EOL);
  
  Real minimum_free
    = scm_to_double (robust_list_ref (beam_count - 1, bemfl))
    * staff_space
    * length_fraction;

  Real minimum_length = max (minimum_free, height_of_my_trem)
    + height_of_my_beams
    /* stem only extends to center of beam */
    - 0.5 * beam_thickness;

  ideal_y *= my_dir;
  Real minimum_y = note_start + minimum_length;
  Real shortest_y = minimum_y * my_dir;

  return scm_list_2 (scm_from_double (ideal_y),
		     scm_from_double (shortest_y));
}

Slice
Stem::beam_multiplicity (Grob *stem)
{
  SCM beaming = stem->get_property ("beaming");
  Slice le = int_list_to_slice (scm_car (beaming));
  Slice ri = int_list_to_slice (scm_cdr (beaming));
  le.unite (ri);
  return le;
}

bool
Stem::is_cross_staff (Grob *stem)
{
  Grob *beam = unsmob_grob (stem->get_object ("beam"));
  return beam && Beam::is_cross_staff (beam);
}

MAKE_SCHEME_CALLBACK (Stem, calc_cross_staff, 1)
SCM
Stem::calc_cross_staff (SCM smob)
{
  return scm_from_bool (is_cross_staff (unsmob_grob (smob)));
}

/* FIXME:  Too many properties  */
ADD_INTERFACE (Stem,
	       "The stem represents the graphical stem.  In addition, it"
	       " internally connects note heads, beams, and tremolos.  Rests"
	       " and whole notes have invisible stems.\n"
	       "\n"
	       "The following properties may be set in the @code{details}"
	       " list.\n"
	       "\n"
	       "@table @code\n"
	       "@item beamed-lengths\n"
	       "List of stem lengths given beam multiplicity.\n"
	       "@item beamed-minimum-free-lengths\n"
	       "List of normal minimum free stem lengths (chord to beams)"
	       " given beam multiplicity.\n"
	       "@item beamed-extreme-minimum-free-lengths\n"
	       "List of extreme minimum free stem lengths (chord to beams)"
	       " given beam multiplicity.\n"
	       "@item lengths\n"
	       "Default stem lengths.  The list gives a length for each"
	       " flag count.\n"
	       "@item stem-shorten\n"
	       "How much a stem in a forced direction should be shortened."
	       "  The list gives an amount depending on the number of flags"
	       " and beams.\n"
	       "@end table\n",

	       /* properties */
	       "avoid-note-head "
	       "beam "
	       "beaming "
	       "beamlet-default-length "
	       "beamlet-max-length-proportion "
	       "default-direction "
	       "details "
	       "direction "
	       "duration-log "
	       "flag "
	       "flag-style "
	       "french-beaming "
	       "length "
	       "length-fraction "
	       "max-beam-connect "
	       "neutral-direction "
	       "no-stem-extend "
	       "note-heads "
	       "positioning-done "
	       "rests "
	       "stem-end-position "
	       "stem-info "
	       "stemlet-length "
	       "stroke-style "
	       "thickness "
	       "tremolo-flag "
	       );

/****************************************************************/

Stem_info::Stem_info ()
{
  ideal_y_ = shortest_y_ = 0;
  dir_ = CENTER;
}

void
Stem_info::scale (Real x)
{
  ideal_y_ *= x;
  shortest_y_ *= x;
}
