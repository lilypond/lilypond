/*
  chord.cc -- implement Chord

  source file of the GNU LilyPond music typesetter

  (c)  1999--2002 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord.hh"
#include "musical-request.hh"
#include "warn.hh"

#include "music-list.hh"
#include "musical-request.hh"


SCM
Chord::base_pitches (SCM tonic)
{
  SCM base = SCM_EOL;

  SCM major = Pitch (0, 2, 0).smobbed_copy ();
  SCM minor = Pitch (0, 2, -1).smobbed_copy ();

  base = gh_cons (tonic, base);
  base = gh_cons (ly_pitch_transpose (ly_car (base), major), base);
  base = gh_cons (ly_pitch_transpose (ly_car (base), minor), base);

  return scm_reverse_x (base, SCM_EOL);
}

SCM
Chord::transpose_pitches (SCM tonic, SCM pitches)
{
  /* map?
     hoe doe je lambda in C?
  */
  SCM transposed = SCM_EOL;
  for (SCM i = pitches; gh_pair_p (i); i = ly_cdr (i))
    {
      transposed = gh_cons (ly_pitch_transpose (tonic, ly_car (i)),
			    transposed);
    }
  return scm_reverse_x (transposed, SCM_EOL);
}

/*
  burp, in SCM duw je gewoon een (if (= (step x) 7) (...)) door pitches

  Lower step STEP.
  If step == 0, lower all.
 */
SCM
Chord::lower_step (SCM tonic, SCM pitches, SCM step)
{
  SCM lowered = SCM_EOL;
  for (SCM i = pitches; gh_pair_p (i); i = ly_cdr (i))
    {
      SCM p = ly_car (i);
      if (gh_equal_p (step_scm (tonic, ly_car (i)), step)
	  || gh_scm2int (step) == 0)
	{
	  p = ly_pitch_transpose (p, Pitch (0, 0, -1).smobbed_copy ());
	}
      lowered = gh_cons (p, lowered);
    }
  return scm_reverse_x (lowered, SCM_EOL);
}

/* Return member that has same notename, disregarding octave or alterations */
SCM
Chord::member_notename (SCM p, SCM pitches)
{
  /* If there's an exact match, make sure to return that */
  SCM member = gh_member (p, pitches);
  if (member == SCM_BOOL_F)
    {
      for (SCM i = pitches; gh_pair_p (i); i = ly_cdr (i))
	{
	  /*
	    Urg, eindelijk gevonden: () != #f, kan maar niet aan wennen.
	    Anders kon iets korter...
	   */
	  if (unsmob_pitch (p)->notename_
	      == unsmob_pitch (ly_car (i))->notename_)
	    {
	      member = ly_car (i);
	      break;
	    }
	}
    }
  else
    member = ly_car (member);
  return member;
}

/* Return member that has same notename and alteration, disregarding octave */
SCM
Chord::member_pitch (SCM p, SCM pitches)
{
  /* If there's an exact match, make sure to return that */
  SCM member = gh_member (p, pitches);
  if (member == SCM_BOOL_F)
    {
      for (SCM i = pitches; gh_pair_p (i); i = ly_cdr (i))
	{
	  if (unsmob_pitch (p)->notename_
	      == unsmob_pitch (ly_car (i))->notename_
	      && unsmob_pitch (p)->alteration_
	      == unsmob_pitch (ly_car (i))->alteration_)
	    {
	      member = ly_car (i);
	      break;
	    }
	}
    }
  else
    member = ly_car (member);
  return member;
}

SCM
Chord::step_scm (SCM tonic, SCM p)
{
  /* De Pitch intervaas is nog beetje sleutelgat? */
  int i = unsmob_pitch (p)->notename_
    - unsmob_pitch (tonic)->notename_
    + (unsmob_pitch (p)->octave_
       - unsmob_pitch (tonic)->octave_) * 7;
  while (i < 0)
    i += 7;
  i++;
  return scm_int2num (i);
}

/*
  Assuming that PITCHES is a chord, with tonic (CAR PITCHES), find
  missing thirds, only considering notenames.  Eg, for

    PITCHES = c gis d'

  return
  
    MISSING = e b'

*/
SCM
Chord::missing_thirds (SCM pitches)
{
  SCM thirds = SCM_EOL;
  
  /* is the third c-e, d-f, etc. small or large? */
  int minormajor_a[] = {0, -1, -1, 0, 0, -1, -1};
  for (int i=0; i < 7; i++)
    thirds = gh_cons (Pitch (0, 2, minormajor_a[i]).smobbed_copy (),
		      thirds);
  thirds = scm_vector (scm_reverse_x (thirds, SCM_EOL));
  
  SCM tonic = ly_car (pitches);
  SCM last = tonic;
  SCM missing = SCM_EOL;

  for (SCM i = pitches; gh_pair_p (i);)
    {
      SCM p = ly_car (i);
      int step = gh_scm2int (step_scm (tonic, p));
      
      if (unsmob_pitch (last)->notename_ == unsmob_pitch (p)->notename_)
	{
	  int third = (unsmob_pitch (last)->notename_
		       - unsmob_pitch (tonic)-> notename_ + 7) % 7;
	  last = ly_pitch_transpose (last, scm_vector_ref (thirds, scm_int2num (third)));
	}
      
      if (step > gh_scm2int (step_scm (tonic, last)))
	{
	  while (step > gh_scm2int (step_scm (tonic, last)))
	    {
	      missing = gh_cons (last, missing);
	      int third = (unsmob_pitch (last)->notename_
			   - unsmob_pitch (tonic)->notename_ + 7) % 7;
	      last = ly_pitch_transpose (last, scm_vector_ref (thirds,
						      scm_int2num (third)));
	    }
	}
      else
	{
	  i = ly_cdr (i);
	}
    }
  
  return lower_step (tonic, missing, scm_int2num (7));
}

/* Return PITCHES with PITCH added not as lowest note */
SCM
Chord::add_above_tonic (SCM pitch, SCM pitches)
{
  /* Should we maybe first make sure that PITCH is below tonic? */
  if (pitches != SCM_EOL)
    while (Pitch::less_p (pitch, ly_car (pitches)) == SCM_BOOL_T)
      pitch = ly_pitch_transpose (pitch, Pitch (1, 0, 0).smobbed_copy ());
   
  pitches = gh_cons (pitch, pitches);
  return scm_sort_list (pitches, Pitch::less_p_proc);
}

/* Return PITCHES with PITCH added as lowest note */
SCM
Chord::add_below_tonic (SCM pitch, SCM pitches)
{
  if (pitches != SCM_EOL)
    while (Pitch::less_p (ly_car (pitches), pitch) == SCM_BOOL_T)
      pitch = ly_pitch_transpose (pitch, Pitch (-1, 0, 0).smobbed_copy ());
  return gh_cons (pitch, pitches);
}



/*
  Parser stuff 
  
  Construct from parser output:

  PITCHES is the plain chord, it does not include bass or inversion
  
  Part of Chord:: namespace for now, because we do lots of
  chord-manipulating stuff.
*/
SCM
Chord::tonic_add_sub_to_pitches (SCM tonic, SCM add, SCM sub)
{
  /* urg: catch dim modifier: 3rd, 5th, 7th, .. should be lowered */
  bool dim_b = false;
  for (SCM i = add; gh_pair_p (i); i = ly_cdr (i))
    {
      Pitch* p = unsmob_pitch (ly_car (i));
      /* Ugr
	This chord modifier stuff should really be fixed
       Cmaj7 yields C 7/7-
      */
      if (p->get_octave ()  == -100)
        {
          p->octave_ = 0;
	  dim_b = true;
	}
    }
  add = transpose_pitches (tonic, add);
  add = lower_step (tonic, add, scm_int2num (7));
  add = scm_sort_list (add, Pitch::less_p_proc);
  add = ly_unique (add);
  
  sub = transpose_pitches (tonic, sub);
  sub = lower_step (tonic, sub, scm_int2num (7));
  sub = scm_sort_list (sub, Pitch::less_p_proc);
  
  /* default chord includes upto 5: <1, 3, 5>   */
  add = gh_cons (tonic, add);
  SCM tmp = add;
  
  SCM fifth = ly_last (base_pitches (tonic));
  int highest_step = gh_scm2int (step_scm (tonic, ly_last (tmp)));
  if (highest_step < 5)
    tmp = ly_snoc (fifth, tmp);
  else if (dim_b)
    {
      add = lower_step (tonic, add, scm_int2num (5));
      add = lower_step (tonic, add, scm_int2num (7));
    }

  /* find missing thirds */
  SCM missing = missing_thirds (tmp);
  if (highest_step < 5)
    missing = ly_snoc (fifth, missing);

  /* if dim modifier is given: lower all missing */
  if (dim_b)
    missing = lower_step (tonic, missing, scm_int2num (0));
  
  /* if additions include any 3, don't add third */
  SCM third = ly_cadr (base_pitches (tonic));
  if (member_notename (third, add) != SCM_BOOL_F)
    missing = scm_delete (third, missing);

  /* if additions include any 4, assume sus4 and don't add third implicitely
     C-sus (4) = c f g (1 4 5) */
  SCM sus = ly_pitch_transpose (tonic, Pitch (0, 3, 0).smobbed_copy ());
  if (member_notename (sus, add) != SCM_BOOL_F)
    missing = scm_delete (third, missing);
  
  /* if additions include some 5, don't add fifth */
  if (member_notename (fifth, add) != SCM_BOOL_F)
    missing = scm_delete (fifth, missing);
    
  /* complete the list of thirds to be added */
  add = gh_append2 (missing, add);
  add = scm_sort_list (add, Pitch::less_p_proc);
  
  SCM pitches = SCM_EOL;
  /* Add all that aren't subtracted */
  for (SCM i = add; gh_pair_p (i); i = ly_cdr (i))
    {
      SCM p = ly_car (i);
      SCM s = member_notename (p, sub);
      if (s != SCM_BOOL_F)
	sub = scm_delete (s, sub);
      else
	pitches = gh_cons (p, pitches);
    }
  pitches = scm_sort_list (pitches, Pitch::less_p_proc);
  
  for (SCM i = sub; gh_pair_p (i); i = ly_cdr (i))
    warning (_f ("invalid subtraction: not part of chord: %s",
		 unsmob_pitch (ly_car (i))->string ()));

  return pitches;
}


/* --Het lijkt me dat dit in het paarse gedeelte moet. */
Simultaneous_music *
Chord::get_chord (SCM tonic, SCM add, SCM sub, SCM inversion, SCM bass, SCM dur)
{
  SCM pitches = tonic_add_sub_to_pitches (tonic, add, sub);
  SCM list = SCM_EOL;
  if (inversion != SCM_EOL)
    {
      /* If inversion requested, check first if the note is part of chord */
      SCM s = member_pitch (inversion, pitches);
      if (s != SCM_BOOL_F)
	{
	  /* Then, delete and add as base note, ie: the inversion */
	  pitches = scm_delete (s, pitches);
	  Note_req* n = new Note_req;
	  n->set_mus_property ("pitch", ly_car (add_below_tonic (s, pitches)));
	  n->set_mus_property ("duration", dur);
	  n->set_mus_property ("inversion", SCM_BOOL_T);
	  list = gh_cons (n->self_scm (), list);
	  scm_gc_unprotect_object (n->self_scm ());
	}
      else
	warning (_f ("invalid inversion pitch: not part of chord: %s",
		     unsmob_pitch (inversion)->string ()));
    }

  /* Bass is easy, just add if requested */
  if (bass != SCM_EOL)
    {
      Note_req* n = new Note_req;
      n->set_mus_property ("pitch", ly_car (add_below_tonic (bass, pitches)));
      n->set_mus_property ("duration", dur);
      n->set_mus_property ("bass", SCM_BOOL_T);
      list = gh_cons (n->self_scm (), list);
      scm_gc_unprotect_object (n->self_scm ());
    }
  
  for (SCM i = pitches; gh_pair_p (i); i = ly_cdr (i))
    {
      Note_req* n = new Note_req;
      n->set_mus_property ("pitch", ly_car (i));
      n->set_mus_property ("duration", dur);
      list = gh_cons (n->self_scm (), list);
      scm_gc_unprotect_object (n->self_scm ());
    }

  Simultaneous_music*v = new Request_chord ();
  v->set_mus_property ("elements", list);

  return v;
}


