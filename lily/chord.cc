/*
  chord.cc -- implement Chord

  source file of the GNU LilyPond music typesetter

  (c)  1999--2000 Jan Nieuwenhuizen <janneke@gnu.org>
*/

#include "chord.hh"
#include "musical-request.hh"
#include "warn.hh"
#include "debug.hh"
#include "music-list.hh"
#include "musical-request.hh"

/* some SCM abbrevs

   zijn deze nou handig?
   zijn ze er al in scheme, maar heten ze anders? */


/* Remove doubles from (sorted) list */
SCM
ly_unique (SCM list)
{
  SCM unique = SCM_EOL;
  for (SCM i = list; gh_pair_p (i); i = gh_cdr (i))
    {
      if (!gh_pair_p (gh_cdr (i))
	  || !gh_equal_p (gh_car (i), gh_cadr (i)))
	unique = gh_cons (gh_car (i), unique);
    }
  return gh_reverse (unique);
}

/* Hmm, rewrite this using ly_split_list? */
SCM
ly_remove_member (SCM s, SCM list)
{
  SCM removed = SCM_EOL;
  for (SCM i = list; gh_pair_p (i); i = gh_cdr (i))
    {
      if (!gh_equal_p (gh_car (i), s))
	removed = gh_cons (gh_car (i), removed);
    }
  return gh_reverse (removed);
}

SCM
ly_last (SCM list)
{
  return gh_car (scm_last_pair (list));
}

/* tail add */
SCM
ly_snoc (SCM s, SCM list)
{
  return gh_append2 (list, gh_list (s, SCM_UNDEFINED));
}


/* Split list at member s, removing s.
   Return (BEFORE . AFTER) */
SCM
ly_split_list (SCM s, SCM list)
{
  SCM before = SCM_EOL;
  SCM after = list;
  for (; gh_pair_p (after);)
    {
      SCM i = gh_car (after);
      after = gh_cdr (after);
      if (gh_equal_p (i, s))
	break;
      before = gh_cons (i, before);
    }
  return gh_cons (gh_reverse (before), after);
}


/* Construct from list of pitches and requests:

  (PITCHES . (INVERSION . BASS))


  Note, the pitches here, are all inclusive.
  We must identify tonic, filter-out (and maybe detect) inversion and bass. */

SCM
Chord::pitches_and_requests_to_chord (SCM pitches,
				      SCM tonic_req,
				      SCM inversion_req,
				      SCM bass_req,
				      bool find_inversion_b)
{
  pitches = scm_sort_list (pitches,
			   scm_eval2 (ly_symbol2scm ("Pitch::less_p"),
				      SCM_EOL));

			   
  if (bass_req != SCM_EOL)
    {
      assert (unsmob_pitch (gh_car (pitches))->notename_i_
	      == unsmob_pitch (bass_req)->notename_i_);
      pitches = gh_cdr (pitches);
    }
    
  if (inversion_req != SCM_EOL)
    {
      assert (unsmob_pitch (gh_car (pitches))->notename_i_
	      == unsmob_pitch (inversion_req)->notename_i_);
      /* huh ? */
      assert (tonic_req != SCM_EOL);
      
      SCM tonic = member_notename (tonic_req, pitches);
      if (tonic != SCM_EOL)
	pitches = rebuild_insert_inversion (pitches); //, tonic);
    }
  else if (find_inversion_b)
    {
      SCM tonic = (tonic_req != SCM_EOL)
	? member_notename (pitches, tonic_req)
	: guess_tonic (pitches);
	
      if (tonic != SCM_EOL)
	{
	  inversion_req = gh_car (pitches);
	  pitches = rebuild_insert_inversion (pitches); //, tonic);
	}
    }

  if (tonic_req != SCM_EOL)
      assert (unsmob_pitch (gh_car (pitches))->notename_i_
	      == unsmob_pitch (tonic_req)->notename_i_);

  return gh_cons (pitches, gh_cons (inversion_req, bass_req));
}

/*
  JUNKME. 
  do something smarter.
  zoals?
 */
SCM
Chord::base_pitches (SCM tonic)
{
  SCM base = SCM_EOL;

  SCM major = Pitch (0, 2, 0).smobbed_copy ();
  SCM minor = Pitch (0, 2, -1).smobbed_copy ();

  base = gh_cons (tonic, base);
  base = gh_cons (Pitch::transpose (gh_car (base), major), base);
  base = gh_cons (Pitch::transpose (gh_car (base), minor), base);

  return gh_reverse (base);
}

SCM
Chord::transpose_pitches (SCM tonic, SCM pitches)
{
  /* map?
     hoe doe je lambda in C?
  */
  SCM transposed = SCM_EOL;
  for (SCM i = pitches; gh_pair_p (i); i = gh_cdr (i))
    {
      transposed = gh_cons (Pitch::transpose (tonic, gh_car (i)),
			    transposed);
    }
  return gh_reverse (transposed);
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
  for (SCM i = pitches; gh_pair_p (i); i = gh_cdr (i))
    {
      SCM p = gh_car (i);
      if (gh_equal_p (step_scm (tonic, gh_car (i)), step)
	  || gh_scm2int (step) == 0)
	{
#if 0
	  Pitch x = *unsmob_pitch (p);
	  x.alteration_i_--;
	  p = x.smobbed_copy ();
#else
	  p = Pitch::transpose (p, Pitch (0, 0, -1).smobbed_copy ());
#endif
	}
      lowered = gh_cons (p, lowered);
    }
  return gh_reverse (lowered);
}

/* Return member that has same notename, disregarding octave or accidentals */
SCM
Chord::member_notename (SCM p, SCM pitches)
{
  /* If there's an exact match, make sure to return that */
  SCM member = gh_member (p, pitches);
  if (member == SCM_BOOL_F)
    {
      for (SCM i = pitches; gh_pair_p (i); i = gh_cdr (i))
	{
	  /*
	    Urg, eindelijk gevonden: () != #f, kan maar niet aan wennen.
	    Anders kon iets korter...
	   */
	  if (unsmob_pitch (p)->notename_i_
	      == unsmob_pitch (gh_car (i))->notename_i_)
	    {
	      member = gh_car (i);
	      break;
	    }
	}
    }
  return member;
}

int
Chord::step_i (Pitch tonic, Pitch p)
{
  int i = p.notename_i_ - tonic.notename_i_
    + (p.octave_i ()  - tonic.octave_i () ) * 7;
  while (i < 0)
    i += 7;
  i++;
  return i;
}

SCM
Chord::step_scm (SCM tonic, SCM p)
{
  return gh_int2scm (step_i (*unsmob_pitch (tonic), *unsmob_pitch (p)));
}

/*
  docme
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
  thirds = scm_vector (gh_reverse (thirds));
  
  SCM tonic = gh_car (pitches);
  SCM last = tonic;
  SCM missing = SCM_EOL;

  for (SCM i = pitches; gh_pair_p (i);)
    {
      SCM p = gh_car (i);
      int step = gh_scm2int (step_scm (tonic, p));
      
      if (unsmob_pitch (last)->notename_i_ == unsmob_pitch (p)->notename_i_)
	{
	  int third = (unsmob_pitch (last)->notename_i_
		       - unsmob_pitch (tonic)-> notename_i_ + 7) % 7;
	  last = Pitch::transpose (last, scm_vector_ref (thirds, gh_int2scm (third)));
	}
      
      if (step > gh_scm2int (step_scm (tonic, last)))
	{
	  while (step > gh_scm2int (step_scm (tonic, last)))
	    {
	      missing = gh_cons (last, missing);
	      int third = (unsmob_pitch (last)->notename_i_
			   - unsmob_pitch (tonic)->notename_i_ + 7) % 7;
	      last = Pitch::transpose (last, scm_vector_ref (thirds,
						      gh_int2scm (third)));
	    }
	}
      else
	{
	  i = gh_cdr (i);
	}
    }
  
  return lower_step (tonic, missing, gh_int2scm (7));
}


/*
 Mangle

 (PITCHES . (INVERSION . BASS))
 
 into list of pitches.
 
 For normal chord entry, inversion and bass pitches are retained in
 specific *_requests */

SCM
Chord::to_pitches (SCM chord)
{
  SCM pitches = gh_car (chord);
  SCM modifiers = gh_cdr (chord);
  SCM inversion = gh_car (modifiers);
  SCM bass = gh_cdr (modifiers);
  if (inversion != SCM_EOL)
    {
      Pitch inversion_pitch = *unsmob_pitch (inversion);
      SCM i = pitches;
      for (; gh_pair_p (i); i = gh_cdr (i))
	{
	  Pitch p = *unsmob_pitch (gh_car (i));
	  if ((p.notename_i_ == inversion_pitch.notename_i_)
	      && (p.alteration_i_ == inversion_pitch.alteration_i_))
	    break;
	}
      if (gh_pair_p (i))
	pitches = rebuild_with_bass (pitches, gh_car (i));
      else
	warning (_f ("invalid inversion pitch: not part of chord: %s",
		     unsmob_pitch (inversion)->str ()));
    }

  if (bass != SCM_EOL)
    {
      pitches = gh_cons (bass, pitches);
      pitches = rebuild_with_bass (pitches, bass);
    }
  return pitches;
}

/*
  This routine tries to guess tonic in a possibly inversed chord, ie
  <e g c'> should produce: C.
  This is only used for chords that are entered as simultaneous notes,
  chords entered in \chord mode are fully defined.
 */

SCM
Chord::guess_tonic (SCM pitches)
{
  return gh_car (scm_sort_list (pitches, Pitch::less_p_proc)); 
} 

SCM
Chord::rebuild_from_base (SCM pitches, SCM base)
{
  SCM split = ly_split_list (base, pitches);
  SCM before = gh_car (split);
  SCM after = gh_cdr (split);

  SCM last = Pitch (0, 0, -5).smobbed_copy ();
  SCM rebuilt = SCM_EOL;
  rebuilt = gh_cons (base, rebuilt);
  for (SCM i = gh_append2 (after, before); gh_pair_p (i); i = gh_cdr (i))
    {
      SCM p = gh_car (i);
      if (Pitch::less_p (p, last) == SCM_BOOL_T)
	{
	  // UHUHUrg
	  p = Pitch (unsmob_pitch (last)->octave_i_,
			     unsmob_pitch (p)->notename_i_,
			     unsmob_pitch (p)->alteration_i_).smobbed_copy ();
	  if (Pitch::less_p (p, last))
	    p = Pitch::transpose (p, Pitch (1, 0, 0).smobbed_copy ());
	}
      rebuilt = gh_cons (p, rebuilt);
      last = p;
    }

  return gh_reverse (rebuilt);
}

SCM
Chord::rebuild_insert_inversion (SCM pitches) //, SCM tonic)
{
  SCM inversion = gh_car (pitches);
  pitches = gh_cdr (pitches);
  SCM tonic = gh_car (pitches);
  pitches = rebuild_from_base (pitches, tonic);
  if (pitches != SCM_EOL)
    {
      // UHUHUrg
      inversion = Pitch (unsmob_pitch (gh_car (pitches))->octave_i_-1,
				 unsmob_pitch (inversion)->notename_i_,
				 unsmob_pitch (inversion)->alteration_i_).smobbed_copy ();
      while (Pitch::less_p (inversion, gh_car (pitches)) == SCM_BOOL_T)
	inversion = Pitch::transpose (inversion, Pitch (1, 0, 0).smobbed_copy ());
    }
  pitches = gh_cons (inversion, pitches);
  return scm_sort_list (pitches,
			scm_eval2 (ly_symbol2scm ("Pitch::less_p"),
				   SCM_EOL));
}

SCM
Chord::rebuild_with_bass (SCM pitches, SCM bass)
{
  pitches = ly_remove_member (bass, pitches);
  // is lowering fine, or should others be raised?
  if (pitches != SCM_EOL)
    while (Pitch::less_p (gh_car (pitches), bass) == SCM_BOOL_T)
      bass = Pitch::transpose (bass, Pitch (-1, 0, 0).smobbed_copy ());
  return gh_cons (bass, pitches);
}



/*********************************/
/* Parser stuff */

/* Construct from parser output:

  (PITCHES . (INVERSION . BASS)) */
SCM
Chord::tonic_add_sub_inversion_bass_to_scm (SCM tonic, SCM add, SCM sub,
					    SCM inversion, SCM bass)
{
  SCM less = scm_eval2 (ly_symbol2scm ("Pitch::less_p"), SCM_EOL);

  /* urg: catch dim modifier: 3rd, 5th, 7th, .. should be lowered */
  bool dim_b = false;
  for (SCM i = add; gh_pair_p (i); i = gh_cdr (i))
    {
      Pitch* p = unsmob_pitch (gh_car (i));
      if (p->octave_i ()  == -100)
        {
          p->octave_i_ = 0;
	  dim_b = true;
	}
    }
  add = transpose_pitches (tonic, add);
  add = lower_step (tonic, add, gh_int2scm (7));
  add = scm_sort_list (add, less);
  add = ly_unique (add);
  
  sub = transpose_pitches (tonic, sub);
  sub = lower_step (tonic, sub, gh_int2scm (7));
  sub = scm_sort_list (sub, less);
  
  /* default chord includes upto 5: <1, 3, 5>   */
  add = gh_cons (tonic, add);
  SCM tmp = add;
  
  SCM fifth = ly_last (base_pitches (tonic));
  int highest_step = gh_scm2int (step_scm (tonic, ly_last (tmp)));
  if (highest_step < 5)
    tmp = ly_snoc (fifth, tmp);
  else if (dim_b)
    add = lower_step (tonic, add, gh_int2scm (5));

  /* find missing thirds */
  SCM missing = missing_thirds (tmp);
  if (highest_step < 5)
    missing = ly_snoc (fifth, missing);

  /* if dim modifier is given: lower all missing */
  if (dim_b)
    missing = lower_step (tonic, missing, gh_int2scm (0));
  
  /* if additions include any 3, don't add third */
  SCM third = gh_cadr (base_pitches (tonic));
  if (member_notename (third, add) != SCM_BOOL_F)
    missing = ly_remove_member (third, missing);

  /* if additions include any 4, assume sus4 and don't add third implicitely
     C-sus (4) = c f g (1 4 5) */
  SCM sus = Pitch::transpose (tonic, Pitch (0, 3, 0).smobbed_copy ());
  if (member_notename (sus, add) != SCM_BOOL_F)
    missing = ly_remove_member (third, missing);
  
  /* if additions include some 5, don't add fifth */
  if (member_notename (fifth, add) != SCM_BOOL_F)
    missing = ly_remove_member (fifth, missing);
    
  /* complete the list of thirds to be added */
  add = gh_append2 (missing, add);
  add = scm_sort_list (add, less);
  
  SCM pitches = SCM_EOL;
  /* Add all that aren't subtracted */
  for (SCM i = add; gh_pair_p (i); i = gh_cdr (i))
    {
      SCM p = gh_car (i);
      SCM s = member_notename (p, sub);
      if (s != SCM_BOOL_F)
	sub = ly_remove_member (s, sub);
      else
	pitches = gh_cons (p, pitches);
    }
  pitches = scm_sort_list (pitches, less);
  
  for (SCM i = sub; gh_pair_p (i); i = gh_cdr (i))
    warning (_f ("invalid subtraction: not part of chord: %s",
		 unsmob_pitch (gh_car (i))->str ()));

  return gh_cons (pitches, gh_cons (inversion, bass));
}


/*
  junk me

  snapnie
  
  Een chord invoer bestaat uit een naam.  Maar, we willen een aantal
  pitch-requests doen, zodat na het parsen van een chord geen verschil
  meer is met een gewoon accoord.  Die vertaalslag is behoorlijk
  harig, hoe wil je dit junken?  Nouja, cleanup lijkt me aardige
  eerste stap enniewee.


  --Het lijkt me dat dit in het paarse gedeelte moet.

  Zo-en-zo, lijktme dat je ipv. Inversion_req een (inversion . #t) aan
  de betreffende Noot_req kan hangen
*/

Simultaneous_music *
Chord::get_chord (SCM tonic, SCM add, SCM sub, SCM inversion, SCM bass, SCM dur)
{
  SCM chord = tonic_add_sub_inversion_bass_to_scm (tonic, add, sub,
						   inversion, bass);
						   
  Tonic_req* t = new Tonic_req;
  t->set_mus_property ("pitch",  tonic);
  SCM l = gh_cons (t->self_scm (), SCM_EOL);

  SCM modifiers = gh_cdr (chord);
  inversion = gh_car (modifiers);
  bass = gh_cdr (modifiers);

  //urg
  if (inversion != SCM_EOL)
    {
      Inversion_req* i = new Inversion_req;
      i->set_mus_property ("pitch",  inversion);
      l = gh_cons (i->self_scm (), l);
      scm_unprotect_object (i->self_scm ());
    }

  if (bass != SCM_EOL)
    {
      Bass_req* b = new Bass_req;
      b->set_mus_property ("pitch", bass);

      l = gh_cons (b->self_scm (), l);
      scm_unprotect_object (b->self_scm ());      
    }

  SCM pitches = Chord::to_pitches (chord);
  for (SCM i = pitches; gh_pair_p (i); i = gh_cdr (i))
    {
      Note_req* n = new Note_req;
      n->set_mus_property ("pitch", gh_car (i));
      n->set_mus_property ("duration", dur);
      l = gh_cons (n->self_scm (), l);

      scm_unprotect_object (n->self_scm ());
    }

  Simultaneous_music*v = new Request_chord (l);

  return v;
}


