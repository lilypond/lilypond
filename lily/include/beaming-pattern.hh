/*
  beaming-pattern.hh -- declare beaming-pattern.hh

  source file of the GNU LilyPond music typesetter

  (c) 1999--2008 Han-Wen Nienhuys <hanwen@xs4all.nl>
*/

#ifndef BEAMING_PATTERN_HH
#define BEAMING_PATTERN_HH

#include "std-vector.hh"
#include "moment.hh"
#include "lily-proto.hh"

struct Beaming_options
{
  SCM grouping_;
  bool subdivide_beams_;
  Moment beat_length_;
  Moment measure_length_;

  Beaming_options ();
  void from_context (Context*);  
};

struct Beam_rhythmic_element
{
  Moment start_moment_;
  Drul_array<int> beam_count_drul_;

  int rhythmic_importance_;
  bool invisible_;
  
  Beam_rhythmic_element (Moment, int, bool);
  Beam_rhythmic_element ();

  int count (Direction d) const;
  void de_grace ();
};

/*
  Generate beaming given durations of notes. Beam uses this to
  set_beaming () for each of its stems.
*/
class Beaming_pattern
{
public:
  Beaming_pattern ();
  
  void beamify (Beaming_options const&);
  void de_grace ();
  void add_stem (Moment d, int beams, bool invisible);
  int beamlet_count (int idx, Direction d) const;
  
private:
  vector<Beam_rhythmic_element> infos_;
  Direction flag_direction (Beaming_options const&, vsize) const;
  void find_rhythmic_importance (Beaming_options const&);
  void unbeam_invisible_stems ();
};

#endif /* BEAMING_PATTERN_HH */
