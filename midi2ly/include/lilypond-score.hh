//
// lilypond-score.hh -- declare Lilypond_score
//
// (c) 1997--2001 Jan Nieuwenhuizen <janneke@gnu.org>

#ifndef LILYPOND_SCORE_HH
#define LILYPOND_SCORE_HH

#include "midi2ly-proto.hh"
#include "flower-proto.hh"
#include "cons.hh"
#include "parray.hh"

/// (lilypond_score)
class Lilypond_score {
public:
  Lilypond_score (int format_i, int tracks_i, int tempo_i);
  ~Lilypond_score ();

  void add_item (Lilypond_item* lilypond_item_p);
  void add_staff (Lilypond_staff* lilypond_staff_p);

  Lilypond_column* find_column_l (Rational mom);
  Lilypond_column* get_column_l (Rational mom);

  void output (String filename_str);
  void process ();

  // ugh
  Lilypond_key* lilypond_key_l_;
  Lilypond_time_signature* lilypond_time_signature_l_;
  Lilypond_tempo* lilypond_tempo_l_;
  Lilypond_staff * last_staff_l_;
private:
  void filter_tempo ();
  void quantify_columns ();
  void quantify_durations ();
  void settle_columns ();

  Cons_list<Lilypond_staff> lilypond_staff_p_list_;
  Link_array<Lilypond_column> column_l_array_;

  // ugh, ugh, ugh
public:
  int format_i_;
  int tracks_i_;
  int tempo_i_;
};

#endif // LILYPOND_SCORE_HH

