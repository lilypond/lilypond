/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 1997--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#ifndef ENGRAVER_GROUP_HH
#define ENGRAVER_GROUP_HH

#include "engraver.hh"
#include "translator-group.hh"

#include <vector>

class Announce_grob_info : public Grob_info
{
  Direction start_end_;

public:
  Announce_grob_info (Grob_info gi, Direction start_end)
    : Grob_info (gi),
      start_end_ (start_end)
  {
  }
  Direction start_end () const { return start_end_; }
};

struct Preinit_Engraver_group
{
  Drul_array<SCM> acknowledge_hash_table_drul_ {SCM_EOL, SCM_EOL};
};

class Engraver_group : Preinit_Engraver_group, public Translator_group
{
protected:
  std::vector<Announce_grob_info> announce_infos_;
  void override (SCM);
  void revert (SCM);

public:
  OVERRIDE_CLASS_NAME (Engraver_group);
  Engraver_group ();
  void derived_mark () const override;
  void do_announces ();
  void connect_to_context (Context *c) override;
  void disconnect_from_context () override;
  virtual void announce_grob (Grob_info, Direction start_end,
                              Context *reroute_context = 0);
  bool pending_grobs () const;

private:
  virtual void acknowledge_grobs ();
};

typedef void (Engraver::*Engraver_method) (void);

void engraver_each (SCM list, Engraver_method method);

#endif /* ENGRAVERGROUP_HH */
