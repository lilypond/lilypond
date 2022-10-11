/*
  This file is part of LilyPond, the GNU music typesetter.

  Copyright (C) 2005--2022 Han-Wen Nienhuys <hanwen@xs4all.nl>

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

#include "smobs.hh"

#include <atomic>

#include <gc/gc.h>

/* Guile runs finalization on a separate thread, so use atomics to
   keep track of counts safely. */
static std::atomic<int> smob_core_count (0);

Smob_core::~Smob_core ()
{
  smob_core_count.fetch_add (-1);
}

Smob_core::Smob_core ()
  : self_scm_ (SCM_UNDEFINED)
{
  smob_core_count.fetch_add (1);
  maybe_grow_heap ();
};

void
Smob_core::maybe_grow_heap ()
{
  /*
    BDWGC has a special case for objects with finalizers

  https://github.com/ivmai/bdwgc/blob/v8.0.4/alloc.c#L1435

  where it decides to not expand the heap if there are more than 500
  objects with finalizers outstanding.

  Since smobs with free functions are implemented with finalizer, we
  always fall into this case.

  The symptom of this problem is that running with GC_PRINT_STATS=1
  will print

    In-use heap: 85% (370824 KiB pointers + 60065 KiB other)
    In-use heap: 85% (370725 KiB pointers + 59737 KiB other)
    ..

  We can reconsider this hack if we have dropped all smob free functions from
  our code base.
  */
  static GC_word last_gc_no;
  GC_word no = GC_get_gc_no ();
  if (no == last_gc_no)
    {
      return;
    }
  last_gc_no = no;

  GC_word size = GC_get_heap_size ();
  GC_word bytes_per_obj = 2000;
  GC_word want_heap = smob_core_count.load () * bytes_per_obj;
  if (size < want_heap)
    {
      GC_expand_hp (want_heap - size);
    }
}

/*
  The CDR contains the actual protected list.
 */
static SCM smob_protection_list = SCM_EOL;

void
init_smob_protection ()
{
  smob_protection_list = scm_cons (SCM_BOOL_F, SCM_EOL);
  scm_gc_protect_object (smob_protection_list);
}
ADD_SCM_INIT_FUNC (init_smob_protection, init_smob_protection);

LY_DEFINE (ly_smob_protects, "ly:smob-protects", 0, 0, 0, (),
           R"(
Return LilyPond's internal smob protection list.
           )")
{
  return scm_is_pair (smob_protection_list) ? scm_cdr (smob_protection_list)
                                            : SCM_EOL;
}

void
protect_smob (SCM smob)
{
  scm_gc_protect_object (smob);
}

void
unprotect_smob (SCM smob)
{
  scm_gc_unprotect_object (smob);
}

Scm_init const *Scm_init::list_ = 0;

void
Scm_init::init ()
{
  for (Scm_init const *p = list_; p; p = p->next_)
    p->fun_ ();
}
