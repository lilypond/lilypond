/*
  page-spacing.cc - implement routines for spacing
  systems vertically on pages

  source file of the GNU LilyPond music typesetter

  (c) 2006 Joe Neeman <joeneeman@gmail.com>
*/

#include "page-spacing.hh"

#include "matrix.hh"
#include "warn.hh"

/*
  A much simplified rods-and-springs problem.
 */
struct Page_spacing
{
  Real force_;
  Real page_height_;
  Real rod_height_;
  Real spring_len_;
  Real inverse_spring_k_;

  Line_details last_line_;

  Page_spacing (Real page_height)
  {
    page_height_ = page_height;
    clear ();
  }

  void calc_force ();

  void append_system (const Line_details &line);
  void prepend_system (const Line_details &line);
  void clear ();
};

void
Page_spacing::calc_force ()
{
  if (rod_height_ + last_line_.bottom_padding_ >= page_height_ || !inverse_spring_k_)
    force_ = infinity_f;
  else
    force_ = (page_height_ - rod_height_ - last_line_.bottom_padding_ - spring_len_) / inverse_spring_k_;
}

void
Page_spacing::append_system (const Line_details &line)
{
  rod_height_ += last_line_.padding_;

  rod_height_ += line.extent_.length ();
  spring_len_ += line.space_;
  inverse_spring_k_ += line.inverse_hooke_;

  last_line_ = line;

  calc_force ();
}

void
Page_spacing::prepend_system (const Line_details &line)
{
  if (rod_height_)
    rod_height_ += line.padding_;
  else
    last_line_ = line;

  rod_height_ += line.extent_.length ();
  spring_len_ += line.space_;
  inverse_spring_k_ += line.inverse_hooke_;

  calc_force ();
}

void
Page_spacing::clear ()
{
  force_ = rod_height_ = spring_len_ = 0;
  inverse_spring_k_ = 0;
}

/* for each forbidden page break, merge the systems around it into one system. */
static vector<Line_details>
compress_lines (const vector<Line_details> &orig)
{
  vector<Line_details> ret;

  for (vsize i = 0; i < orig.size (); i++)
    {
      if (ret.size () && ret.back ().page_permission_ == SCM_EOL)
	{
	  Line_details const &old = ret.back ();
	  Line_details compressed = orig[i];
	  compressed.extent_[DOWN] = old.extent_[DOWN];
	  compressed.extent_[UP] = old.extent_[UP] + orig[i].extent_.length () + old.padding_;
	  compressed.space_ += old.space_;
	  compressed.inverse_hooke_ += old.inverse_hooke_;

	  /* we don't need the force_ field for the vertical spacing,
	     so we use force_ = n to signal that the line was compressed,
	     reducing the number of lines by n (and force_ = 0 otherwise).
	     This makes uncompression much easier. */
	  compressed.force_ = old.force_ + 1;
	  ret.back () = compressed;
	}
      else
	{
	  ret.push_back (orig[i]);
	  ret.back ().force_ = 0;
	}
    }
  return ret;
}

/* translate the number of systems-per-page into something meaningful for
   the uncompressed lines.
*/
static vector<vsize>
uncompress_solution (vector<vsize> const &systems_per_page,
		     vector<Line_details> const &compressed)
{
  vector<vsize> ret;
  vsize start_sys = 0;

  for (vsize i = 0; i < systems_per_page.size (); i++)
    {
      int compressed_count = 0;
      for (vsize j = start_sys; j < start_sys + systems_per_page[i]; j++)
	compressed_count += (int)compressed[j].force_;

      ret.push_back (systems_per_page[i] + compressed_count);
      start_sys += systems_per_page[i];
    }
  return ret;
}

/* the cases for page_count = 1 or 2 can be done in O(n) time. Since they
   are by far the most common cases, we have special functions for them */
static Spacing_result
space_systems_on_1_page (vector<Line_details> const &lines, Real page_height, bool ragged)
{
  Page_spacing space (page_height);
  Spacing_result ret;

  for (vsize i = 0; i < lines.size (); i++)
    space.append_system (lines[i]);

  ret.systems_per_page_.push_back (lines.size ());
  ret.force_.push_back (ragged ? min (space.force_, 0.0) : space.force_);
  ret.penalty_ = lines.back ().page_penalty_ + lines.back ().turn_penalty_;
  ret.demerits_ = ret.force_.back () * ret.force_.back () + ret.penalty_;

  return ret;
}

static Spacing_result
space_systems_on_2_pages (vector<Line_details> const &lines,
			  Real page_height,
			  bool ragged,
			  bool ragged_last)
{
  /* if there is a forced break, this reduces to 2 1-page problems */
  for (vsize i = 0; i < lines.size () - 1; i++)
    if (lines[i].page_permission_ == ly_symbol2scm ("force"))
      {
	vector<Line_details> lines1 (lines.begin (), lines.begin () + i + 1);
	vector<Line_details> lines2 (lines.begin () + i + 1, lines.end ());
	Spacing_result p1 = space_systems_on_1_page (lines1, page_height, ragged);
	Spacing_result p2 = space_systems_on_1_page (lines2, page_height, ragged || ragged_last);

	p1.systems_per_page_.push_back (p2.systems_per_page_[0]);
	p1.force_.push_back (p2.force_[0]);
	p1.penalty_ += p2.penalty_ - lines[i].turn_penalty_;
	p1.demerits_ += p2.demerits_ - lines[i].turn_penalty_;
	return p1;
      }

  vector<Real> page1_force;
  vector<Real> page2_force;
  Page_spacing page1 (page_height);
  Page_spacing page2 (page_height);

  page1_force.resize (lines.size () - 1, infinity_f);
  page2_force.resize (lines.size () - 1, infinity_f);

  for (vsize i = 0; i < page1_force.size (); i++)
    {
      page1.append_system (lines[i]);
      page2.prepend_system (lines[lines.size () - 1 - i]);
      page1_force[i] = (ragged && page1.force_ < 0 && i > 0) ? infinity_f : page1.force_;

      if (ragged || ragged_last)
	page2_force[page2_force.size () - 1 - i] =
	  (page2.force_ < 0 && i < page1_force.size () - 1) ? infinity_f : 0;
      else
	page2_force[page2_force.size () - 1 - i] = page2.force_;
    }

  vsize best_sys_count = 1;
  Real best_demerits = infinity_f;
  for (vsize i = 0; i < page1_force.size (); i++)
    {
      Real dem = page1_force[i] * page1_force[i]
	+ page2_force[i] * page2_force[i]
	+ lines[i+1].page_penalty_
	+ lines.back ().page_penalty_ + lines.back ().turn_penalty_;
      if (dem < best_demerits)
	{
	  best_demerits = dem;
	  best_sys_count = i+1;
	}
    }

  Spacing_result ret;
  ret.systems_per_page_.push_back (best_sys_count);
  ret.systems_per_page_.push_back (lines.size () - best_sys_count);
  ret.force_.push_back (page1_force[best_sys_count-1]);
  ret.force_.push_back (page2_force[best_sys_count-1]);
  ret.penalty_ = lines[best_sys_count-1].page_penalty_
    + lines.back ().page_penalty_
    + lines.back ().turn_penalty_;
  ret.demerits_ = best_demerits;

  return ret;
}

Page_spacer::Page_spacer (vector<Line_details> const &lines, Real page_height, bool ragged, bool ragged_last)
  : lines_ (lines)
{
  page_height_ = page_height;
  max_page_count_ = 0;
  ragged_ = ragged;
  ragged_last_ = ragged_last;
}

Spacing_result
Page_spacer::solve (vsize page_count)
{
  if (page_count > max_page_count_)
    resize (page_count);

  Spacing_result ret;
  ret.force_.resize (page_count);
  ret.systems_per_page_.resize (page_count);

  vsize system = lines_.size () - 1;

  if (isinf (state_.at (system, page_count-1).demerits_))
    {
      programming_error ("tried to space systems on a bad number of pages");
      return Spacing_result (); /* bad number of pages */
    }

  ret.penalty_ = state_.at (system, page_count-1).penalty_
    + lines_.back ().page_penalty_ + lines_.back ().turn_penalty_;

  ret.demerits_ = 0;
  for (vsize p = page_count; p--;)
    {
      assert (system != VPOS);

      Page_spacing_node const &ps = state_.at (system, p);
      ret.force_[p] = ps.force_;
      ret.demerits_ += ps.force_ * ps.force_;
      if (p == 0)
	ret.systems_per_page_[p] = system + 1;
      else
	ret.systems_per_page_[p] = system - ps.prev_;
      system = ps.prev_;
    }
  ret.demerits_ += ret.penalty_;
  return ret;
}

void
Page_spacer::resize (vsize page_count)
{
  assert (page_count > 0);

  if (max_page_count_ >= page_count)
    return;

  state_.resize (lines_.size (), page_count, Page_spacing_node ());
  for (vsize page = max_page_count_; page < page_count; page++)
    for (vsize line = page; line < lines_.size (); line++)
      if (!calc_subproblem (page, line))
	break;

  max_page_count_ = page_count;
}

bool
Page_spacer::calc_subproblem (vsize page, vsize line)
{
  Page_spacing space (page_height_);
  Page_spacing_node &cur = state_.at (line, page);
  bool ragged = ragged_ || (ragged_last_ && line == lines_.size () - 1);

  for (vsize page_start = line+1; page_start > page && page_start--;)
    {
      Page_spacing_node const *prev = page > 0 ? &state_.at (page_start-1, page-1) : 0;

      space.prepend_system (lines_[page_start]);
      if (page_start < line && (isinf (space.force_) || (space.force_ < 0 && ragged)))
	break;

      if (page > 0 || page_start == 0)
	{
	  if (line == lines_.size () - 1 && ragged_last_ && space.force_ > 0)
	    space.force_ = 0;

	  /* we may have to deal with single lines that are taller than a page */
	  if (isinf (space.force_) && page_start == line)
	    space.force_ = -200000;

	  Real dem = fabs (space.force_) + (prev ? prev->demerits_ : 0);
	  Real penalty = 0;
	  if (page_start > 0)
	    penalty = lines_[page_start-1].page_penalty_
	      + (page % 2 == 0) ? lines_[page_start-1].turn_penalty_ : 0;

	  dem += penalty;
	  if (dem < cur.demerits_ || page_start == line)
	    {
	      cur.demerits_ = dem;
	      cur.force_ = space.force_;
	      cur.penalty_ = penalty + (prev ? prev->penalty_ : 0);
	      cur.prev_ = page_start - 1;
	    }
	}

      if (page_start > 0
	  && lines_[page_start-1].page_permission_ == ly_symbol2scm ("force"))
	break;
    }
  return !isinf (cur.demerits_);
}

vsize
min_page_count (vector<Line_details> const &uncompressed_lines,
		Real page_height, bool ragged, bool ragged_last)
{
  vsize ret = 1;
  Real cur_rod_height = 0;
  vector<Line_details> lines = compress_lines (uncompressed_lines);

  assert (lines.size ());
  for (vsize i = lines.size (); i--;)
    {
      bool rag = ragged || (ragged_last && ret == 1);
      Real ext_len = lines[i].extent_.length ();
      Real next_height = cur_rod_height + ext_len
	+ (rag ? lines[i].space_ : 0)
	+ ((cur_rod_height > 0) ? lines[i].padding_: 0);

      if ((next_height > page_height && cur_rod_height > 0)
	  || (i < lines.size () - 1 && lines[i].page_permission_ == ly_symbol2scm ("force")))
	{
	  ret++;
	  cur_rod_height = ext_len + (rag ? lines[i].space_ : 0);
	}
      else
	cur_rod_height = next_height;
    }

  return ret;
}

Spacing_result
space_systems_on_n_pages (vector<Line_details> const &lines,
			  vsize n,
			  Real page_height,
			  bool ragged,
			  bool ragged_last)
{
  vector<Line_details> compressed_lines = compress_lines (lines);
  Spacing_result ret;
  assert (n >= min_page_count (lines, page_height, ragged, ragged_last));

  if (n > compressed_lines.size ())
    return Spacing_result ();
  if (n == 1)
    ret = space_systems_on_1_page (compressed_lines, page_height, ragged || ragged_last);
  else if (n == 2)
    ret = space_systems_on_2_pages (compressed_lines, page_height, ragged, ragged_last);

  Page_spacer ps (compressed_lines, page_height, ragged, ragged_last);
  ret = ps.solve (n);

  ret.systems_per_page_ = uncompress_solution (ret.systems_per_page_, compressed_lines);
  return ret;
}

Spacing_result
space_systems_on_n_or_one_more_pages (vector<Line_details> const &lines,
				      vsize n,
				      Real page_height,
				      Real odd_pages_penalty,
				      bool ragged,
				      bool ragged_last)
{
  Spacing_result n_res = space_systems_on_n_pages (lines, n, page_height, ragged, ragged_last);
  Spacing_result m_res = space_systems_on_n_pages (lines, n+1, page_height, ragged, ragged_last);
  n_res.demerits_ += odd_pages_penalty;
  n_res.force_.back () += odd_pages_penalty;

  if (n_res.demerits_ < m_res.demerits_)
    return n_res;
  return m_res;
}

Spacing_result
space_systems_on_best_pages (vector<Line_details> const &lines,
			     Real page_height,
			     Real odd_pages_penalty,
			     bool ragged,
			     bool ragged_last)
{
  vector<Line_details> compressed_lines = compress_lines (lines);
  vsize min_p_count = min_page_count (compressed_lines, page_height, ragged, ragged_last);

  Page_spacer ps (compressed_lines, page_height, ragged, ragged_last);
  Spacing_result best = ps.solve (min_p_count);
  best.force_.back () += (min_p_count % 2) ? odd_pages_penalty : 0;
  best.demerits_ += (min_p_count % 2) ? odd_pages_penalty : 0;

  for (vsize i = min_p_count+1; i <= compressed_lines.size (); i++)
    {
      Spacing_result cur = ps.solve (i);
      cur.demerits_ += (i % 2) ? odd_pages_penalty : 0;
      if (cur.demerits_ < best.demerits_)
	best = cur;
    }

  best.systems_per_page_ = uncompress_solution (best.systems_per_page_, compressed_lines);
  return best;
}
