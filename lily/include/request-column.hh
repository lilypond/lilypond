/*
  request-column.hh -- declare Request_column

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/


#ifndef REQUEST_COLUMN_HH
#define REQUEST_COLUMN_HH
#include "plist.hh"
#include "lily-proto.hh"
#include "moment.hh"
#include "varray.hh"
/**
  Like staff_column, but Score wide. One per when().
 */
class Request_column 
{
    Pointer_list<Staff_column*> staff_cols_;
    Array<Staff_column*> staff_col_l_arr_;
    Moment when_;
    
public:
    Score_column *musical_column_l_, *command_column_l_;
    Request_column(Link_list<Staff*> const& );
    bool used_b()const;
    Moment when();
    void add_reqs(int staff_idx, Array<Request*> const&);
    void update_time(int staff_idx, Time_description &);
    void set_score_cols(Score_column*, Score_column*);
};

#endif // REQUEST_COLUMN_HH
