/*
  rest-collision.cc -- implement Rest_collision

  source file of the GNU LilyPond music typesetter

  (c) 1997 Han-Wen Nienhuys <hanwen@stack.nl>
*/

#include "rest-collision.hh"
#include "rest-column.hh"
#include "collision.hh"

void
Rest_collision::add(Rest_column *rc_l)
{
    rest_l_arr_.push(rc_l);
    add_dependency(rc_l);
}
void
Rest_collision::add(Collision * c_l)
{
    add_dependency(c_l);
    for (int i=0; i < c_l->clash_l_arr_.size(); i ++)
	ncol_l_arr_.push(c_l->clash_l_arr_[i]);
}


void
Rest_collision::do_post_processing()
{
#if 0
        bool rest_b_a[4];
	rest_b_a[j] = (col_l_a[j]) ? col_l_a[j]->rest_b_ : false;	
    do {
	int i1 = idx(d, false);
	int i2 = idx(d,true);
	if (!intersection(y_extent[i1] , 
			  y_extent[i2]).empty_b()) {
	    if (rest_b_a[i1]) {
		y_off[i1] = -y_extent[i1][-d] + y_extent[1][d] + d*4; // ugh
		y_extent[i1] += y_off[i1];
	    }
	}
    } while ((d *= -1) != 1);

    do {
	int i1 = idx(d, false);
	int i2 = idx(-d,false);
	
	if (d*(y_extent[i1][-d] - y_extent[i2][d] )< 0&& rest_b_a[i1]) {
	    y_off[i1] = -y_extent[i1][-d] + y_extent[i2][d] +d* 4; // ugh
	    y_extent[i1] += y_off[i1];
	}
    } while ((d *= -1) != 1);
    
#endif
}
IMPLEMENT_STATIC_NAME(Rest_collision);
void
Rest_collision::do_substitute_dependency(Score_elem*o,Score_elem*n)
{
    Item*o_l = o->item();
    Item*n_l = n?n->item():0;
    
    rest_l_arr_.substitute((Rest_column*)o_l,(Rest_column*)n_l);
    ncol_l_arr_.substitute((Note_column*)o_l,(Note_column*)n_l);
}
