struct Rhythmic_staff;

/// column of Rhythmic_staff
struct Rhythmic_column : Staff_column {
    // mega-stupido. only do notes, one at a time
    Request *the_note;
    Rhythmic_staff* staff_;

    /****************/
    
    void typeset_req(Request *rq);
    void take_request(Request *rq);
    void typeset_command(Command *, int brs);
    void process_commands( );
    void process_requests();

    Rhythmic_column(Score_column*s,Rhythmic_staff*rs);
};


/// simple percussion staff
struct Rhythmic_staff : Staff {
    /// indirection to the PStaff.
    PStaff *theline;
    void set_output(PScore *);
    void process_commands( PCursor<Command*> &where);

    void grant_requests();
    Staff_column * create_col(Score_column*);
};

