
#define iterator(set)		typeof((set).top())
#define iterator_bot(set)		typeof((set).bottom())

// should use top()
#define iter_top(set,var)	iterator(set) var(set)
#define iter_bot(set,var)	iterator(set) var(set.bottom())

