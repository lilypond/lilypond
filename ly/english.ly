%{
     English note names spelled out in full, and abbreviated

         ff  for double-flat
			tqf for three-quarters flat
			f   for flat
			qf  for quarter-flat
			
			qs  for quarter-sharp
			s   for sharp
			tqs for three-quarters sharp
			x   for double-sharp
%}

pitchnamesEnglish = #`(
	(cflatflat . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(cflat . ,(ly:make-pitch -1 0 FLAT))
	(c . ,(ly:make-pitch -1 0 NATURAL))
	(csharp . ,(ly:make-pitch -1 0 SHARP))
	(csharpsharp . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(dflatflat . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(dflat . ,(ly:make-pitch -1 1 FLAT))
	(d . ,(ly:make-pitch -1 1 NATURAL))
	(dsharp . ,(ly:make-pitch -1 1 SHARP))
	(dsharpsharp . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(eflatflat . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(eflat . ,(ly:make-pitch -1 2 FLAT))
	(e . ,(ly:make-pitch -1 2 NATURAL))
	(esharp . ,(ly:make-pitch -1 2 SHARP))
	(esharpsharp . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(fflatflat . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(fflat . ,(ly:make-pitch -1 3 FLAT))
	(f . ,(ly:make-pitch -1 3 NATURAL))
	(fsharp . ,(ly:make-pitch -1 3 SHARP))
	(fsharpsharp . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(gflatflat . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(gflat . ,(ly:make-pitch -1 4 FLAT))
	(g . ,(ly:make-pitch -1 4 NATURAL))
	(gsharp . ,(ly:make-pitch -1 4 SHARP))
	(gsharpsharp . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(aflatflat . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(aflat . ,(ly:make-pitch -1 5 FLAT))
	(a . ,(ly:make-pitch -1 5 NATURAL))
	(asharp . ,(ly:make-pitch -1 5 SHARP))
	(asharpsharp . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(bflatflat . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(bflat . ,(ly:make-pitch -1 6 FLAT))
	(b . ,(ly:make-pitch -1 6 NATURAL))
	(bsharp . ,(ly:make-pitch -1 6 SHARP))
	(bsharpsharp . ,(ly:make-pitch -1 6 DOUBLE-SHARP))

	(cff . ,(ly:make-pitch -1 0 DOUBLE-FLAT))
	(ctqf . ,(ly:make-pitch -1 0 THREE-Q-FLAT))
	(cf . ,(ly:make-pitch -1 0 FLAT))
	(cqf . ,(ly:make-pitch -1 0 SEMI-FLAT))
	(c . ,(ly:make-pitch -1 0 NATURAL))
	(cqs . ,(ly:make-pitch -1 0 SEMI-SHARP))
	(cs . ,(ly:make-pitch -1 0 SHARP))
	(ctqs . ,(ly:make-pitch -1 0 THREE-Q-SHARP))
	(css . ,(ly:make-pitch -1 0 DOUBLE-SHARP))
	(cx . ,(ly:make-pitch -1 0 DOUBLE-SHARP))

	(dff . ,(ly:make-pitch -1 1 DOUBLE-FLAT))
	(dtqf . ,(ly:make-pitch -1 1 THREE-Q-FLAT))
	(df . ,(ly:make-pitch -1 1 FLAT))
	(dqf . ,(ly:make-pitch -1 1 SEMI-FLAT))
	(d . ,(ly:make-pitch -1 1 NATURAL))
	(dqs . ,(ly:make-pitch -1 1 SEMI-SHARP))
	(ds . ,(ly:make-pitch -1 1 SHARP))
	(dtqs . ,(ly:make-pitch -1 1 THREE-Q-SHARP))
	(dss . ,(ly:make-pitch -1 1 DOUBLE-SHARP))
	(dx . ,(ly:make-pitch -1 1 DOUBLE-SHARP))

	(eff . ,(ly:make-pitch -1 2 DOUBLE-FLAT))
	(etqf . ,(ly:make-pitch -1 2 THREE-Q-FLAT))
	(ef . ,(ly:make-pitch -1 2 FLAT))
	(eqf . ,(ly:make-pitch -1 2 SEMI-FLAT))
	(e . ,(ly:make-pitch -1 2 NATURAL))
	(eqs . ,(ly:make-pitch -1 2 SEMI-SHARP))
	(es . ,(ly:make-pitch -1 2 SHARP))
	(etqs . ,(ly:make-pitch -1 2 THREE-Q-SHARP))
	(ess . ,(ly:make-pitch -1 2 DOUBLE-SHARP))
	(ex . ,(ly:make-pitch -1 2 DOUBLE-SHARP))

	(fff . ,(ly:make-pitch -1 3 DOUBLE-FLAT))
	(ftqf . ,(ly:make-pitch -1 3 THREE-Q-FLAT))
	(ff . ,(ly:make-pitch -1 3 FLAT))
	(fqf . ,(ly:make-pitch -1 3 SEMI-FLAT))
	(f . ,(ly:make-pitch -1 3 NATURAL))
	(fqs . ,(ly:make-pitch -1 3 SEMI-SHARP))
	(fs . ,(ly:make-pitch -1 3 SHARP))
	(ftqs . ,(ly:make-pitch -1 3 THREE-Q-SHARP))
	(fss . ,(ly:make-pitch -1 3 DOUBLE-SHARP))
	(fx . ,(ly:make-pitch -1 3 DOUBLE-SHARP))

	(gff . ,(ly:make-pitch -1 4 DOUBLE-FLAT))
	(gtqf . ,(ly:make-pitch -1 4 THREE-Q-FLAT))
	(gf . ,(ly:make-pitch -1 4 FLAT))
	(gqf . ,(ly:make-pitch -1 4 SEMI-FLAT))
	(g . ,(ly:make-pitch -1 4 NATURAL))
	(gqs . ,(ly:make-pitch -1 4 SEMI-SHARP))
	(gs . ,(ly:make-pitch -1 4 SHARP))
	(gtqs . ,(ly:make-pitch -1 4 THREE-Q-SHARP))
	(gss . ,(ly:make-pitch -1 4 DOUBLE-SHARP))
	(gx . ,(ly:make-pitch -1 4 DOUBLE-SHARP))

	(aff . ,(ly:make-pitch -1 5 DOUBLE-FLAT))
	(atqf . ,(ly:make-pitch -1 5 THREE-Q-FLAT))
	(af . ,(ly:make-pitch -1 5 FLAT))
	(aqf . ,(ly:make-pitch -1 5 SEMI-FLAT))
	(a . ,(ly:make-pitch -1 5 NATURAL))
	(aqs . ,(ly:make-pitch -1 5 SEMI-SHARP))
	(as . ,(ly:make-pitch -1 5 SHARP))
	(atqs . ,(ly:make-pitch -1 5 THREE-Q-SHARP))
	(ass . ,(ly:make-pitch -1 5 DOUBLE-SHARP))
	(ax . ,(ly:make-pitch -1 5 DOUBLE-SHARP))

	(bff . ,(ly:make-pitch -1 6 DOUBLE-FLAT))
	(btqf . ,(ly:make-pitch -1 6 THREE-Q-FLAT))
	(bf . ,(ly:make-pitch -1 6 FLAT))
	(bqf . ,(ly:make-pitch -1 6 SEMI-FLAT))
	(b . ,(ly:make-pitch -1 6 NATURAL))
	(bqs . ,(ly:make-pitch -1 6 SEMI-SHARP))
	(bs . ,(ly:make-pitch -1 6 SHARP))
	(btqs . ,(ly:make-pitch -1 6 THREE-Q-SHARP))
	(bss . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
	(bx . ,(ly:make-pitch -1 6 DOUBLE-SHARP))
)

pitchnames = \pitchnamesEnglish

\version "2.12.0"


#(ly:parser-set-note-names parser pitchnames)
