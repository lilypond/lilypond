# -*- Makefile-*-
# sourcefiles to be shipped. Also for dependencies

hdr=    qlp.hh linespace.hh qlpsolve.hh\
	pcol.hh proto.hh pstaff.hh  scoreline.hh staffline.hh\
	const.hh glob.hh molecule.hh  boxes.hh pscore.hh item.hh tex.hh\
	request.hh voice.hh command.hh staff.hh  linestaff.hh \
	tstream.hh rhythmstaff.hh\
	parseconstruct.hh  debug.hh globvars.hh keyword.hh\
	misc.hh score.hh notename.hh lexer.hh symtable.hh\
	symbol.hh main.hh dimen.hh paper.hh lookup.hh\
	sccol.hh stcol.hh scommands.hh melodicstaff.hh\
	identifier.hh simplestaff.hh spanner.hh stem.hh\
	notehead.hh leastsquares.hh beam.hh rest.hh\
	swalker.hh bar.hh meter.hh

mycc=   qlp.cc qlpsolve.cc \
	break.cc linespace.cc molecule.cc staffline.cc\
	pscore.cc tex.cc item.cc pcol.cc staff.cc \
	rhythmstaff.cc 	score.cc note.cc  main.cc misc.cc\
	symbol.cc request.cc notename.cc  voice.cc\
	keyword.cc linestaff.cc table.cc command.cc\
	warn.cc debug.cc symtable.cc boxes.cc\
	pstaff.cc  tstream.cc\
	calcideal.cc scores.cc identifier.cc \
	dimen.cc paper.cc lookup.cc scommands.cc\
	sccol.cc stcol.cc getcommands.cc simplestaff.cc\
	melodicstaff.cc simpleprint.cc stem.cc\
	spanner.cc notehead.cc leastsquares.cc beam.cc\
	texbeam.cc rest.cc swalker.cc scoreline.cc\
	simplewalker.cc bar.cc meter.cc\
	template1.cc template2.cc template3.cc\
	version.cc