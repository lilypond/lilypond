# -*- Makefile-*-
# sourcefiles to be shipped. Also for dependencies

hdr=    qlp.hh linespace.hh qlpsolve.hh\
	pcol.hh proto.hh pstaff.hh  scoreline.hh staffline.hh\
	const.hh glob.hh molecule.hh  boxes.hh pscore.hh item.hh tex.hh\
	request.hh voice.hh command.hh staff.hh  linestaff.hh \
	tstream.hh rhythmstaff.hh\
	parseconstruct.hh debug.hh globvars.hh keyword.hh\
	misc.hh score.hh notename.hh lexer.hh symtable.hh\
	symbol.hh main.hh dimen.hh paper.hh lookup.hh\
	spanner.hh  beam.hh directionalspanner.hh slur.hh textspanner.hh\
	sccol.hh stcol.hh staffcommands.hh melodicstaff.hh\
	identifier.hh simplestaff.hh  stem.hh\
	notehead.hh leastsquares.hh  rest.hh\
	swalker.hh bar.hh meter.hh accidental.hh\
	key.hh keyitem.hh localkeyitem.hh simplewalker.hh\
	clef.hh clefitem.hh  inputcommands.hh\
	getcommand.hh inputmusic.hh timedescription.hh\
	inputscore.hh inputstaff.hh identparent.hh\
	inputcommand.hh grouping.hh moment.hh offset.hh\
	staffelem.hh idealspacing.hh break.hh\
	scriptdef.hh script.hh textdef.hh textitem.hh

mycc=   qlp.cc qlpsolve.cc leastsquares.cc\
	inputcommands.cc inputmusic.cc	\
	inputscore.cc inputstaff.cc inputcommand.cc\
	timedescription.cc\
	break.cc linespace.cc molecule.cc staffline.cc\
	pscore.cc tex.cc item.cc pcol.cc staff.cc \
	rhythmstaff.cc 	score.cc note.cc  main.cc misc.cc\
	symbol.cc request.cc notename.cc  voice.cc\
	keyword.cc linestaff.cc table.cc command.cc\
	warn.cc debug.cc symtable.cc boxes.cc\
	pstaff.cc  tstream.cc\
	calcideal.cc scores.cc  \
	dimen.cc paper.cc lookup.cc staffcommands.cc\
	sccol.cc stcol.cc getcommands.cc simplestaff.cc\
	melodicstaff.cc simpleprint.cc\
	spanner.cc textspanner.cc directionalspanner.cc \
	notehead.cc  stem.cc \
	rest.cc bar.cc meter.cc keyitem.cc localkeyitem.cc clefitem.cc\
	swalker.cc scoreline.cc\
	simplewalker.cc scriptdef.cc script.cc\
	texbeam.cc texslur.cc clef.cc key.cc slur.cc beam.cc\
	idealspacing.cc  grouping.cc identifier.cc\
	lexerinit.cc mylexer.cc textdef.cc textitem.cc\
	staffelem.cc wordwrap.cc\
	template1.cc template2.cc template3.cc template4.cc\
	template5.cc\
	version.cc



