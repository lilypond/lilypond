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

mycc=bar.cc beam.cc boxes.cc break.cc calcideal.cc clef.cc\
	clefitem.cc command.cc debug.cc dimen.cc directionalspanner.cc\
	getcommands.cc grouping.cc idealspacing.cc identifier.cc\
	inputcommand.cc inputcommands.cc inputmusic.cc inputscore.cc\
	inputstaff.cc item.cc key.cc keyitem.cc keyword.cc\
	leastsquares.cc lexerinit.cc linespace.cc linestaff.cc\
	localkeyitem.cc lookup.cc main.cc melodicstaff.cc meter.cc\
	misc.cc molecule.cc mylexer.cc note.cc notehead.cc notename.cc\
	paper.cc pcol.cc pscore.cc pstaff.cc qlp.cc qlpsolve.cc\
	request.cc rest.cc rhythmstaff.cc sccol.cc score.cc\
	scoreline.cc scores.cc script.cc scriptdef.cc simpleprint.cc\
	simplestaff.cc simplewalker.cc slur.cc spanner.cc staff.cc\
	staffcommands.cc staffelem.cc staffline.cc stcol.cc stem.cc\
	swalker.cc symbol.cc symtable.cc table.cc tex.cc texbeam.cc\
	texslur.cc textdef.cc textitem.cc textspanner.cc\
	timedescription.cc tstream.cc voice.cc warn.cc wordwrap.cc\
	template1.cc template2.cc template3.cc template4.cc\
	template5.cc version.cc