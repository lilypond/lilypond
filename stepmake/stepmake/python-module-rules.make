# Different versions of Python compile to different places.  We ignore
# those details and base build dependencies on the *.py script alone.
# Whenever the script is updated, we also try to compile it.  We rely
# on GNU make's DELETE_ON_ERROR option to remove the script if it
# doesn't compile.
$(outdir)/%.py: %.py $(config_make) $(depth)/VERSION
	$(call ly_progress,Making,$@,(sed))
	sed $(sed-atfiles) < $< | sed $(sed-atvariables) > $@
	PYTHONOPTIMIZE= $(PYTHON) -c 'import py_compile; py_compile.compile ("$@", doraise=True)'
	chmod 755 $@
