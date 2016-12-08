HAS_ELIXIR=1

include bu.mk

doc::
	${MKDIR_P} doc
	${CP} _doc/* doc

