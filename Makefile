retome.osk: retome.zip
	mv retome.zip retome.osk

retome.zip: _out
	cd .out && 7z a ../retome.zip *

_out:
	racket mkosuskin.rkt -m ja -m external -r dev
