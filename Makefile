retome.osk: retome.zip
	mv retome.zip retome.osk

retome.zip: .out
	cd .out && 7z a ../retome.zip *
