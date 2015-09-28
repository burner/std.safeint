all:
	dmd -main -unittest -debug -g std/experimental/safeint.d -ofsafeint
	./safeint
