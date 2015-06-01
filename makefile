all:
	dmd -main -unittest -debug -gc -cov safeint.d
	./safeint
