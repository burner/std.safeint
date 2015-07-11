all:
	dmd -main -unittest -debug -gc -cov std/experimental/safeint.d -ofsafeint
	./safeint
