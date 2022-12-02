all: clean test-advanced test-simple

dev-advanced:
	cd ../; ghcid day__day__/advanced.hs --setup ":set args day__day__/input/advanced-example.txt" --test main

dev-simple:
	cd ../; ghcid day__day__/simple.hs --setup ":set args day__day__/input/simple-example.txt" --test main

run-advanced:
	cd ../; runhaskell day__day__/advanced.hs day__day__/input/advanced.txt

run-simple:
	cd ../; runhaskell day__day__/simple.hs day__day__/input/simple.txt

pretest:
	mkdir -p test/

test-advanced: pretest
	make -s run-advanced > test/output-advanced.log
	diff -w output/advanced.txt test/output-advanced.log

test-simple: pretest
	make -s run-simple > test/output-simple.log
	diff -w output/simple.txt test/output-simple.log
