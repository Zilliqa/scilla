all:
	cd src && make all
	
clean:
	cd src && make clean
		
test:
	cd test && make all

test-clean:
	cd test && make clean