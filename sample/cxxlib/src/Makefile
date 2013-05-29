libmysample.so: A.o B.o 
	gcc -shared -Wl,-soname,libmysample.so -o libmysample.so A.o B.o 

A.o: A.cpp
	g++ -fPIC -I../include -c A.cpp 

B.o: B.cpp
	g++ -fPIC -I../include -c B.cpp 

