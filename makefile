all: HAPR

HAPR: HAPR.o DATEOK.o PROJADD.o PROJVIEW.o EDITPROJ.o
	cobc -x -o HAPR HAPR.o DATEOK.o PROJADD.o PROJVIEW.o EDITPROJ.o

HAPR.o: HAPR.COB
	cobc -c -x HAPR.COB

DATEOK.o: DATEOK.COB
	cobc -c DATEOK.COB

PROJADD.o: PROJADD.COB
	cobc -c PROJADD.COB

PROJVIEW.o: PROJVIEW.COB
	cobc -c PROJVIEW.COB

EDITPROJ.o: EDITPROJ.COB
	cobc -c EDITPROJ.COB

clean: 
	rm *.o *~ HAPR