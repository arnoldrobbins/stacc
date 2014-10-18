# makefile for boostrap of stacc

# note that when cleaning, we don't remove st.parse.[ch] since we need them
# for bootstrapping an initial version

DESTDIR=/usr/local/bin
MANSEC=l
CP= cp

stacc: stacc.o hsearch.o
	$(CC) $(CFLAGS) stacc.o hsearch.o -o stacc

install: stacc stacc.1
	$(CP) stacc $(DESTDIR)
	$(CP) stacc.1 /usr/man/man$(MANSEC)/stacc.$(MANSEC)
	
clean:
	rm -f stacc.o hsearch.o

clobber: clean
	rm -f stacc
