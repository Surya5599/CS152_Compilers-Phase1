parser: phase3.lex phase3.y
	bison -v -d --file-prefix=y phase3.y
	flex phase3.lex
	g++ -std=c++11 -o parser y.tab.c lex.yy.c -lfl 
clean:
	rm -f lex.yy.c y.tab.* y.output *.o parser lex.yy.cc stack.hh
