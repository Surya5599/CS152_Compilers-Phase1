parse: mini_l.lex
	flex mini_l.lex
	gcc -o lexer lex.yy.c -lfl

clean:
	rm -f lex.yy.c *.o lexer
