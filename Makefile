TARGETS=ExprYacc.cmx TestHack.native TestWithAst.native
all:
		ocamlbuild -use-ocamlfind $(TARGETS)

clean:
		rm -fr *~ *.s *.cm[oixa] $(OUT) _build Lexer.ml Parser.ml Parser.mli *.o HelloWorld.class LexerExpr.ml ExprYacc.ml ExprYacc.mli



