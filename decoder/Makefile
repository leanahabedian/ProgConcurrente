
NULL =

SOURCES = \
  configuration.ml \
  ast.ml \
  specification.ml \
  parser.mli \
  parser.ml \
  lexer.ml \
  decoder.ml \
  $(NULL)

NAME = rosetta

$(NAME): $(SOURCES)
	ocamlc -o $@ $^

parser.ml parser.mli parser.output: parser.mly
	ocamlyacc -v $<

lexer.ml: lexer.mll
	ocamllex $<

.PHONY: clean
clean:
	rm -f parser.{ml,mli,output} $(NAME)
