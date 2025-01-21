NAME		=	lazyp-demo

BIN_DIR		=	$(shell stack path --local-install-root)/bin

$(NAME): build
	cp $(BIN_DIR)/gui ./$(NAME)

build:
	stack build

all: $(NAME)

clean:
	stack clean

fclean: clean
	rm -f $(NAME)

re: fclean $(NAME)

unit_tests:
	stack test

coverage:
	stack test --coverage

.PHONY: all clean fclean re unit_tests coverage
