##
## EPITECH PROJECT, 2023
## GlaDos
## File description:
## Makefile
##

NAME	=	glados
STACK	=	stack
RM		=	rm -f

all:        $(NAME)

$(NAME):
	$(STACK) build
	find . -type f -name $(NAME) -exec mv {} . \;

clean:
	$(RM) $(SRC:.hs=.hi)
	$(RM) $(SRC:.hs=.o)

fclean: clean
	$(RM) $(NAME)
	$(RM) -r .stack-work
	$(RM) -rf test/coverage

re: fclean all

tests_run:
	$(STACK) test --coverage
	$(STACK) hpc report --all --destdir test/coverage

.PHONY: all clean fclean re tests_run