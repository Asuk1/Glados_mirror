##
## EPITECH PROJECT, 2023
## Glados
## File description:
## Makefile
##

GLADOS_DIRECTORY = Glados

GLADOS_EXECUTABLE = $(GLADOS_DIRECTORY)/glados

GLADOS_MAKEFILE = $(GLADOS_DIRECTORY)/Makefile

all:
	$(MAKE) -C $(GLADOS_DIRECTORY)
	@mv $(GLADOS_EXECUTABLE) .

clean:
	$(MAKE) -C $(GLADOS_DIRECTORY) clean

fclean:
	$(MAKE) -C $(GLADOS_DIRECTORY) fclean
	@rm -f glados

re:
	$(MAKE) -C $(GLADOS_DIRECTORY) re
	@mv $(GLADOS_EXECUTABLE) .

tests_run:
	$(MAKE) -C $(GLADOS_DIRECTORY) tests_run

.PHONY: all clean fclean re tests_run