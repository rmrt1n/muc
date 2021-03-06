CC=gcc
FLAGS=-g -Wall
SRC=part_9/main.c

main: $(SRC)
	$(CC) $(FLAGS) $(SRC) -o muc.out
asm: muc.out
	./muc.out $(filter-out $@, $(MAKECMDGOALS))
	gcc *.s -o asm.out
	./get_err.sh
clean:
	rm *.s *.c *.out
