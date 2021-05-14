CC=gcc
FLAGS=-g -Wall
PART1=part_1/main.c

main: $(PART1)
	$(CC) $(FLAGS) $(PART1) -o muc.out
asm: muc.out
	./muc.out $(filter-out $@, $(MAKECMDGOALS))
	gcc *.s -o asm.out
	./get_err.sh
clean:
	rm *.s *.out
