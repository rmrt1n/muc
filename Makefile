CC=gcc
FLAGS=-g -Wall
# PART1=part_1/main.c
PART2=part_2/main.c

main: $(PART2)
	$(CC) $(FLAGS) $(PART2) -o muc.out
asm: muc.out
	./muc.out $(filter-out $@, $(MAKECMDGOALS))
	gcc *.s -o asm.out
	./get_err.sh
clean:
	rm *.s *.out
