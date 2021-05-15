CC=gcc
FLAGS=-g -Wall
PART1=part_1/main.c
PART2=part_2/main.c
PART3=part_3/main.c

main: $(PART3)
	$(CC) $(FLAGS) $(PART3) -o muc.out
asm: muc.out
	./muc.out $(filter-out $@, $(MAKECMDGOALS))
	gcc *.s -o asm.out
	./get_err.sh
clean:
	rm *.s *.out
