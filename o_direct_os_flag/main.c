#define _GNU_SOURCE
#include <stdio.h>
#include <fcntl.h>


int main(void){
#ifdef O_DIRECT
	printf("O_DIRECT");
#else
	printf("Otherwise");
#endif
}
