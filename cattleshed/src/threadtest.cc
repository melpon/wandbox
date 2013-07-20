#include <unistd.h>
#include <pthread.h>
#include <time.h>
#include <stdio.h>

void *start(void *) {
	timespec t;
	t.tv_sec = 2;
	t.tv_nsec = 0;
	puts("hage");
	nanosleep(&t, NULL);
	puts("hige");
	return NULL;
}

int main() {
	pthread_t thr;
	printf("%d\n", pthread_create(&thr, NULL, &start, NULL));
	close(2);
	pthread_join(thr, NULL);
}
