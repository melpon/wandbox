#include <unistd.h>
#include <stdio.h>

int main() {
	const bool blocked = execl("/bin/echo", "/bin/echo", "hoge", (void *)0) == -1;
	puts("blocked");
	return blocked ? 0 : 1;
}
