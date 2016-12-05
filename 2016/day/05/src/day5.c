#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <openssl/md5.h>

#define BUFFERSIZE 512

#define PASSWORD1SIZE 8
#define PASSWORD2SIZE 8

/* Lifted mostly wholesale from http://stackoverflow.com/a/8389763
 * Upvotes for todd.
 */
char *str2md5(const char *inputStr, char *outputStr, int length) {
	int n;
	MD5_CTX c;
	unsigned char digest[16];
	char *out = outputStr;

	MD5_Init(&c);

	while (length > 0) {
		if (length > 512) {
			MD5_Update(&c, inputStr, 512);
		} else {
			MD5_Update(&c, inputStr, length);
		}
		length -= 512;
		inputStr += 512;
	}

	MD5_Final(digest, &c);

	for (n = 0; n < 16; ++n) {
		snprintf(&(out[n*2]), 16*2, "%02x", (unsigned int)digest[n]);
	}

	return out;
}

/* I wrote the main() function though */
int main() {
	unsigned int counter1 = 0, counter2 = 0;
	unsigned p1counter = 0, p2flag = 0;
	int p2index = 0;
	char *inputStr = "reyedfim";
	char input[BUFFERSIZE];
	char output[33];
	char password1[PASSWORD1SIZE+1];
	char password2[PASSWORD2SIZE+1];

	memset(password1, 0, PASSWORD1SIZE+1);
	memset(password2, 0, PASSWORD2SIZE+1);

	for (counter1 = 0; counter1 < UINT_MAX; counter1++) {
		memset(input, 0, BUFFERSIZE);
		memset(output, 0, BUFFERSIZE);

		sprintf(input, "%s%u", inputStr, counter1);
		str2md5(input, output, strlen(input));

		if (!strncmp(output, "00000", 5)) {
			printf("Input: %s, Hash: %s.\t", input, output);
			printf("MATCH FOUND!\n");

			if (p1counter < PASSWORD1SIZE) {
				password1[p1counter] = output[5];
				p1counter++;
			}

			p2index = output[5] - 48;
			if ((p2index >= 0) && (p2index <= 7) && (!password2[p2index])) {
				password2[p2index] = output[6];
			}
		}
		else {
			/* printf("No match.\n"); */
			continue;
		}

		p2flag = 1;
		for (counter2 = 0; counter2 < PASSWORD2SIZE; counter2++) {
			if (!password2[counter2]) {
				p2flag = 0;
				break;
			}
		}
		if ((p1counter >= PASSWORD1SIZE) && (p2flag)) {
			printf("Done searching.\n");
			printf("Part 1 password: %s\n", password1);
			printf("Part 2 password: %s\n", password2);
			break;
		}
	}

	return 0;
}
