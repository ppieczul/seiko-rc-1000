/*
 * This is to read an input file of notes, alarms, etc. and send it to a
 * Seiko RC-1000 watch.
 * Rob Braun <bbraun@synack.net> 4/12/2015
 * Updated 5/12/2016
 */
#define _XOPEN_SOURCE 500
#define _BSD_SOURCE
#define _DARWIN_C_SOURCE
#define _GNU_SOURCE
#include <stdlib.h>
#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <getopt.h>
#include <arpa/inet.h>
#include <sys/stat.h>
#include <time.h>

#define BUFFERLEN 2051

void prepare_buffer(unsigned char *buffer) {
	int i;
	memset(buffer, 0, BUFFERLEN);
	buffer[1] = 0x4c;

	for(i = 0x1a; i < BUFFERLEN; i += 25) {
		buffer[i] = 0x40;
	}

	buffer[0x1a] = 0x4c;
}

void add_entry(int start, const char *one, const char *two, unsigned char *buffer) {
	unsigned char pad1[12], pad2[12];

	memset(pad1, 0x20, sizeof(pad1));
	memset(pad2, 0x20, sizeof(pad2));
	
	if(one) {
		size_t len = strlen(one);
		if(len > 12) {
			fprintf(stderr, "String %s is too long\n", one);
			return;
		}

		memcpy(pad1, one, len);
	}

	if(two) {
		size_t len = strlen(two);
		if(len > 12) {
			fprintf(stderr, "String %s is too long\n", two);
			return;
		}

		memcpy(pad2, two, len);
	}

	memcpy(buffer + start, pad1, sizeof(pad1));
	memcpy(buffer + start + sizeof(pad1), pad2, sizeof(pad2));
	buffer[start + 24] = 0x64;
	
	return;
	
}

void process_line(char *one, size_t onelen, char *line, size_t linelen) {
	int i;

	memset(one, ' ', onelen);
	one[onelen-1] = '\0';

	if(linelen > onelen-1) {
		linelen = onelen-1;
	}
	memcpy(one, line, linelen);
	if(one[linelen-1] == '\n') {
		one[linelen-1] = ' ';
	}
	for(i = 0; i < linelen; i++) {
		if(one[i] == '\n') {
			one[i] = '\0';
			break;
		}
	}
	return;
}

int main(int argc, char *argv[]) {
	static struct option longopts[] = {
		{ "file", required_argument, NULL, 'f' },
		{ "serialport", required_argument, NULL, 's' },
		{ "raw", required_argument, NULL, 'r' },
		{ NULL, 0, NULL, 0 }
	};
	int ch;
	char *ttydev = NULL;
	char *infile = NULL;
	int ttyfd;
	FILE *infp = NULL;
	unsigned char buffer[BUFFERLEN];
	unsigned char *bufptr = buffer;
	int israw = 0;

	while((ch = getopt_long(argc, argv, "f:s:r:", longopts, NULL)) != -1) {
		switch(ch) {
			case 'f':
				infile = optarg;
				break;
			case 's':
				ttydev = optarg;
				break;
			case 'r':
				infile = optarg;
				israw = 1;
				break;
		}
	}

	if(!ttydev) {
		fprintf(stderr, "No serial port specified\n");
		exit(1);
	}

	if(!infile) {
		fprintf(stderr, "No input file specified\n");
		exit(1);
	}

	if(!israw) {
		prepare_buffer(buffer);
		int offset = 0x1b;

		infp = fopen(infile, "r");
		if(!infp) {
			fprintf(stderr, "Error opening input file %s: %d %s\n", infile, errno, strerror(errno));
			exit(1);
		}
	
		char *line = NULL;
		size_t linelen = 0;
		ssize_t ret = 0;
		int headercnt = 0;
		int ishdr = 1;
		while((ret = getline(&line, &linelen, infp)) > 0) {
			char one[13];
			char two[13];
	
			if(ret == 1) {
				int diff = offset - 0x1A - 1;
				buffer[offset-1] = 0x4c;
				headercnt++;
				buffer[(headercnt+1)*2] = (diff &0xFF00) >> 8;
				buffer[(headercnt+1)*2 + 1] = diff & 0xFF;
				ishdr = 1;
				continue;
			}
	
			process_line(one, sizeof(one), line, linelen);
			
			free(line);
			line = NULL;
			linelen = 0;
			if(getline(&line, &linelen, infp) > 0) {
				process_line(two, sizeof(two), line, linelen);
			}
			free(line);
			line = NULL;
			linelen = 0;

			if(ishdr) {
				if(strcasestr(two, "alarm") != NULL) {
					if(strcasestr(one, "week") != NULL) {
						buffer[(headercnt+1)*2] |= 0x20;
					} else {
						buffer[(headercnt+1)*2] |= 0x10;
					}
				}
				ishdr = 0;
			}
	
			printf("'%s' '%s'\n", one, two);
			add_entry(offset, one, two, buffer);
			offset += 25;
		}
		buffer[(headercnt+2)*2] = 0x4c;
		buffer[offset-1] = 0x4c;
		fclose(infp);
	} else {
		int fd = open(infile, O_RDONLY);
		if(fd < 0) {
			printf("Error opening raw file %s: %s\n", infile, strerror(errno));
			exit(1);
		}

		read(fd, buffer, sizeof(buffer));
		close(fd);
	} 

	ttyfd = open(ttydev, O_RDWR | O_NOCTTY); if(ttyfd < 0) {
		fprintf(stderr, "Could not open serial port %s: %d %s\n", ttydev, errno, strerror(errno));
		exit(1);
	}

	struct termios opts;
	memset(&opts, 0, sizeof(opts));
	opts.c_iflag = IGNBRK | IGNPAR | ICRNL;
	opts.c_oflag = ONOCR | ONLRET;
	opts.c_cflag = CSTOPB | CS8 | CLOCAL;
	cfsetospeed(&opts, B2400);
	cfmakeraw(&opts);
	tcsetattr(ttyfd, TCSANOW, &opts);

	ssize_t r = write(ttyfd, buffer, sizeof(buffer));
	if(r < 0) {
		printf("write: %s\n", strerror(errno));
	} else if(r == 0) {
		printf("write == 0\n");
	} else if(r < sizeof(buffer)) {
		printf("write only wrote %zd bytes\n", r);
	}
	tcdrain(ttyfd);
	close(ttyfd);

#if DEBUG
	errno = 0;
	ttyfd = open("r1000.bin", O_WRONLY|O_CREAT|O_TRUNC, 0644);
	if(ttyfd < 0) {
		printf("open r1000.bin: %s\n", strerror(errno));
	}
	write(ttyfd, buffer, sizeof(buffer));
	close(ttyfd);
#endif
}
