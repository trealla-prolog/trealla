#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static char *symbol_for(const char *name)
{
	size_t len = strlen(name);
	char *symbol = malloc(len + 12);

	if (!symbol)
		return NULL;

	strcpy(symbol, "library_");
	char *dst = symbol + 8;

	for (const char *src = name; *src; src++)
		*dst++ = ((*src >= 'a') && (*src <= 'z'))
			|| ((*src >= 'A') && (*src <= 'Z'))
			|| ((*src >= '0') && (*src <= '9')) ? *src : '_';

	strcpy(dst, "_pl");
	return symbol;
}

static const char *logical_name(const char *name)
{
	const char *slash = strrchr(name, '/');
	return slash ? slash + 1 : name;
}

int main(int argc, char **argv)
{
	if (argc < 2) {
		fprintf(stderr, "usage: %s library [...]\n", argv[0]);
		return 1;
	}

	puts("#include \"library.h\"\n");

	for (int i = 1; i < argc; i++) {
		char *symbol = symbol_for(argv[i]);

		if (!symbol)
			return 1;

		printf("extern unsigned char %s[];\n", symbol);
		printf("extern unsigned int %s_len;\n", symbol);
		free(symbol);
	}

	puts("\nlibrary g_libs[] = {");

	for (int i = 1; i < argc; i++) {
		char *symbol = symbol_for(argv[i]);

		if (!symbol)
			return 1;

		printf("\t{\"%s\", %s, &%s_len},\n", logical_name(argv[i]), symbol, symbol);
		free(symbol);
	}

	puts("\t{0}\n};");
	return 0;
}
