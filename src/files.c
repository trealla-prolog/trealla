#include <stdbool.h>
#include <stdlib.h>

#include "tpl_features.h"
#include "files.h"
#include "threads.h"

#if defined(_WIN32) || defined(__wasi__)
extern char *realpath(const char *path, char *resolved_path);
#endif

char *tpl_realpath(const char *path)
{
#if !TPL_FEATURE_FILESYSTEM
	(void)path;
	return NULL;
#elif defined(_WIN32) || defined(__wasi__)
	// Trealla supplies realpath() on these targets and its NULL-buffer form
	// already uses the TPL allocation family.
	return realpath(path, NULL);
#else
	// POSIX owns the allocation returned by realpath(path, NULL). Transfer
	// the text into Trealla-owned storage at the boundary.
	char *system_path = realpath(path, NULL);

	if (!system_path)
		return NULL;

	char *path_copy = TPL_strdup(system_path);
	free(system_path);
	return path_copy;
#endif
}
