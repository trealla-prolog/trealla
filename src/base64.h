#pragma once

// If user specified '*pdst' buffer, have to trust the length is
// enough and won't overflow. But this allows for using a static or
// stack-based buffer in controlled circumstances. If '*pdst' is zero
// a buffer will be allocated which must subsequently be freed by the
// caller...

size_t b64_encode(const char *src, size_t nbytes, char **pdst, int breaks, int cr);
size_t b64_decode(const char *src, size_t nbytes, char **pdst);
