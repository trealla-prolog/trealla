#define _XOPEN_SOURCE
#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <float.h>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/stat.h>

#if !defined(_WIN32) && !defined(__wasi__) && !defined(__ANDROID__)
#include <spawn.h>
#include <sys/wait.h>
#endif

#ifdef _WIN32
#define mkdir(p1,p2) mkdir(p1)
#else
#ifndef USE_MMAP
#define USE_MMAP 1
#endif
#if USE_MMAP
#ifndef _WIN32
#include <sys/mman.h>
#endif
#endif
#endif

#include "heap.h"
#include "module.h"
#include "network.h"
#include "parser.h"
#include "prolog.h"
#include "query.h"

#define MAX_ARGS 128

#ifdef __wasi__
#include <limits.h>
#include <unistd.h>

/*
	realpath implementation borrowed from musl
	realpath is excluded from WASI because it doesn't have the concept of absolute paths
	(apparently)

	musl as a whole is licensed under the following standard MIT license:

	----------------------------------------------------------------------
	Copyright © 2005-2020 Rich Felker, et al.

	Permission is hereby granted, free of charge, to any person obtaining
	a copy of this software and associated documentation files (the
	"Software"), to deal in the Software without restriction, including
	without limitation the rights to use, copy, modify, merge, publish,
	distribute, sublicense, and/or sell copies of the Software, and to
	permit persons to whom the Software is furnished to do so, subject to
	the following conditions:

	The above copyright notice and this permission notice shall be
	included in all copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
	EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
	MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
	IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
	CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
	TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
	SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
	----------------------------------------------------------------------
*/

static size_t slash_len(const char *s)
{
	const char *s0 = s;
	while (*s == '/') s++;
	return s-s0;
}



char *realpath(const char *restrict filename, char *restrict resolved)
{
	char stack[PATH_MAX+1];
	char output[PATH_MAX];
	size_t p, q, l, l0, cnt=0, nup=0;
	int check_dir=0;

	if (!filename) {
		errno = EINVAL;
		return 0;
	}
	l = strnlen(filename, sizeof stack);
	if (!l) {
		errno = ENOENT;
		return 0;
	}
	if (l >= PATH_MAX) goto toolong;
	p = sizeof stack - l - 1;
	q = 0;
	memcpy(stack+p, filename, l+1);

	/* Main loop. Each iteration pops the next part from stack of
	 * remaining path components and consumes any slashes that follow.
	 * If not a link, it's moved to output; if a link, contents are
	 * pushed to the stack. */
restart:
	for (; ; p+=slash_len(stack+p)) {
		/* If stack starts with /, the whole component is / or //
		 * and the output state must be reset. */
		if (stack[p] == '/') {
			check_dir=0;
			nup=0;
			q=0;
			output[q++] = '/';
			p++;
			/* Initial // is special. */
			if (stack[p] == '/' && stack[p+1] != '/')
				output[q++] = '/';
			continue;
		}

		char *z = strchrnul(stack+p, '/');
		l0 = l = z-(stack+p);

		if (!l && !check_dir) break;

		/* Skip any . component but preserve check_dir status. */
		if (l==1 && stack[p]=='.') {
			p += l;
			continue;
		}

		/* Copy next component onto output at least temporarily, to
		 * call readlink, but wait to advance output position until
		 * determining it's not a link. */
		if (q && output[q-1] != '/') {
			if (!p) goto toolong;
			stack[--p] = '/';
			l++;
		}
		if (q+l >= PATH_MAX) goto toolong;
		memcpy(output+q, stack+p, l);
		output[q+l] = 0;
		p += l;

		int up = 0;
		if (l0==2 && stack[p-2]=='.' && stack[p-1]=='.') {
			up = 1;
			/* Any non-.. path components we could cancel start
			 * after nup repetitions of the 3-byte string "../";
			 * if there are none, accumulate .. components to
			 * later apply to cwd, if needed. */
			if (q <= 3*nup) {
				nup++;
				q += l;
				continue;
			}
			/* When previous components are already known to be
			 * directories, processing .. can skip readlink. */
			if (!check_dir) goto skip_readlink;
		}
		ssize_t k = readlink(output, stack, p);
		if (k==(ssize_t)p) goto toolong;
		if (!k) {
			errno = ENOENT;
			return 0;
		}
		if (k<0) {
			if (errno != EINVAL) return 0;
skip_readlink:
			check_dir = 0;
			if (up) {
				while(q && output[q-1]!='/') q--;
				if (q>1 && (q>2 || output[0]!='/')) q--;
				continue;
			}
			if (l0) q += l;
			check_dir = stack[p];
			continue;
		}
		if (++cnt == SYMLOOP_MAX) {
			errno = ELOOP;
			return 0;
		}

		/* If link contents end in /, strip any slashes already on
		 * stack to avoid /->// or //->/// or spurious toolong. */
		if (stack[k-1]=='/') while (stack[p]=='/') p++;
		p -= k;
		memmove(stack+p, stack, k);

		/* Skip the stack advancement in case we have a new
		 * absolute base path. */
		goto restart;
	}

 	output[q] = 0;

	if (output[0] != '/') {
		if (!getcwd(stack, sizeof stack)) return 0;
		l = strlen(stack);
		/* Cancel any initial .. components. */
		p = 0;
		while (nup--) {
			while(l>1 && stack[l-1]!='/') l--;
			if (l>1) l--;
			p += 2;
			if (p<q) p++;
		}
		if (q-p && stack[l-1]!='/') stack[l++] = '/';
		if (l + (q-p) + 1 >= PATH_MAX) goto toolong;
		memmove(output + l, output + p, q - p + 1);
		memcpy(output, stack, l);
		q = l + q-p;
	}

	struct stat st = {0};
	if (resolved) {
		if (stat(resolved, &st) == -1) return NULL;
		return memcpy(resolved, output, q+1);
	}
	if (stat(output, &st) == -1) return NULL;
	return strdup(output);

toolong:
	errno = ENAMETOOLONG;
	return 0;
}
/* end of code borrowed from musl */
#endif

#ifdef _WIN32
#include <windows.h>

char *realpath(const char *path, char resolved_path[PATH_MAX])
{
  char *return_path = 0;

  if (path) //Else EINVAL
  {
    if (resolved_path)
    {
      return_path = resolved_path;
    }
    else
    {
      //Non standard extension that glibc uses
      return_path = malloc(PATH_MAX);
    }

    if (return_path) //Else EINVAL
    {
      //This is a Win32 API function similar to what realpath() is supposed to do
      size_t size = GetFullPathNameA(path, PATH_MAX, return_path, 0);

      //GetFullPathNameA() returns a size larger than buffer if buffer is too small
      if (size > PATH_MAX)
      {
        if (return_path != resolved_path) //Malloc'd buffer - Unstandard extension retry
        {
          size_t new_size;

          free(return_path);
          return_path = malloc(size);

          if (return_path)
          {
            new_size = GetFullPathNameA(path, size, return_path, 0); //Try again

            if (new_size > size) //If it's still too large, we have a problem, don't try again
            {
              free(return_path);
              return_path = 0;
              errno = ENAMETOOLONG;
            }
            else
            {
              size = new_size;
            }
          }
          else
          {
            //I wasn't sure what to return here, but the standard does say to return EINVAL
            //if resolved_path is null, and in this case we couldn't malloc large enough buffer
            errno = EINVAL;
          }
        }
        else //resolved_path buffer isn't big enough
        {
          return_path = 0;
          errno = ENAMETOOLONG;
        }
      }

      //GetFullPathNameA() returns 0 if some path resolve problem occured
      if (!size)
      {
        if (return_path != resolved_path) //Malloc'd buffer
        {
          free(return_path);
        }

        return_path = 0;

        //Convert MS errors into standard errors
        switch (GetLastError())
        {
          case ERROR_FILE_NOT_FOUND:
            errno = ENOENT;
            break;

          case ERROR_PATH_NOT_FOUND: case ERROR_INVALID_DRIVE:
            errno = ENOTDIR;
            break;

          case ERROR_ACCESS_DENIED:
            errno = EACCES;
            break;

          default: //Unknown Error
            errno = EIO;
            break;
        }
      }

      //If we get to here with a valid return_path, we're still doing good
      if (return_path)
      {
        struct stat stat_buffer;

        //Make sure path exists, stat() returns 0 on success
        if (stat(return_path, &stat_buffer))
        {
          if (return_path != resolved_path)
          {
            free(return_path);
          }

          return_path = 0;
          //stat() will set the correct errno for us
        }
        //else we succeeded!
      }
    }
    else
    {
      errno = EINVAL;
    }
  }
  else
  {
    errno = EINVAL;
  }

  return return_path;
}
#endif

#ifdef _WIN32
ssize_t getline(char **lineptr, size_t *n, FILE *stream) {
    size_t pos;
    int c;

    if (lineptr == NULL || stream == NULL || n == NULL) {
        errno = EINVAL;
        return -1;
    }

    c = getc(stream);
    if (c == EOF) {
        return -1;
    }

    if (*lineptr == NULL) {
        *lineptr = malloc(128);
 		check_error(*lineptr);
       if (*lineptr == NULL) {
            return -1;
        }
        *n = 128;
    }

    pos = 0;
    while(c != EOF) {
        if (pos + 1 >= *n) {
            size_t new_size = *n + (*n >> 2);
            if (new_size < 128) {
                new_size = 128;
            }
            char *new_ptr = realloc(*lineptr, new_size);
            if (new_ptr == NULL) {
                return -1;
            }
            *n = new_size;
            *lineptr = new_ptr;
        }

        ((unsigned char *)(*lineptr))[pos ++] = c;
        if (c == '\n') {
            break;
        }
        c = getc(stream);
    }

    (*lineptr)[pos] = '\0';
    return pos;
}
#endif

// FIXME: this is too slow. There should be one overall
// alias map, not one per stream.

int get_named_stream(prolog *pl, const char *name, size_t len)
{
	acquire_lock(&pl->guard);

	for (int i = 0; i < MAX_STREAMS; i++) {
		stream *str = &pl->streams[i];

		if (!str->fp || str->ignore || !str->alias)
			continue;

		if (sl_get(str->alias, name, NULL)) {
			release_lock(&pl->guard);
			return i;
		}

		if (str->filename && (strlen(str->filename) == len)
			&& !strncmp(str->filename, name, len)) {
			release_lock(&pl->guard);
			return i;
		}
	}

	release_lock(&pl->guard);
	return -1;
}

int new_stream(prolog *pl)
{
	acquire_lock(&pl->guard);

	for (int i = 0; i < MAX_STREAMS; i++) {
		unsigned n = pl->str_cnt++ % MAX_STREAMS;

		if (n < 4)
			continue;

		stream *str = &pl->streams[n];

		if (!str->fp && !str->ignore) {
			release_lock(&pl->guard);
			return n;
		}
	}

	release_lock(&pl->guard);
	return -1;
}

int get_stream(query *q, cell *p1)
{
	if (is_atom(p1)) {
		int n = get_named_stream(q->pl, C_STR(q, p1), C_STRLEN(q, p1));

		if (n < 0)
			return -1;

		return n;
	}

	if (p1->tag != TAG_INTEGER)
		return -1;

	if (!(p1->flags & FLAG_INT_STREAM))
		return -1;

	int n = get_smallint(p1);

	if (!q->pl->streams[n].fp)
		return -1;

	return n;
}

static bool is_closed_stream(prolog *pl, cell *p1)
{
	if (!(p1->flags & FLAG_INT_STREAM))
		return false;

	if (pl->streams[get_smallint(p1)].fp)
		return false;

	return true;
}

static void add_stream_properties(query *q, int n)
{
	stream *str = &q->pl->streams[n];
	char tmpbuf[1024*8];
	char *dst = tmpbuf;
	*dst = '\0';
	off_t pos = !str->is_map && !str->is_engine && !str->is_thread && !str->is_queue && !str->is_mutex ? ftello(str->fp) : 0;
	bool at_end_of_file = false;

	if (!str->at_end_of_file && (n > 2) && !str->is_engine && !str->is_map && !str->is_thread && !str->is_queue && !str->is_mutex && !str->p) {
#if 0
		if (str->p) {
			if (str->p->srcptr && *str->p->srcptr) {
				str->ungetch = get_char_utf8((const char**)&str->p->srcptr);
			}
		}
#endif

		int ch = str->ungetch ? str->ungetch : net_getc(str);

		if (str->ungetch)
			;
		else if (feof(str->fp) || ferror(str->fp)) {
			clearerr(str->fp);

			if (str->eof_action != eof_action_reset)
				at_end_of_file = true;
		} else
			str->ungetch = ch;
	}

	sliter *iter = sl_first(str->alias);

	while (sl_next(iter, NULL)) {
		const char *alias = sl_key(iter);
		char *dst2 = formatted(alias, strlen(alias), false, false);
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, alias('%s')).\n", n, dst2);
		free(dst2);
	}

	sl_done(iter);

	if (!str->is_engine && !str->is_map && !str->is_thread && !str->is_queue && !str->is_mutex) {
		char *dst2 = formatted(str->filename, strlen(str->filename), false, false);
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, file_name('%s')).\n", n, dst2);
		free(dst2);
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, file_no(%u)).\n", n, fileno(str->fp));

		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, file(%llu)).\n", n, (unsigned long long)(size_t)str->fp);
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, mode(%s)).\n", n, str->mode);
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, type(%s)).\n", n, str->binary ? "binary" : "text");
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, line_count(%d)).\n", n, str->p ? str->p->line_nbr : 1);
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, position(%llu)).\n", n, (unsigned long long)(pos != -1 ? pos : 0));
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, reposition(%s)).\n", n, (n < 3) || str->socket ? "false" : "true");
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, end_of_stream(%s)).\n", n, str->at_end_of_file ? "past" : at_end_of_file ? "at" : "not");
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, eof_action(%s)).\n", n, str->eof_action == eof_action_eof_code ? "eof_code" : str->eof_action == eof_action_error ? "error" : str->eof_action == eof_action_reset ? "reset" : "none");

		if (!str->binary) {
			dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, bom(%s)).\n", n, str->bom ? "true" : "false");
			dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, encoding('%s')).\n", n, "UTF-8");
		}

		if (!strcmp(str->mode, "read"))
			dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, input).\n", n);
		else
			dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, output).\n", n);

		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, newline(%s)).\n", n, NEWLINE_MODE);
	}

	if (str->is_engine)
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, engine(true)).\n", n);
	else if (str->is_map)
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, skiplist(true)).\n", n);
	else if (str->is_mutex)
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, mutex(true)).\n", n);
	else if (str->is_queue)
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, queue(true)).\n", n);
	else if (str->is_thread)
		dst += snprintf(dst, sizeof(tmpbuf)-strlen(tmpbuf), "'$stream_property'(%d, thread(true)).\n", n);

	parser *p = parser_create(q->st.m);
	p->srcptr = tmpbuf;
	p->consulting = true;
	tokenize(p, false, false);
	parser_destroy(p);
}

static bool del_stream_properties(query *q, int n)
{
	cell *tmp = alloc_on_heap(q, 3);
	check_heap_error(tmp);
	make_atom(tmp+0, g_sys_stream_property_s);
	make_int(tmp+1, n);
	make_ref(tmp+2, create_vars(q, 1), q->st.curr_frame);
	tmp->nbr_cells = 3;
	tmp->arity = 2;
	q->retry = QUERY_OK;

	while (do_retract(q, tmp, q->st.curr_frame, DO_RETRACTALL)) {
		if (q->did_throw) return false;
		q->retry = QUERY_RETRY;
		retry_choice(q);
	}

	q->retry = QUERY_OK;
	return true;
}

static bool do_stream_property(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	cell *c = p1 + 1;
	c = deref(q, c, p1_ctx);
	pl_idx c_ctx = q->latest_ctx;

	if (!CMP_STRING_TO_CSTR(q, p1, "file_name")) {
		cell tmp;
		make_cstring(&tmp, str->filename);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "file_no")) {
		if (!str->fp)
			return false;

		cell tmp;
		make_int(&tmp, fileno(str->fp));
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "file")) {
		if (!str->fp)
			return false;

		cell tmp;
		make_uint(&tmp, (size_t)str->fp);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "alias")) {
		cell tmp;
		sliter *iter = sl_first(str->alias);
		bool ok = false;

		while (sl_next(iter, NULL)) {
			const char *alias = sl_key(iter);
			make_cstring(&tmp, alias);
			ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
			unshare_cell(&tmp);
			if (ok) break;
		}

		sl_done(iter);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "mode")) {
		cell tmp;
		make_cstring(&tmp, str->mode);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "engine")) {
		cell tmp;
		make_cstring(&tmp, str->is_engine?"true":"false");
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "mutex")) {
		cell tmp;
		make_cstring(&tmp, str->is_mutex?"true":"false");
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "queue")) {
		cell tmp;
		make_cstring(&tmp, str->is_queue?"true":"false");
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "thread")) {
		cell tmp;
		make_cstring(&tmp, str->is_thread?"true":"false");
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "skiplist")) {
		cell tmp;
		make_cstring(&tmp, str->is_map?"true":"false");
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "bom") && !str->binary) {
		cell tmp;
		make_cstring(&tmp, str->bom?"true":"false");
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "type")) {
		cell tmp;
		make_cstring(&tmp, str->binary ? "binary" : "text");
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "reposition")) {
		cell tmp;
		make_cstring(&tmp, str->socket || (n <= 2) ? "false" : str->repo ? "true" : "false");
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "encoding") && !str->binary) {
		cell tmp;
		make_cstring(&tmp, "UTF-8");
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "newline")) {
		cell tmp;
		make_cstring(&tmp, NEWLINE_MODE);
		bool ok = unify(q, c, c_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		return ok;
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "input"))
		return !strcmp(str->mode, "read");

	if (!CMP_STRING_TO_CSTR(q, p1, "output"))
		return strcmp(str->mode, "read");

	if (!CMP_STRING_TO_CSTR(q, p1, "eof_action") && is_stream(pstr)) {
		cell tmp;

		if (str->eof_action == eof_action_eof_code)
			make_atom(&tmp, new_atom(q->pl, "eof_code"));
		else if (str->eof_action == eof_action_error)
			make_atom(&tmp, new_atom(q->pl, "error"));
		else if (str->eof_action == eof_action_reset)
			make_atom(&tmp, new_atom(q->pl, "reset"));
		else
			make_atom(&tmp, new_atom(q->pl, "none"));

		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "end_of_stream") && is_stream(pstr)) {
		bool at_end_of_file = false;

		if (!str->at_end_of_file && (n > 2)) {
			if (str->p) {
				if (str->p->srcptr && *str->p->srcptr) {
					int ch = get_char_utf8((const char**)&str->p->srcptr);
					str->ungetch = ch;
				}
			}

			int ch = str->ungetch ? str->ungetch : net_getc(str);

			if (str->ungetch)
				;
			else if (feof(str->fp) || ferror(str->fp)) {
				clearerr(str->fp);

				if (str->eof_action != eof_action_reset)
					at_end_of_file = true;
			} else
				str->ungetch = ch;
		}

		cell tmp;

		if (str->at_end_of_file)
			make_atom(&tmp, new_atom(q->pl, "past"));
		else if (at_end_of_file)
			make_atom(&tmp, new_atom(q->pl, "at"));
		else
			make_atom(&tmp, new_atom(q->pl, "not"));

		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "position") && !is_var(pstr)) {
		cell tmp;
		make_int(&tmp, ftello(str->fp));
		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	if (!CMP_STRING_TO_CSTR(q, p1, "line_count") && !is_var(pstr)) {
		cell tmp;
		make_int(&tmp, str->p->line_nbr);
		return unify(q, c, c_ctx, &tmp, q->st.curr_frame);
	}

	return false;
}

static void clear_streams_properties(query *q)
{
	cell tmp;
	make_atom(&tmp, g_sys_stream_property_s);
	tmp.nbr_cells = 1;
	tmp.arity = 2;

	predicate *pr = find_predicate(q->st.m, &tmp);

	if (pr) {
		for (rule *r = pr->head; r;) {
			rule *save = r;
			r = r->next;
			retract_from_db(pr->m, save);
		}

		pr->head = pr->tail = NULL;
		pr->cnt = 0;
	}
}

static const char *s_properties =
	"alias,file_name,mode,encoding,type,line_count,"			\
	"position,reposition,end_of_stream,eof_action,"				\
	"input,output,newline,engine,skiplist";

static bool bif_iso_stream_property_2(query *q)
{
	GET_FIRST_ARG(pstr,any);
	GET_NEXT_ARG(p1,any);

	if (!is_stream_or_var(pstr)) {
		if (is_closed_stream(q->pl, pstr))
			return throw_error(q, pstr, q->st.curr_frame, "existence_error", "stream");
		else
			return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");
	}

	if (p1->arity > 1)
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

	if (!is_var(p1) && !is_callable(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

	if (!is_var(pstr) && !is_var(p1))
		return do_stream_property(q);

	if (!q->retry) {
		clear_streams_properties(q);

		for (int i = 0; i < MAX_STREAMS; i++) {
			if (!q->pl->streams[i].fp)
				continue;

			stream *str = &q->pl->streams[i];

			if (!str->socket && !str->is_thread && !str->is_mutex && !str->is_queue)
				add_stream_properties(q, i);
		}
	}

	check_heap_error(init_tmp_heap(q));
	cell *tmp = deep_clone_to_tmp(q, q->st.curr_instr, q->st.curr_frame);
	check_heap_error(tmp);
	tmp->val_off = g_sys_stream_property_s;

	if (match_clause(q, tmp, q->st.curr_frame, DO_CLAUSE) != true) {
		clear_streams_properties(q);

		if (is_callable(p1) && !strstr(s_properties, C_STR(q, p1)))
			return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

		return false;
	}

	clause *cl = &q->st.r->cl;
	GET_FIRST_ARG(pstrx,any);
	pstrx->flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	stash_frame(q, cl, false);
	return true;
}

void convert_path(char *filename)
{
	char *src = filename;

	while (*src) {
		if ((*src == '/') || (*src == '\\'))
			*src = PATH_SEP_CHAR;

		src++;
	}
}

bool valid_list(query *q, cell *c, pl_idx c_ctx)
{
	while (is_iso_list(c)) {
		c = c + 1;
		c += c->nbr_cells;
		c = deref(q, c, c_ctx);
		c_ctx = q->latest_ctx;

		if (!is_iso_list_or_nil_or_var(c))
			return false;
	}

	return true;
}

#if !defined(_WIN32) && !defined(__wasi__)
static bool bif_popen_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,list_or_nil);
	int n = new_stream(q->pl);
	char *src = NULL;

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	char *filename;

	if (is_atom(p1))
		filename = src = DUP_STRING(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	}

	convert_path(filename);

	stream *str = &q->pl->streams[n];
	str->pipe = true;
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	check_heap_error(str->filename = strdup(filename));
	check_heap_error(str->mode = DUP_STRING(q, p2));
	bool binary = false;
	uint8_t eof_action = eof_action_eof_code, is_alias = false;
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		if (is_compound(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, q->latest_ctx);


			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, q->latest_ctx, "permission_error", "open,source_sink");

			if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
				if (!CMP_STRING_TO_CSTR(q, name, "current_input")) {
					q->pl->current_input = n;
				} else if (!CMP_STRING_TO_CSTR(q, name, "current_output")) {
					q->pl->current_output = n;
				} else if (!CMP_STRING_TO_CSTR(q, name, "current_error")) {
					q->pl->current_error = n;
				} else {
					sl_set(str->alias, DUP_STRING(q, name), NULL);
#if 0
					cell tmp;
					make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

					if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
						return false;

					is_alias = true;
#endif
				}
			} else if (!CMP_STRING_TO_CSTR(q, c, "type")) {
				if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "binary")) {
					binary = true;
				} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "text"))
					binary = false;
			} else if (!CMP_STRING_TO_CSTR(q, c, "eof_action")) {
				if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "error")) {
					eof_action = eof_action_error;
				} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "eof_code")) {
					eof_action = eof_action_eof_code;
				} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "reset")) {
					eof_action = eof_action_reset;
				}
			}
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_var(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	str->binary = binary;
	str->eof_action = eof_action;

	if (!strcmp(str->mode, "read"))
		str->fp = popen(filename, binary?"rb":"r");
	else if (!strcmp(str->mode, "write"))
		str->fp = popen(filename, binary?"wb":"w");
	else
		return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");

	free(src);

	if (!str->fp) {
		if ((errno == EACCES) || (strcmp(str->mode, "read") && (errno == EROFS)))
			return throw_error(q, p1, p1_ctx, "permission_error", "open,source_sink");
		else
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

	if (!is_alias) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

		if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
			return false;
	}

	return true;
}
#endif

extern char **g_envp;

#if !defined(_WIN32) && !defined(__wasi__) && !defined(__ANDROID__)
static bool bif_process_create_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,list_or_nil);
	GET_NEXT_ARG(p3,list_or_nil);
	char *src = NULL;
	char *filename;

	if (is_atom(p1))
		filename = src = DUP_STRING(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	}

	convert_path(filename);
    int args = 0, envs = 0;
    char *arguments[MAX_ARGS] = {NULL};
    char *environments[MAX_ARGS] = {NULL};
	arguments[args++] = strdup(filename);

	for (int i = 0; g_envp[i] != NULL; i++)
		environments[envs++] = strdup(g_envp[i]);

	LIST_HANDLER(p2);

	while (is_iso_list(p2)) {
		assert(args < MAX_ARGS);
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (!is_atom(c))
			return throw_error(q, c, c_ctx, "domain_error", "args");

		arguments[args++] = DUP_STRING(q, c);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	arguments[args] = NULL;
    posix_spawn_file_actions_t file_actions;
    posix_spawn_file_actions_init(&file_actions);
    posix_spawnattr_t attrp;
    posix_spawnattr_init(&attrp);
    cell *ppid = NULL;
    const char *cwd = NULL;
    pl_idx ppid_ctx = 0;
	LIST_HANDLER(p3);

	while (is_iso_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (is_compound(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, c_ctx);
			pl_idx name_ctx = q->latest_ctx;

			if (!CMP_STRING_TO_CSTR(q, c, "process") || !CMP_STRING_TO_CSTR(q, c, "pid")) {
				ppid = name;
				ppid_ctx = name_ctx;
			} else if (!CMP_STRING_TO_CSTR(q, c, "detached")) {
#if (defined(__GLIBC__) && (__GLIBC__ < 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ < 26))) || !defined(POSIX_SPAWN_SETSID)
				return throw_error(q, c, c_ctx, "not available", "posix_spawnattr_setflags");
#else
				posix_spawnattr_setflags(&attrp, POSIX_SPAWN_SETSID);
#endif
			} else if (!CMP_STRING_TO_CSTR(q, c, "cwd")) {
#ifndef posix_spawn_file_actions_addchdir_np
				return throw_error(q, c, c_ctx, "not available", "posix_spawn_file_actions_addchdir_np");
#else
				cwd = C_STR(q, name);
				posix_spawn_file_actions_addchdir_np(&file_actions, cwd);
#endif
			} else if (!CMP_STRING_TO_CSTR(q, c, "env") && is_list_or_nil(name)) {
				LIST_HANDLER(name);
				memset(environments, 0, sizeof(environments));
				envs = 0;

				while (is_iso_list(name)) {
					cell *h = LIST_HEAD(name);
					cell *c = deref(q, h, name_ctx);

					if (is_compound(c) && (c->arity == 2) && (c->val_off == g_eq_s)) {
						cell *p1 = c + 1, *p2 = c + 2;
						SB(pr);

						if (is_atom(p1) && is_atom(p2)) {
							SB_sprintf(pr, "%s=%s", C_STR(q, p1), C_STR(q, p2));
						} else if (is_atom(p1) && is_smallint(p2)) {
							SB_sprintf(pr, "%s=%d", C_STR(q, p1), (int)get_smallint(p2));
						}

						environments[envs++] = SB_cstr(pr);
					}

					name = LIST_TAIL(name);
					name = deref(q, name, name_ctx);
					name_ctx = q->latest_ctx;
				}

			} else if (!CMP_STRING_TO_CSTR(q, c, "environment") && is_list_or_nil(name)) {
				LIST_HANDLER(name);

				while (is_iso_list(name)) {
					cell *h = LIST_HEAD(name);
					cell *c = deref(q, h, name_ctx);

					if (is_compound(c) && (c->arity == 2) && (c->val_off == g_eq_s)) {
						cell *p1 = c + 1, *p2 = c + 2;
						SB(pr);

						if (is_atom(p1) && is_atom(p2)) {
							SB_sprintf(pr, "%s=%s", C_STR(q, p1), C_STR(q, p2));
						} else if (is_atom(p1) && is_smallint(p2)) {
							SB_sprintf(pr, "%s=%d", C_STR(q, p1), (int)get_smallint(p2));
						}

						environments[envs++] = SB_cstr(pr);
					}

					name = LIST_TAIL(name);
					name = deref(q, name, name_ctx);
					name_ctx = q->latest_ctx;
				}

			} else if (!CMP_STRING_TO_CSTR(q, c, "stdin") && !CMP_STRING_TO_CSTR(q, name, "std")) {
				posix_spawn_file_actions_adddup2(&file_actions, q->pl->current_input, 0);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stdin") && !CMP_STRING_TO_CSTR(q, name, "null")) {
				posix_spawn_file_actions_addopen(&file_actions, 0, "/dev/null", O_RDONLY, 0);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stdin") && !CMP_STRING_TO_CSTR(q, name, "pipe")
				&& is_compound(name) && (name->arity == 1) && is_var(name+1)) {
				cell *ns = deref(q, name+1, name_ctx);
				pl_idx ns_ctx = q->latest_ctx;
				int n = new_stream(q->pl);
				int fds[2];
				if (pipe(fds)) return false;
				posix_spawn_file_actions_adddup2(&file_actions, fds[0], 0);
				q->pl->streams[n].fp = fdopen(fds[1], "w");
				q->pl->streams[n].pipe = true;
				cell tmp;
				make_int(&tmp, n);
				tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
				unify(q, ns, ns_ctx, &tmp, q->st.curr_frame);

			} else if (!CMP_STRING_TO_CSTR(q, c, "stdin") && !CMP_STRING_TO_CSTR(q, name, "stream")) {
				cell *ns = deref(q, name, name_ctx);
				int n = get_stream(q, ns);
				posix_spawn_file_actions_adddup2(&file_actions, fileno(q->pl->streams[n].fp), 0);

			} else if (!CMP_STRING_TO_CSTR(q, c, "stdout") && !CMP_STRING_TO_CSTR(q, name, "std")) {
				posix_spawn_file_actions_adddup2(&file_actions, q->pl->current_output, 1);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stdout") && !CMP_STRING_TO_CSTR(q, name, "null")) {
				posix_spawn_file_actions_addopen(&file_actions, 1, "/dev/null", O_WRONLY, 0);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stdout") && !CMP_STRING_TO_CSTR(q, name, "pipe")
				&& is_compound(name) && (name->arity == 1) && is_var(name+1)) {
				cell *ns = deref(q, name+1, name_ctx);
				pl_idx ns_ctx = q->latest_ctx;
				int n = new_stream(q->pl);
				int fds[2];
				if (pipe(fds)) return false;
				posix_spawn_file_actions_adddup2(&file_actions, fds[1], 1);
				q->pl->streams[n].fp = fdopen(fds[0], "r");
				q->pl->streams[n].pipe = true;
				cell tmp;
				make_int(&tmp, n);
				tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
				unify(q, ns, ns_ctx, &tmp, q->st.curr_frame);

			} else if (!CMP_STRING_TO_CSTR(q, c, "stdout") && !CMP_STRING_TO_CSTR(q, name, "stream")) {
				cell *ns = deref(q, name, name_ctx);
				int n = get_stream(q, ns);
				posix_spawn_file_actions_adddup2(&file_actions, fileno(q->pl->streams[n].fp), 1);

			} else if (!CMP_STRING_TO_CSTR(q, c, "stderr") && !CMP_STRING_TO_CSTR(q, name, "std")) {
				posix_spawn_file_actions_adddup2(&file_actions, q->pl->current_error, 2);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stderr") && !CMP_STRING_TO_CSTR(q, name, "null")) {
				posix_spawn_file_actions_addopen(&file_actions, 2, "/dev/null", O_WRONLY, 0);
			} else if (!CMP_STRING_TO_CSTR(q, c, "stderr") && !CMP_STRING_TO_CSTR(q, name, "pipe")
				&& is_compound(name) && (name->arity == 1) && is_var(name+1)) {
				cell *ns = deref(q, name+1, name_ctx);
				pl_idx ns_ctx = q->latest_ctx;
				int n = new_stream(q->pl);
				int fds[2];
				if (pipe(fds)) return false;
				posix_spawn_file_actions_adddup2(&file_actions, fds[1], 2);
				q->pl->streams[n].fp = fdopen(fds[0], "r");
				q->pl->streams[n].pipe = true;
				cell tmp;
				make_int(&tmp, n);
				tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
				unify(q, ns, ns_ctx, &tmp, q->st.curr_frame);

			} else if (!CMP_STRING_TO_CSTR(q, c, "stderr") && !CMP_STRING_TO_CSTR(q, name, "stream")) {
				cell *ns = deref(q, name, name_ctx);
				int n = get_stream(q, ns);
				posix_spawn_file_actions_adddup2(&file_actions, fileno(q->pl->streams[n].fp), 2);
			}
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "process_create_option");

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	pid_t pid;
	int ok = posix_spawnp(&pid, C_STR(q, p1), &file_actions, &attrp, (char * const*)arguments, (char * const*)environments);
	posix_spawn_file_actions_destroy(&file_actions);
	posix_spawnattr_destroy(&attrp);
	free(src);

	for (int i = 0; i < args; i++)
		free(arguments[i]);

	for (int i = 0; i < envs; i++)
		free(environments[i]);

	if (ok != 0)
		return throw_error(q, p1, p1_ctx, "system_error", "posix_spawnp");

	if (ppid) {
		cell tmp;
		make_uint(&tmp, pid);
		return unify(q, ppid, ppid_ctx, &tmp, q->st.curr_frame);
	} else {
		waitpid(pid, NULL, 0);
	}

	return true;
}

static bool bif_process_wait_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,list_or_nil);
	LIST_HANDLER(p2);
	int secs = -1;

	while (is_iso_list(p2)) {
		cell *h = LIST_HEAD(p2);
		cell *c = deref(q, h, p2_ctx);

		if (is_compound(c) && (c->arity == 1) && !CMP_STRING_TO_CSTR(q, c, "timeout")) {
			if (is_integer(FIRST_ARG(c)))
				secs = get_smallint(FIRST_ARG(c));
			else if (is_atom(FIRST_ARG(c)) && !CMP_STRING_TO_CSTR(q, FIRST_ARG(c), "infinite"))
				secs = -1;
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "process_wait_option");

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	int status = 0, pid = get_smalluint(p1);
	int ok = waitpid(pid, &status, secs != -1 ? WNOHANG : 0);
	return ok == pid;
}

static bool bif_process_wait_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int pid = get_smalluint(p1);
	waitpid(pid, NULL, 0);
	return true;
}

static bool bif_process_kill_2(query *q)
{
	GET_FIRST_ARG(p1,integer);
	GET_NEXT_ARG(p2,integer);
	int pid = get_smalluint(p1), sig = get_smallint(p2);
	kill(pid, sig);
	return true;
}

static bool bif_process_kill_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int pid = get_smalluint(p1);
	kill(pid, SIGKILL);
	return true;
}
#endif

#ifdef _WIN32
#include <windows.h>
#include <io.h>

/* macro definitions extracted from git/git-compat-util.h */
#define PROT_READ  1
#define PROT_WRITE 2
#define MAP_FAILED ((void*)-1)

/* macro definitions extracted from /usr/include/bits/mman.h */
#define MAP_SHARED	0x01		/* Share changes.  */
#define MAP_PRIVATE	0x02		/* Changes are private.  */

static void *mmap(void *start, size_t length, int prot, int flags, int fd, off_t offset)
{
	size_t len;
	struct stat st;
	uint64_t o = offset;
	uint32_t l = o & 0xFFFFFFFF;
	uint32_t h = (o >> 32) & 0xFFFFFFFF;

	if (!fstat(fd, &st))
		len = (size_t) st.st_size;
	else {
		fprintf(stderr, "ERROR: mmap could not determine filesize");
		return NULL;
	}

	if ((length + offset) > len)
		length = len - offset;

	if (!(flags & MAP_PRIVATE)) {
		fprintf(stderr, "ERROR: Invalid usage of mmap");
		return NULL;
	}

	HANDLE hmap = CreateFileMapping((HANDLE)_get_osfhandle(fd), 0, PAGE_WRITECOPY, 0, 0, 0);

	if (!hmap) {
		fprintf(stderr, "ERROR: CreateFileMapping failed");
		return NULL;
	}

	void *temp = MapViewOfFileEx(hmap, FILE_MAP_COPY, h, l, length, start);

	if (!CloseHandle(hmap))
		fprintf(stderr, "Unable to close file mapping handle\n");

	return temp ? temp : MAP_FAILED;
}
#endif


static bool bif_iso_open_4(query *q)
{
	GET_FIRST_ARG(p1,atom_or_compound);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,list_or_nil);
	int n = new_stream(q->pl);
	char *src = NULL;

	if (n < 0)
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");

	char *filename;
	stream *oldstr = NULL;

	if (is_compound(p1) && (p1->arity == 1) && !CMP_STRING_TO_CSTR(q, p1, "stream")) {
		int oldn = get_stream(q, p1+1);

		if (oldn < 0)
			return throw_error(q, p1, p1_ctx, "type_error", "not_a_stream");

		oldstr = &q->pl->streams[oldn];
		filename = oldstr->filename;
	} else if (is_atom(p1))
		filename = src = DUP_STRING(q, p1);
	else
		return throw_error(q, p1, p1_ctx, "domain_error", "source_sink");

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = src = chars_list_to_string(q, p1, p1_ctx, len);
	}

	convert_path(filename);
	stream *str = &q->pl->streams[n];
	check_heap_error(str->filename = strdup(filename));
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	check_heap_error(str->mode = DUP_STRING(q, p2));
	bool binary = false, repo = true;
	uint8_t eof_action = eof_action_eof_code;
	free(src);

#if USE_MMAP
	cell *mmap_var = NULL;
	pl_idx mmap_ctx = 0;
#endif

	bool bom_specified = false, use_bom = false, is_alias = false;
	LIST_HANDLER(p4);

	while (is_list(p4)) {
		cell *h = LIST_HEAD(p4);
		cell *c = deref(q, h, p4_ctx);
		pl_idx c_ctx = q->latest_ctx;

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		cell *name = c + 1;
		name = deref(q, name, c_ctx);

		//printf("*** %s %s : %s\n", str->filename, C_STR(q, c), C_STR(q, name));

		if (!CMP_STRING_TO_CSTR(q, c, "mmap")) {
#if USE_MMAP
			mmap_var = name;
			mmap_var = deref(q, mmap_var, q->latest_ctx);
			mmap_ctx = q->latest_ctx;
#endif
		} else if (!CMP_STRING_TO_CSTR(q, c, "encoding")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		} else if (!CMP_STRING_TO_CSTR(q, c, "alias")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name)) >= 0)
				return throw_error(q, c, c_ctx, "permission_error", "open,source_sink");

			if (!CMP_STRING_TO_CSTR(q, name, "current_input")) {
				q->pl->current_input = n;
			} else if (!CMP_STRING_TO_CSTR(q, name, "current_output")) {
				q->pl->current_output = n;
			} else if (!CMP_STRING_TO_CSTR(q, name, "current_error")) {
				q->pl->current_error = n;
			} else {
				sl_set(str->alias, DUP_STRING(q, name), NULL);
#if 0
				cell tmp;
				make_atom(&tmp, new_atom(q->pl, C_STR(q, name)));

				if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
					return false;

				is_alias = true;
#endif
			}
		} else if (!CMP_STRING_TO_CSTR(q, c, "type")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "binary")) {
				binary = true;
			} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "text"))
				binary = false;
			else
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		} else if (!CMP_STRING_TO_CSTR(q, c, "bom")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			bom_specified = true;

			if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "true"))
				use_bom = true;
			else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "false"))
				use_bom = false;
		} else if (!CMP_STRING_TO_CSTR(q, c, "reposition")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "true"))
				repo = true;
			else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "false"))
				repo = false;
		} else if (!CMP_STRING_TO_CSTR(q, c, "eof_action")) {
			if (is_var(name))
				return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

			if (!is_atom(name))
				return throw_error(q, c, c_ctx, "domain_error", "stream_option");

			if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "error")) {
				eof_action = eof_action_error;
			} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "eof_code")) {
				eof_action = eof_action_eof_code;
			} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "reset")) {
				eof_action = eof_action_reset;
			}
		} else {
			return throw_error(q, c, c_ctx, "domain_error", "stream_option");
		}

		p4 = LIST_TAIL(p4);
		p4 = deref(q, p4, p4_ctx);
		p4_ctx = q->latest_ctx;

		if (is_var(p4))
			return throw_error(q, p4, p4_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	str->repo = repo;
	str->binary = binary;
	str->eof_action = eof_action;
	str->bom = false;
	str->did_getc = false;
	str->srclen = str->ungetch = 0;
	str->at_end_of_file = false;

	if (oldstr) {
		int fd = fileno(oldstr->fp);

		if (!strcmp(str->mode, "read"))
			str->fp = fdopen(fd, str->binary?"rb":"r");
		else if (!strcmp(str->mode, "write"))
			str->fp = fdopen(fd, str->binary?"wb":"w");
		else if (!strcmp(str->mode, "append"))
			str->fp = fdopen(fd, str->binary?"ab":"a");
		else if (!strcmp(str->mode, "update"))
			str->fp = fdopen(fd, str->binary?"rb+":"r+");
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");
	} else {
		if (!strcmp(str->mode, "read"))
			str->fp = fopen(str->filename, str->binary?"rb":"r");
		else if (!strcmp(str->mode, "write"))
			str->fp = fopen(str->filename, str->binary?"wb":"w");
		else if (!strcmp(str->mode, "append"))
			str->fp = fopen(str->filename, str->binary?"ab":"a");
		else if (!strcmp(str->mode, "update"))
			str->fp = fopen(str->filename, str->binary?"rb+":"r+");
		else
			return throw_error(q, p2, p2_ctx, "domain_error", "io_mode");
	}

	if (!str->fp) {
		if ((errno == EACCES) || (strcmp(str->mode, "read") && (errno == EROFS)))
			return throw_error(q, p1, p1_ctx, "permission_error", "open,source_sink");
		//else if ((strcmp(str->mode, "read") && (errno == EISDIR)))
		//	return throw_error(q, p1, p1_ctx, "permission_error", "open,isadir");
		else
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

#if USE_MMAP
	size_t offset = 0;
#endif

	if (!strcmp(str->mode, "read") && !str->binary && (!bom_specified || use_bom)) {
		int ch = xgetc_utf8(net_getc, str);

		if (feof(str->fp))
			clearerr(str->fp);

		if ((unsigned)ch == 0xFEFF) {
			str->bom = true;
#if USE_MMAP
			offset = 3;
#endif
		} else
			fseek(str->fp, 0, SEEK_SET);
	} else if (!strcmp(str->mode, "write") && !str->binary && use_bom) {
		int ch = 0xFEFF;
		char tmpbuf[10];
		put_char_utf8(tmpbuf, ch);
		net_write(tmpbuf, strlen(tmpbuf), str);
		str->bom = true;
	}

#if USE_MMAP
	int prot = 0;

	if (!strcmp(str->mode, "read"))
		prot = PROT_READ;
	else
		prot = PROT_WRITE;

	if (mmap_var && is_var(mmap_var)) {
		int fd = fileno(str->fp);
		struct stat st = {0};
		fstat(fd, &st);
		size_t len = st.st_size;
		void *addr = mmap(0, len, prot, MAP_PRIVATE, fd, offset);
		check_error(addr);
		cell tmp = {0};
		tmp.tag = TAG_CSTR;
		tmp.flags = FLAG_CSTR_BLOB | FLAG_CSTR_STRING | FLAG_CSTR_SLICE;
		tmp.nbr_cells = 1;
		tmp.arity = 2;
		tmp.val_str = addr;
		tmp.str_len = len;
		unify(q, mmap_var, mmap_ctx, &tmp, q->st.curr_frame);
	}
#endif

	if (!is_alias) {
		cell tmp ;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;

		if (!unify(q, p3, p3_ctx, &tmp, q->st.curr_frame))
			return false;
	}

	return true;
}

static bool stream_close(query *q, int n)
{
	stream *str = &q->pl->streams[n];
	parser_destroy(str->p);
	str->p = NULL;

	if ((str->fp == stdin)
		|| (str->fp == stdout)
		|| (str->fp == stderr))
		return true;

	if ((int)q->pl->current_input == n)
		q->pl->current_input = 0;

	if ((int)q->pl->current_output == n)
		q->pl->current_output = 1;

	if ((int)q->pl->current_error == n)
		q->pl->current_error = 2;

	if (sl_get(str->alias, "user_input", NULL)) {
		stream *str2 = &q->pl->streams[0];
		sl_set(str2->alias, strdup("user_input"), NULL);
	}

	if (sl_get(str->alias, "user_output", NULL)) {
		stream *str2 = &q->pl->streams[1];
		sl_set(str2->alias, strdup("user_output"), NULL);
	}

	if (sl_get(str->alias, "user_error", NULL)) {
		stream *str2 = &q->pl->streams[2];
		sl_set(str2->alias, strdup("user_error"), NULL);
	}

	if (!str->socket && !str->is_mutex && !str->is_queue && !str->is_thread)
		del_stream_properties(q, n);

	bool ok = true;

	if (str->is_map) {
		sl_destroy(str->keyval);
		str->keyval = NULL;
	} else if (str->is_engine) {
		query_destroy(str->engine);
		str->engine = NULL;
	} else if (str->is_thread || str->is_queue || str->is_mutex) {
	} else
		ok = !net_close(str);

	sl_destroy(str->alias); str->alias = NULL;
	str->fp = NULL;
	//str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	free(str->mode); str->mode = NULL;
	free(str->filename); str->filename = NULL;
	free(str->data); str->data = NULL;
	str->at_end_of_file = true;

	if (!ok)
		return throw_error(q, q->st.curr_instr, q->st.curr_frame, "io_error", strerror(errno));

	return true;
}

bool bif_iso_close_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	return stream_close(q, n);
}

static bool bif_iso_close_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,list_or_nil);
	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		h = deref(q, h, p1_ctx);

		if (!is_compound(h)
			|| CMP_STRING_TO_CSTR(q, h, "force")
			|| CMP_STRING_TO_CSTR(q, h+1, "true"))
			return throw_error(q, h, q->latest_ctx, "domain_error", "close_option");

		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	if (is_var(p1))
		return throw_error(q, p1, p1_ctx, "instantiation_error", "close_option");

	if (!is_nil(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "list");

	return bif_iso_close_1(q);
}

static bool bif_iso_at_end_of_stream_0(query *q)
{
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (!str->ungetch && isatty(fileno(str->fp)))
		return false;

	if (!str->socket) {
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = ch;
	}

	if (!feof(str->fp) && !ferror(str->fp))
		return false;

	if (str->eof_action == eof_action_reset)
		clearerr(str->fp);

	return true;
}

static bool bif_iso_at_end_of_stream_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (strcmp(str->mode, "read") && strcmp(str->mode, "update"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (!str->ungetch && isatty(fileno(str->fp)))
		return false;

	if (!str->socket) {
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = ch;
	}

	if (!feof(str->fp) && !ferror(str->fp))
		return false;

	if (str->eof_action == eof_action_reset)
		clearerr(str->fp);

	return true;
}

static bool bif_iso_flush_output_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	int err = fflush(str->fp);

	if (err == EOF)
		return throw_error(q, q->st.curr_instr, q->st.curr_frame, "io_error", strerror(errno));

	return !ferror(str->fp);
}

static bool bif_iso_flush_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	int err = fflush(str->fp);

	if (err == EOF)
		return throw_error(q, pstr, pstr_ctx, "io_error", strerror(errno));

	return !ferror(str->fp);
}

static bool bif_iso_nl_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	fputc('\n', str->fp);
	int err = fflush(str->fp);

	if (err == EOF)
		return throw_error(q, q->st.curr_instr, q->st.curr_frame, "io_error", strerror(errno));

	return !ferror(str->fp);
}

static bool bif_iso_nl_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	fputc('\n', str->fp);
	int err = fflush(str->fp);

	if (err == EOF)
		return throw_error(q, pstr, pstr_ctx, "io_error", strerror(errno));

	return !ferror(str->fp);
}

static bool bif_iso_read_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, make_nil(), q->st.curr_frame, NULL);
}

static bool bif_iso_read_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, make_nil(), q->st.curr_frame, NULL);
}

static bool parse_read_params(query *q, stream *str, cell *c, pl_idx c_ctx, cell **vars, pl_idx *vars_ctx, cell **varnames, pl_idx *varnames_ctx, cell **sings, pl_idx *sings_ctx)
{
	parser *p = str->p;

	if (!is_compound(c)) {
		throw_error(q, c, c_ctx, "domain_error", "read_option");
		return false;
	}

	cell *c1 = deref(q, FIRST_ARG(c), c_ctx);
	pl_idx c1_ctx = q->latest_ctx;

	if (!CMP_STRING_TO_CSTR(q, c, "character_escapes")) {
		if (is_interned(c1))
			p->flags.character_escapes = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "json")) {
		if (is_interned(c1))
			p->flags.json = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "var_prefix")) {
		if (is_interned(c1))
			p->flags.var_prefix = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "double_quotes")) {
		if (is_interned(c1)) {
			if (!CMP_STRING_TO_CSTR(q, c1, "atom")) {
				p->flags.double_quote_codes = p->flags.double_quote_chars = false;
				p->flags.double_quote_atom = true;
			} else if (!CMP_STRING_TO_CSTR(q, c1, "chars")) {
				p->flags.double_quote_atom = p->flags.double_quote_codes = false;
				p->flags.double_quote_chars = true;
			} else if (!CMP_STRING_TO_CSTR(q, c1, "codes")) {
				p->flags.double_quote_atom = p->flags.double_quote_chars = false;
				p->flags.double_quote_codes = true;
			}
		}
	} else if (!CMP_STRING_TO_CSTR(q, c, "variables")) {
		if ((is_iso_list_or_nil_or_var(c1) && valid_list(q, c1, c1_ctx)) || true) {
			if (vars) *vars = c1;
			if (vars_ctx) *vars_ctx = c1_ctx;
		} else {
			throw_error(q, c, c_ctx, "domain_error", "read_option");
			return false;
		}
	} else if (!CMP_STRING_TO_CSTR(q, c, "variable_names")) {
		if ((is_iso_list_or_nil_or_var(c1) && valid_list(q, c1, c1_ctx)) || true) {
			if (varnames) *varnames = c1;
			if (varnames_ctx) *varnames_ctx = c1_ctx;
		} else {
			throw_error(q, c, c_ctx, "domain_error", "read_option");
			return false;
		}
	} else if (!CMP_STRING_TO_CSTR(q, c, "singletons")) {
		if ((is_iso_list_or_nil_or_var(c1) && valid_list(q, c1, c1_ctx)) || true) {
			if (sings) *sings = c1;
			if (sings_ctx) *sings_ctx = c1_ctx;
		} else {
			throw_error(q, c, c_ctx, "domain_error", "read_option");
			return false;
		}
	} else if (!CMP_STRING_TO_CSTR(q, c, "positions") && (c->arity == 2) && str->fp) {
		p->pos_start = ftello(str->fp);
	} else if (!CMP_STRING_TO_CSTR(q, c, "line_counts") && (c->arity == 2)) {
	} else {
		throw_error(q, c, c_ctx, "domain_error", "read_option");
		return false;
	}

	return true;
}

bool do_read_term(query *q, stream *str, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, char *src)
{
	if (!str->p) {
		str->p = parser_create(q->st.m);
		check_heap_error(str->p);
		str->p->flags = q->st.m->flags;
		str->p->fp = str->fp;
		if (q->p) str->p->no_fp = q->p->no_fp;
	} else
		reset(str->p);

	str->p->one_shot = true;
	cell *vars = NULL, *varnames = NULL, *sings = NULL;
	pl_idx vars_ctx = 0, varnames_ctx = 0, sings_ctx = 0;
	cell *p21 = p2;
	pl_idx p21_ctx = p2_ctx;

	LIST_HANDLER(p21);

	while (is_list(p21)) {
		cell *h = LIST_HEAD(p21);
		h = deref(q, h, p21_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (is_var(h))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

		if (!parse_read_params(q, str, h, h_ctx, &vars, &vars_ctx, &varnames, &varnames_ctx, &sings, &sings_ctx))
			return true;

		p21 = LIST_TAIL(p21);
		p21 = deref(q, p21, p21_ctx);
		p21_ctx = q->latest_ctx;
	}

	if (is_var(p21))
		return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

	if (!is_nil(p21))
		return throw_error(q, p2, p2_ctx, "type_error", "list");

	if (!src && !str->p->srcptr && str->fp) {
		if (str->p->no_fp || getline(&str->p->save_line, &str->p->n_line, str->fp) == -1) {
			if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
				clearerr(str->fp);
				return do_yield(q, 1);
			}

			str->p->srcptr = "";
		} else
			str->p->srcptr = str->p->save_line;
	}

	if (str->p->srcptr) {
		char *src = (char*)eat_space(str->p);

		if (str->p->error)
			return throw_error(q, q->st.curr_instr, q->st.curr_frame, "syntax_error", str->p->error_desc?str->p->error_desc:"read_term");

		str->p->line_nbr_start = str->p->line_nbr;
		str->p->srcptr = src;
	}

	for (;;) {
#if 0
		if (isatty(fileno(str->fp)) && !src) {
			fprintf(str->fp, "%s", PROMPT);
			fflush(str->fp);
		}
#endif

		if (!src && (!str->p->srcptr || !*str->p->srcptr || (*str->p->srcptr == '\n'))) {
			if (str->p->srcptr && (*str->p->srcptr == '\n'))
				str->p->line_nbr++;

			if (str->p->no_fp || getline(&str->p->save_line, &str->p->n_line, str->fp) == -1) {
				if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
					clearerr(str->fp);
					return do_yield(q, 1);
				}

				str->p->srcptr = "";
				str->at_end_of_file = str->eof_action != eof_action_reset;

				if (str->eof_action == eof_action_reset)
					clearerr(str->fp);

				if (vars)
					if (!unify(q, vars, vars_ctx, make_nil(), q->st.curr_frame))
						return false;

				if (varnames)
					if (!unify(q, varnames, varnames_ctx, make_nil(), q->st.curr_frame))
						return false;

				if (sings)
					if (!unify(q, sings, sings_ctx, make_nil(), q->st.curr_frame))
						return false;

				cell *p22 = p2;
				pl_idx p22_ctx = p2_ctx;
				LIST_HANDLER(p22);

				while (is_list(p22)) {
					cell *h = LIST_HEAD(p22);
					h = deref(q, h, p22_ctx);
					pl_idx h_ctx = q->latest_ctx;

					if (is_var(h))
						return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

					if (!CMP_STRING_TO_CSTR(q, h, "positions") && (h->arity == 2)) {
						cell *p = h+1;
						p = deref(q, p, h_ctx);
						pl_idx p_ctx = q->latest_ctx;
						cell tmp;
						make_int(&tmp, str->p->pos_start);
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
						p = h+2;
						p = deref(q, p, h_ctx);
						p_ctx = q->latest_ctx;
						make_int(&tmp, ftello(str->fp));
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
					} else if (!CMP_STRING_TO_CSTR(q, h, "line_counts") && (h->arity == 2)) {
						cell *p = h+1;
						p = deref(q, p, h_ctx);
						pl_idx p_ctx = q->latest_ctx;
						cell tmp;
						make_int(&tmp, str->p->line_nbr_start);
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
						p = h+2;
						p = deref(q, p, h_ctx);
						p_ctx = q->latest_ctx;
						make_int(&tmp, str->p->line_nbr);
						unify(q, p, p_ctx, &tmp, q->st.curr_frame);
					}

					p22 = LIST_TAIL(p22);
					p22 = deref(q, p22, p22_ctx);
					p22_ctx = q->latest_ctx;
				}

				cell tmp;
				make_atom(&tmp, g_eof_s);
				return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
			}

			//if (!*p->save_line || (*p->save_line == '\r') || (*p->save_line == '\n'))
			//	continue;

			str->p->srcptr = str->p->save_line;
		} else if (src)
			str->p->srcptr = src;

		break;
	}

	if (str->p->did_getline)
		q->is_input = true;

	frame *f = GET_CURR_FRAME();
	str->p->read_term_slots = f->actual_slots;
	str->p->do_read_term = true;
	tokenize(str->p, false, false);
	str->p->read_term_slots = 0;

	if (str->p->error || !str->p->end_of_term) {
		str->p->error = false;

		if (!str->p->fp || !isatty(fileno(str->p->fp))) {
			void *save_fp = str->p->fp;
			str->p->fp = NULL;

			while (get_token(str->p, false, false)
				&& SB_strlen(str->p->token) && SB_strcmp(str->p->token, ".")) {
			}

			str->p->fp = save_fp;
			str->p->did_getline = false;
		}

		str->p->do_read_term = false;
		return throw_error(q, make_nil(), q->st.curr_frame, "syntax_error", str->p->error_desc?str->p->error_desc:"read_term");
	}

	str->p->do_read_term = false;

	cell *p22 = p2;
	pl_idx p22_ctx = p2_ctx;
	LIST_HANDLER(p22);

	while (is_list(p22)) {
		cell *h = LIST_HEAD(p22);
		h = deref(q, h, p22_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (is_var(h))
			return throw_error(q, p2, p2_ctx, "instantiation_error", "read_option");

		if (!CMP_STRING_TO_CSTR(q, h, "positions") && (h->arity == 2)) {
			cell *p = h+1;
			p = deref(q, p, h_ctx);
			pl_idx p_ctx = q->latest_ctx;
			cell tmp;
			make_int(&tmp, str->p->pos_start);

			if (!unify(q, p, p_ctx, &tmp, q->st.curr_frame))
				return false;

			p = h+2;
			p = deref(q, p, h_ctx);
			p_ctx = q->latest_ctx;
			make_int(&tmp, ftello(str->fp));

			if (!unify(q, p, p_ctx, &tmp, q->st.curr_frame))
				return false;
		} else if (!CMP_STRING_TO_CSTR(q, h, "line_counts") && (h->arity == 2)) {
			cell *p = h+1;
			p = deref(q, p, h_ctx);
			pl_idx p_ctx = q->latest_ctx;
			cell tmp;
			make_int(&tmp, str->p->line_nbr_start);

			if (!unify(q, p, p_ctx, &tmp, q->st.curr_frame))
				return false;

			p = h+2;
			p = deref(q, p, h_ctx);
			p_ctx = q->latest_ctx;
			make_int(&tmp, str->p->line_nbr);

			if (!unify(q, p, p_ctx, &tmp, q->st.curr_frame))
				return false;
		}

		p22 = LIST_TAIL(p22);
		p22 = deref(q, p22, p22_ctx);
		p22_ctx = q->latest_ctx;
	}

	if (!str->p->cl->cidx) {
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	xref_clause(str->p->m, str->p->cl);

	if (str->p->nbr_vars) {
		if (create_vars(q, str->p->nbr_vars) < 0)
			return throw_error(q, p1, p1_ctx, "resource_error", "stack");
	}

	q->tab_idx = 0;

	if (str->p->nbr_vars)
		collect_vars(q, str->p->cl->cells, q->st.curr_frame);

	if (vars) {
		unsigned cnt = q->tab_idx;
		check_heap_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*2)+1);
		check_heap_error(tmp);
		unsigned idx = 0;

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->tab_idx; i++) {
				make_atom(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*2)+1;
				cell v;
				make_ref(&v, q->pl->tabs[i].var_nbr, q->st.curr_frame);
				tmp[idx++] = v;
				done++;
			}

			make_atom(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			check_heap_error(tmp);
			dup_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			if (!unify(q, vars, vars_ctx, tmp, q->st.curr_frame))
				return false;
		} else {
			if (!unify(q, vars, vars_ctx, make_nil(), q->st.curr_frame))
				return false;
		}
	}

	if (varnames) {
		unsigned cnt = 0;
		check_heap_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*4)+1);
		check_heap_error(tmp);
		unsigned idx = 0;

		for (unsigned i = 0; i < q->tab_idx; i++) {
			if (q->pl->tabs[i].is_anon)
				continue;

			cnt++;
		}

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->tab_idx; i++) {
				if (q->pl->tabs[i].is_anon)
					continue;

				make_atom(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*4)+1;
				cell v;
				make_atom(&v, g_unify_s);
				v.flags |= FLAG_BUILTIN;
				//v.bif_ptr = get_fn_ptr(bif_iso_unify_2);
				v.arity = 2;
				v.nbr_cells = 3;
				SET_OP(&v,OP_XFX);
				tmp[idx++] = v;
				make_atom(&v, q->pl->tabs[i].val_off);
				tmp[idx++] = v;
				make_ref(&v, q->pl->tabs[i].var_nbr, q->st.curr_frame);
				v.flags |= FLAG_VAR_FRESH;
				tmp[idx++] = v;
				done++;
			}

			make_atom(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			check_heap_error(tmp);
			dup_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			if (!unify(q, varnames, varnames_ctx, tmp, q->st.curr_frame))
				return false;
		} else {
			if (!unify(q, varnames, varnames_ctx, make_nil(), q->st.curr_frame))
				return false;
		}
	}

	if (sings) {
		unsigned cnt = 0;
		check_heap_error(init_tmp_heap(q));
		cell *tmp = alloc_on_tmp(q, (cnt*4)+1);
		check_heap_error(tmp);
		unsigned idx = 0;

		for (unsigned i = 0; i < q->tab_idx; i++) {
			if (q->pl->tabs[i].cnt != 1)
				continue;

			if (varnames && (q->pl->tabs[i].is_anon))
				continue;

			cnt++;
		}

		if (cnt) {
			unsigned done = 0;

			for (unsigned i = 0; i < q->tab_idx; i++) {
				if (q->pl->tabs[i].cnt != 1)
					continue;

				if (varnames && (q->pl->tabs[i].is_anon))
					continue;

				make_atom(tmp+idx, g_dot_s);
				tmp[idx].arity = 2;
				tmp[idx++].nbr_cells = ((cnt-done)*4)+1;
				cell v;
				make_atom(&v, g_unify_s);
				v.flags |= FLAG_BUILTIN;
				//v.bif_ptr = get_fn_ptr(bif_iso_unify_2);
				v.arity = 2;
				v.nbr_cells = 3;
				SET_OP(&v,OP_XFX);
				tmp[idx++] = v;
				make_atom(&v, q->pl->tabs[i].val_off);
				tmp[idx++] = v;
				make_ref(&v, q->pl->tabs[i].var_nbr, q->st.curr_frame);
				v.flags |= FLAG_VAR_FRESH;
				tmp[idx++] = v;
				done++;
			}

			make_atom(tmp+idx++, g_nil_s);
			tmp[0].arity = 2;
			tmp[0].nbr_cells = idx;

			cell *save = tmp;
			tmp = alloc_on_heap(q, idx);
			check_heap_error(tmp);
			dup_cells(tmp, save, idx);
			tmp->nbr_cells = idx;
			if (!unify(q, sings, sings_ctx, tmp, q->st.curr_frame))
				return false;
		} else {
			if (!unify(q, sings, sings_ctx, make_nil(), q->st.curr_frame))
				return false;
		}
	}

	cell *tmp = alloc_on_heap(q, str->p->cl->cidx-1);
	check_heap_error(tmp);
	dup_cells(tmp, str->p->cl->cells, str->p->cl->cidx-1);
	bool ok = unify(q, p1, p1_ctx, tmp, q->st.curr_frame);
	clear_clause(str->p->cl);
	return ok;
}

static bool bif_iso_read_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static bool bif_iso_read_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	return do_read_term(q, str, p1, p1_ctx, p2, p2_ctx, NULL);
}

static bool bif_iso_write_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;

	if (isatty(fileno(str->fp)))
		fflush(str->fp);

	return !ferror(str->fp);
}

static bool bif_iso_write_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;

	if (isatty(fileno(str->fp))) {
		if (fflush(str->fp))
			return false;
	}

	return !ferror(str->fp);
}

static bool bif_iso_writeq_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->quoted = 1;
	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;
	q->quoted = 0;

	if (isatty(fileno(str->fp)))
		fflush(str->fp);

	return !ferror(str->fp);
}

static bool bif_iso_writeq_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->quoted = 1;
	q->numbervars = true;
	print_term_to_stream(q, str, p1, p1_ctx, 1);
	q->numbervars = false;
	q->quoted = 0;

	if (isatty(fileno(str->fp)))
		fflush(str->fp);

	return !ferror(str->fp);
}

static bool bif_iso_write_canonical_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	print_canonical(q, str->fp, p1, p1_ctx, 1);

	if (isatty(fileno(str->fp)))
		fflush(str->fp);

	return !ferror(str->fp);
}

static bool bif_iso_write_canonical_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	print_canonical(q, str->fp, p1, p1_ctx, 1);

	if (isatty(fileno(str->fp)))
		fflush(str->fp);

	return !ferror(str->fp);
}

bool parse_write_params(query *q, cell *c, pl_idx c_ctx, cell **vnames, pl_idx *vnames_ctx)
{
	if (is_var(c)) {
		throw_error(q, c, c_ctx, "instantiation_error", "write_option");
		return false;
	}

	if (!is_interned(c) || !is_compound(c)) {
		throw_error(q, c, c_ctx, "domain_error", "write_option");
		return false;
	}

	cell *c1 = deref(q, FIRST_ARG(c), c_ctx);
	pl_idx c1_ctx = q->latest_ctx;

	if (!CMP_STRING_TO_CSTR(q, c, "max_depth")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (is_integer(c1) && (get_smallint(c1) >= 0))
			q->max_depth = get_smallint(c1);
		else {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}
	} else if (!CMP_STRING_TO_CSTR(q, c, "fullstop")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STRING_TO_CSTR(q, c1, "true") && CMP_STRING_TO_CSTR(q, c1, "false"))) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->fullstop = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "nl")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STRING_TO_CSTR(q, c1, "true") && CMP_STRING_TO_CSTR(q, c1, "false"))) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->nl = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "json")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STRING_TO_CSTR(q, c1, "true") && CMP_STRING_TO_CSTR(q, c1, "false"))) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->json = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "quoted")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STRING_TO_CSTR(q, c1, "true") && CMP_STRING_TO_CSTR(q, c1, "false"))) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->quoted = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "varnames")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STRING_TO_CSTR(q, c1, "true") && CMP_STRING_TO_CSTR(q, c1, "false"))) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->varnames = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "ignore_ops")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STRING_TO_CSTR(q, c1, "true") && CMP_STRING_TO_CSTR(q, c1, "false"))) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->ignore_ops = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "numbervars")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STRING_TO_CSTR(q, c1, "true") && CMP_STRING_TO_CSTR(q, c1, "false"))) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->numbervars = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "double_quotes")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_interned(c1) || (CMP_STRING_TO_CSTR(q, c1, "true") && CMP_STRING_TO_CSTR(q, c1, "false"))) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		q->double_quotes = !CMP_STRING_TO_CSTR(q, c1, "true");
	} else if (!CMP_STRING_TO_CSTR(q, c, "variable_names")) {
		if (is_var(c1)) {
			throw_error(q, c1, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_list_or_nil(c1)) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		cell *c1_orig = c1;
		pl_idx c1_orig_ctx = c1_ctx;
		LIST_HANDLER(c1);

		while (is_list(c1)) {
			cell *h = LIST_HEAD(c1);
			h = deref(q, h, c1_ctx);
			pl_idx h_ctx = q->latest_ctx;

			if (is_var(h)) {
				throw_error(q, h, h_ctx, "instantiation_error", "write_option");
				return false;
			}

			if (!is_compound(h)) {
				throw_error(q, c, c_ctx, "domain_error", "write_option");
				return false;
			}

			if (CMP_STRING_TO_CSTR(q, h, "=")) {
				throw_error(q, c, c_ctx, "domain_error", "write_option");
				return false;
			}

			if (is_interned(h)) {
				cell *h1 = deref(q, h+1, h_ctx);

				if (is_var(h1)) {
					throw_error(q, c, c_ctx, "instantiation_error", "write_option");
					return false;
				} else if (!is_atom(h1)) {
					throw_error(q, c, c_ctx, "domain_error", "write_option");
					return false;
				}

#if 0
				cell *h2 = deref(q, h+2, h_ctx);

				if (!is_var(h2)) {
					throw_error(q, c, c_ctx, "domain_error", "write_option");
					return false;
				}
#endif
			}

			c1 = LIST_TAIL(c1);
			c1 = deref(q, c1, c1_ctx);
			c1_ctx = q->latest_ctx;
		}

		if (is_var(c1)) {
			throw_error(q, c1_orig, c_ctx, "instantiation_error", "write_option");
			return false;
		}

		if (!is_nil(c1)) {
			throw_error(q, c, c_ctx, "domain_error", "write_option");
			return false;
		}

		if (vnames) *vnames = c1_orig;
		if (vnames_ctx) *vnames_ctx = c1_orig_ctx;
	} else {
		throw_error(q, c, c_ctx, "domain_error", "write_option");
		return false;
	}

	return true;
}

static bool bif_iso_write_term_2(query *q)
{
	GET_FIRST_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->flags = q->st.m->flags;
	q->numbervars = false;
	cell *p2_orig = p2, *vnames = NULL;
	pl_idx p2_orig_ctx = p2_ctx, vnames_ctx = 0;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (!parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx)) {
			clear_write_options(q);
			return true;
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (is_var(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "instantiation_error", "write_option");
	}

	if (!is_nil(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "type_error", "list");
	}

	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;

	if (q->ignore_ops)
		print_canonical_to_stream(q, str, p1, p1_ctx, 1);
	else
		print_term_to_stream(q, str, p1, p1_ctx, 1);

	if (q->fullstop)
		net_write(".", 1, str);

	if (q->nl) {
		net_write("\n", 1, str);
		//fflush(str->fp);
	}

	clear_write_options(q);
	return !ferror(str->fp);
}

static bool bif_iso_write_term_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	q->flags = q->st.m->flags;
	q->numbervars = false;
	cell *p2_orig = p2, *vnames = NULL;
	pl_idx p2_orig_ctx = p2_ctx, vnames_ctx;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (!parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx)) {
			clear_write_options(q);
			return true;
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	if (is_var(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "instantiation_error", "write_option");
	}

	if (!is_nil(p2)) {
		clear_write_options(q);
		return throw_error(q, p2_orig, p2_orig_ctx, "type_error", "list");
	}

	q->latest_ctx = p1_ctx;
	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;

	if (q->ignore_ops)
		print_canonical_to_stream(q, str, p1, p1_ctx, 1);
	else
		print_term_to_stream(q, str, p1, p1_ctx, 1);

	if (q->fullstop)
		net_write(".", 1, str);

	if (q->nl) {
		net_write("\n", 1, str);
		//fflush(str->fp);
	}

	clear_write_options(q);
	return !ferror(str->fp);
}

static bool bif_iso_put_char_1(query *q)
{
	GET_FIRST_ARG(p1,character);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	size_t len = len_char_utf8(C_STR(q, p1));

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (len != C_STRLEN(q, p1))
		return throw_error(q, p1, p1_ctx, "type_error", "character");

	const char *src = C_STR(q, p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static bool bif_iso_put_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,character);
	size_t len = len_char_utf8(C_STR(q, p1));

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (len != C_STRLEN(q, p1))
		return throw_error(q, p1, p1_ctx, "type_error", "character");

	const char *src = C_STR(q, p1);
	int ch = get_char_utf8(&src);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static bool bif_iso_put_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_smallint(p1);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static bool bif_iso_put_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,binary_stream");
	}

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_smallint(p1);
	char tmpbuf[80];
	put_char_utf8(tmpbuf, ch);
	net_write(tmpbuf, strlen(tmpbuf), str);
	return !ferror(str->fp);
}

static bool bif_iso_put_byte_1(query *q)
{
	GET_FIRST_ARG(p1,byte);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "output,text_stream");
	}

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_smallint(p1);
	char tmpbuf[80];
	snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
	net_write(tmpbuf, 1, str);
	return !ferror(str->fp);
}

static bool bif_iso_put_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,byte);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	if (!str->binary)
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,text_stream");

	if (!is_integer(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "integer");

	if (is_integer(p1) && is_le(p1,-1))
		return throw_error(q, p1, p1_ctx, "representation_error", "character_code");

	int ch = (int)get_smallint(p1);
	char tmpbuf[80];
	snprintf(tmpbuf, sizeof(tmpbuf), "%c", ch);
	net_write(tmpbuf, 1, str);
	return !ferror(str->fp);
}

static bool bif_iso_get_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	}

	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_get_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_character_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	}

	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_get_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	} else if (ch == EOF)
		str->did_getc = false;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_get_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer_or_var);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	if (ch == '\n') {
		str->did_getc = false;

		if (str->p)
			str->p->line_nbr++;
	}

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_get_byte_1(query *q)
{
	GET_FIRST_ARG(p1,in_byte_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = *str->p->srcptr++;
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;

	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_get_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_byte_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = *str->p->srcptr;
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	str->did_getc = true;

	if (FEOF(str)) {
		str->did_getc = false;
		str->at_end_of_file = str->eof_action != eof_action_reset;

		if (str->eof_action == eof_action_reset)
			clearerr(str->fp);

		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = 0;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_unget_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	str->did_getc = false;
	str->at_end_of_file = false;
	str->ungetch = get_smallint(p1);
	return true;
}

static bool bif_unget_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_character);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = get_char_utf8((const char**)&str->p->srcptr);
			str->ungetch = ch;
		}
	}

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	str->did_getc = false;
	str->at_end_of_file = false;
	str->ungetch = get_smallint(p1);
	return true;
}

static bool bif_unget_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	str->did_getc = false;
	str->at_end_of_file = false;
	str->ungetch = get_smallint(p1);
	return true;
}

static bool bif_unget_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	str->did_getc = false;
	str->at_end_of_file = false;
	str->ungetch = get_smallint(p1);
	return true;
}

static bool bif_unget_byte_1(query *q)
{
	GET_FIRST_ARG(p1,in_byte);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	str->did_getc = false;
	str->at_end_of_file = false;
	str->ungetch = get_smallint(p1);
	return true;
}

static bool bif_unget_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_byte);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	str->did_getc = false;
	str->at_end_of_file = false;
	str->ungetch = get_smallint(p1);
	return true;
}

static bool bif_iso_peek_char_1(query *q)
{
	GET_FIRST_ARG(p1,in_character_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}


	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_peek_char_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_character_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	char tmpbuf[80];
	n = put_char_utf8(tmpbuf, ch);
	cell tmp;
	make_smalln(&tmp, tmpbuf, n);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_peek_code_1(query *q)
{
	GET_FIRST_ARG(p1,integer_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_peek_code_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer_or_var);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && (get_smallint(p1) < -1))
		return throw_error(q, p1, p1_ctx, "representation_error", "in_character_code");

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,binary_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	if (FEOF(str)) {
		str->did_getc = false;
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_peek_byte_1(query *q)
{
	GET_FIRST_ARG(p1,in_byte_or_var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	if (FEOF(str)) {
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_peek_byte_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,in_byte_or_var);

	if (strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	if (!str->binary) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,text_stream");
	}

	if (str->at_end_of_file && (str->eof_action == eof_action_error)) {
		cell tmp;
		make_int(&tmp, n);
		tmp.flags |= FLAG_INT_HEX;
		return throw_error(q, &tmp, q->st.curr_frame, "permission_error", "input,past_end_of_stream");
	}

	if (!str->ungetch && str->p) {
		if (str->p->srcptr && *str->p->srcptr) {
			int ch = peek_char_utf8((const char*)str->p->srcptr);
			str->ungetch = ch;
		}
	}

	int ch = str->ungetch ? str->ungetch : net_getc(str);

	if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
		clearerr(str->fp);
		return do_yield(q, 1);
	}

	if (FEOF(str)) {
		clearerr(str->fp);
		cell tmp;
		make_int(&tmp, -1);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	str->ungetch = ch;
	cell tmp;
	make_int(&tmp, ch);
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool bif_iso_current_input_1(query *q)
{
	GET_FIRST_ARG(pstr,any);

	if (is_var(pstr)) {
		cell tmp;
		make_int(&tmp, q->pl->current_input);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
		return unify(q, pstr, pstr_ctx, &tmp, q->st.curr_frame);
	}

	if (!is_stream(pstr))
		return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");

	int n = get_stream(q, pstr);
	return n == (int)q->pl->current_input ? true : false;
}

static bool bif_iso_current_output_1(query *q)
{
	GET_FIRST_ARG(pstr,any);

	if (is_var(pstr)) {
		cell tmp;
		make_int(&tmp, q->pl->current_output);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
		return unify(q, pstr, pstr_ctx, &tmp, q->st.curr_frame);
	}

	if (!is_stream(pstr))
		return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");

	int n = get_stream(q, pstr);
	return n == (int)q->pl->current_output ? true : false;
}

static bool bif_iso_current_error_1(query *q)
{
	GET_FIRST_ARG(pstr,any);

	if (is_var(pstr)) {
		cell tmp;
		make_int(&tmp, q->pl->current_error);
		tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
		return unify(q, pstr, pstr_ctx, &tmp, q->st.curr_frame);
	}

	if (!is_stream(pstr))
		return throw_error(q, pstr, q->st.curr_frame, "domain_error", "stream");

	int n = get_stream(q, pstr);
	return n == (int)q->pl->current_error ? true : false;
}

static bool bif_iso_set_input_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (strcmp(str->mode, "read") && strcmp(str->mode, "update"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "input,stream");

	q->pl->current_input = n;
	return true;
}

static bool bif_iso_set_output_1(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	if (!strcmp(str->mode, "read"))
		return throw_error(q, pstr, q->st.curr_frame, "permission_error", "output,stream");

	q->pl->current_output = n;
	return true;
}

static bool bif_iso_set_stream_position_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);

	if (str->socket || (n <= 2) || !str->repo)
		return throw_error(q, p1, p1_ctx, "permission_error", "reposition,stream");

	if (!is_smallint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_position");

	off_t pos = get_smallint(p1);

	if (fseeko(str->fp, pos, SEEK_SET))
		return throw_error(q, p1, p1_ctx, "domain_error", "position");

	return true;
}

static bool bif_sys_read_term_from_chars_4(query *q)
{
	GET_FIRST_ARG(p_term,any);
	GET_NEXT_ARG(p_opts,list_or_nil);
	GET_NEXT_ARG(p_chars,any);
	GET_NEXT_ARG(p_rest,any);
	stream tmps;
	memset(&tmps, 0, sizeof(stream));
	stream *str = &tmps;
	char *src = NULL;
	bool has_var, is_partial;
	size_t srclen;

	if (is_atom(p_chars) && !is_string(p_chars)) {
		if (!strcmp(C_STR(q, p_chars), "[]")) {
			cell tmp;
			make_atom(&tmp, g_eof_s);
			return unify(q, p_term, p_term_ctx, &tmp, q->st.curr_frame);
		} else
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");
	} else if (is_string(p_chars)) {
		src = C_STR(q, p_chars);
		srclen = C_STRLEN(q, p_chars);
	} else if (!check_list(q, p_chars, p_chars_ctx, &is_partial, NULL)) {
		return throw_error(q, p_chars, p_chars_ctx, "type_error", "list");
	} else if ((srclen = scan_is_chars_list2(q, p_chars, p_chars_ctx, false, &has_var, &is_partial)) > 0) {
		if (!srclen)
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");

		src = chars_list_to_string(q, p_chars, p_chars_ctx, srclen);
	} else {
		if (has_var)
			return throw_error(q, p_chars, p_chars_ctx, "instantiation_error", "var");

		return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");
	}

	str->p = parser_create(q->st.m);
	str->p->flags = q->st.m->flags;
	str->p->fp = str->fp;
	reset(str->p);
	str->p->srcptr = src;

	if (!src || !*src) {
		parser_destroy(str->p);
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p_term, p_term_ctx, &tmp, q->st.curr_frame);
	}

	bool ok = do_read_term(q, str, p_term, p_term_ctx, p_opts, p_opts_ctx, NULL);

	if (ok != true) {
		if (!is_string(p_chars))
			free(src);

		parser_destroy(str->p);
		str->p = NULL;
		return false;
	}

	char *rest = str->p->srcptr = eat_space(str->p);

	if (str->p->error) {
		parser_destroy(str->p);
		return throw_error(q, q->st.curr_instr, q->st.curr_frame, "syntax_error", str->p->error_desc?str->p->error_desc:"read_term");
	}

	cell tmp;

	if (*rest) {
		const char *ptr = strstr(src, rest);
		size_t off = ptr - src;
		size_t len = srclen - off;

		if (!is_string(p_chars))
			make_string(&tmp, rest);
		else
			make_slice(q, &tmp, p_chars, off, len);
	} else {
		make_atom(&tmp, g_nil_s);
	}

	parser_destroy(str->p);

	if (!is_string(p_chars))
		free(src);

	unify(q, p_rest, p_rest_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_read_term_from_chars_3(query *q)
{
	GET_FIRST_ARG(p_chars,any);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p_opts,list_or_nil);
	stream tmps;
	memset(&tmps, 0, sizeof(stream));
	stream *str = &tmps;
	char *src = NULL;
	size_t len;
	bool has_var, is_partial;

	if (is_atom(p_chars) && !is_string(p_chars)) {
		if (!strcmp(C_STR(q, p_chars), "[]")) {
			cell tmp;
			make_atom(&tmp, g_eof_s);
			return unify(q, p_term, p_term_ctx, &tmp, q->st.curr_frame);
		} else
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");
	} else if (is_string(p_chars)) {
		len = C_STRLEN(q, p_chars);
		src = malloc(len+1+1);		// +1 is to allow adding a '.'
		check_heap_error(src);
		memcpy(src, C_STR(q, p_chars), len);
		src[len] = '\0';
	} else if (!check_list(q, p_chars, p_chars_ctx, &is_partial, NULL)) {
		return throw_error(q, p_chars, p_chars_ctx, "type_error", "list");
	} else if ((len = scan_is_chars_list2(q, p_chars, p_chars_ctx, false, &has_var, &is_partial)) > 0) {
		if (!len)
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");

		src = chars_list_to_string(q, p_chars, p_chars_ctx, len);
	} else {
		if (has_var)
			return throw_error(q, p_chars, p_chars_ctx, "instantiation_error", "var");

		return throw_error(q, p_chars, p_chars_ctx, "type_error", "character");
	}

	str->p = parser_create(q->st.m);
	str->p->flags = q->st.m->flags;
	str->p->fp = str->fp;
	reset(str->p);
	char *save_src = src;
	str->p->srcptr = src;

	if (!src || !*src) {
		parser_destroy(str->p);
		free(save_src);
		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p_term, p_term_ctx, &tmp, q->st.curr_frame);
	}

	const char *end_ptr = src + strlen(src) - 1;

	while (isspace(*end_ptr) && (end_ptr != src))
		end_ptr--;

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	bool ok = do_read_term(q, str, p_term, p_term_ctx, p_opts, p_opts_ctx, NULL);
	free(save_src);
	parser_destroy(str->p);

	if (ok != true)
		return false;

	return ok;
}

static bool bif_read_term_from_atom_3(query *q)
{
	GET_FIRST_ARG(p_chars,any);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p_opts,list_or_nil);
	stream tmps;
	memset(&tmps, 0, sizeof(stream));
	stream *str = &tmps;
	char *src;
	size_t len;

	if (is_cstring(p_chars)) {
		len = C_STRLEN(q, p_chars);
		src = malloc(len+1+1);	// final +1 is for look-ahead
		check_heap_error(src);
		memcpy(src, C_STR(q, p_chars), len);
		src[len] = '\0';
	} else if ((len = scan_is_chars_list(q, p_chars, p_chars_ctx, false)) > 0) {
		if (!len)
			return throw_error(q, p_chars, p_chars_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p_chars, p_chars_ctx, len);
	} else
		return throw_error(q, p_chars, p_chars_ctx, "type_error", "atom");

	const char *end_ptr = src + strlen(src) - 1;

	while (isspace(*end_ptr) && (end_ptr != src))
		end_ptr--;

	if (src[strlen(src)-1] != '.')
		strcat(src, ".");

	bool ok = do_read_term(q, str, p_term, p_term_ctx, p_opts, p_opts_ctx, src);
	parser_destroy(str->p);
	free(src);
	return ok;
}

static bool bif_write_term_to_atom_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx vnames_ctx = 0;
	q->flags = q->st.m->flags;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;
	char *dst = print_term_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	make_cstring(&tmp, dst);
	free(dst);
	bool ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_write_term_to_chars_3(query *q)
{
	GET_FIRST_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	GET_NEXT_ARG(p_chars,atom_or_var);
	cell *vnames = NULL;
	pl_idx vnames_ctx = 0;
	q->flags = q->st.m->flags;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	q->variable_names = vnames;
	q->variable_names_ctx = vnames_ctx;
	char *dst = print_term_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	make_string(&tmp, dst);
	free(dst);
	bool ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_write_canonical_to_chars_3(query *q)
{
	GET_FIRST_ARG(p_chars,atom_or_var);
	GET_NEXT_ARG(p_term,any);
	GET_NEXT_ARG(p2,list_or_nil);
	cell *vnames = NULL;
	pl_idx vnames_ctx = 0;
	q->flags = q->st.m->flags;
	LIST_HANDLER(p2);

	while (is_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx h_ctx = q->latest_ctx;
		parse_write_params(q, h, h_ctx, &vnames, &vnames_ctx);
		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	char *dst = print_canonical_to_strbuf(q, p_term, p_term_ctx, 1);
	clear_write_options(q);
	cell tmp;
	make_string(&tmp, dst);
	free(dst);
	bool ok = unify(q, p_chars, p_chars_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_edin_redo_1(query *q)
{
	GET_FIRST_ARG(p1,integer);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	for (;;) {
		str->did_getc = true;
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = 0;

		if (feof(str->fp)) {
			str->did_getc = false;
			break;
		} else if (ch == '\n')
			str->did_getc = false;

		if (ch == get_smallint(p1))
			break;
	}

	return true;
}

static bool bif_edin_redo_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,integer);

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (isatty(fileno(str->fp)) && !str->did_getc && !str->ungetch) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	for (;;) {
		str->did_getc = true;
		int ch = str->ungetch ? str->ungetch : xgetc_utf8(net_getc, str);
		str->ungetch = 0;

		if (feof(str->fp)) {
			str->did_getc = false;
			break;
		} else if (ch == '\n')
			str->did_getc = false;

		if (ch == get_smallint(p1))
			break;
	}

	return true;
}

static bool bif_edin_tab_1(query *q)
{
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, p1_tmp_ctx, "type_error", "integer");

	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	for (int i = 0; i < get_smallint(&p1); i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static bool bif_edin_tab_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_FIRST_ARG(p1_tmp,any);
	cell p1 = eval(q, p1_tmp);

	if (!is_integer(&p1))
		return throw_error(q, &p1, p1_tmp_ctx, "type_error", "integer");

	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	for (int i = 0; i < get_smallint(&p1); i++)
		fputc(' ', str->fp);

	return !ferror(str->fp);
}

static bool bif_edin_seen_0(query *q)
{
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];

	if (n <= 2)
		return true;

	if ((str->fp != stdin)
		&& (str->fp != stdout)
		&& (str->fp != stderr))
		fclose(str->fp);

	sl_destroy(str->alias);
	free(str->filename);
	free(str->mode);
	memset(str, 0, sizeof(stream));
	q->pl->current_input = 0;
	return true;
}

static bool bif_edin_told_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (n <= 2)
		return true;

	if ((str->fp != stdin)
		&& (str->fp != stdout)
		&& (str->fp != stderr))
		fclose(str->fp);

	sl_destroy(str->alias);
	free(str->filename);
	free(str->mode);
	memset(str, 0, sizeof(stream));
	q->pl->current_output = 0;
	return true;
}

static bool bif_edin_seeing_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	sliter *iter = sl_first(q->pl->streams[q->pl->current_input].alias);
	sl_next(iter, NULL);
	const char *alias = sl_key(iter);
	sl_done(iter);
	const char *name = q->pl->current_input==0?"user":alias;
	cell tmp;
	make_cstring(&tmp, name);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_edin_telling_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	sliter *iter = sl_first(q->pl->streams[q->pl->current_output].alias);
	sl_next(iter, NULL);
	const char *alias = sl_key(iter);
	sl_done(iter);
	const char *name =q->pl->current_output==1?"user":alias;
	cell tmp;
	make_cstring(&tmp, name);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_read_line_to_string_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);

		if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
			clearerr(str->fp);
			return do_yield(q, 1);
		}

		cell tmp;
		make_atom(&tmp, g_eof_s);
		return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	}

	len = strlen(line);

	if (len && (line[len-1] == '\n')) {
		line[len-1] = '\0';
		len--;
	}

	if (len && (line[len-1] == '\r')) {
		line[len-1] = '\0';
		len--;
	}

	cell tmp;
	make_string(&tmp, line);
	free(line);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_read_file_to_string_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,list_or_nil);
	char *filename;
	char *src = NULL;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		src = chars_list_to_string(q, p1, p1_ctx, len);
		filename = src;
	} else
		filename = src = DUP_STRING(q, p1);

	convert_path(filename);

	bool bom_specified = false, use_bom = false, is_binary = false;
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);

		if (is_var(c))
			return throw_error(q, c, q->latest_ctx, "instantiation_error", "args_not_sufficiently_instantiated");

		if (is_compound(c) && (c->arity == 1)) {
			cell *name = c + 1;
			name = deref(q, name, q->latest_ctx);

			if (!CMP_STRING_TO_CSTR(q, c, "type")) {
				if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "binary")) {
					is_binary = true;
				} else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "text"))
					is_binary = false;
				else
					return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");
			} else if (!CMP_STRING_TO_CSTR(q, c, "bom")) {
				bom_specified = true;

				if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "true"))
					use_bom = true;
				else if (is_atom(name) && !CMP_STRING_TO_CSTR(q, name, "false"))
					use_bom = false;
			}
		} else
			return throw_error(q, c, q->latest_ctx, "domain_error", "stream_option");

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;

		if (is_var(p3))
			return throw_error(q, p3, p3_ctx, "instantiation_error", "args_not_sufficiently_instantiated");
	}

	FILE *fp = fopen(filename, is_binary?"rb":"r");
	free(src);

	if (!fp)
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");

	// Check for a BOM

	size_t offset = 0;

	if (!is_binary && (!bom_specified || use_bom)) {
		int ch = getc_utf8(fp);

		if ((unsigned)ch != 0xFEFF)
			fseek(fp, 0, SEEK_SET);
		else
			offset = 3;
	}

	struct stat st = {0};

	if (fstat(fileno(fp), &st)) {
		return false;
	}

	size_t len = st.st_size - offset;
	char *s = malloc(len+1);
	check_heap_error(s, fclose(fp));

	if (fread(s, 1, len, fp) != (size_t)len) {
		free(s);
		fclose(fp);
		return throw_error(q, p1, p1_ctx, "domain_error", "cannot_read");
	}

	s[st.st_size] = '\0';
	fclose(fp);
	cell tmp;
	make_stringn(&tmp, s, len);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	free(s);
	return ok;
}

static bool do_consult(query *q, cell *p1, pl_idx p1_ctx)
{
	if (is_atom(p1)) {
		char *src = DUP_STRING(q, p1);
		char *filename = relative_to(q->st.m->filename, src);
		convert_path(filename);
		unload_file(q->st.m, filename);
		free(src);

		if (!load_file(q->st.m, filename, false)) {
			free(filename);
			return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
		}

		free(filename);
		return true;
	}

	if (!is_compound(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	if (CMP_STRING_TO_CSTR(q, p1, ":"))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	cell *mod = deref(q, p1+1, p1_ctx);
	cell *file = deref(q, p1+2, p1_ctx);

	if (!is_atom(mod) || !is_atom(file))
		return throw_error(q, p1, p1_ctx, "type_error", "atom");

	module *tmp_m = module_create(q->pl, C_STR(q, mod));
	char *filename = C_STR(q, file);
	tmp_m->make_public = 1;
	filename = relative_to(q->st.m->filename, filename);
	convert_path(filename);
	unload_file(q->st.m, filename);

	if (!load_file(tmp_m, filename, false)) {
		module_destroy(tmp_m);
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "source_sink");
	}

	free(filename);
	return true;
}

static bool do_deconsult(query *q, cell *p1, pl_idx p1_ctx)
{
	if (is_atom(p1)) {
		char *src = DUP_STRING(q, p1);
		char *filename = relative_to(q->st.m->filename, src);
		convert_path(filename);
		unload_file(q->st.m, filename);
		free(filename);
		free(src);
		return true;
	}

	if (!is_compound(p1))
		return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

	if (CMP_STRING_TO_CSTR(q, p1, ":"))
		return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

	cell *mod = deref(q, p1+1, p1_ctx);
	cell *file = deref(q, p1+2, p1_ctx);

	if (!is_atom(mod) || !is_atom(file))
		return throw_error(q, p1, p1_ctx, "type_error", "source_sink");

	module *tmp_m = module_create(q->pl, C_STR(q, mod));
	char *filename = C_STR(q, file);
	tmp_m->make_public = 1;
	filename = relative_to(q->st.m->filename, filename);
	convert_path(filename);
	unload_file(q->st.m, filename);
	free(filename);
	return true;
}

static bool bif_load_files_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);

	if (is_atom(p1)) {
		check_heap_error(do_consult(q, p1, p1_ctx));
		return true;
	}

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		cell *c = deref(q, h, p1_ctx);
		pl_idx c_ctx = q->latest_ctx;
		check_heap_error(do_consult(q, c, c_ctx));
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return true;
}

static bool bif_unload_files_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_compound);

	if (is_atom(p1)) {
		check_heap_error(do_deconsult(q, p1, p1_ctx));
		return true;
	}

	LIST_HANDLER(p1);

	while (is_list(p1)) {
		cell *h = LIST_HEAD(p1);
		cell *c = deref(q, h, p1_ctx);
		pl_idx c_ctx = q->latest_ctx;
		check_heap_error(do_deconsult(q, c, c_ctx));
		p1 = LIST_TAIL(p1);
		p1 = deref(q, p1, p1_ctx);
		p1_ctx = q->latest_ctx;
	}

	return true;
}

static bool bif_make_0(query *q)
{
	for (module *m = q->pl->modules; m; m = m->next)
		make(m);

	return true;
}

static bool bif_savefile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	FILE *fp = fopen(filename, "wb");
	check_heap_error(fp);
	fwrite(C_STR(q, p2), 1, C_STRLEN(q, p2), fp);
	fclose(fp);
	free(filename);
	return true;
}

static bool bif_loadfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,var);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	FILE *fp = fopen(filename, "rb");
	free(filename);

	if (!fp)
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");

	// Check for a BOM

	int ch = getc_utf8(fp), offset = 0;

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);
	else
		offset = 3;

	struct stat st = {0};

	if (fstat(fileno(fp), &st)) {
		return false;
	}

	size_t len = st.st_size - offset;
	char *s = malloc(len+1);
	check_heap_error(s, fclose(fp));

	if (fread(s, 1, len, fp) != (size_t)len) {
		free(s);
		fclose(fp);
		return throw_error(q, p1, p1_ctx, "domain_error", "cannot_read");
	}

	s[st.st_size] = '\0';
	fclose(fp);
	cell tmp;
	make_stringn(&tmp, s, len);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	free(s);
	return ok;
}

static bool bif_getfile_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,var);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	FILE *fp = fopen(filename, "r");
	free(filename);

	if (!fp)
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");

	// Check for a BOM

	int ch = getc_utf8(fp);

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);

	char *line = NULL;
	size_t len = 0;
	int nbr = 1;
	check_heap_error(init_tmp_heap(q));

	while (getline(&line, &len, fp) != -1) {
		int len = strlen(line);

		if (len && (line[len-1] == '\n')) {
			line[len-1] = '\0';
			len--;
		}

		if (len && (line[len-1] == '\r')) {
			line[len-1] = '\0';
			len--;
		}

		cell tmp;
		make_stringn(&tmp, line, len);
		append_list(q, &tmp);
	}

	free(line);
	fclose(fp);
	cell *l = end_list(q);
	check_heap_error(l);
	unify(q, p2, p2_ctx, l, q->st.curr_frame);
	return true;
}

static bool get_terminator(query *q, cell *l, pl_idx l_ctx)
{
	bool terminator = false;
	LIST_HANDLER(l);

	while (is_iso_list(l)) {
		cell *h = LIST_HEAD(l);
		h = deref(q, h, l_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (is_compound(h)) {
			if (!CMP_STRING_TO_CSTR(q, h, "terminator")) {
				h = h + 1;
				h = deref(q, h, h_ctx);

				if (is_atom(h))
					terminator = !CMP_STRING_TO_CSTR(q, h, "true");
			}
		}

		l = LIST_TAIL(l);
		l = deref(q, l, l_ctx);
		l_ctx = q->latest_ctx;
	}

	return terminator;
}

static bool bif_getfile_3(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,list_or_nil);
	char *filename;
	bool terminator = get_terminator(q, p3, p3_ctx);

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	FILE *fp = fopen(filename, "r");
	free(filename);

	if (!fp)
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_file");

	// Check for a BOM

	int ch = getc_utf8(fp);

	if ((unsigned)ch != 0xFEFF)
		fseek(fp, 0, SEEK_SET);

	char *line = NULL;
	size_t len = 0;
	int nbr = 1;
	check_heap_error(init_tmp_heap(q));

	while (getline(&line, &len, fp) != -1) {
		int len = strlen(line);

		if (!terminator) {
			if (len && (line[len-1] == '\n')) {
				line[len-1] = '\0';
				len--;
			}

			if (len && (line[len-1] == '\r')) {
				line[len-1] = '\0';
				len--;
			}
		}

		cell tmp;
		make_stringn(&tmp, line, len);
		append_list(q, &tmp);
	}

	free(line);
	fclose(fp);
	cell *l = end_list(q);
	check_heap_error(l);
	unify(q, p2, p2_ctx, l, q->st.curr_frame);
	return true;
}

static bool bif_getlines_1(query *q)
{
	GET_NEXT_ARG(p1,var);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;
	int nbr = 1;
	check_heap_error(init_tmp_heap(q));

	while (getline(&line, &len, str->fp) != -1) {
		int len = strlen(line);

		if (len && (line[len-1] == '\n')) {
			line[len-1] = '\0';
			len--;
		}

		if (len && (line[len-1] == '\r')) {
			line[len-1] = '\0';
			len--;
		}

		cell tmp;
		make_stringn(&tmp, line, len);
		append_list(q, &tmp);
	}

	free(line);
	cell *l = end_list(q);
	check_heap_error(l);
	unify(q, p1, p1_ctx, l, q->st.curr_frame);
	return true;
}

static bool bif_getlines_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,var);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;
	int nbr = 1;
	check_heap_error(init_tmp_heap(q));

	while (getline(&line, &len, str->fp) != -1) {
		int len = strlen(line);

		if (len && (line[len-1] == '\n')) {
			line[len-1] = '\0';
			len--;
		}

		if (len && (line[len-1] == '\r')) {
			line[len-1] = '\0';
			len--;
		}

		cell tmp;
		make_stringn(&tmp, line, len);
		append_list(q, &tmp);
	}

	free(line);
	cell *l = end_list(q);
	check_heap_error(l);
	unify(q, p1, p1_ctx, l, q->st.curr_frame);
	return true;
}

static bool bif_getlines_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,var);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;
	int nbr = 1;
	bool terminator = get_terminator(q, p2, p2_ctx);
	check_heap_error(init_tmp_heap(q));

	while (getline(&line, &len, str->fp) != -1) {
		int len = strlen(line);

		if (!terminator) {
			if (len && (line[len-1] == '\n')) {
				line[len-1] = '\0';
				len--;
			}

			if (len && (line[len-1] == '\r')) {
				line[len-1] = '\0';
				len--;
			}
		}

		cell tmp;
		make_stringn(&tmp, line, len);
		append_list(q, &tmp);
	}

	free(line);
	cell *l = end_list(q);
	check_heap_error(l);
	unify(q, p1, p1_ctx, l, q->st.curr_frame);
	return true;
}

static char *fixup(const char *srcptr)
{
	char *tmpbuf = strdup(srcptr);
	const char *src = srcptr;
	char *dst = tmpbuf;

	while (*src) {
		if ((src[0] == '.') && (src[1] == '.') && ((src[2] == '/') || (src[2] == '\\'))) {
			dst -= 2;

			while ((dst != tmpbuf) && ((*dst != '/') && (*dst != '\\')
#ifdef _WIN32
				&& (*dst != ':')
#endif
				))
				dst--;

			src += 2;
			dst++;
		} else if ((src[0] == '.') && ((src[1] == '/') || (src[1] == '\\')
#ifdef _WIN32
				|| (src[1] == ':')
#endif
			))
			src += 1;
		else
			*dst++ = *src;

		src++;
	}

	*dst = '\0';
	return tmpbuf;
}

static bool bif_is_absolute_file_name_1(query *q)
{
	GET_FIRST_ARG(p1,atom);
	const char *filename = C_STR(q, p1);
	return *filename == '/';
}

static bool bif_absolute_file_name_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom_or_var);
	GET_NEXT_ARG(p_opts,list_or_nil);
	bool expand = false;
	char *filename = NULL;
	char cwdbuf[1024*4];
	char *here = strdup(getcwd(cwdbuf, sizeof(cwdbuf)));
	check_heap_error(here);
	char *cwd = here;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	LIST_HANDLER(p_opts);

	while (is_list(p_opts)) {
		cell *h = LIST_HEAD(p_opts);
		h = deref(q, h, p_opts_ctx);

		if (is_compound(h) && (h->arity == 1)) {
			if (!CMP_STRING_TO_CSTR(q, h, "expand")) {
				if (is_interned(h+1)) {
					if (!CMP_STRING_TO_CSTR(q, h+1, "true"))
						expand = true;
				}
			} else if (!CMP_STRING_TO_CSTR(q, h, "relative_to")) {
				if (is_atom(h+1))
					cwd = DUP_STRING(q, h+1);
			}
		}

		p_opts = LIST_TAIL(p_opts);
		p_opts = deref(q, p_opts, p_opts_ctx);
		p_opts_ctx = q->latest_ctx;
	}

	char *tmpbuf = NULL;
	const char *s = filename;

	if (expand && (*s == '$')) {
		char envbuf[PATH_MAX];
		char *dst = envbuf;
		s++;

		while (*s && (*s != '/') && (*s != '\\') && ((dst-envbuf-1) != sizeof(envbuf)))
			*dst++ = *s++;

		if ((*s == '\\') || (*s == '/'))
			s++;

		*dst = '\0';
		char *ptr = getenv(envbuf);

		if (!ptr) {
			free(filename);
			return throw_error(q, p1, p1_ctx, "existence_error", "environment_variable");
		}

		size_t buflen = strlen(ptr)+1+strlen(s)+1;
		tmpbuf = malloc(buflen);
		check_heap_error(tmpbuf);
		snprintf(tmpbuf, buflen, "%s/%s", ptr, s);
		convert_path(tmpbuf);
		char *tmpbuf2;

		if ((tmpbuf2 = realpath(tmpbuf, NULL)) == NULL) {
		} else {
			free(tmpbuf);
			tmpbuf = tmpbuf2;
		}
	} else {
		if ((tmpbuf = realpath(s, NULL)) == NULL) {
			if ((tmpbuf = realpath(cwd, NULL)) == NULL)
				tmpbuf = realpath(".", NULL);

			check_heap_error(tmpbuf);

			if ((*s != '/') && (*s != '\\')
#ifdef _WIN32
				&& (s[1] != ':')
#endif
				) {
				size_t buflen = strlen(tmpbuf)+1+strlen(s)+1;
				char *tmp = malloc(buflen);
				check_heap_error(tmp, free(tmpbuf));
				snprintf(tmp, buflen, "%s/%s", tmpbuf, s);
				convert_path(tmp);
				free(tmpbuf);
				tmpbuf = fixup(tmp);
				check_heap_error(tmpbuf);
				free(tmp);
			} else {
				free(tmpbuf);
				tmpbuf = fixup(s);
				check_heap_error(tmpbuf);
			}
		}
	}

	free(filename);

	if (cwd != here)
		free(cwd);

	free(here);
	cell tmp;

	if (is_string(p1))
		make_string(&tmp, tmpbuf);
	else
		make_cstring(&tmp, tmpbuf);

	free(tmpbuf);
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_getline_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_input;
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);
		return false;
	}

	len = strlen(line);

	if (len && (line[len-1] == '\n')) {
		line[len-1] = '\0';
		len--;
	}

	if (len && (line[len-1] == '\r')) {
		line[len-1] = '\0';
		len--;
	}

	cell tmp;
	make_string(&tmp, line);
	free(line);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_getline_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);

		if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
			clearerr(str->fp);
			return do_yield(q, 1);
		}

		return false;
	}

	len = strlen(line);

	if (line[strlen(line)-1] == '\n')
		line[strlen(line)-1] = '\0';

	if (line[strlen(line)-1] == '\r')
		line[strlen(line)-1] = '\0';

	cell tmp;
	make_string(&tmp, line);
	free(line);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_getline_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,any);
	GET_NEXT_ARG(p2,list_or_nil);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	char *line = NULL;
	size_t len = 0;
	bool terminator = get_terminator(q, p2, p2_ctx);

	if (isatty(fileno(str->fp))) {
		fprintf(str->fp, "%s", PROMPT);
		fflush(str->fp);
	}

	if (net_getline(&line, &len, str) == -1) {
		free(line);

		if (q->is_task && !feof(str->fp) && ferror(str->fp)) {
			clearerr(str->fp);
			return do_yield(q, 1);
		}

		return false;
	}

	len = strlen(line);

	if (!terminator) {
		if (line[strlen(line)-1] == '\n')
			line[strlen(line)-1] = '\0';

		if (line[strlen(line)-1] == '\r')
			line[strlen(line)-1] = '\0';
	}

	cell tmp;
	make_string(&tmp, line);
	free(line);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_access_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	int amode = R_OK;

	if (!CMP_STRING_TO_CSTR(q, p2, "read"))
		amode = R_OK;
	else if (!CMP_STRING_TO_CSTR(q, p2, "write"))
		amode = W_OK;
	else if (!CMP_STRING_TO_CSTR(q, p2, "append"))
		amode = W_OK;
	else if (!CMP_STRING_TO_CSTR(q, p2, "execute"))
		amode = X_OK;
	else if (!CMP_STRING_TO_CSTR(q, p2, "none")) {
		free(filename);
		return true;
	} else {
		free(filename);
		return throw_error(q, p2, p2_ctx, "domain_error", "mode");
	}

	convert_path(filename);
	struct stat st = {0};
	int status = stat(filename, &st);

	if (status && (!CMP_STRING_TO_CSTR(q, p2, "read") || !CMP_STRING_TO_CSTR(q, p2, "exist") || !CMP_STRING_TO_CSTR(q, p2, "execute") || !CMP_STRING_TO_CSTR(q, p2, "none"))) {
		free(filename);
		return false;
	}

	if (status && (!CMP_STRING_TO_CSTR(q, p2, "write") || !CMP_STRING_TO_CSTR(q, p2, "append"))) {
		free(filename);
		return true;
	}

	int ok = !access(filename, amode);
	free(filename);
	return ok;
}

static bool bif_exists_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return false;
	}

	free(filename);

	if ((st.st_mode & S_IFMT) != S_IFREG)
		return false;

	return true;
}

static bool bif_directory_files_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,var);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "directory");
	}

	convert_path(filename);
	DIR *dirp = opendir(filename);

	if (!dirp) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "directory");
	}

	struct dirent *dire = readdir(dirp);
	cell tmp;

	if (is_string(p1))
		make_string(&tmp, dire->d_name);
	else
		make_cstring(&tmp, dire->d_name);

	allocate_list(q, &tmp);

	for (dire = readdir(dirp); dire; dire = readdir(dirp)) {
		if (is_string(p1))
			make_string(&tmp, dire->d_name);
		else
			make_cstring(&tmp, dire->d_name);

		append_list(q, &tmp);
	}

	closedir(dirp);
	free(filename);
	cell *l = end_list(q);
	bool ok = unify(q, p2, p2_ctx, l, q->st.curr_frame);
	return ok;
}

static bool bif_delete_file_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	remove(filename);
	free(filename);
	return true;
}

static bool bif_rename_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom_or_list);
	char *filename1, *filename2;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename1 = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename1 = DUP_STRING(q, p1);

	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);

		if (!len) {
			free(filename1);
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		}

		filename2 = chars_list_to_string(q, p2, p2_ctx, len);
	} else
		filename2 = DUP_STRING(q, p2);

	convert_path(filename1);
	convert_path(filename2);
	struct stat st = {0};

	if (stat(filename1, &st)) {
		free(filename1);
		free(filename2);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	bool ok = !rename(filename1, filename2);
	free(filename1);
	free(filename2);
	return ok ? true : false;
}

static bool bif_copy_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,atom_or_list);
	char *filename1, *filename2;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename1 = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename1 = DUP_STRING(q, p1);

	if (is_iso_list(p2)) {
		size_t len = scan_is_chars_list(q, p2, p2_ctx, true);

		if (!len) {
			free(filename1);
			return throw_error(q, p2, p2_ctx, "type_error", "atom");
		}

		filename2 = chars_list_to_string(q, p2, p2_ctx, len);
	} else
		filename2 = DUP_STRING(q, p2);

	convert_path(filename1);
	convert_path(filename2);
	FILE *fp1 = fopen(filename1, "rb");

	if (!fp1) {
		free(filename1);
		free(filename2);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	free(filename1);
	FILE *fp2 = fopen(filename2, "wb");

	if (!fp2) {
		fclose(fp1);
		free(filename2);
		return throw_error(q, p2, p2_ctx, "permission_error", "file");
	}

	free(filename2);
	char buffer[1024];
	size_t n;

	while ((n = fread(buffer, 1, sizeof(buffer), fp1)) > 0) {
		if (fwrite(buffer, 1, n, fp2) != n) {
			fclose(fp2);
			fclose(fp1);
			return throw_error(q, p2, p2_ctx, "system_error", "file");
		}
	}

	fclose(fp2);

	if (!feof(fp1)) {
		fclose(fp1);
		return throw_error(q, p1, p1_ctx, "system_error", "file");
	}

	fclose(fp1);
	return true;
}

static bool bif_time_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,var);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	free(filename);
	cell tmp;
	make_float(&tmp, st.st_mtime);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool bif_size_file_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	GET_NEXT_ARG(p2,integer_or_var);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	free(filename);
	cell tmp;
	make_int(&tmp, st.st_size);
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool bif_exists_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (stat(filename, &st)) {
		free(filename);
		return false;
	}

	free(filename);

	if ((st.st_mode & S_IFMT) != S_IFDIR)
		return false;

	return true;
}

static bool bif_make_directory_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	struct stat st = {0};

	if (!stat(filename, &st)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "existence_error", "file");
	}

	if (mkdir(filename, 0777)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "permission_error", "file");
	}

	free(filename);
	return true;
}

static bool bif_make_directory_path_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);

		if (!len)
			return throw_error(q, p1, p1_ctx, "type_error", "atom");

		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	struct stat st = {0};

	for (char *ptr = filename+1; *ptr; ptr++) {
		if (*ptr == PATH_SEP_CHAR) {
			*ptr = '\0';

			if (stat(filename, &st)) {
				if (mkdir(filename, 0777)) {
					free(filename);
					return throw_error(q, p1, p1_ctx, "permission_error", "directory");
				}
			}

			*ptr = PATH_SEP_CHAR;
		}
	}

	if (!stat(filename, &st)) {
		free(filename);
		return true;
	}

	if (mkdir(filename, 0777)) {
		free(filename);
		return throw_error(q, p1, p1_ctx, "permission_error", "directory");
	}

	free(filename);
	return true;
}

static bool bif_working_directory_2(query *q)
{
	GET_FIRST_ARG(p_old,var);
	GET_NEXT_ARG(p_new,atom_or_list_or_var);
	char tmpbuf[PATH_MAX], tmpbuf2[PATH_MAX];
	char *oldpath = getcwd(tmpbuf, sizeof(tmpbuf));
	snprintf(tmpbuf2, sizeof(tmpbuf2), "%s/", oldpath);
	convert_path(tmpbuf2);
	oldpath = tmpbuf2;
	cell tmp;
	make_string(&tmp, oldpath);

	if (is_atom_or_list(p_new)) {
		char *filename;

		if (is_iso_list(p_new)) {
			size_t len = scan_is_chars_list(q, p_new, p_new_ctx, true);

			if (!len) {
				unshare_cell(&tmp);
				return throw_error(q, p_new, p_new_ctx, "type_error", "atom");
			}

			filename = chars_list_to_string(q, p_new, p_new_ctx, len);
		} else
			filename = DUP_STRING(q, p_new);

		if (chdir(filename)) {
			unshare_cell(&tmp);
			return throw_error(q, p_new, p_new_ctx, "existence_error", "path");
		}

		free(filename);
	}

	bool ok = unify(q, p_old, p_old_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

static bool bif_chdir_1(query *q)
{
	GET_FIRST_ARG(p1,atom_or_list);
	char *filename;

	if (is_iso_list(p1)) {
		size_t len = scan_is_chars_list(q, p1, p1_ctx, true);
		filename = chars_list_to_string(q, p1, p1_ctx, len);
	} else
		filename = DUP_STRING(q, p1);

	convert_path(filename);
	bool ok = !chdir(filename);
	free(filename);
	return ok;
}

static void parse_host(const char *src, char hostname[1024], char path[4096], unsigned *port, int *ssl, int *domain)
{
	if (!strncmp(src, "https://", 8)) {
		src += 8;
		*ssl = 1;
		*port = 443;
	} else if (!strncmp(src, "http://", 7)) {
		src += 7;
		*ssl = 0;
		*port = 80;
	} else if (!strncmp(src, "unix://", 7)) {
		src += 7;
		*domain = 1;
	}

	if (*src == ':')
		sscanf(src, ":%u/%4095s", port, path);
	else
		sscanf(src, "%1023[^:/]:%u/%4095s", hostname, port, path);

	hostname[1023] = '\0';
	path[4095] = '\0';
}

static bool bif_server_3(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,list_or_nil);
	char hostname[1024], path[4096];
	char *keyfile = "privkey.pem", *certfile = "fullchain.pem";
	int udp = 0, nodelay = 1, nonblock = 0, ssl = 0, domain = 0, level = 0;
	unsigned port = 80;
	snprintf(hostname, sizeof(hostname), "localhost");
	path[0] = '\0';
	LIST_HANDLER(p3);

	while (is_list(p3)) {
		cell *h = LIST_HEAD(p3);
		cell *c = deref(q, h, p3_ctx);

		if (is_compound(c) && (c->arity == 1)) {
			if (!CMP_STRING_TO_CSTR(q, c, "udp")) {
				c = c + 1;

				if (is_atom(c))
					udp = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "nodelay")) {
				c = c + 1;

				if (is_atom(c))
					nodelay = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "ssl")) {
				c = c + 1;

				if (is_atom(c))
					ssl = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "keyfile")) {
				c = c + 1;

				if (is_atom(c))
					keyfile = C_STR(q, c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "certfile")) {
				c = c + 1;

				if (is_atom(c))
					certfile = C_STR(q, c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "hostname")) {
				c = c + 1;

				if (is_atom(c))
					slicecpy(hostname, sizeof(hostname), C_STR(q, c), C_STRLEN(q, c));
			} else if (!CMP_STRING_TO_CSTR(q, c, "scheme")) {
				c = c + 1;

				if (is_atom(c)) {
					ssl = !CMP_STRING_TO_CSTR(q, c, "https") ? 1 : 0;
					port = 443;
				}
			} else if (!CMP_STRING_TO_CSTR(q, c, "port")) {
				c = c + 1;

				if (is_integer(c))
					port = get_smallint(c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "level")) {
				c = c + 1;

				if (is_integer(c))
					level = (int)get_smallint(c);
			}
		}

		p3 = LIST_TAIL(p3);
		p3 = deref(q, p3, p3_ctx);
		p3_ctx = q->latest_ctx;
	}

	const char *url = C_STR(q, p1);
	parse_host(url, hostname, path, &port, &ssl, &domain);
	nonblock = q->is_task;

	int fd = net_server(hostname, port, udp, ssl?keyfile:NULL, ssl?certfile:NULL);

	if (fd == -1)
		return throw_error(q, p1, p1_ctx, "existence_error", "server_failed");

	int n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	sl_set(str->alias, strdup(hostname), NULL);
	check_heap_error(str->filename = DUP_STRING(q, p1));
	check_heap_error(str->mode = strdup("update"));
	str->nodelay = nodelay;
	str->nonblock = nonblock;
	str->udp = udp;
	str->fp = fdopen(fd, "r+");
	str->ssl = ssl;
	str->level = level;
	str->sslptr = NULL;

	if (str->fp == NULL) {
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
		close(fd);
	}

	if (!str->ssl)
		net_set_nonblocking(str);

	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	return unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
}

static bool bif_accept_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,var);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];

	int fd = net_accept(str);

	if (fd == -1) {
		if (q->is_task)
			return do_yield(q, 1);

		return false;
	}

	n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str2 = &q->pl->streams[n];
	sl_set(str2->alias, strdup(str->filename), NULL);
	check_heap_error(str2->filename = strdup(str->filename));
	check_heap_error(str2->mode = strdup("update"));
	str2->socket = true;
	str2->nodelay = str->nodelay;
	str2->nonblock = str->nonblock;
	str2->udp = str->udp;
	str2->ssl = str->ssl;
	str2->fp = fdopen(fd, "r+");

	if (str2->fp == NULL) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}

	if (str->ssl) {
		str2->sslptr = net_enable_ssl(fd, str->filename, 1, str->level, NULL);

		if (!str2->sslptr) {
			close(fd);
			return false;
		}
	}

	if (!str->ssl)
		net_set_nonblocking(str2);

	check_heap_error(push_choice(q));
	cell tmp;
	make_int(&tmp, n);
	tmp.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool do_parse_parts(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, bool full)
{
	char protocol[256], host[1024], path[8192], search[8192], fragment[8192];
	protocol[0] = host[0] = path[0] = search[0] = fragment[0] = '\0';
	int port = 0;
	LIST_HANDLER(p2);

	while (is_iso_list(p2)) {
		cell *h = LIST_HEAD(p2);
		h = deref(q, h, p2_ctx);
		pl_idx h_ctx = q->latest_ctx;

		if (!strcmp(C_STR(q, h), "protocol")) {
			if (!is_atom(h+1))
				return throw_error(q, h+1, p2_ctx, "type_error", "atom");

			sprintf(protocol, "%s", C_STR(q, h+1));
		} else if (!strcmp(C_STR(q, h), "host")) {
			if (!is_atom(h+1))
				return throw_error(q, h+1, p2_ctx, "type_error", "atom");

			sprintf(host, "%s", C_STR(q, h+1));
		} else if (!strcmp(C_STR(q, h), "port")) {
			if (!is_smallint(h+1))
				return throw_error(q, h+1, p2_ctx, "type_error", "integer");

			port = get_smallint(h+1);
		} else if (!strcmp(C_STR(q, h), "path")) {
			if (!is_atom(h+1))
				return throw_error(q, h+1, p2_ctx, "type_error", "atom");

			sprintf(path, "%s", C_STR(q, h+1));
		} else if (!strcmp(C_STR(q, h), "search")) {
			cell *h1 = h + 1;
			h1 = deref(q, h1, h_ctx);
			pl_idx h1_ctx = q->latest_ctx;

			if (!is_iso_list(h1))
				return throw_error(q, h1, h_ctx, "type_error", "list");

			char *dst = search;
			LIST_HANDLER(h1);

			while (is_iso_list(h1)) {
				cell *c = LIST_HEAD(h1);
				c = deref(q, c, h1_ctx);

				if (!is_compound(c) || (c->val_off != g_eq_s) || (c->arity != 2))
					return throw_error(q, c, h1_ctx, "type_error", "compound");

				if (!is_atom(c+1))
					return throw_error(q, c+1, h1_ctx, "type_error", "atom");

				if (!is_atomic(c+2))
					return throw_error(q, c+2, h1_ctx, "type_error", "atom");

				size_t len1 = C_STRLEN(q, c+1);
				char *dstbuf1 = malloc(len1+1);
				check_heap_error(dstbuf1);
				url_encode(C_STR(q, c+1), len1, dstbuf1);
				dst += sprintf(dst, "%s", dstbuf1);
				free(dstbuf1);

				if (is_atom(c+2)) {
					size_t len2 = C_STRLEN(q, c+2);
					char *dstbuf2 = malloc(len2+1);
					check_heap_error(dstbuf2);
					url_encode(C_STR(q, c+2), len2, dstbuf2);
					dst += sprintf(dst, "=%s", dstbuf2);
					free(dstbuf2);
				} else if (is_smallint(c+2)) {
					char tmpbuf[256];
					snprintf(tmpbuf, sizeof(tmpbuf), "%lld", (long long)get_smallint(c+2));
					size_t len2 = strlen(tmpbuf);
					char *dstbuf2 = malloc(len2+1);
					check_heap_error(dstbuf2);
					url_encode(tmpbuf, len2, dstbuf2);
					dst += sprintf(dst, "=%s", dstbuf2);
					free(dstbuf2);
				} else {
					char tmpbuf[256];
					snprintf(tmpbuf, sizeof(tmpbuf), "%.17g", get_float(c+2));
					size_t len2 = strlen(tmpbuf);
					char *dstbuf2 = malloc(len2+1);
					check_heap_error(dstbuf2);
					url_encode(tmpbuf, len2, dstbuf2);
					dst += sprintf(dst, "=%s", dstbuf2);
					free(dstbuf2);
				}

				h1 = LIST_TAIL(h1);
				h1 = deref(q, h1, h1_ctx);
				h1_ctx = q->latest_ctx;

				if (!is_nil(h1))
					dst += sprintf(dst, "&");
			}
		} else if (!strcmp(C_STR(q, h), "fragment")) {
			if (!is_atom(h+1))
				return throw_error(q, h+1, p2_ctx, "type_error", "atom");

			sprintf(fragment, "%s", C_STR(q, h+1));
		}

		p2 = LIST_TAIL(p2);
		p2 = deref(q, p2, p2_ctx);
		p2_ctx = q->latest_ctx;
	}

	SB(pr);
	SB_sprintf(pr, "%s://%s", protocol, host);
	if (port) SB_sprintf(pr, ":%d", port);
	if (path[0]) SB_sprintf(pr, "%s", path);
	if (search[0]) SB_sprintf(pr, "?%s", search);
	if (fragment[0]) SB_sprintf(pr, "#%s", fragment);
	cell tmp;
	make_cstring(&tmp,  SB_cstr(pr));
	return unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);
}

static bool do_parse_url(query *q, cell *p1, pl_idx p1_ctx, cell *p2, pl_idx p2_ctx, bool full)
{
	const char *src = C_STR(q, p1);
	char protocol[256], host[1024], path[8192], search[8192], fragment[8192];
	protocol[0] = host[0] = path[0] = search[0] = fragment[0] = '\0';

	if (full)
		sscanf(src, "%255[^:\r\n]://%1023[^/\r\n]%8191[^?\r\n]?%8191[^#\r\n]#%8191[^\r\n]", protocol, host, path, search, fragment);
	else
		sscanf(src, "%8191[^?\r\n]?%8191[^#\r\n]#%8191[^\r\n]", path, search, fragment);

	protocol[255] = host[1023] = path[8191] = search[8191] = fragment[8191] = '\0';
	char *dstbuf;
	size_t len;
	cell tmp[3];

	if (search[0]) {
		allocate_list(q, tmp);
		char key[8192], search2[8192];
		key[0] = search2[0] = '\0';
		char *src2 = search, *dst2 = key;
		bool first = true;

		while (*src2) {
			if (*src2 == '=') {
				src2++;
				dst2 = search2;
				*dst2 = '\0';
			} else if (*src2 == '&') {
				make_struct(tmp, new_atom(q->pl, "="), NULL, 2, 2);
				SET_OP(tmp, OP_YFX);

				len = strlen(key);
				dstbuf = malloc(len+1);
				check_heap_error(dstbuf);
				url_decode(key, dstbuf);
				make_cstring(tmp+1, dstbuf);
				free(dstbuf);

				len = strlen(search2);
				dstbuf = malloc(len+1);
				check_heap_error(dstbuf);
				url_decode(search2, dstbuf);
				make_cstring(tmp+2, dstbuf);
				free(dstbuf);

				if (first) {
					allocate_list(q, tmp);
					first = false;
				} else
					append_list(q, tmp);

				src2++;
				dst2 = key;
				*dst2 = '\0';
			}

			*dst2++ = *src2++;
			*dst2 = '\0';
		}

		make_struct(tmp, new_atom(q->pl, "="), NULL, 2, 2);
		SET_OP(tmp, OP_YFX);

		len = strlen(key);
		dstbuf = malloc(len+1);
		check_heap_error(dstbuf);
		url_decode(key, dstbuf);
		make_cstring(tmp+1, dstbuf);
		free(dstbuf);

		len = strlen(search2);
		dstbuf = malloc(len+1);
		check_heap_error(dstbuf);
		url_decode(search2, dstbuf);
		make_cstring(tmp+2, dstbuf);
		free(dstbuf);
		append_list(q, tmp);

		cell *l = end_list(q);
		cell *tmp2 = alloc_on_heap(q, 1 + l->nbr_cells);
		make_struct(tmp2, new_atom(q->pl, "search"), NULL, 1, l->nbr_cells);
		dup_cells(tmp2+1, l, l->nbr_cells);
		allocate_list(q, tmp2);
	}

	if (protocol[0]) {
		make_struct(tmp, new_atom(q->pl, "protocol"), NULL, 1, 1);
		make_cstring(tmp+1, protocol);

		if (search[0])
			append_list(q, tmp);
		else
			allocate_list(q, tmp);
	}

	if (host[0]) {
		char host2[256]; host2[0] = '\0';
		int port = 0;
		sscanf(host, "%255[^:]:%d", host2, &port);
		host2[255] = '\0';

		make_struct(tmp, new_atom(q->pl, "host"), NULL, 1, 1);
		make_cstring(tmp+1, host2);
		append_list(q, tmp);

		if (port) {
			make_struct(tmp, new_atom(q->pl, "port"), NULL, 1, 1);
			make_int(tmp+1, port);
			append_list(q, tmp);
		}
	}

	if (!path[0])
		strcpy(path, "/");

	if (path[0]) {
		len = strlen(path);
		dstbuf = malloc(len+1);
		check_heap_error(dstbuf);
		url_decode(path, dstbuf);
		src = dstbuf;
		make_struct(tmp, new_atom(q->pl, "path"), NULL, 1, 1);
		make_cstring(tmp+1, path);
		append_list(q, tmp);
		free(dstbuf);
	}

	if (fragment[0]) {
		len = strlen(fragment);
		dstbuf = malloc(len+1);
		check_heap_error(dstbuf);
		url_decode(path, dstbuf);
		src = dstbuf;
		make_struct(tmp, new_atom(q->pl, "fragment"), NULL, 1, 1);
		make_cstring(tmp+1, fragment);
		append_list(q, tmp);
		free(dstbuf);
	}

	return unify(q, p2, p2_ctx, end_list(q), q->st.curr_frame);
}

static bool bif_parse_url_2(query *q)
{
	GET_FIRST_ARG(p1,atom_or_var);
	GET_NEXT_ARG(p2,iso_list_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error2(q, p1, p1_ctx, "uninstantiation_error", "not_sufficiently_instantiated", p2);

	if (is_var(p2))
		return do_parse_url(q, p1, p1_ctx, p2, p2_ctx, true);
	else
		return do_parse_parts(q, p1, p1_ctx, p2, p2_ctx, true);
}

static bool bif_http_location_2(query *q)
{
	GET_FIRST_ARG(p1,iso_list_or_var);
	GET_NEXT_ARG(p2,atom_or_var);

	if (is_var(p1) && is_var(p2))
		return throw_error2(q, p1, p1_ctx, "uninstantiation_error", "not_sufficiently_instantiated", p2);

	if (is_var(p1))
		return do_parse_url(q, p2, p2_ctx, p1, p1_ctx, false);
	else
		return do_parse_parts(q, p2, p2_ctx, p1, p1_ctx, false);
}

static bool bif_client_5(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	GET_NEXT_ARG(p3,var);
	GET_NEXT_ARG(p4,var);
	GET_NEXT_ARG(p5,list_or_nil);
	char hostname[1024], path[1024*4];
	char *certfile = NULL;
	int udp = 0, nodelay = 1, nonblock = 0, ssl = 0, domain = 0, level = 0;
	hostname[0] = path[0] = '\0';
	unsigned port = 80;
	LIST_HANDLER(p5);

	while (is_list(p5)) {
		cell *h = LIST_HEAD(p5);
		cell *c = deref(q, h, p5_ctx);

		if (is_compound(c) && (c->arity == 1)) {
			if (!CMP_STRING_TO_CSTR(q, c, "udp")) {
				c = c + 1;

				if (is_atom(c))
					udp = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "nodelay")) {
				c = c + 1;

				if (is_atom(c))
					nodelay = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "ssl")) {
				c = c + 1;

				if (is_atom(c))
					ssl = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			} else if (!CMP_STRING_TO_CSTR(q, c, "certfile")) {
				c = c + 1;

				if (is_atom(c))
					certfile = C_STR(q, c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "scheme")) {
				c = c + 1;

				if (is_atom(c)) {
					ssl = !CMP_STRING_TO_CSTR(q, c, "https") ? 1 : 0;
					port = 443;
				}
			} else if (!CMP_STRING_TO_CSTR(q, c, "port")) {
				c = c + 1;

				if (is_integer(c))
					port = (int)get_smallint(c);
			} else if (!CMP_STRING_TO_CSTR(q, c, "level")) {
				c = c + 1;

				if (is_integer(c))
					level = (int)get_smallint(c);
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	const char *url = C_STR(q, p1);
	parse_host(url, hostname, path, &port, &ssl, &domain);
	nonblock = q->is_task;

	while (is_list(p5)) {
		cell *h = LIST_HEAD(p5);
		cell *c = deref(q, h, p5_ctx);

		if (is_compound(c) && (c->arity == 1)) {
			if (!CMP_STRING_TO_CSTR(q, c, "host")) {
				c = c + 1;

				//if (is_atom(c))
				//	;//udp = !CMP_STRING_TO_CSTR(q, c, "true") ? 1 : 0;
			}
		}

		p5 = LIST_TAIL(p5);
		p5 = deref(q, p5, p5_ctx);
		p5_ctx = q->latest_ctx;
	}

	int fd = net_connect(hostname, port, udp, nodelay);

	if (fd == -1)
		return throw_error(q, p1, p1_ctx, "resource_error", "could_not_connect");

	int n = new_stream(q->pl);

	if (n < 0) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "resource_error", "too_many_streams");
	}

	stream *str = &q->pl->streams[n];
	if (!str->alias) str->alias = sl_create((void*)fake_strcmp, (void*)keyfree, NULL);
	sl_set(str->alias, DUP_STRING(q, p1), NULL);
	check_heap_error(str->filename = DUP_STRING(q, p1));
	check_heap_error(str->mode = strdup("update"));
	str->socket = true;
	str->nodelay = nodelay;
	str->nonblock = nonblock;
	str->udp = udp;
	str->ssl = ssl;
	str->level = level;
	str->fp = fdopen(fd, "r+");

	if (!str->filename || !str->mode) {
		sl_destroy(str->alias);
		free(str->filename);
		free(str->mode); //cehteh: maybe from pool?
		return false;
	}

	if (str->fp == NULL) {
		close(fd);
		return throw_error(q, p1, p1_ctx, "existence_error", "cannot_open_stream");
	}

	if (str->ssl) {
		str->sslptr = net_enable_ssl(fd, hostname, 0, str->level, certfile);
		check_heap_error (str->sslptr, close(fd));
	}

	if (nonblock && !str->ssl)
		net_set_nonblocking(str);

	cell tmp;
	make_string(&tmp, hostname);
	unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	make_string(&tmp, path);
	unify(q, p3, p3_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	cell tmp2;
	make_int(&tmp2, n);
	tmp2.flags |= FLAG_INT_STREAM | FLAG_INT_HEX;
	return unify(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
}

static bool bif_bread_3(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,integer_or_var);
	GET_NEXT_ARG(p2,var);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	size_t len;

	if (is_bigint(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "small_integer_range");

	if (is_integer(p1) && is_positive(p1)) {
		if (!str->data) {
			str->data = malloc(get_smallint(p1)+1);
			check_heap_error(str->data);
			str->data_len = 0;
		}

		for (;;) {
			len = get_smallint(p1) - str->data_len;
			size_t nbytes = net_read(str->data+str->data_len, len, str);
			str->data_len += nbytes;
			str->data[str->data_len] = '\0';

			if (nbytes == len)
				break;

			if (feof(str->fp)) {
				free(str->data);
				str->data = NULL;
				return false;
			}

			if (q->is_task) {
				clearerr(str->fp);
				return do_yield(q, 1);
			}
		}

		cell tmp;
		make_stringn(&tmp, str->data, str->data_len);
		bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		free(str->data);
		str->data = NULL;
		return ok;
	}

	if (is_integer(p1)) {
		if (!str->data) {
			str->data = malloc((str->alloc_nbytes=1024)+1);
			check_heap_error(str->data);
			str->data_len = 0;
		}

		size_t nbytes = net_read(str->data, str->alloc_nbytes, str);
		str->data[nbytes] = '\0';
		str->data = realloc(str->data, nbytes+1);
		check_heap_error(str->data);
		cell tmp;
		make_stringn(&tmp, str->data, nbytes);
		bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
		unshare_cell(&tmp);
		free(str->data);
		str->data = NULL;
		return ok;
	}

	if (!str->data) {
		str->data = malloc((str->alloc_nbytes=1024)+1);
		check_heap_error(str->data);
		str->data_len = 0;
	}

	for (;;) {
		size_t len = str->alloc_nbytes - str->data_len;
		size_t nbytes = net_read(str->data+str->data_len, len, str);
		str->data_len += nbytes;
		str->data[str->data_len] = '\0';

		if (!nbytes || feof(str->fp))
			break;

		if (str->alloc_nbytes == str->data_len) {
			str->data = realloc(str->data, (str->alloc_nbytes*=2)+1);
			check_heap_error(str->data);
		}
	}

	cell tmp1;
	make_int(&tmp1, str->data_len);
	unify(q, p1, p1_ctx, &tmp1, q->st.curr_frame);
	cell tmp2;

	if (str->data_len)
		make_stringn(&tmp2, str->data, str->data_len);
	else
		make_atom(&tmp2, g_nil_s);

	bool ok = unify(q, p2, p2_ctx, &tmp2, q->st.curr_frame);
	unshare_cell(&tmp2);
	free(str->data);
	str->data = NULL;
	return ok;
}

static bool bif_bwrite_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	GET_NEXT_ARG(p1,atom);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	const char *src = C_STR(q, p1);
	size_t len = C_STRLEN(q, p1);

	while (len) {
		size_t nbytes = net_write(src, len, str);

		if (!nbytes) {
			if (feof(str->fp) || ferror(str->fp))
				return false; // can feof() happen on writing?
		}

		// TODO: make this yieldable

		clearerr(str->fp);
		len -= nbytes;
		src += nbytes;
	}

	return true;
}

static bool bif_sys_put_chars_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	size_t len;

	if (is_cstring(p1)) {
		const char *src = C_STR(q, p1);
		size_t len = C_STRLEN(q, p1);
		net_write(src, len, str);
	} else if ((len = scan_is_chars_list(q, p1, p1_ctx, true)) > 0) {
		char *src = chars_list_to_string(q, p1, p1_ctx, len);
		net_write(src, len, str);
		free(src);
	} else if (is_nil(p1)) {
		;
	} else
		return throw_error(q, p1, p1_ctx, "type_error", "cchars");

	return !ferror(str->fp);
}

static bool bif_sys_put_chars_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,list_or_nil);
	size_t len;

	if (is_cstring(p1)) {
		const char *src = C_STR(q, p1);
		size_t len = C_STRLEN(q, p1);
		net_write(src, len, str);
	} else if ((len = scan_is_chars_list(q, p1, p1_ctx, true)) > 0) {
		char *src = chars_list_to_string(q, p1, p1_ctx, len);
		net_write(src, len, str);
		free(src);
	} else if (is_nil(p1)) {
		;
	} else
		return throw_error(q, p1, p1_ctx, "type_error", "chars");

	return !ferror(str->fp);
}

static bool bif_sys_capture_output_0(query *q)
{
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];

	if (str->is_memory) {
		str->is_memory = false;
		SB_free(str->sb);
	} else
		str->is_memory = true;

	return true;
}

static bool bif_sys_capture_output_to_chars_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	const char *src = SB_cstr(str->sb);
	size_t len = SB_strlen(str->sb);
	cell tmp;
	make_stringn(&tmp, src, len);
	str->is_memory = false;
	SB_free(str->sb);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);;
	unshare_cell(&tmp);
	return ok;
}

static bool bif_sys_capture_output_to_atom_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	const char *src = SB_cstr(str->sb);
	size_t len = SB_strlen(str->sb);
	cell tmp;
	make_cstringn(&tmp, src, len);
	str->is_memory = false;
	SB_free(str->sb);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);;
	unshare_cell(&tmp);
	return ok;
}

static bool bif_sys_capture_error_0(query *q)
{
	int n = q->pl->current_error;
	stream *str = &q->pl->streams[n];

	if (str->is_memory) {
		str->is_memory = false;
		SB_free(str->sb);
	} else
		str->is_memory = true;

	return true;
}

static bool bif_sys_capture_error_to_chars_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	int n = q->pl->current_error;
	stream *str = &q->pl->streams[n];
	const char *src = SB_cstr(str->sb);
	size_t len = SB_strlen(str->sb);
	cell tmp;
	make_stringn(&tmp, src, len);
	str->is_memory = false;
	SB_free(str->sb);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);;
	unshare_cell(&tmp);
	return ok;
}

static bool bif_sys_capture_error_to_atom_1(query *q)
{
	GET_FIRST_ARG(p1,var);
	int n = q->pl->current_error;
	stream *str = &q->pl->streams[n];
	const char *src = SB_cstr(str->sb);
	size_t len = SB_strlen(str->sb);
	cell tmp;
	make_cstringn(&tmp, src, len);
	str->is_memory = false;
	SB_free(str->sb);
	bool ok = unify(q, p1, p1_ctx, &tmp, q->st.curr_frame);;
	unshare_cell(&tmp);
	return ok;
}

static bool bif_set_stream_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	cell *name = p1 + 1;
	name = deref(q, name, p1_ctx);

	if (!is_structure(p1))
		return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

	if (!CMP_STRING_TO_CSTR(q, p1, "alias")) {
		if (is_var(name))
			return throw_error(q, name, q->latest_ctx, "instantiation_error", "stream_option");

		if (!is_atom(name))
			return throw_error(q, p1, p1_ctx, "domain_error", "stream_property");

		if (!CMP_STRING_TO_CSTR(q, name, "current_input")) {
			q->pl->current_input = n;
		} else if (!CMP_STRING_TO_CSTR(q, name, "current_output")) {
			q->pl->current_output = n;
		} else if (!CMP_STRING_TO_CSTR(q, name, "current_error")) {
			q->pl->current_error = n;
		} else {
			int n2 = get_named_stream(q->pl, C_STR(q, name), C_STRLEN(q, name));

			if (n2 >= 0) {
				stream *str2 = &q->pl->streams[n2];
				sl_del(str2->alias, C_STR(q, name));
			}

			sl_set(str->alias, DUP_STRING(q, name), NULL);
		}

		return true;
	}

	return false;
}

static bool bif_portray_clause_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	int n = q->pl->current_output;
	stream *str = &q->pl->streams[n];
	q->quoted = 1;
	q->portray_vars = true;
	q->print_idx = 0;
	print_term(q, str->fp, p1, p1_ctx, 1);
	fputc('.', str->fp);
	fputc('\n', str->fp);
	q->quoted = 0;
	q->portray_vars = false;
	clear_write_options(q);
	return true;
}

static bool bif_portray_clause_2(query *q)
{
	GET_FIRST_ARG(pstr,stream);
	int n = get_stream(q, pstr);
	stream *str = &q->pl->streams[n];
	GET_NEXT_ARG(p1,any);
	q->quoted = 1;
	q->portray_vars = true;
	q->print_idx = 0;
	print_term(q, str->fp, p1, p1_ctx, 1);
	fputc('.', str->fp);
	fputc('\n', str->fp);
	q->quoted = 0;
	q->portray_vars = false;
	return true;
}

static bool bif_is_stream_1(query *q)
{
	GET_FIRST_ARG(p1,any);
	return is_stream(p1);
}

builtins g_streams_bifs[] =
{
	// ISO...

	{"open", 4, bif_iso_open_4, "+atom,+mode,--stream,+list", true, false, BLAH},
	{"close", 1, bif_iso_close_1, "+stream", true, false, BLAH},
	{"close", 2, bif_iso_close_2, "+stream,+opts", true, false, BLAH},
	{"read_term", 2, bif_iso_read_term_2, "+stream,-term", true, false, BLAH},
	{"read_term", 3, bif_iso_read_term_3, "+stream,-term,+list", true, false, BLAH},
	{"read", 1, bif_iso_read_1, "-term", true, false, BLAH},
	{"read", 2, bif_iso_read_2, "+stream,-term", true, false, BLAH},
	{"write_canonical", 1, bif_iso_write_canonical_1, "+term", true, false, BLAH},
	{"write_canonical", 2, bif_iso_write_canonical_2, "+stream,+term", true, false, BLAH},
	{"write_term", 2, bif_iso_write_term_2, "+stream,+term", true, false, BLAH},
	{"write_term", 3, bif_iso_write_term_3, "+stream,+term,+list", true, false, BLAH},
	{"writeq", 1, bif_iso_writeq_1, "+term", true, false, BLAH},
	{"writeq", 2, bif_iso_writeq_2, "+stream,+term", true, false, BLAH},
	{"write", 1, bif_iso_write_1, "+term", true, false, BLAH},
	{"write", 2, bif_iso_write_2, "+stream,+term", true, false, BLAH},
	{"nl", 0, bif_iso_nl_0, NULL, true, false, BLAH},
	{"nl", 1, bif_iso_nl_1, "+stream", true, false, BLAH},
	{"at_end_of_stream", 0, bif_iso_at_end_of_stream_0, NULL, true, false, BLAH},
	{"at_end_of_stream", 1, bif_iso_at_end_of_stream_1, "+stream", true, false, BLAH},
	{"set_stream_position", 2, bif_iso_set_stream_position_2, "+stream,+integer", true, false, BLAH},
	{"flush_output", 0, bif_iso_flush_output_0, NULL, true, false, BLAH},
	{"flush_output", 1, bif_iso_flush_output_1, "+stream", true, false, BLAH},
	{"put_char", 1, bif_iso_put_char_1, "+integer", true, false, BLAH},
	{"put_char", 2, bif_iso_put_char_2, "+stream,+integer", true, false, BLAH},
	{"put_code", 1, bif_iso_put_code_1, "+integer", true, false, BLAH},
	{"put_code", 2, bif_iso_put_code_2, "+stream,+integer", true, false, BLAH},
	{"put_byte", 1, bif_iso_put_byte_1, "+integer", true, false, BLAH},
	{"put_byte", 2, bif_iso_put_byte_2, "+stream,+integer", true, false, BLAH},
	{"get_char", 1, bif_iso_get_char_1, "-integer", true, false, BLAH},
	{"get_char", 2, bif_iso_get_char_2, "+stream,-integer", true, false, BLAH},
	{"get_code", 1, bif_iso_get_code_1, "-integer", true, false, BLAH},
	{"get_code", 2, bif_iso_get_code_2, "+stream,-integer", true, false, BLAH},
	{"get_byte", 1, bif_iso_get_byte_1, "-integer", true, false, BLAH},
	{"get_byte", 2, bif_iso_get_byte_2, "+stream,-integer", true, false, BLAH},
	{"peek_char", 1, bif_iso_peek_char_1, "-integer", true, false, BLAH},
	{"peek_char", 2, bif_iso_peek_char_2, "+stream,-integer", true, false, BLAH},
	{"peek_code", 1, bif_iso_peek_code_1, "-integer", true, false, BLAH},
	{"peek_code", 2, bif_iso_peek_code_2, "+stream,-integer", true, false, BLAH},
	{"peek_byte", 1, bif_iso_peek_byte_1, "-integer", true, false, BLAH},
	{"peek_byte", 2, bif_iso_peek_byte_2, "+stream,-integer", true, false, BLAH},
	{"current_input", 1, bif_iso_current_input_1, "--stream", true, false, BLAH},
	{"current_output", 1, bif_iso_current_output_1, "--stream", true, false, BLAH},
	{"current_error", 1, bif_iso_current_error_1, "--stream", true, false, BLAH},
	{"set_input", 1, bif_iso_set_input_1, "+stream", true, false, BLAH},
	{"set_output", 1, bif_iso_set_output_1, "+stream", true, false, BLAH},
	{"set_error", 1, bif_iso_set_output_1, "+stream", true, false, BLAH},
	{"stream_property", 2, bif_iso_stream_property_2, "+stream,+compound", true, false, BLAH},


	// Edinburgh...

	{"seeing", 1, bif_edin_seeing_1, "-atom", false, false, BLAH},
	{"telling", 1, bif_edin_telling_1, "-atom", false, false, BLAH},
	{"seen", 0, bif_edin_seen_0, NULL, false, false, BLAH},
	{"told", 0, bif_edin_told_0, NULL, false, false, BLAH},
	{"redo", 1, bif_edin_redo_1, "+integer", false, false, BLAH},
	{"redo", 2, bif_edin_redo_2, "+stream,+integer", false, false, BLAH},
	{"tab", 1, bif_edin_tab_1, "+integer", false, false, BLAH},
	{"tab", 2, bif_edin_tab_2, "+stream,+integer", false, false, BLAH},
	{"portray_clause", 1, bif_portray_clause_1, "+term", false, false, BLAH},
	{"portray_clause", 2, bif_portray_clause_2, "+stream,+term", false, false, BLAH},

	// Other...

	{"is_stream", 1, bif_is_stream_1, "+term", false, false, BLAH},
	{"unget_char", 1, bif_unget_char_1, "+integer", true, false, BLAH},
	{"unget_char", 2, bif_unget_char_2, "+stream,+integer", true, false, BLAH},
	{"unget_code", 1, bif_unget_code_1, "+integer", true, false, BLAH},
	{"unget_code", 2, bif_unget_code_2, "+stream,+integer", true, false, BLAH},
	{"unget_byte", 1, bif_unget_byte_1, "+integer", true, false, BLAH},
	{"unget_byte", 2, bif_unget_byte_2, "+stream,+integer", true, false, BLAH},
	{"set_stream", 2, bif_set_stream_2, "+stream,+term", true, false, BLAH},
	{"getline", 1, bif_getline_1, "-atom", false, false, BLAH},
	{"getline", 2, bif_getline_2, "+stream,-string", false, false, BLAH},
	{"getline", 3, bif_getline_3, "+stream,-string,+list", false, false, BLAH},
	{"getlines", 1, bif_getlines_1, "-list", false, false, BLAH},
	{"getlines", 2, bif_getlines_2, "+stream,-list", false, false, BLAH},
	{"getlines", 3, bif_getlines_3, "+stream,-list,+list", false, false, BLAH},
	{"load_files", 2, bif_load_files_2, "+atom,+list", false, false, BLAH},
	{"unload_files", 1, bif_unload_files_1, "+atom", false, false, BLAH},
	{"make", 0, bif_make_0, NULL, false, false, BLAH},
	{"getfile", 2, bif_getfile_2, "+atom,-list", false, false, BLAH},
	{"getfile", 3, bif_getfile_3, "+atom,-list,+list", false, false, BLAH},
	{"loadfile", 2, bif_loadfile_2, "+atom,-atom", false, false, BLAH},
	{"savefile", 2, bif_savefile_2, "+atom,+atom", false, false, BLAH},
	{"rename_file", 2, bif_rename_file_2, "+atom,+atom", false, false, BLAH},
	{"copy_file", 2, bif_copy_file_2, "+atom,+atom", false, false, BLAH},
	{"directory_files", 2, bif_directory_files_2, "+atom,-list", false, false, BLAH},
	{"delete_file", 1, bif_delete_file_1, "+atom", false, false, BLAH},
	{"exists_file", 1, bif_exists_file_1, "+atom", false, false, BLAH},
	{"access_file", 2, bif_access_file_2, "+atom,+atom", false, false, BLAH},
	{"time_file", 2, bif_time_file_2, "+atom,-float", false, false, BLAH},
	{"size_file", 2, bif_size_file_2, "+atom,-integer", false, false, BLAH},
	{"exists_directory", 1, bif_exists_directory_1, "+atom", false, false, BLAH},
	{"make_directory", 1, bif_make_directory_1, "+atom", false, false, BLAH},
	{"make_directory_path", 1, bif_make_directory_path_1, "+atom", false, false, BLAH},
	{"working_directory", 2, bif_working_directory_2, "-atom,+atom", false, false, BLAH},
	{"absolute_file_name", 3, bif_absolute_file_name_3, "+atom,-atom,+list", false, false, BLAH},
	{"is_absolute_file_name", 1, bif_is_absolute_file_name_1, "+atom", false, false, BLAH},
	{"chdir", 1, bif_chdir_1, "+atom", false, false, BLAH},
	{"$put_chars", 1, bif_sys_put_chars_1, "+string", false, false, BLAH},
	{"$put_chars", 2, bif_sys_put_chars_2, "+stream,+string", false, false, BLAH},
	{"read_term_from_atom", 3, bif_read_term_from_atom_3, "+atom,?term,+list", false, false, BLAH},
	{"read_term_from_chars", 3, bif_read_term_from_chars_3, "+string,?term,+list", false, false, BLAH},
	{"$read_term_from_chars", 4, bif_sys_read_term_from_chars_4, "?term,+list,+string,-string", false, false, BLAH},
	{"write_term_to_atom", 3, bif_write_term_to_atom_3, "?atom,?term,+list", false, false, BLAH},
	{"write_canonical_to_atom", 3, bif_write_canonical_to_chars_3, "?atom,?term,+list", false, false, BLAH},
	{"write_term_to_chars", 3, bif_write_term_to_chars_3, "?term,+list,?string", false, false, BLAH},
	{"write_canonical_to_chars", 3, bif_write_canonical_to_chars_3, "?string,?term,+list", false, false, BLAH},
	{"read_line_to_string", 2, bif_read_line_to_string_2, "+stream,-string", false, false, BLAH},
	{"read_file_to_string", 3, bif_read_file_to_string_3, "+atom,-string,+options", false, false, BLAH},

	{"http_location", 2, bif_http_location_2, "?list,?atom", false, false, BLAH},
	{"parse_url", 2, bif_parse_url_2, "?atom,?list", false, false, BLAH},
	{"client", 5, bif_client_5, "+atom,-atom,-atom,-atom,+list", false, false, BLAH},
	{"server", 3, bif_server_3, "+atom,--stream,+list", false, false, BLAH},
	{"accept", 2, bif_accept_2, "+stream,--stream", false, false, BLAH},
	{"bread", 3, bif_bread_3, "+stream,+integer,-string", false, false, BLAH},
	{"bwrite", 2, bif_bwrite_2, "+stream,-string", false, false, BLAH},

	{"$capture_output", 0, bif_sys_capture_output_0, NULL, false, false, BLAH},
	{"$capture_output_to_chars", 1, bif_sys_capture_output_to_chars_1, "-string", false, false, BLAH},
	{"$capture_output_to_atom", 1, bif_sys_capture_output_to_atom_1, "-atom", false, false, BLAH},

	{"$capture_error", 0, bif_sys_capture_error_0, NULL, false, false, BLAH},
	{"$capture_error_to_chars", 1, bif_sys_capture_error_to_chars_1, "-string", false, false, BLAH},
	{"$capture_error_to_atom", 1, bif_sys_capture_error_to_atom_1, "-atom", false, false, BLAH},

#if !defined(_WIN32) && !defined(__wasi__) && !defined(__ANDROID__)
	{"process_create", 3, bif_process_create_3, "+atom,+list,+list", false, false, BLAH},
	{"process_wait", 2, bif_process_wait_2, "+integer,-integer", false, false, BLAH},
	{"process_wait", 1, bif_process_wait_1, "+integer", false, false, BLAH},
	{"process_kill", 2, bif_process_kill_2, "+integer,+integer", false, false, BLAH},
	{"process_kill", 1, bif_process_kill_1, "+integer", false, false, BLAH},
#endif

#if !defined(_WIN32) && !defined(__wasi__)
	{"popen", 4, bif_popen_4, "+atom,+atom,--stream,+list", false, false, BLAH},
#endif

	{0}
};

