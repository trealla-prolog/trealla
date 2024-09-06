#include <stdlib.h>
#include <stdio.h>

#include "prolog.h"
#include "query.h"

#include "sre/re.h"

bool bif_sre_compile_2(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,var);
	const char *pattern = C_STR(q, p1);
	unsigned char *buf;
	re_t reg = re_compile(pattern, &buf);
	if (!reg) return false;
	cell tmp = {0};
	tmp.tag = TAG_BLOB;
	tmp.flags = FLAG_MANAGED | FLAG_BLOB_SREGEX;
	tmp.nbr_cells = 1;
	tmp.val_blob = malloc(sizeof(blob));
	check_heap_error(tmp.val_blob);
	tmp.val_blob->ptr = (void*)reg;
	tmp.val_blob->ptr2 = (void*)buf;
	tmp.val_blob->refcnt = 1;
	bool ok = unify(q, p2, p2_ctx, &tmp, q->st.curr_frame);
	unshare_cell(&tmp);
	return ok;
}

bool bif_sre_matchp_4(query *q)
{
	GET_FIRST_ARG(p1,sregex);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,atom_or_var);
	re_t re = (void*)p1->val_blob->ptr;
	const char *text = C_STR(q, p2);
	int len = 0;
	int off = re_matchp(re, text, &len);
	cell tmp1, tmp2;

	if (!len)
		make_atom(&tmp1, g_nil_s);
	else
		make_slice(q, &tmp1, p2, off, len);

	bool ok = unify(q, p3, p3_ctx, &tmp1, q->st.curr_frame);
	unshare_cell(&tmp1);
	if (!ok) return false;

	if ((size_t)(off + len) >= C_STRLEN(q, p2))
		make_atom(&tmp2, g_nil_s);
	else
		make_slice(q, &tmp2, p2, off + len, C_STRLEN(q, p2)-(off+len));

	ok = unify(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	unshare_cell(&tmp2);
	return ok;
}

bool bif_sre_match_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,atom_or_var);
	const char *pattern = C_STR(q, p1);
	const char *text = C_STR(q, p2);
	int len = 0;
	int off = re_match(pattern, text, &len);
	cell tmp1, tmp2;

	if (!len)
		make_atom(&tmp1, g_nil_s);
	else
		make_slice(q, &tmp1, p2, off, len);

	bool ok = unify(q, p3, p3_ctx, &tmp1, q->st.curr_frame);
	unshare_cell(&tmp1);
	if (!ok) return false;

	if ((size_t)(off + len) >= C_STRLEN(q, p2))
		make_atom(&tmp2, g_nil_s);
	else
		make_slice(q, &tmp2, p2, off + len, C_STRLEN(q, p2)-(off+len));

	ok = unify(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	unshare_cell(&tmp2);
	return ok;
}

bool bif_sre_substp_4(query *q)
{
	GET_FIRST_ARG(p1,sregex);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,atom_or_var);
	re_t re = (void*)p1->val_blob->ptr;
	const char *text = C_STR(q, p2);
	int len = 0;
	int off = re_matchp(re, text, &len);
	cell tmp1, tmp2;

	if (!len)
		make_stringn(&tmp1, text, C_STRLEN(q, p2));
	else
		make_slice(q, &tmp1, p2, 0, off);

	bool ok = unify(q, p3, p3_ctx, &tmp1, q->st.curr_frame);
	unshare_cell(&tmp1);
	if (!ok) return false;

	if ((size_t)(off + len) >= C_STRLEN(q, p2))
		make_atom(&tmp2, g_nil_s);
	else
		make_slice(q, &tmp2, p2, off + len, C_STRLEN(q, p2)-(off+len));

	ok = unify(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	unshare_cell(&tmp2);
	return ok;
}

bool bif_sre_subst_4(query *q)
{
	GET_FIRST_ARG(p1,atom);
	GET_NEXT_ARG(p2,atom);
	GET_NEXT_ARG(p3,atom_or_var);
	GET_NEXT_ARG(p4,atom_or_var);
	const char *pattern = C_STR(q, p1);
	const char *text = C_STR(q, p2);
	int len = 0;
	int off = re_match(pattern, text, &len);
	cell tmp1, tmp2;

	if (!len)
		make_stringn(&tmp1, text, C_STRLEN(q, p2));
	else
		make_slice(q, &tmp1, p2, 0, off);

	bool ok = unify(q, p3, p3_ctx, &tmp1, q->st.curr_frame);
	unshare_cell(&tmp1);
	if (!ok) return false;

	if ((size_t)(off + len) >= C_STRLEN(q, p2))
		make_atom(&tmp2, g_nil_s);
	else
		make_slice(q, &tmp2, p2, off + len, C_STRLEN(q, p2)-(off+len));

	ok = unify(q, p4, p4_ctx, &tmp2, q->st.curr_frame);
	unshare_cell(&tmp2);
	return ok;
}

builtins g_sregex_bifs[] =
{
	{"sre_compile", 2, bif_sre_compile_2, "+string,-string,", false, false, BLAH},
	{"sre_matchp", 4, bif_sre_matchp_4, "+string,+string,-string,-string,", false, false, BLAH},
	{"sre_match", 4, bif_sre_match_4, "+string,+string,-string,-string,", false, false, BLAH},
	{"sre_substp", 4, bif_sre_substp_4, "+string,+string,-string,-string,", false, false, BLAH},
	{"sre_subst", 4, bif_sre_subst_4, "+string,+string,-string,-string,", false, false, BLAH},

	{0}
};
