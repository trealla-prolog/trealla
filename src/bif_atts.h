#pragma once

#include "query.h"

bool bif_put_atts_2(query *q);
bool bif_get_atts_2(query *q);
bool bif_attribute_3(query *q);

bool bif_sys_list_attributed_1(query *q);
bool bif_sys_unattributed_var_1(query *q);
bool bif_sys_attributed_var_1(query *q);
bool bif_sys_undo_trail_2(query *q);
bool bif_sys_redo_trail_1(query *q);

bool do_post_unify_hook(query *q, bool is_builtin);
bool any_attributed(query *q);

