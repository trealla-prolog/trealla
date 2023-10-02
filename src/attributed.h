#include "query.h"

bool fn_sys_list_attributed_1(query *q);
bool fn_sys_put_attributes_2(query *q);
bool fn_sys_get_attributes_2(query *q);
bool fn_sys_unattributed_var_1(query *q);
bool fn_sys_attributed_var_1(query *q);

bool do_post_unification_hook(query *q, bool is_builtin);
bool fn_sys_undo_trail_2(query *q);
bool fn_sys_redo_trail_1(query *q);

