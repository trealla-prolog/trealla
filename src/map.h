#pragma once

#include "skiplist.h"

#define map skiplist
#define miter sliter

#define map_create sl_create
#define map_allow_dups sl_allow_dups
#define map_set_tmp sl_set_tmp
#define map_set sl_set
#define map_app sl_app
#define map_get sl_get
#define map_del sl_del
#define map_count sl_count
#define map_find sl_find
#define map_remove sl_remove

#define map_first sl_first
#define map_is_next sl_is_next
#define map_next sl_next
#define map_key sl_key

#define map_find_key sl_find_key
#define map_is_next_key sl_is_next_key
#define map_next_key sl_next_key

#define map_get_map sl_get_map
#define map_wild_card sl_wild_card
#define map_is_find sl_is_find
#define map_done sl_done
#define map_destroy sl_destroy
