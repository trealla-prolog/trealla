#include "library.h"

extern unsigned char library_builtins_pl[];
extern unsigned int library_builtins_pl_len;
extern unsigned char library_lists_pl[];
extern unsigned int library_lists_pl_len;
extern unsigned char library_heaps_pl[];
extern unsigned int library_heaps_pl_len;
extern unsigned char library_http_pl[];
extern unsigned int library_http_pl_len;
extern unsigned char library_atts_pl[];
extern unsigned int library_atts_pl_len;
extern unsigned char library_dcgs_pl[];
extern unsigned int library_dcgs_pl_len;
extern unsigned char library_pio_pl[];
extern unsigned int library_pio_pl_len;
extern unsigned char library_si_pl[];
extern unsigned int library_si_pl_len;
extern unsigned char library_format_pl[];
extern unsigned int library_format_pl_len;
extern unsigned char library_charsio_pl[];
extern unsigned int library_charsio_pl_len;
extern unsigned char library_assoc_pl[];
extern unsigned int library_assoc_pl_len;
extern unsigned char library_ordsets_pl[];
extern unsigned int library_ordsets_pl_len;
extern unsigned char library_dict_pl[];
extern unsigned int library_dict_pl_len;
extern unsigned char library_freeze_pl[];
extern unsigned int library_freeze_pl_len;
extern unsigned char library_dif_pl[];
extern unsigned int library_dif_pl_len;
extern unsigned char library_error_pl[];
extern unsigned int library_error_pl_len;
extern unsigned char library_when_pl[];
extern unsigned int library_when_pl_len;
extern unsigned char library_pairs_pl[];
extern unsigned int library_pairs_pl_len;
extern unsigned char library_random_pl[];
extern unsigned int library_random_pl_len;
extern unsigned char library_lambda_pl[];
extern unsigned int library_lambda_pl_len;
extern unsigned char library_ugraphs_pl[];
extern unsigned int library_ugraphs_pl_len;
extern unsigned char library_sqlite3_pl[];
extern unsigned int library_sqlite3_pl_len;
extern unsigned char library_json_pl[];
extern unsigned int library_json_pl_len;
extern unsigned char library_abnf_pl[];
extern unsigned int library_abnf_pl_len;
extern unsigned char library_raylib_pl[];
extern unsigned int library_raylib_pl_len;
extern unsigned char library_curl_pl[];
extern unsigned int library_curl_pl_len;
extern unsigned char library_gsl_pl[];
extern unsigned int library_gsl_pl_len;
extern unsigned char library_concurrent_pl[];
extern unsigned int library_concurrent_pl_len;
extern unsigned char library_gensym_pl[];
extern unsigned int library_gensym_pl_len;
extern unsigned char library_uuid_pl[];
extern unsigned int library_uuid_pl_len;
extern unsigned char library_linda_pl[];
extern unsigned int library_linda_pl_len;
extern unsigned char library_rbtrees_pl[];
extern unsigned int library_rbtrees_pl_len;
extern unsigned char library_reif_pl[];
extern unsigned int library_reif_pl_len;
extern unsigned char library_debug_pl[];
extern unsigned int library_debug_pl_len;
extern unsigned char library_arithmetic_pl[];
extern unsigned int library_arithmetic_pl_len;
extern unsigned char library_clpz_pl[];
extern unsigned int library_clpz_pl_len;
extern unsigned char library_iso_ext_pl[];
extern unsigned int library_iso_ext_pl_len;
extern unsigned char library_aggregate_pl[];
extern unsigned int library_aggregate_pl_len;
extern unsigned char library_time_pl[];
extern unsigned int library_time_pl_len;

library g_libs[] = {
	 {"abnf", library_abnf_pl, &library_abnf_pl_len},
	 {"aggregate", library_aggregate_pl, &library_aggregate_pl_len},
	 {"arithmetic", library_arithmetic_pl, &library_arithmetic_pl_len},
	 {"assoc", library_assoc_pl, &library_assoc_pl_len},
	 {"atts", library_atts_pl, &library_atts_pl_len},
	 {"builtins", library_builtins_pl, &library_builtins_pl_len},
	 {"charsio", library_charsio_pl, &library_charsio_pl_len},
	 {"concurrent", library_concurrent_pl, &library_concurrent_pl_len},
	 {"clpz", library_clpz_pl, &library_clpz_pl_len},
	 {"curl", library_curl_pl, &library_curl_pl_len},
	 {"dcgs", library_dcgs_pl, &library_dcgs_pl_len},
	 {"debug", library_debug_pl, &library_debug_pl_len},
	 {"dict", library_dict_pl, &library_dict_pl_len},
	 {"dif", library_dif_pl, &library_dif_pl_len},
	 {"error", library_error_pl, &library_error_pl_len},
	 {"format", library_format_pl, &library_format_pl_len},
	 {"freeze", library_freeze_pl, &library_freeze_pl_len},
	 {"gensym", library_gensym_pl, &library_gensym_pl_len},
	 {"gsl", library_gsl_pl, &library_gsl_pl_len},
	 {"heaps", library_heaps_pl, &library_heaps_pl_len},
	 {"http", library_http_pl, &library_http_pl_len},
	 {"iso_ext", library_iso_ext_pl, &library_iso_ext_pl_len},
	 {"json", library_json_pl, &library_json_pl_len},
	 {"lambda", library_lambda_pl, &library_lambda_pl_len},
	 {"linda", library_linda_pl, &library_linda_pl_len},
	 {"lists", library_lists_pl, &library_lists_pl_len},
	 {"ordsets", library_ordsets_pl, &library_ordsets_pl_len},
	 {"pairs", library_pairs_pl, &library_pairs_pl_len},
	 {"pio", library_pio_pl, &library_pio_pl_len},
	 {"random", library_random_pl, &library_random_pl_len},
	 {"raylib", library_raylib_pl, &library_raylib_pl_len},
	 {"rbtrees", library_rbtrees_pl, &library_rbtrees_pl_len},
	 {"reif", library_reif_pl, &library_reif_pl_len},
	 {"si", library_si_pl, &library_si_pl_len},
	 {"sqlite3", library_sqlite3_pl, &library_sqlite3_pl_len},
	 {"time", library_time_pl, &library_time_pl_len},
	 {"ugraphs", library_ugraphs_pl, &library_ugraphs_pl_len},
	 {"uuid", library_uuid_pl, &library_uuid_pl_len},
	 {"when", library_when_pl, &library_when_pl_len},

	 {0}
};
