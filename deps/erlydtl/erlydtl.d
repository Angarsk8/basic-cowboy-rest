src/erlydtl_beam_compiler.erl:: include/erlydtl_ext.hrl src/erlydtl_compiler.erl src/erlydtl_compiler_utils.erl; @touch $@
src/erlydtl_compiler.erl:: include/erlydtl_ext.hrl src/erlydtl_compiler_utils.erl; @touch $@
src/erlydtl_compiler_utils.erl:: include/erlydtl_ext.hrl; @touch $@
src/erlydtl_filters.erl:: src/erlydtl_time_compat.erl; @touch $@
src/i18n/sources_parser.erl:: include/erlydtl_ext.hrl; @touch $@

COMPILE_FIRST += erlydtl_compiler_utils erlydtl_compiler erlydtl_time_compat
