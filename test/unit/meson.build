fs = import('fs')

c = meson.get_compiler('c')
python = find_program('python3')

tests = {
  'expr': [
    'num0',
    'num42',
    'add_sub',
    'spaces',
    'div',
    'mul',
    'paren',
    'unary_minus',
    'unary_plus',
    'double_unary_minus',
    'eq_ne',
    'eq_eq',
    'ne_ne',
    'ne_eq',
    'lt_lt',
    'lt_eq',
    'lt_gt',
    'le_lt',
    'le_eq',
    'le_gt',
    'gt_lt',
    'gt_eq',
    'gt_gt',
    'ge_lt',
    'ge_eq',
    'ge_gt',
    'multiple_stmts',
    'mod',
    'i32',
    'cast',
    'shl',
    'shr',
    'and',
    'or',
    'inc',
    'dec',
    'char',
    'compound_assign',
    'null_assign',
    'hex',
    'octal',
    'unsigned_literal',
    'long_suffix'
  ],
  'var': [
    'simple',
    'simple_split',
    'arithmetic',
    'multiple',
    'long_ident',
    'mixed_ident',
    'mutate',
    'align',
    'static',
    'fn_ptr',
    'fn_ptr_nested'
  ],
  'return': [
    'early',
    'mid',
    'last'
  ],
  'stmt': [
    'block',
    'block_scope',
    'block_scope_assign',
    'empty'
  ],
  'if': [
    'simple_false',
    'simple_true',
    'expr_false',
    'expr_true',
    'else_false',
    'else_true',
    'nested'
  ],
  'loop': [
    'simple_for',
    'return_from_for',
    'simple_while',
    'nested',
    'break',
    'break_nested',
    'continue',
    'continue_nested',
    'do_while',
    'do_while_once'
  ],
  'pointer': [
    'simple',
    'double',
    'mutate',
    'add',
    'sub',
    'add_lhs',
    'sub_lhs',
    'diff',
    'void'
  ],
  'call': [
    'simple',
    'expr',
    'args',
    'args6',
    'nested',
    'very_nested',
    'variadic',
    'static'
  ],
  'fn': [
    'simple',
    'multiple',
    'params',
    'recursive'
  ],
  'array': [
    'simple',
    'pointer',
    '2d',
    'index',
    'index_2d',
    'pointer_index'
  ],
  'char': [
    'simple',
    'arithmetic'
  ],
  'global': [
    'simple',
    'array',
    'init'
  ],
  'string': [
    'empty',
    'index',
    'arithmetic',
    'arg',
    'concat'
  ],
  'escape': [
    'single_quote',
    'double_quote',
    'question',
    'backslash',
    'bell',
    'backspace',
    'escape',
    'form_feed',
    'lf',
    'cr',
    'tab',
    'vtab',
    'text',
    'octal',
    'hex'
  ],
  'sizeof': [
    'int',
    'ptr',
    'array',
    'i8',
    'i16',
    'i32',
    'i64',
    'short',
    'long',
    'unsigned',
    'signed',
    'type'
  ],
  'comment': [
    'line',
    'block'
  ],
  'struct': [
    'simple',
    'multiple_types',
    'array',
    'array_member',
    'nested',
    'comma',
    'size_simple',
    'size_multiple_types',
    'alignment',
    'named',
    'named_decl',
    'named_scoped',
    'named_ptr',
    'named_global_decl',
    'global',
    'assign',
    'ptr_access',
    'forward_decl'
  ],
  'union': [
    'simple',
    'size',
    'nested',
    'name',
    'ptr',
    'assign'
  ],
  'ternary': [
    'simple',
    'nested',
    'omit_true'
  ],
  'decl': [
    'simple',
    'array',
    'array_of_array',
    'array_of_ptr',
    'ptr_to_array',
    'composite',
    'extern',
    'extern_fn',
    'const',
    'const_eval',
    'extern_unsized_array'
  ],
  'typedef': [
    'simple',
    'struct',
    'scope',
    'empty',
    'global',
    'nested',
    'suffix'
  ],
  'std': [
    'assert',
    'ctype',
    'stdio',
    'string'
  ],
  'enum': [
    'simple',
    'named',
    'size',
    'explicit'
  ],
  'init': [
    'array'
  ]
}

libtest = static_library(
  'testlib',
  'lib/test.c',
  override_options: 'b_sanitize=none',
)

foreach suite, ts : tests
  foreach t : ts
    path = meson.current_source_dir() / suite

    test(
      t,
      python,
      depends: libtest,
      suite: suite,
      args: [
        meson.current_source_dir() / 'test.py',
        fcc,
        libtest,
        path / t + '.c',
        path / t + '.out'
      ],
    )
  endforeach
endforeach

test_bin_cflags = c.get_supported_arguments('-w')
extra_tests = run_command(
  [
    python,
    meson.current_source_dir() / 'extra_tests.py',
    'valid',
    meson.current_source_dir() / 'extra'
  ],
  check: true
).stdout().strip().split('\n')

foreach t : extra_tests
  s = t.split('\0')
  if s.length() != 2
    continue
  endif

  name = s[0]
  file = s[1]

  correct = executable(
    name,
    file,
    override_options: 'warning_level=0',
    c_args: test_bin_cflags
  )

  test(
    name,
    python,
    depends: correct,
    suite: 'extra',
    args: [
      meson.current_source_dir() / 'test.py',
      fcc,
      libtest,
      file,
      correct
    ]
  )
endforeach

extra_invalid = run_command(
  [
    python,
    meson.current_source_dir() / 'extra_tests.py',
    'invalid',
    meson.current_source_dir() / 'extra',
  ],
  check: true
).stdout().strip().split('\n')

foreach t : extra_invalid
  s = t.split('\0')
  if s.length() != 2
    continue
  endif

  name = s[0]
  file = s[1]

  test(
    name,
    python,
    suite: 'extra/invalid',
    args: [
      meson.current_source_dir() / 'test_invalid.py',
      fcc,
      file
    ]
  )
endforeach
