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
  ],
  'var': [
    'simple',
    'simple_split',
    'arithmetic',
    'multiple',
    'long_ident',
    'mixed_ident',
    'mutate',
    'align'
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
    'diff'
  ],
  'call': [
    'simple',
    'expr',
    'args',
    'args6',
    'nested',
    'very_nested'
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
    'array'
  ],
  'string': [
    'empty',
    'index',
    'arithmetic',
    'arg'
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
    'array'
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
    'ptr_access'
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
  ]
}

libtest = static_library(
  'testlib',
  'test.c',
  override_options: 'b_sanitize=none',
  # Redeclaration of puts() without const qualifier.
  c_args: c.get_supported_arguments('-Wno-builtin-declaration-mismatch')
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
foreach stage : ['1', '2', '3', '4', '5', '6', '7']
  extra_tests = run_command(
    [
      python,
      meson.current_source_dir() / 'extra_tests.py',
      'valid',
      meson.current_source_dir() / 'extra',
      stage
    ],
    check: true
  ).stdout().strip().split('\n')

  foreach t : extra_tests
    name = fs.stem(t)
    correct = executable(
      stage + '_' + name,
      t,
      override_options: 'warning_level=0',
      c_args: test_bin_cflags
    )

    test(
      stage / name,
      python,
      depends: correct,
      suite: 'extra',
      args: [
        meson.current_source_dir() / 'test.py',
        fcc,
        libtest,
        t,
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
      stage
    ],
    check: true
  ).stdout().strip().split('\n')

  foreach t : extra_invalid
    name = fs.stem(t)

    test(
      name,
      python,
      suite: 'extra/invalid',
      args: [
        meson.current_source_dir() / 'test_invalid.py',
        fcc,
        t
      ]
    )
  endforeach
endforeach
