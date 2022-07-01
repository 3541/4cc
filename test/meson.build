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
  'assign': [
    'simple',
    'simple_split',
    'arithmetic',
    'multiple',
    'long_ident',
    'mixed_ident',
    'mutate'
  ],
  'return': [
    'early',
    'mid',
    'last'
  ],
  'stmt': [
    'block',
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
    'nested'
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
  ]
}

libtest = static_library('testlib', 'test.c')

foreach suite, ts : tests
  foreach t : ts
    test(
      suite / t,
      python,
      depends: libtest,
      args: [
        meson.current_source_dir() / 'test.py',
        fcc,
        libtest,
        meson.current_source_dir() / suite,
        t
      ],
    )
  endforeach
endforeach
