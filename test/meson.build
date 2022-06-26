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
  ]
}

foreach suite, ts : tests
  foreach t : ts
    test(
      suite / t,
      python,
      args: [
        meson.current_source_dir() / 'test.py',
        fcc,
        meson.current_source_dir() / suite,
        t
      ],
    )
  endforeach
endforeach
