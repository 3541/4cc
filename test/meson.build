fs = import('fs')

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
  ]
}

libtest = static_library('testlib', 'test.c', override_options: 'b_sanitize=none')

foreach suite, ts : tests
  foreach t : ts
    path = meson.current_source_dir() / suite

    test(
      suite / t,
      python,
      depends: libtest,
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

extra_stage = 2
foreach stage : range(1, extra_stage)
  extra_tests = run_command(
    [
      python,
      meson.current_source_dir() / 'extra_tests.py',
      'valid',
      meson.current_source_dir() / 'extra',
      stage.to_string()
    ],
    check: true
  ).stdout().strip().split('\n')

  foreach t : extra_tests
    name = fs.stem(t)
    correct = executable(name, t)

    test(
      'extra' / stage.to_string() / name,
      python,
      depends: correct,
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
      stage.to_string()
    ],
    check: true
  ).stdout().strip().split('\n')

  foreach t : extra_invalid
    name = fs.stem(t)

    test(
      'extra' / stage.to_string() / 'invalid' / name,
      python,
      args: [
        meson.current_source_dir() / 'test_invalid.py',
        fcc,
        t
      ]
    )
  endforeach
endforeach
