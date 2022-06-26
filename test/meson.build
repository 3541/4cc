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
    'double_unary_minus'
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
