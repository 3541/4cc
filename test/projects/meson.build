python = find_program('python3')

projects = ['getopt']

foreach project : projects
  dir = meson.current_source_dir() / project

  test(
    project,
    python,
    suite: 'third_party',
    args: [
      dir / 'build.py',
      fcc
    ]
  )
endforeach
