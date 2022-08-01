subdir('include')

c = meson.get_compiler('c')
c_arg = c.get_argument_syntax()

cflags = ['-D_XOPEN_SOURCE=700']
ldflags = []

if get_option('profile')
  cflags += '-D4C_PROFILE'
endif

flags_wanted = {
  'gcc': ['-fstack-protector', '-fstack-clash-protection'],
  'msvc': ['/GS', '/Zc:__cplusplus']
}
cflags += c.get_supported_arguments(flags_wanted[c_arg])
ldflags += c.get_supported_link_arguments(flags_wanted[c_arg])

warnings_wanted = {
  'gcc': [
    '-Wdisabled-optimization', '-Wduplicated-branches', '-Wduplicated-cond', '-Wfloat-equal',
    '-Wformat-nonliteral', '-Wformat-security', '-Wlogical-op', '-Wmissing-declarations',
    '-Wmissing-include-dirs', '-Wnull-dereference', '-Wpacked', '-Wshadow', '-Wstack-protector',
    '-Wundef', '-Wcast-align', '-Wbad-function-cast', '-Wimplicit', '-Wmissing-prototypes',
    '-Wnested-externs', '-Wstrict-prototypes', '-Wpointer-arith', '-Wno-pedantic',
    '-Wno-missing-field-initializers'
  ],

  'msvc': [
    '-wd5105'
  ]
}
cflags += c.get_supported_arguments(warnings_wanted[c_arg])

wconversion_test = '''
#include <stdint.h>

int main(void) {
    uint8_t x = 1;
    x *= 16;
    return (int)x;
}
'''
if c.compiles(wconversion_test, args: ['-Werror=conversion', '-Wconversion'])
  message('-Wconversion works correctly.')
  cflags += '-Wconversion'
else
  message('-Wconversion missing or broken.')
endif

a3 = dependency('a3', fallback: ['a3', 'a3_dep'])

src = files([
  'ast.c',
  'dump.c',
  'eval.c',
  'error.c',
  'gen.c',
  'lex.c',
  'main.c',
  'parse.c',
  'subprocess.c',
  'type.c',
  'util.c'
])

fcc = executable(
  '4cc',
  src,
  dependencies: a3,
  c_args: cflags,
  link_args: ldflags,
  build_by_default: true,
  install: true
)
