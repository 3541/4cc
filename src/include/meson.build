fs = import('fs')

internal_headers = files(['stdarg.h', 'stddef.h'])
foreach header : internal_headers
  configure_file(
    input: header,
    output: fs.name('@0@'.format(header)),
    copy: true
  )
endforeach
