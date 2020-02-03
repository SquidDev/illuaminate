-- config: (lint (only syntax:string-escape))
local _ = {
  "a",
  "a\nb",
  "a\0b",
  "a\x00b",
  "a\(b",

  'a',
  'a\nb',
  'a\0b',
  'a\x00b',
  'a\(b',
}
