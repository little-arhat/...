(require 'prh-bufsw)
(setq stesla-hated-buffer-regexps '("^ " "*Buffer"
                                    "*Scratch*"
                                    "^\\*trace"
                                    "^\\*tramp"
                                    "^\\*"
                                    "*Messages*"
                                    "*Completions*"))
(setq pc-bufsw::quite-time 1)
