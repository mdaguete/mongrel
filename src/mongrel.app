{application, mongrel,
 [{description, "MongoDB Record/Document Mapper"},
  {vsn, "1.1.6"},
  {modules, [mongrel, mongrel_app, mongrel_cursor, mongrel_mapper, mongrel_sup, mongrel_types]},
  {registered, [mongrel_sup, mongrel_mapper]},
  {applications, [kernel, stdlib, mongodb]},
  {mod, {mongrel_app, []}}
 ]}.
