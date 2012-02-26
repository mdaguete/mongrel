{application, mongrel,
 [{description, "Record/Document Mapper for MongoDB"},
  {vsn, "0.0.2"},
  {modules, [mongrel_app, mongrel_types]},
  {registered, []},
  {applications, [kernel, stdlib, mongodb]},
  {mod, {mongrel_app, []}}
 ]}.
