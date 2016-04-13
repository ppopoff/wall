{application, wall,
 [{description, "A wall where anyone can post"},
  {vsn, "0.1.0"},
  {modules, [wall_client,
	     wall_server]},
  {registered, [wall_server]},
  {applications, [kernel, stdlib]},
  {mod, {myapp_app, []}}
  ]}.
