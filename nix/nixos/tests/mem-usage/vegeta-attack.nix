{ writeText, port }:

writeText "vegeta.atk" ''
  POST http://localhost:${builtins.toString port}/metadata/query
  Content-Type: application/json
  @${./large-query.json}
''
