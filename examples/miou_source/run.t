  $ ./main.exe
  Parsed 9 log entries across 3 domains:
    2024-01-15T11:00:00 ERROR [db] Connection refused
    2024-01-15T12:00:30 ERROR [auth] Token expired
    2024-01-15T10:30:02 WARN  [http] GET /missing.png 404
    2024-01-15T12:00:00 WARN  [auth] Token near expiry
    2024-01-15T10:30:00 INFO  [http] GET /index.html 200
    2024-01-15T10:30:01 INFO  [http] GET /style.css 200
    2024-01-15T11:00:05 INFO  [db] Reconnected successfully
    2024-01-15T11:00:06 INFO  [http] POST /api/data 201
    2024-01-15T12:01:00 INFO  [auth] Token refreshed
