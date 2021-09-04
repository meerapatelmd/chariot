SELECT *
FROM {log_schema}.{log_table}
WHERE {log_timestamp_field} IN
  (SELECT MAX({log_timestamp_field}) FROM {log_schema}.{log_table});
