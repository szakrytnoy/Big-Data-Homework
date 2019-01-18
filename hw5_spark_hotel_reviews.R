library(sparklyr)
spark_install("2.1.0")
sc <- spark_connect(master = 'local')
