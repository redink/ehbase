import happybase

connection = happybase.Connection(host = '127.0.0.1', transport = 'framed')

table = connection.table('redink_test')

for x in range(1,1000000):
	table.put(str(x), {'name:': 'tom'})
