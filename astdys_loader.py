import urllib.request

url = 'http://hamilton.dm.unipi.it/~astdys2/catalogs/allnum.cat'
filename = 'input/allnum.cat'

urllib.request.urlretrieve(url, filename)

print('"allnum.cat" successfully downloaded')
