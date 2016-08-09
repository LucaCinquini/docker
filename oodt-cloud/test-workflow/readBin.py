# Python program that reads a binary file
from array import array

# method taht reads 1MB at a time
def read_in_chunks(file_object, chunk_size=1024*1024): # 1024 bytes x 1024 bytes = 1 MB
    while True:
        data = file_object.read(chunk_size)
        if not data:
            break
        yield data

input_file = open('file', 'rb')
float_array = array('d')
for data in read_in_chunks(input_file):
   float_array.fromstring( data )
   #print float_array
