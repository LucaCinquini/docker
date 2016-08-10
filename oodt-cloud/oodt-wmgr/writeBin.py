# Python program that writes a binary file of N number of MB

from array import array

size_in_mb = 10

output_file = open('file', 'wb')

# each loop iteration will write out 1 KB of data
for i in range(size_in_mb*1024): # loop over KB

  # array of 128 doubles = 128x8 bytes = 1024 bytes = 1 KB
  data = range(1024/8)
  float_array = array('d', data) # array of 'double' - each double is 8 bytes
  float_array.tofile(output_file)

output_file.close()
