import sys
import string


if __name__ == '__main__':
   
   # args check
   if (len(sys.argv) < 2):
      print("missing input file name")
      sys.exit()

   # parse file
   f = open(sys.argv[1], "r")
   exp = string.strip(f.readline())
   
   

