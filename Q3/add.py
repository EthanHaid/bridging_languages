import sys
import string


if __name__ == '__main__':
   
   # args check
   if (len(sys.argv) < 3):
      print("missing input file name")
      sys.exit()

   # parse file
   print(float(sys.argv[1]) + float(sys.argv[2]))
   
   

