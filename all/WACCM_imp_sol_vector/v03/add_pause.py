import glob

files = glob.glob('*.gnuplot')
#print len(files)

for f in files:

    with open(f, 'a') as file:
        file.write('pause -1')

