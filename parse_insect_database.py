import sys

outfile = open('cams_fixed_up_db.csv', 'w')

def keep_it(l):
    outfile.write(l + '\r')

spec_list = []
spec_wing_list = []

infile = open(sys.argv[1], 'r')
lines = infile.readlines()[0].split('\r')
outfile.write(lines[0]+'\r')
outlines = []
for l in lines:
    spl_line = l.split(',')
    species = spl_line[1].strip()
    part = spl_line[9].strip()
    meas = spl_line[10].strip()
    if meas == 'length':
        if spec_list.count(species) < 2:
            if part == 'body':
                keep_it(l)
                spec_list.append(species)
                print('species: {0}, part: {1}, meas: {2}'.format(species,part,meas))
            elif 'wing' in part or 'elyt' in part or 'teg' in part or 'tergum' in part or 'complete' in part:
                if 'wing pads' in part or 'wing scale' in part or 'nymph' in part:
                    continue
                if species not in spec_wing_list:
                    keep_it(l)
                    spec_list.append(species)
                    spec_wing_list.append(species)
                    print('species: {0}, part: {1}, meas: {2}'.format(species,part,meas))

infile.close()
outfile.close()
