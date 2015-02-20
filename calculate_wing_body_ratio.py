import sys
import math

infile = open(sys.argv[1], 'r')
outfile = open('wing_body_ratio_2_{0}'.format(sys.argv[1]), 'w')

fam_dict = {}
outfile.write('family,wing_body_ratio\r')
lines = infile.readlines()[0].split('\r')
entry = 0
for l in lines:
    entry += 1
    spl_line = l.split(',')
    if len(spl_line) < 2: continue
    family = spl_line[3]
    if len(family) < 2:
        print('entry {0}: {1}'.format(entry, family))
    species = spl_line[1]
    if family in fam_dict:
        fam_dict[family].update({species : [None,None]})
    else:
        fam_dict.update({family : {species : [None,None]}})

for l in lines[1:]:
    spl_line = l.split(',')
    if len(spl_line) < 2: continue
    family = spl_line[3]
    species = spl_line[1]
    size = float(spl_line[14])
    part = spl_line[9].lower()
    if part == 'body':
        fam_dict[family][species][0] = (size / 6)**2 * size * math.pi * 0.5
    else:
        fam_dict[family][species][1] = size**2 / 2

out_dict = dict.fromkeys(family)
for family in fam_dict.keys():
    species_ratios = []
    for species in fam_dict[family].keys():
        body_wing = fam_dict[family][species]
        if None not in body_wing:
            species_ratios.append(body_wing[1] / body_wing[0])
    if species_ratios:
        out_dict[family] = sum(species_ratios) / len(species_ratios)
    else:
        out_dict[family] = 0

for family in out_dict.keys():
    outfile.write('{0},{1}\r'.format(family, out_dict[family]))

infile.close()
outfile.close()
