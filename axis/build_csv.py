import math
import os
import stat
import argparse
import re

matrices_2body = 'output/id_matrices/id_matrix_{0}.dat'
matrices_3body = 'output/id_matrices/id_matrix_{0}_{1}.dat'

output_csv_dir = 'output/id_matrices/csv/'
output_2body = 'output/id_matrices/csv/id_matrix_2body.csv'
output_3body = 'output/id_matrices/csv/id_matrix_3body.csv'

header_2body = '"planet1","m1","m","p1","p","semi_axis"'
header_3body = '"planet1","planet2","m1","m2","m","p1","p2","p","semi_axis"'

planets = ['MERCURY', 'VENUS', 'EARTHMOO', 'MARS', 'JUPITER', 'SATURN', 'URANUS', 'NEPTUNE']

os.makedirs(output_csv_dir, exist_ok=True)

csv_2body = open(output_2body,"w+")
csv_3body = open(output_3body,"w+")

csv_2body.write(header_2body+"\n");
csv_3body.write(header_3body+"\n");

pattern_2body = "{0},{1},{2},{3},{4},{5}\n";
pattern_3body = "{0},{1},{2},{3},{4},{5},{6},{7},{8}\n";

i=0
for planet1 in planets:
    input_file_2body = matrices_2body.format(planet1)
    with open(input_file_2body, "r+") as data:
        for line in data:
            tmp = re.sub(' +', ' ', line).strip().split(" ")
            csv_2body.write(pattern_2body.format(planet1, tmp[0], tmp[1], tmp[2], tmp[3], tmp[4]))

    j=0
    for planet2 in planets:
        if j>i:
            input_file_3body = matrices_3body.format(planet1, planet2)
            with open(input_file_3body, "r+") as data:
                for line in data:
                    tmp = re.sub(' +', ' ', line).strip().split(" ")
                    csv_3body.write(pattern_3body.format(planet1, planet2, tmp[0], tmp[1], tmp[2], tmp[3], tmp[4], tmp[5], tmp[6]))
        j+=1
    i+=1

csv_2body.close()
csv_3body.close()
