import math
import os
import stat
import argparse

parser = argparse.ArgumentParser()

parser.add_argument("start", type=int, help="Start")
parser.add_argument("stop", type=int, help="End")
parser.add_argument("localdir", help="Local directory to mount")
parser.add_argument("step", type=int, help="Asteroids per step")
parser.add_argument("cpu", type=int, help="Number of CPUs")

args = parser.parse_args()

num_iterations = math.ceil(round((args.stop - args.start)/args.step)/args.cpu)
step = args.step

docker_command = "docker run -v {0}/{1}:/root/resonances/output smirik/resonances-fortran ./comp.x -range {2} {3}"
cat_command = "cat {0}/{1}/results/result_{2}body.txt >> {2}body.txt"
commands = []
for i in range(args.cpu):
    commands.append([])

flag = False
for i in range(args.cpu):
    for j in range(num_iterations):
        tmp = args.start+step*j+i*step*num_iterations
        if tmp > args.stop:
            flag = True
            break
        commands[i].append(docker_command.format(args.localdir, i, tmp, tmp+step-1))
        commands[i].append(cat_command.format(args.localdir, i, 3))
        commands[i].append(cat_command.format(args.localdir, i, 2))
    if flag:
        break

localdir = args.localdir+'/{0}/{1}'
sub_dirs = ['aei', 'aei_planets', 'id_matrices', 'results']
for i in range(args.cpu):
    os.makedirs(localdir.format(i, 'aei'), exist_ok=True)
    os.makedirs(localdir.format(i, 'aei_planets'), exist_ok=True)
    os.makedirs(localdir.format(i, 'id_matrices'), exist_ok=True)
    os.makedirs(localdir.format(i, 'results'), exist_ok=True)

for i in range(args.cpu):
    filename = args.localdir+'/cpu{0}.sh'.format(i)
    f= open(filename,"w+")
    tmp = "\n".join(commands[i])
    f.write(tmp)
    f.close
    st = os.stat(filename)
    os.chmod(filename, st.st_mode | stat.S_IEXEC)

run_filename = args.localdir+'/run.sh'
run_file = open(run_filename,"w+")
run_command = "nohup ./cpu{0}.sh &>/dev/null &\n"
for i in range(args.cpu):
    filename = run_command.format(i)
    run_file.write(filename)
run_file.close
st = os.stat(run_filename)
os.chmod(run_filename, st.st_mode | stat.S_IEXEC)
