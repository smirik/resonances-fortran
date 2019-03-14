[![Build Status](https://travis-ci.org/4xxi/resonances-fortran.svg?branch=master)](https://travis-ci.org/4xxi/resonances-fortran) [![Build Status](https://travis-ci.org/4xxi/resonances-fortran.svg?branch=develop)](https://travis-ci.org/4xxi/resonances-fortran)

The purpose of the program is to find potential resonances for asteroids and identify them. Currently, 2-body and 3-body cases are available for handling.

The software consists of the following modules:

* resonant axis calculator,
* resonance finder,
* integrator ([mercury6](https://github.com/4xxi/mercury) is used),
* phase builder,
* classifier,
* compositor etc.

For the references, please use:

1. Smirnov E., Dovgalev I. and Popova E. Asteroids in three-body mean motion resonances with planets. Icarus. 2017. (https://doi.org/10.1016/j.icarus.2017.09.032)
2. Smirnov E., Shevchenko I. Massive identification of asteroids in three-body resonances. Icarus. 2013. (http://dx.doi.org/10.1016/j.icarus.2012.10.034)

## Usage

There are some logical keys in `global_parameters.f90` that can be configured to make a special mode of running the program. Those keys are:

* `force_source_building`: if true, the program will create new binary source file and replace existing (requires basic source file `allnum.cat` from AstDyS catalog).
* `use_only_integration`: if true, the program will just perform an integration (using Mercury6 package) and create .aei files without finding any resonances.
* `just_id_matrices` will force the program only create files with id_matrices without finding any resonances and .aei files
* `force_aei_rebuilding`: if true, the integration will be performed even if corresponding .aei files already exist
* `mode_2body`: turns on 2-body case
* `mode_3body` turns on 3-body case

When all keys are configured, the program can be launched.

## Use program

1. Download [AstDyS](http://hamilton.dm.unipi.it/astdys/index.php?pc=4) catalog of the orbital elements (`allnum.cat` by default) and move to the input directory
   or, if you have installed Python 3 on your computer, run:

```bash
make source
```

   The currently used catalog version is available at [AstDyS 'allnum.cat'](http://hamilton.dm.unipi.it/~astdys2/catalogs/allnum.cat).
   At the first time the program will build the binary source file `asteroids.bin` from it. This file will be used in consequent sessions.

2. To compile the software run:

```bash
make
```

After these steps you can use the program:
```bash
./comp.x [-range | -list] <argument list/range>
```

* if `-list` key is used, just provide a list of asteroid names.
* if `-range` key is used, specify only first and last asteroid.

### Examples

```bash
./comp.x -list 1013 99942 309239 499
./comp.x -range 1 1000
```

#### Run under docker

Note that you should have a folder called `/root/output` (or replace it with another one) that has 4 sub-folders: `aei`, `aei_planets`, `id_matrices`, `results`. It will be mounted to docker in order to save the results in the files.

Of course, you have to install docker locally.

```bash
docker run -v /root/output:/root/resonances/output smirik/resonances-fortran ./comp.x -list 490 2348
docker run -v /root/output:/root/resonances/output smirik/resonances-fortran ./comp.x -range 1 100
```

If you want to build a new container:

```bash
docker build -t yourname/your-container-name .
```

### Meta information

* When the program finishes, `.aei` files will be created and placed in `./output/aei/` directory, and `.aei` files for planets will appear in `./output/aei_planets/`.
* Files with `id_matrices` will appear in `./output/id_matrices/` directory. The files with results by resonances will be placed in `./output/results/` directory.
* The common files `result_2body.txt` and `result_2body.txt` contain the information about all found resonances over all given asteroid list. Each record includes:
  * Asteroid name
  * Resonance (with planets and numbers)
  * Classification verdict (1 - pure resonance, 0 - transient)

#### Cleaning

The command `make clean` frees the disk memory from temporary files (including metadata, object files etc). Graphics, `.rpout` and `.aei` files will not be deleted.

## Running tests

1. `resonances-fortran` uses [funit](https://rubygems.org/gems/funit) gem for unit tests. To install it:

```bash
sudo gem install funit
```

> Please make sure that you have `ruby` installed.

You should also export `FC` variable (and some other variables if they are not):

```bash
export FC="gfortran"
```

```bash
export CXX="g++"
export CC="gcc"
export FSFLAG=-I
```

2. To launch tests run:

```
make tests
```

After launching the tests please make sure that you clean the directory:

```bash
make clean
```
