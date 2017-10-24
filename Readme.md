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

* `use_only_integration`: if true, the program will just perform an integration (using Mercury6 package) and create .aei files without finding any resonances.
* `just_id_matrices` will force the program only create files with id_matrices without finding any resonances and .aei files
* `force_aei_rebuilding`: if true, the integration will be performed even if corresponding .aei files already exist
* `mode_2body`: turns on 2-body case
* `mode_3body` turns on 3-body case

When all keys are configured, the program can be launched.

## Use program

1. To compile the software run:

```bash
make
```

2. Download [AstDyS](http://hamilton.dm.unipi.it/astdys/index.php?pc=4) catalog of the orbital elements. 

3. Build `asteroids.bin` from it and move to the main directory. In order to build `asteroids.bin` file run the following command:

```bash
??
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

### Meta information

* When the program finishes, `.aei` files will be created and placed in `./aeibase/` directory, and `.aei` files for planets will appear in `./aei_planet/`.
* Files with `id_matrices` will appear in `./id_matrices/` directory. Other files will be placed in `./wd/` directory.
* `.rpout2/3` files contain the information about identified resonances for each asteroid. Each record includes:
  * Resonance (with planets and numbers)
  * Classification verdict (1 - pure resonance, 0 - transient, -1 - circulation)
  * Status of acknowledged, based on cross-Fourier analysis (1 - true, or 0 - false)
* `.rp`, `.circ`, `.phout`, `.per`, `.smooth` files are metadata
* `.png` files are graphics

The common files `current_result_2.txt` and `current_result_3.txt` contain the information about all found resonances over all given asteroid list in the same format.

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
