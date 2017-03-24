**Resonant axis calculator, resonance finder, integrator, phase builder,
classifier, compositor etc.**

-------------------------------------------------------------
-------------------------------------------------------------
    24.03.2017
    This is a description of a program "Compositor".
    The purpose of the program is to find potential resonances for
    asteroids and identify them. Currently 2-body and 3-body cases are available
    for handling.
    
    Before launching the program be sure that you specified
    the path for main directory as a variable "pwd" in "global_parameters.f90".

    There are some logical keys in "global_parameters.f90" that can be configured
    to make a special mode of running the program. Those keys are:
    
    "use_only_integration" - if true, the program will just perform an integration
    (using Mercury6 package) and create .aei files without finding any resonances.
    
    "just_id_matrices" - that will force the program only create files with id_matrices
    without finding any resonances and .aei files
    
    "allow_writing_metadata" - allows creating metadata files (usually for creating graphics
    following after that)
    
    "allow_plotting" - allow plotting graphics
    (works when creating metadata is allowed too)
    
    "dispose_metadata" - delete metadata after using
    (because it can take a lot of disk memory
    
    "force_aei_rebuilding" - if true, the integration will be performed even if corresponding
    .aei files already exist
    
    "mode_2body" - turns on 2-body case
    "mode_3body" - - turns on 3-body case


    When all keys are configured, the program can be launched.

    To launch the program, run:
    --> make
    --> ./comp.x [-range | -list] <argument list/range>

    If "-list" key is used, just provide a list of asteroid names.
    If "-range" key is used, specify only first and last asteroid.
    
    Example:
    --> ./comp.x -list 1013 99942 309239 499
    --> ./comp.x -range 1 1000
    
    When the program finishes, .aei files will be created and
    placed in ./aeibase/ directory.
    Files with id_matrices will appear in ./id_matrices/ directory.
    Other files will be placed in ./wd/ directory.
    
    .rpout2/3 files contain the information about identified resonances for each asteroid.
    Each record includes:
    - Resonance (with planets and numbers)
    - Classification verdict (1 - pure resonance, 0 - transient, -1 - circulation)
    - Additional data (circulation time and ratio, slow circulation/transient time and ratio)
    
    .rp, .circ, .phout, .per, .smooth files are metadata
    .png files are graphics

    The command "make clean" frees the disk memory from temporary files
    (including metadata, object files etc).
    Graphics, .rpout and .aei files will not be deleted
    
    Unit tests:
    There are some facilities for doing unit tests.
    To start them, run:
    --> make test
    If the program needs to be launced after that, it is strictly recommended
    to "make clean" before.