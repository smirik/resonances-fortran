**Resonant axis calculator, resonance finder, integrator, phase builder,
classifier, compositor etc.**

-------------------------------------------------------------
-------------------------------------------------------------
    13.03.2017
    Here is described just how to launch the current version.
    Documentation will be prepared in the future.


    NOTE: before launching the program be sure that you specified
    the path for main directory as a variable "pwd" in "global_parameters.f90".

    Launch the program:
    --> make
    --> ./comp.x [-range | -list] <argument list/range>

    If "-list" key is used, just provide a list of asteroid names.
    If "-range" key is used, specify only first and last asteroid.
    
    Example:
    --> ./comp.x -list 1013 99942 309239 499
    --> ./comp.x -range 1 1000
    
    When the program finishes, .aei files will be created and
    placed in ./aeibase/ directory and other files will be placed
    in ./wd directory.
    
    FOR CURRENT VERSION: parameter "use_only_integration" in the file
    "global_parameters.f90" is set to "1". It means that only integration
    by MERCURY6 is performed for now, while other functions are avaiable,
    but unactive.