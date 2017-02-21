**Integrator**

-------------------------------------------------------------
-------------------------------------------------------------
    Here is described how to launch the current version of the integrator
    and what it does. The full documentation will be prepared in the future.

    Launch the program:
    python3 integrator.py [key] <arguments>
    where:
    [key] - can be "-range" or "-list" if specified
    <arguments>:
        - if no key or "-list' key specified, expected to be a list of asteroid names
        - if "-range" key specified, there must be names of the first and last asteroids
        in an increasing range (useful only for numbered asteroids)

    Expamples:
    python3 integrator.py 309239
    python3 integrator.py 1862 345 90549
    python3 integrator.py -list 1862 345 90549
    python3 integrator.py -range 1 1000
    
    Notes: it is expected that mercury submodule already has executable files
    (usually via "init.sh" in the parent directory).


    The program checks for each asteroid in a set if it needs to be integrated and tries
    to find its orbital elements in a source file (see current "asteroids_format.csv").
    The source must be strictly formatted. After that the integration starts and .aei files
    are placed in "aeibase/" directory.
    
    
    Possible malfunctions:
        - .aei files are checked only for existance and it is possible to pass
        "broken" .aei files.
        - There is no protection from occurence when './element6' process unexpectedly
        interrupts (by any reason), causing "broken" .aei files to appear. Be sure that
        your session can successfully finish.