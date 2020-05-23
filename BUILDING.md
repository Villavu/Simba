# Building Simba

You'll need Lazarus 2.0.4: https://sourceforge.net/projects/lazarus/files/

If using Linux:
  - `Xtst` is required: `sudo apt-get install libxtst-dev`
  - `GTK2` is required: `sudo apt-get install gtk2.0 libgtk2.0-dev`
  
1) Clone the Simba repository with git. `git clone https://github.com/MerlijnWajer/Simba`. 
2) Install submodules `git submodule init; git submodule update`.  
3) Simba is a Lazarus project. Open `Simba.lpi` located in `Projects/Simba/` in Lazarus and start building. 
4) When built, the Simba executable will be located in the root directory of the repository. 

-------

### Notes

* If you run into a `internal error` attempt building rather than running. FPC seems to have some issues with progressive building.

* The build process (below) is only fully executed when building, not running. So if changes are made to the SimbaScript project make sure you rebuild.

-----
### About The Build Process

  1) `SimbaScript.lpi` is built. This is the executable which runs scripts.
  2) `MakeSimbaResources.lpi` is built and executed. This creates a resource file that contains OpenSSL binaries and the SimbaScript executable that was just built.
  3) `Simba.lpi` is built. The resource file that was just created is included into the Simba binary. When Simba is started the resources are extracted which keeps Simba fully portable.

  This is achieved by using `Execute before` & `Execute after` options in `Project Options > Compiler Commands`.