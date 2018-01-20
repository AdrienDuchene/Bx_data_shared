# Databases synchronization using bidirectional transformations and peer-to-peer system

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.

### Prerequisites

To running our project you need to have installed Haskell, Cabal and BiGUL.
```
apt update
apt install ghc
apt install cabal-install
```

BiGUL works with GHC 7.10 and above, and is released to Hackage, so the installation of the latest release of BiGUL is as simple as executing
```
cabal update
cabal install BiGUL --allow-newer base
```
in the command line (i.e., the standard way of installing Haskell packages). [\[PRL - Bidirectional Programming - BiGUL\]](https://bitbucket.org/prl_tokyo/bigul/overview)

The project needs some Haskell packages to work

Required :
```
cabal install split
cabal install random-strings-0.1.1.0
```

Also to install HDBC to interact with databases, there are some other command :
````
apt install libmariadb-client-lgpl-dev-compat
cd /usr/bin
ln -s /usr/bin/mariadb_config /usr/bin/mysql_config
cabal install HDBC-mysql
````

### Installing
To install our project you have to clone the Git repository as follow

```
git clone https://github.com/qlombat/duduloma.git
```

The useful code to run example is in the following directory
```
cd duduloma/Using/Example
```

### Using
#### Simple example 
In the file Config.hs, you can find all parameters to setup the conection to a database. 
After that, you compile the Main.hs file

````
ghci Main.hs
````

Then, the system need to be setup by this command :
````
setup
````
Follow the instructions.
Once the setup is finish, the system is ready.
To run the example, you need to use runhaskell :
````
runhaskell Main.hs
````
Now, follow the instructions on the terminal. 

The share data will be now on the .TXT file called "directory" in Config.hs.
The data pushed will appear in this file and the data pulled will appears directly in your database.

To test easily, you can try to synchronize two table in the same database.

For now, you can only use String and Integer to select data from a table.

## Authors

* **Adrien Duchêne** - [AdrienDuchene](https://github.com/AdrienDuchene)
* **Hugues Marchal** - [marchalh](https://github.com/marchalh)
