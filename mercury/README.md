# Trying out mercury

[Prolog to Mercury](doc/mercury_trans_guide.html)

mmc --make module.m

# Install script for mercury (on Ubuntu 15.04)

```Shell
wget http://dl.mercurylang.org/release/mercury-srcdist-14.01.1.tar.gz
tar -zxf mercury-srcdist-14.01.1.tar.gz
cd mercury-srcdist-14.01.1/
sudo apt-get install flex
sudo apt-get install bison
./configure --disable-most-grades
time make
sudo make install
```
