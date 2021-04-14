GC-AutoFit
==========

Server
------
```bash
Hosting: Google Computing Engine

# Server Access (public version)
ssh gcms@gc-autofit.wishartlab.com

# as root
ssh root@gc-autofit.wishartlab.com

# TOP command
# check & manage process (login as gems)
top -> ‘u’ -> ‘gcms’ -> ‘c’ for list -> ‘k’ for kill process

note)	 puma and sidekiq should be only one. If these are more than two, then kill all and restart it.

# reboot server
* login as root
* use ‘reboot’ command

# restart process [gcms userid] from the "local" [anything with CAP]
* goto /gc-autofit directory
* bundle exec cap production sidekiq:start
* bundle exec cap production deploy:start # if no puma, then running (web 504 error?)

## change file upload size limitation
* /etc/nginx/conf.d/gcms.conf
    * change file size limit : right now 1200mb
* service nginx restart # restart for updated configuration

## log files
* general log: /var/log/nginx
* gc-autofit’s log: /apps/gcms/project/shared/log

## to set to increase session timeout
add a bunch of settings to the /etc/nginx/conf.d/gcms.conf file
```

Sidekiq was configured to use capistrano-sidekiq 2.x with Systemd. See
[Configure capistrano-sidekiq 2.x](http://wiki.wishartlab.com/wiki/index.php/Configure_capistrano-sidekiq_2.x).


Getting Started
---------------
### Development on Local PC

```bash
## install R if it is not installed already

## install R packages
R
> install.packages('gdata')
> source("http://bioconductor.org/biocLite.R")
> biocLite("xcms")

## install rbenv if it is not already installed.
## Using Homebrew on MacOS:
brew install rbenv ruby-build

## clone the repository, e.g.
git clone git@bitbucket.org:wishartlab/gc-autofit.git

## change into project directory and check ruby version
cd gc-autofit
cat .ruby-version

## check versions of Ruby installed by rbenv to see if
## that Ruby version is installed
rbenv versions

## install missing Ruby version if needed, e.g.
rbenv install 2.2.2
rbenv rehash

## Install bundler if it is not already installed
gem install bundler

## Install all gems for the project
bundle install

## note: if you get a puma error:
# gem install puma -- --with-cppflags=-I/usr/local/opt/openssl/include

## Set up YML files. In the config directory of the project,
## copy all files that have a .sample suffix to create
## versions without the suffix, e.g.:
cp config/database.yml.sample config/database.yml

## Install MySQL and start MySQL server. One way to start it: (may be optional if you already have MySQL runing)
#mysqld_safe &

## Create database
bundle exec rake db:create

## Load database schema
bundle exec rake db:schema:load

## Install redis if it is not already installed.
## With Homebrew on MacOS:
brew install redis


### To run the app

## Start redis server
redis-server

## Start guard daemon
bundle exec guard

## Visit site in web browser. Use the port on which guard is running, e.g.
http://localhost:3010


## Update codebase and database schema later on

git pull
bundle install
bundle exec rake db:migrate
```

### deploy command

update server after modification on local rails project directory

```bash
update git
git update
git push
bundle exec cap production deploy

bundle exec cap production sidekiq:start
bundle exec cap production deploy:start # if no puma on the processes, then running (web 504 error?)
```

Documentation and Support
-------------------------
### Google analytics code:
```
GA.tracker = "UA-59743942-6"
```

## Workflow

```bash
Phase I) Preprocessing
1. Upload spectra files (CDF or mzXML)
    1. Alkane Standard, Blank (only one), Samples (multiple)
    2. Individual file or zip of them
2. Parsing/Checking Alkane Standard
    1. peak picking —> collect RT (Retention times of Alkane Peaks)
3. Generating the Spectrum Plots for each spectrum
4. save all information tables as .RData


Phase II) Profiling
1. Upload spectra files (CDF or mzXML)
    1. Alkane Standard, Blank (only one), Samples (multiple)
    2. Individual file or zip of them
2. Parsing each spectrum file
    1. Generate Peak Tables with using threshold
3. Calculate RI (Retention Index)
4. Identification with using library
    1. calculate scores including Match Factor, M/Z correlations, and etc
    2. matching and screening with scores
    3. Generate final tables for identified compounds
5. Quantification with using calibration curves
    1. using ratio of EIC areas (target Ion’s)
    2. calculate concentration values
6. Generate Report (JSON file)
    1. JSON will be used for spectrum viewer and tables
```


## Run sidekiq:
```
bundle exec sidekiq -e development  -C config/sidekiq.yml 
```


## Run Rails:
```
bundle exec rails s -e development
```