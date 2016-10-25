Gc Autofit
================

This application was generated with the [rails_apps_composer](https://github.com/RailsApps/rails_apps_composer) gem
provided by the [RailsApps Project](http://railsapps.github.io/).

Rails Composer is open source and supported by subscribers. Please join RailsApps to support development of Rails Composer.


Server
-----------
```bash

Hosting: Google Computing Engine

# Server Access
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



Problems? Issues?
-----------

Need help? Ask on Stack Overflow with the tag 'railsapps.'

Your application contains diagnostics in the README file. Please provide a copy of the README file when reporting any issues.

If the application doesn't work as expected, please [report an issue](https://github.com/RailsApps/rails_apps_composer/issues)
and include the diagnostics.

Ruby on Rails
-------------

This application requires:

- Ruby 2.2.2
- Rails 4.2.1

Learn more about [Installing Rails](http://railsapps.github.io/installing-rails.html).

Getting Started
---------------
### Development on Local PC

```bash
## mysql setting

## running mysql daemon
> mysqld_safe &

## in Sequel Pro
> mysql root without password 

## shutdown
$ mysqladmin -u root -p shutdown 

> mysqld_safe &

## inside of project folder
cd [project]/config
check database.yml

# check database.ml file
# copy database.ml.sample —> database.yml
application\config\database.yml

rake db:create # database/mysql 
rake db:migrate

## running daemon
> bundle exec guard # monitoring 
 
## Run Local Rails Daemon
localhost:3010
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
