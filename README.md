Gc Autofit
================

This application was generated with the [rails_apps_composer](https://github.com/RailsApps/rails_apps_composer) gem
provided by the [RailsApps Project](http://railsapps.github.io/).

Rails Composer is open source and supported by subscribers. Please join RailsApps to support development of Rails Composer.

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
############################################
# Development on Local PC
##

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
> bundle exec guard
> Rails s
OR 
> bundle exec guard # monitoring 
 
## Run Local Rails Daemon
localhost:3010


############################################
# deploy command
# update server after modification
####
# on local rails project directory

update git 
git update
git push
bundle exec cap production deploy

bundle exec cap production sidekiq:start
bundle exec cap production deploy:start # if no puma on the processes, then running (web 504 error?)

Documentation and Support
-------------------------

Issues
-------------

Similar Projects
----------------

Contributing
------------

Credits
-------

License
-------