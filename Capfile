set :rbenv_ruby, '2.7.1'

# Load DSL and Setup Up Stages
require 'capistrano/setup'

# Includes default deployment tasks
require 'capistrano/deploy'


require "capistrano/scm/git"
install_plugin Capistrano::SCM::Git

# Includes tasks from other gems included in your Gemfile
#
# For documentation on these, see for example:
#
#   https://github.com/capistrano/rvm
#   https://github.com/capistrano/rbenv
#   https://github.com/capistrano/chruby
#   https://github.com/capistrano/bundler
#   https://github.com/capistrano/rails/tree/master/assets
#   https://github.com/capistrano/rails/tree/master/migrations
#
# require 'capistrano/rvm'
# require 'capistrano/chruby'
require 'capistrano/bundler'
require 'capistrano/rails/assets'
require 'capistrano/rails/migrations'
require 'capistrano/rbenv'
require 'capistrano/rails'
require 'capistrano/rails/console'

require 'capistrano/puma'
install_plugin Capistrano::Puma

require 'capistrano/sidekiq'
install_plugin Capistrano::Sidekiq  # Default sidekiq tasks
install_plugin Capistrano::Sidekiq::Systemd

# Loads custom tasks from `lib/capistrano/tasks' if you have any defined.
Dir.glob('lib/capistrano/tasks/*.cap').each { |r| import r }