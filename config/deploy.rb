#require 'puma/capistrano'
require 'rails'

set :application, 'gc-autofit'
set :repo_url,  "git@bitbucket.org:wishartlab/gc-autofit.git"
set :scm, :git
set :deploy_to, '/apps/gcms/project'
set :use_sudo, false
set :linked_files, %w{config/database.yml}
set :linked_dirs, %w{public/system log gcmsjobs tmp gcms}
set :keep_releases, 3

# set :sidekiq_config, "#{Rails.root}/config/sidekiq.yml"
# set :sidekiq_pid,  File.join('/', 'tmp', 'gc-autofit.sidekiq.pid')

# set :rbenv_map_bins, %w{rake gem bundle ruby rails sidekiq sidekiqctl}

#set :branch, ENV['BRANCH'] if ENV['BRANCH']
set :branch, 'master'


namespace :deploy do

  desc 'Start application'
  task :start do
    on roles(:web) do
      invoke('puma:start')
    end
  end

  # desc 'Restart application'
  # task :restart do
  #   on roles(:web) do
  #     invoke('puma:phased-restart')
  #   end
  # end

  desc 'Hard-restart application'
  task :restart do
    on roles(:web) do
      invoke('puma:restart')
    end
  end

  desc 'Stop application'
  task :stop do
    on roles(:web) do
      invoke('puma:stop')
    end
  end

  after :finishing, 'deploy:cleanup'
  after :finished, :restart
end
